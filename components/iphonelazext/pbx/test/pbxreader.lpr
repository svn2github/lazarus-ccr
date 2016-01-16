program pbxreader;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  heaptrc,
  Classes, SysUtils, pbxfile, pbxcontainer, xcodeproj
  { you can add units after this };

function ReadFileToString(const fn: string): string;
var
  fs : TFileStream;
begin
  fs:=TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, fs.Size);
    fs.Read(Result[1], fs.Size);
  finally
    fs.Free;
  end;
end;

procedure WriteStringToFile(const s, fn: string);
var
  fs : TFileStream;
begin
  fs:=TFileStream.Create(fn, fmCreate);
  try
    if length(s)>0 then begin
      fs.Write(s[1], length(s));
      fs.Size:=length(s);
    end;
  finally
    fs.Free;
  end;
end;

procedure TestProject(const buf: string);
var
  c     : TPBXContainer;
  st    : TStringStream;
  info  : TPBXFileInfo;
  prj   : PBXProject;
  i     : Integer;
begin
  c:= TPBXContainer.Create;
  st := TStringStream.Create(buf);
  try
    c.ReadFile(st, info);
    writeln('arch ver: ',info.archiveVersion);
    writeln(' obj ver: ',info.objectVersion);
    writeln('root obj: ', PtrUInt( info.rootObject ));

    if info.rootObject is PBXProject then begin
      writeln('project!');
      prj:=PBXProject(info.rootObject);
      writeln(prj.knownRegions.Text);
      writeln('targets: ', prj.targets.Count );
      for i:=0 to prj.targets.Count-1 do begin
        writeln(prj.targets.Items[i].ClassName);
        writeln(PBXNativeTarget(prj.targets.Items[i]).name);
      end;

      writeln(PtrUInt(prj.buildConfigurationList));
      writeln('build configuration:');
      for i:=0 to prj.buildConfigurationList.buildConfigurations.Count-1 do begin
        writeln('  ',XCBuildConfiguration(prj.buildConfigurationList.buildConfigurations[i]).name);
      end;
    end;
  finally
    st.Free;
    c.Free;
  end;
end;

type

  { MyClass }

  MyClass = class(TObject)
  private
    fInt: Integer;
    fInt2: Integer;
    fInt3: Integer;
    fRO: TList;
    fRW: TList;
  protected
    function GetInt3: Integer;
    procedure SetInt3(AValue: Integer);
    function GetInt2: Integer;
  public
    constructor Create;
  published
    property Int: Integer read fInt write fInt;
    property Int2: Integer read GetInt2 write fInt2;
    property Int3: Integer read GetInt3 write SetInt3;
    property RO: TList read fRO;
    property RW: TList read fRW write fRW;

  end;

var
  lst : TList;
  m  : MyClass;

{ MyClass }

function MyClass.GetInt3: Integer;
begin
  Result:=fInt3;
end;

procedure MyClass.SetInt3(AValue: Integer);
begin
  fInt3:=AValue;
end;

function MyClass.GetInt2: Integer;
begin
  Result:=fInt2;
end;

constructor MyClass.Create;
begin
  fRO:=TList.Create;
  fRW:=TList.Create;
end;

procedure TestRTTI;
var
  lst : TList;
  m   : MyClass;
begin
  lst:=TList.Create;
  m:=MyClass.Create;
  PBXGatherObjects(m, lst);
end;

procedure TestWriter;
var
  w : TPBXWriter;
begin
  w := TPBXWriter.Create;
  try
    w.OpenBlock('{');
    w.WriteName('archiveVersion');
    w.WriteValue('1');
    w.WriteName('classes');
    w.OpenBlock('{');
    w.CloseBlock('}');
    w.WriteName('objectVersion');
    w.WriteValue('46');
    w.WriteName('objects');
    w.OpenBlock('{');
    w.CloseBlock('}');
    w.WriteName('rootObject');
    w.WriteValue('aaaa','Project object');
    w.CloseBlock('}');
    write(w.Buffer);
  finally
    w.Free;
  end;
end;


procedure TestReadThenWrite;
var
  prj   : PBXProject;
  list  : TList;
  i     : Integer;
  st    : TStringList;
begin
  if ParamCount=0 then begin
    writeln('please provide pbx file');
    Exit;
  end;
  //ScanAString( ReadFileToString(ParamStr(1)));
  //ParseAString( ReadFileToString(ParamStr(1)));
  //TestProject( ReadFileToString(ParamStr(1)));
  if ProjectLoadFromFile(ParamStr(1), prj) then begin
    //list:=TList.Create;
    //PBXGatherObjects(prj, list);
    Write(ProjectWrite(prj));

    prj.Free;
    {st:=TStringList.Create;
    try
      for i:=0 to list.Count-1 do begin
        st.AddObject( PBXObject(list[i]).ClassName, PBXObject(list[i]));
      end;
      st.Sort;
      for i:=0 to st.Count-1 do begin
        writeln(PBXObject(st.Objects[i]).__id,' : ',PBXObject(st.Objects[i]).ClassName);
      end;

    finally
      st.Free;
    end;}

    //list.Free;
  end else
    writeln('not a project');
end;

procedure TestWriteAProject;
var
  p : PBXProject;
  s : string;
  t : PBXNativeTarget;
  prd : PBXGroup;
  //cfg : XCBuildConfiguration;
  ph   : PBXShellScriptBuildPhase;
begin
  p:=ProjectCreateMin;
  p.buildConfigurationList._headerComment:=p.buildConfigurationList._headerComment+' for PBXProject "test"';
  p.attributes.AddStr('LastUpgradeCheck','0610');
  t:=ProjectAddTarget(p,'targetto');
  ph:=TargetAddRunScript(t);
  ph.shellScript:='echo "hello world"';
  //ph.buildActionMask:='0';

  ph.runOnlyForDeploymentPostprocessing:=false;
  t.productReference:=FileRefCreate('targetto', FILETYPE_EXEC);
  PBXFileReference(t.productReference).sourceTree:='BUILT_PRODUCTS_DIR';
  t.productName:='targetto';
  t.productType:=PRODTYPE_TOOL;

  // at least one configuration is added !
  //todo: a target should automatically copy project's building settings
  t.buildConfigurationList:=XCConfigurationList.Create;
  t.buildConfigurationList._headerComment:='Build configuration list for PBXNativeTarget "targetto"';
  t.buildConfigurationList.addConfig('Default').buildSettings.AddStr('PRODUCT_NAME','targetto');
  t.buildConfigurationList.addConfig('Release').buildSettings.AddStr('PRODUCT_NAME','targetto');
  t.buildConfigurationList.defaultConfigurationIsVisible:='0';
  t.buildConfigurationList.defaultConfigurationName:='Release';


{  cfg:=XCBuildConfiguration(p.buildConfigurationList.buildConfigurations[0]);
  cfg.buildSettings.AddStr('COPY_PHASE_STRIP', 'NO');
  cfg.buildSettings.AddStr('GCC_DYNAMIC_NO_PIC', 'NO');
  cfg.buildSettings.AddStr('GCC_OPTIMIZATION_LEVEL', '0');
  cfg.buildSettings.AddStr('PRODUCT_NAME', 'targetto');   }


  p.mainGroup:=GroupCreateRoot('/Users/dmitry/pbx/utils/test.xcodeproj');
  // requirements ?
  prd:=p.mainGroup.addSubGroup('Products');
  prd.children.Add(t.productReference);

  p.productRefGroup:=prd;

  p.compatibilityVersion:='Xcode 3.2';

  s:=ProjectWrite(p);
  WriteStringToFile(s, 'test.xcodeproj/project.pbxproj');
  p.Free;
end;

begin
  if FileExists('leaks.txt') then DeleteFile('leaks.txt');
  SetHeapTraceOutput('leaks.txt');
  try
    TestReadThenWrite;
  except
    on e: exception do
      writeln(e.Message);
  end;
end.


