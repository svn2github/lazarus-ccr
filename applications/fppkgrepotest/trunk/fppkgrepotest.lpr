program fppkgrepotest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  ShellApi,
  windows,
  {$ENDIF}
  Classes,
  SysUtils,
  process,
  CustApp,
  pkgrepos,
  pkgoptions,
  pkgcommands,
  pkgfpmake,
  pkgglobals,
  pkghandler,
  typinfo,
  fprepos,
  IniFiles,
  {$if (defined(unix) and not defined(android)) or defined(windows)}
    pkgwget,
    pkglnet,
  {$endif}
  fpjson;

type

  { TfppkgRepoTest }

  TfppkgRepoTest = class(TCustomApplication)
  private
    FJsonResult: TJSONObject;
    FJsonCommandArr: TJSONArray;
    FCurrentCommand: TJSONObject;
    FRepoDir: String;
    FStartCompiler: String;
    function GetJsonLogArray(ForceCreate: Boolean): TJSONArray;
    function SetupRepository(): Boolean;
    function LastError: String;
    procedure LoadIniFile();
    function ExecuteProcess(ACmd: string; AParamList: array of const): boolean;
    function RemoveTree(APath: String): Boolean;
  protected
    procedure DoRun; override;
    procedure InitializeRepository();
    procedure TestPackage(APackageName: String);
    procedure TestSinglePackage(APackage: TFPPackage);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure AddLogLine(Level:TLogLevel; Const Msg: String);
  end;

var
  Application: TfppkgRepoTest;

procedure LogCmd(Level:TLogLevel; Const Msg: String);
begin
  if (Level in LogLevels) then
    Application.AddLogLine(Level, Msg);
end;

{ TfppkgRepoTest }

function TfppkgRepoTest.GetJsonLogArray(ForceCreate: Boolean): TJSONArray;
var
  Parent: TJSONObject;
begin
  if Assigned(FCurrentCommand) then
    Parent := FCurrentCommand
  else
    Parent := FJsonResult;

  Result := Parent.Find('log', jtArray) as TJSONArray;
  if ForceCreate and not assigned(Result) then
    begin
    Result := TJSONArray.Create;
    Parent.Add('log', Result);
    end;
end;

function TfppkgRepoTest.SetupRepository: Boolean;
begin
  Result := False;
  try
    LogLevels:=DefaultLogLevels;
    pkgoptions.LoadGlobalDefaults(FRepoDir+'etc/fppkg.cfg');
    LoadCompilerDefaults;

    FPMakeCompilerOptions.CheckCompilerValues;
    CompilerOptions.CheckCompilerValues;

    LoadLocalAvailableRepository;
    FindInstalledPackages(CompilerOptions);
    CheckFPMakeDependencies;

    LoadLocalAvailableMirrors;

    Result := True;
  except
    on E: Exception do
      Log(etError, Format('Failed to setup repository: %s', [E.Message]));
  end;
end;

function TfppkgRepoTest.LastError: String;
var
  LogArr: TJSONArray;
  LogItem: TJSONObject;
  i: Integer;
begin
  result := '';
  LogArr := GetJsonLogArray(False);
  if Assigned(LogArr) then
    begin
    for i := LogArr.Count-1 downto 0 do
      begin
      LogItem := (LogArr.Items[i]) as TJSONObject;
      if LogItem.Get('LogLevel', '') = GetEnumName(TypeInfo(TLogLevel),LongInt(llError)) then
        begin
        result := LogItem.Get('Message', '');
        break;
        end;
      end;
    end;
end;

procedure TfppkgRepoTest.LoadIniFile;
var
  IniFile: TIniFile;
  CfgFile: String;
begin
  CfgFile:=ChangeFileExt(ParamStr(0), '.ini');
  IniFile := TIniFile.Create(CfgFile);
  try
    FRepoDir := IncludeTrailingPathDelimiter(ExpandFileName(IniFile.ReadString('Settings','repodir','repotest')));
    FStartCompiler := ExpandFileName(IniFile.ReadString('Settings','startcompiler','ppc386'+ExeExt));
  finally
    IniFile.Free;
  end;
end;

function TfppkgRepoTest.ExecuteProcess(ACmd: string; AParamList: array of const): boolean;
var
  P: TProcess;
  i: Integer;
begin
  result := False;
  P := TProcess.Create(nil);
  try
    P.Executable:=ACmd;
    for i := 0 to high(AParamList) do
      begin
      if AParamList[i].VType=vtAnsiString then
        P.Parameters.Add(ansistring(AParamList[i].VAnsiString))
      else
        raise exception.CreateFmt('parameter type %d not supported',[AParamList[i].VType]);
      end;
    P.Options:=[poWaitOnExit];
    P.Execute;
    result := P.ExitCode=0;
  finally
    P.Free;
  end;
end;

function TfppkgRepoTest.RemoveTree(APath: String): Boolean;

var
{$ifdef MSWINDOWS}
  SHFileOpStruct: TSHFileOpStruct;
  DirBuf: array[0..MAX_PATH+1] of TCHAR;
{$else MSWINDOWS}
  searchRec: TSearchRec;
  SearchResult: longint;
  s: string;
{$endif MSWINDOWS}

begin
  result := true;
{$ifdef MSWINDOWS}
  try
    FillChar(SHFileOpStruct, Sizeof(SHFileOpStruct), 0);
    FillChar(DirBuf, Sizeof(DirBuf), 0);
    StrPCopy(DirBuf, APath);
    with SHFileOpStruct do
    begin
      pFrom := @DirBuf;
      wFunc := FO_DELETE;
      fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
    end;
    Result := SHFileOperation(SHFileOpStruct) = 0;
  except
    Result := False;
  end;
{$else MSWINDOWS}
  SearchResult := FindFirst(IncludeTrailingPathDelimiter(ADirectoryName)+AllFilesMask, faAnyFile+faSymLink, searchRec);
  try
    while SearchResult=0 do
      begin
        if (searchRec.Name<>'.') and (searchRec.Name<>'..') then
           begin
             s := IncludeTrailingPathDelimiter(ADirectoryName)+searchRec.Name;
             if (searchRec.Attr and faDirectory)=faDirectory then
               begin
                 if not IntRemoveTree(s) then
                   result := false;
               end
             else if not DeleteFile(s) then
               result := False
             else
               log(vldebug, SDbgDeletedFile, [s]);
           end;
        SearchResult := FindNext(searchRec);
      end;
  finally
    FindClose(searchRec);
  end;

  // There were reports of RemoveDir failing due to locking-problems. To solve
  // these the RemoveDir is tried three times, with a delay of 5 seconds. See
  // bug 21868
  result := RemoveDir(ADirectoryName);
{$endif WINDOWS}
end;

procedure TfppkgRepoTest.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('htidv', ['help','test','initializerepository']);
  if ErrorMsg<>'' then
    begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
    end;

  // parse parameters

  if HasOption('v','verbose') then
    LogLevels:=AllLogLevels;
  if HasOption('d','debug') then
    LogLevels:=AllLogLevels+[llDebug];

  LoadIniFile();

  if HasOption('h', 'help') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  if HasOption('t','test') then
    begin
    TestPackage(GetOptionValue('t','test'));
    end;

  if HasOption('i','initializerepository') then
    begin
    InitializeRepository();
    end;

  // stop program loop
  Terminate;

  WriteLn(FJsonResult.FormatJSON);
end;

procedure TfppkgRepoTest.InitializeRepository;
var
  FpcmkcfgBin: string;
  FpcPath: string;
  FpccfgName: string;
  FpcBin: string;
  sr: TSearchRec;
  UnitDir: string;
begin
  if not DirectoryExistsLog(FRepoDir+'fpcsrc') then
    begin
    writeln('Not a valid repository-test directory: '+FRepoDir);
    Exit;
    end;
  SetCurrentDir(FRepoDir+'fpcsrc');
  if not ExecuteProcess('svn'+ExeExt,['update']) then
    raise exception.create('Failed to run svn update');
  if not ExecuteProcess('make'+ExeExt, ['clean', 'all', 'PP="'+FStartCompiler+'"', 'FPMAKEOPT="-T 4"']) then
    raise exception.create('Failed to compile fpc');
  RemoveDir(FRepoDir+'fpc');
  if not ExecuteProcess('make'+ExeExt, ['install', 'PREFIX="'+FRepoDir+'fpc"']) then
    raise exception.create('Failed to install fpc');

  FpcmkcfgBin:=FRepoDir+'fpc'+DirectorySeparator+'bin'+DirectorySeparator+'i386-win32'+DirectorySeparator+'fpcmkcfg'+ExeExt;
  FpcPath:=FRepoDir+'fpc';
  FpccfgName:=FRepoDir+'fpc'+DirectorySeparator+'bin'+DirectorySeparator+'i386-win32'+DirectorySeparator+'fpc.cfg';
  FpcBin:=FRepoDir+'fpc'+DirectorySeparator+'bin'+DirectorySeparator+'i386-win32'+DirectorySeparator+'ppc386'+ExeExt;
  UnitDir:=FRepoDir+'fpc'+DirectorySeparator+'units'+DirectorySeparator+'i386-win32'+DirectorySeparator;

  if not ExecuteProcess(FpcmkcfgBin, ['-p', '-d "basepath='+FpcPath+'"', '-d "basepath='+FpcPath+'"', '-o "'+FpccfgName+'"']) then
    raise exception.create('Failed to create fpc.cfg');
  if not ExecuteProcess(FpcmkcfgBin, ['-p', '-3','-d "LocalRepository='+FRepoDir+'fppkg'+DirectorySeparator+'"', '-o "'+FRepoDir+'etc'+DirectorySeparator+'fppkg.cfg"']) then
    raise exception.create('Failed to create fppkg.cfg');
  if not ExecuteProcess(FpcmkcfgBin, ['-p', '-4', '-d "GlobalPrefix='+FpcPath+'"', '-d "FpcBin='+FpcBin+'"', '-o "'+FRepoDir+'fppkg'+DirectorySeparator+'config'+DirectorySeparator+'default"']) then
    raise exception.create('Failed to create fppkg.cfg');

  RemoveTree(FRepoDir+'fpc'+DirectorySeparator+'fpmkinst');

  if FindFirst(UnitDir+AllFiles, faDirectory, sr) = 0 then
    begin
    repeat
    if (sr.Name <> 'rtl') and (sr.Name <> '.') and (sr.Name <> '..') then
      begin
      RemoveTree(UnitDir+sr.Name);
      end;
    until FindNext(sr)<>0;
    end;
end;


procedure TfppkgRepoTest.TestSinglePackage(APackage: TFPPackage);
var
  JsonResult: TJSONObject;
  ErrMsg: String;
begin
  FCurrentCommand := TJSONObject.Create;
  try
    FJsonCommandArr.Add(FCurrentCommand);

    FCurrentCommand.Add('action', 'test');
    JsonResult := TJSONObject.Create;
    FCurrentCommand.Add('result', JsonResult);

    if not Assigned(APackage) then
      begin
      JsonResult.Add('status','failed');
      JsonResult.Add('message', 'Package not found');
      exit;
      end;

    if not (CompilerOptions.CompilerCPU in APackage.CPUs) or
       not (CompilerOptions.CompilerOS in APackage.OSes) then
      begin
      JsonResult.Add('status','skipped');
      JsonResult.Add('message', 'Package '+APackage.Name+' is not supported fot this target');
      exit;
      end;

    FCurrentCommand.Add('packagename', APackage.Name);

    try
      pkghandler.ExecuteAction(APackage.Name, 'install');
    except
      on E: Exception do
        pkgglobals.Log(llError, Format('Failed to install package %s: %s', [APackage.Name, E.Message]))
    end;

    ErrMsg:=LastError;
    if ErrMsg<>'' then
      begin
      JsonResult.Add('status','failed');
      JsonResult.Add('message', Format('Test failed: %s', [ErrMsg]));
      end
    else
      begin
      JsonResult.Add('status','ok');
      JsonResult.Add('message', 'Test passed');
      end;
  finally
    FCurrentCommand := nil;
  end;
end;

procedure TfppkgRepoTest.TestPackage(APackageName: String);
var
  i: Integer;
begin
  if SetupRepository then
    begin
    if APackageName='all' then
      begin
      for i:=0 to AvailableRepository.PackageCount-1 do
        TestSinglePackage(AvailableRepository.Packages[i]);
      end
    else
      TestSinglePackage(AvailableRepository.FindPackage(APackageName));
    end;
end;

constructor TfppkgRepoTest.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  LogHandler := @LogCmd;
  FJsonResult := TJSONObject.Create;
  FJsonCommandArr := TJSONArray.Create;
  FJsonResult.Add('commands',FJsonCommandArr);
  StopOnException:=True;
end;

destructor TfppkgRepoTest.Destroy;
begin
  FJsonResult.Free;
  inherited Destroy;
end;

procedure TfppkgRepoTest.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

procedure TfppkgRepoTest.AddLogLine(Level: TLogLevel; const Msg: String);
var
  AJsonObj: TJSONObject;
begin
  AJsonObj := TJSONObject.Create;
  GetJsonLogArray(True).Add(AJsonObj);
  AJsonObj.Add('LogLevel', GetEnumName(TypeInfo(TLogLevel),LongInt(Level)));
  AJsonObj.Add('Message', Msg);
end;

begin
  Application:=TfppkgRepoTest.Create(nil);
  Application.Title:='Fppkg repository tester';
  Application.Run;
  Application.Free;
end.

