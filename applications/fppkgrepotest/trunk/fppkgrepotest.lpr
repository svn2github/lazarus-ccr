program fppkgrepotest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
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
    function GetJsonLogArray(ForceCreate: Boolean): TJSONArray;
    function SetupRepository(): Boolean;
    function LastError: String;
    procedure LoadIniFile();
  protected
    procedure DoRun; override;
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
  finally
    IniFile.Free;
  end;
end;

procedure TfppkgRepoTest.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('htdv', ['help','test','init']);
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

  // stop program loop
  Terminate;

  WriteLn(FJsonResult.FormatJSON);
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

