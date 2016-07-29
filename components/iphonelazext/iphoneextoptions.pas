{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the iPhone Laz Extension                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit iPhoneExtOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, LazIDEIntf, ProjectIntf, MacroIntf,
  iPhoneBundle, XMLConf, XcodeUtils
  , LazFileUtils, LazFilesUtils
  , iphonesimctrl, xcodeproj;

const
  DefaultResourceDir = 'Resources';

type

  { TiPhoneProjectOptions }

  TiPhoneProjectOptions = class(TAbstractIDEOptions)
  private
    fisiPhone     : Boolean;
    fAppID        : String;
    fSDK          : String;
    DataWritten   : Boolean;
    fSpaceName    : String;
    fResourceDir  : String;
    fExcludeMask  : String;
    fMainNib      : String;
    fResFiles     : TStrings;
  public
    //constructor Create; override;
    constructor Create;
    destructor Destroy; override;
    class function GetGroupCaption: String; override;
    class function GetInstance: TAbstractIDEOptions; override;
    function Load(AProject: TLazProject = nil): Boolean;
    function Save(AProject: TLazProject = nil): Boolean;
    procedure Reset;
    property isIPhoneApp: Boolean read fisIPhone write fisIPhone;
    property SDK: String read fSDK write fSDK;
    property AppID: String read fAppID write fAppID;
    property SpaceName: String read fSpaceName write fSpaceName;
    property ResourceDir: String read fResourceDir write fResourceDir;
    property ExcludeMask: String read fExcludeMask write fExcludeMask;
    property MainNib: String read fMainNib write fMainNib;
    property ResFiles: TStrings read fResFiles;
  end;

  { TiPhoneEnvironmentOptions }

  TSDKInfo = class(TObject)
    devName : String;
    devPath : String;
    simName : String;
    simPath : String;
    options : String;
  end;

  TiPhoneEnvironmentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    fPlatformsBaseDir : String;
    fCompilerPath     : String;
    fBaseRTLPath      : String;
    fCommonOpt        : String;
    fSimAppsPath      : String;
    fSimBundle        : String;
    fDefaultSDK       : String;
    fDefaultSimType   : String;
    fDefaultDeviceID  : String;
    fScriptTemplate   : String;

    fVersions   : TStringList;
    fDeviceList : TList;
    function GetDevice(i: integer): TSimDevice;
    function GetDeviceCount: Integer;
  protected
    function XMLFileName: String;

    procedure ClearVersionsInfo;
    procedure FoundSDK(const Version, DevSDKName, DevSDKPath, SimSDKName, SimSDKPath: String);
    function GetSDKInfo(const Version: String): TSDKInfo;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetGroupCaption: String; override;
    class function GetInstance: TAbstractIDEOptions; override;

    function Load: Boolean;
    function Save: Boolean;

    function GetSDKName(const SDKVer: String; simulator: Boolean): String;
    function GetSDKFullPath(const SDKVer: String; simulator: Boolean): String;

    function SubstituteMacros(var s: string): boolean;

    procedure GetSDKVersions(Strings: TStrings);
    procedure RefreshVersions;

    procedure DeviceListClear;
    procedure DeviceListReload;

    property ScriptTemplate: String read fScriptTemplate write fScriptTemplate;
    property PlatformsBaseDir: String read fPlatformsBaseDir write fPlatformsBaseDir;
    property CompilerPath: String read fCompilerPath write fCompilerPath;
    property BaseRTLPath: String read fBaseRTLPath write fBaseRTLPath;
    property CommonOpt: String read fCommonOpt write fCommonOpt;

    property SimBundle: String read fSimBundle write fSimBundle;
    property SimAppsPath: String read fSimAppsPath write fSimAppsPath;

    property DefaultSDK: String read fDefaultSDK write fDefaultSDK;

    property DefaultSimType: String read fDefaultSimType write fDefaultSimType; // it's currently Simulator via instruments
    property DefaultDeviceID: String read fDefaultDeviceID write fDefaultDeviceID;

    property DeviceCount: Integer read GetDeviceCount;
    property Device[i: integer]: TSimDevice read GetDevice;
  end;

function EnvOptions: TiPhoneEnvironmentOptions;
function ProjOptions: TiPhoneProjectOptions;


var
  iPhoneEnvGroup : Integer;
  iPhonePrjGroup : Integer;

function LazToXcodeProjFile(AProject: TLazProject): string;
function LazToXcodePlistFile(AProject: TLazProject): string;
procedure ReadUploadFilesList(pbx: PBXProject; lst: TStrings);

const
  Default_ScriptTemplate = 'def_buildscript.sh';

function DefaultScriptTemplateFileName(const packageDir: string): string;

implementation

function DefaultScriptTemplateFileName(const packageDir: string): string;
begin
  if packageDir <> '' then
    Result:=IncludeTrailingPathDelimiter(packageDir)+Default_ScriptTemplate
  else
    Result:='';
end;

var
  fEnvOptions   : TiPhoneEnvironmentOptions = nil;
  fProjOptions  : TiPhoneProjectOptions = nil;

const
  DefaultXMLName = 'iphoneextconfig.xml';

  optisIphone    = 'iPhone/isiPhoneApp';
  optSDK         = 'iPhone/SDK';
  optAppID       = 'iPhone/AppID';
  optSpaceName   = 'iPhone/SimSpaceName';
  optResourceDir = 'iPhone/ResourceDir';
  optExcludeMask = 'iPhone/ExcludeMask';
  optMainNib     = 'iPhone/MainNib';
  optResFiles    = 'iPhone/ResFiles';

function EnvOptions: TiPhoneEnvironmentOptions;
begin
  if not Assigned(fEnvOptions) then
    fEnvOptions:=TiPhoneEnvironmentOptions.Create;
  Result:=fEnvOptions;
end;

function ProjOptions: TiPhoneProjectOptions;
begin
  if not Assigned(fProjOptions) then
    fProjOptions:=TiPhoneProjectOptions.Create;
  Result:=fProjOptions;
end;

procedure FreeOptions;
begin
  fEnvOptions.Free;
end;

{ TiPhoneEnvironmentOptions }

class function TiPhoneEnvironmentOptions.GetGroupCaption: String;
begin
  Result:='iPhone Environment';
end;

class function TiPhoneEnvironmentOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=EnvOptions;
end;

function TiPhoneEnvironmentOptions.GetDevice(i: integer): TSimDevice;
begin
  if (i<0) and (i>=fDeviceList.Count) then Result:=nil
  else Result:=TSimDevice(fDeviceList[i]);
end;

function TiPhoneEnvironmentOptions.GetDeviceCount: Integer;
begin
  Result:=fDeviceList.Count;
end;

function TiPhoneEnvironmentOptions.XMLFileName: String;
begin
  Result:=IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath)+DefaultXMLName;
end;

procedure TiPhoneEnvironmentOptions.ClearVersionsInfo;
var
  i : Integer;
begin
  for i:=0 to fVersions.Count-1 do begin
    fVersions.Objects[i].Free;
    fVersions.Objects[i]:=nil;
  end;
  fVersions.Clear;
end;

procedure TiPhoneEnvironmentOptions.FoundSDK(const Version, DevSDKName,
  DevSDKPath, SimSDKName, SimSDKPath: String);
var
  info: TSDKInfo;
begin
  info:=TSDKInfo.Create;
  info.devName:=DevSDKName;
  info.devPath:=DevSDKPath;
  info.simName:=SimSDKName;
  info.simPath:=SimSDKPath;
  fVersions.AddObject(Version, info);
end;

function TiPhoneEnvironmentOptions.GetSDKInfo(const Version: String): TSDKInfo;
var
  i : Integer;
begin
  i:=fVersions.IndexOf(Version);
  if i<0 then Result:=nil
  else Result:=TSDKInfo(fVersions.Objects[i]);
end;

function GetDefaultPlatformPath: WideString;
begin
  result := '/Applications/Xcode.app/Contents/Developer/Platforms';
  if not DirectoryExistsUTF8(UTF8Encode(result)) then
    Result:='/Developer/Platforms';
end;

function GetDefaultSimBundlePath: WideString;
begin
  Result:='$(iOSPlatformsPath)' +
          'iPhoneSimulator.platform/Developer/Applications/iPhone Simulator.app';
end;

function GetDefaultSimAppPath: WideSTring;
begin
  Result:='$(home)'+
          'Library/Application Support/iPhone Simulator/$(iOSSDK)/Applications/';
end;

constructor TiPhoneEnvironmentOptions.Create;
begin
  inherited Create;
  fPlatformsBaseDir := UTF8Encode(GetDefaultPlatformPath);
  fSimAppsPath := UTF8Encode(GetDefaultSimAppPath);
  fSimBundle := UTF8Encode(GetDefaultSimBundlePath);
  fCompilerPath := '/usr/local/bin/fpc';
  fVersions:=TStringList.Create;
  fDeviceList:=TList.Create;
end;

destructor TiPhoneEnvironmentOptions.Destroy;
begin
  ClearVersionsInfo;
  fVersions.Free;
  DeviceListClear;
  fDeviceList.Free;
  inherited Destroy;
end;


function TiPhoneEnvironmentOptions.Load: Boolean;
var
  xmlcfg : TXMLConfig;
begin
  Result:=true;
  try
    xmlcfg := TXMLConfig.Create(nil);
    try
      xmlcfg.RootName:='config';
      xmlcfg.Filename:=XMLFileName;
      fPlatformsBaseDir := UTF8Encode(xmlcfg.GetValue('Platforms', UTF8Decode(fPlatformsBaseDir) ));
      fCompilerPath := UTF8Encode(xmlcfg.GetValue('Compiler', UTF8Decode(fCompilerPath)));
      fBaseRTLPath  := UTF8Encode(xmlcfg.GetValue('RTLPath', UTF8Decode(fBaseRTLPath)));
      fCommonOpt    := UTF8Encode(xmlcfg.GetValue('CompilerOptions', UTF8Decode(fCommonOpt)));
      fSimBundle    := UTF8Encode(xmlcfg.GetValue('SimBundle', UTF8Decode(fSimBundle)));
      fSimAppsPath  := UTF8Encode(xmlcfg.GetValue('SimAppPath', UTF8Decode(fSimAppsPath)));
      fDefaultSDK := UTF8Encode(xmlcfg.GetValue('DefaultSDK', UTF8Decode(fDefaultSDK)));
      fDefaultDeviceID := UTF8Encode(xmlcfg.GetValue('DefaultDevice', UTF8Decode(fDefaultDeviceID)));
      fScriptTemplate := UTF8Encode(xmlcfg.GetValue('ScriptTemplate', UTF8Decode(fScriptTemplate)));

      RefreshVersions;
      if (fDefaultSDK = '') and (fVersions.Count>0) then
        fDefaultSDK:=fVersions[0];
    finally
      xmlcfg.Free;
    end;
  except
    Result:=false;
  end;
end;

function TiPhoneEnvironmentOptions.Save: Boolean;
var
  xmlcfg : TXMLConfig;
begin
  Result:=true;
  try
    xmlcfg := TXMLConfig.Create(nil);
    try
      xmlcfg.RootName:='config';
      xmlcfg.Filename:=XMLFileName;
      xmlcfg.SetValue('Platforms', UTF8Decode(fPlatformsBaseDir));
      xmlcfg.SetValue('Compiler', UTF8Decode(fCompilerPath));
      xmlcfg.SetValue('RTLPath', UTF8Decode(fBaseRTLPath));
      xmlcfg.SetValue('CompilerOptions', UTF8Decode(fCommonOpt));
      xmlcfg.SetValue('SimBundle', UTF8Decode(fSimBundle));
      xmlcfg.SetValue('SimAppPath', UTF8Decode(fSimAppsPath));
      xmlcfg.SetValue('DefaultSDK', UTF8Decode(fDefaultSDK));
      xmlcfg.SetValue('DefaultDevice', UTF8Decode(fDefaultDeviceID));
      xmlcfg.SetValue('ScriptTemplate', UTF8Decode(fScriptTemplate));
      xmlcfg.Flush;
    finally
      xmlcfg.Free;
    end;
  except
    Result:=false;
  end;

end;

function TiPhoneEnvironmentOptions.GetSDKName(const SDKVer: String; simulator: Boolean): String;
var
  info : TSDKInfo;
begin
  info:=GetSDKInfo(SDKVer);
  if not Assigned(info) then Result:=''
  else begin
    if simulator then Result:=info.simName
    else Result:=info.devName;
  end;
end;

function TiPhoneEnvironmentOptions.GetSDKFullPath(const SDKVer: String; simulator: Boolean): String;
var
  info : TSDKInfo;
begin
  info:=GetSDKInfo(SDKVer);
  if not Assigned(info) then Result:=''
  else begin
    if simulator then Result:=info.simPath
    else Result:=info.devPath;
  end;
end;

function TiPhoneEnvironmentOptions.SubstituteMacros(var s: string): boolean;
begin
  s := StringReplace(s, '$(iOSPlatformsPath)', IncludeTrailingPathDelimiter(PlatformsBaseDir), [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '$(iOSSDK)', DefaultSDK, [rfReplaceAll, rfIgnoreCase]);
  result := IDEMacros.SubstituteMacros(s);
end;

procedure TiPhoneEnvironmentOptions.GetSDKVersions(Strings: TStrings);
var
  i : Integer;
begin
  for i:=0 to fVersions.Count-1 do
    Strings.Add( fVersions[i] );
end;

procedure TiPhoneEnvironmentOptions.RefreshVersions;
begin
  ClearVersionsInfo;
  ScanForSDK(EnvOptions.PlatformsBaseDir, @FoundSDK);
end;

procedure TiPhoneEnvironmentOptions.DeviceListClear;
var
  i : integer;
begin
  for i:=0 to fDeviceList.Count-1 do
    TObject(fDeviceList[i]).Free;
  fDeviceList.Clear;
end;

procedure TiPhoneEnvironmentOptions.DeviceListReload;
begin
  ListDevice(fDeviceList);
end;

{ TiPhoneProjectOptions }

procedure TiPhoneProjectOptions.Reset;
begin
  fisiPhone:=false;
  fSDK:='iPhone 2.0';
  fAppID:='com.mycompany.myapplication';
  fSpaceName:='';
  DataWritten:=false;
  fResFiles.Clear;
end;

constructor TiPhoneProjectOptions.Create;
begin
  inherited Create;
  fResFiles := TStringList.Create;
  Reset;
end;

destructor TiPhoneProjectOptions.Destroy;
begin
  fResFiles.Free;
  inherited Destroy;
end;

class function TiPhoneProjectOptions.GetGroupCaption: String;
begin
  Result:='iPhone';
end;

class function TiPhoneProjectOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=ProjOptions;
end;

procedure ReadUploadFilesList(pbx: PBXProject; lst: TStrings);
var
  trg  : PBXNativeTarget;
  i    : Integer;
  res  : PBXResourcesBuildPhase;
  bf   : PBXBuildFile;
  grp  : PBXGroup;
  p    : string;
begin
  if not Assigned(pbx) or not Assigned(lst) or not Assigned(pbx.targets)
    or (pbx.targets.Count=0) then Exit;
  trg:=PBXNativeTarget(pbx.targets.Items[0]);

  res := PBXResourcesBuildPhase(TargetFindBuildPhase(trg, PBXResourcesBuildPhase));
  if not Assigned(res) then begin
    writeln('failed');
    Exit;
  end;
  grp:=pbx.mainGroup.findGroup('Resources', true); // name must match to the target name!

  for i:=0 to res.files.Count-1 do begin
    bf:=PBXBuildFile(res.files[i]);
    // making sure that the file is one of "Resources" files
    p:=bf.fileRef.path;
    if Assigned(grp.findFileRefByPathName(p)) then
      lst.Add(p);
  end;
end;

procedure FileNamesRelateiveToProject(st: TStrings);
var
  i : integer;
  s : string;
begin
  //todo: it shouldn't really be hardcoded
  if not Assigned(st) then Exit;

  for i:=0 to st.Count-1 do begin
    s:=st[i];
    if s='' then Continue;
    if Pos('../',s)=1 then
      s:=Copy(s, 4, length(s));
    st[i]:=s;
  end;
end;

function TiPhoneProjectOptions.Load(AProject: TLazProject): Boolean;
var
  projname  : string;
  pbx       : PBXProject;
  st        : TStringList;
begin
  Result:=True;
  if not Assigned(AProject) then
    AProject:=LazarusIDE.ActiveProject;

  with AProject do begin
    DataWritten:=CustomData.Contains(optisIphone);
    fisiPhone:=(DataWritten) and (CustomData.Values[optisIphone] = 'true');

    if CustomData.Contains(optSDK) then fSDK:=CustomData.Values[optSDK];
    if CustomData.Contains(optAppID) then fAppID:=CustomData.Values[optAppID];
    fSpaceName:=CustomData.Values[optSpaceName];
    if fSpaceName='' then fSpaceName:=UTF8Encode(RandomSpaceName);
    if CustomData.Contains(optResourceDir) then fResourceDir:=CustomData.Values[optResourceDir]
    else fResourceDir:=DefaultResourceDir;
    if CustomData.Contains(optExcludeMask) then fExcludeMask:=CustomData.Values[optExcludeMask];
    if CustomData.Contains(optMainNib) then fMainNib:=CustomData.Values[optMainNib];

    projname:=LazToXcodeProjFile(AProject);
    if FileExists(projname) then begin
      ProjectLoadFromFile(projname, pbx);
      if Assigned(pbx) then begin
        st:=TStringList.Create;
        try
          ReadUploadFilesList(pbx, st);
          FileNamesRelateiveToProject(st);
          ResFiles.Assign(st);
        finally
          st.Free;
          pbx.Free;
        end;
      end;
    end;

    //todo: this must go away. the only intention is to cleanup the old code
    if CustomData.Contains(optResFiles) then begin
      CustomData.Remove(optResFiles);
    end;
  end;
end;

function TiPhoneProjectOptions.Save(AProject: TLazProject): Boolean;
const
  BoolStr : array[Boolean] of String = ('false', 'true');
var
  modflag: Boolean;
begin
  Result:=True;
  {do not write iPhone related info to non-iPhone projects}
  if DataWritten or fisiPhone then
    with LazarusIDE.ActiveProject do begin

      modflag:=false;
      modflag:=(CustomData.Values[optisIPhone] <> BoolStr[fisiPhone])
             or (CustomData.Values[optSDK]<>fSDK)
             or (CustomData.Values[optAppID]<>fAppID)
             or (CustomData.Values[optSpaceName]<>fSpaceName)
             or (CustomData.Values[optResourceDir]<>fResourceDir)
             or (CustomData.Values[optExcludeMask]<>fExcludeMask)
             or (CustomData.Values[optMainNib]<>fMainNib)
             or (CustomData.Values[optResFiles]<>ResFiles.Text);

      if modflag then begin
        LazarusIDE.ActiveProject.Modified:=true;
        CustomData.Values[optisIPhone] := BoolStr[fisiPhone];
        CustomData.Values[optSDK]:=fSDK;
        CustomData.Values[optAppID]:=fAppID;
        CustomData.Values[optSpaceName]:=fSpaceName;
        CustomData.Values[optResourceDir]:=fResourceDir;
        CustomData.Values[optExcludeMask]:=fExcludeMask;
        CustomData.Values[optMainNib]:=fMainNib;
        CustomData.Values[optResFiles]:=ResFiles.Text;
      end;
    end;
end;

function LazToXcodeProjFile(AProject: TLazProject): string;
begin
  if not Assigned(AProject) then
    AProject:=LazarusIDE.ActiveProject;

  Result:=
        IncludeTrailingPathDelimiter( ResolveProjectPath('xcode', AProject) )
      + ChangeFileExt( ExtractFileName(LazarusIDE.ActiveProject.MainFile.Filename),'.xcodeproj')
      + PathDelim
      +'project.pbxproj';
end;

function LazToXcodePlistFile(AProject: TLazProject): string;
begin
  if not Assigned(AProject) then
    AProject:=LazarusIDE.ActiveProject;
  Result:=
        IncludeTrailingPathDelimiter( ResolveProjectPath('xcode', AProject) )
        +'info.plist';
end;

initialization

finalization
  FreeOptions;

end.

