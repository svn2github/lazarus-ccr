unit xcodeproj;
{--------------------------------------------------------------------------------
* by Dmitry Boyarintsev - Oct 2014                                              *
*                                                                               *
* license: free for use, but please leave a note to the origin of the library   *
*                                                                               *
* PBXcontainer unit is a library to read/write the pbx formatter file as a      *
* whole The file structure is made to keep the reference in a complex objects   *
*                                                                               *
* Memory Management.                                                            *
*   Cocoa is reference - counted library, thus the usage of an object           *
*   controlled natively by ref counting.                                        *
* A bit trickier for pascal                                                     *
* Following rules are applied                                                   *
*   * read-only property objects are freed by the host (obviously)              *
*   * other "freeing" operations are noted at "mmgr" comments                   *
*   * any objects within key-value tables are freed                             *
* Alternative solution - implement ref counting!                                *
*                                                                               *
* PBXShellScriptBuildPhase - the give script must be using MacOS line breaks    *
*   which are \n (#13). Using unix line breaks \r (#10) will cause issues       *
*   in Xcode.                                                                   *
--------------------------------------------------------------------------------}

interface

{$ifdef fpc}{$mode delphi}{$endif}

uses
  Classes, SysUtils,
  typinfo, pbxcontainer;

type
  { XCBuildConfiguration }

  XCBuildConfiguration = class(PBXObject)
  private
    fname : string;
    fbuildSettings: TPBXKeyValue;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property buildSettings : TPBXKeyValue read fbuildSettings;
    property name: string read fname write fname;
  end;

  { XCConfigurationList }

  // mmgr: XCConfigurationList frees
  //  * content of buildConfigurations
  XCConfigurationList = class(PBXObject)
  private
    fdefaultConfigurationIsVisible: string;
    fdefaultConfigurationName: string;
    fbuildConfigurations: TPBXObjectsList;

    function GetConfigItem(i: integer): XCBuildConfiguration;
    function GetCount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function addConfig(const aname: string): XCBuildConfiguration;
    function findConfig(const aname: string; aforce: Boolean = false): XCBuildConfiguration;
    // Count and Items are just for convenience. MUST NOT BE in "published" section
    property Count: integer read GetCount;
    property Items[i: integer]: XCBuildConfiguration read GetConfigItem; default;
  published
    property buildConfigurations: TPBXObjectsList read fbuildConfigurations;
	  property defaultConfigurationIsVisible: string read fdefaultConfigurationIsVisible write fdefaultConfigurationIsVisible;
		property defaultConfigurationName: string read fdefaultConfigurationName write fdefaultConfigurationName;
  end;

  { PBXContainerItemProxy }

  PBXContainerItemProxy = class(PBXObject)
  private
    FcontainerPortal : PBXObject;
    fproxyType : string;
    fremoteGlobalIDString: string;
    fremoteInfo: string;
  published
	  property containerPortal: PBXObject read fcontainerPortal write fcontainerPortal; // Object = 0AFA6EA519F60EFD004C8FD9 /* Project object */;
		property proxyType: string read FproxyType write fproxyType;
		property remoteGlobalIDString: string read fremoteGlobalIDString write fremoteGlobalIDString;   //object = 0AFA6EAC19F60EFD004C8FD9;
		property remoteInfo : string read fremoteInfo write fremoteInfo; // ttestGame;
  end;

  { PBXFileReference }

  // these files might not be physically present in for the project.
  // i.e. a bundle .app file doesn't physically exists until it's actual "built" by the xcode
  PBXFileReference = class(PBXObject)
  private
    FexplicitFileType: string;
    FincludeInIndex: Boolean;
    FlastKnownFileType: string;
    Fname: string;
    Fpath: string;
    FsourceTree: string;
  published
    property explicitFileType: string read FexplicitFileType write FexplicitFileType;
    property includeInIndex: Boolean read FincludeInIndex write FincludeInIndex;
    property lastKnownFileType: string read flastKnownFileType write flastKnownFileType;
    property name: string read Fname write Fname;
    property path: string read Fpath write Fpath;
    property sourceTree: string read FsourceTree write FsourceTree;
  end;

  { PBXBuildFile }

  PBXBuildFile = class(PBXObject)
  private
    fFileRef  : PBXFileReference;
  published
    property fileRef : PBXFileReference read ffileRef write ffileRef; // obj
  end;

  { PBXBuildPhase }

  // mmgr: on free
  //       * content of files
  PBXBuildPhase = class(PBXObject)
  private
    fbuildActionMask : Integer;
    ffiles: TPBXObjectsList;
    frunOnlyForDeploymentPostprocessing: Boolean;
    fname: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddFile(ref: PBXFileReference): PBXBuildFile;
  published
	  property buildActionMask: Integer read fbuildActionMask write fbuildActionMask;
    property files: TPBXObjectsList read ffiles;
    property name: string read fname write fname;
    property runOnlyForDeploymentPostprocessing: Boolean read frunOnlyForDeploymentPostprocessing write frunOnlyForDeploymentPostprocessing;
  end;
  { PBXFrameworksBuildPhase }

  PBXFrameworksBuildPhase = class(PBXBuildPhase);
	PBXResourcesBuildPhase = class(PBXBuildPhase);
  PBXSourcesBuildPhase = class(PBXBuildPhase);

  PBXBuildPhaseClass = class of PBXBuildPhase;

  { PBXShellScriptBuildPhase }

  PBXShellScriptBuildPhase = class(PBXBuildPhase)
  private
    finputPaths: TPBXStringArray;
    foutputPaths: TPBXStringArray;
    fshellpath: string;
    fshellScript: string;
    fshowEnvVarsInLog: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property inputPaths: TPBXStringArray read finputPaths;
    property outputPaths: TPBXStringArray read foutputPaths;
    property shellPath: string read fshellpath write fshellPath;
    property shellScript: string read fshellScript write fshellScript;
    property showEnvVarsInLog: Boolean read fshowEnvVarsInLog write fshowEnvVarsInLog default true;
  end;

  { PBXGroup }

  // mmgt: PBXGroup owns children object (PBXGroup and PBXFileRefernece)
  //       and would free then on release;
  //       note, that PBXFileReference objects might be used in other places
  PBXGroup = class(PBXObject)
  private
    fsourceTree : string;
    fchildren: TPBXObjectsList;
    fname: string;
    fpath: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function addSubGroup(const aname: string): PBXGroup;
    function findGroup(const aname: string; aforce: Boolean = false): PBXGroup;
    function findFileRefByPathName(const afilename: string): PBXFileReference;
  published
    property children: TPBXObjectsList read fchildren;
    property name: string read fname write fname;
    property path: string read fpath write fpath;
  	property sourceTree: string read fsourceTree write fsourceTree;
  end;

  PBXVariantGroup = class(PBXGroup);

  { PBXNativeTarget }

  // mmgr: PBXNativeTarget
  //   * buildConfigurationList
  //   * contents of buildPhases
  //   * contents of buildRules
  //   * content of dependencies
  PBXNativeTarget = class(PBXObject)
  private
    fbuildConfigurationList: XCConfigurationList;
    fname         : string;
    fproductName  : string;
    fproductReference : PBXObject;
    fproductType  : string;
    fbuildPhases  : TPBXObjectsList;
    fbuildRules   : TPBXObjectsList;
    fdependencies : TPBXObjectsList;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
		property buildConfigurationList : XCConfigurationList  read fbuildConfigurationList write fbuildConfigurationList; //= 0AFA6ED419F60F01004C8FD9 /* Build configuration list for PBXNativeTarget "ttestGame" */;
    property buildPhases: TPBXObjectsList read fbuildPhases;
    property buildRules: TPBXObjectsList read fbuildRules;
    property dependencies: TPBXObjectsList read fdependencies;
		property name: string read fname write fname;
		property productName: string read fproductName write fproductName; // = ttestGame;
		property productReference: PBXObject read fproductReference write fproductReference;   // producut resulting file
		property productType: string read fproductType write fproductType; // = "com.apple.product-type.application";
	end;

  { PBXLegacyTarget }

  PBXLegacyTarget = class(PBXObject)
  private
    fpassBuildSettingsInEnvironment : Boolean;
    fbuildArgumentsString  : string;
    fbuildToolPath         : string;
    fbuildWorkingDirectory : string;
  public
    constructor Create; override;
  published
    property buildArgumentsString: string read fbuildArgumentsString write fbuildArgumentsString;
    property buildToolPath: string read fbuildToolPath write fbuildToolPath;
    property buildWorkingDirectory: string read fbuildWorkingDirectory write fbuildWorkingDirectory;
    property passBuildSettingsInEnvironment: Boolean read fpassBuildSettingsInEnvironment write fpassBuildSettingsInEnvironment;
  end;

  { PBXTargetDependency }

  // mmgt:
  //   targetProxy - is freed
  PBXTargetDependency = class(PBXObject)
  private
    ftargetProxy: PBXContainerItemProxy;
    ftarget: PBXNativeTarget;
  public
    destructor Destroy; override;
  published
	  property target : PBXNativeTarget read ftarget write ftarget;
		property targetProxy: PBXContainerItemProxy read ftargetProxy write ftargetProxy; {* PBXContainerItemProxy *}
  end;

  { PBXProject }

  // mmgt: PBXProject frees the following property objects, if assigned:
  //       * mainGroup
  //       * buildConfigurationList
  //       * contents of targets
  PBXProject = class(PBXObject)
  private
    fattributes : TPBXKeyValue;
    fcompatibilityVersion : string;
    fdevelopmentRegion : string;
    fhasScannedForEncodings: Boolean;
    fmainGroup: PBXGroup;
    fknownRegions: TPBXStringArray;
    fproductRefGroup: PBXGroup;
    fprojectDirPath: string;
    fprojectRoot : string;
    ftargets: TPBXObjectsList;
    fbuildConfigurationList: XCConfigurationList;
  protected
    class procedure _WriteEmpty(propnames: TStrings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function addTarget(const aname: string): PBXNativeTarget;
  published
    property attributes: TPBXKeyValue read fattributes;
    property buildConfigurationList: XCConfigurationList read fbuildConfigurationList write fbuildConfigurationList;
    property compatibilityVersion: string read fcompatibilityVersion write fcompatibilityVersion;
    property developmentRegion: string read fdevelopmentRegion write fdevelopmentRegion;
    property hasScannedForEncodings: Boolean read fhasScannedForEncodings write fhasScannedForEncodings;
    property knownRegions: TPBXStringArray read fknownRegions;
    property mainGroup: PBXGroup read fmainGroup write fmainGroup;
    property productRefGroup: PBXGroup read fproductRefGroup write fproductRefGroup;
    property projectDirPath: string read fprojectDirPath write fprojectDirPath;
    property projectRoot: string read fprojectRoot write fprojectRoot;
    property targets: TPBXObjectsList read ftargets;
  end;

function ProjectLoadFromStream(st: TStream; var prj: PBXProject): Boolean;
function ProjectLoadFromFile(const fn: string; var prj: PBXProject): Boolean;
function ProjectSaveToFile(prj: PBXProject; const fn: string): Boolean;

// serializes a PBX project to the string, using PBXContainer structure
function ProjectWrite(prj: PBXProject): string;

// creates a minimum possible project
function ProjectCreateMin: PBXProject;

// creates main group and product groups
procedure ProjectDefaultGroups(prj: PBXProject);

// adds necessary flags for Xcode3.2 not to throw any warnings
procedure ProjectUpdateForXcode3_2(prj: PBXProject);
// creates a minimum project, defaults the structure and sets Xcode 3.2 compat flags
function ProjectCreate3_2: PBXProject;

function ProjectAddTarget(prj: PBXProject; const ATargetName: string): PBXNativeTarget;
// adds if doesn't exist
function ProjectForceTarget(prj: PBXProject; const ATargetName: string): PBXNativeTarget;

const
  SCRIPT_RUNPATH = '/bin/sh';
  SCRIPT_DEFAULT = '';
  SCRIPT_DEFNAME = 'Run Script';

function TargetAddRunScript(atarget: PBXNativeTarget): PBXShellScriptBuildPhase;
function TargetFindBuildPhase(atarget: PBXNativeTarget; aclass: PBXBuildPhaseClass; aforce: Boolean = false): PBXBuildPhase;
function TargetFindRunScript(atarget: PBXNativeTarget; aforce: Boolean = false): PBXShellScriptBuildPhase;
function TargetFindResources(atarget: PBXNativeTarget; aforce: Boolean = false): PBXResourcesBuildPhase;

const
  //FILETYPE_SCRIPT = 'text.script.sh';
  FILETYPE_EXEC   = 'compiled.mach-o.executable';
  FILETYPE_MACHO  = FILETYPE_EXEC;
  FILETYPE_BUNDLE = 'wrapper.application';
  FILETYPE_PLIST  = 'text.plist.xml';
  FILETYPE_OBJC   = 'sourcecode.c.objc';

// defaults to SRCTREE_GROUP for source related
function FileRefCreate(const afilename: string; const filetype: string = ''): PBXFileReference; overload;
function FileRefCreate(const afilename: string; const filetype, ASrcTreeRel: string): PBXFileReference; overload;

const
  SRCTREE_ABSOLUTE = '<absolute>';           // path is absolute path
  SRCTREE_GROUP    = '<group>';              // path is relative to the parent group
  SRCTREE_PRODUCT  = 'BUILT_PRODUCTS_DIR';   // path is relative to the product build directory
  SRCTREE_PROJECT  = 'SOURCE_ROOT';          // path is relative for .xcodeproj directory location
  SRCTREE_DEV      = 'DEVELOPER_DIR';        // path is relative to developer dir
  SRCTREE_SDK      = 'SDKROOT';              // path is relative to selected SDK dir

function GroupCreate(const aname: string; const srcTree: string = SRCTREE_GROUP): PBXGroup;
//todo: need a rountine to update the path whenever the project is saved
function GroupCreateRoot(const projectfolder: string = ''): PBXGroup;

const
  PRODTYPE_TOOL = 'com.apple.product-type.tool';
  PRODTYPE_APP  = 'com.apple.product-type.application'; // use it for OSX / iOS app targets
    // prodtype of app type should have productReference to the result bundle! (.app) file

//
// PBXSourcesBuildPhase (sources) - is part of a PBXNativeTarget
// PBXNativeTarget - is part of Target
// PBXNativeTarget
//buildPhases = (
			//0AA67B651A04929900CF0DD7 /* Sources */,
			//0AA67B661A04929900CF0DD7 /* Frameworks */,
			//0AA67B671A04929900CF0DD7 /* CopyFiles */,
		//);

const
  TARGET_IOS_8_0 = '8.0';
  TARGET_IOS_8_1 = '8.1';

procedure ConfigIOS(cfg: XCBuildConfiguration; const targetiOS: string);

const
  CFG_SDKROOT = 'SDKROOT';
  CFG_IOSTRG  = 'IPHONEOS_DEPLOYMENT_TARGET';
  CFG_DEVICE  = 'TARGET_DEVICE_FAMILY';
  CFG_DEVICE_ALL    = '1,2';
  CFG_DEVICE_IPHONE = '1';
  CFG_DEVICE_IPAD   = '2';

procedure SetNewStr(kv: TPBXKeyValue; const aname, avalue: string);

implementation

procedure SetNewStr(kv: TPBXKeyValue; const aname, avalue: string);
begin
  if Assigned(kv) then begin
    if kv.FindIndexOf(aname)<0 then
      kv.AddStr(aname, avalue);
  end;
end;

procedure ConfigIOS(cfg: XCBuildConfiguration; const targetiOS: string);
begin
  if not Assigned(cfg) then Exit;

  SetNewStr(cfg.buildSettings, CFG_IOSTRG, targetiOS);
  SetNewStr(cfg.buildSettings, CFG_SDKROOT, 'iphoneos');
  SetNewStr(cfg.buildSettings, CFG_DEVICE, CFG_DEVICE_ALL);
end;

{ PBXLegacyTarget }

constructor PBXLegacyTarget.Create;
begin
  inherited Create;
  fpassBuildSettingsInEnvironment:=true;
end;

{ PBXTargetDependency }

destructor PBXTargetDependency.Destroy;
begin
  ftargetProxy.Free;
  inherited Destroy;
end;

{ PBXShellScriptBuildPhase }

constructor PBXShellScriptBuildPhase.Create;
begin
  inherited Create;
  finputPaths:=TPBXStringArray.Create;
  foutputPaths:=TPBXStringArray.Create;
end;

destructor PBXShellScriptBuildPhase.Destroy;
begin
  finputPaths.Free;
  foutputPaths.Free;
  inherited Destroy;
end;

{ PBXNativeTarget }

constructor PBXNativeTarget.Create;
begin
  inherited Create;
  fbuildPhases := TPBXObjectsList.Create(true);
  fdependencies := TPBXObjectsList.Create(true);
  fbuildRules   := TPBXObjectsList.Create(true);
end;

destructor PBXNativeTarget.Destroy;
begin
  fbuildConfigurationList.Free;
  fbuildRules.Free;
  fbuildPhases.Free;
  fdependencies.Free;
  inherited Destroy;
end;

{ PBXProject }

class procedure PBXProject._WriteEmpty(propnames: TStrings);
begin
  propnames.Add('projectDirPath');
  propnames.Add('projectRoot');
end;

constructor PBXProject.Create;
begin
  inherited Create;
  ftargets:=TPBXObjectsList.create(true);
  fknownRegions:=TPBXStringArray.Create;
  fattributes:=TPBXKeyValue.Create(true);
end;

destructor PBXProject.Destroy;
begin
  fattributes.Free;
  fknownRegions.Free;
  ftargets.Free;

  fmainGroup.Free;
  fbuildConfigurationList.Free;
  inherited Destroy;
end;

function PBXProject.addTarget(const aname: string): PBXNativeTarget;
begin
  Result:=PBXNativeTarget.Create;
  targets.Add(Result);
  Result._headerComment:=aname;
  Result.name:=aname;
  Result.productName:=aname;
end;

{ XCConfigurationList }

function XCConfigurationList.GetConfigItem(i: integer): XCBuildConfiguration;
begin
  if (i<0) or (i>=fbuildConfigurations.Count) then Result:=nil
  else Result:=XCBuildConfiguration(fbuildConfigurations[i]);
end;

function XCConfigurationList.GetCount: integer;
begin
  Result:=fbuildConfigurations.Count;
end;

constructor XCConfigurationList.Create;
begin
  inherited Create;
  fbuildConfigurations:=TPBXObjectsList.Create(true);
end;

destructor XCConfigurationList.Destroy;
begin
  fbuildConfigurations.Free;
  inherited Destroy;
end;

function XCConfigurationList.addConfig(const aname: string
  ): XCBuildConfiguration;
begin
  Result:=XCBuildConfiguration.Create;
  Result.name:=aname;
  Result._headerComment:=aname;
  fbuildConfigurations.Add(Result);
end;

function XCConfigurationList.findConfig(const aname: string; aforce: Boolean): XCBuildConfiguration;
var
  i : integer;
begin
  for i:=0 to fbuildConfigurations.Count-1 do begin
    Result:=XCBuildConfiguration(fbuildConfigurations[i]);
    if Result.name=aname then
      Exit;
  end;
  if aforce then
    Result:=addConfig(aname)
  else
    Result:=nil;
end;

{ XCBuildConfiguration }

constructor XCBuildConfiguration.Create;
begin
  inherited Create;
  fbuildSettings:=TPBXKeyValue.Create(true);
end;

destructor XCBuildConfiguration.Destroy;
begin
  fbuildSettings.Free;
  inherited Destroy;
end;

{ PBXGroup }

constructor PBXGroup.Create;
begin
  inherited Create;
  fchildren:=TPBXObjectsList.Create(true);
end;

destructor PBXGroup.Destroy;
begin
  fchildren.Free;
  inherited Destroy;
end;

function PBXGroup.addSubGroup(const aname: string): PBXGroup;
begin
  Result:=PBXGroup.Create;
  fchildren.Add(Result);
  Result.name:=aname;
  Result._headerComment:=aname;
  Result.sourceTree:=SRCTREE_GROUP;
end;

function PBXGroup.findGroup(const aname: string; aforce: Boolean = false): PBXGroup;
var
  i   : integer;
  obj : TObject;
begin
  for i:=0 to fchildren.Count-1 do begin
    obj:=fchildren[i];
    if (obj is PBXGroup) and (PBXGroup(obj).name=aname) then begin
      Result:=PBXGroup(obj);
      Exit;
    end;
  end;
  if aforce then
    Result:=addSubGroup(aname)
  else
    Result:=nil;
end;

function PBXGroup.findFileRefByPathName(const afilename: string): PBXFileReference;
var
  i   : integer;
  obj : TObject;
begin
  for i:=0 to fchildren.Count-1 do begin
    obj:=fchildren[i];
    if (obj is PBXFileReference) and (PBXFileReference(obj).path=afilename) then begin
      Result:=PBXFileReference(obj);
      Exit;
    end;
  end;
  Result:=nil;
end;

{ PBXBuildPhase }

constructor PBXBuildPhase.Create;
begin
  inherited Create;
  ffiles:=TPBXObjectsList.Create(true);
end;

destructor PBXBuildPhase.Destroy;
begin
  ffiles.Free;
end;

function PBXBuildPhase.AddFile(ref: PBXFileReference): PBXBuildFile;
begin
  if not Assigned(ref) then begin
    Result:=nil;
    Exit;
  end;
  Result := PBXBuildFile.Create;
  Result.fileRef:=ref;
  ffiles.Add(Result);
end;

function ProjectLoadFromStream(st: TStream; var prj: PBXProject): Boolean;
var
  c : TPBXContainer;
  info : TPBXFileInfo;
begin
  prj:=nil;
  c:= TPBXContainer.Create;
  try
    Result:=c.ReadFile(st, info);
    if Result then begin
      if not (info.rootObject is PBXProject) then begin
        info.rootObject.Free;
      end else begin
        prj:=PBXProject(info.rootObject);
      end;
    end
  finally
    c.Free;
  end;
end;

function ProjectLoadFromFile(const fn: string; var prj: PBXProject): Boolean;
var
  fs :TFileStream;
begin
  try
    prj:=nil;
    fs:=TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    try
      Result:=ProjectLoadFromStream(fs, prj);
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;
end;

function ProjectSaveToFile(prj: PBXProject; const fn: string): Boolean;
var
  fs : TFileStream;
  s  : string;
begin
  s:=ProjectWrite(prj);
  try
    fs:=TFileStream.Create(fn, fmCreate);
    try
      if length(s)>0 then fs.Write(s[1], length(s));
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;

end;

function ProjectWrite(prj: PBXProject): string;
var
  info : TPBXFileInfo;
begin
  info.archiveVersion:='1';
  info.objectVersion:='46';
  info.rootObject:=prj;
  Result:=PBXWriteContainer(info);
end;

function ProjectCreateMin: PBXProject;
var
  p : PBXProject;
  //cfg : XCBuildConfiguration;
begin
  // requirements:
  //  * at least one build configuration
  p := PBXProject.Create;
  p._headerComment:='Project object';

  p.buildConfigurationList:=XCConfigurationList.Create;
  p.buildConfigurationList._headerComment:='Build configuration list for PBXProject';
  p.buildConfigurationList.defaultConfigurationIsVisible:='0';

  p.buildConfigurationList.addConfig('Debug');
  p.buildConfigurationList.addConfig('Release');
  // default name must be present
  p.buildConfigurationList.defaultConfigurationName:='Release';
  Result:=p;
end;

procedure ProjectDefaultGroups(prj: PBXProject);
var
  prd : PBXGroup;
begin
  if not Assigned(prj) then Exit;
  if not Assigned(prj.mainGroup) then
    prj.mainGroup:=GroupCreateRoot;

  if not Assigned(prj.productRefGroup) then begin
    prd:=prj.mainGroup.findGroup('Products');
    if not Assigned(prd) then
      prd:=prj.mainGroup.addSubGroup('Products');
    prj.productRefGroup:=prd;
  end;
end;

procedure ProjectUpdateForXcode3_2(prj: PBXProject);
begin
  if not Assigned(prj) then Exit;
  // without the attribute Xcode complains aboutt updating settings
  prj.attributes.AddStr('LastUpgradeCheck','0600');
  prj.compatibilityVersion:='Xcode 3.2';
end;

function ProjectCreate3_2: PBXProject;
begin
  Result:=ProjectCreateMin;
  ProjectDefaultGroups(Result);
  ProjectUpdateForXcode3_2(Result);
end;

function ProjectAddTarget(prj: PBXProject; const ATargetName: string): PBXNativeTarget;
begin
  Result:=nil;
  if not Assigned(prj) then Exit;
  Result:=prj.addTarget(ATargetName);
end;

function ProjectForceTarget(prj: PBXProject; const ATargetName: string): PBXNativeTarget;
var
  i : integer;
begin
  for i:=0 to prj.targets.Count-1 do begin
    Result:=PBXNativeTarget(prj.targets[i]);
    if Result.name=ATargetName then
      Exit;
  end;
  Result:=ProjectAddTarget(prj, ATargetName);
end;

function TargetFindBuildPhase(atarget: PBXNativeTarget; aclass: PBXBuildPhaseClass; aforce: Boolean = false): PBXBuildPhase;
var
  i : integer;
begin
  Result:=nil;
  if not Assigned(atarget) then Exit;
  for i:=0 to atarget.buildPhases.Count-1 do
    if atarget.buildPhases[i] is aclass then begin
      Result:=PBXBuildPhase(atarget.buildPhases[i]);
      Break;
    end;
  if not Assigned(Result) and aforce then begin
    Result:=aclass.Create;
    atarget.buildPhases.Add(Result);
  end;
end;

function TargetFindResources(atarget: PBXNativeTarget; aforce: Boolean = false): PBXResourcesBuildPhase;
begin
  Result:=PBXResourcesBuildPhase(TargetFindBuildPhase(atarget, PBXResourcesBuildPhase, aforce));
end;

function TargetFindRunScript(atarget: PBXNativeTarget; aforce: Boolean = false): PBXShellScriptBuildPhase;
begin
  Result:=PBXShellScriptBuildPhase(TargetFindBuildPhase(atarget, PBXShellScriptBuildPhase, false));
  if not Assigned(Result) and aforce then
    Result:=TargetAddRunScript(atarget);
end;

function TargetAddRunScript(atarget: PBXNativeTarget): PBXShellScriptBuildPhase;
begin
  if not Assigned(atarget) then begin
    Result:=nil;
    Exit;
  end;
  Result:=PBXShellScriptBuildPhase.Create;
  Result.name:=SCRIPT_DEFNAME;
  Result._headerComment:=SCRIPT_DEFNAME;
  Result.shellScript:=SCRIPT_DEFAULT;
  Result.shellPath:=SCRIPT_RUNPATH;
  atarget.buildPhases.Add(Result);
end;

function FileRefCreate(const afilename: string; const filetype: string ): PBXFileReference;
begin
  Result:=FileRefCreate(afilename, filetype, SRCTREE_GROUP);
end;

function FileRefCreate(const afilename: string; const filetype, ASrcTreeRel: string): PBXFileReference;
begin
  Result:=PBXFileReference.Create;
  Result.path:=afilename;
  Result._headerComment:=afilename;
  Result.explicitFileType:=FILETYPE_EXEC;
  Result.sourceTree:=ASrcTreeRel;
end;

function GroupCreate(const aname, srcTree: string): PBXGroup;
begin
  Result:=PBXGroup.Create;
  Result.name:=aname;
  Result.sourceTree:=srcTree;
  Result._headerComment:=aname;
end;

function GroupCreateRoot(const projectfolder: string): PBXGroup;
begin
  Result:=GroupCreate('', SRCTREE_GROUP);
  Result.path:=projectfolder;
  Result._headerComment:=projectfolder;
end;

initialization
  PBXRegisterClass(PBXBuildFile);
  PBXRegisterClass(PBXContainerItemProxy);
  PBXRegisterClass(PBXFileReference);
  PBXRegisterClass(PBXFrameworksBuildPhase);
  PBXRegisterClass(PBXGroup);
  PBXRegisterClass(PBXNativeTarget);
  PBXRegisterClass(PBXProject);
  PBXRegisterClass(PBXResourcesBuildPhase);
  PBXRegisterClass(PBXSourcesBuildPhase);
  PBXRegisterClass(PBXTargetDependency);
  PBXRegisterClass(PBXVariantGroup);
  PBXRegisterClass(XCBuildConfiguration);
  PBXRegisterClass(XCConfigurationList);
  PBXRegisterClass(PBXShellScriptBuildPhase);

end.



