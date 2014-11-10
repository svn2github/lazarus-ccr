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
--------------------------------------------------------------------------------}

interface

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
  public
    constructor Create; override;
    destructor Destroy; override;
    function addConfig(const aname: string): XCBuildConfiguration;
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

  PBXFileReference = class(PBXObject)
  private
    FexplicitFileType: string;
    FincludeInIndex: string;
    FlastKnownFileType: string;
    Fname: string;
    Fpath: string;
    FsourceTree: string;
  published
    property explicitFileType: string read FexplicitFileType write FexplicitFileType;
    property includeInIndex: string read FincludeInIndex write FincludeInIndex;
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
    frunOnlyForDeploymentPostprocessing: string;
    fname: string;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
	  property buildActionMask: Integer read fbuildActionMask write fbuildActionMask;
    property files: TPBXObjectsList read ffiles;
    property name: string read fname write fname;
    property runOnlyForDeploymentPostprocessing: string read frunOnlyForDeploymentPostprocessing write frunOnlyForDeploymentPostprocessing;
  end;
  { PBXFrameworksBuildPhase }

  PBXFrameworksBuildPhase = class(PBXBuildPhase);
	PBXResourcesBuildPhase = class(PBXBuildPhase);
  PBXSourcesBuildPhase = class(PBXBuildPhase);

  { PBXShellScriptBuildPhase }

  PBXShellScriptBuildPhase = class(PBXBuildPhase)
  private
    finputPaths: TPBXStringArray;
    foutputPaths: TPBXStringArray;
    fshellpath: string;
    fshellScript: string;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property inputPaths: TPBXStringArray read finputPaths;
    property outputPaths: TPBXStringArray read foutputPaths;
    property shellPath: string read fshellpath write fshellPath;
    property shellScript: string read fshellScript write fshellScript;
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
		property productReference: PBXObject read fproductReference write fproductReference; // = 0AFA6EAD19F60EFE004C8FD9 /* ttestGame.app */;
		property productType: string read fproductType write fproductType; // = "com.apple.product-type.application";
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
    fhasScannedForEncodings: string;
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
    property hasScannedForEncodings: string read fhasScannedForEncodings write fhasScannedForEncodings;
    property knownRegions: TPBXStringArray read fknownRegions;
    property mainGroup: PBXGroup read fmainGroup write fmainGroup;
    property productRefGroup: PBXGroup read fproductRefGroup write fproductRefGroup;
    property projectDirPath: string read fprojectDirPath write fprojectDirPath;
    property projectRoot: string read fprojectRoot write fprojectRoot;
    property targets: TPBXObjectsList read ftargets;
  end;

function LoadProjectFromStream(st: TStream; var prj: PBXProject): Boolean;
function LoadProjectFromFile(const fn: string; var prj: PBXProject): Boolean;

function ProjectWrite(prj: PBXProject): string;

function CreateMinProject: PBXProject;
function ProjectAddTarget(prj: PBXProject; const ATargetName: string): PBXNativeTarget;

const
  SCRIPT_RUNPATH = '/bin/sh';
  SCRIPT_DEFAULT = '';
  SCRIPT_DEFNAME = 'Run Script';

function TargetAddRunScript(atarget: PBXNativeTarget): PBXShellScriptBuildPhase;

const
  //FILETYPE_SCRIPT = 'text.script.sh';
  FILETYPE_EXEC   = 'compiled.mach-o.executable';
  FILETYPE_MACHO  = FILETYPE_EXEC;

function CreateFileRef(const afilename: string; const filetype: string = ''): PBXFileReference;

const
  GROUPSRC_ABSOLUTE = '<absolute>';
  GROUPSRC_GROUP    = '<group>';

function CreateGroup(const aname: string; const srcTree: string = GROUPSRC_GROUP): PBXGroup;
//todo: need a rountine to update the path whenever the project is saved
function CreateRootGroup(const projectfolder: string): PBXGroup;

const
  PRODTYPE_TOOL = 'com.apple.product-type.tool';

//
// PBXSourcesBuildPhase (sources) - is part of a PBXNativeTarget
// PBXNativeTarget - is part of Target
// PBXNativeTarget
//buildPhases = (
			//0AA67B651A04929900CF0DD7 /* Sources */,
			//0AA67B661A04929900CF0DD7 /* Frameworks */,
			//0AA67B671A04929900CF0DD7 /* CopyFiles */,
		//);


implementation

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
  // productName?
  // productReference - is a resulting file
end;

{ XCConfigurationList }

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

function XCConfigurationList.AddConfig(const aname: string): XCBuildConfiguration;
begin
  Result:=XCBuildConfiguration.Create;
  Result.name:=aname;
  Result._headerComment:=aname;
  fbuildConfigurations.Add(Result);
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
  Result.sourceTree:=GROUPSRC_GROUP;
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

function LoadProjectFromStream(st: TStream; var prj: PBXProject): Boolean;
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

function LoadProjectFromFile(const fn: string; var prj: PBXProject): Boolean;
var
  fs :TFileStream;
begin
  fs:=TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    Result:=LoadProjectFromStream(fs, prj);
  finally
    fs.Free;
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

function CreateMinProject: PBXProject;
var
  p : PBXProject;
  cfg : XCBuildConfiguration;
begin
  // requirements:
  //  * at least one build configuration
  p := PBXProject.Create;
  p._headerComment:='Project object';

  p.buildConfigurationList:=XCConfigurationList.Create;
  p.buildConfigurationList._headerComment:='Build configuration list';
  p.buildConfigurationList.defaultConfigurationIsVisible:='0';

  cfg:=p.buildConfigurationList.addConfig('Default');
  cfg:=p.buildConfigurationList.addConfig('Release');
  // default name must be present
  p.buildConfigurationList.defaultConfigurationName:='Release';
  Result:=p;
end;

function ProjectAddTarget(prj: PBXProject; const ATargetName: string): PBXNativeTarget;
begin
  Result:=nil;
  if not Assigned(prj) then Exit;
  Result:=prj.addTarget(ATargetName);
end;

function TargetAddRunScript(atarget: PBXNativeTarget): PBXShellScriptBuildPhase;
begin
  Result:=PBXShellScriptBuildPhase.Create;
  Result.name:=SCRIPT_DEFNAME;
  Result._headerComment:=SCRIPT_DEFNAME;
  Result.shellScript:=SCRIPT_DEFAULT;
  Result.shellPath:=SCRIPT_RUNPATH;
  atarget.buildPhases.Add(Result);
end;

function CreateFileRef(const afilename: string; const filetype: string ): PBXFileReference;
begin
  Result:=PBXFileReference.Create;
  Result.path:=afilename;
  Result._headerComment:=afilename;
  Result.explicitFileType:=FILETYPE_EXEC;
end;

function CreateGroup(const aname, srcTree: string): PBXGroup;
begin
  Result:=PBXGroup.Create;
  Result.name:=aname;
  Result.sourceTree:=srcTree;
  Result._headerComment:=aname;
end;

function CreateRootGroup(const projectfolder: string): PBXGroup;
begin
  Result:=CreateGroup('', GROUPSRC_ABSOLUTE);
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

end.



