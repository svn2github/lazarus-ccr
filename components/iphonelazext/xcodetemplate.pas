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
unit xcodetemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, xcodeproj;

procedure PrepareTemplateFile_(Src, TemplateValues: TStrings; BuildSettings: TFPStringHashTable);
procedure PrepareTemplateFile(Src, TemplateValues, ResFiles: TStrings);
procedure PrepareTemplateFile(prj: PBXProject; TemplateValues, ResFiles: TStrings);
function UpdateProject(const ProjFileName: string; TemplateValues, ResFiles: TStrings): Boolean;

procedure UpdateBldConfig(const proj: PBXProject; optName, optVal: string);
procedure UpdateMainFile(const proj: PBXProject; mainfile: string);
procedure UpdateCompileOpts(const proj: PBXProject; const aoptions: string);

const
  XCodeProjectTemplateIconID : AnsiString ='0AE3FFA610F3C9AF00A9B007,';
  XCodeProjectTemplateIcon   : AnsiString =
    '0AE3FFA610F3C9AF00A9B007 /* Icon.png */ = {isa = PBXFileReference; lastKnownFileType = image.png; path = Icon.png; sourceTree = "<group>"; };';

  XCodeIconFile : AnsiString = '0A2C67AE10F3CFB800F48811,';
  XCodeIconFileRef : AnsiString =
    '0A2C67AE10F3CFB800F48811 /* Icon.png in Resources */ = {isa = PBXBuildFile; fileRef = 0AE3FFA610F3C9AF00A9B007 /* Icon.png */; };';


  XCodeProjectTemplate : AnsiString =
    '// !$*UTF8*$!'#10+
    '{'#10+
    '	archiveVersion = 1;'#10+
    '	classes = {'#10+
    '	};'#10+
    '	objectVersion = 45;'#10+
    '	objects = {'#10+
    '  '#10+
    '  ??iconfileref'#10+
    '  '#10+
    '/* Begin PBXFileReference section */'#10+
    '		0A85A8AE10F0D28700AB8400 /* ??bundle */ = {isa = PBXFileReference; explicitFileType = wrapper.application; includeInIndex = 0; path = ??bundle; sourceTree = BUILT_PRODUCTS_DIR; };'#10+
    '		0A85A8B110F0D28700AB8400 /* ??plist */ = {isa = PBXFileReference; explicitFileType = text.plist.xml; name = ??plist; path = ??plist; sourceTree = SOURCE_ROOT; };'#10+
    '   ??icon'#10+
    '/* End PBXFileReference section */'#10+
    ''#10+
    '/* Begin PBXGroup section */'#10+
    '		0A52AE8110F0D05300478C4F = {'#10+
    '			isa = PBXGroup;'#10+
    '			children = ('#10+
    '       ??iconid'#10+
    '				0A85A8AF10F0D28700AB8400 /* Products */,'#10+
    '				0A85A8B110F0D28700AB8400 /* ??plist */,'#10+
    '			);'#10+
    '			sourceTree = "<group>";'#10+
    '		};'#10+
    '		0A85A8AF10F0D28700AB8400 /* Products */ = {'#10+
    '			isa = PBXGroup;'#10+
    '			children = ('#10+
    '				0A85A8AE10F0D28700AB8400 /* ??bundle */,'#10+
    '			);'#10+
    '			name = Products;'#10+
    '			sourceTree = "<group>";'#10+
    '		};'#10+
    '/* End PBXGroup section */'#10+
    ''#10+
    '/* Begin PBXNativeTarget section */'#10+
    '		0A85A8AD10F0D28700AB8400 = {'#10+
    '			isa = PBXNativeTarget;'#10+
    '			buildConfigurationList = 0A85A8B410F0D28800AB8400 /* Build configuration list for PBXNativeTarget */;'#10+
    '			buildPhases = ('#10+
    '				0A85A8B810F0D2D400AB8400 /* ShellScript */,'#10+
    '       0A2C67A610F3CEFA00F48811 /* Resources */,'#10+
    '			);'#10+
    '			buildRules = ('#10+
    '			);'#10+
    '			dependencies = ('#10+
    '			);'#10+
    '			name = ??targetname;'#10+
    '			productName = ??productname;'#10+
    '			productReference = 0A85A8AE10F0D28700AB8400 /* ??bundle */;'#10+
    '			productType = "com.apple.product-type.application";'#10+
    '		};'#10+
    '/* End PBXNativeTarget section */'#10+
    ''#10+
    '/* Begin PBXProject section */'#10+
    '		0A52AE8310F0D05300478C4F /* Project object */ = {'#10+
    '			isa = PBXProject;'#10+
    '			buildConfigurationList = 0A52AE8610F0D05300478C4F /* Build configuration list for PBXProject "project1" */;'#10+
    '			compatibilityVersion = "Xcode 3.1";'#10+
    '			hasScannedForEncodings = 0;'#10+
    '			mainGroup = 0A52AE8110F0D05300478C4F;'#10+
    '			productRefGroup = 0A85A8AF10F0D28700AB8400 /* Products */;'#10+
    '			projectDirPath = "";'#10+
    '			projectRoot = "";'#10+
    '			targets = ('#10+
    '				0A85A8AD10F0D28700AB8400,'#10+
    '			);'#10+
    '		};'#10+
    '/* End PBXProject section */'#10+
    ''#10+
    '/* Begin PBXResourcesBuildPhase section */'#10+
    '		0A2C67A610F3CEFA00F48811 /* Resources */ = {'#10+
    '			isa = PBXResourcesBuildPhase;'#10+
    '			buildActionMask = 2147483647;'#10+
    '			files = ('#10+
    '				??iconfile'#10+
    '			);'#10+
    '			runOnlyForDeploymentPostprocessing = 0;'#10+
    '		};'#10+
    '/* End PBXResourcesBuildPhase section */'#10+
    ''#10+
    '/* Begin PBXShellScriptBuildPhase section */'#10+
    '		0A85A8B810F0D2D400AB8400 /* ShellScript */ = {'#10+
    '			isa = PBXShellScriptBuildPhase;'#10+
    '			buildActionMask = 2147483647;'#10+
    '			files = ('#10+
    '			);'#10+
    '			inputPaths = ('#10+
    '			);'#10+
    '			outputPaths = ('#10+
    '			);'#10+
    '			runOnlyForDeploymentPostprocessing = 0;'#10+
    '			shellPath = /bin/sh;'#10+
    '			shellScript = "if [ x\"$ACTION\" != \"xbuild\" ]; then\n  # in case running scripts during cleaning gets fixed\n  exit 0\nfi\n\necho $FPC_COMPILER_PATH $FPC_COMPILER_OPTIONS $FPC_MAIN_FILE\n\n$FPC_COMPILER_PATH $FPC_COMPILER_OPTIONS $FPC_MAIN_FILE";'#10+
    '		};'#10+
    '/* End PBXShellScriptBuildPhase section */'#10+
    ''#10+
    '/* Begin XCBuildConfiguration section */'#10+
    '		0A52AE8510F0D05300478C4F /* Release */ = {'#10+
    '			isa = XCBuildConfiguration;'#10+
    '			buildSettings = {'#10+
    '				ARCHS = "$(ARCHS_STANDARD_32_BIT)";'#10+
    '                           "ARCHS[sdk=iphonesimulator*]" = "$(ARCHS_STANDARD_32_BIT)";'#10+
    '				COPY_PHASE_STRIP = YES;'#10+
    '				FPC_OUTPUT_FILE = $BUILT_PRODUCTS_DIR/$EXECUTABLE_PATH;'#10+
    '				FPC_COMPILER_OPTIONS = "-Parm -o$FPC_OUTPUT_FILE $FPC_CUSTOM_OPTIONS";'#10+
    '       "FPC_COMPILER_OPTIONS[sdk=iphonesimulator*]" = "-Tiphonesim -Pi386 -o$FPC_OUTPUT_FILE $FPC_CUSTOM_OPTIONS";'#10+
    '				FPC_COMPILER_PATH = ;'#10+
    '				FPC_CUSTOM_OPTIONS = ;'#10+
    '       "FPC_CUSTOM_OPTIONS[sdk=iphonesimulator*]" = ;'#10+
    '				FPC_MAIN_FILE = ;'#10+
    '				SDKROOT = iphoneos2.0;'#10+
    '				VALID_ARCHS = "armv6 armv7";'#10+
    '			};'#10+
    '			name = Release;'#10+
    '		};'#10+
    '		0A85A8B310F0D28800AB8400 /* Release */ = {'#10+
    '			isa = XCBuildConfiguration;'#10+
    '			buildSettings = {'#10+
    '				ALWAYS_SEARCH_USER_PATHS = YES;'#10+
    '				COPY_PHASE_STRIP = YES;'#10+
    '				DEBUG_INFORMATION_FORMAT = "dwarf-with-dsym";'#10+
    '				GCC_ENABLE_FIX_AND_CONTINUE = NO;'#10+
    '				GCC_PRECOMPILE_PREFIX_HEADER = YES;'#10+
    '				GCC_PREFIX_HEADER = "$(SYSTEM_LIBRARY_DIR)/Frameworks/UIKit.framework/Headers/UIKit.h";'#10+
    '				INFOPLIST_FILE = ??plist;'#10+
    '				INSTALL_PATH = "$(HOME)/Applications";'#10+
    '				OTHER_LDFLAGS = ('#10+
    '					"-framework",'#10+
    '					Foundation,'#10+
    '					"-framework",'#10+
    '					UIKit,'#10+
    '				);'#10+
    '				PREBINDING = NO;'#10+
    '				PRODUCT_NAME = ??productname;'#10+
    '				SDKROOT = iphoneos2.0;'#10+
    '				ZERO_LINK = NO;'#10+
    '			};'#10+
    '			name = Release;'#10+
    '		};'#10+
    '/* End XCBuildConfiguration section */'#10+
    '    '#10+
    '/* Begin XCConfigurationList section */'#10+
    '		0A52AE8610F0D05300478C4F /* Build configuration list for PBXProject "project1" */ = {'#10+
    '			isa = XCConfigurationList;'#10+
    '			buildConfigurations = ('#10+
    '				0A52AE8510F0D05300478C4F /* Release */,'#10+
    '			);'#10+
    '			defaultConfigurationIsVisible = 0;'#10+
    '			defaultConfigurationName = Release;'#10+
    '		};'#10+
    '		0A85A8B410F0D28800AB8400 /* Build configuration list for PBXNativeTarget */ = {'#10+
    '			isa = XCConfigurationList;'#10+
    '			buildConfigurations = ('#10+
    '				0A85A8B310F0D28800AB8400 /* Release */,'#10+
    '			);'#10+
    '			defaultConfigurationIsVisible = 0;'#10+
    '			defaultConfigurationName = Release;'#10+
    '		};'#10+
    '/* End XCConfigurationList section */'#10+
    '	};'#10+
    '	rootObject = 0A52AE8310F0D05300478C4F /* Project object */;'#10+
    '}'#10;

var
  // global variable... a bad pattern must be replaced
  DefaultBuildScript : string = '';

function ReadBuildScriptFile(const fn: string): string;

implementation

function EmptyBuildScript: string;
begin
  Result:='';
end;

function GetValueName(const Source: String; idx: Integer): String;
var
  i : integer;
  InQuote: boolean;
const
  //todo: expand symbols charset
  Symbols: set of char = [#9, #32, #10,#13,
    '=',':',';','-','+','*','/','\','!','@','#',
    '$','%','^','&','(',')','~','`',''''];
begin
  InQuote:=false;
  for i:=idx to length(Source) do begin
    if InQuote then begin
      if Source[i]='"' then InQuote := false;
    end else if Source[i] = '"' then
      InQuote := true
    else if Source[i] in Symbols then begin
      Result:=Copy(Source, idx, i-idx);
      Exit;
    end;
  end;
  Result:=Copy(Source, idx, length(Source)-idx+1);
end;

function ChangeValues(const Prefix, Source: String; Values: TStrings): String;
var
  i   : integer;
  nm  : string;
  v   : string;
begin
  Result:=Source;
  i:=Pos(Prefix, Result);
  while i>0 do begin
    nm:=GetValueName(Result, i+length(Prefix));
    Delete(Result, i, length(Prefix)+length(nm));
    v:=Values.Values[nm];

    if Pos(Prefix, v) <= 0 then  // don't allow circular prefix used, to avoid infinite loops
      Insert(v, Result, i);
    i:=Pos(Prefix, Result);
  end;
end;

procedure PrepareTemplateFile_(Src, TemplateValues: TStrings; BuildSettings: TFPStringHashTable);
//todo: Better code to update XCode project file!
var
  i, j       : Integer;
  nm, s, v   : String;
  isSettings : Boolean;

begin
  if not Assigned(Src) then Exit;

  if Assigned(TemplateValues) then
    for i:=0 to Src.Count-1 do
      Src[i]:=ChangeValues('??', Src[i], TemplateValues);

  isSettings:=false;

  if Assigned(BuildSettings) and (BuildSettings.Count>0) then begin

    for i:=0 to Src.Count-1 do begin
      if not isSettings then
        isSettings:=Pos('buildSettings', Src[i])>0
      else begin
        if Trim(Src[i])='};' then
          isSettings:=false
        else begin
          j:=1;
          s:=Src[i];
          while (j<=length(s)) and (s[j] in [#9, #32]) do
            inc(j);

          nm:=GetValueName(s, j);

          if Assigned(BuildSettings.Find(nm)) then begin
            v:=BuildSettings.Items[nm];
            Src[i]:=Copy(Src[i], 1, j-1)+nm+ ' = ' + v + ';';
          end;
        end;
      end; {of else}
    end;
  end;
end;

procedure UpdateBldConfig(const proj: PBXProject; optName, optVal: string);
var
  trg : PBXNativeTarget;
  cfg : XCBuildConfiguration;
  i   : integer;
  j   : integer;
begin
  for i:=0 to proj.targets.Count-1 do begin
    trg := PBXNativeTarget(proj.targets[i]);
    for j:=0 to trg.buildConfigurationList.Count-1 do begin
      cfg:=XCBuildConfiguration(trg.buildConfigurationList.buildConfigurations[j]);
      cfg.buildSettings.str[optName]:=optVal;
    end;
  end;
end;

function isCPUorOS(const s: string): boolean;
begin
  Result:=(Pos('$(TargetCPU)', s)>0) or (Pos('$(TargetOS)', s)>0)
end;

function ResolveCPUOS(const s: string; const cpu, os: string): string;
begin
  Result:=StringReplace(s, '$(TargetCPU)', cpu, [rfReplaceAll, rfIgnoreCase]);
  Result:=StringReplace(Result, '$(TargetOS)', os, [rfReplaceAll, rfIgnoreCase]);
end;

procedure UpdateCompileOpts(const proj: PBXProject; const aoptions: string);
var
  options : string;
  opt    : string;
  i32opt : string;
  i64opt : string;
  a32opt : string;
  a64opt : string;
  i,j,l  : integer;
  s      : string;
begin
  //UpdateBldConfig(proj, 'FPC_CUSTOM_OPTIONS', options);
  UpdateBldConfig(proj, 'FPC_CUSTOM_OPTIONS', '');
  options:=aoptions+' '; // trailing space, always to consume the last options
  i:=1;
  j:=1;
  l:=length(options);

  opt:='';
  i32opt:='';
  i64opt:='';
  a32opt:='';
  a64opt:='';

  while i<=l do begin
    if (options[i]=' ') and (i>=j) then begin
      if (options[j]=' ') then begin
        j:=i+1;
      end else begin
        s:=Copy(options,j,i-j);
        if isCPUorOS(s) then begin
          i32opt:=i32opt+ResolveCPUOS(s, 'i386', 'iphonesim')+' ';
          i64opt:=i64opt+ResolveCPUOS(s, 'x86_64', 'iphonesim')+' ';
          a32opt:=a32opt+ResolveCPUOS(s, 'arm', 'darwin')+' ';
          a64opt:=a64opt+ResolveCPUOS(s, 'aarch64', 'darwin')+' ';
        end else begin
          opt:=opt+s+' ';
        end;
        j:=i+1;
      end;
    end;
    inc(i);
  end;
  UpdateBldConfig(proj, 'FPC_CUSTOM_OPTIONS[sdk=iphoneos*]', opt);
  UpdateBldConfig(proj, 'FPC_CUSTOM_OPTIONS[sdk=iphonesimulator*]', opt);
  UpdateBldConfig(proj, 'FPC_OPT_I32', i32opt);
  UpdateBldConfig(proj, 'FPC_OPT_I64', i64opt);
  a32opt:=a32opt + ' -Cparmv7 -Cg -Cfvfpv3 ';
  UpdateBldConfig(proj, 'FPC_OPT_A32', a32opt);
  UpdateBldConfig(proj, 'FPC_OPT_A64', a64opt);
end;

procedure UpdateMainFile(const proj: PBXProject; mainfile: string);
begin
  UpdateBldConfig(proj, 'FPC_MAIN_FILE', mainfile);
  UpdateBldConfig(proj, 'FPC_MAIN_DIR', ExtractFileDir(mainfile));
end;

procedure PrepareTemplateFile(Src, TemplateValues, ResFiles: TStrings);
var
  prj : PBXProject;
begin
  prj:=ProjectCreate3_2;
  PrepareTemplateFile(prj, TemplateValues, ResFiles);
  src.Text:=ProjectWrite(prj);
end;

function ReadBuildScriptFile(const fn: string): string;
var
  fs : TFileStream;
begin
  fs:=TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    if fs.Size=0 then begin
      Result:='';
      Exit;
    end;
    SetLength(Result, fs.Size);
    fs.Read(Result[1], fs.Size);

    // replacing line breaks with #13 for Xcode
    Result:=StringReplace(Result, #13#10, #13,  [rfReplaceAll]);
    Result:=StringReplace(Result, #10#13, #13,  [rfReplaceAll]);
    Result:=StringReplace(Result, #10, #13,  [rfReplaceAll]);
  finally
    fs.Free;
  end;
end;


procedure PrepareTemplateFile(prj: PBXProject; TemplateValues, ResFiles: TStrings);
var
  trg : PBXNativeTarget;
  cfg : XCBuildConfiguration;
  fr  : PBXFileReference;
  grp : PBXGroup;
  pgrp : PBXGroup;
  scr : PBXShellScriptBuildPhase;
  res : PBXResourcesBuildPhase;
  i   : integer;
  plist      : string;
  targetName : string;
  bundle     : string;
  main       : string;
  fnm  : string;
const
  FILE_COPY_MASK = 2147483647;
begin
  targetName:=TemplateValues.Values['targetname'];
  bundle:=TemplateValues.Values['bundle'];
  plist:=TemplateValues.Values['plist'];
  if plist='' then plist:='info.plist';
  main:=TemplateValues.Values['mainfile'];

  trg:=ProjectForceTarget(prj, targetName);
  for i:=0 to prj.buildConfigurationList.Count-1 do begin
    cfg:=prj.buildConfigurationList[i];
    ConfigIOS(cfg, TARGET_IOS_8_1);
    // Enable Build Active Architecture Only When Debugging
    // it would be amd-32 for iPhone5 (and earlier)
    //         and amd-64 for iPhone6 (and later)
    // Release requires to have a fat binary 32+64 amd, if target is less than iphone6
    if (cfg.name='Debug') then begin
      SetNewStr(cfg.buildSettings, 'ONLY_ACTIVE_ARCH', 'YES');
    end;
  end;

  // adding application type
  trg.productName:=TemplateValues.Values['productname'];
  trg.productType:=PRODTYPE_APP;

  // target configuration
  if not Assigned(trg.buildConfigurationList) then
    trg.buildConfigurationList:=XCConfigurationList.Create;

  // Debug
  cfg:=trg.buildConfigurationList.findConfig('Debug', true);
  SetNewStr(cfg.buildSettings, 'INFOPLIST_FILE', '$(SRCROOT)/'+plist);
  SetNewStr(cfg.buildSettings, 'PRODUCT_NAME', trg.productName);

  // Release
  cfg:=trg.buildConfigurationList.findConfig('Release', true);
  SetNewStr(cfg.buildSettings, 'INFOPLIST_FILE', '$(SRCROOT)/'+plist);
  SetNewStr(cfg.buildSettings, 'PRODUCT_NAME', trg.productName);

  if trg.buildConfigurationList.defaultConfigurationName = '' then
    trg.buildConfigurationList.defaultConfigurationName:='Debug';

  // Adding the ".app" directory for the bundle and bind it to the target
  if not Assigned(trg.productReference) then begin
    fr:=FileRefCreate(bundle, FILETYPE_MACHO, SRCTREE_PRODUCT);
    trg.productReference:=fr;
  end;

  // Creating "content" for the directory. It should also contain .plist
  pgrp:=prj.mainGroup.findGroup(targetName, true); // name must match to the target name!
  grp:=pgrp.findGroup('Supporting Files', true); // name must match!

  // creating a reference to info.plist. It's located at "xcode" folder.
  // Thus at the same directar as .xcodeproj dir
  if not Assigned(grp.findFileRefByPathName(plist)) then
  begin
    fr:=FileRefCreate(plist, FILETYPE_PLIST, SRCTREE_PROJECT );
    grp.children.Add( fr );
  end;

  if not Assigned(TargetFindRunScript(trg)) then begin
    scr:=TargetAddRunScript(trg);
    scr.shellScript:=DefaultBuildScript;
    scr.showEnvVarsInLog:=true;
  end;

  // todo: removal of files!
  if Assigned(ResFiles) and (ResFiles.Count>0) then begin
    grp:=prj.mainGroup.findGroup('Resources', true); // name must match to the target name!
    //grp:=pgrp.addSubGroup('Supporting Files'); // name must match!

    res := PBXResourcesBuildPhase(TargetFindBuildPhase(trg, PBXResourcesBuildPhase));
    if not Assigned(res) then begin
      res := PBXResourcesBuildPhase.Create;
      //res.name:='Resources';
      res.buildActionMask:=FILE_COPY_MASK;
      trg.buildPhases.Add(res);
    end;

    for i:=0 to ResFiles.Count-1 do begin
      fnm:='../'+ResFiles[i];
      if not Assigned(grp.findFileRefByPathName(fnm)) then begin
        fr:=FileRefCreate(fnm, FILETYPE_MACHO, SRCTREE_PROJECT);
        grp.children.add( fr );
        res.AddFile(fr);
      end;
    end;
  end;

  UpdateMainFile(prj, main);
  UpdateCompileOpts(prj, TemplateValues.Values['projoptions']);
end;

function UpdateProject(const ProjFileName: string; TemplateValues, ResFiles: TStrings): Boolean;
var
  prj : PBXProject;
begin
  if FileExists(ProjFileName) then begin
    prj:=nil;
    Result:=ProjectLoadFromFile(ProjFileName, prj);
    if not Result then begin
      prj.Free;
      Exit;
    end;
  end else begin
    prj:=ProjectCreate3_2;
  end;

  try
    PrepareTemplateFile(prj, TemplateValues, Resfiles);
    Result:=ProjectSaveToFile(prj, ProjFileName);
  finally
    prj.Free;
  end;
end;

end.

