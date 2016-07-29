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
unit iPhoneBundle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, PlistFile;

const
  platform_iPhoneSim = 'iphonesimulator';
  sdk_iPhoneSim2_0   = 'iphonesimulator2.0';
  sdk_iPhoneSim2_1   = 'iphonesimulator2.1';
  sdk_iPhoneSim2_2   = 'iphonesimulator2.2';
  sdk_iPhoneSim3_0   = 'iphonesimulator3.0';
  sdk_iPhoneSim3_1   = 'iphonesimulator3.1';

type
  TiPhoneBundleInfo = record
    DisplayName : WideString; {if DisplayName='' then DisplayName=BundleName}
    iPlatform   : WideString;
    SDKVersion  : WideString;
    AppID       : WideString;
    MainNib     : WideString;
  end;

function GetiPhoneSimUserPath: WideString;

procedure MakeSimSpaceStruct(const BundleName: WideString; var BundleAppDir: WideString);
procedure MakeSimSpaceStruct(const iPhoneSimUserPath, BundleName: WideString; var BundleAppDir: WideString);
procedure MakeSimSpaceStruct(const iPhoneSimUserPath, SpaceName, BundleName: WideString; var BundleAppDir: WideString);

function GetBundleExeName(const BundleAppDir, ExeName: WideString): WideString;

procedure WritePkgFile(const FileName: WideString);
function WriteDefInfoList_(const InfoFileName, BundleName, ExeName: WideString; const info: TiPhoneBundleInfo): Boolean;
function WriteDefInfoList(const InfoFileName, BundleName, ExeName: WideString; const info: TiPhoneBundleInfo): Boolean;

procedure CreateBundle(const BundleName, ExeName: WideString; const Info: TiPhoneBundleInfo; var FullBundlePath, FullExeName: WideString);
procedure CreateBundle(const BundleName, SpaceName, ExeName: WideString; const Info: TiPhoneBundleInfo; var RealSpace, FullBundlePath, FullExeName: WideString);
function GetBundleFullDir(const BundleName, SpaceName: WideString): WideString;
function GetSandBoxDir(const SpaceName: WideString): WideString;

function AddPathDelim(const w: WideString): WideString;

function RandomSpaceName: WideString;

implementation

(* Build script:
## start
echo "compiling FPC project"

export RESULT_EXE=${BUILT_PRODUCTS_DIR}/${EXECUTABLE_PATH}
export IOSHEADERS=
cd $FPC_MAIN_DIR

export TargetCPU=${PLATFORM_PREFERRED_ARCH}

export OPT32=${FPC_OPT_A32}
export OPT64=${FPC_OPT_A64}
export CPU32="arm"
export CPU64="aarch64"
export TargetOS="darwin"

if [ "${PLATFORM_NAME}" == "iphonesimulator" ]; then
export OPT32=${FPC_OPT_I32}
export OPT64=${FPC_OPT_I64}
export CPU32="i386"
export CPU64="x86_64"
export TargetOS="iphonesim"
fi

# 64-bit compilation
export Result64=${RESULT_EXE}_64
export TargetCPU=${CPU64}
export Target=${TargetCPU}-${TargetOS}
## making output directory
export outdir=lib/${Target}
mkdir -p ${outdir}

echo ${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} -MDelphi -Scghi -O3 -l -dIPHONEALL \
${FPC_CUSTOM_OPTIONS} ${OPT64} \
-Filib/${Target} -FUlib/${Target} \
-XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
-o${Result64}

${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} -MDelphi -Scghi -O3 -l -dIPHONEALL \
 ${FPC_CUSTOM_OPTIONS} ${OPT64} \
-Filib/${Target} -FUlib/${Target} \
-XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
 -k-F/Users/dmitry/FPC_laz/paseng/tests/testBomber3UI -k-t \
 -o${Result64}
export RES=$?

if [ $RES != 0 ]; then
  exit $RES
fi


# 32-bit complication
export Result32=${RESULT_EXE}_32
export TargetCPU=${CPU32}
export Target=${TargetCPU}-${TargetOS}
## making output directory
export outdir=lib/${Target}
mkdir -p ${outdir}

echo ${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} -MDelphi -Scghi -Cg -O3 -l -dIPHONEALL \
${FPC_CUSTOM_OPTIONS} ${OPT32} \
-Filib/${Target} -FUlib/${Target} \
-XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
-k-F/Users/dmitry/FPC_laz/paseng/tests/testBomber3UI \
-o${Result32}

${FPC_DIR}fpc -T${TargetOS} -P${TargetCPU} -MDelphi -Scghi -Cg -O3 -l -dIPHONEALL \
${FPC_CUSTOM_OPTIONS} ${OPT32} \
-Filib/${Target} -FUlib/${Target} \
-XR${SDKROOT}  -FD${PLATFORM_DEVELOPER_BIN_DIR} $FPC_MAIN_FILE \
-k-F/Users/dmitry/FPC_laz/paseng/tests/testBomber3UI \
-o${Result32}
export RES=$?

if [ $RES != 0 ]; then
exit $RES
fi

lipo -create ${Result32} ${Result64} -output ${RESULT_EXE}
rm ${Result32}
rm ${Result64}

exit $FPCRES

*)

uses
  iPhoneExtOptions;

function RandomSpaceName: WideString;
var
  g   : TGUID;
  id  : String;
begin
  CreateGUID(g);
  id:=GUIDToString(g);
  id:=Copy(id, 2, length(id)-2);
  Result:=UTF8Decode(id);
end;


procedure CreateBundle(const BundleName, SpaceName, ExeName: WideString; const Info: TiPhoneBundleInfo; var RealSpace, FullBundlePath, FullExeName: WideString);
var
  appdir : WideString;
begin
  if SpaceName='' then
    RealSpace:=RandomSpaceName
  else
    RealSpace:=SpaceName;

  MakeSimSpaceStruct(GetiPhoneSimUserPath, RealSpace, BundleName, appdir);
  FullBundlePath:=appdir;
  appdir:=AddPathDelim(appdir);
  FullExeName:=appdir+ExeName;
  WritePkgFile(appdir+'PkgInfo');
  WriteDefInfoList(appdir+'Info.plist', BundleName, ExeName, Info);
end;

function GetBundleFullDir(const BundleName, SpaceName: WideString): WideString;
var
  path8   : String;
  space8  : String;
  p : string;
begin
  path8:=UTF8Encode(GetiPhoneSimUserPath);
  space8:=UTF8Encode(SpaceName);

  p:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(path8)+space8);
  Result:=UTF8Decode(p+UTF8Encode(BundleName)+'.app');
end;

function GetSandBoxDir(const SpaceName: WideString): WideString;
var
  path8   : String;
  space8  : String;
  p : string;
begin
  path8:=UTF8Encode(GetiPhoneSimUserPath);
  space8:=UTF8Encode(SpaceName);

  p:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(path8)+space8);
  if DirectoryExistsUTF8(p) then
    Result:=UTF8Decode(p)
  else if DirectoryExistsUTF8(path8) then
    result:=UTF8Decode(path8)
  else
    result :='';
end;

procedure CreateBundle(const BundleName, ExeName: WideString; const Info: TiPhoneBundleInfo; var FullBundlePath, FullExeName: WideString);
var
  sp : WideString;
begin
  CreateBundle(BundleName, '', ExeName, Info, sp, FullBundlePath, FullExeName);
end;

function AddPathDelim(const w: WideString): WideString;
begin
  if w='' then
    Result:=PathDelim
  else if w[length(w)]<>PathDelim then
    Result:=w+PathDelim;
end;

function GetiPhoneSimUserPath: WideString;
var
  s : String;
begin
  s := EnvOptions.SimAppsPath;
  EnvOptions.SubstituteMacros(s);
  result := UTF8Decode(s);
end;

{
~/Library/Application Support/iPhone Simulator/Users/%SPACENAME%

%SPACENAME%/Applications
%SPACENAME%/Applications/%AppBundle.app%
%SPACENAME%/Documents
%SPACENAME%/tmp
}
procedure MakeSimSpaceStruct(const iPhoneSimUserPath, SpaceName, BundleName: WideString; var BundleAppDir: WideString);
var
  path8   : String;
  space8  : String;
  p : string;
begin
  path8:=UTF8Encode(iPhoneSimUserPath);
  space8:=UTF8Encode(SpaceName);

  p:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(path8)+space8);
  BundleAppDir:=UTF8Decode(p+UTF8Encode(BundleName)+'.app');
  ForceDirectories(UTF8Encode(BundleAppDir));
  ForceDirectories(p+'Documents');
  ForceDirectories(p+'tmp');

end;

procedure MakeSimSpaceStruct(const iPhoneSimUserPath, BundleName: WideString; var BundleAppDir: WideString);
begin
  MakeSimSpaceStruct(iPhoneSimUserPath, RandomSpaceName, BundleName, BundleAppDir);
end;

procedure MakeSimSpaceStruct(const BundleName: WideString; var BundleAppDir: WideString);
begin
  MakeSimSpaceStruct( GetiPhoneSimUserPath, BundleName, BundleAppDir);
end;

function GetBundleExeName(const BundleAppDir, ExeName: WideString): WideString;
begin
  Result:=AddPathDelim(BundleAppDir)+ExeName;
end;

procedure WritePkgFile(const FileName: WideString);
var
  fs : TFileStream;
  s  : String;
begin
  fs:=TFileStream.Create( UTF8Encode(FileName), fmCreate);
  s:='APPL????';
  fs.Write(s[1], length(s));
  fs.Free;
end;

procedure InitDefaultPlist(pl: TPListFile);
var
  arr : TPListValue;
begin
  SetStr(pl, 'CFBundleDevelopmentRegion', 'English');
  SetStr(pl, 'CFBundleDisplayName', '');
  SetStr(pl, 'CFBundleExecutable', '');
  SetStr(pl, 'CFBundleIdentifier', '');
  SetStr(pl, 'CFBundleInfoDictionaryVersion', '6.0');
  SetStr(pl, 'CFBundleName', '');
  SetStr(pl, 'CFBundlePackageType', 'APPL');
  SetStr(pl, 'CFBundleSignature', '????');
  SetArr(pl, 'CFBundleSupportedPlatforms');
  SetStr(pl, 'CFBundleVersion', '1.0');
  SetStr(pl, 'DTPlatformName', '');
  SetStr(pl, 'DTSDKName', '');
  SetBool(pl, 'LSRequiresIPhoneOS', true);
  arr:=SetArr(pl, 'UISupportedInterfaceOrientations');
  AddStr(arr, 'UIInterfaceOrientationPortrait');
  AddStr(arr, 'UIInterfaceOrientationLandscapeLeft');
end;

function WriteDefInfoList(const InfoFileName, BundleName, ExeName: WideString; const info: TiPhoneBundleInfo): Boolean;
var
  pl : TPListFile;
  arr : TPListValue;
begin
  Result:=false;
  pl := TPListFile.Create;
  try
    if not FileExists(InfoFileName) then begin
      InitDefaultPlist(pl);
    end else
      LoadFromFile( UTF8Encode(InfoFileName), pl);
    SetStr(pl, 'CFBundleDisplayName', info.DisplayName);
    SetStr(pl, 'CFBundleExecutable', ExeName);
    SetStr(pl, 'CFBundleIdentifier', info.AppID);
    SetStr(pl, 'CFBundleName', BundleName);
    arr:=SetArr(pl, 'CFBundleSupportedPlatforms');
    SetStr(arr, 0, info.iPlatform);
    SetStr(pl, 'DTPlatformName', info.iPlatform);
    SetStr(pl, 'DTSDKName', info.SDKVersion);

    Result:=SaveToXMLFile(pl, UTF8Encode(InfoFileName));
  finally
    pl.Free;
  end;
end;

function WriteDefInfoList_(const InfoFileName, BundleName, ExeName: WideString; const info: TiPhoneBundleInfo): Boolean;
const
  BundleFormat : AnsiString =
    '<?xml version="1.0" encoding="UTF-8"?>'#10+
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">'#10+
    '<plist version="1.0">'#10+
    '<dict>'#10+
    '	<key>CFBundleDevelopmentRegion</key>'#10+     '	<string>English</string>'#10+
    '	<key>CFBundleDisplayName</key>'#10+           '	<string>%s</string>'#10+    {display name}
    '	<key>CFBundleExecutable</key>'#10+            '	<string>%s</string>'#10+    {exe name}
    '	<key>CFBundleIdentifier</key>'#10+            '	<string>%s</string>'#10+ {company + bundle name}
    '	<key>CFBundleInfoDictionaryVersion</key>'#10+ '	<string>6.0</string>'#10+
    '	<key>CFBundleName</key>'#10+                  '	<string>%s</string>'#10+    {bundle name}
    '	<key>CFBundlePackageType</key>'#10+           '	<string>APPL</string>'#10+  // must be present for AppStore deployment!
    '	<key>CFBundleSignature</key>'#10+             '	<string>????</string>'#10+
    '	<key>CFBundleSupportedPlatforms</key>'#10+    '	<array>'#10+'		<string>%s</string>'#10+'	</array>'#10+ {platform}
    '%s'+ // optional MainNib name
    '	<key>CFBundleVersion</key>'#10+               '	<string>1.0</string>'#10+
    '	<key>DTPlatformName</key>'#10+                '	<string>%s</string>'#10+     {platform}
    '	<key>DTSDKName</key>'#10+                     '	<string>%s</string>'#10+     {sdk version}
    '	<key>LSRequiresIPhoneOS</key>'#10+            '	<true/>'#10+
    '	<key>UISupportedInterfaceOrientations</key>'#10+
  	' <array>'#10+
  	'	  <string>UIInterfaceOrientationPortrait</string>'#10+
  	'	  <string>UIInterfaceOrientationLandscapeLeft</string>'#10+
  	' </array>'#10+
    '</dict>'#10+
    '</plist>';

  function MainNibString(const NibName: WideString): AnsiString;
  begin
    if NibName='' then Result:=''
    else Result:='<key>NSMainNibFile</key><string>'+UTF8Encode(NibName)+'</string>'#10;
  end;

var
  dispName : WideString;
  s        : String;
  fs       : TFileStream;
begin
  Result:=false;
  if BundleName='' then Exit;

  dispName:=info.DisplayName;
  if dispName='' then dispName:=BundleName;

  with info do
    s:=Format( BundleFormat,
      [ UTF8Encode(dispName),
        UTF8Encode(ExeName),
        UTF8Encode(AppID),
        UTF8Encode(BundleName),
        UTF8Encode(iPlatform),
        MainNibString(info.MainNib),
        UTF8Encode(iPlatform),
        UTF8Encode(SDKVersion)
      ]);
  if FileExists(InfoFileName) then DeleteFile(InfoFileName);

  fs:=TFileStream.Create(UTF8Encode(InfoFileName), fmCreate or fmOpenWrite);
  try
    if s<>'' then fs.Write(s[1], length(s));
  finally
    fs.Free;
  end;
end;

end.

