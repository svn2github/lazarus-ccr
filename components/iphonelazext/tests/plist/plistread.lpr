program plistread;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, plistfile
  { you can add units after this };

procedure TestRead(const fn: string);
var
  pl : TPListFile;
begin
  pl:=TPlistFile.Create;
  try
    LoadFromXML(fn, pl);
    writeln(GetStr(pl.root, 'CFBundleDevelopmentRegion'));
    SetStr(pl.root, 'DTPlatformName','TESTPLATFORMNAME');
    SaveToXMLFile(pl, ChangeFileExt(fn, '.xml.out'));
  finally
    pl.Free;
  end;
end;

procedure TestDefault;
var
  pl: TPListFile;
  arr: TPListValue;
begin
  pl:=TPListFile.Create;
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
  write(WriteXML(pl));
  pl.Free;
end;

var
  fn : string;
begin
  if ParamCount=0 then begin
    writeln('please specify the .info file');
    Exit;
  end;
  fn := ParamStr(1);
  if fn='-def' then TestDefault
  else TestRead(fn);
end.

