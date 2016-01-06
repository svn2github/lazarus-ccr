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
    writeln(pl.GetStrValue('CFBundleDevelopmentRegion'));
    pl.SetStrValue('DTPlatformName','TESTPLATFORMNAME');
    SaveToXMLFile(pl, ChangeFileExt(fn, '.xml.out'));
  finally
    pl.Free;
  end;
end;

var
  fn : string;
begin
  if ParamCount=0 then begin
    writeln('please specify the .info file');
    Exit;
  end;
  fn := ParamStr(1);
  TestRead(fn);
end.

