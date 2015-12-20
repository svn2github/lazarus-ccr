unit xcodeprojutils;

{$mode delphi}

interface

uses
  Classes, SysUtils, xcodeproj;

// proj name is used for ".xcodeprj" bundle-directory
// hostdir is the hosting directory for the project
// prj - the content of the project
function ProjectWriteStruct(prj: PBXProject; const projName: string; const HostDir: string): Boolean;

const
  ProjExt = '.xcodeproj';
  ProjFileName = 'project.pbxproj';

implementation

function ProjectWriteStruct(prj: PBXProject; const projName: string; const HostDir: string): Boolean;
var
  prjdir : string;
  s      : string;
  fs     : TFileStream;
  fn     : string;
begin
  if HostDir = ''
    then prjdir:=IncludeTrailingPathDelimiter(GetCurrentDir)+projName+ProjExt
    else prjdir:=IncludeTrailingPathDelimiter(HostDir)+projName+ProjExt;

  Result:=ForceDirectories(prjdir);
  if not Result then Exit;
  fn:=IncludeTrailingPathDelimiter(prjdir)+ProjFileName;

  s:=ProjectWrite(prj);

  try
    fs:=TFileStream.Create(fn, fmCreate);
    try
      if length(s)>0 then begin
        fs.Write(s[1], length(s));
        fs.Size:=length(s);
      end;
      Result:=true;
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;
end;

end.

