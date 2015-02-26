unit cconvlog;

interface

{$ifdef fpc}{$mode delphi}{$endif}

uses
  SysUtils;


procedure log(const s: string); overload;
procedure log(const fmt: string; const params: array of const); overload;

var
  _log : procedure (const s: string) = nil;

procedure _stdOutLog(const s: string);
procedure _stdErrLog(const s: string);

implementation

procedure _stdErrLog(const s: string);
begin
  writeln(StdErr, s);
end;

procedure _stdOutLog(const s: string);
begin
  writeln(s);
end;

procedure log(const s: string); overload;
begin
  if Assigned(_log) then _log(s);
end;

procedure log(const fmt: string; const params: array of const); overload;
begin
  if not assigned(_log) then Exit;
  if fmt<>'' then Log(Format(fmt, params)) else Log('');
end;

end.

