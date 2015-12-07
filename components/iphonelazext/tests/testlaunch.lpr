program testlaunch;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, iphonesimctrl;

procedure PrintList;
var
  lst : TList;
  i   : integer;
  dev : TSimDevice;
begin
  lst := TList.Create;
  try
    ListDevice(lst);
    for i:=0 to lst.Count-1 do begin
      dev := TSimDevice(lst[i]);
      writeln(dev.id,' ',dev.isavail,' ',dev.sdk,' ', dev.name,' ',dev.state);
    end;
  finally
    lst.Free;
  end;
end;

procedure RunDevice(const nm: string);
begin
  RunSim(nm);
end;

procedure RunApp(const appid: string);
var
  res : string;
  pid : Integer;
begin
  res:='';
  if not RunAppOnSim(appid, '', False, pid, res) then begin
    writeln('failed to run app');
  end else begin
    writeln('launching!');
    writeln('pid = ', pid);
    writeln('outstr: ');
    writeln(res);
  end;
end;

procedure PrintHelp;
begin
  writeln('testlaunch %action% [%parameters%]');
  writeln('action:');
  writeln('  list - list devices');
  writeln('  run %deviceid% - runs a simulator with specified id');
  writeln('  runapp %appid% - runs an application on booted device');
end;

var
  act : string = '';

procedure ParseParam;
begin
  if ParamCount=0 then Exit;
  act:=ParamStr(1);
  act:=lowercase(act);
end;

begin
  if ParamCount=0 then begin
    PrintHelp;
    exit;
  end;
  ParseParam;
  if act='list' then PrintList
  else if act='run' then begin
    if ParamCount=1 then begin
      writeln('Please specify deviceid');
      Exit;
    end;
    RunDevice(ParamStr(2));
  end else if act='runapp' then begin
    if ParamCount=1 then begin
      writeln('Please specify application id to run');
      Exit;
    end;
    RunApp(ParamStr(2));
  end;
end.

