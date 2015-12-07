unit iphonesimctrl;

{$mode delphi}

interface

uses
  Classes, SysUtils, process
  , jsonparser, fpjson
  {$ifdef unix}, BaseUnix{$endif} // for StopProc()
  ;

procedure RunSim(const SimName: string);
function RunAppOnSim(const AppID, DeviceID: string; WaitDebugger: Boolean; var pid: Integer; var outstr: string): Boolean;
procedure StopProc(pid: Integer);

type
  TSimDevice = class(TObject)
  public
    sdk      : string;
    state    : string;
    isavail  : Boolean;
    name     : string;
    id       : string;
  end;

function ListDevice(lst: TList): Boolean;

implementation

procedure RunSim(const SimName: string);
var
  outstr: string;
begin
  outstr:='';
  RunCommand('xcrun', ['instruments', '-w' ,SimName], outstr);
end;

function RunAppOnSim(const AppID, DeviceID: string; WaitDebugger: Boolean; var pid: Integer; var outstr: string): Boolean;
var
  devid: string;
  err : Integer;
  i   : Integer;
  j   : Integer;
begin
  if DeviceID='' then devid:='booted'
  else devid:=DeviceID;

  if WaitDebugger then
    Result:=RunCommand('xcrun', ['simctl', 'launch', '-w', devid, AppID], outstr)
  else
    Result:=RunCommand('xcrun', ['simctl', 'launch', devid, AppID], outstr);

  if Result and (length(outstr)>0) then begin
    i:=length(outstr);
    // skipping white spaces, if any
    while (i>0) and not (outstr[i] in ['0'..'9']) do dec(i);

    j:=i;
    while (i>0) and (outstr[i] in ['0'..'9']) do dec(i);
    Val( copy(outstr, i+1, j-i), pid, err);
    writeln('err = ', err);
    if err>0 then pid:=0;
  end;
end;

procedure StopProc(pid: Integer);
begin
  {$ifdef unix}
  FpKill(pid, SIGTERM);
  {$endif}
end;

function JSStr(v: TJSONData): string;
begin
  if not Assigned(v) then Result:=''
  else Result:=v.AsString;
end;

function JsonToSimDev(oj: TJsonObject): TSimDevice;
begin
  Result:=nil;
  if not Assigned(oj) or (oj.JSONType<>jtObject) then Exit;
  Result:=TSimDevice.Create;
  Result.state:=JSStr(oj.Find('state'));
  Result.isavail:=JSStr(oj.Find('availability'))='(available)';
  Result.name:=JSStr(oj.Find('name'));
  Result.id:=JSStr(oj.Find('udid'));
end;

procedure CollectDevices(list: TJSONArray; const asdk: string; dst: TList);
var
  oj : TJSONObject;
  i  : Integer;
  d  : TSimDevice;
begin
  for i:=0 to list.Count-1 do begin
    oj:=TJSONObject(list.Items[i]);
    d:=JsonToSimDev(oj);
    d.sdk:=asdk;
    if Assigned(d) then
      dst.Add(d);
  end;
end;

function ListDevice(lst: TList): Boolean;
var
  s  : string;
  j  : TJSONParser;
  dt : TJSONData;
  d  : TJSONObject;
  v  : TJSONData;
  i  : Integer;
begin
  s:='';
  RunCommand('xcrun', ['simctl', 'list', 'devices', '-j'], s);
  try
    dt:=nil;
    j:=TJSONParser.Create(s, []);
    try
      dt:=j.Parse;
    finally
      j.Free;
    end;

    try
      Result:=false;
      if not Assigned(dt) or (dt.JSONType<>jtObject) or (dt.Count=0) then Exit;
      v:=TJSONObject(dt).Find('devices');
      if not Assigned(v) or (v.JSONType<>jtObject) then Exit;
      d:=TJSONObject(v);

      for i:=0 to d.Count-1 do begin
        if Pos('iOS', d.Names[i])=1 then begin
          if (d.items[i].JSONType=jtArray) then
            CollectDevices( TJSONArray(d.Items[i]), d.Names[i], lst);
        end;
      end;

    finally
      dt.Free;
    end;

  except
    on e:exception do
      writeln('error: ', e.message);
  end;
end;

end.

