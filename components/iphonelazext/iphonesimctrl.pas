unit iphonesimctrl;

{$mode delphi}

interface

uses
  {$ifdef unix}
  BaseUnix, Unix, termio,
  {$endif}
  Classes, SysUtils, process
  , jsonparser, fpjson
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

function InstallXcodePrj(const project, sdk, deviceid: string): Boolean;

type
  { TRWProcess }

  TRWProcess = class(TObject)
  protected
    process      : TProcess;
    bytesread    : integer;
    stderrbytesread : integer;
    outputstring : string;
    stderrstring : string;
    exitstatus   : Integer;
    procedure CloseProcess;
    procedure ReadProc(TimeOut: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function WriteLn(const s: string): Boolean;
    function ReadLn(var s: string): Boolean;
    function HasLine: Boolean;
    procedure Await(TimeOut: integer = -1);
    procedure Run(const exename: string; const commands: array of string;
      const curdir: string);
    procedure Terminate(exitCode: integer);
    function isRunning: Boolean;
  end;

const
  DEF_LLDB_EXENAME = 'lldb';

procedure LLDBRediretIO(const exename: string; pid, ttyfn: string); overload;
procedure LLDBRediretIO(const pid, ttyfn: string); overload;
procedure LLDBRediretIO(pid: integer; const ttyfn: string); overload;

{$ifdef unix}
type
  Ptermios = ^termios;
  Pwinsize = ^winsize;

function openpty(amaster:pcint; aslave:pcint;
  name:Pchar; termp:Ptermios; winp:Pwinsize):longint;cdecl;external clib name 'openpty';

type
  TReadEvent = procedure (Sender: TObject; const buf: string) of object;
  { TReadThread }

  TReadThread = class(TThread)
  protected
    readfd: cint;
    waitbuf: string;
    fOnInputBytes: TReadEvent;
    procedure Execute; override;
    procedure DoInputBytes;
  public
    constructor Create(afd: cint);
    property OnInputBytes: TReadEvent read fOnInputBytes write fOnInputBytes;
  end;

  { TPTY }

  TPTY = class(TObject)
  private
    fSlave    : cint;
    fMaster   : cint;
    fFileName : string;
  public
    constructor Create;
    destructor Destroy; override;
    property Master: cint read fMaster;
    property Slave: cint read fSlave;
    property FileName: string read fFileName;
  end;

  { TPTYReader }

  TPTYReader = class(TObject)
  private
    fPTY        : TPTY;
    fOnBytesRead: TReadEvent;
    fThread     : TReadThread;
    fLog        : string;
  protected
    procedure OnInput(Sender: TObject; const Buf: string);
  public
    constructor Create;
    destructor Destroy; override;
    property PTY: TPTY read fPTY;
    property OnBytesRead: TReadEvent read fOnBytesRead write fOnBytesRead;
    property Log: string read fLog;
  end;
{$endif}

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
    j:=TJSONParser.Create(s);
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

function checkOptVal(const s: string; nm: string; var vl: string): Boolean;
var
  i : integer;
begin
  i:=Pos(nm, s);
  Result:=(i>0);
  if not Result then Exit;
  //if i<0 then
  inc(i, length(nm));
  for i:=i to length(s) do
    if s[i]='=' then begin
      vl:=trim(Copy(s, i+1, length(s)));
      Result:=true;
      Exit;
    end;
  Result:=false;
end;

function InstallXcodePrj(const project, sdk, deviceid: string): Boolean;
var
  outstr: string;
  s  : string;
  l  : string;
  st : TStringList;
  i  : integer;
  ip : string; // install path
  fp : string; // content path
begin
  Result:=RunCommand('xcodebuild',
    ['install'
    ,'-project' ,project, '-sdk',sdk], outstr);

  if not Result then Exit;

  Result:=RunCommand('xcodebuild',
    ['install'
    ,'-project' ,project, '-sdk',sdk
    ,'-showBuildSettings'], outstr);
  st:=TStringList.Create;
  try
    st.Text:=outstr;
    ip:='';
    fp:='';
    for i:=st.Count-1 downto 0 do begin
      s:=st[i];
      l:=AnsiLowerCase(s);
      if (ip='') then checkOptVal(s, 'INSTALL_DIR', ip);
      if (fp='') then checkOptVal(s, 'CONTENTS_FOLDER_PATH', fp);
      if pos('build settings for', l)>0 then Break;
    end;
  finally
    st.Free;
  end;
  Result:=(ip<>'') and (fp<>'');

  if Result then begin
    Result:=RunCommand('xcrun',
      ['simctl','install'
      ,deviceid, IncludeTrailingPathDelimiter(ip)+fp ], outstr);
  end;
end;

Const
  READ_BYTES = 65536;

{ TRWProcess }

procedure TRWProcess.Run(const exename: string;
  const commands: array of string; const curdir: string);
var
  i : integer;
begin
  if Assigned(process) then CloseProcess;

  process:=TProcess.create(nil);
  process.Executable:=exename;
  if curdir<>'' then process.CurrentDirectory:=curdir;

  if high(commands)>=0 then
    for i:=low(commands) to high(commands) do
    begin
      process.Parameters.add(commands[i]);
    end;
  process.Options :=  [poUsePipes];

  process.Execute;
end;

procedure TRWProcess.Terminate(exitCode: integer);
begin
  if Assigned(process) then process.Terminate(exitCode);
end;

function TRWProcess.isRunning: Boolean;
begin
  Result:=Assigned(process) and (process.Running);
end;

procedure TRWProcess.CloseProcess;
begin
  process.Free;
  process:=nil;
end;

procedure TRWProcess.ReadProc(TimeOut: Integer);
var
  numbytes        : integer;
  available       : integer;
  outputlength    : integer;
  stderrlength    : integer;
  stderrnumbytes  : integer;
  l : integer;
begin
  outputlength:=0;
  stderrbytesread:=0;
  stderrlength:=0;
  try

    while process.Running do begin
      // Only call ReadFromStream if Data from corresponding stream
      // is already available, otherwise, on  linux, the read call
      // is blocking, and thus it is not possible to be sure to handle
      // big data amounts bboth on output and stderr pipes. PM.
      available:=process.Output.NumBytesAvailable;
      if available > 0 then begin
        while (BytesRead + available > length(outputstring)) do begin
          outputlength:=length(outputstring) + READ_BYTES;
          l:=length(outputstring);
          Setlength(outputstring,outputlength);
          FillChar(outputstring[l+1], length(outputstring)-l, #0);
        end;
        NumBytes := process.Output.Read(outputstring[1+bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
      end
      // The check for assigned(P.stderr) is mainly here so that
      // if we use poStderrToOutput in p.Options, we do not access invalid memory.
      else if assigned(process.stderr) and (process.StdErr.NumBytesAvailable > 0) then begin
        available:=process.StdErr.NumBytesAvailable;
        while (StderrBytesRead + available > length(stderrstring)) do begin
          stderrlength:=length(stderrstring) + READ_BYTES;
          l:=length(stderrstring);
          Setlength(stderrstring,stderrlength);
          FillChar(stderrstring[l+1], length(stderrstring)-l, #0);
        end;
        StderrNumBytes := process.StdErr.Read(stderrstring[1+StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end else begin
        Sleep(100);
        if TimeOut>0 then begin
          TimeOut:=TimeOut-100;
          if TimeOut<=0 then Exit;
          // This is Exit, not Break to prevent reading "available"
        end;
      end;
    end;

    // Get left output after end of execution
    available:=process.Output.NumBytesAvailable;
    while available > 0 do begin
      if (BytesRead + available > outputlength) then begin
        outputlength:=BytesRead + READ_BYTES;
        Setlength(outputstring,outputlength);
      end;
      NumBytes := process.Output.Read(outputstring[1+bytesread], available);
      if NumBytes > 0 then Inc(BytesRead, NumBytes);
      available:=process.Output.NumBytesAvailable;
    end;

    setlength(outputstring,BytesRead);
    while assigned(process.stderr) and (process.Stderr.NumBytesAvailable > 0) do begin
      available:=process.Stderr.NumBytesAvailable;
      if (StderrBytesRead + available > stderrlength) then begin
        stderrlength:=StderrBytesRead + READ_BYTES;
        Setlength(stderrstring,stderrlength);
      end;
      StderrNumBytes := process.StdErr.Read(stderrstring[1+StderrBytesRead], available);
      if StderrNumBytes > 0 then Inc(StderrBytesRead, StderrNumBytes);
    end;

    setlength(stderrstring,StderrBytesRead);
    exitstatus:=process.exitstatus;
  except
    on e : Exception do begin
      setlength(outputstring,BytesRead);
    end;
  end;

end;

constructor TRWProcess.Create;
begin
  inherited Create;
end;

destructor TRWProcess.Destroy;
begin
  CloseProcess;
  inherited Destroy;
end;

function TRWProcess.WriteLn(const s: string): Boolean;
var
  e : string;
begin
  if not Assigned(process) or (not process.Running) then begin
    Result:=false;
    Exit;
  end;
  process.Input.Write(s[1], length(s));
  e:=LineEnding;
  process.Input.Write(e[1], length(e));
  Result:=true;
end;

function TRWProcess.ReadLn(var s: string): Boolean;
var
  bk: integer;
  i: integer;
begin
  s:='';
  if not Assigned(process) or (outputstring='') then begin
    Result:=false;
    Exit;
  end;

  // read remaining bytes
  if (not process.Running) and (process.Output.NumBytesAvailable>0) then
    ReadProc(-1);

  bk:=Pos(#10, outputstring);
  if bk<=0 then bk:=Pos(#13, outputstring);
  if not process.Running and (bk<=0) then begin
    s:=outputstring;
    outputstring:='';
    bytesread:=0;
    Result:=true;
    Exit;
  end;
  Result:=bk>0;
  if not Result then Exit;

  i:=bk;
  if (bk<length(outputstring)) and (outputstring[bk+1] in [#10,#13])
   and (outputstring[bk]<>outputstring[bk+1]) then
    inc(i);
  s:=Copy(outputstring, 1, bk-1);
  outputstring:=Copy(outputstring, i+1, length(outputstring));
  dec(bytesread, i);
end;

function TRWProcess.HasLine: Boolean;
var
  bk: integer;
begin
  if not Assigned(process) or (outputstring='') then begin
    Result:=false;
    Exit;
  end;

  // read remaining bytes
  if (not process.Running) and (process.Output.NumBytesAvailable>0) then
    ReadProc(-1);

  bk:=Pos(#10, outputstring);
  if bk<=0 then bk:=Pos(#13, outputstring);
  Result:=(process.Running and (bk>0))
          or ((not process.Running) and (length(outputstring)>0));
end;

procedure TRWProcess.Await(TimeOut: integer);
begin
  if not Assigned(process) or (not process.Running) then Exit;
  ReadProc(TimeOut);
end;

procedure LLDBRediretIO(const pid, ttyfn: string); overload;
begin
  LLDBRediretIO(DEF_LLDB_EXENAME, pid, ttyfn);
end;

procedure LLDBRediretIO(pid: integer; const ttyfn: string);
begin
  LLDBRediretIO(DEF_LLDB_EXENAME, IntToStr(pid), ttyfn);
end;

procedure LLDBRediretIO(const exename: string; pid, ttyfn: string);
var
  p   : TRWProcess;
  cmd : array[0..1] of string;
  s   : string;
begin
  cmd[0]:='-p';
  cmd[1]:=pid;

  s:='';
  p := TRWProcess.Create;
  try
    p.Run(exename, cmd, '');
    p.WriteLn('version');
    p.WriteLn('breakpoint set --name main');
    p.WriteLn('breakpoint command add 1');
    p.WriteLn('p (int) dup2 ( (int) open("'+ttyfn+'",1), 2 )');
    p.WriteLn('p (int) dup2 ( (int) open("'+ttyfn+'",1), 1 )');
    p.WriteLn('detach');
    p.WriteLn('DONE');
    p.WriteLn('c');

    repeat
      p.Await(300);
      while p.HasLine do begin
        p.ReadLn(s);
        if (Pos('Process', s)>0) and (Pos('detached',s)>0) then begin
          p.Writeln('exit');
          //p.Terminate;
        end;
      end;
    until not p.isRunning;

  finally
    p.Free;
  end;
end;

{$ifdef unix}
{ TPTYReader }

procedure TPTYReader.OnInput(Sender: TObject; const Buf: string);
begin
  fLog:=fLog+Buf;
  if Assigned(OnBytesRead) then
    OnBytesRead(Self, Buf);
end;

constructor TPTYReader.Create;
begin
  inherited Create;
  fPTY:=TPTY.Create;
  fThread:=TReadThread.Create(fPTY.Master);
  fThread.OnInputBytes:=OnInput;
  fThread.Start;
end;

destructor TPTYReader.Destroy;
begin
  fPTY.Free;
  fThread.Terminate;
  fThread.WaitFor;
  fThread.Free;
  inherited Destroy;
end;

{ TPTY }

constructor TPTY.Create;
var
  ttyname : string;
  res     : integer;
begin
  inherited Create;
  SetLength(ttyname, 1024);
  res:=openpty(@fMaster, @fSlave, @ttyname[1], nil, nil);
  if res=0 then fFileName:=Copy(ttyname, 1, StrLen(@ttyname[1]));
end;

destructor TPTY.Destroy;
begin
  // Slave must be closed before Master!
  fpclose(fSlave);
  fpclose(fMaster);
  inherited Destroy;
end;

{ TReadThread }

procedure TReadThread.Execute;
var
  buf: string;
  sz: integer;
begin
  SetLength(buf, 1024);
  while not Terminated do begin
    sz:=FpRead(readfd, buf[1], length(Buf));
    if sz>0 then begin
      waitbuf:=copy(buf, 1, sz);
      Synchronize(DoInputBytes);
      waitbuf:='';
    end else if sz<=0 then begin
      Break;
    end;
  end;
end;

procedure TReadThread.DoInputBytes;
begin
  if Assigned(OnInputBytes) then OnInputBytes(Self, waitbuf);
end;

constructor TReadThread.Create(afd: cint);
begin
  inherited Create(true);
  readfd:=afd;
end;

{$endif}

end.

