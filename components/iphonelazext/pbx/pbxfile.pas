unit pbxfile;

interface

(*-------------------------------------------------------------------------------
* by Dmitry Boyarintsev - Oct 2014                                              *
*                                                                               *
* license: free for use, but please leave a note to the origin of the library   *
*                                                                               *
* pbxfile is JSON file format by Apple. Unlike JSON, it allows to add comments. *
* (was it introduced in NextStep system?)                                       *
* other differences                                                             *
*   no explicit type in values/identifiers                                      *
*   . and / are valud value/identifier character                                *
*   ; - is a separator in values                                                *
*   () - is an array. the last element can (should?) end up with comma          *
*   {} - is an object (just like json)                                          *
*  escaping characters with C-style escaping:                                   *
*    * quotes (")                                                               *
*    * line breaks (note OSX is typically using \n, unlike Unix \r)             *
*                                                                               *
*                                                                               *
* TPBXScanner - scans through the file                                          *
* TPBXParser - parses the file, returning a higher level entities of the file:  *
*             values, open/close of object/array                                *
* The parser doesn't produce any kind of structure. Instead it only allows to   *
* build one. i.e. PBXContainer                                                  *
-------------------------------------------------------------------------------*)

{$ifdef fpc}{$mode delphi}{$endif}

uses
  SysUtils, StrUtils;

type
  TPBXToken = (
    tkEOF,
    tkComma,           // ','
    tkSemiColon,       // ';'
    tkEqual,           // '='
    tkCurlyBraceOpen,  // '{'
    tkCurlyBraceClose, // '}'
    tkRoundBraceOpen,  // '('
    tkRoundBraceClose, // ')'
    tkIdentifier,
    tkUnknown
  );


  TCommentEvent = procedure (Sender: TObject; const cmtText: string) of object;
  { TPBXScanner }

  TPBXScanner = class(TObject)
  private
    buf       : string;
    idx       : Integer;
    FCurLine  : string;
    FCurRow   : Integer;
    FCurToken : TPBXToken;
    FCurTokenString: string;
    function GetCurColumn: Integer;
  protected
    procedure DoComment(const cmt: string);
    procedure SkipComment(const EndOfLine: Boolean);
    function DoFetchToken: TPBXToken;
  public
    OnComment: TCommentEvent;
    procedure SetBuf(const abuf: string);
    function FetchToken: TPBXToken;

    property CurLine: string read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TPBXToken read FCurToken;
    property CurTokenString: string read FCurTokenString;
  end;

  TPBXEntity = (
    etOpenArray, etCloseArray
    , etOpenObject, etCloseObject
    , etValue
    , etEOF
    , etError
  );
  TPBXParserState = (stInit, stObject, stObjectNext, stArray, stArrayNext, stError);

  { TPBXParser }

  TPBXParser = class(TObject)
  private
    fState    : TPBXParserState;
    fStStack  : array of TPBXParserState;
    fStCount  : Integer;

    fFetchComment: TCommentEvent;
    procedure PushState(AState: TPBXParserState);
    function PopState: TPBXParserState;

    function DefaultFetch(tk: TPBXToken): TPBXEntity;
    procedure DoScanComment(sender: TObject; const acomment: string);
  public
    scanner     : TPBXScanner;
    Name        : string;
    Value       : string;
    CurEntity   : TPBXEntity;
    LastComment : string;
    procedure Reset;
    function FetchNextEntity: TPBXEntity;
    constructor Create;
    destructor Destroy; override;
    property Level: Integer read fStCount;
  end;

  { TPBXWriter }

  TPBXWriter = class(TObject)
  private
    fbuf : string;
    idx  : Integer;
  protected
    fprefix : Integer;
    fManualLineBreak : Boolean;
    fisNewLine : Boolean;
    //fstack  : array of fstack;
    procedure IncPrefix;
    procedure DecPrefix;
    function GetBuf: string;
    procedure DoWriteRaw(const s: string);
    procedure DoWrite(const s: string);
    procedure DoLineBreak;
  public
    constructor Create;
    procedure OpenBlock(const openchar: string);
    procedure CloseBlock(const closechar: string);
    procedure WriteRaw(const s: string);
    procedure WriteLineBreak;
    procedure WriteLineComment(const s: string);
    procedure WriteName(const nm: string; const cmt: string = '');
    procedure WriteValue(const v: string; const cmt: string = '');
    procedure WriteArrValue(const v: string; const cmt: string = '');
    procedure WriteNamedValue(const nm, v: string; const cmt: string = '');
    property Buffer: string read GetBuf;
    property ManualLineBreak: Boolean read fManualLineBreak write fManualLineBreak;
  end;

const
  CharOffset    = #$09;
  CharLineBreak = #$0A;
  CharSeparator = ';';
  CharArrSeparator = ',';
  CharSpace     = #$20;

procedure ScanAString(const test: string);
procedure ParseAString(const test: string);

function PBXParserSkipLevel(p: TPBXParser): Boolean;
function PBXRawWriteValue(const v: string): string;

function Unescape(const s: string): string;

implementation

type
  TCharSet = set of char;

const
  LineBreaks       = [#10, #13];
  WhiteSpace       = [#9,#8,#32];
  WhiteSpaceBreaks = LineBreaks+WhiteSpace;
  Alpha            = ['a'..'z','A'..'Z'];
  Numeric          = ['0'..'9'];
  AlphaNumeric     = Alpha+Numeric;
  IdentName        = AlphaNumeric+['_','.','/']; // . and / are allowed in values
  ToEscape         = ['"',#13,#9,#10,'\'];
  // commas are not

function Unescape(const s: string): string;
var
  i   : Integer;
  j   : Integer;
  cnt : Integer;
begin
  if s='' then begin
    Result:='';
    Exit;
  end;
  SetLength(Result, length(s));
  cnt := length(s);
  i:=1;
  j:=1;
  while i<=cnt do begin
    if s[i]='\' then begin
      inc(i);
      if i<=cnt then begin
        case s[i] of
          'r': Result[j]:=#10;
          'n': Result[j]:=#13;
          't': Result[j]:=#9;
          '0': Result[j]:=#0;
        else
          Result[j]:=s[i];
        end;
        inc(j);
        inc(i);
      end;
    end else begin
      Result[j]:=s[i];
      inc(j);
      inc(i);
    end;
  end;
  Result:=Copy(Result, 1, j-1);
end;

function PBXRawWriteValue(const v: string): string;
var
  i : Integer;
  k : Integer;
begin
  Result:='';
  k:=0;
  for i:=1 to length(v) do begin
    if not (v[i] in IdentName) then begin
      if Result='' then begin
        SetLength(Result, length(v)*2+2);
        Result[1]:='"';
        Move(v[1], Result[2], i);
        k:=i+1;
      end;
      if (v[i] in ToEscape) then begin
        Result[k]:='\';
        inc(k);
        case v[i] of
          '"': Result[k]:='"';
          #13: Result[k]:='n';
          #10: Result[k]:='r';
           #9: Result[k]:='t';
          '\': Result[k]:='\';
        end;
        inc(k);
      end else begin
        Result[k]:=v[i];
        inc(k);
      end;
    end else if k>0 then begin
      Result[k]:=v[i];
      inc(k);
    end;
  end;
  if k=0 then
    Result:=v
  else begin
    Result[k]:='"';
    SetLength(Result,k);
  end;
end;

function ScanTo(const s: string; var idx: Integer; ToChars: TCharSet): string;
var
  i : integer;
begin
  i:=idx;
  while (idx<=length(s)) and not (s[idx] in ToChars) do inc(idx);
  Result:=Copy(s, i, idx-i);
end;

function ScanWhile(const s: string; var idx: Integer; WhileChars: TCharSet): string;
var
  i : integer;
begin
  i:=idx;
  while (idx<=length(s)) and (s[idx] in WhileChars) do
    inc(idx);
  Result:=Copy(s, i, idx-i);
end;

{ TPBXWriter }

procedure TPBXWriter.IncPrefix;
begin
  inc(fprefix);
end;

procedure TPBXWriter.DecPrefix;
begin
  dec(fprefix);
end;

function TPBXWriter.GetBuf: string;
begin
  Result:=fbuf;
  SetLength(Result, idx-1);
end;

procedure TPBXWriter.DoWriteRaw(const s: string);
var
  sz : Integer;
  bufsz : Integer;
begin
  if s ='' then Exit;
  sz:=length(s)+idx-1;
  bufsz:=length(fbuf);
  while bufsz<sz do begin
    if bufsz=0 then bufsz:=1024
    else bufsz:=bufsz*2;
  end;
  SetLength(fbuf, bufsz);
  Move(s[1], fbuf[idx], length(s));
  inc(idx, length(s));
  fisNewLine:=false;
end;

procedure TPBXWriter.DoWrite(const s: string);
var
  pfx : string;
begin
  if fisNewLine and (fprefix>0) then begin
    SetLength(pfx, fprefix);
    FillChar(pfx[1], fprefix, CharOffset);
    DoWriteRaw(pfx);
  end;
  DoWriteRaw(s);
end;

procedure TPBXWriter.DoLineBreak;
begin
  DoWriteRaw(CharLineBreak);
  fisNewLine:=true;
end;

constructor TPBXWriter.Create;
begin
  idx:=1;
end;

procedure TPBXWriter.OpenBlock(const openchar: string);
begin
  DoWrite(openchar);
  IncPrefix;
  if not fManualLineBreak then DoLineBreak;
end;

procedure TPBXWriter.CloseBlock(const closechar: string);
begin
  DecPrefix;
  DoWrite(closechar);
  if fprefix>0 then DoWriteRaw(CharSeparator);
  if not fManualLineBreak then DoLineBreak;
end;

procedure TPBXWriter.WriteRaw(const s: string);
begin
  DoWriteRaw(s);
end;

procedure TPBXWriter.WriteLineBreak;
begin
  DoLineBreak;
end;

procedure TPBXWriter.WriteLineComment(const s: string);
begin
  DoWriteRaw('/* ');
  DoWriteRaw(s);
  DoWriteRaw(' */');
  DoLineBreak;
end;

procedure TPBXWriter.WriteName(const nm: string; const cmt: string = '');
begin
  if nm='' then Exit;
  DoWrite(PBXRawWriteValue(nm));
  if cmt<>'' then begin
    DoWriteRaw(' /* ');
    DoWriteRaw(cmt);
    DoWriteRaw(' */');
  end;
  DoWriteRaw(' = ');
end;

procedure TPBXWriter.WriteValue(const v: string; const cmt: string = '');
begin
  if v ='' then DoWriteRaw('""')
  else DoWriteRaw(PBXRawWriteValue(v));
  if cmt<>'' then begin
    DoWriteRaw(' /* ');
    DoWriteRaw(cmt);
    DoWriteRaw(' */');
  end;
  DoWriteRaw(CharSeparator);
  if not fManualLineBreak then DoLineBreak
  else DoWriteRaw(CharSpace);
end;

procedure TPBXWriter.WriteArrValue(const v: string; const cmt: string);
begin
  DoWrite(PBXRawWriteValue(v));
  if cmt<>'' then begin
    DoWriteRaw(' /* ');
    DoWriteRaw(cmt);
    DoWriteRaw(' */');
  end;
  DoWriteRaw(CharArrSeparator);
  if not fManualLineBreak then DoLineBreak
  else DoWriteRaw(CharSpace);
end;

procedure TPBXWriter.WriteNamedValue(const nm, v: string; const cmt: string);
begin
  WriteName(nm);
  WriteValue(v, cmt);
end;

{ TPBXParser }

procedure TPBXParser.PushState(AState: TPBXParserState);
begin
  if fStCount=length(fStStack) then begin
    if fStCount=0 then SetLength(fStStack, 4)
    else SetLength(fStStack, fStCount*2);
  end;
  fStStack[fStcount]:=AState;
  inc(fStcount);
  fState:=AState;
end;

function TPBXParser.PopState: TPBXParserState;
begin
  dec(fStCount);
  if fStCount>0 then begin
    fState:=fStStack[fStCount-1];
    if fState = stObject then fState:=stObjectNext
    else if fState = stArray then fState:=stArrayNext;
  end else
    fState:=stInit;
  Result:=fState;
end;

function TPBXParser.DefaultFetch(tk: TPBXToken): TPBXEntity;
begin
  case tk of
    tkIdentifier: begin
      Value:=scanner.CurTokenString;
      Result:=etValue;
    end;
    tkCurlyBraceOpen: begin
      Result:=etOpenObject;
      PushState(stObject)
    end;
    tkRoundBraceOpen: begin
      Result:=etOpenArray;
      PushState(stArray)
    end;
  else
    Result:=etError;
  end;
end;

procedure TPBXParser.DoScanComment(sender: TObject; const acomment: string);
begin
  LastComment:=acomment;
  if Assigned(fFetchComment) then
    fFetchComment(sender, acomment);
end;

procedure TPBXParser.Reset;
begin
  fState:=stInit;
end;

function TPBXParser.FetchNextEntity: TPBXEntity;
var
  tk    : TPBXToken;
  done  : Boolean;
begin
  LastComment:='';
  Name:='';
  Value:='';
  Result:=etError;
  case fState of
    stInit :
      case scanner.FetchToken of
        tkCurlyBraceOpen:
        begin
          PushState(stObject);
          Result:=etOpenObject;
        end;
        tkEOF:
          Result:=etEOF;
      else
        Result:=etError;
      end;
    stObject, stObjectNext:
      repeat
        done:=true;
        case scanner.FetchToken of
          tkSemiColon: begin
            if fState = stObjectNext then begin
              done:=false;
              fState:=stObject;
            end else
              Result:=etError;
          end;
          tkCurlyBraceClose: begin
            PopState;
            Result:=etCloseObject;
          end;
          tkIdentifier:
          begin
            Name:=scanner.CurTokenString;
            LastComment:='';
            if scanner.FetchToken <> tkEqual then begin
              Result:=etError;
            end else begin
              tk:=scanner.FetchToken;
              Result:=DefaultFetch(tk);
              if Result=etValue then fState:=stObjectNext;
            end;
          end;
        end;
      until done;
    stArray, stArrayNext: begin
      repeat
        done:=true;
        tk:=scanner.FetchToken;
        case tk of
          tkComma: begin
            if fState = stArrayNext then begin
              fState:=stArray;
              done:=false;
            end else
              Result:=etError; // unexpected comma
          end;
          tkRoundBraceClose: begin
            PopState;
            Result:=etCloseArray;
          end;
        else
          Result:=DefaultFetch(tk);
          if Result=etValue then fState:=stArrayNext;
        end;
      until done;
    end;
    stError:
      Result:=etError;
  end;
  if Result=etError then
    fState:=stError;
  CurEntity:=Result;
end;

constructor TPBXParser.Create;
begin
  inherited Create;
  scanner:=TPBXScanner.Create;
  scanner.OnComment:=DoScanComment;
  Reset;
end;

destructor TPBXParser.Destroy;
begin
  scanner.Free;
  inherited Destroy;
end;


{ TPBXScanner }

function TPBXScanner.GetCurColumn: Integer;
begin
  Result:=0;
end;

procedure TPBXScanner.DoComment(const cmt: string);
begin
  if Assigned(OnComment) then OnComment(Self, cmt);
end;

procedure TPBXScanner.SkipComment(const EndOfLine: Boolean);
var
  cmt : string;
  i   : integer;
  cnt : string;
begin
  if EndOfLine then begin
    cmt:=ScanTo(buf, idx, LineBreaks);
    cnt:=trim(cmt);
  end else begin
    i:=PosEx('*/', buf, idx+2);
    cnt:=trim(Copy(buf, idx+2, i-idx-2));
    if i>0 then inc(i,2);
    cmt:=Copy(buf, idx, i-idx);
    inc(idx, length(cmt));
  end;
  DoComment(cnt);
end;

function TPBXScanner.DoFetchToken: TPBXToken;
var
  donestr: Boolean;
begin
  if idx>length(buf) then begin
    Result:=tkEOF;
    Exit;
  end;

  // skipping comments
  while true do begin
    ScanWhile(buf, idx, WhiteSpaceBreaks);
    if (idx<length(buf)) and (buf[idx]='/') then begin
      if (buf[idx+1]='*') then
        SkipComment(false)
      else if buf[idx+1]='/' then begin
        SkipComment(true);
      end else begin
        Break;
      end;
    end else
      Break;
  end;
  if idx>length(buf) then begin
    Result:=tkEOF;
    Exit;
  end;

  if buf[idx] in IdentName then begin
    Result:=tkIdentifier;
    FCurTokenString:=ScanWhile(buf, idx, IdentName);
  end else
    case buf[idx] of
    '"': begin
      inc(idx);
      Result:=tkIdentifier;
      donestr:=false;
      FCurTokenString:='';
      repeat
        FCurTokenString:=FCurTokenString+ScanTo(buf, idx, ['"']);
        donestr:=(buf[idx-1]<>'\');
        if not donestr then begin
          FCurTokenString:=FCurTokenString+'"';
          inc(idx);
        end;
      until donestr;
      FCurTokenString:=Unescape(FCurTokenString);
      inc(idx);
    end;
    '=': begin
      Result:= tkEqual;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;
    '{': begin
      Result:=tkCurlyBraceOpen;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;
    '}': begin
      Result:=tkCurlyBraceClose;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;
    ')': begin
      Result:=tkRoundBraceClose;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;
    '(': begin
      Result:=tkRoundBraceOpen;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;
    ';': begin
      Result:=tkSemiColon;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;
    ',': begin
      Result:=tkComma;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;
    else
      Result:= tkUnknown;
      FCurTokenString:=buf[idx];
      inc(idx);
    end;

end;

procedure TPBXScanner.SetBuf(const abuf: string);
begin
  buf:=abuf;
  idx:=1;
end;

function TPBXScanner.FetchToken: TPBXToken;
begin
  Result:=DoFetchToken;
  FCurToken:=Result;
end;

procedure ScanAString(const test: string);
var
  sc : TPBXScanner;
begin
  sc := TPBXScanner.Create;
  try
    sc.SetBuf(test);
    while sc.FetchToken<>tkEOF do begin
      if sc.CurToken=tkUnknown then begin
        writeln(sc.CurToken:20,' ', IntToHex( byte(sc.CurTokenString[1]), 2 ) );
        writeln('idx = ', sc.idx);
      end else
        ;
        //writeln(sc.CurToken:20,' ', sc.CurTokenString);
    end;
  finally
    sc.Free;
  end;
end;

procedure ParseAString(const test: string);
var
  pr : TPBXParser;
  et : TPBXEntity;
begin
  pr := TPBXParser.Create;
  try
    pr.scanner.SetBuf(test);
    et:=pr.FetchNextEntity;
    while et <> etEOF  do begin
      if pr.Name<>'' then write('"',pr.Name,'":');

      case et of
        etValue: writeln('"',pr.Value,'",');
        etCloseObject: writeln('},');
        etOpenObject:  writeln('{');
        etOpenArray:   writeln('[');
        etCloseArray:  writeln('],');
      else
        writeln(et);
      end;
      if et = etError then Break;
      //writeln(pr.fState);
      et:=pr.FetchNextEntity;
    end;
  finally
    pr.Free;
  end;
end;

function PBXParserSkipLevel(p: TPBXParser): Boolean;
var
  lvl : Integer;
  tk  : TPBXEntity;
begin
  Result:=false;
  if not Assigned(p) then Exit;
  lvl:=p.Level;
  while (p.Level>=lvl) do begin
    tk:=p.FetchNextEntity;
    if tk=etError then begin
      Result:=false;
      Exit;
    end;
  end;
  Result:=true;
end;

end.
