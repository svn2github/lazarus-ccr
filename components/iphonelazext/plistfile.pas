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
unit PlistFile;

{$mode delphi}{$h+}

interface

uses
  Classes, SysUtils, DOM, XMLRead{$ifdef darwin},process{$endif};

type
  TPlistType = (ltString, ltArray, ltDict, ltData, ltDate, ltBoolean, ltNumber);

  { TPListValue }

  TPListValue = class(TObject)
  private
    fType : TPlistType;
  protected
    function GetValueIndex(const aname: string; force: Boolean): Integer;
  public
    str       : WideString;
    binary    : array of byte;
    date      : TDateTime;
    bool      : Boolean;
    number    : Double;

    count     : Integer;
    items     : array of TPListValue;
    names     : array of string;
    constructor Create(AType: TPlistType);
    destructor Destroy; override;
    function AddValue: Integer;
    function FindValue(const nm: string): Integer;
    procedure Delete(idx: Integer);
    procedure Clear;
    property ValueType: TPListType read fType;
  end;

  { TPListFile }

  TPListFile = class(TObject)
  protected
  public
    root : TPListValue;
    constructor Create;
    destructor Destroy; override;
  end;

function LoadFromXML(const fn: string; plist: TPListFile): Boolean; overload;
function LoadFromXML(doc: TXMLDocument; plist: TPListFile): Boolean; overload;
function WriteXML(const plist: TPlistFile): string;
function SaveToXMLFile(plist: TPlistFile; const fn: string): Boolean;

procedure DebugPlistFile(const fl: TPListFile);

function LoadFromFile(const fn: string; plist: TPListFile): Boolean;

function GetStr(dict: TPListValue; const AName: string): string; overload;
function GetStr(pl: TPListFile; const AName: string): string; overload; inline;

procedure SetStr(vl: TPListValue; const AValue: WideString); overload;
procedure SetStr(dict: TPListValue; const AName: string; const AValue: WideString); overload;
procedure SetStr(arr: TPListValue; idx: Integer; const AValue: WideString); overload;
procedure SetStr(pl: TPListFile; const AName: string; const AValue: WideString); overload; inline;

procedure SetBool(dict: TPListvalue; const AName: string; AValue: Boolean); overload;
procedure SetBool(pl: TPListFile;  const AName: string; AValue: Boolean); overload; inline;

function AddItem(vl: TPListValue; const name: string; tp: TPListType = ltString): Integer; overload;
function AddItem(vl: TPListValue; tp: TPListType = ltString ): Integer; overload;
procedure AddStr(arr: TPListValue; const AValue: WideString);

function SetArr(vl: TPListValue; const AName: string): TPListValue; overload;
function SetArr(pl: TPListFile; const AName: string): TPListValue; overload; inline;

implementation

function AddItem(vl: TPListValue; const name: string; tp: TPListType = ltString): Integer; overload;
begin
  Result:=AddItem(vl, tp);
  if vl.ValueType=ltDict then
    vl.names[Result]:=name;
end;

function AddItem(vl: TPListValue; tp: TPListType = ltString ): Integer; overload;
var
  idx : integer;
begin
  if not Assigned(vl) then begin
    Result:=-1;
    Exit;
  end;
  idx:=vl.AddValue;
  if not Assigned(vl.items[idx]) then
    vl.items[idx]:=TPListValue.Create(tp);
  Result:=idx;
end;

procedure DebugValue(kv: TPListValue; const prefix: string );
var
  i : integer;
begin
  for i:=0 to kv.count-1 do begin
    if kv.fType=ltDict then
      writeln(prefix,kv.names[i],' (',kv.items[i].ValueType,')');

    case kv.items[i].fType of
      ltString:  writeln(prefix+'  ', kv.items[i].str);
      ltBoolean: writeln(prefix+'  ', kv.items[i].bool);
      ltDict: begin
        writeln;
        DebugValue(kv.items[i],prefix+'  ');
      end;
      ltArray: begin
        //writeln;
        DebugValue(kv.items[i],prefix+'  ');
      end;
    end;
  end;
end;

procedure DebugPlistFile(const fl: TPListFile);
begin
  DebugValue(fl.root,'');
end;

function LoadFromFile(const fn: string; plist: TPListFile): Boolean;
var
  st : TFileStream;
  buf : string[5];
  res : integer;
  {$ifdef darwin}
  xs  : string;
  m   : TStringStream;
  doc : TXMLDocument;
  {$endif}
begin
  //todo: detect plist type and convert is necessary
  st:=TFileSTream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    SetLength(buf, 5);
    res:=st.Read(buf[1], 5);
  finally
    st.Free;
  end;
  if (res=5) and (buf='<?xml') then begin
    Result:=LoadFromXML(fn, plist)
  end else begin
    {$ifdef darwin}
    // the utility is not available anywhere else but OSX
    if not RunCommand('plutil', ['-convert','xml1','-o' ,'-', fn], xs) then begin
      Result:=false;
      Exit;
    end;
    m:=TStringStream.Create(xs);
    try
      ReadXMLFile(doc, m);
      Result:=LoadFromXML(doc, plist);
      doc.Free;
    finally
      m.Free;
    end;
    {$else}
    Result:=false;
    {$endif}
  end;
end;

const
  PlistXMLPrefix=
  '<?xml version="1.0" encoding="UTF-8"?>'+LineEnding+
  '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">'+LineEnding+
  '<plist version="1.0">';

const
  EncText = ['<', '>', '&'];
  amp = '&amp;';
  lt = '&lt;';
  gt = '&gt;';

function XMLEncodeText(const v: WideString): string;
var
  i : integer;
  j : Integer;
  k : integer;
  b : string;
  rp : string;
begin
  Result:='';
  b:=UTF8Encode(v);
  j:=1;
  for i:=1 to length(b) do begin
    if b[i] in EncText then begin
      if length(Result)=0 then begin
        SetLength(Result, length(b)*5);
        k:=1;
      end;
      Move(b[j], Result[k], i-j);
      inc(k, i-j);
      case b[i] of
        '<': rp:=lt;
        '>': rp:=gt;
        '&': rp:=amp;
      end;
      j:=i+1;
      Move(rp[1], Result[k], length(rp));
      inc(k, length(rp));
    end;

  end;

  if (Result='') and (b<>'') then
    Result:=b
  else begin
    if b='' then
      Result:=''
    else begin
      if j<length(b) then begin
        i:=length(b)+1;
        Move(b[j], Result[k], i-j);
        inc(k, i-j);
      end;
      SetLength(Result, k-1);
    end;
  end;
end;

const
  XMLPFX = #9;

procedure WriteXMLValue(v: TPListValue; dst: TStrings; const pfx: string);
const
  boolTag : array [boolean] of string = ('<false/>','<true/>');
var
  i : integer;
begin
  case v.ValueType of
    ltBoolean: dst.Add(pfx+boolTag[v.bool]);
    ltString: dst.Add(pfx+'<string>'+XMLEncodeText(v.str)+'</string>');
    ltDict: begin
      dst.Add(pfx+'<dict>');
      for i:=0 to v.count-1 do begin
        dst.Add(XMLPFX+'<key>'+XMLEncodeText(UTF8Decode(v.names[i]))+'</key>');
        WriteXMLValue(v.items[i], dst, pfx+XMLPFX);
      end;
      dst.Add(pfx+'</dict>');
    end;
    ltArray: begin
      dst.Add(pfx+'<array>');
      for i:=0 to v.count-1 do
        WriteXMLValue(v.items[i], dst, pfx+XMLPFX);
      dst.Add(pfx+'</array>');
    end;
  end;
end;

function WriteXML(const plist: TPlistFile): string;
var
  st: TSTringList;
begin
  st:=TSTringList.Create;
  try
    st.Add(PlistXMLPrefix);
    WriteXMLValue(plist.root, st, '');
    st.Add('</plist>');
    Result:=st.Text;
  finally
    st.Free;
  end;
end;

function SaveToXMLFile(plist: TPlistFile; const fn: string): Boolean;
var
  fs  : TFileStream;
  s   : string;
begin
  if not Assigned(plist) then begin
    Result:=false;
    Exit;
  end;
  try
    fs:=TfileStream.Create(fn, fmCreate);
    try
      s:=WriteXML(plist);
      if length(s)>0 then
        fs.Write(s[1], length(s));
      Result:=true;
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;
end;


function LoadFromXML(const fn: string; plist: TPListFile): Boolean; overload;
var
  doc : TXMLDocument;
begin
  ReadXMLFile(doc, fn);
  Result:=LoadFromXML(doc, plist);
  doc.Free;
end;

function ReadValByNode(valnode: TDomNode): TPListValue; forward;

function NodeNameToPListType(const nd: string; var pl: TPlistType) : Boolean;
begin
  Result:=true;
  if nd='string' then pl:=ltString
  else if nd ='array' then pl:=ltArray
  else if (nd ='fasle') or (nd = 'true') then pl:=ltBoolean
  else if (nd = 'dict') then pl:=ltDict
  //TPlistType = (ltData, ltDate, ltNumber);
  else Result:=false;
end;

procedure ReadArrVal(parent: TDomNode; kv: TPListValue);
var
  idx : Integer;
  nd  : TDomNode;
begin
  if not Assigned(parent) then Exit;

  nd:=parent.FirstChild;
  while Assigned(nd) do begin
    idx:=kv.AddValue;
    kv.items[idx]:=ReadValByNode(nd);
    nd:=nd.NextSibling;
  end;
end;

procedure ReadKeyVal(parent: TDomNode; kv: TPListValue);
var
  nd  : TDOMNode;
  idx : integer;
begin
  if not Assigned(parent) then Exit;

  nd:=parent.FirstChild;
  while Assigned(nd) do begin
    if nd.NodeName='key' then begin
      idx:=kv.AddValue;
      kv.names[idx]:=UTF8Encode(nd.TextContent);
      nd:=nd.NextSibling;
      if Assigned(nd) then begin
        kv.items[idx]:=ReadValByNode(nd);
        nd:=nd.NextSibling;
      end;
    end else
      nd:=nd.NextSibling;
  end;
end;

function ReadValByNode(valnode: TDomNode): TPListValue;
var
  tp : TPlistType;
begin
  Result:=nil;
  if not Assigned(valnode) then Exit;
  if not NodeNameToPListType( UTF8Encode(valnode.NodeName), tp) then Exit;
  Result:=TPListValue.Create(tp);
  case tp of
    ltBoolean: Result.bool:=(valnode.NodeName='true'); // false is false
    ltString:  Result.str:=valnode.TextContent;
    ltArray:   ReadArrVal(valnode, Result);
    ltDict:    ReadKeyVal(valnode, Result);
  end;
end;

function LoadFromXML(doc: TXMLDocument; plist: TPListFile): Boolean; overload;
var
  root  : TDOMNode;
  nd    : TDOMNode;
  r     : TPListValue;
begin
  Result:=false;
  root:=doc.FirstChild; //('plist');
  if not Assigned(root) then Exit;

  while Assigned(root) do begin
    if (root.NodeType = ELEMENT_NODE) and (root.NodeName = 'plist') then
      Break;
    root:=root.NextSibling;
  end;
  if not Assigned(root) then Exit;

  nd:=root.FirstChild;
  r:=plist.root;
  plist.root:=ReadValByNode(nd);
  if Assigned(plist.root) then r.Free;
  Result:=true;
end;

function TPListValue.GetValueIndex(const aname: string; force: Boolean): Integer;
var
  idx: integer;
begin
  idx:=FindValue(aname);
  if (idx<0) and (force) then begin
    idx:=AddValue;
    if not Assigned(Items[idx]) then
      items[idx]:=TPListValue.Create(ltString);
    names[idx]:=aname;
  end;
  Result:=idx;
end;

constructor TPListFile.Create;
begin
  inherited Create;
  root:=TPListValue.Create(ltDict);
end;

destructor TPListFile.Destroy;
begin
  root.Free;
  inherited Destroy;
end;

function GetStr(dict: TPListValue; const AName: string): string;
var
  i : integer;
begin
  if not Assigned(dict) or (dict.ValueType<>ltDict) then begin
    Result:='';
    Exit;
  end;

  for i:=0 to dict.count-1 do
    if dict.names[i]=AName then begin
      Result:=UTF8Encode(dict.items[i].str);
      Exit;
    end;
  Result:='';
end;

procedure SetStr(vl: TPListValue; const AValue: WideString);
begin
  if not Assigned(vl) then Exit;
  vl.str:=AValue;
  vl.fType:=ltString;
end;

procedure SetStr(dict: TPListValue; const AName: string; const AValue: WideString);
var
  idx: integer;
begin
  idx:=dict.GetValueIndex(AName, true);
  SetStr(dict.items[idx], Avalue);
end;

procedure SetBool(dict: TPListvalue; const AName: string; AValue: Boolean); overload;
var
  idx: integer;
begin
  idx:=dict.GetValueIndex(AName, true);
  dict.items[idx].bool:=AValue;
  dict.items[idx].fType:=ltBoolean;
end;

procedure SetStr(pl: TPListFile; const AName: string; const AValue: WideString); overload; inline;
begin
  SetStr(pl.root, AName, AValue);
end;

procedure SetBool(pl: TPListFile;  const AName: string; AValue: Boolean); overload; inline;
begin
  SetBool(pl.root, AName, AValue);
end;

procedure SetStr(arr: TPListValue; idx: Integer; const AValue: WideString); overload;
begin
  if not Assigned(arr) or (arr.ValueType<>ltArray) or (idx<0) or (idx>arr.count) then Exit;
  if idx=arr.count then
    AddStr(arr, Avalue)
  else begin
    if not Assigned(arr.items[idx]) then arr.items[idx]:=TPListValue.Create(ltString);
    SetStr(arr.items[idx], AValue);
  end;
end;

procedure AddStr(arr: TPListValue; const AValue: WideString);
var
  idx: integer;
begin
  idx:=AddItem(arr, ltString);
  arr.items[idx].str:=AValue;
end;

function GetStr(pl: TPListFile; const AName: string): string; overload; inline;
begin
  Result:=GetStr(pl.root, AName);
end;

function SetArr(vl: TPListValue; const AName: string): TPListValue; overload;
var
  idx: integer;
begin
  idx:=vl.GetValueIndex(AName, true);
  Result:=vl.items[idx];
  Result.fType:=ltArray;
end;

function SetArr(pl: TPListFile; const AName: string): TPListValue; overload; inline;
begin
  Result := SetArr(pl.root, AName);
end;

{ TPListValue }

constructor TPListValue.Create(AType: TPlistType);
begin
  inherited Create;
  fType:=AType;
end;

destructor TPListValue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TPListValue.AddValue: Integer;
begin
  if not (fType in [ltArray, ltDict]) then begin
    Result:=0;
    Exit;
  end;
  Result:=count;
  if count=length(items) then begin
    if count=0 then SetLength(items, 4)
    else SetLength(items, length(items)*2);
    if fType=ltDict then SetLength(names, length(items));
  end;
  inc(count);
end;

function TPListValue.FindValue(const nm: string): Integer;
var
  i : integer;
begin
  for i:=0 to count-1 do
    if names[i]=nm then begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

procedure TPListValue.Delete(idx: Integer);
begin
  if (idx<0) or (idx>=count) then Exit;
  items[idx].Free;
  if ValueType=ltDict then names[idx]:='';

  if idx<count-1 then begin
    Move(items[idx+1], items[idx], (count-idx)*sizeof(TPListValue));
    if ValueType=ltDict then begin
      Move(names[idx+1], names[idx], (count-idx)*sizeof(PtrUInt));
      names[count-1]:='';
    end;
  end;
  dec(count);
  items[count]:=nil;
end;

procedure TPListValue.Clear;
var
  i : Integer;
begin
  for i:=0 to length(items)-1 do
    if Assigned(items[i]) then
      items[i].Free;
  count:=0;
  SetLength(names, 0);
  SetLength(items, 0);
end;

end.

