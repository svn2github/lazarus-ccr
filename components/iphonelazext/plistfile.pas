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

{$mode delphi}

interface

uses
  Classes, SysUtils, DOM, XMLRead;

type
  TPlistType = (ltString, ltArray, ltDict, ltData, ltDate, ltBoolean, ltNumber);

  { TPListValue }

  TPListValue = class(TObject)
  private
    fType : TPlistType;
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
    function AddValue: Integer;
    property ValueType: TPListType read fType;
  end;

  { TPListFile }

  TPListFile = class(TObject)
  public
    root : TPListValue;
    destructor Destroy; override;
  end;

function LoadFromXML(const fn: string; plist: TPListFile): Boolean; overload;
function LoadFromXML(doc: TXMLDocument; plist: TPListFile): Boolean; overload;

procedure DebugPlistFile(const fl: TPListFile; Recursive: Boolean = false);

function WriteXML(const plist: TPlistFile): string;

implementation

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

procedure DebugPlistFile(const fl: TPListFile; Recursive: Boolean = false);
begin
  DebugValue(fl.root,'');
end;

const
  prefix=
  '<?xml version="1.0" encoding="UTF-8"?>'+LineEnding+
  '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">'+LineEnding+
  '<plist version="1.0">'+LineEnding;

function WriteXML(const plist: TPlistFile): string;
begin
  Result:=prefix;
  Result:=Result+'</plist>';
end;

function LoadFromXML(const fn: string; plist: TPListFile): Boolean; overload;
var
  doc : TXMLDocument;
begin
  ReadXMLFile(doc, fn);
  Result:=LoadFromXML(doc, plist);
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
      kv.names[idx]:=nd.TextContent;
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
  t : string;
  tp : TPlistType;
begin
  Result:=nil;
  if not Assigned(valnode) then Exit;
  if not NodeNameToPListType(valnode.NodeName, tp) then Exit;
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
  plist.root:=ReadValByNode(nd);
  Result:=true;
end;

destructor TPListFile.Destroy;
begin
  root.Free;
  inherited Destroy;
end;

{ TPListValue }

constructor TPListValue.Create(AType: TPlistType);
begin
  inherited Create;
  fType:=AType;
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

end.

