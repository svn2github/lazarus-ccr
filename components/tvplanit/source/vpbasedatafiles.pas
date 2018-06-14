{ Visual PlanIt basic data files for import.
  Is used for import of vCard and iCal files }

unit VpBaseDataFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TVpFileItem = class
  protected
    FRaw: String;
    FKey: String;
    FTags: TStrings;
    FValue: String;
    procedure GetParts(AText: String; out AKey: String; out ATags: TStringArray;
      out AValue: String);
    function UnEscape(AValueText: String): String;
    function UnquotePrintable(AValueText: String): String;
  public
    constructor Create(AText: String);
    destructor Destroy; override;
    procedure Analyze;
    property Key: String read FKey;
    property Tags: TStrings read FTags;
    property Value: String read FValue;
  end;

  TVpFileItemClass = class of TVpFileItem;

  TVpFileBlock = class
  private
    FItemClass: TVpFileItemClass;
    function GetValue(const AKey, ATags: String): String;
  protected
    FItems: TObjectList;
  public
    constructor Create(AClass: TVpFileItemClass);
    destructor Destroy; override;
    procedure Add(const AText: String);
    procedure Analyze; virtual;
    function FindItem(AKey, ATags: String): TVpFileItem;
    property Value[AKey: String; const ATags: String]: String read GetValue;
  end;

const
  VALUE_DELIMITER = ';';      // semicolon
  KEY_DELIMITER = ';';
  KEY_VALUE_DELIMITER = ':';  // colon
  TYPE_DELIMITER = ',';


implementation

uses
  VpBase, VpMisc;

{==============================================================================}
{                              TVpFileItem                                     }
{==============================================================================}

constructor TVpFileItem.Create(AText: String);
begin
  inherited Create;
  FRaw := AText;
end;

destructor TVpFileItem.Destroy;
begin
  FTags.Free;
  inherited;
end;

procedure TVpFileItem.Analyze;
var
  tagarray: TStringArray;
  i: Integer;
begin
  GetParts(FRaw, FKey, tagarray, FValue);
  FTags := TStringList.Create;
  for i:=Low(tagarray) to High(tagarray) do
    FTags.Add(tagarray[i]);
end;

// Example
//   ADR;TYPE=WORK,POSTAL,PARCEL:;;One Microsoft Way;Redmond;WA;98052-6399;USA
procedure TVpFileItem.GetParts(AText: String; out AKey: String;
  out ATags: TStringArray; out AValue: String);
var
  p: Integer;
  keypart, valuepart: String;
  i: Integer;
  QuotedPrintable: Boolean = false;
begin
  // Split at ':' into key and value parts
  p := pos(KEY_VALUE_DELIMITER, AText);
  if p = 0 then
    raise EVpException.CreateFmt('Illegal file structure in line "%s"', [AText]);
  keypart := Uppercase(copy(AText, 1, p-1));
  valuepart := copy(AText, p+1, MaxInt);

  // Process key part
  p := pos(KEY_DELIMITER, keypart);
  if p = 0 then begin
    AKey := keypart;
    SetLength(ATags, 0);
  end else begin
    AKey := Copy(keypart, 1, p-1);
    keypart := Copy(keypart, p+1, MaxInt);
    if pos('TYPE=', keypart) = 1 then begin
      keypart := copy(keypart, Length('TYPE='), MaxInt);
      ATags := Split(keypart, TYPE_DELIMITER);  // Split at ','
    end else
      ATags := Split(keypart, KEY_DELIMITER);   // Split at ';'
    for i:=Low(ATags) to High(ATags) do
      if ATags[i] = 'QUOTED-PRINTABLE' then begin
        QuotedPrintable := true;
        break;
      end;
  end;

  // Process value part
  if quotedPrintable then
    AValue := UnquotePrintable(valuepart)
  else
    AValue := UnEscape(valuepart);
end;

function TVpFileItem.UnEscape(AValueText: String): String;
const
  BUFSIZE = 100;
var
  p, q: PChar;
  idx: Integer;

  procedure AddChar(ch: Char);
  begin
    Result[idx] := ch;
    inc(idx);
    if idx > Length(Result) then SetLength(Result, Length(Result) + BUFSIZE);
  end;

begin
  if AValueText = '' then begin
    Result := '';
    exit;
  end;

  SetLength(Result, Length(AValueText));
  idx := 1;

  p := @AValueText[1];
  while p^ <> #0 do begin
    if p^ = '\' then begin
      inc(p);
      if p^ = 'n' then begin
        q := PChar(LineEnding);
        AddChar(Char(q^));
        if Length(LineEnding) > 1 then begin
          inc(q);
          AddChar(char(q^));
        end;
      end else
        AddChar(char(p^));
    end else
      AddChar(char(p^));
    inc(p);
  end;
  SetLength(Result, idx-1);
end;

function TVpFileItem.UnQuotePrintable(AValueText: String): String;
const
  BUFSIZE = 100;
var
  p: PChar;
  idx: Integer;
  code: String[2];
  inUTF8: Boolean;
  ch: Char;

  procedure AddChar(ch: Char);
  begin
    Result[idx] := ch;
    inc(idx);
    if idx > Length(Result) then SetLength(Result, Length(Result) + BUFSIZE);
  end;

begin
  if AValueText = '' then begin
    Result := '';
    exit;
  end;

  SetLength(Result, Length(AValueText));
  idx := 1;
  inUTF8 := false;

  p := @AValueText[1];
  while p^ <> #0 do begin
    if p^ = '=' then begin
      code := '';
      inUTF8 := true;
    end else
    if inUTF8 then begin
      if code = '' then
        code := p^
      else begin
        code := code + p^;
        ch := char(StrToInt('$'+code));
        inUTF8 := false;
        AddChar(ch);
      end;
    end else
      AddChar(char(p^));
    inc(p);
  end;
  SetLength(Result, idx-1);
end;


{==============================================================================}
{                               TVpFileBlock                                   }
{==============================================================================}

constructor TVpFileBlock.Create(AClass: TVpFileItemClass);
begin
  inherited Create;
  FItems := TObjectList.Create;
  FItemClass := AClass;
end;

destructor TVpFileBlock.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TVpFileBlock.Add(const AText: String);
begin
  FItems.Add(FItemClass.Create(AText));
end;

procedure TVpFileBlock.Analyze;
var
  i: Integer;
  item: TVpFileItem;
begin
  for i := 0 to FItems.Count-1 do begin
    item := TVpFileItem(FItems[i]);
    item.Analyze;
  end;
end;

{ Finds the item with the specified key and tags. Several tags can be combined
  by a semicolon. If a tag name begins with a '-' then it must NOT be present.
  The conditions are and-ed, i.e. all conditions must be met for the item to
  be accepted. }
function TVpFileBlock.FindItem(AKey, ATags: String): TVpFileItem;
var
  i: Integer;
  item: TVpFileItem;
  tagArr: TStringArray;
  tag, notTag: String;
  ok: Boolean;
begin
  tagArr := Split(ATags, ';');

  for i:=0 to FItems.Count-1 do begin
    item := TVpFileItem(FItems[i]);
    if (AKey = item.Key) then
    begin
      ok := true;                  // No tags specified --> use first item found
      if Length(tagArr) > 0 then begin
        for tag in tagArr do begin
          if tag[1] = '-' then
            notTag := Copy(tag, 2, MaxInt);
          if item.Tags.IndexOf(tag) = -1 then begin  // Tag not found --> reject
            ok := false;
            break;
          end;
          if item.Tags.Indexof(notTag) <> -1 then begin // "NOT" tag found --> reject
            ok := false;
            break;
          end;
        end;
      end;
      if ok then begin
        Result := item;
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVpFileBlock.GetValue(const AKey, ATags: String): String;
var
  item: TVpFileItem;
begin
  item := FindItem(AKey, ATags);
  if item <> nil then
    Result := item.Value
  else
    Result := '';
end;

end.

