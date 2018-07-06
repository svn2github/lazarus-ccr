{ Visual PlanIt basic data files for import.
  Is used for import of vCard and iCal files }

unit VpBaseDataFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  VpBase;

type
  TVpFileItem = class
  protected
    FRaw: String;
    FKey: String;
    FAttributes: TStrings;
    FValue: String;
    procedure GetParts(AText: String; out AKey: String; out Attr: TStringArray;
      out AValue: String);
    function UnEscape(AValueText: String): String;
    function UnquotePrintable(AValueText: String): String;
  public
    constructor Create(AText: String);
    destructor Destroy; override;
    procedure Analyze;
    property Key: String read FKey;
    property Attributes: TStrings read FAttributes;
    property Value: String read FValue;
  end;

  TVpFileItemClass = class of TVpFileItem;

  TVpFileBlock = class
  private
    FItemClass: TVpFileItemClass;
    function GetValue(const AKey, Attributes: String): String;
  protected
    FItems: TObjectList;
  public
    constructor Create(AClass: TVpFileItemClass);
    destructor Destroy; override;
    procedure Add(const AText: String);
    procedure Analyze; virtual;
    function FindItem(AKey, Attributes: String): TVpFileItem;
    property Value[AKey: String; const Attributes: String]: String read GetValue;
  end;

const
  VALUE_DELIMITER = ';';      // semicolon
  KEY_DELIMITER = ';';
  KEY_VALUE_DELIMITER = ':';  // colon
  TYPE_DELIMITER = ',';


implementation

uses
  VpMisc;

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
  FAttributes.Free;
  inherited;
end;

procedure TVpFileItem.Analyze;
var
  attrArray: TStringArray;
  i: Integer;
begin
  GetParts(FRaw, FKey, attrArray, FValue);
  FAttributes := TStringList.Create;
  for i:=Low(attrArray) to High(attrArray) do
    FAttributes.Add(attrArray[i]);
end;

// Example
//   ADR;TYPE=WORK,POSTAL,PARCEL:;;One Microsoft Way;Redmond;WA;98052-6399;USA
procedure TVpFileItem.GetParts(AText: String; out AKey: String;
  out Attr: TStringArray; out AValue: String);
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
    SetLength(Attr, 0);
  end else begin
    AKey := Copy(keypart, 1, p-1);
    keypart := Copy(keypart, p+1, MaxInt);
    if pos('TYPE=', keypart) = 1 then begin
      keypart := copy(keypart, Length('TYPE='), MaxInt);
      Attr := Split(keypart, TYPE_DELIMITER);  // Split at ','
    end else
      Attr := Split(keypart, KEY_DELIMITER);   // Split at ';'
    for i:=Low(Attr) to High(Attr) do
      if Attr[i] = 'QUOTED-PRINTABLE' then begin
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

{ Finds the item with the specified key and attributes.
  Several attributes can be combined by a semicolon.
  If an attribute name begins with a '-' then it must NOT be present.
  The conditions are and-ed, i.e. all conditions must be met for the item to
  be accepted. }
function TVpFileBlock.FindItem(AKey, Attributes: String): TVpFileItem;
var
  i: Integer;
  item: TVpFileItem;
  attrArray: TStringArray;
  attr, notAttr: String;
  ok: Boolean;
begin
  attrArray := Split(Attributes, ';');

  for i:=0 to FItems.Count-1 do begin
    item := TVpFileItem(FItems[i]);
    if (AKey = item.Key) then
    begin
      ok := true;                  // No attr specified --> use first item found
      if Length(attrArray) > 0 then begin
        for attr in attrArray do begin
          if attr[1] = '-' then
            notAttr := Copy(attr, 2, MaxInt)
          else
            notAttr := '';
          if item.Attributes.IndexOf(attr) = -1 then begin
            // required attribute not found --> reject
            ok := false;
            break;
          end;
          if (notAttr <> '') and (item.Attributes.IndexOf(notAttr) <> -1) then begin
            // forbidden attribute found --> reject
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

function TVpFileBlock.GetValue(const AKey, Attributes: String): String;
var
  item: TVpFileItem;
begin
  item := FindItem(AKey, Attributes);
  if item <> nil then
    Result := item.Value
  else
    Result := '';
end;

end.

