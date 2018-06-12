{ Reads vCard contact files }

unit VpVCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TVpVCardItem = class
  private
    FRaw: String;
    FKey: String;
    FTags: TStrings;
    FValue: String;
  protected
    procedure Analyze(AVersion: String);
    procedure GetParts(AText, AVersion: String;
      out AKey: String; out ATags: TStringArray; out AValue: String);
    function UnEscape(AValueText: String): String;
    function UnquotePrintable(AValueText: String): String;
  public
    constructor Create(AText: String);
    destructor Destroy; override;
    property Key: String read FKey;
    property Tags: TStrings read FTags;
    property Value: String read FValue;
  end;

  TVpVCard = class
  private
    FItems: TObjectList;
    FVersion: String;

    FFirstName: String;
    FLastName: String;
    FTitle: String;

    FCompany: String;
    FWorkAddress: String;
    FWorkCity: String;
    FWorkZip: String;
    FWorkState: String;
    FWorkCountry: String;
    FWorkEMail: String;
    FWorkPhone: String;
    FWorkFax: String;

    FHomeAddress: String;
    FHomeCity: String;
    FHomeZip: String;
    FHomeState: String;
    FHomeCountry: String;
    FHomeEMail: String;
    FHomePhone: String;
    FHomeFax: String;

    FMobile: String;
    FCarPhone: String;
    FISDN: String;
    FPager: String;

    function GetValue(const AKey, ATags: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AText: String);
    procedure Analyze;
    function FindItem(AKey, ATags: String): TVpVCardItem;

    property FirstName: String read FFirstName;
    property LastName: String read FLastName;
    property Title: String read FTitle;

    property Company: String read FCompany;
    property WorkAddress: String read FWorkAddress;
    property WorkCity: String read FWorkCity;
    property WorkZip: String read FWorkZip;
    property WorkState: String read FWorkState;
    property WorkCountry: String read FWorkCountry;
    property WorkEMail: String read FWorkEMail;
    property WorkPhone: String read FWorkPhone;
    property WorkFax: String read FWorkFax;

    property HomeAddress: String read FHomeAddress;
    property HomeCity: String read FHomeCity;
    property HomeZip: String read FHomeZip;
    property HomeState: String read FHomeState;
    property HomeCountry: String read FHomeCountry;
    property HomeEMail: String read FHomeEMail;
    property HomePhone: String read FHomePhone;
    property HomeFax: String read FHomeFax;

    property CarPhone: String read FCarPhone;
    property Mobile: String read FMobile;
    property ISDN: String read FISDN;
    property Pager: String read FPager;

    property Version: String read FVersion;
    property Value[AKey: String; const ATags: String]: String read GetValue;
  end;

  TVpVCards = class
  private
    FCards: array of TVpVCard;
    function GetCard(AIndex: Integer): TVpVCard;
    function GetCount: Integer;
  protected
    procedure LoadFromStrings(const AStrings: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearCards;
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromStream(const AStream: TStream);
    property Count: Integer read GetCount;
    property Card[AIndex: Integer]: TVpVCard read GetCard; default;
  end;

procedure VCardAddress(AText: String;
  out Address, ACity, AZip, AState, ACountry: String);

function VCardDate(AText: String): TDate;

procedure VCardName(AText: String; out ALastName, AFirstName, ATitle: String);

implementation

uses
  StrUtils, DateUtils,
  vpBase, vpMisc;

const
  ITEMS_DELIMITER = ';';

{ Example:
    ADR;TYPE=home:;;Heidestrasse 17;Koeln;;51147;Germany }
procedure VCardAddress(AText: String;
  out Address, ACity, AZip, AState, ACountry: String);
var
  strArr: TStringArray;
begin
  Address := '';
  ACity := '';
  AState := '';
  ACountry := '';
  strArr := Split(AText, ITEMS_DELIMITER);
  { strArr[0] - post office box                                    ---> not used
    strArr[1] - extended address (e.g., apartment or suite number) ---> not used
    strArr[2] - street address
    strArr[3] - locality (e.g., city)
    strArr[4] - region (e.g., state or province)
    strArr[5] - postal code
    strArr[6] - country name }
  if Length(strArr) > 2 then Address := strArr[2];
  if Length(strArr) > 3 then ACity := strArr[3];
  if Length(strArr) > 4 then AState := strArr[4];
  if Length(strArr) > 5 then AZip := strArr[5];
  if Length(strArr) > 6 then ACountry := strArr[6];
end;

function VCardDate(AText: String): TDate;
var
  fs: TFormatSettings;
  y, m, d: Integer;
begin
  if AText <> '' then begin
    if TryStrToInt(copy(AText, 1, 4), y) and
       TryStrToInt(copy(AText, 5, 2), m) and
       TryStrToInt(copy(AText, 7, 2), d) then
    begin
      if TryEncodeDate(y, m, d, Result) then
        exit;
    end;
    fs.ShortDateFormat := 'yyyy-mm-dd';
    fs.LongDateFormat := fs.ShortDateFormat;
    if TryStrToDate(AText, Result, fs) then
      exit;
  end;
  Result := -1;
end;

{ Example:
    N:Mustermann;Erika;;Dr.; }
procedure VCardName(AText: String; out ALastName, AFirstName, ATitle: String);
var
  strArr: TStringArray;
begin
  ALastName := '';
  AFirstName := '';
  ATitle := '';
  strArr := Split(AText, ITEMS_DELIMITER);
  if Length(strArr) > 0 then ALastName := strArr[0];
  if Length(strArr) > 1 then AFirstName := strArr[1];
  if Length(strArr) > 3 then ATitle := strArr[3];
end;


{==============================================================================}
{                              TVpVCardItem                                    }
{==============================================================================}

constructor TVpVCardItem.Create(AText: String);
begin
  inherited Create;
  FRaw := AText;
end;

destructor TVpVCardItem.Destroy;
begin
  FTags.Free;
  inherited;
end;

procedure TVpVCardItem.Analyze(AVersion: String);
var
  tagarray: TStringArray;
  i: Integer;
begin
  GetParts(FRaw, AVersion, FKey, tagarray, FValue);
  FTags := TStringList.Create;
  for i:=0 to High(tagarray) do
    FTags.Add(tagarray[i]);
end;

// Example:
//   ADR;TYPE=WORK,POSTAL,PARCEL:;;One Microsoft Way;Redmond;WA;98052-6399;USA
procedure TVpVCardItem.GetParts(AText, AVersion: String; out AKey: String;
  out ATags: TStringArray; out AValue: String);
var
  p: Integer;
  keypart, valuepart: String;
  i: Integer;
  QuotedPrintable: Boolean = false;
  typeSeparator: Char;
begin
  // Split at ':' into key and value parts
  p := pos(':', AText);
  if p = 0 then
    raise EVpException.CreateFmt('Illegal vcf structure in line "%s"', [AText]);
  keypart := Uppercase(copy(AText, 1, p-1));
  valuepart := copy(AText, p+1, MaxInt);

  if AVersion = '2.1' then
    typeseparator := ';'
  else
    typeseparator := ',';

  // Process key part
  p := pos(';', keypart);
  if p = 0 then begin
    AKey := keypart;
    SetLength(ATags, 0);
  end else begin
    AKey := Copy(keypart, 1, p-1);
    keypart := Copy(keypart, p+1, MaxInt);
    p := pos('=', keypart);
    if p > 0 then
      keypart := copy(keypart, p+1, MaxInt);
    ATags := Split(keypart, typeSeparator);
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

function TVpVCardItem.UnEscape(AValueText: String): String;
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

function TVpVCardItem.UnQuotePrintable(AValueText: String): String;
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
{                                TVpVCard                                      }
{==============================================================================}

constructor TVpVCard.Create;
begin
  inherited;
  FItems := TObjectList.Create;
end;

destructor TVpVCard.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TVpVCard.Add(const AText: String);
begin
  if Pos('version', Lowercase(AText)) > 0 then
    FVersion := Copy(AText, Pos(':', AText)+1, MaxInt);
  FItems.Add(TVpVCardItem.Create(AText));
end;

procedure TVpVCard.Analyze;
const
  ITEM_SEPARATOR = '; ';
var
  i: Integer;
  item: TVpVCardItem;
begin
  for i := 0 to FItems.Count-1 do begin
    item := TVpVCardItem(FItems[i]);
    item.Analyze(FVersion);
    case item.Key of
      'FN':
        VCardName(item.Value, FLastName, FFirstName, FTitle);
      'ORG':
        FCompany := item.Value;
      'ADR':
        if item.Tags.IndexOf('WORK') <> -1 then
          VCardAddress(item.Value, FWorkAddress, FWorkCity, FWorkZip, FWorkState, FWorkCountry)
        else if item.Tags.IndexOf('HOME') <> -1 then
          VCardAddress(item.value, FHomeAddress, FHomeCity, FHomeZip, FHomeState, FHomeCountry)
        else
        if FCompany = '' then
          VCardAddress(item.Value, FHomeAddress, FHomeCity, FHomeZip, FHomeState, FHomeCountry)
        else
          VCardAddress(item.Value, FWorkAddress, FWorkCity, FWorkZip, FWorkState, FWorkCountry);
      'EMAIL':
        if (FCompany = '') or (item.Tags.IndexOf('HOME') <> -1) then
          FHomeEMail := IfThen(FHomeEMail = '', item.Value, FHomeEMail + ITEM_SEPARATOR + item.Value)
        else
          FWorkEMail := IfThen(FWorkEMail = '', item.Value, FWorkEMail + ITEM_SEPARATOR + item.Value);
      'TEL':
        if item.Tags.IndexOf('CELL') <> -1 then
          FMobile := item.Value
        else
        if item.Tags.IndexOf('PAGER') <> -1 then
          FPager := item.Value
        else
        if item.Tags.IndexOf('FAX') <> -1 then begin
          if (FCompany = '') or (item.Tags.IndexOf('HOME') <> -1) then
            FHomeFax := item.Value
          else
            FWorkFax := item.Value;
        end else
        if item.Tags.IndexOf('CAR') <> -1 then
          FCarPhone := item.Value
        else
        if item.Tags.IndexOf('ISDN') <> -1 then
          FISDN := item.Value
        else
        if (FCompany = '') or (item.tags.IndexOf('HOME') <> -1) then
          FHomePhone := IfThen(FHomePhone = '', item.Value, FHomePhone + ITEM_SEPARATOR + item.Value)
        else
          FWorkPhone := IfThen(FWorkPhone = '', item.Value, FWorkPhone + ITEM_SEPARATOR + item.Value);
    end;
  end;
end;

{ Finds the item with the specified key and tags. Several tags can be combined
  by a semicolon. If a tag name begins with a '-' then it must NOT be present.
  The conditions are and-ed, i.e. all conditions must be met for the item to
  be accepted. }
function TVpVCard.FindItem(AKey, ATags: String): TVpVCardItem;
var
  i: Integer;
  item: TVpVCardItem;
  tagArr: TStringArray;
  tag, notTag: String;
  ok: Boolean;
begin
  tagArr := Split(ATags, ';');

  for i:=0 to FItems.Count-1 do begin
    item := TVpVCardItem(FItems[i]);
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

function TVpVCard.GetValue(const AKey, ATags: String): String;
var
  item: TVpVCardItem;
begin
  item := FindItem(AKey, ATags);
  if item <> nil then Result := item.Value else Result := '';
end;


{==============================================================================}
{                                TVpCards                                      }
{==============================================================================}

constructor TVpVCards.Create;
begin
  inherited;
  SetLength(FCards, 0);
end;

destructor TVpVCards.Destroy;
begin
  ClearCards;
  inherited;
end;

procedure TVpVCards.ClearCards;
var
  j: Integer;
begin
  for j := Count-1 downto 0 do
    FCards[j].Free;
  SetLength(FCards, 0);
end;

function TVpVCards.GetCard(AIndex: Integer): TVpVCard;
begin
  Result := FCards[AIndex];
end;

function TVpVCards.GetCount: Integer;
begin
  Result := Length(FCards);
end;

procedure TVpVCards.LoadFromFile(const AFilename: String);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    LoadFromStrings(L);
  finally
    L.Free;
  end;
end;

procedure TVpVCards.LoadFromStream(const AStream: TStream);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);
    LoadFromStrings(L);
  finally
    L.Free;
  end;
end;

procedure TVpVCards.LoadFromStrings(const AStrings: TStrings);
const
  BLOCK_SIZE = 100;
var
  p: Integer;
  itemName: String;
  itemValue: String;
  i, n: Integer;
  s: String;
begin
  // Clear item list
  ClearCards;
  n := 0;
  SetLength(FCards, BLOCK_SIZE);

  for i:=0 to AStrings.Count-1 do begin
    s := AStrings[i];
    if s = '' then
      continue;
    p := pos(':', s);
    if p = 0 then
      continue;
    itemName := Uppercase(copy(s, 1, p-1));
    itemValue := Uppercase(copy(s, p+1, MaxInt));
    if (itemName = 'BEGIN') and (itemValue = 'VCARD') then begin
      FCards[n] := TVpVCard.Create;
      inc(n);
      if n mod BLOCK_SIZE = 0 then
        SetLength(FCards, Length(FCards) + BLOCK_SIZE);
    end else
    if (itemName = 'END') and (itemValue = 'VCARD') then
      FCards[n-1].Analyze
    else
      FCards[n-1].Add(s);
  end;
  SetLength(FCards, n);
end;

end.

