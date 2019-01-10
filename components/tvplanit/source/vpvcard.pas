{ Imports vCard contact files }

unit VpVCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VpBaseDataFiles;

const
  ITEM_SEPARATOR = ';';

type
  TVpVCardItem = class(TVpFileItem)
  end;

  TVpVCard = class(TVpFileBlock)
  private
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
  public
    constructor Create;
    procedure Analyze; override;
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
  VPBase, VpMisc;

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
{                                TVpVCard                                      }
{==============================================================================}

constructor TVpVCard.Create;
begin
  inherited Create(TVpVCardItem);
end;

procedure TVpVCard.Analyze;
var
  i: Integer;
  item: TVpVCardItem;
  fn, ln, t: String;
begin
  inherited;

  for i := 0 to FItems.Count-1 do begin
    item := TVpVCardItem(FItems[i]);
    case item.Key of
      'VERSION':
        FVersion := item.Value;
      'FN', 'N':
        begin
          VCardName(item.Value, ln, fn, t);
          if FLastName = '' then FLastName := ln;
          if FFirstName = '' then FFirstName := fn;
          if FTitle = '' then FTitle := t;
        end;
      'ORG':
        FCompany := item.Value;
      'ADR':
        if item.Attributes.IndexOf('WORK') <> -1 then
          VCardAddress(item.Value, FWorkAddress, FWorkCity, FWorkZip, FWorkState, FWorkCountry)
        else if item.Attributes.IndexOf('HOME') <> -1 then
          VCardAddress(item.value, FHomeAddress, FHomeCity, FHomeZip, FHomeState, FHomeCountry)
        else
        if FCompany = '' then
          VCardAddress(item.Value, FHomeAddress, FHomeCity, FHomeZip, FHomeState, FHomeCountry)
        else
          VCardAddress(item.Value, FWorkAddress, FWorkCity, FWorkZip, FWorkState, FWorkCountry);
      'EMAIL':
        if (FCompany = '') or (item.Attributes.IndexOf('HOME') <> -1) then
          FHomeEMail := IfThen(FHomeEMail = '', item.Value, FHomeEMail + ITEM_SEPARATOR + item.Value)
        else
          FWorkEMail := IfThen(FWorkEMail = '', item.Value, FWorkEMail + ITEM_SEPARATOR + item.Value);
      'TEL':
        if item.Attributes.IndexOf('CELL') <> -1 then
          FMobile := item.Value
        else
        if item.Attributes.IndexOf('PAGER') <> -1 then
          FPager := item.Value
        else
        if item.Attributes.IndexOf('FAX') <> -1 then begin
          if (FCompany = '') or (item.Attributes.IndexOf('HOME') <> -1) then
            FHomeFax := item.Value
          else
            FWorkFax := item.Value;
        end else
        if item.Attributes.IndexOf('CAR') <> -1 then
          FCarPhone := item.Value
        else
        if item.Attributes.IndexOf('ISDN') <> -1 then
          FISDN := item.Value
        else
        if (FCompany = '') or (item.Attributes.IndexOf('HOME') <> -1) then
          FHomePhone := IfThen(FHomePhone = '', item.Value, FHomePhone + ITEM_SEPARATOR + item.Value)
        else
          FWorkPhone := IfThen(FWorkPhone = '', item.Value, FWorkPhone + ITEM_SEPARATOR + item.Value);
    end;
  end;
end;

function TVpVCard.FindItem(AKey, ATags: String): TVpVCardItem;
begin
  Result := TVpVCardItem(inherited FindItem(AKey, ATags));
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

