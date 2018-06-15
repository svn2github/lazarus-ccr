unit VpICAL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VpBaseDataFiles;

type
  TVpICalendar = class;

  TVpICalItem = class(TVpFileItem)
  public
    function GetAttribute(AName: String): string;
  end;

  TVpICalEntry = class(TVpFileBlock)
  private
    FCalendar: TVpICalendar;
  public
    constructor Create(ACalendar: TVpICalendar);
    function FindItem(AKey: String): TVpICalItem;
  end;

  TVpICalTimeZoneInfo = class(TVpICalEntry)
  public
    TimeZoneID: String;     // e.g. Europe/Berlin
    TimeZoneName: String;   // e.g. CEST
    UTCOffset: Double;      // add to UTC to get local time
  end;

  TVpICalAlarm = class(TVpICalEntry)
  private
    FDuration: Double;   // "SnoozeTime"
    FRepeat: Integer;
    FTrigger: Double;    // "AlarmAdvance"
    FAudio: Boolean;
    FAudioSrc: String;
  public
    procedure Analyze; override;
    property Duration: Double read FDuration;
    property RepeatCount: Integer read FRepeat;
    property Trigger: Double read FTrigger;
    property Audio: Boolean read FAudio;
    property AudioSrc: String read FAudioSrc;
  end;

  TVpICalEvent = class(TVpICalEntry)
  private
    FSummary: String;        // --> Description
    FDescription: String;    // --> Notes
    FLocation: String;
    FStartTime: TDateTime;
    FStartTimeTZ: String;
    FEndTime: TDateTime;
    FEndTimeTZ: String;
    FDuration: double;
    FRecurrenceFreq: String;
    FRecurrenceInterval: Integer;
    FRecurrenceEndDate: TDateTime;
    FRecurrenceCount: Integer;
    FRecurrenceByXXX: String;
    FAlarm: TVpICalAlarm;
    function GetEndTime(UTC: Boolean): TDateTime;
    function GetStartTime(UTC: Boolean): TDateTime;
  public
    destructor Destroy; override;
    procedure Analyze; override;
    procedure UseAlarm;
    property Summary: String read FSummary;            // is "Description" of tvp
    property Description: String read FDescription;    // is "Notes" of tvp
    property Location: String read FLocation;
    property StartTime[UTC: Boolean]: TDateTime read GetStartTime;
    property EndTime[UTC: Boolean]: TDateTime read GetEndTime;
    property Alarm: TVpICalAlarm read FAlarm;
    property RecurrenceFrequency: String read FRecurrenceFreq;
    property RecurrenceInterval: Integer read FRecurrenceInterval;
    property RecurrenceEndDate: TDateTime read FRecurrenceEndDate;
    property RecurrenceCount: Integer read FRecurrenceCount;
    property RecurrenceByXXX: String read FRecurrenceByXXX;
  end;

  TVpICalendar = class
  private
    FEntries: array of TVpICalEntry;
    FVersion: String;
    function GetCount: Integer;
    function GetEntry(AIndex: Integer): TVpICalEntry;
  protected
    // Reading
    procedure LoadFromStrings(const AStrings: TStrings);
    // Time conversion
    function ConvertTime(ADateTime: TDateTime; ATimeZoneID: String; ToUTC: Boolean): TDateTime;
    function LocalTimeToUTC(ADateTime: TDateTime; ATimeZoneID: String): TDateTime;
    function UTCToLocalTime(ADateTime: TDateTime; ATimeZoneID: String): TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromStream(const AStream: TStream);
    property Count: Integer read GetCount;
    property Entry[AIndex: Integer]: TVpICalEntry read GetEntry; default;
  end;


implementation

uses
  VpConst, VpBase;

const
  ITEMS_DELIMITER = ';';

// Examples: 19970702T160000, or T123000, or 20120101
function iCalDateTime(AText: String; out IsUTC: Boolean): TDateTime;
type
  TDateMask = packed record
    year: array[1..4] of char;
    month: array[1..2] of char;
    day: array[1..2] of char;
  end;
  PDateMask = ^TDatemask;
  TTimeMask = packed record
    hour: array[1..2] of char;
    minute: array[1..2] of char;
    second: array[1..2] of char;
  end;
  PTimeMask = ^TTimeMask;
var
  shour, smin, ssec: String;
  yr, mon, day, hr, min, sec: Integer;
  p: Integer;
  d: TDate = 0;
  t: TTime = 0;
begin
  Result := 0;
  if AText = '' then exit;

  if AText = '' then
    exit;

  if (AText[1] <> 'T') and (Length(AText) >= 8) then begin
    if TryStrToInt(Copy(AText, 1, 4), yr) and
       TryStrToInt(Copy(AText, 5, 2), mon) and
       TryStrToInt(Copy(AText, 7, 2), day)
    then
      if not TryEncodeDate(yr, mon, day, d) then exit;
  end;

  shour := '0';
  smin := '0';
  ssec := '0';
  p := pos('T', AText);
  if p > 0 then begin
    if Length(AText) >= p + 2 then shour := Copy(AText, p+1, 2);
    if Length(AText) >= p + 4 then smin := Copy(AText, p+3, 2);
    if Length(AText) >= p + 6 then ssec := Copy(AText, p+5, 2);
  end;
  if TryStrToInt(shour, hr) and
     TryStrToInt(smin, min) and
     TryStrToInt(ssec, sec)
  then
    if not TryEncodeTime(hr, min, sec, 0, t) then exit;

  Result := d + t;
  IsUTC := AText[Length(AText)] = 'Z';
end;

// Example: PT0H20M0S, or -PT15M, or -P2D
function iCalDuration(AText: String): Double;
var
  isNeg: Boolean = false;
  inDate: Boolean = true;
  p: PChar;
  s: String;
  n: Integer;
begin
  Result := 0;
  if AText = '' then
    exit;

  p := @AText[1];
  if p^ = '-' then begin
    isNeg := true;
    inc(p);
  end;
  if p^ <> 'P' then   // 'P' = "period"
    exit;

  inc(p);
  s := '';
  while true do begin
    case p^ of
      #0 : break;
      'T': begin
             inDate := false;
             s := '';
           end;
      'D': begin
             Result := Result + StrToInt(s);
             s := '';
           end;
      'H': begin
             Result := Result + StrToInt(s)/24;
             s := '';
           end;
      'M': begin
             if inDate then
               // don't know about months... ?!
             else
               Result := Result + StrToInt(s)/MinutesInDay;
               ;  // don't know about months... ?!
             s := '';
           end;
      'S': begin
             Result := Result + StrToInt(s) / SecondsInDay;
             s := '';
           end;
      '0'..'9': s := s + p^;
      else raise EVpException.CreateFmt('Invalid character in DURATION string "%s"', [AText]);
    end;
    inc(p);
  end;
  if isNeg then Result := -Result;
end;


{==============================================================================}
{                             TVpICalItem                                      }
{==============================================================================}

function TVpICalItem.GetAttribute(AName: String): String;
begin
  Result := FAttributes.Values[AName];
end;


{==============================================================================}
{                             TVpICalEntry                                     }
{==============================================================================}

constructor TVpICalEntry.Create(ACalendar: TVpICalendar);
begin
  inherited Create(TVpICalItem);
  FCalendar := ACalendar;
end;

function TVpICalEntry.FindItem(AKey: String): TVpICalItem;
begin
  Result := TVpICalItem(inherited FindItem(AKey, ''));
end;


{==============================================================================}
{                               TVpICalAlarm                                   }
{==============================================================================}
procedure TVpICalAlarm.Analyze;
var
  i: Integer;
  item: TVpICalItem;
  s: String;
  isUTC: Boolean;
begin
  inherited;
  for i := 0 to FItems.Count-1 do begin
    item := TVpICalItem(FItems[i]);
    case item.Key of
      'TRIGGER':
        FTrigger := ICalDuration(item.Value);
      'DURATION' :
        FDuration := ICalDuration(item.Value);
      'REPEAT':
        FRepeat := StrToInt(item.Value);
      'ACTION':
        FAudio := Uppercase(item.Value) = 'AUDIO';
      'ATTACH':
        if Lowercase(item.GetAttribute('FMTTYPE')) = 'audio' then
          FAudioSrc := item.Value;
    end;
  end;
end;


{==============================================================================}
{                              TVpICalEvent                                    }
{==============================================================================}

destructor TVpICalEvent.Destroy;
begin
  FAlarm.Free;
  inherited;
end;

procedure TVpICalEvent.Analyze;
var
  i, j: Integer;
  item: TVpICalItem;
  L: TStrings;
  s: String;
  isUTC: Boolean;
begin
  inherited;

  for i := 0 to FItems.Count-1 do begin
    item := TVpICalItem(FItems[i]);
    case item.Key of
      'SUMMARY':
        FSummary := item.Value;
      'DTSTART':
        begin
          FStartTimeTZ := item.GetAttribute('TZID');
          FStartTime := iCalDateTime(item.Value, isUTC);
          if not isUTC then
            FStartTime := FCalendar.LocalTimeToUTC(FStartTime, FStartTimeTZ);
        end;
      'DTEND':
        begin
          FEndTimeTZ := item.GetAttribute('TZID');
          FEndTime := iCalDateTime(item.Value, isUTC);
          if not isUTC then
            FEndTime := FCalendar.LocalTimeToUTC(FEndTime, FEndTimeTZ);
        end;
      'DESCRIPTION':
        FDescription := item.Value;
      'LOCATION':
        FLocation := item.Value;
      'DURATION':
        FDuration := ICalDuration(item.Value);
      'RRULE':
        begin
          L := TStringList.Create;
          try
            L.StrictDelimiter := true;
            L.Delimiter := VALUE_DELIMITER; // ';'
            L.DelimitedText := item.Value;
            FRecurrenceFreq := L.Values['FREQ'];
            FRecurrenceInterval := StrToIntDef(L.Values['INTERVAL'], 0);
            FRecurrenceEndDate := iCalDateTime(L.Values['UNTIL'], isUTC);
            FRecurrenceCount := StrToIntDef(L.Values['COUNT'], 0);
            FRecurrenceByXXX := '';
            for j:=0 to L.Count-1 do begin
              s := L[j];
              if pos('BY', s) = 1 then FRecurrenceByXXX := FRecurrenceByXXX + ';' + s;
            end;
            if FRecurrenceByXXX <> '' then
              Delete(FRecurrenceByXXX, 1, 1);
          finally
            L.Free;
          end;
        end;
    end;
  end;
end;

function TVpICalEvent.GetEndTime(UTC: Boolean): TDateTime;
begin
  if FEndTime <> 0 then
    Result := FEndTime
  else
    Result := FStartTime + FDuration;
  if not UTC then
    Result := FCalendar.UTCToLocalTime(Result, FEndTimeTZ);
end;

function TVpICalEvent.GetStartTime(UTC: Boolean): TDateTime;
begin
  if UTC then
    Result := FStartTime
  else
    Result := FCalendar.LocalTimeToUTC(FStartTime, FStartTimeTZ);
end;

procedure TVpICalEvent.UseAlarm;
begin
  FAlarm.Free;
  FAlarm := TVpICalAlarm.Create(FCalendar);
end;


{==============================================================================}
{                             TVpICalendar                                     }
{==============================================================================}

constructor TVpICalendar.Create;
begin
  inherited;
  SetLength(FEntries, 0);
end;

destructor TVpICalendar.Destroy;
begin
  SetLength(FEntries, 0);
  inherited;
end;

procedure TVpICalendar.Clear;
var
  j: Integer;
begin
  for j := Count-1 downto 0 do
    FEntries[j].Free;
  SetLength(FEntries, 0);
end;

function TVpICalendar.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TVpICalendar.GetEntry(AIndex: Integer): TVpICalEntry;
begin
  Result := FEntries[AIndex];
end;

procedure TVpICalendar.LoadFromFile(const AFilename: String);
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

procedure TVpICalendar.LoadFromStream(const AStream: TStream);
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

procedure TVpICalendar.LoadFromStrings(const AStrings: TStrings);
const
  BLOCK_SIZE = 100;
var
  p: Integer;
  itemName: String;
  itemValue: String;
  i, n: Integer;
  s: String;
  currEntry: TVpICalEntry = nil;
  oldEntry: TVpICalEntry = nil;
begin
  // Clear item list
  Clear;
  n := 0;
  SetLength(FEntries, BLOCK_SIZE);
  for i:=0 to AStrings.Count-1 do begin
    s := AStrings[i];
    if s = '' then
      continue;
    p := pos(':', s);
    if p = 0 then
      continue;
    itemName := Uppercase(copy(s, 1, p-1));
    itemValue := Uppercase(copy(s, p+1, MaxInt));
    case ItemName of
      'BEGIN':
        begin
          FEntries[n] := nil;
          case itemValue of
            'VTIMEZONE':
              begin
                currEntry := TVpICalTimeZoneInfo.Create(self);
                FEntries[n] := currEntry;
              end;
            'VEVENT':
              begin
                currEntry := TVpICalEvent.Create(self);
                FEntries[n] := currEntry;
              end;
            'VFREEBUSY':
              currEntry := nil;
            'VTODO':
              currEntry := nil;
            'VJOURNAL':
              currEntry := nil;
            'VALARM':
              if currEntry is TVpICalEvent then begin
                oldEntry := currEntry;
                TVpICalEvent(currEntry).UseAlarm;
                currEntry := TVpICalEvent(currEntry).Alarm;
              end;
            else
              Continue;
          end;
          if FEntries[n] <> nil then begin
            inc(n);
            if n mod BLOCK_SIZE = 0 then
              SetLength(FEntries, Length(FEntries) + BLOCK_SIZE);
          end;
        end;
      'END':
        begin
          if currEntry <> nil then
            currEntry.Analyze;
          if oldEntry <> nil then begin
            currEntry := oldEntry;
            oldEntry := nil;
          end else
            currEntry := nil;
        end;
      'VERSION':
        FVersion := itemValue;
      else
        if currEntry <> nil then
          currEntry.Add(s);
    end;
  end;
  SetLength(FEntries, n);
end;

function TVpICalendar.ConvertTime(ADateTime: TDateTime;
  ATimeZoneID: String; ToUTC: Boolean): TDateTime;
var
  offs: Double;
  i: Integer;
begin
  offs := 0;
  for i:=0 to Count-1 do begin
    if (FEntries[i] is TVpICalTimeZoneInfo) and
       (TVpICalTimeZoneInfo(FEntries[i]).TimeZoneID = ATimeZoneID) then
    begin
      offs := TVpICalTimeZoneInfo(FEntries[i]).UTCOffset;
      break;
    end;
  end;
  if ToUTC then
    Result := ADateTime - offs
  else
    Result := ADateTime + offs;
end;

function TVpICalendar.LocalTimeToUTC(ADateTime: TDateTime;
  ATimeZoneID: String): TDateTime;
begin
  Result := ConvertTime(ADateTime, ATimeZoneID, true);
end;

function TVpICalendar.UTCToLocalTime(ADateTime: TDateTime;
  ATimeZoneID: String): TDateTime;
begin
  Result := ConvertTime(ADateTime, ATimeZoneID, false);
end;

end.

