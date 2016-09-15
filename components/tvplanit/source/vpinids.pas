{ Visual PlanIt datastore using an ini file }

{$I vp.inc}

unit VpIniDs;

interface

uses
  SysUtils, Classes,
  VpData, VpBaseDS;

type
  TVpIniVersion = (iv105, iv104);

  TVpIniDatastore = class(TVpCustomDatastore)
  private
    FFilename: String;
    FFormatSettings: TFormatSettings;
    FIniVersionRead: TVpIniVersion;
    FIniVersionWrite: TVpIniVersion;
    procedure SetFilename(const AValue: String);

  protected
    function ContactToStr(AContact: TVpContact): String;
    function EventToStr(AEvent: TVpEvent): String;
    function ResourceToStr(AResource: TVpResource): String;
    function TaskToStr(ATask: TVpTask): String;

    procedure StrToContact(AString: String; AContact: TVpContact);
    procedure StrToEvent(AString: String; AEvent: TVpEvent);
    procedure StrToEventTimes(AString: String; out AStartTime, AEndTime: TDateTime);
    procedure StrToResource(AString: String; AResource: TVpResource);
    procedure StrToTask(AString: String; ATask: TVpTask);

    procedure SetConnected(const AValue: Boolean); override;
    function UniqueID(AValue: Integer): Boolean;

    procedure Loaded; override;
    procedure ReadFromIni;
    procedure WriteToIni;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): Integer; override;
    procedure LoadEvents; override;
    procedure LoadContacts; override;
    procedure LoadTasks; override;
    procedure PostContacts; override;
    procedure PostEvents; override;
    procedure PostResources; override;
    procedure PostTasks; override;
    procedure SetResourceByName(Value: String); override;

  published
    property AutoConnect default false;
    property FileName: String read FFileName write SetfileName;

  end;

implementation

uses
  typinfo, StrUtils, Strings, IniFiles,
  VpConst, VpMisc, VpSR;

procedure IniError(const AMsg: String);
begin
  raise Exception.Create(AMsg);
end;

type
  PFormatSettings = ^TFormatSettings;

  TVpIniStrings = class(TStringList)
  private
    FFormatSettings: PFormatSettings;
  protected
    function Strip(AIndex: Integer): String;
  public
    constructor Create(AFormatSettings: PFormatSettings = nil);
    procedure AddField(AName, AValue: String); virtual; abstract;
    procedure AddDateTimeField(AName: String; AValue: TDateTime; AFormat: String); virtual;
    procedure Extract(AIndex: Integer; out AName, AValue: String); virtual; abstract; overload;
    function Extract(AIndex: Integer): String; virtual; abstract; overload;
    function Extract(AName: String): String; virtual; abstract; overload;
  end;

  TVpIniStrings_v104 = class(TVpIniStrings)
  public
    procedure AddField(AName, AValue: String); override;
    procedure Extract(AIndex: Integer; out AName, AValue: String); override;
    function Extract(AIndex: Integer): String; override; overload;
    function Extract(AName: String): String; override; overload;
  end;

  TVpIniStrings_v105 = class(TVpIniStrings)
  public
    procedure AddField(AName, AValue: String); override;
    procedure AddDateTimeField(AName: String; AValue: TDateTime; AFormat: String); override;
    procedure Extract(AIndex: Integer; out AName, AValue: String); override;
    function Extract(AIndex: Integer): String; override; overload;
    function Extract(AName: String): String; override; overload;
  end;

{ TVpIniStrings }
constructor TVpIniStrings.Create(AFormatSettings: PFormatSettings = nil);
begin
  inherited Create;
  Delimiter := '|';
  StrictDelimiter := true;
  if AFormatSettings = nil then
    FFormatSettings := @FormatSettings else
    FFormatSettings := AFormatSettings;
end;

procedure TVpIniStrings.AddDateTimeField(AName: String; AValue: TDateTime; AFormat: String);
begin
  AddField(AName, FormatDateTime(AFormat, AValue, FFormatSettings^));
end;

function TVpIniStrings.Strip(AIndex: Integer): String;
begin
  Result := Strings[AIndex];
  System.Delete(Result, 1, 1);
  System.Delete(Result, Length(Result), 1);
end;

{ TVpIniStrings_v104 }
procedure TVpIniStrings_v104.AddField(AName, AValue: String);
begin
  Add('{' + AValue + '}');
end;

procedure TVpIniStrings_v104.Extract(AIndex: Integer; out AName, AValue: String);
begin
  AName := '';
  AValue := Extract(AIndex);
end;

function TVpIniStrings_v104.Extract(AIndex: Integer): String;
begin
  Result := Strip(AIndex);
end;

function TVpIniStrings_v104.Extract(AName: String): String;
begin
  Result := '';
end;

{ TVpIniStrings_v105 }
procedure TVpIniStrings_v105.AddField(AName, AValue: String);
begin
  if AValue <> '' then
    Add('{' + AName + ':' + AValue + '}');
end;

procedure TVpIniStrings_v105.AddDateTimeField(AName: String; AValue: TDateTime;
  AFormat: String);
begin
  if AValue <> 0 then
    inherited;
end;

procedure TVpIniStrings_v105.Extract(AIndex: Integer; out AName, AValue: String);
var
  s: String;
  p: Integer;
begin
  s := Strip(AIndex);
  p := pos(':', s);
  AName := Copy(s, 1, p-1);
  AValue := Copy(s, p+1, MaxInt);
end;

function TVpIniStrings_v105.Extract(AIndex: Integer): String;
var
  fn: String;
begin
  Extract(AIndex, fn, Result);
end;

function TVpIniStrings_v105.Extract(AName: String): String;
var
  i: Integer;
  fn, fv: String;
begin
  AName := Lowercase(AName);
  for i:=0 to Count-1 do begin
    Extract(i, fn, fv);
    if Lowercase(fn) = AName then begin
      Result := fv;
      exit;
    end;
  end;
  Result := '';
end;

{ TVpIniDatastore }

constructor TVpIniDatastore.Create(AOwner: TComponent);
begin
  inherited;
  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ThousandSeparator := #0;
  FFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  FFormatSettings.LongTimeFormat := 'hh:nn:ss';
  FFormatSettings.DateSeparator := '/';
  FFormatSettings.TimeSeparator := ':';
  FDayBuffer := 1000*365;  // 1000 years, i.e. deactivate daybuffer mechanism
end;

destructor TVpIniDatastore.Destroy;
begin
  SetConnected(false);
  inherited;
end;

function TVpIniDatastore.ContactToStr(AContact: TVpContact): String;
var
  L: TVpIniStrings;
begin
  case FIniVersionWrite of
    iv104: L := TVpIniStrings_v104.Create(@FFormatSettings);
    iv105: L := TVpIniStrings_v105.Create(@FFormatSettings);
    else    raise Exception.CreateFmt('Writing of ini version "%s" not supported.',
              [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionWrite))]);
  end;
  try
    L.AddField('FirstName', AContact.FirstName);                          // 0
    L.AddField('LastName', AContact.LastName);
    L.AddDateTimeField('BirthDate', AContact.BirthDate, 'ddddd');
    L.AddDateTimeField('Anniversary', AContact.Anniversary, 'ddddd');
    L.AddField('Title', AContact.Title);
    L.AddField('Company', AContact.Company);                              // 5
    L.AddField('Job_Position', AContact.Job_Position);
    L.AddField('EMail1', AContact.EMail1);
    L.AddField('Address1', AContact.Address1);
    L.AddField('City1', AContact.City1);
    L.AddField('State1', AContact.State1);                                // 10
    L.AddField('Zip1', AContact.Zip1);
    L.AddField('Country1', AContact.Country1);
    L.AddField('Notes', EncodeLineEndings(AContact.Note));
    L.AddField('Phone1', AContact.Phone1);
    L.AddField('Phone2', AContact.Phone2);                                // 15
    L.AddField('Phone3', AContact.Phone3);
    L.AddField('Phone4', AContact.Phone4);
    L.AddField('Phone5', AContact.Phone5);
    L.AddField('PhoneType1', IntToStr(AContact.PhoneType1));
    L.AddField('PhoneType2', IntToStr(AContact.PhoneType2));              // 20
    L.AddField('PhoneType3', IntToStr(AContact.PhoneType3));
    L.AddField('PhoneType4', IntToStr(AContact.PhoneType4));
    L.AddField('PhoneType5', IntToStr(AContact.PhoneType5));
    L.AddField('Category', IntToStr(AContact.Category));
    L.AddField('Custom1', AContact.Custom1);                              // 25
    L.AddField('Custom2', AContact.Custom2);
    L.AddField('Custom3', AContact.Custom3);
    L.AddField('Custom4', AContact.Custom4);
    L.AddField('UserField0', AContact.UserField0);
    L.AddField('UserField1', AContact.UserField1);                        // 30
    L.AddField('UserField2', AContact.UserField2);
    L.AddField('UserField3', AContact.UserField3);
    L.AddField('UserField4', AContact.UserField4);
    L.AddField('UserField5', AContact.UserField5);
    L.AddField('UserField6', AContact.UserField6);                        // 35
    L.AddField('UserField7', AContact.UserField7);
    L.AddField('UserField8', AContact.UserField8);
    L.AddField('UserField9', AContact.UserField9);                        // 38

    if FIniVersionWrite <> iv104 then begin
      // new fields of v105
      L.AddField('EMailType1', IntToStr(AContact.EMailType1));
      L.AddField('EMail2', AContact.EMail2);
      L.AddField('EMailType2', IntToStr(AContact.EMailType2));
      L.AddField('EMail3', AContact.EMail3);
      L.AddField('EMailType3', IntToStr(AContact.EMailType3));
      L.AddField('Website1', AContact.Website1);
      L.AddField('WebsiteType1', IntToStr(AContact.WebsiteType1));
      L.AddField('Website2', AContact.Website2);
      L.AddField('WebsiteType2', IntToStr(AContact.WebsiteType2));
      L.AddField('AddressType1', IntToStr(AContact.AddressType1));
      L.AddField('Address2', AContact.Address2);
      L.AddField('City2', AContact.City2);
      L.AddField('State2', AContact.State2);
      L.AddField('Zip2', AContact.Zip2);
      L.AddField('Country2', AContact.Country2);
      L.AddField('AddressType2', IntToStr(AContact.AddressType2));
      L.AddField('PathToPhoto', AContact.PathToPhoto);
    end;

    Result := L.DelimitedText;
  finally
    L.Free;
  end;
end;

function TVpIniDatastore.EventToStr(AEvent: TVpEvent): String;
var
  L: TVpIniStrings;
begin
  case FIniVersionWrite of
    iv104: L := TVpIniStrings_v104.Create(@FFormatSettings);
    iv105: L := TVpIniStrings_v105.Create(@FFormatSettings);
    else    raise Exception.CreateFmt('Writing of ini version "%s" not supported.',
              [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionWrite))]);
  end;
  try
    L.AddDateTimeField('StartTime', AEvent.StartTime, 'c');   // short date + long time
    L.AddDateTimeField('EndTime', AEvent.EndTime, 'c');                    // 1
    L.AddField('Description', AEvent.Description);
    L.AddField('Location', AEvent.Location);
    L.AddField('Notes', EncodeLineEndings(AEvent.Notes));
    L.AddField('Category', IntToStr(AEvent.Category));                     // 5
    L.AddField('DingPath', AEvent.DingPath);
    L.AddField('AllDayEvent', BoolToStr(AEvent.AllDayEvent, strTRUE, strFALSE));
    L.AddField('AlarmSet', BoolToStr(AEvent.AlarmSet, strTRUE, strFALSE));
    L.AddField('AlarmAdvance', IntToStr(AEvent.AlarmAdvance));             // 9
    L.AddField('AlarmAdvanceType', GetEnumName(TypeInfo(TVpAlarmAdvType), ord(AEvent.AlarmAdvanceType)));
    L.AddDateTimeField('SnoozeTime', AEvent.SnoozeTime, 'tt');  // long time format
    L.AddField('RepeatCode', GetEnumName(TypeInfo(TVpRepeatType), ord(AEvent.RepeatCode)));
    L.AddDateTimeField('RepeatRangeEnd', AEvent.RepeatRangeEnd, 'ddddd');  // short date format
    L.AddField('CustomInterval', IntToStr(AEvent.CustomInterval));
    L.AddField('UserField0', AEvent.UserField0);                           // 15
    L.AddField('UserField1', AEvent.UserField1);
    L.AddField('UserField2', AEvent.UserField2);
    L.AddField('UserField3', AEvent.UserField3);
    L.AddField('UserField4', AEvent.UserField4);
    L.AddField('UserField5', AEvent.UserField5);                           // 20
    L.AddField('UserField6', AEvent.UserField6);
    L.AddField('UserField7', AEvent.UserField7);
    L.AddField('UserField8', AEvent.UserField8);
    L.AddField('UserField9', AEvent.UserField9);                           // 24
    Result := L.DelimitedText;
  finally
    L.Free;
  end;
end;

function TVpIniDatastore.GetNextID(TableName: string): Integer;
begin
  Unused(TableName);
  repeat
    Result := Random(High(Integer));
  until UniqueID(Result) and (Result <> -1);
end;

function TVpIniDatastore.UniqueID(AValue: Integer): Boolean;
var
  i, j: Integer;
  res: TVpResource;
begin
  Result := false;
  for i:=0 to Resources.Count-1 do begin
    res := Resources.Items[i];
    if res.ResourceID = AValue then
      exit;
    for j:=0 to res.Contacts.Count-1 do
      if res.Contacts.GetContact(j).RecordID = AValue then
        exit;
    for j:=0 to res.Tasks.Count-1 do
      if res.Tasks.GetTask(j).RecordID = AValue then
        exit;
    for j:=0 to res.Schedule.EventCount-1 do
      if res.Schedule.GetEvent(j).RecordID = AValue then
        exit;
  end;
  Result := true;
end;

function TVpIniDatastore.ResourceToStr(AResource: TVpResource): String;
var
  L: TVpIniStrings;
begin
  case FIniVersionWrite of
    iv104: L := TVpIniStrings_v104.Create(@FFormatSettings);
    iv105: L := TVpIniStrings_v105.Create(@FFormatSettings);
    else   raise Exception.CreateFmt('Writing of ini version "%s" not supported.',
             [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionWrite))]);
  end;
  try
    L.AddField('Description', AResource.Description);                      // 0
    L.AddField('Notes', EncodeLineEndings(AResource.Notes));
    L.AddField('ResourceActive', BoolToStr(AResource.ResourceActive, strTRUE, strFALSE));
    L.AddField('UserField0', AResource.UserField0);
    L.AddField('UserField1', AResource.UserField1);
    L.AddField('UserField2', AResource.UserField2);                        // 5
    L.AddField('UserField3', AResource.UserField3);
    L.AddField('UserField4', AResource.UserField4);
    L.AddField('UserField5', AResource.UserField5);
    L.AddField('UserField6', AResource.UserField6);
    L.AddField('UserField7', AResource.UserField7);                        // 10
    L.AddField('UserField8', AResource.UserField8);
    L.AddField('UserField9', AResource.UserField9);                        // 12

    Result := L.DelimitedText;
  finally
    L.Free;
  end;
end;

procedure TVpIniDatastore.SetConnected(const AValue: Boolean);
begin
  if AValue = Connected then
    exit;

  if AValue then
    ReadFromIni
  else
    WriteToIni;

  inherited SetConnected(AValue);
end;

procedure TVpIniDatastore.SetResourceByName(Value: string);
var
  I: integer;
  res : TVpResource;
begin
  for I := 0 to pred(Resources.Count) do begin
    res := Resources.Items[I];
    if Res = nil then
      Continue;

    if res.Description = Value then begin
      if ResourceID <> Res.ResourceID then begin
        ResourceID := Res.ResourceID;
        RefreshResource;
      end;
      Exit;
    end;
  end;
end;

function TVpIniDatastore.TaskToStr(ATask: TVpTask): String;
var
  L: TVpIniStrings;
begin
  case FIniVersionWrite of
    iv104: L := TVpIniStrings_v104.Create(@FFormatSettings);
    iv105: L := TVpIniStrings_v105.Create(@FFormatSettings);
    else   raise Exception.CreateFmt('Writing of ini version "%s" not supported.',
             [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionWrite))]);
  end;
  try
    L.AddField('Complete', BoolToStr(ATask.Complete, strTRUE, strFALSE));  // 0
    L.AddField('Description', ATask.Description);
    L.AddField('Details', EncodeLineEndings(ATask.Details));
    L.AddDateTimeField('CreatedOn', ATask.CreatedOn, 'ddddd');
    L.AddDateTimeField('CompletedOn', ATask.CompletedOn, 'ddddd');
    L.AddField('Priority', IntToStr(ATask.Priority));                      // 5
    L.AddField('Category', IntToStr(ATask.Category));
    L.AddDateTimeField('DueDate', ATask.DueDate, 'ddddd');
    L.AddField('UserField0', ATask.UserField0);
    L.AddField('UserField1', ATask.UserField1);
    L.AddField('UserField2', ATask.UserField2);                            // 10
    L.AddField('UserField3', ATask.UserField3);
    L.AddField('UserField4', ATask.UserField4);
    L.AddField('UserField5', ATask.UserField5);
    L.AddField('UserField6', ATask.UserField6);
    L.AddField('UserField7', ATask.UserField7);                            // 15
    L.AddField('UserField8', ATask.UserField8);
    L.AddField('UserField9', ATask.UserField9);                            // 17
    Result := L.DelimitedText;
  finally
    L.Free;
  end;
end;

procedure TVpIniDatastore.SetFileName(const AValue: String);
begin
  FFileName := AValue;
  if AutoConnect then ReadFromIni;
end;

procedure TVpIniDatastore.LoadContacts;
begin
  // Nothing to do here...
end;

procedure TVpIniDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;

procedure TVpIniDatastore.LoadEvents;
begin
  // Nothing to do here...
end;

procedure TVpIniDatastore.LoadTasks;
begin
  // Nothing to do here...
end;

procedure TVpIniDatastore.PostContacts;
var
  i: Integer;
  contact: TVpContact;
begin
  if Resource = nil then
    exit;
  for i := Resource.Contacts.Count-1 downto 0 do begin
    contact := Resource.Contacts.GetContact(i);
    if contact.Deleted then
      contact.Free;
  end;
  RefreshContacts;
end;

procedure TVpIniDatastore.PostEvents;
var
  i: Integer;
  event: TVpEvent;
begin
  if Resource = nil then
    exit;
  for i := Resource.Schedule.EventCount-1 downto 0 do begin
    event := Resource.Schedule.GetEvent(i);
    if event.Deleted then
      event.Free;
  end;
  RefreshEvents;
end;

procedure TVpIniDatastore.PostResources;
begin
  // Nothing to do...
end;

procedure TVpIniDatastore.PostTasks;
var
  i: Integer;
  task: TVpTask;
begin
  if Resource = nil then
    exit;
  for i := Resource.Tasks.Count-1 downto 0 do begin
    task := Resource.Tasks.GetTask(i);
    if task.Deleted then
      task.Free;
  end;
  RefreshTasks;
end;

procedure TVpIniDatastore.StrToContact(AString: String; AContact: TVpContact);
var
  L: TVpIniStrings;
  i: Integer;
  fn: String;
  fv: String;
begin
  case FIniVersionRead of
    iv104:
      begin
        L := TVpIniStrings_v104.Create;
        try
          L.DelimitedText := AString;
          if L.Count <> 39 then
            IniError(RSIniFileStructure);
          AContact.FirstName := L.Extract(0);
          AContact.LastName := L.Extract(1);
          if L[2] = '' then
            AContact.BirthDate := 0.0 else
            AContact.BirthDate := StrToDate(L.Extract(2), FFormatSettings);
          if L[3] = '' then
            AContact.Anniversary := 0.0 else
            AContact.Anniversary := StrToDate(L.Extract(3), FFormatSettings);
          AContact.Title := L.Extract(4);
          AContact.Company := L.Extract(5);
          AContact.Job_Position := L.Extract(6);
          AContact.EMail := L.Extract(7);
          AContact.Address := L.Extract(8);
          AContact.City := L.Extract(9);
          AContact.State := L.Extract(10);
          AContact.Zip := L.Extract(11);
          AContact.Country := L.Extract(12);
          AContact.Notes := DecodeLineEndings(L.Extract(13));
          AContact.Phone1 := L.Extract(14);
          AContact.Phone2 := L.Extract(15);
          AContact.Phone3 := L.Extract(16);
          AContact.Phone4 := L.Extract(17);
          AContact.Phone5 := L.Extract(18);
          AContact.PhoneType1 := StrToInt(L.Extract(19));
          AContact.PhoneType2 := StrToInt(L.Extract(20));
          AContact.PhoneType3 := StrToInt(L.Extract(21));
          AContact.PhoneType4 := StrToInt(L.Extract(22));
          AContact.PhoneType5 := StrToInt(L.Extract(23));
          AContact.Category := StrToInt(L.Extract(24));
          AContact.Custom1 := L.Extract(25);
          AContact.Custom2 := L.Extract(26);
          AContact.Custom3 := L.Extract(27);
          AContact.Custom4 := L.Extract(28);
          AContact.UserField0 := L.Extract(29);
          AContact.UserField1 := L.Extract(30);
          AContact.UserField2 := L.Extract(31);
          AContact.UserField3 := L.Extract(32);
          AContact.UserField4 := L.Extract(33);
          AContact.UserField5 := L.Extract(34);
          AContact.UserField6 := L.Extract(35);
          AContact.UserField7 := L.Extract(36);
          AContact.UserField8 := L.Extract(37);
          AContact.UserField9 := L.Extract(38);
        finally
          L.Free;
        end;
      end;

    iv105:
      begin
        L := TVpIniStrings_v105.Create;
        try
          L.DelimitedText := AString;
          for i:=0 to L.Count-1 do begin
            L.Extract(i, fn, fv);
            fn := Lowercase(fn);
            if fn = 'firstname' then
              AContact.FirstName := fv
            else if fn = 'lastname' then
              AContact.LastName := fv
            else if fn = 'birthdate' then begin
              if fv = '' then
                AContact.Birthdate := 0 else
                AContact.Birthdate := StrToDate(fv, FFormatSettings)
            end else if fn = 'anniversary' then begin
              if fv = '' then
                AContact.Anniversary := 0 else
                AContact.Anniversary := StrToDate(fv, FFormatSettings);
            end else
            if fn = 'title' then
              AContact.Title := fv
            else if fn = 'pathtophoto' then
              AContact.PathToPhoto := fv
            else if fn = 'category' then
              AContact.Category := StrToInt(fv)
            else if fn = 'notes' then
              AContact.Notes := DecodeLineEndings(fv)
            else if fn = 'company' then
              AContact.Company := fv
            else if fn = 'job_position' then
              AContact.Job_Position := fv
            else if fn = 'addresstype1' then
              AContact.AddressType1 := StrToInt(fv)
            else if fn = 'address1' then
              AContact.Address1 := fv
            else if fn = 'city1' then
              AContact.City1 := fv
            else if fn = 'state1' then
              AContact.State1 := fv
            else if fn = 'zip1' then
              AContact.Zip1 := fv
            else if fn = 'country1' then
              AContact.Country1 := fv
            else if fn = 'addresstype2' then
              AContact.AddressType2 := StrToInt(fv)
            else if fn = 'address2' then
              AContact.Address2 := fv
            else if fn = 'city2' then
              AContact.City2 := fv
            else if fn = 'state2' then
              AContact.State2 := fv
            else if fn = 'zip2' then
              AContact.Zip2 := fv
            else if fn = 'country2' then
              AContact.Country2 := fv
            else if fn = 'email1' then
              AContact.EMail1 := fv
            else if fn = 'email2' then
              AContact.EMail2 := fv
            else if fn = 'email3' then
              AContact.EMail3 := fv
            else if fn = 'emailtype1' then
              AContact.EMailType1 := StrToInt(fv)
            else if fn = 'emailtype2' then
              AContact.EMailType2 := StrToInt(fv)
            else if fn = 'emailtype3' then
              AContact.EMailType3 := StrToInt(fv)
            else if fn = 'phone1' then
              AContact.Phone1 := fv
            else if fn = 'phone2' then
              AContact.Phone2 := fv
            else if fn = 'phone3' then
              AContact.Phone3 := fv
            else if fn = 'phone4' then
              AContact.Phone4 := fv
            else if fn = 'phone5' then
              AContact.Phone5 := fv
            else if fn = 'phonetype1' then
              AContact.PhoneType1 := StrToInt(fv)
            else if fn = 'phonetype2' then
              AContact.PhoneType2 := StrToInt(fv)
            else if fn = 'phonetype3' then
              AContact.PhoneType3 := StrToInt(fv)
            else if fn = 'phonetype4' then
              AContact.PhoneType4 := StrToInt(fv)
            else if fn = 'phonetype5' then
              AContact.PhoneType5 := StrToInt(fv)
            else if fn = 'website1' then
              AContact.Website1 := fv
            else if fn = 'website2' then
              AContact.Website2 := fv
            else if fn = 'websitetype1' then
              AContact.WebsiteType1 := StrToInt(fv)
            else if fn = 'websitetype2' then
              AContact.WebsiteType2 := StrToInt(fv)
            else if fn = 'custom1' then
              AContact.Custom1 := fv
            else if fn = 'custom2' then
              AContact.Custom2 := fv
            else if fn = 'custom3' then
              AContact.Custom3 := fv
            else if fn = 'custom4' then
              AContact.Custom4 := fv
            else if fn = 'userfield0' then
              AContact.UserField0 := fv
            else if fn = 'userfield1' then
              AContact.UserField1 := fv
            else if fn = 'userfield2' then
              AContact.UserField2 := fv
            else if fn = 'userfield3' then
              AContact.UserField3 := fv
            else if fn = 'userfield4' then
              AContact.UserField4 := fv
            else if fn = 'userfield5' then
              AContact.UserField5 := fv
            else if fn = 'userfield6' then
              AContact.UserField6 := fv
            else if fn = 'userfield7' then
              AContact.UserField7 := fv
            else if fn = 'userfield8' then
              AContact.UserField8 := fv
            else if fn = 'userfield9' then
              AContact.UserField9 := fv
            else
              raise Exception.CreateFmt('Unknown contact field "%s".', [fn]);
          end;
        finally
          L.Free;
        end;
      end;

    else
      raise Exception.CreateFmt('Reading of ini version "%s" not supported.',
            [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionRead))]);
  end;  // case
end;

procedure TVpIniDatastore.StrToEvent(AString: String; AEvent: TVpEvent);
var
  L: TVpIniStrings;
  i: Integer;
  fn, fv: String;
begin
  case FIniVersionRead of
    iv104:
      begin
        L := TVpIniStrings_v104.Create;
        try
          L.DelimitedText := AString;
          if L.Count <> 25 then
            IniError(RSIniFileStructure);
          if L[0] = '' then
            AEvent.StartTime := 0 else
            AEvent.StartTime := StrToDateTime(L.Extract(0), FFormatSettings);
          if L[1] = '' then
            AEvent.EndTime := 0 else
            AEvent.EndTime := StrToDateTime(L.Extract(1), FFormatSettings);
          AEvent.Description := L.Extract(2);
          AEvent.Location := L.Extract(3);
          AEvent.Notes := DecodeLineEndings(L.Extract(4));
          AEvent.Category := StrToInt(L.Extract(5));
          AEvent.DingPath := L.Extract(6);
          AEvent.AllDayEvent := StrToBool(L.Extract(7));
          AEvent.AlarmSet := StrToBool(L.Extract(8));
          AEvent.AlarmAdvance := StrToInt(L.Extract(9));
          AEvent.AlarmAdvanceType := TVpAlarmAdvType(GetEnumValue(TypeInfo(TVpAlarmAdvType), L.Extract(10)));
          AEvent.SnoozeTime := StrToTime(L[11]);
          AEvent.RepeatCode := TVpRepeatType(GetEnumValue(TypeInfo(TVpRepeatType), L.Extract(12)));
          if L[13] = '' then
            AEvent.RepeatRangeEnd := 0 else
            AEvent.RepeatRangeEnd := StrToDate(L.Extract(13), FFormatSettings);
          AEvent.CustomInterval := StrToInt(L.Extract(14));
          AEvent.UserField0 := L.Extract(15);
          AEvent.UserField1 := L.Extract(16);
          AEvent.UserField2 := L.Extract(17);
          AEvent.UserField3 := L.Extract(18);
          AEvent.UserField4 := L.Extract(19);
          AEvent.UserField5 := L.Extract(20);
          AEvent.UserField6 := L.Extract(21);
          AEvent.UserField7 := L.Extract(22);
          AEvent.UserField8 := L.Extract(23);
          AEvent.UserField9 := L.Extract(24);
        finally
          L.Free;
        end;
      end;

    iv105:
      begin
        L := TVpIniStrings_v105.Create;
        try
          L.DelimitedText := AString;
          for i:=0 to L.Count-1 do begin
            L.Extract(i, fn, fv);
            fn := Lowercase(fn);
            if fn = 'starttime' then begin
              if fv = '' then
                AEvent.StartTime := 0 else
                AEvent.StartTime := StrToDateTime(fv, FFormatSettings);
            end
            else if fn = 'endtime' then begin
              if fv = '' then
                AEvent.EndTime := 0 else
                AEvent.EndTime := StrToDateTime(fv, FFormatSettings);
            end
            else if fn = 'description' then
              AEvent.Description := fv
            else if fn = 'location' then
              AEvent.Location := fv
            else if fn = 'notes' then
              AEvent.Notes := DecodeLineEndings(fv)
            else if fn = 'category' then
              AEvent.Category := StrToInt(fv)
            else if fn = 'dingpath' then
              AEvent.DingPath := fn
            else if fn = 'alldayevent' then
              AEvent.AllDayEvent := StrToBool(fv)
            else if fn = 'alarmset' then
              AEvent.AlarmSet := StrToBool(fv)
            else if fn = 'alarmadvance' then
              AEvent.AlarmAdvance := StrToInt(fv)
            else if fn = 'alarmadvancetype' then
              AEvent.AlarmAdvanceType := TVpAlarmAdvType(GetEnumValue(TypeInfo(TVpAlarmAdvType), fv))
            else if fn = 'snoozetime' then
              AEvent.SnoozeTime := StrToTime(fv)
            else if fn = 'repeatcode' then
              AEvent.RepeatCode := TVpRepeatType(GetEnumValue(TypeInfo(TVpRepeatType), fv))
            else if fn = 'repeatrangeend' then begin
              if fv = '' then
                AEvent.RepeatRangeEnd := 0 else
                AEvent.RepeatRangeEnd := StrToDate(fv, FFormatSettings);
            end
            else if fn = 'custominterval' then
              AEvent.CustomInterval := StrToInt(fv)
            else if fn = 'userfield0' then
              AEvent.UserField0 := fv
            else if fn = 'userfield1' then
              AEvent.UserField1 := fv
            else if fn = 'userfield2' then
              AEvent.UserField2 := fv
            else if fn = 'userfield3' then
              AEvent.UserField3 := fv
            else if fn = 'userfield4' then
              AEvent.UserField4 := fv
            else if fn = 'userfield5' then
              AEvent.UserField5 := fv
            else if fn = 'userfield6' then
              AEvent.UserField6 := fv
            else if fn = 'userfield7' then
              AEvent.UserField7 := fv
            else if fn = 'userfield8' then
              AEvent.UserField8 := fv
            else if fn = 'userfield9' then
              AEvent.UserField9 := fv
            else
              raise Exception.CreateFmt('Unknown event field "%s"', [fn]);
          end;
        finally
          L.Free;
        end;
      end;

    else
      raise Exception.CreateFmt('Reading of ini version "%s" not supported.',
            [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionRead))]);
  end;
end;

procedure TVpIniDatastore.StrToEventTimes(AString: String;
  out AStartTime, AEndTime: TDateTime);
var
  L: TVpIniStrings;
  i: Integer;
  fn, fv: String;
begin
  AStartTime := 0;
  AEndTime := 0;
  case FIniVersionRead of
    iv104:
      begin
        L := TVpIniStrings_v104.Create;
        try
          L.DelimitedText := AString;
          if L.Count < 2 then
            IniError(RSIniFileStructure);
          if L[0] = '' then
            AStartTime := 0 else
            AStartTime := StrToDateTime(L.Extract(0), FFormatSettings);
          if L[1] = '' then
            AEndTime := 0 else
            AEndtime := StrToDateTime(L.Extract(1), FFormatSettings);
        finally
          L.Free;
        end;
      end;

    iv105:
      begin
        L := TVpIniStrings_V105.Create;
        try
          L.DelimitedText := AString;
          for i:=0 to L.Count-1 do begin
            if (AStartTime <> 0) and (AEndTime <> 0) then
              exit;
            L.Extract(i, fn, fv);
            fn := Lowercase(fn);
            if fn = 'starttime' then begin
              if fv <> '' then
                AStartTime := StrToDateTime(fv, FFormatSettings);
            end
            else if fn = 'endtime' then begin
              if fv <> '' then
                AEndTime := StrToDateTime(fv, FFormatSettings);
            end;
          end;
        finally
          L.Free;
        end;
      end;
  end;
end;

procedure TVpIniDatastore.StrToResource(AString: String; AResource: TVpResource);
var
  L: TVpIniStrings;
  fn, fv: String;
  i: Integer;
begin
  case FIniVersionRead of
    iv104:
      begin
        L := TVpIniStrings_v104.Create;
        try
          L.DelimitedText := AString;
          if L.Count <> 13 then
            IniError(RSIniFileStructure);
          AResource.Description := L.Extract(0);
          AResource.Notes := DecodeLineEndings(L.Extract(1));
          AResource.ResourceActive := StrToBool(L.Extract(2));
          AResource.UserField0 := L.Extract(3);
          AResource.UserField1 := L.Extract(4);
          AResource.UserField2 := L.Extract(5);
          AResource.UserField3 := L.Extract(6);
          AResource.UserField4 := L.Extract(7);
          AResource.UserField5 := L.Extract(8);
          AResource.UserField6 := L.Extract(9);
          AResource.UserField7 := L.Extract(10);
          AResource.UserField8 := L.Extract(11);
          AResource.UserField9 := L.Extract(12);
        finally
          L.Free;
        end;
      end;

    iv105:
      begin
        L := TVpIniStrings_v105.Create;
        try
          L.DelimitedText := AString;
          for i:=0 to L.Count-1 do begin
            L.Extract(i, fn, fv);
            fn := Lowercase(fn);
            if fn = 'description' then
              AResource.Description := fv
            else if fn = 'notes' then
              AResource.Notes := DecodeLineEndings(fv)
            else if fn = 'resourceactive' then
              AResource.ResourceActive := StrToBool(fv)
            else if fn = 'userfield0' then
              AResource.UserField0 := fv
            else if fn = 'userfield1' then
              AResource.UserField1 := fv
            else if fn = 'userfield2' then
              AResource.UserField2 := fv
            else if fn = 'userfield3' then
              AResource.UserField3 := fv
            else if fn = 'userfield4' then
              AResource.UserField4 := fv
            else if fn = 'userfield5' then
              AResource.UserField5 := fv
            else if fn = 'userfield6' then
              AResource.UserField6 := fv
            else if fn = 'userfield7' then
              AResource.UserField7 := fv
            else if fn = 'userfield8' then
              AResource.UserField8 := fv
            else if fn = 'userfield9' then
              AResource.UserField9 := fv
            else
              raise Exception.CreateFmt('Unknown resource field "%s"', [fn]);
          end;
        finally
          L.Free;
        end;
      end;

      else
        raise Exception.CreateFmt('Reading of ini version "%s" not supported.',
          [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionRead))]);
  end;  // case
end;

procedure TVpIniDatastore.StrToTask(AString: String; ATask: TVpTask);
var
  L: TVpIniStrings;
  i: Integer;
  fn, fv: String;
begin
  case FIniVersionRead of
    iv104:
      begin
        L := TVpIniStrings_v104.Create;
        try
          L.DelimitedText := AString;
          if L.Count <> 18 then
            IniError(RSIniFileStructure);
          ATask.Complete := StrToBool(L.Extract(0));
          ATask.Description := L.Extract(1);
          ATask.Details := DecodeLineEndings(L.Extract(2));
          if L[3] = '' then
            ATask.CreatedOn := 0.0 else
            ATask.CreatedOn := StrToDate(L.Extract(3), FFormatSettings);
          if L[4] = '' then
            ATask.CompletedOn := 0.0 else
            ATask.CompletedOn := StrToDate(L.Extract(4), FFormatSettings);
          ATask.Priority := StrToInt(L.Extract(5));
          ATask.Category := StrToInt(L.Extract(6));
          if L[7] = '' then
            ATask.DueDate := 0.0 else
            ATask.DueDate := StrtoDate(L.Extract(7), FFormatSettings);
          ATask.UserField0 := L.Extract(8);
          ATask.UserField1 := L.Extract(9);
          ATask.UserField2 := L.Extract(10);
          ATask.UserField3 := L.Extract(11);
          ATask.UserField4 := L.Extract(12);
          ATask.UserField5 := L.Extract(13);
          ATask.UserField6 := L.Extract(14);
          ATask.UserField7 := L.Extract(15);
          ATask.UserField8 := L.Extract(16);
          ATask.UserField9 := L.Extract(17);
        finally
          L.Free;
        end;
      end;

    iv105:
      begin
        L := TVpIniStrings_v105.Create;
        try
          L.DelimitedText := AString;
          for i:=0 to L.Count-1 do begin
            L.Extract(i, fn, fv);
            fn := Lowercase(fn);
            if fn ='complete' then
              ATask.Complete := StrToBool(fv)
            else if fn = 'description' then
              ATask.Description := fv
            else if fn = 'details' then
              ATask.Details := DecodeLineEndings(fv)
            else if fn = 'createdon' then begin
              if fv = '' then
                ATask.CreatedOn := 0.0 else
                ATask.CreatedOn := StrToDate(fv, FFormatSettings);
            end else if fn = 'completedon' then begin
              if fv = '' then
                ATask.CompletedOn := 0.0 else
                ATask.CompletedOn := StrToDate(fv, FFormatSettings);
            end
            else if fn = 'priority' then
              ATask.Priority := StrToInt(fv)
            else if fn = 'category' then
              ATask.Category := StrToInt(fv)
            else if fn = 'duedate' then begin
              if fv = '' then
                ATask.DueDate := 0.0 else
                ATask.DueDate := StrtoDate(fv, FFormatSettings);
            end
            else if fn = 'userfield0' then
              ATask.UserField0 := fv
            else if fn = 'userfield1' then
              ATask.UserField1 := fv
            else if fn = 'userfield2' then
              ATask.UserField2 := fv
            else if fn = 'userfield3' then
              ATask.UserField3 := fv
            else if fn = 'userfield4' then
              ATask.UserField4 := fv
            else if fn = 'userfield5' then
              ATask.UserField5 := fv
            else if fn = 'userfield6' then
              ATask.UserField6 := fv
            else if fn = 'userfield7' then
              ATask.UserField7 := fv
            else if fn = 'userfield8' then
              ATask.UserField8 := fv
            else if fn = 'userfield9' then
              ATask.UserField9 := fv
            else
              raise Exception.CreateFmt('Unknown task field "%s"', [fn]);
          end;

        finally
          L.Free;
        end;
      end;

    else
      raise Exception.CreateFmt('Reading of ini version "%s" not supported.',
            [GetEnumName(TypeInfo(TVpIniVersion), ord(FIniVersionRead))]);
  end;  // case
end;

procedure TVpIniDatastore.ReadFromIni;
var
  ini: TCustomIniFile;
  ResList, L: TStrings;
  res: TVpResource;
  contact: TVpContact;
  event: TVpEvent;
  task: TVpTask;
  i,j: Integer;
  s: String;
  key: String;
  resID, id: Integer;
  tStart, tEnd: TDateTime;
begin
  if FFileName = '' then
    exit;

  ini := TMemIniFile.Create(FFileName);
  ResList := TStringList.Create;
  L := TStringList.Create;
  try
    Resources.ClearResources;

    s := Lowercase(ini.ReadString('General', 'Version', ''));
    if (s = 'v105') then
      FIniVersionRead := iv105
    else
      FIniVersionRead := iv104;

    ini.ReadSection('Resources', ResList);
    for i:=0 to ResList.Count-1 do begin
      s := ini.ReadString('Resources', ResList[i], '');
      if s = '' then
        IniError(RSIniFileStructure);
      resID := StrToInt(ResList[i]);
      res := Resources.AddResource(resID);
      StrToResource(s, res);

      key := Format('Contacts of resource %d', [resID]);
      L.Clear;
      ini.ReadSection(key, L);
      for j:=0 to L.Count-1 do begin
        id := StrToInt(L[j]);
        contact := res.Contacts.AddContact(id);
        s := ini.ReadString(key, L[j], '');
        StrToContact(s, contact);
      end;

      key := Format('Events of resource %d', [resID]);
      L.Clear;
      ini.ReadSection(key, L);
      for j:=0 to L.Count-1 do begin
        id := StrToInt(L[j]);
        s := ini.ReadString(key, L[j], '');
        StrToEventTimes(s, tStart, tEnd);
        event := res.Schedule.AddEvent(id, tStart, tEnd);
        StrToEvent(s, event);
      end;

      key := Format('Tasks of resource %d', [resID]);
      L.Clear;
      ini.ReadSection(key, L);
      for j:=0 to L.Count-1 do begin
        id := StrToInt(L[j]);
        task := res.Tasks.AddTask(id);
        s := ini.ReadString(key, L[j], '');
        StrToTask(s, task);
      end;
    end;

  finally
    ini.Free;
    L.Free;
    ResList.Free;
  end;
end;

procedure TVpIniDatastore.WriteToIni;
var
  ini: TMemIniFile;
  i, j: Integer;
  res: TVpResource;
  contact: TVpContact;
  event: TVpEvent;
  task: TVpTask;
  key: String;
begin
  if FFileName = '' then
    exit;

  ini := TMemIniFile.Create(FFileName);
  try
    ini.Clear;

    if FIniVersionWrite = iv105 then
      ini.WriteString('General', 'Version', 'v105')
    else
      ini.WriteString('General', 'Version', 'v104');

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      if not res.Deleted then
        ini.WriteString('Resources', IntToStr(res.ResourceID), ResourceToStr(res));
    end;

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      key := Format('Contacts of resource %d', [res.ResourceID]);
      for j:=0 to res.Contacts.Count-1 do begin
        contact := res.Contacts.GetContact(j);
        if not contact.Deleted then
          ini.WriteString(key, IntToStr(contact.RecordID), ContactToStr(contact));
      end;
    end;

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      key := Format('Tasks of resource %d', [res.ResourceID]);
      for j:=0 to res.Tasks.Count-1 do begin
        task := res.Tasks.GetTask(j);
        if not task.Deleted then
          ini.WriteString(key, IntToStr(task.RecordID), TaskToStr(task));
      end;
    end;

    for i:=0 to Resources.Count-1 do begin
      res := Resources.Items[i];
      key := Format('Events of resource %d', [res.ResourceID]);
      for j:=0 to res.Schedule.EventCount-1 do begin
        event := res.Schedule.GetEvent(j);
        if not event.Deleted then
          ini.WriteString(key, IntToStr(event.RecordID), EventToStr(event));
      end;
    end;

  finally
    ini.Free;
  end;
end;

end.
