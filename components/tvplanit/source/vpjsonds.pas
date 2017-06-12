{ Visual PlanIt datastore for json files }

{$I vp.inc}

unit VpJSONDs;

interface

uses
  SysUtils, Classes, db, fpjson,
  VpData, VpBaseDS;

type
  TVpJSONDataStore = class(TVpCustomDataStore)
  private
    FFileName: String;
    FFormatSettings: TFormatSettings;
    procedure SetFilename(AValue: String);

  protected
    { ancestor methods }
    procedure Loaded; override;
    procedure SetConnected(const Value: boolean); override;

    { JSON access methods }
    function ContactToJSON(AContact: TVpContact): TJSONObject;
    function EventToJSON(AEvent: TVpEvent): TJSONObject;
    function ResourceToJSON(AResource: TVpResource): TJSONObject;
    function TaskToJSON(ATask: TVpTask): TJSONObject;

    function JSONToContact(AObj: TJSONObject; AResource: TVpResource): TVpContact;
    function JSONToEvent(AObj: TJSONObject; AResource: TVpResource): TVpEvent;
    function JSONToResource(AObj: TJSONObject): TVpResource;
    function JSONToTask(AObj: TJSONObject; AResource: TVpResource): TVpTask;

    procedure ReadJSON;
    procedure WriteJSON;

    { other methods }
    function UniqueID(AValue: Integer): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): Integer; override;

    procedure LoadEvents; override;
    procedure LoadContacts; override;
    procedure LoadTasks; override;

    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;
    procedure PostResources; override;

    procedure SetResourceByName(Value: String); override;

  published
    property AutoConnect default false;
    property FileName: String read FFileName write SetFilename;
  end;


implementation

uses
  LazFileUtils,
  jsonparser,
  VpConst, VpSR, VpMisc;

constructor TVpJSONDatastore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ThousandSeparator := #0;
  FFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  FFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FFormatSettings.LongTimeFormat := 'hh:nn:ss';
  FFormatSettings.ShortTimeFormat := 'hh:nn';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';

  FDayBuffer := 1000*365;  // 1000 years, i.e. deactivate daybuffer mechanism
end;

destructor TVpJSONDatastore.Destroy;
begin
  SetConnected(false);
  inherited;
end;

function TVpJSONDatastore.ContactToJSON(AContact: TVpContact): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('RecordID', AContact.RecordID);
  Result.Add('Job_Position', AContact.Job_Position);
  Result.Add('FirstName', AContact.FirstName);
  Result.Add('LastName', AContact.LastName);
  Result.Add('Birthdate', DateToStr(AContact.Birthdate, FFormatSettings));
  Result.Add('Anniversary', DateToStr(AContact.Anniversary, FFormatSettings));
  Result.Add('Title', AContact.Title);
  Result.Add('Company', AContact.Company);
  Result.Add('Department', AContact.Department);
  Result.Add('EMail1', AContact.EMail1);
  Result.Add('EMail2', AContact.EMail2);
  Result.Add('EMail3', AContact.EMail3);
  Result.Add('EMailType1', ord(AContact.EMailType1));
  Result.Add('EMailType2', ord(AContact.EMailType2));
  Result.Add('EMailType3', ord(AContact.EMailType3));
  Result.Add('Phone1', AContact.Phone1);
  Result.Add('Phone2', AContact.Phone2);
  Result.Add('Phone3', AContact.Phone3);
  Result.Add('Phone4', AContact.Phone4);
  Result.Add('Phone5', AContact.Phone5);
  Result.Add('PhoneType1', AContact.PhoneType1);
  Result.Add('PhoneType2', AContact.PhoneType2);
  Result.Add('PhoneType3', AContact.PhoneType3);
  Result.Add('PhoneType4', AContact.PhoneType4);
  Result.Add('PhoneType5', AContact.PhoneType5);
  Result.Add('Website1', AContact.Website1);
  Result.Add('Website2', AContact.Website2);
  Result.Add('WebsiteType1', AContact.WebsiteType1);
  Result.Add('WebsiteType2', AContact.WebsiteType2);
  Result.Add('Address1', AContact.Address1);
  Result.Add('Address2', AContact.Address2);
  Result.Add('City1', AContact.City1);
  Result.Add('City2', AContact.City2);
  Result.Add('State1', AContact.State1);
  Result.Add('State2', AContact.State2);
  Result.Add('Zip1', AContact.Zip1);
  Result.Add('Zip2', AContact.Zip2);
  Result.Add('Country1', AContact.Country1);
  Result.Add('Country2', AContact.Country2);
  Result.Add('AddressType1', ord(AContact.AddressType1));
  Result.Add('AddressType2', ord(AContact.AddressType2));
  Result.Add('Notes', AContact.Notes);
  Result.Add('Category', ord(AContact.Category));
  Result.Add('Custom1', AContact.Custom1);
  Result.Add('Custom2', AContact.Custom2);
  Result.Add('Custom3', AContact.Custom3);
  Result.Add('Custom4', AContact.Custom4);
  Result.Add('UserField0', AContact.UserField0);
  Result.Add('UserField1', AContact.UserField1);
  Result.Add('UserField2', AContact.UserField2);
  Result.Add('UserField3', AContact.UserField3);
  Result.Add('UserField4', AContact.UserField4);
  Result.Add('UserField5', AContact.UserField5);
  Result.Add('UserField6', AContact.UserField6);
  Result.Add('UserField7', AContact.UserField7);
  Result.Add('UserField8', AContact.UserField8);
  Result.Add('UserField9', AContact.UserField9);
end;


function TVpJSONDatastore.EventToJSON(AEvent: TVpEvent): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('RecordID', AEvent.RecordID);
  Result.Add('Description', AEvent.Description);
  Result.Add('Notes', AEvent.Notes);
  Result.Add('Location', AEvent.Location);
  Result.Add('Category', AEvent.Category);
  Result.Add('AllDayEvent', AEvent.AllDayEvent);
  if AEvent.StartTime = 0 then
    Result.Add('StartTime', '') else
    Result.Add('StartTime', FormatDateTime('c', AEvent.StartTime, FFormatSettings));
  if AEvent.EndTime = 0 then
    Result.Add('EndTime', '') else
    Result.Add('EndTime', FormatDateTime('c', AEvent.EndTime, FFormatSettings));
  Result.Add('DingPath', AEvent.DingPath);
  Result.Add('AlertDisplayed', AEvent.AlertDisplayed);
  Result.Add('AlarmSet', AEvent.AlarmSet);
  Result.Add('AlarmAdvance', AEvent.AlarmAdvance);
  Result.Add('AlarmAdvanceType', ord(AEvent.AlarmAdvanceType));
  Result.Add('SnoozeTime', TimeToStr(AEvent.SnoozeTime, FFormatSettings));
  Result.Add('RepeatCode', ord(AEvent.RepeatCode));
  if AEvent.RepeatRangeEnd = 0 then
    Result.Add('RepeatRangeEnd', '') else
    Result.Add('RepeatRangeEnd', FormatDateTime('c', AEvent.RepeatRangeEnd, FFormatSettings));
  Result.Add('CustomInterval', AEvent.CustomInterval);
  Result.Add('UserField0', AEvent.UserField0);
  Result.Add('UserField1', AEvent.UserField1);
  Result.Add('UserField2', AEvent.UserField2);
  Result.Add('UserField3', AEvent.UserField3);
  Result.Add('UserField4', AEvent.UserField4);
  Result.Add('UserField5', AEvent.UserField5);
  Result.Add('UserField6', AEvent.UserField6);
  Result.Add('UserField7', AEvent.UserField7);
  Result.Add('UserField8', AEvent.UserField8);
  Result.Add('UserField9', AEvent.UserField9);
end;

function TVpJSONDatastore.GetNextID(TableName: string): Integer;
begin
  Unused(TableName);
  (*
  if FUseAutoInc then
    { This is not needed in the BufDataset datastore as these tables use
      autoincrement fields. }
    Result := -1
  else
    { If autoincrement fields are not wanted the ID values are created from
      random numbers. }
      *)
  { The ID values are created from unique random numbers. }
  repeat
    Result := Random(High(Integer));
  until UniqueID(Result) and (Result <> -1);
end;

function TVpJSONDatastore.JSONToContact(AObj: TJSONObject;
  AResource: TVpResource): TVpContact;
var
  recID: Integer;
  s: String;
begin
  if AObj = nil then
    exit;

  recID := AObj.Find('RecordID').AsInteger;
  Result := AResource.Contacts.AddContact(recID);
  Result.Job_Position := AObj.Get('Job_Position', '');
  Result.FirstName := AObj.Get('FirstName', '');
  Result.LastName := AObj.Get('LastName', '');
  s := AObj.Get('Birthdate', '');
  if s <> '' then
    Result.BirthDate := StrToDate(s, FFormatSettings) else
    Result.BirthDate := 0;
  s := AObj.Get('Anniversary', '');
  if s <> '' then
    Result.Anniversary := StrToDate(s, FFormatSettings) else
    Result.Anniversary := 0;
  Result.Title := AObj.Get('Title', '');
  Result.Company := AObj.Get('Company', '');
  Result.Department := AObj.Get('Department', '');
  Result.Email1 := AObj.Get('EMail1', '');
  Result.Email2 := AObj.Get('EMail2', '');
  Result.Email3 := AObj.Get('EMail3', '');
  Result.EmailType1 := AObj.Get('EMailType1', 0);
  Result.EmailType2 := AObj.Get('EMailType2', 0);
  Result.EmailType3 := AObj.Get('EMailType3', 0);
  Result.Phone1 := AObj.Get('Phone1', '');
  Result.Phone2 := AObj.Get('Phone2', '');
  Result.Phone3 := AObj.Get('Phone3', '');
  Result.Phone4 := AObj.Get('Phone4', '');
  Result.Phone5 := AObj.Get('Phone5', '');
  Result.PhoneType1 := AObj.Get('PhoneType1', 0);
  Result.PhoneType2 := AObj.Get('PhoneType2', 0);
  Result.PhoneType3 := AObj.Get('PhoneType3', 0);
  Result.PhoneType4 := AObj.Get('PhoneType4', 0);
  Result.PhoneType5 := AObj.Get('PhoneType5', 0);
  Result.Website1 := AObj.Get('Website1', '');
  Result.Website2 := AObj.Get('Website2', '');
  Result.WebsiteType1 := AObj.Get('WebsiteType1', 0);
  Result.WebsiteType2 := AObj.Get('WebsiteType2', 0);
  Result.Address1 := AObj.Get('Address1', '');
  Result.Address2:= AObj.Get('Address2', '');
  Result.AddressType1 := AObj.Get('AddressType1', 0);
  Result.AddressType2:= AObj.Get('AddressType2', 0);
  Result.City1 := AObj.Get('City1', '');
  Result.City2 := AObj.Get('City2', '');
  Result.Zip1 := AObj.Get('Zip1', '');
  Result.Zip2 := AObj.Get('Zip2', '');
  Result.Country1 := AObj.Get('Country1', '');
  Result.Country2 := AObj.Get('Country2', '');
  Result.Notes := AObj.Get('Notes', '');
  Result.Category := AObj.Get('Category', 0);
  Result.Custom1 := AObj.Get('Custom1', '');
  Result.Custom2 := AObj.Get('Custom2', '');
  Result.Custom3 := AObj.Get('Custom3', '');
  Result.Custom4 := AObj.Get('Custom4', '');
  Result.UserField0 := AObj.Get('UserField0', '');
  Result.UserField1 := AObj.Get('UserField1', '');
  Result.UserField2 := AObj.Get('UserField2', '');
  Result.UserField3 := AObj.Get('UserField3', '');
  Result.UserField4 := AObj.Get('UserField4', '');
  Result.UserField5 := AObj.Get('UserField5', '');
  Result.UserField6 := AObj.Get('UserField6', '');
  Result.UserField7 := AObj.Get('UserField7', '');
  Result.UserField8 := AObj.Get('UserField8', '');
  Result.UserField9 := AObj.Get('UserField9', '');
end;

function TVpJSONDatastore.JSONToEvent(AObj: TJSONObject;
  AResource: TVpResource): TVpEvent;
var
  evID: Integer;
  startDate, endDate: TDateTime;
  s: String;
begin
  if AObj = nil then
    exit;

  evID := AObj.Find('RecordID').AsInteger;
  s := AObj.Get('StartTime', '');
  if s <> '' then
    startDate := StrToDateTime(s, FFormatSettings) else
    startDate := 0;
  s := AObj.Get('EndTime', '');
  if s <> '' then
    endDate := StrToDateTime(s, FFormatSettings) else
    endDate := 0;
  Result := AResource.Schedule.AddEvent(evID, startDate, endDate);
  Result.Description := AObj.Get('Description', '');
  Result.Notes := AObj.Get('Notes', '');
  Result.Location := AObj.Get('Location', '');
  Result.Category := AObj.Get('Category', 0);
  Result.AllDayEvent := AObj.Get('AllDayEvent', false);
  Result.DingPath := AObj.Get('DingPath', '');
  Result.AlertDisplayed := AObj.Get('AlertDisplayed', false);
  Result.AlarmSet := AObj.Get('AlarmSet', false);
  Result.AlarmAdvance := AObj.Get('AlarmAdvance', 0);
  Result.AlarmAdvanceType := TVpAlarmAdvType(AObj.Get('AlarmAdvanceType', 0));
  s := AObj.Get('SnoozeTime', '');
  if s <> '' then
    Result.SnoozeTime := StrToTime(s, FFormatSettings) else
    Result.SnoozeTime := 0;
  Result.RepeatCode := TVpRepeatType(AObj.Get('RepeatCode', 0));
  s := AObj.Get('RepeatRangeEnd', '');
  if s <> '' then
    Result.RepeatRangeEnd := StrToDateTime(s, FFormatSettings) else
    Result.RepeatRangeEnd := 0;
  Result.CustomInterval := AObj.Get('CustomInterval', 0);
  Result.UserField0 := AObj.Get('UserField0', '');
  Result.UserField1 := AObj.Get('UserField1', '');
  Result.UserField2 := AObj.Get('UserField2', '');
  Result.UserField3 := AObj.Get('UserField3', '');
  Result.UserField4 := AObj.Get('UserField4', '');
  Result.UserField5 := AObj.Get('UserField5', '');
  Result.UserField6 := AObj.Get('UserField6', '');
  Result.UserField7 := AObj.Get('UserField7', '');
  Result.UserField8 := AObj.Get('UserField8', '');
  Result.UserField9 := AObj.Get('UserField9', '');
end;

function TVpJSONDatastore.JSONToResource(AObj: TJSONObject): TVpResource;
var
  resID: Integer;
begin
  resID := AObj.Find('ResourceID').AsInteger;
  Result := Resources.AddResource(resID);
  Result.Description := AObj.Get('Description','');
  Result.Notes := AObj.Get('Notes', '');
  Result.ResourceActive := AObj.Get('ResourceActive', false);
  Result.UserField0 := AObj.Get('UserField0', '');
  Result.UserField1 := AObj.Get('UserField1', '');
  Result.UserField2 := AObj.Get('UserField2', '');
  Result.UserField3 := AObj.Get('UserField3', '');
  Result.UserField4 := AObj.Get('UserField4', '');
  Result.UserField5 := AObj.Get('UserField5', '');
  Result.UserField6 := AObj.Get('UserField6', '');
  Result.UserField7 := AObj.Get('UserField7', '');
  Result.UserField8 := AObj.Get('UserField8', '');
  Result.UserField9 := AObj.Get('UserField9', '');
end;

function TvpJSONDatastore.JSONToTask(AObj: TJSONObject;
  AResource: TVpResource): TVpTask;
var
  recID: Integer;
  s: String;
begin
  recID := AObj.Find('RecordID').AsInteger;
  Result := AResource.Tasks.AddTask(recID);
  s := AObj.Get('DueDate', '');
  if s <> '' then
    Result.DueDate := StrToDateTime(s, FFormatSettings) else
    Result.DueDate := 0;
  Result.Description := AObj.Get('Description', '');
  Result.Details := AObj.Get('Details', '');
  Result.Complete := AObj.Get('Complete', false);
  s := AObj.Get('CreatedOn', '');
  if s <> '' then
    Result.CreatedOn := StrToDate(s, FFormatSettings) else
    Result.CreatedOn := 0;
  s := AObj.Get('CompletedOn', '');
  if s <> '' then
    Result.CompletedOn := StrToDate(s, FFormatSettings) else
    Result.CompletedOn := 0;
  Result.Priority := AObj.Get('Priority', 0);
  Result.Category := AObj.Get('Category', 0);
  Result.UserField0 := AObj.Get('UserField0', '');
  Result.UserField1 := AObj.Get('UserField1', '');
  Result.UserField2 := AObj.Get('UserField2', '');
  Result.UserField3 := AObj.Get('UserField3', '');
  Result.UserField4 := AObj.Get('UserField4', '');
  Result.UserField5 := AObj.Get('UserField5', '');
  Result.UserField6 := AObj.Get('UserField6', '');
  Result.UserField7 := AObj.Get('UserField7', '');
  Result.UserField8 := AObj.Get('UserField8', '');
  Result.UserField9 := AObj.Get('UserField9', '');
end;

procedure TVpJSONDatastore.LoadContacts;
begin
  // nothing to do
end;

procedure TVpJSONDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;

procedure TVpJSONDatastore.LoadEvents;
begin
  // nothing to do
end;

procedure TVpJSONDatastore.LoadTasks;
begin
  // nothing to do
end;

procedure TVpJSONDatastore.PostContacts;
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

procedure TVpJSONDatastore.PostEvents;
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

procedure TVpJSONDatastore.PostResources;
begin
  // Nothing to do...
end;

procedure TVpJSONDatastore.PostTasks;
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

procedure TVpJSONDatastore.ReadJSON;
var
  p: TJSONParser;
  stream: TFileStream;
  json: TJSONObject;
  resObj: TJSONObject;
  resObjArray: TJSONArray;
  objArray: TJSONArray;
  res: TVpResource;
  i, j: Integer;
begin
  if FFileName = '' then
    raise Exception.Create(RSNoFilenameSpecified);

  stream := TFileStream.Create(FFilename, fmOpenRead + fmShareDenyWrite);
  try
    Resources.ClearResources;
    p := TJSONParser.Create(stream);
    try
      json := p.Parse as TJSONObject;
      resObjArray := json.Find('Resources', jtArray) as TJSONArray;
      if Assigned(resObjArray) then
        for i := 0 to resObjArray.Count-1 do begin
          resObj := resObjArray.Objects[i];
          res := JSONToResource(resObj);
          // Extract events
          objArray := resObj.Find('Events', jtArray) as TJSONArray;
          if Assigned(objArray) then
            for j := 0 to objArray.Count-1 do
              JSONToEvent(objArray.Objects[j], res);
          // Extract contacts
          objArray := resObj.Find('Contacts', jtArray) as TJSONArray;
          if Assigned(objArray) then
            for j := 0 to  objArray.Count-1 do
              JSONToContact(objArray.Objects[j], res);
          // Extract tasks
          objArray := resObj.Find('Tasks', jtArray) as TJSONArray;
          if Assigned(objArray) then
            for j := 0 to objArray.Count - 1 do
              JSONToTask(objArray.Objects[j], res);
        end;
    finally
      p.Free;
    end;
  finally
    stream.Free;
  end;
end;

function TVPJSONDatastore.ResourceToJSON(AResource: TVpResource): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('ResourceID', AResource.ResourceID);
  Result.Add('Description', AResource.Description);
  Result.Add('Notes', AResource.Notes);
  Result.Add('ResourceActive', AResource.ResourceActive);
  Result.Add('UserField0', AResource.UserField0);
  Result.Add('UserField1', AResource.UserField1);
  Result.Add('UserField2', AResource.UserField2);
  Result.Add('UserField3', AResource.UserField3);
  Result.Add('UserField4', AResource.UserField4);
  Result.Add('UserField5', AResource.UserField5);
  Result.Add('UserField6', AResource.UserField6);
  Result.Add('UserField7', AResource.UserField7);
  Result.Add('UserField8', AResource.UserField8);
  Result.Add('UserField9', AResource.UserField9);
end;

procedure TVpJSONDatastore.SetConnected(const Value: boolean);
begin
  { Don't do anything with live data until run time. }
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;

  { Connecting or disconnecting? }
  if Value then
    ReadJSON
  else
    WriteJSON;

  inherited SetConnected(Value);
end;

procedure TVpJSONDatastore.SetResourceByName(Value: string);
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

procedure TVpJSONDatastore.SetFilename(AValue: String);
begin
  if AValue = FFilename then
    exit;
  if Connected then
    raise Exception.Create('The datastore must not be connected when the filename is set.');
  FFileName := AValue;
  if AutoConnect then ReadJSON;
end;

function TVPJSONDatastore.TaskToJSON(ATask: TVpTask): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('RecordID', ATask.RecordID);
  if ATask.DueDate = 0 then
    Result.Add('DueDate', '') else
    Result.Add('DueDate', FormatDateTime('c', ATask.DueDate, FFormatSettings));
  Result.Add('Description', ATask.Description);
  Result.Add('Details', ATask.Details);
  Result.Add('Complete', ATask.Complete);
  if ATask.CreatedOn = 0 then
    Result.Add('CreatedOn', '') else
    Result.Add('CreatedOn', DateToStr(ATask.CreatedOn, FFormatSettings));
  if ATask.CompletedOn = 0 then
    Result.Add('CompletedOn', '') else
    Result.Add('CompletedOn', DateToStr(ATask.CompletedOn, FFormatSettings));
  Result.Add('Priority', ATask.Priority);
  Result.Add('Category', ATask.Category);
  Result.Add('UserField0', ATask.UserField0);
  Result.Add('UserField1', ATask.UserField1);
  Result.Add('UserField2', ATask.UserField2);
  Result.Add('UserField3', ATask.UserField3);
  Result.Add('UserField4', ATask.UserField4);
  Result.Add('UserField5', ATask.UserField5);
  Result.Add('UserField6', ATask.UserField6);
  Result.Add('UserField7', ATask.UserField7);
  Result.Add('UserField8', ATask.UserField8);
  Result.Add('UserField9', ATask.UserField9);
end;


function TVpJSONDatastore.UniqueID(AValue: Integer): Boolean;
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

procedure TVpJSONDatastore.WriteJSON;
var
  json: TJSONObject;
  resObj: TJSONObject;
  resObjArray, evObjArray, contObjArray, tasksObjArray: TJSONArray;
  res: TVpResource;
  ev: TVpEvent;
  cont: TVpContact;
  task: TvpTask;
  i, j: Integer;
//  s: String;
  stream: TStream;
begin
  if FFilename = '' then
    raise Exception.Create(RSNoFilenameSpecified);

  if not Connected then
    exit;

  json := TJSONObject.Create;
  try
    resObjArray := TJSONArray.Create;

    json.Add('Resources', resObjArray);
    for i := 0 to Resources.Count-1 do begin
      // Add resource
      res := Resources.Items[i];
      if res.Deleted then
        Continue;
      resObj := ResourceToJSON(res);
      resObjArray.Add(resObj);

      // Add events of resource
      if res.Schedule.EventCount > 0 then
      begin
        evObjArray := TJSONArray.Create;
        resObj.Add('Events', evObjArray);
        for j:=0 to res.Schedule.EventCount - 1 do begin
          ev := res.Schedule.GetEvent(j);
          if ev.Deleted then
            Continue;
          evObjArray.Add(EventToJSON(ev));
        end;
      end;

      // Add contacts of resource
      if res.Contacts.Count > 0 then
      begin;
        contObjArray := TJSONArray.Create;
        resObj.Add('Contacts', contObjArray);
        for j:=0 to res.Contacts.Count-1 do begin
          cont := res.Contacts.GetContact(j);
          if cont.Deleted then
            Continue;
          contObjArray.Add(ContactToJSON(cont));
        end;
      end;

      // Add tasks
      if res.Tasks.Count > 0 then
      begin
        tasksObjArray := TJSONArray.Create;
        resObj.Add('Tasks', tasksObjArray);
        for j := 0 to res.Tasks.Count - 1 do begin
          task := res.Tasks.GetTask(j);
          if task.Deleted then
            Continue;
          tasksObjArray.Add(TaskToJSON(task));
        end;
      end;
    end;

    stream := TFileStream.Create(FFilename, fmCreate);
    try
      json.DumpJSON(stream);
//      s := json.FormatJSON;
//      stream.Write(s[1], Length(s));
    finally
      stream.Free;
    end;
  finally
    json.Free;
  end;
end;

end.
