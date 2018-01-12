{$I vp.inc}

unit VpZeosDs;

interface

uses
  SysUtils, Classes, DB,
  VpBaseDS, VpDBDS,
  ZCompatibility, ZConnection, ZDataset;

type
  TVpZeosDatastore = class(TVpCustomDBDatastore)
  private
    FConnection: TZConnection;
    FContactsTable: TZTable;
    FEventsTable: TZTable;
    FResourceTable: TZTable;
    FTasksTable: TZTable;
    procedure SetConnection(const AValue: TZConnection);

  protected
    procedure CreateTable(const ATableName: String; CreateIndex: Boolean = true);
    procedure CreateAllTables;
    function GetContactsTable: TDataset; override;
    function GetEventsTable: TDataset; override;
    function GetResourceTable: TDataset; override;
    function GetTasksTable: TDataset; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetConnected(const AValue: Boolean); override;

  protected
    // Fix old tables
    procedure AddField(ATableName, AFieldName: String; AFieldType: TFieldType;
      ASize: Integer=0);
    procedure RenameFields(ATableName: String; AFields: TStrings);
    procedure FixContactsTable;

  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateTables;
    function GetNextID({%H-}TableName: string): integer; override;

    property ResourceTable;
    property EventsTable;
    property ContactsTable;
    property TasksTable;

  published
    property Connection: TZConnection read FConnection write SetConnection;

    // inherited
    property AutoConnect default false;
    property AutoCreate default false;
    property Daybuffer;
  end;


implementation

uses
  LazFileUtils, ZAbstractDataset,
  VpConst;

{ TVpZeosDatastore }

constructor TVpZeosDatastore.Create(AOwner: TComponent);
begin
  inherited;

  FContactsTable := TZTable.Create(self);
  FContactsTable.TableName := 'Contacts';
  FContactsTable.UpdateMode := umUpdateAll;

  FEventsTable := TZTable.Create(Self);
  FEventsTable.TableName := 'Events';
  FEventsTable.UpdateMode := umUpdateAll;

  FResourceTable := TZTable.Create(self);
  FResourceTable.TableName := 'Resources';
  FResourceTable.UpdateMode := umUpdateAll;

  FTasksTable := TZTable.Create(self);
  FTasksTable.TableName := 'Tasks';
  FTasksTable.UpdateMode := umUpdateAll;
end;

procedure TVpZeosDatastore.AddField(ATableName, AFieldName: String;
  AFieldType: TFieldType; ASize: Integer=0);
var
  ft: String;
  sql: String;
begin
  if AFieldType = ftInteger then
    ft := 'INTEGER' else
  if (AFieldType = ftString) then
    ft := 'VARCHAR(' + intToStr(ASize) + ')'
  else
    raise Exception.Create('Field type not supported here.');
  sql := Format('ALTER TABLE %s ADD COLUMN %s %s;', [ATablename, AFieldName, ft]);
  FConnection.ExecuteDirect(sql);
end;

procedure TVpZeosDatastore.CreateAllTables;
begin
  if not FContactsTable.Exists then CreateTable(ContactsTableName);
  if not FEventsTable.Exists then CreateTable(EventsTableName);
  if not FResourceTable.Exists then CreateTable(ResourceTableName);
  if not FTasksTable.Exists then CreateTable(TasksTableName);
end;

procedure TVpZeosDatastore.CreateTable(const ATableName: String;
  CreateIndex: Boolean = true);
begin
  if ATableName = ContactsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Contacts ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'ResourceID INTEGER, ' +
        'FirstName VARCHAR(50), '+
        'LastName VARCHAR(50), '+
        'Title VARCHAR(20) ,'+
        'Category INTEGER, '+
        'Birthdate DATE, '+
        'Anniversary DATE, '+
        'Company VARCHAR(50), '+
        'Department VARCHAR(50), '+
        'Job_Position VARCHAR(30), '+
        'AddressType1 INTEGER, '+
        'Address1 VARCHAR(100), '+
        'City1 VARCHAR(50), '+
        'State1 VARCHAR(25), '+
        'Zip1 VARCHAR(10), '+
        'Country1 VARCHAR(25), '+
        'AddressType2 INTEGER, '+
        'Address2 VARCHAR(100), '+
        'City2 VARCHAR(50), '+
        'State2 VARCHAR(25), '+
        'Zip2 VARCHAR(10), '+
        'Country2 VARCHAR(25), '+
        'Notes VARCHAR(1024), '+
        'EMail1 VARCHAR(100), '+
        'EMail2 VARCHAR(100), '+
        'EMail3 VARCHAR(100), '+
        'EMailType1 INTEGER, '+
        'EMailType2 INTEGER, '+
        'EMailType3 INTEGER, '+
        'Phone1 VARCHAR(25), '+
        'Phone2 VARCHAR(25), '+
        'Phone3 VARCHAR(25), '+
        'Phone4 VARCHAR(25), '+
        'Phone5 VARCHAR(25), '+
        'PhoneType1 INTEGER, '+
        'PhoneType2 INTEGER, '+
        'PhoneType3 INTEGER, '+
        'PhoneType4 INTEGER, '+
        'PhoneType5 INTEGER, '+
        'Website1 VARCHAR(100), '+
        'Website2 VARCHAR(100), '+
        'WebsiteType1 INTEGER, '+
        'WebsiteType2 INTEGER, '+
        'Custom1 VARCHAR(100), '+
        'Custom2 VARCHAR(100),'+
        'Custom3 VARCHAR(100), '+
        'Custom4 VARCHAR(100), '+
        'UserField0 VARCHAR(100), '+
        'UserField1 VARCHAR(100), '+
        'UserField2 VARCHAR(100), '+
        'UserField3 VARCHAR(100), '+
        'UserField4 VARCHAR(100), '+
        'UserField5 VARCHAR(100), '+
        'UserField6 VARCHAR(100), '+
        'UserField7 VARCHAR(100), '+
        'UserField8 VARCHAR(100), '+
        'UserField9 VARCHAR(100) )'
    );
    if CreateIndex then begin
      FConnection.ExecuteDirect(
        'CREATE INDEX ContactsResourceID_idx ON Contacts(ResourceID)'
      );
      FConnection.ExecuteDirect(
        'CREATE INDEX ContactsName_idx ON Contacts(LastName, FirstName)'
      );
      FConnection.ExecuteDirect(
        'CREATE INDEX ContactsCompany_idx ON Contacts(Company)'
      );
    end;
  end else
  if ATableName = EventsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Events ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'StartTime TIMESTAMP, '+
        'EndTime TIMESTAMP, '+
        'ResourceID INTEGER, '+
        'Description VARCHAR(255), '+
        'Location VARCHAR(255), '+
        'Notes VARCHAR(1024), ' +
        'Category INTEGER, '+
        'AllDayEvent BOOL, '+
        'DingPath VARCHAR(255), '+
        'AlarmSet BOOL, '+
        'AlarmAdvance INTEGER, '+
        'AlarmAdvanceType INTEGER, '+
        'SnoozeTime TIMESTAMP, '+
        'RepeatCode INTEGER, '+
        'RepeatRangeEnd TIMESTAMP, '+
        'CustomInterval INTEGER, '+
        'UserField0 VARCHAR(100), '+
        'UserField1 VARCHAR(100), '+
        'UserField2 VARCHAR(100), '+
        'UserField3 VARCHAR(100), '+
        'UserField4 VARCHAR(100), '+
        'UserField5 VARCHAR(100), '+
        'UserField6 VARCHAR(100), '+
        'UserField7 VARCHAR(100), '+
        'UserField8 VARCHAR(100), '+
        'UserField9 VARCHAR(100) )'
    );
    if CreateIndex then begin
      FConnection.ExecuteDirect(
        'CREATE INDEX EventsResourceID_idx ON Events(ResourceID)'
      );
      FConnection.ExecuteDirect(
        'CREATE INDEX EventsStartTime_idx ON Events(StartTime)'
      );
      FConnection.ExecuteDirect(
        'CREATE INDEX EventsEndTime_idx ON Events(EndTime)'
      );
    end;
  end else
  if ATableName = ResourceTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Resources ( '+
         'ResourceID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
         'Description VARCHAR(255), '+
         'Notes VARCHAR(1024), '+
         'ImageIndex INTEGER, '+
         'ResourceActive BOOL, '+
         'UserField0 VARCHAR(100), '+
         'UserField1 VARCHAR(100), '+
         'UserField2 VARCHAR(100), '+
         'UserField3 VARCHAR(100), '+
         'UserField4 VARCHAR(100), '+
         'UserField5 VARCHAR(100), '+
         'UserField6 VARCHAR(100), '+
         'UserField7 VARCHAR(100), '+
         'UserField8 VARCHAR(100), '+
         'UserField9 VARCHAR(100) )'
    );
  end else
  if ATableName = TasksTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Tasks ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'ResourceID INTEGER, '+
        'Complete BOOL, '+
        'Description VARCHAR(255), '+
        'Details VARCHAR(1024), '+
        'CreatedOn TIMESTAMP, '+
        'Priority INTEGER, '+
        'Category INTEGER, '+
        'CompletedOn TIMESTAMP, '+
        'DueDate TIMESTAMP, '+
        'UserField0 VARCHAR(100), '+
        'UserField1 VARCHAR(100), '+
        'UserField2 VARCHAR(100), '+
        'UserField3 VARCHAR(100), '+
        'UserField4 VARCHAR(100), '+
        'UserField5 VARCHAR(100), '+
        'UserField6 VARCHAR(100), '+
        'UserField7 VARCHAR(100), '+
        'UserField8 VARCHAR(100), '+
        'UserField9 VARCHAR(100) )'
    );
    if CreateIndex then begin
      FConnection.ExecuteDirect(
        'CREATE INDEX TasksResourceID_idx ON Tasks(ResourceID)'
      );
      FConnection.ExecuteDirect(
        'CREATE INDEX TasksDueDate_idx ON Tasks(DueDate)'
      );
      FConnection.ExecuteDirect(
        'CREATE INDEX TasksCompletedOn_idx ON Tasks(CompletedOn)'
      );
    end;
  end;
end;

procedure TVpZeosDatastore.CreateTables;
var
  wasConnected: Boolean;
begin
  wasConnected := FConnection.Connected;
  FConnection.Connected := true;
  CreateAllTables;
  SetConnected(wasConnected or AutoConnect);
end;

procedure TVpZeosDatastore.FixContactsTable;
var
  list: TStrings;
  autocommit: Boolean;
  fieldnames: TStrings;
begin
  autocommit := FConnection.AutoCommit;
  ContactsTable.Close;
  list := TStringList.Create;
  try
    FConnection.GetColumnNames(ContactsTableName, '', list);
    FConnection.AutoCommit := false;
    try
      // Fields renamed in 1.05
      fieldnames := TStringList.Create;
      try
        if list.IndexOf('Address') > -1 then fieldnames.Add('Address|Address1');
        if list.IndexOf('City') > -1 then fieldnames.Add('City|City1');
        if list.IndexOf('State') > -1 then fieldnames.Add('State|State1');
        if list.IndexOf('Zip') > -1 then fieldnames.Add('Zip|Zip1');
        if list.IndexOf('Country') > -1 then fieldnames.Add('Country|Country1');
        if list.IndexOf('EMail') > -1 then fieldnames.Add('EMail|EMail1');
        if fieldnames.Count > 0 then begin
          RenameFields(ContactsTableName, fieldnames);
          exit;   // This automatically creates the new fields
        end;
      finally
        fieldnames.Free;
      end;

      // Fields added in 1.05
      if list.IndexOf('Department') = -1 then
        AddField(ContactsTableName, 'Department', ftString, 50);
      if list.IndexOf('AddressType1') = -1 then
        AddField(ContactsTableName, 'AddressType1', ftInteger);
      if list.IndexOf('AddressType2') = -1 then
        AddField(ContactsTableName, 'AddressType2', ftInteger);
      if list.IndexOf('Address2') = -1 then
        AddField(ContactsTableName, 'Address2', ftString, 100);
      if list.IndexOf('City2') = -1 then
        AddField(ContactsTableName, 'City2', ftString, 50);
      if list.IndexOf('State2') = -1 then
        AddField(ContactsTableName, 'State2', ftString, 25);
      if list.IndexOf('Zip2') = -1 then
        AddField(ContactsTableName, 'Zip2', ftString, 10);
      if list.IndexOf('country2') = -1 then
        AddField(ContactsTableName, 'Country2', ftString, 25);
      if list.IndexOf('EMail2') = -1 then
        AddField(ContactsTableName, 'EMail2', ftString, 100);
      if list.IndexOf('EMail3') = -1 then
        AddField(ContactsTableName, 'EMail3', ftString, 100);
      if list.IndexOf('EMailType1') = -1 then
        AddField(ContactsTableName, 'EMailType1', ftInteger);
      if list.IndexOf('EMailType2') = -1 then
        AddField(ContactsTableName, 'EMailType2', ftInteger);
      if list.IndexOf('EMailType3') = -1 then
        AddField(ContactsTableName, 'EMailType3', ftInteger);
      if list.IndexOf('Website1') = -1 then
        AddField(ContactsTableName, 'Website1', ftString, 100);
      if list.IndexOf('Website2') = -1 then
        AddField(ContactsTableName, 'Website2', ftString, 100);
      if list.IndexOf('WebsiteType1') = -1 then
        AddField(ContactsTableName, 'WebsiteType1', ftInteger);
      if list.IndexOf('WebsiteType2') = -1 then
        AddField(ContactsTableName, 'WebsiteType2', ftInteger);

      FConnection.Commit;
    except
      FConnection.Rollback;
      raise Exception.Create('Failure to update table structure to current VisualPlanIt version');
    end;
  finally
    list.Free;
  end;

  FConnection.Connected := false;
  FConnection.AutoCommit := autocommit;
  FConnection.Connected := true;
end;

function TVpZeosDatastore.GetContactsTable: TDataset;
begin
  Result := FContactsTable;
end;

function TVpZeosDatastore.GetEventsTable: TDataset;
begin
  Result := FEventsTable;
end;

function TVpZeosDataStore.GetNextID(TableName: string): integer;
begin
  { This is not needed in the ZEOS datastore as these tables use
    autoincrement fields. }
  result := -1;
end;

function TVpZeosDatastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpZeosDatastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

procedure TVpZeosDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect and (
      AutoCreate or
      (FContactsTable.Exists and FEventsTable.Exists and FResourceTable.Exists and FTasksTable.Exists)
    );
end;

procedure TVpZeosDatastore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

{ Renames the fields specified in the list. Each list item contains old and new
  fieldnames separated by a bar character (|).

  Note that sqlite3 does not provide a command for renaming of fields. Therefore,
  the old table is renamed to a temp table, a new table with the renamed fields
  is created and the content of the temp table is copied to the new table.
  Finally the temp table is deleted.

  See:
  https://stackoverflow.com/questions/805363/how-do-i-rename-a-column-in-a-sqlite-database-table

  TO DO:
  Take care of the case that a renamed field belongs to an index, constraint, etc.
  (this case is ignored currently). }
procedure TVpZeosDatastore.RenameFields(ATableName: String; AFields: TStrings);
const
  NO_INDEX = false;
var
  sql: String;
  oldFields: TStrings;
  oldfn, newfn: String;
  srcfn, destfn: String;
  i, j, p: Integer;
  done: Boolean;
begin
  oldfields := TStringList.Create;
  try
    FConnection.GetColumnNames(ATableName, '', oldfields);

    { 1 - Rename old table (append _TMP to tablename) }
    sql := Format('ALTER TABLE %0:s RENAME TO %0:s_TMP;', [ATableName]);
    FConnection.ExecuteDirect(sql);

    { 2 - Create new table }
    if ATableName = ContactsTableName then
      CreateTable(ContactsTableName, NO_INDEX) else
    if ATablename = EventsTableName then
      CreateTable(EventsTablename, NO_INDEX) else
    if ATableName = ResourceTableName then
      CreateTable(ResourceTableName, NO_INDEX) else
    if ATableName = TasksTableName then
      CreateTable(TasksTableName, NO_INDEX)
    else
      raise Exception.Create('Unknown table in RenameFields.');

    { 3 - Copy contents from temp table to new table }
    srcfn := '';
    destfn := '';
    for i:=0 to oldfields.Count-1 do begin
      done := false;
      // Is field "oldfields[i]" contained in the list of fields to be renamed?
      for j:=0 to AFields.Count-1 do begin
        p := pos('|', AFields[j]);
        oldfn := copy(AFields[j], 1, p-1);
        newfn := copy(AFields[j], p+1, MaxInt);
        if oldfn = oldfields[i] then begin
          // yes: add old field name to srcfn, new field name to destfn
          srcfn := srcfn + ',' + oldfn;
          destfn := destfn + ',' + newfn;
          done := true;
          break;
        end;
      end;
      if not done then begin
        // no: add current field name to srcfn and destfn
        srcfn := srcfn + ',' + oldfields[i];
        destfn := destfn + ',' + oldfields[i];
      end;
    end;
    // Remove the beginning comma added above.
    if srcfn <> '' then System.Delete(srcfn, 1, 1);
    if destfn <> '' then System.Delete(destfn, 1, 1);
    // Execute INSERT command
    sql := Format(
      'INSERT INTO %0:s (%1:s) SELECT %2:s FROM %0:s_TMP;', [
      ATableName, destfn, srcfn
    ]);
    FConnection.ExecuteDirect(sql);

    { 4 - Finally delete the temp table }
    sql := Format('DROP TABLE %s_TMP;', [ATableName]);
    FConnection.ExecuteDirect(sql);

    FConnection.Disconnect;
    FConnection.Connect;
  finally
    oldfields.Free;
  end;
end;

procedure TVpZeosDatastore.SetConnected(const AValue: Boolean);
begin
  if (AValue = Connected) or (FConnection = nil) then
    exit;

  if AValue and AutoCreate then
    CreateTables;

  FConnection.Connected := AValue;
  if FConnection.Connected then begin
    FixContactsTable;
    FContactsTable.Open;
    FEventsTable.Open;
    FResourceTable.Open;
    FTasksTable.Open;
  end;

  inherited SetConnected(AValue);

  if FConnection.Connected then
    Load;
end;

procedure TVpZeosDatastore.SetConnection(const AValue: TZConnection);
var
  wasConnected: Boolean;
begin
  if AValue = FConnection then
    exit;

  // To do: clear planit lists...
  if FConnection <> nil then begin
    wasConnected := FConnection.Connected;
    Connected := false;
  end else
    wasConnected := false;
  FConnection := AValue;
  FContactsTable.Connection := FConnection;
  FEventsTable.Connection := FConnection;
  FResourceTable.Connection := FConnection;
  FTasksTable.Connection := FConnection;
  if wasConnected then Connected := true;
end;

end.
