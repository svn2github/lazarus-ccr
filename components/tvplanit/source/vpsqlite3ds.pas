{$I vp.inc}

unit VpSQLite3DS;

interface

uses
  SysUtils, Classes, DB,
  VpBaseDS, VpDBDS,
  sqlite3conn, sqldb;

type
  TVpSqlite3Datastore = class(TVpCustomDBDatastore)
  private
    FConnection: TSqlite3Connection;
    FContactsTable: TSQLQuery;
    FEventsTable: TSQLQuery;
    FResourceTable: TSQLQuery;
    FTasksTable: TSQLQuery;
    procedure SetConnection(const AValue: TSqlite3Connection);

  protected
    procedure CreateTable(const ATableName: String; WithIndex: Boolean = true);
    function GetContactsTable: TDataset; override;
    function GetEventsTable: TDataset; override;
    function GetResourceTable: TDataset; override;
    function GetTasksTable: TDataset; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OpenTables;
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
    function GetNextID(TableName: string): integer; override;
    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;
    procedure PostResources; override;

    property ResourceTable;
    property EventsTable;
    property ContactsTable;
    property TasksTable;

  published
    property Connection: TSqlite3Connection read FConnection write SetConnection;

    // inherited
    property AutoConnect;
    property AutoCreate;
    property DayBuffer;
  end;

var
  // More information on the use of these values is below.
  // They need not be set as constants in your application. They can be any valid value
  APPLICATION_ID : LongWord = 1189021115; // must be a 32-bit Unsigned Integer (Longword 0..4294967295)
  USER_VERSION   : LongInt  = 23400001;   // must be a 32-bit Signed Integer (LongInt -2147483648..2147483647)

implementation

uses
  LazFileUtils,
  VpConst, VpMisc;


{ TVpSqlite3Datastore }

constructor TVpSqlite3Datastore.Create(AOwner: TComponent);
begin
  inherited;

  FContactsTable := TSQLQuery.Create(self);
  FContactsTable.SQL.Add('SELECT * FROM Contacts');

  FEventsTable := TSQLQuery.Create(Self);
  FEventsTable.SQL.Add('SELECT * FROM Events');

  FResourceTable := TSQLQuery.Create(self);
  FResourceTable.SQL.Add('SELECT * FROM Resources');

  FTasksTable := TSQLQuery.Create(self);
  FTasksTable.SQL.Add('SELECT * FROM Tasks');
end;

procedure TVpSqlite3Datastore.AddField(ATableName, AFieldName: String;
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
  (this case is ignored currently).
}
procedure TVpSqlite3Datastore.RenameFields(ATableName: String; AFields: TStrings);
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
  { 1 - Rename old table (append _TMP to tablename) }
  sql := Format('ALTER TABLE %0:s RENAME TO %0:s_TMP;', [ContactsTableName]);
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
  oldFields := TStringList.Create;
  try
    // Get old field list
    FConnection.GetFieldNames(ATableName + '_TMP', oldfields);
    // Combine comma-separated old and new field names in srcfn and destfn
    // strings ready for use by INSERT command.
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
  finally
    oldfields.Free;
  end;

  { 4 - Finally delete the temp table }
  sql := Format('DROP TABLE %s_TMP;', [ATableName]);
  FConnection.ExecuteDirect(sql);
end;

// Connection and tables are active afterwards!
procedure TVpSqlite3Datastore.CreateTables;
var
  wasConnected: Boolean;
begin
  if FileExists(FConnection.DatabaseName) then
    exit;

  wasConnected := FConnection.Connected;

  FConnection.Close;

  if FContactsTable.Transaction = nil then
    FContactsTable.Transaction := FConnection.Transaction;
  if FEventsTable.Transaction = nil then
    FEventsTable.Transaction := FConnection.Transaction;
  if FResourceTable.Transaction = nil then
    FResourceTable.Transaction := FConnection.Transaction;
  if FTasksTable.Transaction = nil then
    FTasksTable.Transaction := FConnection.Transaction;

  CreateTable(ContactsTableName);
  CreateTable(EventsTableName);
  CreateTable(ResourceTableName);
  CreateTable(TasksTableName);

  SetConnected(wasConnected or AutoConnect);
end;

procedure TVpSqlite3Datastore.CreateTable(const ATableName: String;
  WithIndex: Boolean = true);
begin
  if ATableName = ContactsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Contacts ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'ResourceID INTEGER, ' +
        'FirstName VARCHAR(50), '+
        'LastName VARCHAR(50), '+
        'Category INTEGER, '+
        'Birthdate DATE, '+
        'Anniversary DATE, '+
        'Title VARCHAR(50), '+
        'Company VARCHAR(50), '+
        'Department VARCHAR(50), '+
        'Job_Position VARCHAR(30), '+
        'AddressType1 INTEGER DEFAULT 0, '+
        'Address1 VARCHAR(100), '+
        'City1 VARCHAR(50), '+
        'State1 VARCHAR(25), '+
        'Zip1 VARCHAR(10), '+
        'Country1 VARCHAR(25), '+
        'AddressType2 INTEGER DEFAULT 1, '+
        'Address2 VARCHAR(100), '+
        'City2 VARCHAR(50), '+
        'State2 VARCHAR(25), '+
        'Zip2 VARCHAR(10), '+
        'Country2 VARCHAR(25), '+
        'Notes VARCHAR(1024), '+
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
        'EMail1 VARCHAR(100), '+
        'EMail2 VARCHAR(100), '+
        'EMail3 VARCHAR(100), '+
        'EMailType1 INTEGER DEFAULT 0, '+
        'EMailType2 INTEGER DEFAULT 1, '+
        'EMailType3 INTEGER DEFAULT 2, '+
        'Website1 VARCHAR(100), '+
        'Website2 VARCHAR(100), '+
        'WebsiteType1 INTEGER DEFAULT 0, '+
        'WebsiteType2 INTEGER DEFAULT 1, '+
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
    if WithIndex then begin
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
    if WithIndex then begin
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
    if WithIndex then begin
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

{ Updates the contacts table and adds/renames new fields in the current version.
  IMPORTANT: The renaming process assumes that the renamed column is not part of
  any index. }
procedure TVpSqlite3Datastore.FixContactsTable;
var
  list: TStrings;
  fnames: TStrings;
begin
  ContactsTable.Close;
  list := TStringList.Create;
  try
    FConnection.GetFieldNames(ContactsTableName, list);

    // Fields renamed in 1.05
    fnames := TStringList.Create;
    try
      if list.IndexOf('Address') > -1 then fnames.Add('Address|Address1');
      if list.IndexOf('City') > -1 then fnames.Add('City|City1');
      if list.IndexOf('State') > -1 then fnames.Add('State|State1');
      if list.IndexOf('Zip') > -1 then fnames.Add('Zip|Zip1');
      if list.IndexOf('Country') > -1 then fnames.Add('Country|Country1');
      if list.IndexOf('EMail') > -1 then fnames.Add('EMail|EMail1');
      if fnames.Count > 0 then begin
        RenameFields(ContactsTableName, fnames);
        exit;   // This automatically creates the new fields
      end;
    finally
      fnames.Free;
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
  finally
    list.Free;
  end;
end;

function TVpSqlite3Datastore.GetContactsTable: TDataset;
begin
  Result := FContactsTable;
end;

function TVpSqlite3Datastore.GetEventsTable: TDataset;
begin
  Result := FEventsTable;
end;

function TVpSqlite3DataStore.GetNextID(TableName: string): integer;
begin
  Unused(TableName);
  { This is not needed in the SQLITE3 datastore as these tables use
    autoincrement fields. }
  Result := -1;
end;

function TVpSqlite3Datastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpSqlite3Datastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

procedure TVpSqlite3Datastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect and (AutoCreate or FileExists(FConnection.DatabaseName));
end;

procedure TVpSqlite3Datastore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TVpSqlite3Datastore.OpenTables;
begin
  if FContactsTable.Transaction = nil then
    FContactsTable.Transaction := FConnection.Transaction;
  FixContactsTable;
  FContactsTable.Open;

  if FEventsTable.Transaction = nil then
    FEventsTable.Transaction := FConnection.Transaction;
  FEventsTable.Open;

  if FResourceTable.Transaction = nil then
    FResourceTable.Transaction := FConnection.Transaction;
  FResourceTable.Open;

  if FTasksTable.Transaction = nil then
    FTasksTable.Transaction := FConnection.Transaction;
  FTasksTable.Open;
end;

procedure TVpSqlite3Datastore.PostContacts;
begin
  inherited;
  FContactsTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.PostEvents;
begin
  inherited;
  FEventsTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.PostResources;
begin
  inherited;
  FResourceTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.PostTasks;
begin
  inherited;
  FTasksTable.ApplyUpdates;
end;

procedure TVpSqlite3Datastore.SetConnected(const AValue: Boolean);
begin
  if (FConnection = nil) or (FConnection.Transaction = nil) then
    exit;

  if AValue = FConnection.Connected then
    exit;

  if AValue and AutoCreate then
    CreateTables;

  FConnection.Connected := AValue;
  if AValue then
  begin
    FConnection.Transaction.Active := true;
    OpenTables;
  end;

  inherited SetConnected(AValue);

  if FConnection.Connected then
    Load;
end;

procedure TVpSqlite3Datastore.SetConnection(const AValue: TSqlite3Connection);
var
  wasConnected: Boolean;
begin
  if AValue = FConnection then
    exit;

  // To do: clear planit lists...
  if FConnection <> nil then begin
    wasConnected := FConnection.Connected;
    Connected := false;
  end;
  FConnection := AValue;

  FContactsTable.Database := FConnection;
  FContactsTable.Transaction := FConnection.Transaction;

  FEventsTable.Database := FConnection;
  FEventsTable.Transaction := FConnection.Transaction;

  FResourceTable.Database := FConnection;
  FResourceTable.Transaction := FConnection.Transaction;

  FTasksTable.Database := FConnection;
  FTasksTable.Transaction := FConnection.Transaction;
  if wasConnected then Connected := true;
end;

end.
