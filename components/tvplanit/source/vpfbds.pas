{$I vp.inc}

{ A datastore for a Firebird database accessed via SQLDB }

unit VpFBDS;

interface

uses
  SysUtils, Classes, DB,
  VpBaseDS, VpDBDS,
  IBConnection, sqldb;

type
  TVpFirebirdDatastore = class(TVpCustomDBDatastore)
  private
    FConnection: TIBConnection;
    FContactsTable: TSQLQuery;
    FEventsTable: TSQLQuery;
    FResourceTable: TSQLQuery;
    FTasksTable: TSQLQuery;
    FConnectLock: Integer;
    procedure SetConnection(const AValue: TIBConnection);

  protected
    procedure CreateAllTables(dbIsNew: Boolean);
    procedure CreateTable(const ATableName: String);
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
    procedure RenameField(ATableName, AOldFieldName, ANewFieldName: String);
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
    property Connection: TIBConnection read FConnection write SetConnection;

    // inherited
    property AutoConnect;
    property AutoCreate;
    property DayBuffer;
  end;


implementation

uses
  LazFileUtils,
  VpConst, VpMisc;

{ TVpIBDatastore }

constructor TVpFirebirdDatastore.Create(AOwner: TComponent);
begin
  inherited;

  FContactsTable := TSQLQuery.Create(self);
  FContactsTable.SQL.Add('SELECT * FROM Contacts');
  FContactsTable.UpdateMode := upWhereAll;

  FEventsTable := TSQLQuery.Create(Self);
  FEventsTable.SQL.Add('SELECT * FROM Events');
  FEventsTable.UpdateMode := upWhereAll;

  FResourceTable := TSQLQuery.Create(self);
  FResourceTable.SQL.Add('SELECT * FROM Resources');
  FResourceTable.UpdateMode := upWhereAll;

  {
  FResourceTable.InsertSQL.Add(
    'INSERT INTO Resources (' +
      'ResourceID, Description, Notes, ResourceActive, ' +
      'UserField0, UserField1, UserField2, UserField3, UserField4, ' +
      'UserField5, UserField6, UserField7, UserField8, UserField9) ' +
    'VALUES(' +
      ':ResourceID, :Description, :Notes, :ResourceActive, ' +
      ':UserField0, :UserField1, :UserField2, :UserField3, :UserField4, ' +
      ':UserField5, :UserField6, :UserField7, :UserField8, :UserField9);'
  );
   }
  FTasksTable := TSQLQuery.Create(self);
  FTasksTable.SQL.Add('SELECT * FROM Tasks');
  FTasksTable.UpdateMode := upWhereAll;
end;

procedure TVpFirebirdDatastore.AddField(ATableName, AFieldName: String;
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
  sql := Format('ALTER TABLE %s ADD %s %s;', [ATablename, AFieldName, ft]);
  FConnection.ExecuteDirect(sql);
end;

procedure TVpFirebirdDatastore.RenameField(
  ATableName, AOldFieldName, ANewFieldName: String);
var
  sql: String;
begin
  sql := Format('ALTER TABLE %s ALTER %s TO %s;',
    [ATableName, AOldFieldName, ANewFieldName]);
  FConnection.ExecuteDirect(sql);
end;

procedure TVpFirebirdDatastore.CreateAllTables(dbIsNew: Boolean);
var
  tableNames: TStringList;
begin
  if dbIsNew then begin
    CreateTable(ContactsTableName);
    CreateTable(EventsTableName);
    CreateTable(ResourceTableName);
    CreateTable(TasksTableName);
    FConnection.Transaction.Commit;
  end else
  begin
    tablenames := TStringList.Create;
    try
      tablenames.CaseSensitive := false;
      FConnection.GetTableNames(tablenames);

      if tablenames.IndexOf(ContactsTableName) = -1 then begin
        CreateTable(ContactsTableName);
        FConnection.Transaction.Commit;
      end;

      if tablenames.IndexOf(EventsTableName) = -1 then begin
        CreateTable(EventsTableName);
        FConnection.Transaction.Commit;
      end;

      if tablenames.IndexOf(ResourceTableName) = -1 then begin
        CreateTable(ResourceTableName);
        FConnection.Transaction.Commit;
      end;

      if tablenames.IndexOf(TasksTableName) = -1 then begin
        CreateTable(TasksTableName);
        FConnection.Transaction.Commit;
      end;
    finally
      tablenames.Free;
    end;
  end;
end;

// Connection and tables are active afterwards!
procedure TVpFirebirdDatastore.CreateTables;
var
  wasConnected: Boolean;
  isNew: Boolean;
begin
  isNew := false;
  wasConnected := FConnection.Connected;
  if not FileExistsUTF8(FConnection.DatabaseName) then begin
    FConnection.Connected := false;
    FConnection.CreateDB;
    isNew := true;
  end;
  FConnection.Connected := true;
  CreateAllTables(isNew);
  SetConnected(wasConnected or AutoConnect);
end;

{ Note: Firebird with version < 3 does not support AutoInc fields.
  Use a generator and trigger to create AutoInc values:
  http://www.firebirdfaq.org/faq29/ }
procedure TVpFirebirdDatastore.CreateTable(const ATableName: String);
begin
  if ATableName = ContactsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Contacts (' +
        'RecordID       INTEGER NOT NULL PRIMARY KEY, '+
        'ResourceID     INTEGER NOT NULL, ' +
        'FirstName      VARCHAR(50), '+
        'LastName       VARCHAR(50), '+
        'Birthdate      DATE, '+
        'Anniversary    DATE, '+
        'Title          VARCHAR(50), '+
        'Category       INTEGER, '+
        'Company        VARCHAR(50), '+
        'Department     VARCHAR(50), '+
        'Job_Position   VARCHAR(30), '+
        'AddressType1   INTEGER, '+
        'Address1       VARCHAR(100), '+
        'City1          VARCHAR(50), '+
        'State1         VARCHAR(25), '+
        'Zip1           VARCHAR(10), '+
        'Country1       VARCHAR(25), '+
        'AddressType2   INTEGER, '+
        'Address2       VARCHAR(100), '+
        'City2          VARCHAR(50), '+
        'State2         VARCHAR(25), '+
        'Zip2           VARCHAR(10), '+
        'Country2       VARCHAR(25), '+
        'Phone1         VARCHAR(25), '+
        'Phone2         VARCHAR(25), '+
        'Phone3         VARCHAR(25), '+
        'Phone4         VARCHAR(25), '+
        'Phone5         VARCHAR(25), '+
        'PhoneType1     INTEGER, '+
        'PhoneType2     INTEGER, '+
        'PhoneType3     INTEGER, '+
        'PhoneType4     INTEGER, '+
        'PhoneType5     INTEGER, '+
        'EMail1         VARCHAR(100), '+
        'EMail2         VARCHAR(100), '+
        'EMail3         VARCHAR(100), '+
        'EMailType1     INTEGER, '+
        'EMailType2     INTEGER, '+
        'EMailType3     INTEGER, '+
        'WebSite1       VARCHAR(100), '+
        'WebSite2       VARCHAR(100), '+
        'WebSiteType1   INTEGER, '+
        'WebSiteType2   INTEGER, '+
        'Notes          VARCHAR(1024), '+
        'Custom1        VARCHAR(100), '+
        'Custom2        VARCHAR(100), '+
        'Custom3        VARCHAR(100), '+
        'Custom4        VARCHAR(100), '+
        'UserField0     VARCHAR(100), '+
        'UserField1     VARCHAR(100), '+
        'UserField2     VARCHAR(100), '+
        'UserField3     VARCHAR(100), '+
        'UserField4     VARCHAR(100), '+
        'UserField5     VARCHAR(100), '+
        'UserField6     VARCHAR(100), '+
        'UserField7     VARCHAR(100), '+
        'UserField8     VARCHAR(100), '+
        'UserField9     VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Contacts_RecordID_idx ON Contacts (RecordID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Contacts_ResourceID_idx ON Contacts (ResourceID);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Contacts_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Contacts_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER C_AUTOINC_TRG FOR Contacts ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.RecordID = GEN_ID(Contacts_AUTOINC, 1); ' +
      'END '
    );
  end else
  if ATableName = EventsTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Events (' +
        'RecordID         INTEGER NOT NULL PRIMARY KEY, ' +
        'ResourceID       INTEGER NOT NULL, ' +
        'StartTime        TIMESTAMP NOT NULL, ' +
        'EndTime          TIMESTAMP NOT NULL, ' +
        'Description      VARCHAR (255), ' +
        'Location         VARCHAR (255), ' +
        'Notes            VARCHAR (1024), ' +
        'Category         INTEGER, ' +
        'AllDayEvent      CHAR(1), ' +
        'DingPath         VARCHAR (255), ' +
        'AlarmSet         CHAR(1), ' +
        'AlarmAdvance     INTEGER, ' +
        'AlarmAdvanceType INTEGER, ' +
        'SnoozeTime       TIMESTAMP, ' +
        'RepeatCode       INTEGER, ' +
        'RepeatRangeEnd   TIMESTAMP, ' +
        'CustomInterval   INTEGER, ' +
        'UserField0       VARCHAR(100), '+
        'UserField1       VARCHAR(100), '+
        'UserField2       VARCHAR(100), '+
        'UserField3       VARCHAR(100), '+
        'UserField4       VARCHAR(100), '+
        'UserField5       VARCHAR(100), '+
        'UserField6       VARCHAR(100), '+
        'UserField7       VARCHAR(100), '+
        'UserField8       VARCHAR(100), '+
        'UserField9       VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Events_RecordID_idx ON Events (RecordID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Events_ResourceID_idx ON Events (ResourceID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Events_StartTime_idx ON Events (StartTime);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Events_EndTime_idx ON Events (EndTime);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Events_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Events_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER E_AUTOINC_TRG FOR Events ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.RecordID = GEN_ID(Events_AUTOINC, 1); ' +
      'END '
    );
  end else
  if ATableName = ResourceTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Resources (' +
        'ResourceID     INTEGER NOT NULL PRIMARY KEY, '+
        'Description    VARCHAR (255), ' +
        'Notes          VARCHAR (1024), ' +
        'ImageIndex     INTEGER, ' +
        'ResourceActive CHAR(1), ' +
        'UserField0     VARCHAR(100), '+
        'UserField1     VARCHAR(100), '+
        'UserField2     VARCHAR(100), '+
        'UserField3     VARCHAR(100), '+
        'UserField4     VARCHAR(100), '+
        'UserField5     VARCHAR(100), '+
        'UserField6     VARCHAR(100), '+
        'UserField7     VARCHAR(100), '+
        'UserField8     VARCHAR(100), '+
        'UserField9     VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Resources_ResourceID_idx ON Resources (ResourceID);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Resources_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Resources_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER R_AUTOINC_TRG FOR Resources ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.ResourceID = GEN_ID(Resources_AUTOINC, 1); ' +
      'END '
    );
  end else
  if ATableName = TasksTableName then begin
    FConnection.ExecuteDirect(
      'CREATE TABLE Tasks (' +
        'RecordID       INTEGER NOT NULL PRIMARY KEY, ' +
        'ResourceID     INTEGER NOT NULL, ' +
        'Complete       CHAR(1), ' +
        'Description    VARCHAR(255), ' +
        'Details        VARCHAR(1024), ' +
        'CreatedOn      TIMESTAMP, ' +
        'Priority       INTEGER, ' +
        'Category       INTEGER, ' +
        'CompletedOn    TIMESTAMP, ' +
        'DueDate        TIMESTAMP, ' +
        'UserField0     VARCHAR(100), '+
        'UserField1     VARCHAR(100), '+
        'UserField2     VARCHAR(100), '+
        'UserField3     VARCHAR(100), '+
        'UserField4     VARCHAR(100), '+
        'UserField5     VARCHAR(100), '+
        'UserField6     VARCHAR(100), '+
        'UserField7     VARCHAR(100), '+
        'UserField8     VARCHAR(100), '+
        'UserField9     VARCHAR(100) )'
    );
    FConnection.ExecuteDirect(
      'CREATE UNIQUE INDEX Tasks_RecordID_idx ON Tasks (RecordID);');
    FConnection.ExecuteDirect(
      'CREATE INDEX Tasks_ResourceID_idx ON Tasks (ResourceID);');

    FConnection.ExecuteDirect(
      'CREATE GENERATOR Tasks_AUTOINC; ');
    FConnection.ExecuteDirect(
      'SET GENERATOR Tasks_AUTOINC TO 0; ');
    FConnection.ExecuteDirect(
      'CREATE TRIGGER T_AUTOINC_TRG FOR Tasks ' +
      'ACTIVE BEFORE INSERT POSITION 0 ' +
      'AS ' +
      'BEGIN ' +
        'NEW.RecordID = GEN_ID(Tasks_AUTOINC, 1); ' +
      'END '
    );
  end;
end;

procedure TVpFirebirdDatastore.FixContactsTable;
var
  list: TStrings;
begin
  ContactsTable.Close;
  list := TStringList.Create;
  try
    FConnection.GetFieldNames(ContactsTableName, list);

    try
      // Fields renamed in 1.05
      if list.IndexOf('Address') > -1 then
        RenameField(ContactsTableName, 'Address', 'Address1');
      if list.IndexOf('City') > -1 then
        RenameField(ContactsTableName, 'City', 'City1');
      if list.IndexOf('State') > -1 then
        RenameField(ContactsTableName, 'State', 'State1');
      if list.IndexOf('Zip') > -1 then
        RenameField(ContactsTableName, 'Zip', 'Zip1');
      if list.IndexOf('Country') > -1 then
        RenameField(ContactsTableName, 'Country', 'Country1');
      if list.IndexOf('EMail') > -1 then
        RenameField(ContactsTableName, 'EMail', 'EMail1');

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

      FConnection.Transaction.Commit;
    except
      FConnection.Transaction.Rollback;
      raise Exception.Create('Failure to update table structure to current VisualPlanIt version');
    end;
  finally
    list.Free;
  end;
end;

function TVpFirebirdDatastore.GetContactsTable: TDataset;
begin
  Result := FContactsTable;
end;

function TVpFirebirdDatastore.GetEventsTable: TDataset;
begin
  Result := FEventsTable;
end;

function TVpFirebirdDataStore.GetNextID(TableName: string): integer;
begin
  Unused(TableName);
  { This is not needed in the Firebird datastore as these tables use a
    generator and trigger for autoincrement fields.
    http://www.firebirdfaq.org/faq29/ }
  Result := -1;
end;

function TVpFirebirdDatastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpFirebirdDatastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

procedure TVpFirebirdDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect and (AutoCreate or FileExists(FConnection.DatabaseName));
end;

procedure TVpFirebirdDatastore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

{ Note: Set the property Required of the PrimaryKey field to false. Otherwise
  Firebird will complain about this field not being specified when posting. }
procedure TVpFirebirdDatastore.OpenTables;
begin
  FixContactsTable;
  FContactsTable.Open;
  FContactsTable.Fields[0].Required := false;

  FEventsTable.Open;
  FEventsTable.Fields[0].Required := false;

  FResourceTable.Open;
  FResourceTable.Fields[0].Required := false;

  FTasksTable.Open;
  FTasksTable.Fields[0].Required := false;
end;

procedure TVpFirebirdDatastore.PostContacts;
begin
  inherited;
  FContactsTable.ApplyUpdates;
  //FConnection.Transaction.CommitRetaining;
  FContactsTable.Refresh;
end;

procedure TVpFirebirdDatastore.PostEvents;
begin
  inherited;
  FEventsTable.ApplyUpdates;
  //FConnection.Transaction.CommitRetaining;
  FEventsTable.Refresh;
end;

procedure TVpFirebirdDatastore.PostResources;
begin
  inherited;
  FResourceTable.ApplyUpdates;
  //FConnection.Transaction.CommitRetaining;

  // Refresh needed in order to get the resource id for the other tables.
  // Without it the other datasets would not be stored after adding a resource.
  // Seems to be pecularity of Firebird.
  FResourceTable.Refresh;
end;

procedure TVpFirebirdDatastore.PostTasks;
begin
  inherited;
  FTasksTable.ApplyUpdates;
  //FConnection.Transaction.CommitRetaining;
  FTasksTable.Refresh;
end;

procedure TVpFirebirdDatastore.SetConnected(const AValue: Boolean);
begin
  if (AValue = Connected) or (FConnection = nil) or (FConnectLock > 0) then
    exit;

  inc(FConnectLock);
  if AValue and AutoCreate then
    CreateTables;
  FConnection.Connected := AValue;
  if FConnection.Connected then
    OpenTables;

  inherited SetConnected(AValue);

  if FConnection.Connected then
    Load;
  dec(FConnectLock);
end;
(*
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
end;      *)


procedure TVpFirebirdDatastore.SetConnection(const AValue: TIBConnection);
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
