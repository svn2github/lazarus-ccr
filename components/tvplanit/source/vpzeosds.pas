unit VpZeosDs;

interface

uses
  Classes, DB,
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
    function GetClientCodepage: String;
    function GetControlsCodePage: TZControlsCodePage;
    function GetDatabase: String;
    function GetHostname: String;
    function GetLibLocation: String;
    function GetLoginPrompt: Boolean;
    function GetPassword: String;
    function GetPort: Integer;
    function GetProtocol: String;
    function GetUser: String;
    procedure SetClientCodepage(const AValue: String);
    procedure SetControlsCodePage(const AValue: TZControlsCodePage);
    procedure SetDatabase(const AValue: String);
    procedure SetHostName(const AValue: String);
    procedure SetLibLocation(const AValue: String);
    procedure SetLoginPrompt(const AValue: Boolean);
    procedure SetPassword(const AValue: String);
    procedure SetPort(const AValue: Integer);
    procedure SetProtocol(const AValue: String);
    procedure SetUser(const AValue: String);

  protected
    procedure CreateTable(const ATableName: String);
    function GetContactsTable: TDataset; override;
    function GetEventsTable: TDataset; override;
    function GetNextID(TableName: string): int64; override;
    function GetResourceTable: TDataset; override;
    function GetTasksTable: TDataset; override;
    procedure Loaded; override;
    procedure SetConnected(const AValue: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateTables;

    property Connection: TZConnection read FConnection;
    property ResourceTable;
    property EventsTable;
    property ContactsTable;
    property TasksTable;

  published
    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Protocol: string read GetProtocol write SetProtocol;
    property LibraryLocation: string read GetLibLocation write SetLibLocation;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property ControlsCodePage: TZControlsCodepage read GetControlsCodepage write SetControlsCodepage;
    property ClientCodePage: String read GetClientCodePage write SetClientCodepage;

    // inherited
    property AutoConnect;
    property AutoCreate default true;
    property Connected;
  end;


implementation

uses
  LazFileUtils,
  VpConst;

{ TVpZeosDatastore }

constructor TVpZeosDatastore.Create(AOwner: TComponent);
begin
  inherited;

  FConnection := TZConnection.Create(self);

  FContactsTable := TZTable.Create(self);
  FContactsTable.Connection := FConnection;
  FContactsTable.TableName := 'Contacts';

  FEventsTable := TZTable.Create(Self);
  FEventsTable.Connection := FConnection;
  FEventsTable.TableName := 'Events';

  FResourceTable := TZTable.Create(self);
  FResourceTable.Connection := FConnection;
  FResourceTable.TableName := 'Resources';

  FTasksTable := TZTable.Create(self);
  FTasksTable.Connection := FConnection;
  FTasksTable.TableName := 'Tasks';
end;

procedure TVpZeosDatastore.CreateTables;
begin
  if FileExistsUTF8(Database) then
    exit;

  CreateTable(ContactsTableName);
  CreateTable(EventsTableName);
  CreateTable(ResourceTableName);
  CreateTable(TasksTableName);
end;

procedure TVpZeosDatastore.CreateTable(const ATableName: String);
begin
  FConnection.Connected := true;
  if ATableName = ContactsTableName then
    FConnection.ExecuteDirect(
      'CREATE TABLE Contacts ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'ResourceID INTEGER,' +
        'FirstName VARCHAR(50) ,'+
        'LastName VARCHAR(50) , '+
        'Birthdate DATE, '+
        'Anniversary DATE, '+
        'Title VARCHAR(50) ,'+
        'Company VARCHAR(50) ,'+
        'Job_Position VARCHAR(30), '+
        'Address VARCHAR(100), '+
        'City VARCHAR(50), '+
        'State VARCHAR(25), '+
        'Zip VARCHAR(10), '+
        'Country VARCHAR(25), '+
        'Notes VARCHAR(1024), '+
        'Phone1 VARCHAR(25), '+
        'Phone2 VARCHAR(25), '+
        'Phone3 VARCHAR(25), '+
        'Phone4 VARCHAR(25), '+
        'Phone5 VARCHAR(25), '+
        'PhoneType1 INEGER, '+
        'PhoneType2 INTEGER, '+
        'PhoneType3 INTEGER, '+
        'PhoneType4 INTEGER, '+
        'PhoneType5 INTEGER, '+
        'Category INTEGER, '+
        'EMail VARCHAR(100), '+
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
        'UserField9 VARCHAR(100) )')
  else
  if ATableName = EventsTableName then
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
        'UserField9 VARCHAR(100) )')
  else
  if ATableName = ResourceTableName then
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
         'UserField9 VARCHAR(100) )')
  else
  if ATableName = TasksTableName then
    FConnection.ExecuteDirect(
      'CREATE TABLE Tasks ('+
        'RecordID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+
        'ResourceID INTEGER, '+
        'Complete TIMESTAMP, '+
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
        'UserField9 VARCHAR(100) )');
end;

function TVpZeosDatastore.GetContactsTable: TDataset;
begin
  Result := FContactsTable;
end;

function TVpZeosDatastore.GetClientCodePage: string;
begin
  Result := FConnection.ClientCodePage;
end;

function TVpZeosDatastore.GetControlsCodePage: TZControlsCodePage;
begin
  Result := FConnection.ControlsCodePage;
end;

function TVpZeosDatastore.GetDatabase: String;
begin
  Result := FConnection.Database;
end;

function TVpZeosDatastore.GetEventsTable: TDataset;
begin
  Result := FEventsTable;
end;

function TVpZeosDatastore.GetHostname: String;
begin
  Result := FConnection.Hostname;
end;

function TVpZeosDatastore.GetLibLocation: String;
begin
  Result := FConnection.LibLocation;
end;

function TVpZeosDatastore.GetLoginPrompt: Boolean;
begin
  Result := FConnection.LoginPrompt;
end;

function TVpZeosDataStore.GetNextID(TableName: string): int64;
begin
  { This is not needed in the ZEOS datastore as these tables use
    autoincrement fields. }
  result := -1;
end;

function TVpZeosDatastore.GetPassword: String;
begin
  Result := FConnection.Password;
end;

function TVpZeosDatastore.GetPort: Integer;
begin
  Result := FConnection.Port;
end;

function TVpZeosDatastore.GetProtocol: String;
begin
  Result := FConnection.Protocol;
end;

function TVpZeosDatastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpZeosDatastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

function TVpZeosDatastore.GetUser: String;
begin
  Result := FConnection.User;
end;

procedure TVpZeosDatastore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;

procedure TVpZeosDatastore.SetConnected(const AValue: Boolean);
begin
  if AValue = Connected then
    exit;

  if AValue and AutoCreate then
    CreateTables;

  FConnection.Connected := AValue;
  if AValue then begin
    FContactsTable.Open;
    FEventsTable.Open;
    FResourceTable.Open;
    FTasksTable.Open;
  end;

  inherited SetConnected(AValue);
end;

procedure TVpZeosDatastore.SetClientCodePage(const AValue: string);
begin
  if AValue = ClientCodePage then exit;
  FConnection.Connected := false;
  FConnection.ClientCodePage := AValue;
end;

procedure TVpZeosDatastore.SetControlsCodePage(const AValue: TZControlsCodePage);
begin
  if AValue = ControlsCodePage then exit;
  FConnection.Connected := false;
  FConnection.ControlsCodePage := AValue;
end;

procedure TVpZeosDatastore.SetDatabase(const AValue: String);
begin
  if AValue = GetDatabase then exit;
  FConnection.Connected := false;
  FConnection.Database := AValue;
end;

procedure TVpZeosDatastore.SetHostName(const AValue: String);
begin
  if AValue = HostName then exit;
  FConnection.Connected := false;
  FConnection.HostName := AValue;
end;

procedure TVpZeosDatastore.SetLibLocation(const AValue: String);
begin
  if AValue = LibraryLocation then exit;
  FConnection.Connected := false;
  FConnection.LibraryLocation := AValue;
end;

procedure TVpZeosDatastore.SetLoginPrompt(const AValue: Boolean);
begin
  if AValue = LoginPrompt then exit;
  FConnection.Connected := false;
  FConnection.LoginPrompt := AValue;
end;

procedure TVpZeosDatastore.SetPassword(const AValue: String);
begin
  if AValue = Password then exit;
  FConnection.Connected := false;
  FConnection.Password := AValue;
end;

procedure TVpZeosDatastore.SetPort(const AValue: Integer);
begin
  if AValue = Port then exit;
  FConnection.Connected := false;
  FConnection.Port := AValue;
end;

procedure TVpZeosDatastore.SetProtocol(const AValue: String);
begin
  if AValue = Protocol then exit;
  FConnection.Connected := false;
  FConnection.Protocol := AValue;
end;

procedure TVpZeosDatastore.SetUser(const AValue: String);
begin
  if AValue = User then exit;
  FConnection.Connected := false;
  FConnection.User := AValue;
end;

end.
