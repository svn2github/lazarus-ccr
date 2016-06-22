{ Visual PlanIt datastore for a TBufDataset }

{$I vp.inc}

unit VpBufDS;

interface

uses
  SysUtils, Classes, db, BufDataset,
  VpDBDS;

type
  TVpBufDSDataStore = class(TVpCustomDBDataStore)
  private
    FResourceTable: TBufDataset;
    FEventsTable: TBufDataset;
    FContactsTable: TBufDataset;
    FTasksTable: TBufDataset;
    FDirectory: String;
    procedure SetDirectory(AValue: String);

  protected
    { ancestor property getters }
    function GetContactsTable: TDataset; override;
    function GetEventsTable: TDataset; override;
    function GetResourceTable: TDataset; override;
    function GetTasksTable: TDataset; override;

    { ancestor methods }
    function GetNextID(TableName: string): int64; override;
    procedure Loaded; override;
    procedure SetConnected(const Value: boolean); override;

    { other methods }
    procedure CloseTables;
    procedure CreateTable(ATableName: String);
    procedure OpenTables;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateTables;

    property ResourceTable;
    property EventsTable;
    property ContactsTable;
    property TasksTable;

  published
    property Directory: String read FDirectory write SetDirectory;
    property AutoConnect;
    property AutoCreate;
  end;


implementation

uses
  LazFileUtils,
  VpConst, VpBaseDS;

const
  TABLE_EXT = '.db';

constructor TVpBufDSDatastore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResourceTable := TBufDataset.Create(nil);
  FEventsTable := TBufDataset.Create(nil);
  FContactsTable := TBufDataset.Create(nil);
  FTasksTable := TBufDataset.Create(nil);
end;

destructor TVpBufDSDatastore.Destroy;
begin
  FreeAndNil(FResourceTable);
  FreeAndNil(FEventsTable);
  FreeAndNil(FContactsTable);
  FreeAndNil(FTasksTable);
  inherited;
end;

procedure TVpBufDSDatastore.CloseTables;
begin
  FResourceTable.Close;
  FEventsTable.Close;
  FContactsTable.Close;
  FTasksTable.Close;
end;

procedure TVpBufDSDatastore.CreateTable(ATableName: String);
var
  dir: String;
  table: TBufDataset;
begin
  if FDirectory = '' then
    dir := ExtractFilePath(ParamStr(0)) else
    dir := IncludeTrailingPathDelimiter(FDirectory);
  dir := ExpandFileName(dir);
  if not DirectoryExistsUTF8(dir) then
  begin
    if AutoCreate then
      CreateDir(dir)
    else
      raise Exception.CreateFmt('Directory "%s" for tables does not exist.', [dir]);
  end;

  if ATableName = ResourceTableName then
    table := FResourceTable
  else if ATableName = EventsTableName then
    table := FEventsTable
  else if ATableName = ContactsTablename then
    table := FContactsTable
  else if ATableName = TasksTableName then
    table := FTasksTable
  else
    raise Exception.CreateFmt('TableName "%s" cannot be processed.', [ATableName]);

  table.Close;
  table.FileName := dir + ATableName + TABLE_EXT;
  if not FileExists(table.FileName) then
  begin
     CreateFieldDefs(ATableName, table.FieldDefs);
     table.FieldDefs[0].DataType := ftAutoInc;
     table.CreateDataset;
  end;
  table.IndexDefs.Clear;
  table.IndexDefs.Update;
  CreateIndexDefs(ATableName, table.IndexDefs);
end;

procedure TVpBufDSDatastore.CreateTables;
begin
  CreateTable(ResourceTablename);
  CreateTable(EventsTableName);
  CreateTable(ContactsTableName);
  CreateTable(TasksTableName);
end;

function TVpBufDSDatastore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;

function TVpBufDSDatastore.GetEventsTable : TDataset;
begin
  Result := FEventsTable;
end;

function TVpBufDSDatastore.GetContactsTable : TDataset;
begin
  Result := FContactsTable;
end;

function TVpBufDSDataStore.GetNextID(TableName: string): int64;
begin
  { This is not needed in the BufDataset datastore as these tables use
    autoincrement fields. }
  result := -1;
end;

function TVpBufDSDatastore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;

procedure TVpBufDSDatastore.Loaded;
begin
  inherited;
  Connected := AutoConnect;
end;

procedure TVpBufDSDatastore.OpenTables;
begin
  FResourceTable.Open;
  FEventsTable.Open;
  FContactsTable.Open;
  FTasksTable.Open;
end;

procedure TVpBufDSDatastore.SetConnected(const Value: boolean);
var
  dir: String;
begin
  { Don't do anything with live data until run time. }
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;

  { Connecting or disconnecting? }
  if Value then begin
    if AutoCreate then CreateTables;
    OpenTables;
    Load;
  end;

  inherited SetConnected(Value);
end;

procedure TVpBufDSDatastore.SetDirectory(AValue: String);
var
  wasConn: Boolean;
begin
  if AValue = FDirectory then
    exit;
  if Connected then
    raise Exception.Create('Set directory before connecting.');
  FDirectory := AValue;
end;


end.