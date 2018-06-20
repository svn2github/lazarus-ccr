unit Unit1;

{$mode objfpc}{$H+}

// Select one of these
{$DEFINE MDB}
{.$DEFINE ACCDB}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, DBGrids, DbCtrls, VpBaseDS, VpDayView, VpWeekView,
  VpTaskList, VpContactGrid, VpMonthView, VpResEditDlg, VpContactButtons,
  db, sqldb, odbcconn, VpData, VpFlxDS;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    BtnApplyToPlanner: TButton;
    DsAllResources: TDataSource;
    DsAllContacts: TDataSource;
    DsAllEvents: TDataSource;
    DsAllTasks: TDataSource;
    Grid: TDBGrid;
    DBNavigator: TDBNavigator;
    DsTasks: TDataSource;
    DsEvents: TDataSource;
    DsContacts: TDataSource;
    DsResources: TDataSource;
    ODBCConnection1: TODBCConnection;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    QryResources: TSQLQuery;
    QryContacts: TSQLQuery;
    QryEvents: TSQLQuery;
    QryTasks: TSQLQuery;
    QryAllResources: TSQLQuery;
    QryAllContacts: TSQLQuery;
    QryAllEvents: TSQLQuery;
    QryAllTasks: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    TabControl1: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpFlexDataStore1: TVpFlexDataStore;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
    procedure BtnApplyToPlannerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure QryGridAfterPost(DataSet: TDataSet);
    procedure QryGridAfterInsert(DataSet: TDataSet);
    procedure QryGridAfterEdit(DataSet: TDataSet);
    procedure TabControl1Change(Sender: TObject);
    procedure VpFlexDataStore1CreateTable(Sender: TObject; TableName: String);
  private
    { private declarations }
    procedure CreateContactsTable;
    procedure CreateEventsTable;
    procedure CreateResourceTable;
    procedure CreateTasksTable;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LazFileUtils,
  VpConst;


const
  {$IFDEF MDB}
  DB_NAME = '.\data.mdb';            // Access 97 file format
  {$ENDIF}
  {$IFDEF ACCDB}
  DB_NAME = '.\data.accdb';         // Access 2007+ file format
  {$ENDIF}

{ TForm1 }

// Adds a new resource
procedure TForm1.BtnNewResClick(Sender: TObject);
begin
  VpResourceEditDialog1.AddNewResource;
end;

// Edits the currently selected resource
procedure TForm1.BtnEditResClick(Sender: TObject);
begin
  // Open the resource editor dialog, everything is done here.
  VpResourceEditDialog1.Execute;
end;

procedure TForm1.BtnApplyToPlannerClick(Sender: TObject);
var
  resID: Integer;
begin
  // Remember id of currently selected resource
  resID := VpFlexDatastore1.ResourceID;

  // Since the datastore does not know about the changes made in the grid
  // we have to force the datastore to re-read everything.
  // Maybe there's better way...
  VpFlexDatastore1.Connected := false;
  VpFlexDatastore1.Connected := true;

  // Return to previous resource
  VpFlexDatastore1.Resources.ClearResources;
  VpFlexDatastore1.Load;
  VpFlexDatastore1.ResourceID := resID;

  // Don't forget to re-activate the grid's datasources
  QryAllResources.Open;
  QryAllEvents.Open;
  QryAllContacts.Open;
  QryAllTasks.Open;
end;

procedure TForm1.CreateContactsTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Contacts ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER,' +
      'FirstName VARCHAR(50) ,'+
      'LastName VARCHAR(50) , '+
      'Birthdate DATE, '+
      'Anniversary DATE, '+
      'Title VARCHAR(50), '+
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
      'Notes VARCHAR, '+
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
      'Category INTEGER, '+
      'Custom1 VARCHAR(100), '+
      'Custom2 VARCHAR(100),'+
      'Custom3 VARCHAR(100), '+
      'Custom4 VARCHAR(100) )'
    );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piCRecordID ON Contacts(RecordID) WITH PRIMARY');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCResourceID ON Contacts(ResourceID)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCName ON Contacts(LastName, FirstName)' );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCCompany ON Contacts(Company)');
end;

procedure TForm1.CreateEventsTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Events ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER, '+
      'StartTime DATETIME, '+
      'EndTime DATETIME, '+
      'Description VARCHAR(255), '+
      'Location VARCHAR(255), '+
      'Notes VARCHAR, ' +
      'Category INTEGER, '+
      'AllDayEvent LOGICAL, '+
      'DingPath VARCHAR(255), '+
      'AlarmSet LOGICAL, '+
      'AlarmAdvance INTEGER, '+
      'AlarmAdvanceType INTEGER, '+
      'SnoozeTime DATETIME, '+
      'RepeatCode INTEGER, '+
      'RepeatRangeEnd DATETIME, '+
      'CustomInterval INTEGER)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piERecordID ON Events(RecordID) WITH PRIMARY');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EResourceID ON Events(ResourceID)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EStartTime ON Events(StartTime)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EEndTime ON Events(EndTime)');
end;

procedure TForm1.CreateResourceTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Resources ( '+
       'ResourceID COUNTER, ' +
       'Description VARCHAR(255), '+
       'Notes VARCHAR, '+                           // 1024 --> -
       'ImageIndex INTEGER, '+
       'ResourceActive LOGICAL, '+                  // BOOL --> LOGICAL
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
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piRResourceID ON Resources(ResourceID) WITH PRIMARY'
  );
end;

procedure TForm1.CreateTasksTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Tasks ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER, '+
      'Complete LOGICAL, '+
      'Description VARCHAR(255), '+
      'Details VARCHAR, '+
      'CreatedOn DATETIME, '+
      'Priority INTEGER, '+
      'Category INTEGER, '+
      'CompletedOn DATETIME, '+
      'DueDate DATETIME)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piTRecordID ON Tasks(RecordID) WITH PRIMARY'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siTDueDate ON Tasks(DueDate)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siTCompletedOn ON Tasks(CompletedOn)'
  );
end;

// Setting up the database connection and the datastore. Preselect a resource
// in the resource combo.
procedure TForm1.FormCreate(Sender: TObject);
begin
  if not FileExists(DB_NAME) then begin
    MessageDlg('Database file "' + DB_NAME + '" does not exist. ' + LineEnding +
      'Please run "CreateAccessDB" to create an empty Access database file.' + LineEnding +
      'Or copy an empty database file, data.mdb or data.accdb, from the '+
      'folder "empty_db" to the current directory.',
      mtError, [mbOK], 0);
    Close;exit;
  end;

  try
    // Connection
    {$IFDEF MDB}
    ODBCConnection1.Driver := 'Microsoft Access Driver (*.mdb)';
    {$ENDIF}
    {$IFDEF ACCDB}
    ODBCConnection1.Driver := 'Microsoft Access Driver (*.mdb, *.accdb)';
    {$ENDIF}
    ODBCConnection1.Params.Clear;
    ODBCConnection1.Params.Add('DBQ=' + DB_NAME);
    ODBCConnection1.Connected := true;
    ODBCConnection1.KeepConnection := true;

    // Transaction
    SQLTransaction1.DataBase := ODBCConnection1;
//    SQLTransaction1.Action := caCommit;
    SQLTransaction1.Active := True;

    // Connect the datastore. This opens the datasets and loads them into the store.
    VpFlexDatastore1.Connected := true;

    // Pre-select the first resource item
    if VpFlexDatastore1.Resources.Count > 0 then
      VpFlexDatastore1.Resource := VpFlexDatastore1.Resources.Items[0];

    // Open the additional datasets displayed in the grid
    QryAllResources.Open;
    QryAllContacts.Open;
    QryAllEvents.Open;
    QryAllTasks.Open;

    PageControl1.ActivePageIndex := 0;

  except
    on E:Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ODBCConnection1.Connected := false;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.PageIndex = 2 then TabControl1Change(nil);
end;

procedure TForm1.QryGridAfterEdit(DataSet: TDataSet);
begin
  BtnApplyToPlanner.Enabled := false;
end;

procedure TForm1.QryGridAfterInsert(DataSet: TDataSet);
begin
  BtnApplyToPlanner.Enabled := false;
end;

procedure TForm1.QryGridAfterPost(DataSet: TDataSet);
begin
  // Note: UpdateMode must be upWhereAll! Otherwise there's an error "No update
  // query specified and failed to generate one. (No fields for inclusion in
  // where statement found)".
  // http://wiki.freepascal.org/SqlDBHowto#How_does_SqlDB_send_the_changes_to_the_database_server.3F

  TSQLQuery(Dataset).ApplyUpdates;
  SQLTransaction1.CommitRetaining;

  BtnApplyToPlanner.Enabled := true;
end;

procedure TForm1.TabControl1Change(Sender: TObject);
var
  i: Integer;
begin
  DsAllResources.Dataset.Close;
  DsAllContacts.Dataset.Close;
  DsAllEvents.Dataset.Close;
  DsAllTasks.Dataset.Close;

  case TabControl1.TabIndex of
    0: Grid.Datasource := DsAllResources;
    1: Grid.Datasource := DsAllContacts;
    2: Grid.Datasource := DsAllEvents;
    3: Grid.Datasource := DsAllTasks;
  end;
  DBNavigator.Datasource := Grid.Datasource;
  Grid.Datasource.Dataset.Open;
  for i:=0 to Grid.Columns.Count-1 do
    Grid.Columns[i].Width := 100;;
end;

procedure TForm1.VpFlexDataStore1CreateTable(Sender: TObject; TableName: String
  );
begin
  if TableName = ResourceTableName then CreateResourceTable;
  if TableName = ContactsTableName then CreateContactsTable;
  if TableName = EventsTableName then CreateEventsTable;
  if TableName = TasksTableName then CreateTasksTable;
end;

end.

