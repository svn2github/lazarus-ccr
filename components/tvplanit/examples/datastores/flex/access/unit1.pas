unit Unit1;

{$mode objfpc}{$H+}

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
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LazFileUtils;


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

// Setting up the database connection and the datastore. Preselect a resource
// in the resource combo.
procedure TForm1.FormCreate(Sender: TObject);
begin
  if not FileExists('data.mdb') then begin
    MessageDlg('Database file "data.mdb" does not exist. ' + LineEnding +
      'Please run "CreateAccessDB" to create an empty Access database file.',
      mtError, [mbOK], 0);
    Close;exit;
  end;

  try
    // Connection
    ODBCConnection1.Driver := 'Microsoft Access Driver (*.mdb)';
    ODBCConnection1.Params.Clear;
    ODBCConnection1.Params.Add('DBQ=.\data.mdb');
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

end.

