unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, DbCtrls, DBGrids, VpBaseDS, VpDayView, VpWeekView,
  VpTaskList, VpContactGrid, VpMonthView, VpResEditDlg, VpContactButtons,
  VpSQLite3DS, sqlite3conn, sqldb, db, VpData;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    BtnApplyToPlanner: TButton;
    DBNavigator: TDBNavigator;
    DsAllContacts: TDataSource;
    DsAllEvents: TDataSource;
    DsAllResources: TDataSource;
    DsAllTasks: TDataSource;
    Grid: TDBGrid;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    QryAllContacts: TSQLQuery;
    QryAllEvents: TSQLQuery;
    QryAllResources: TSQLQuery;
    QryAllTasks: TSQLQuery;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    TabControl1: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpSqlite3Datastore1: TVpSqlite3Datastore;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    procedure BtnApplyToPlannerClick(Sender: TObject);
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure QryGridAfterEdit(DataSet: TDataSet);
    procedure QryGridAfterPost(DataSet: TDataSet);
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

const
  DBFILENAME = 'data.db';

{ TForm1 }

procedure TForm1.BtnApplyToPlannerClick(Sender: TObject);
var
  resID: Integer;
begin
  // Remember id of currently selected resource
  resID := VpSqlite3Datastore1.ResourceID;

  // Since the datastore does not know about the changes made in the grid
  // we have to force the datastore to re-read everything.
  // Maybe there's better way...
  VpSqlite3Datastore1.Connected := false;
  VpSqlite3Datastore1.Connected := true;

  // Return to previous resource
  VpSqlite3Datastore1.Resources.ClearResources;
  VpSqlite3Datastore1.Load;
  VpSqlite3Datastore1.ResourceID := resID;

  // Don't forget to re-activate the grid's datasources
  QryAllResources.Open;
  QryAllEvents.Open;
  QryAllContacts.Open;
  QryAllTasks.Open;
end;

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

// Setting up the database connection and the datastore. Preselect a resource
// in the resource combo.
procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;

  try
    SQLite3Connection1.DatabaseName := AppendPathDelim(Application.Location) + DBFILENAME;
    SQLTransaction1.Action := caCommit;

    VpSqlite3Datastore1.Connection := SQLite3Connection1;
    VpSqlite3Datastore1.AutoCreate := true;
    VpSqlite3Datastore1.Connected := true;

    if VpSqlite3Datastore1.Resources.Count > 0 then
      VpSqlite3Datastore1.ResourceID := VpSqlite3Datastore1.Resources.Items[0].ResourceID;

  except
    on E:Exception do
    begin
      MessageDlg('sqlite3.dll not found. Copy it to the exe folder and restart the program.',
        mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.PageIndex = 2 then TabControl1Change(nil);
end;

procedure TForm1.QryGridAfterEdit(DataSet: TDataSet);
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

