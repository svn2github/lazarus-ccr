unit Unit1;

{$mode objfpc}{$H+}

{ Activate this define to use a JSON string instead of a file }

{.$DEFINE USE_JSON_STRING}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus,
  VpBaseDS, VpDayView, VpWeekView, VpTaskList, VpContactGrid, VpMonthView,
  VpResEditDlg, VpContactButtons, VpJSONDs;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpJSONDataStore1: TVpJSONDataStore;
    VpMonthView1: TVpMonthView;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VpJSONDataStore1Disconnect(Sender: TObject);
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
  LazFileUtils,
  VpData;


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

// Load the last resource.
procedure TForm1.FormCreate(Sender: TObject);
var
  lastRes: TVpResource;
  datastore: TVpCustomDatastore;
begin
  {$IFDEF USE_JSON_STRING}
  VpJSONDataStore1.FileName := '';
  VpJSONDataStore1.JSONString := '{"Resources":[{"ResourceID":1178568021,"Description":"TEST","Notes":"","ResourceActive":true,"UserField0":"","UserField1":"","UserField2":"","UserField3":"","UserField4":"","UserField5":"","UserField6":"","UserField7":"","UserField8":"","UserField9":"","Events":[{"RecordID":1273124118,"Description":"teset","Notes":"","Location":"test test","Category":0,"AllDayEvent":false,"StartTime":"2018-09-30 08:00:00","EndTime":"2018-09-30 08:30:00","DingPath":"","AlertDisplayed":false,"AlarmSet":false,"AlarmAdvance":15,"AlarmAdvanceType":0,"SnoozeTime":"00:00:00","RepeatCode":0,"RepeatRangeEnd":"","CustomInterval":0,"UserField0":"","UserField1":"","UserField2":"","UserField3":"","UserField4":"","UserField5":"","UserField6":"","UserField7":"","UserField8":"","UserField9":""}]}]}';
  VpJSONDataStore1.JSONStoreType := jstString;
  {$ENDIF}

  datastore := VpControlLink1.Datastore;
  datastore.Connected := true;
  if datastore.Resources.Count > 0 then
  begin
    lastRes := datastore.Resources.Items[datastore.Resources.Count-1];
    datastore.ResourceID := lastRes.ResourceID;
  end;
end;

procedure TForm1.VpJSONDataStore1Disconnect(Sender: TObject);
begin
  if VpJSONDatastore1.JSONStoreType = jstString then
    ShowMessage(VpJSONDatastore1.JSONString);
end;

end.

