unit gpslistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ExtCtrls, Buttons, mvGpsObj, mvMapViewer;

const
  // IDs of GPS items
  _CLICKED_POINTS_ = 10;

type

  { TGPSListViewer }

  TGPSListViewer = class(TForm)
    BtnClearAll: TBitBtn;
    BtnDeletePoint: TBitBtn;
    BtnGoToPoint: TBitBtn;
    BtnClose: TBitBtn;
    ListView: TListView;
    Panel1: TPanel;
    procedure BtnClearAllClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnDeletePointClick(Sender: TObject);
    procedure BtnGoToPointClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FViewer: TMapView;
    FList: TGpsObjList;
    procedure SetViewer(AValue: TMapView);
  protected
    procedure Populate;
    procedure UpdateButtonStates;

  public
    destructor Destroy; override;
    property MapViewer: TMapView read FViewer write SetViewer;

  end;

var
  GPSListViewer: TGPSListViewer;

implementation

{$R *.lfm}

uses
  mvTypes;

destructor TGPSListViewer.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TGPSListViewer.Populate;
const
  GPS_FORMAT = '0.000000';
var
  i: Integer;
  item: TListItem;
  gpsObj: TGpsObj;
  area: TRealArea;
begin
  if FViewer = nil then begin
    ListView.Items.Clear;
    exit;
  end;

  FViewer.GPSItems.GetArea(area);
  FList.Free;
  FList := FViewer.GPSItems.GetObjectsInArea(area);
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    for i:=0 to FList.Count-1 do begin
      gpsObj := FList[i];
      item := ListView.Items.Add;
      item.Caption := gpsObj.Name;
      if gpsObj is TGpsPoint then begin
        item.Subitems.Add(FormatFloat(GPS_FORMAT, TGpsPoint(gpsObj).Lat));
        item.Subitems.Add(FormatFloat(GPS_FORMAT, TGpsPoint(gpsObj).Lon));
      end;
    end;
  finally
    ListView.items.EndUpdate;
    UpdateButtonStates;
  end;
end;

procedure TGPSListViewer.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TGPSListViewer.BtnClearAllClick(Sender: TObject);
begin
  FViewer.GpsItems.Clear(_CLICKED_POINTS_);
  ListView.Items.Clear;
end;

procedure TGPSListViewer.BtnDeletePointClick(Sender: TObject);
var
  gpsObj: TGpsObj;
  i: Integer;
  rPt: TRealPoint;
  item: TListItem;
begin
  if ListView.itemIndex > -1 then begin
    ListView.Items.Delete(ListView.ItemIndex);
    // Clear all GPS items in MapViewer
    FViewer.GpsItems.Clear(_CLICKED_POINTS_);
    // Recreate remaining GPS items from data in ListView
    for i:=0 to ListView.Items.Count-1 do begin
      item := ListView.Items[i];
      rPt.Lon := StrToFloat(item.SubItems[1]);
      rPt.Lat := StrToFloat(item.SubItems[0]);
      gpsObj := TGpsPoint.CreateFrom(rPt);
      gpsObj.Name := item.Caption;
      FViewer.GPSItems.Add(gpsObj, _CLICKED_POINTS_);
    end;
    UpdateButtonStates;
  end;
end;

procedure TGPSListViewer.BtnGoToPointClick(Sender: TObject);
var
  gpsPt: TGpsPoint;
  gpsObj: TGpsObj;
begin
  if ListView.ItemIndex > -1 then begin
    gpsObj := FList[ListView.ItemIndex];
    if gpsObj is TGpsPoint then begin
      gpsPt := TGpsPoint(gpsObj);
      if Assigned(FViewer) then FViewer.Center := gpsPt.RealPoint;
    end;
  end;
end;

procedure TGPSListViewer.ListViewDblClick(Sender: TObject);
begin
  BtnGotoPointClick(nil);
end;

procedure TGPSListViewer.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateButtonStates;
end;

procedure TGPSListViewer.SetViewer(AValue: TMapView);
begin
  if FViewer = AValue then
    exit;
  FViewer := AValue;
  Populate;
end;

procedure TGPSListViewer.UpdateButtonStates;
begin
  BtnGotoPoint.Enabled := ListView.ItemIndex > -1;
  BtnDeletePoint.Enabled := ListView.ItemIndex > -1;
end;

end.

