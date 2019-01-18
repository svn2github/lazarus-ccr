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
    BtnDeletePoint: TBitBtn;
    BtnGoToPoint: TBitBtn;
    BtnClose: TBitBtn;
    ListView: TListView;
    Panel1: TPanel;
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnDeletePointClick(Sender: TObject);
    procedure BtnGoToPointClick(Sender: TObject);
  private
    FViewer: TMapView;
    FList: TGpsObjList;
    procedure SetViewer(AValue: TMapView);
  protected
    procedure Populate;

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
//      item.Caption := IntToStr(gpsObj.ID);
      if gpsObj is TGpsPoint then begin
        item.SubItems.Add(gpsObj.Name);
        item.Subitems.Add(FormatFloat(GPS_FORMAT, TGpsPoint(gpsObj).Lat));
        item.Subitems.Add(FormatFloat(GPS_FORMAT, TGpsPoint(gpsObj).Lon));
      end;
    end;
  finally
    ListView.items.EndUpdate;
  end;
end;

procedure TGPSListViewer.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TGPSListViewer.BtnDeletePointClick(Sender: TObject);
var
  gpsObj: TGpsObj;
  i: Integer;
  rPt: TRealPoint;
  item: TListItem;
begin
  if ListView.Selected <> nil then begin
    gpsObj := FList[ListView.Selected.Index];
    ListView.Selected.Free;
    FViewer.GpsItems.Clear(_CLICKED_POINTS_);
    for i:=0 to ListView.Items.Count-1 do begin
      item := ListView.Items[i];
      rPt.Lon := StrToFloat(item.SubItems[2]);
      rPt.Lat := StrToFloat(item.SubItems[1]);
      gpsObj := TGpsPoint.CreateFrom(rPt);
      gpsObj.Name := item.SubItems[0];
      FViewer.GPSItems.Add(gpsObj, _CLICKED_POINTS_);
    end;
  end;
end;

procedure TGPSListViewer.BtnGoToPointClick(Sender: TObject);
var
  gpsPt: TGpsPoint;
  gpsObj: TGpsObj;
begin
  if ListView.Selected <> nil then begin
    gpsObj := FList[ListView.Selected.Index];
    if gpsObj is TGpsPoint then begin
      gpsPt := TGpsPoint(gpsObj);
      if Assigned(FViewer) then FViewer.Center := gpsPt.RealPoint;
    end;
  end;
end;

procedure TGPSListViewer.SetViewer(AValue: TMapView);
begin
  if FViewer = AValue then
    exit;
  FViewer := AValue;
  Populate;
end;

end.

