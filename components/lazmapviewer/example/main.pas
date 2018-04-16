unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, mvgeonames, mvMapViewer;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnSearch: TButton;
    CbDoubleBuffer: TCheckBox;
    CbLocations: TComboBox;
    CbProviders: TComboBox;
    CbUseThreads: TCheckBox;
    CbMouseCoords: TGroupBox;
    GbCenterCoords: TGroupBox;
    InfoCenterLatitude: TLabel;
    InfoCenterLongitude: TLabel;
    LblCenterLatitude: TLabel;
    LblPositionLongitude: TLabel;
    LblPositionLatitude: TLabel;
    InfoPositionLongitude: TLabel;
    InfoPositionLatitude: TLabel;
    LblCenterLongitude: TLabel;
    LblProviders: TLabel;
    LblZoom: TLabel;
    MapView: TMapView;
    GeoNames: TMVGeoNames;
    Panel1: TPanel;
    ZoomTrackBar: TTrackBar;
    procedure BtnSearchClick(Sender: TObject);
    procedure CbDoubleBufferChange(Sender: TObject);
    procedure CbProvidersChange(Sender: TObject);
    procedure CbUseThreadsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);

  private
    procedure UpdateLocationHistory(ALocation: String);

  public
    procedure ReadFromIni;
    procedure WriteToIni;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IniFiles, mvTypes;

const
  MAX_LOCATIONS_HISTORY = 50;
  HOMEDIR = '';

var
  PointFormatSettings: TFormatsettings;

function CalcIniName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;


{ TMainForm }

procedure TMainForm.BtnSearchClick(Sender: TObject);
begin
  MapView.Center := GeoNames.Search(CbLocations.Text, MapView.DownloadEngine);
  {
  ClearFoundLocations;
  GeoNames.LocationName := CbLocations.Text;
  GeoNames.ListLocations(MapView.DownloadEngine);
  //CbFoundLocations.Text := CbFoundLocations.Items[0];
  UpdateDropdownWidth(CbFoundLocations);
  }
  UpdateLocationHistory(CbLocations.Text);
end;

procedure TMainForm.CbDoubleBufferChange(Sender: TObject);
begin
  MapView.DoubleBuffered := CbDoubleBuffer.Checked;
end;

procedure TMainForm.CbProvidersChange(Sender: TObject);
begin
  MapView.MapProvider := CbProviders.Text;
end;

procedure TMainForm.CbUseThreadsChange(Sender: TObject);
begin
  MapView.UseThreads := CbUseThreads.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ForceDirectories(HOMEDIR + 'cache/');
  MapView.CachePath := HOMEDIR + 'cache/';
  MapView.GetMapProviders(CbProviders.Items);
  CbProviders.ItemIndex := CbProviders.Items.Indexof(MapView.MapProvider);
  MapView.DoubleBuffered := true;
  MapView.Zoom := 1;
  CbUseThreads.Checked := MapView.UseThreads;
  CbDoubleBuffer.Checked := MapView.DoubleBuffered;
//  GeoNames.LocationName := 'New York';
//  MapView.Center := GeoNames.Search('New York', MapView.DownloadEngine);

  ReadFromIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteToIni;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  MapView.Active := true;
end;

procedure TMainForm.MapViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  rPt: TRealPoint;
begin
  rPt := MapView.ScreenToLonLat(Point(X, Y));
      (*
    p := MapView.GetMouseMapPixel(X, Y);
    LblZoom.Caption := Format('Pixel: %d:%d', [p.X, p.Y]);
    p := mv.GetMouseMapTile(X, Y);
    Label3.Caption := Format('Tile: %d:%d', [p.X, p.Y]);
    r := mv.GetMouseMapLongLat(X, Y);
    *)
  InfoPositionLongitude.Caption := Format('%.6f°', [rPt.Lon]);
  InfoPositionLatitude.Caption := Format('%.6f°', [rPt.Lat]);

  rPt := MapView.Center;
  InfoCenterLongitude.Caption := Format('%.6f°', [rPt.Lon]);
  InfoCenterLatitude.Caption := Format('%.6f°', [rPt.Lat]);
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  ZoomTrackbar.Position := MapView.Zoom;
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  List: TStringList;
  L, T, W, H: Integer;
  R: TRect;
  i: Integer;
  s: String;
  pt: TRealPoint;
begin
  ini := TMemIniFile.Create(CalcIniName);
  try
    R := Screen.DesktopRect;
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    if L + W > R.Right then L := R.Right - W;
    if L < R.Left then L := R.Left;
    if T + H > R.Bottom then T := R.Bottom - H;
    if T < R.Top then T := R.Top;
    SetBounds(L, T, W, H);

    MapView.MapProvider := ini.ReadString('MapView', 'Provider', MapView.MapProvider);
    CbProviders.Text := MapView.MapProvider;
    MapView.Zoom := ini.ReadInteger('MapView', 'Zoom', MapView.Zoom);
    pt.Lon := StrToFloatDef(ini.ReadString('MapView', 'Center.Longitude', ''), 0.0, PointFormatSettings);
    pt.Lat := StrToFloatDef(ini.ReadString('MapView', 'Center.Latitude', ''), 0.0, PointFormatSettings);
    MapView.Center := pt;

    List := TStringList.Create;
    try
      ini.ReadSection('Locations', List);
      for i:=0 to List.Count-1 do begin
        s := ini.ReadString('Locations', List[i], '');
        if s <> '' then
          CbLocations.Items.Add(s);
      end;
    finally
      List.Free;
    end;

  finally
    ini.Free;
  end;
end;

procedure TMainForm.UpdateLocationHistory(ALocation: String);
var
  idx: Integer;
begin
  idx := CbLocations.Items.IndexOf(ALocation);
  if idx <> -1 then
    CbLocations.Items.Delete(idx);
  CbLocations.Items.Insert(0, ALocation);
  while CbLocations.Items.Count > MAX_LOCATIONS_HISTORY do
    CbLocations.Items.Delete(Cblocations.items.Count-1);
  CbLocations.Text := ALocation;
end;

procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  L: TStringList;
  i: Integer;
begin
  ini := TMemIniFile.Create(CalcIniName);
  try
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);

    ini.WriteString('MapView', 'Provider', MapView.MapProvider);
    ini.WriteInteger('MapView', 'Zoom', MapView.Zoom);
    ini.WriteString('MapView', 'Center.Longitude', FloatToStr(MapView.Center.Lon, PointFormatSettings));
    ini.WriteString('MapView', 'Center.Latitude', FloatToStr(MapView.Center.Lat, PointFormatSettings));

    ini.EraseSection('Locations');
    for i := 0 to CbLocations.Items.Count-1 do
      ini.WriteString('Locations', 'Item'+IntToStr(i), CbLocations.Items[i]);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.ZoomTrackBarChange(Sender: TObject);
begin
  MapView.Zoom := ZoomTrackBar.Position;
  LblZoom.Caption := Format('Zoom (%d):', [ZoomTrackbar.Position]);
end;


initialization
  PointFormatSettings.DecimalSeparator := '.';

end.

