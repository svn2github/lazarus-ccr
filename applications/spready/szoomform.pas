unit sZoomForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ButtonPanel, Spin,
  fpspreadsheet;

type

  { TZoomForm }

  TZoomForm = class(TForm)
    ButtonPanel: TButtonPanel;
    EdZoom: TSpinEdit;
    LblPercent: TLabel;
    LblZoom: TLabel;
    TrackBar: TTrackBar;
    procedure EdZoomChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    FWorksheet: TsWorksheet;
    FInitialZoom: Double;
    FOKClicked: Boolean;
    FLevels: array of Integer;
    function GetZoomLevel: Integer;
    procedure SetWorksheet(AValue: TsWorksheet);
    procedure SetZoomLevel(AValue: Integer);
  protected
    procedure ApplyZoom;
    function FindZoomLevelIndex(AValue: Integer): Integer;

  public
    property Worksheet: TsWorksheet read FWorksheet write SetWorksheet;
    property ZoomLevel: Integer read GetZoomLevel;

  end;

var
  ZoomForm: TZoomForm;

implementation

{$R *.lfm}

const
  ZOOM_LEVELS: array[0..26] of Integer = (
    10,   20,  30,  40,  50,  60,  65,  70,  75,  80,  85,  90,  95,
    100,
    105, 110, 115, 120, 130, 140, 150, 160, 180, 200, 250, 300, 400);

procedure TZoomForm.ApplyZoom;
begin
  if Assigned(FWorksheet) then
    FWorksheet.Zoomfactor := GetZoomLevel / 100;
end;

procedure TZoomForm.EdZoomChange(Sender: TObject);
var
  idx: Integer;
begin
  idx := FindZoomLevelIndex(EdZoom.Value);
  if idx > -1 then Trackbar.Position := idx;
  ApplyZoom;
end;

function TZoomForm.FindZoomLevelIndex(AValue: Integer): Integer;
begin
  for Result := Low(ZOOM_LEVELS) to High(ZOOM_LEVELS) do
    if ZOOM_LEVELS[Result] = AValue then exit;
  Result := -1;
end;

procedure TZoomForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FWorksheet <> nil then begin
    if not FOKClicked then
      FWorksheet.ZoomFactor := FInitialZoom;
  end;
end;

procedure TZoomForm.FormCreate(Sender: TObject);
begin
  Trackbar.Min := 0;
  TrackBar.Max := High(ZOOM_LEVELS);
end;

function TZoomForm.GetZoomLevel: Integer;
begin
  Result := EdZoom.Value;
end;

procedure TZoomForm.OKButtonClick(Sender: TObject);
begin
  FOKClicked := true;
end;

procedure TZoomForm.TrackBarChange(Sender: TObject);
begin
  EdZoom.Value := ZOOM_LEVELS[Trackbar.Position];
end;

procedure TZoomForm.SetWorksheet(AValue: TsWorksheet);
begin
  if FWorksheet = AValue then exit;
  FWorksheet := AValue;
  FInitialZoom := FWorksheet.ZoomFactor;
  SetZoomLevel(Round(FWorksheet.ZoomFactor * 100));
end;

procedure TZoomForm.SetZoomLevel(AValue: Integer);
begin
  if AValue = GetZoomLevel then exit;
  EdZoom.Value := AValue;
end;

end.

