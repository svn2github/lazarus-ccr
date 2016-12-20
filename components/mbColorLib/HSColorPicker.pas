unit HSColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$DEFINE USE COLOR_TO_RGB}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Math, Forms,
  RGBHSLUtils, HTMLColors, mbColorPickerControl;

type

  { THSColorPicker }

  THSColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FLum: Double;
    FMaxHue, FMaxSat, FMaxLum: Integer;
    dx, dy, mxx, myy: integer;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    procedure SetHue(H: integer);
    procedure SetLum(L: Integer);
    procedure SetSat(S: integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
  protected
    procedure CorrectCoords(var x, y: integer);
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    procedure SetSelectedColor(c: TColor); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawMarker(x, y: integer);
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    function PredictColor: TColor;
  public
    constructor Create(AOwner: TComponent); override;
    property Hue: integer read GetHue write SetHue;
    property Saturation: integer read GetSat write SetSat;
  published
    property SelectedColor default clRed;
    property Luminance: Integer read GetLum write SetLum default 120;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 240;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum default 240;
    property MarkerStyle default msCross;
    property OnChange;
  end;

implementation

uses
  mbUtils;

{THSColorPicker}

constructor THSColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FMaxHue := 359;
  FMaxSat := 240;
  FMaxLum := 240;
  FGradientWidth := FMaxHue + 1;
  FGradientHeight := FMaxSat + 1;
  SetInitialBounds(0, 0, FGradientWidth, FGradientHeight);
  HintFormat := 'H: %h S: %hslS'#13'Hex: %hex';
  FHue := 0;
  FSat := 1.0;
  FLum := 0.5;
  FSelected := clRed;
  FManual := false;
  dx := 0;
  dy := 0;
  mxx := 0;
  myy := 0;
  MarkerStyle := msCross;
end;

procedure THSColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure THSColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure THSColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
  L: Double;
begin
  CorrectCoords(x, y);

  {$IFDEF USE_COLOR_TO_RGB}
  ColorToHSL(FSelected, FHue, FSat, L);
  {$ELSE}
  RGBToHSL(FSelected, FHue, FSat, L);
  {$ENDIF}

  dx := x;
  dy := y;
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clWhite;
  InternalDrawMarker(x, y, c);
end;

function THSColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
begin
  {$IFDEF USE_COLOR_TO_RGB}
  Result := HSLToColor(x / FMaxHue, (FBufferBmp.Height - 1 - y) / FMaxSat, FLum);
  {$ELSE}
  Result := HSLtoRGB(x / FMaxHue, (FMaxSat - y) / FMaxSat, FLum);
  {$ENDIF}
end;

function THSColorPicker.GetHue: Integer;
begin
  Result := Round(FHue * FMaxHue);
end;

function THSColorPicker.GetLum: Integer;
begin
  Result := Round(FLum * FMaxLum);
end;

function THSColorPicker.GetSat: Integer;
begin
  Result := Round(FSat * FMaxSat);
end;

procedure THSColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  if (ssCtrl in Shift) then
    delta := 10
  else
    delta := 1;
  case Key of
    VK_LEFT:
      begin
        mxx := dx - delta;
        myy := dy;
        FSelected := GetColorAtPoint(mxx, myy);
        if Assigned(OnChange) then OnChange(Self);
        FManual := true;
        Invalidate;
      end;
    VK_RIGHT:
      begin
        mxx := dx + delta;
        myy := dy;
        FSelected := GetColorAtPoint(mxx, myy);
        if Assigned(OnChange) then OnChange(Self);
        FManual := true;
        Invalidate;
      end;
    VK_UP:
      begin
        mxx := dx;
        myy := dy - delta;
        FSelected := GetColorAtPoint(mxx, myy);
        if Assigned(OnChange) then OnChange(Self);
        FManual := true;
        Invalidate;
      end;
    VK_DOWN:
      begin
        mxx := dx;
        myy := dy + delta;
        FSelected := GetColorAtPoint(mxx, myy);
        if Assigned(OnChange) then OnChange(Self);
        FManual := true;
        Invalidate;
      end;
    else
      eraseKey := false;
  end;

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure THSColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  mxx := x;
  myy := y;
  if Button = mbLeft then
  begin
    SetSelectedColor(GetColorAtPoint(x, y));
    FManual := true;
    Invalidate;
  end;
  SetFocus;
end;

procedure THSColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := x;
    myy := y;
    SetSelectedColor(GetColorAtPoint(x, y));
    FManual := true;
    Invalidate;
  end;
end;

procedure THSColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := x;
    myy := y;
    SetSelectedColor(GetColorAtPoint(x, y));
    FManual := true;
    Invalidate;
  end;
end;

procedure THSColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  CorrectCoords(mxx, myy);
  DrawMarker(mxx, myy);
end;

function THSColorPicker.PredictColor: TColor;
var
  H, S, L: Double;
begin
  {$IFDEF USE_COLOR_TO_RGB}
  ColorToHSL(GetColorUnderCursor, H, S, L);
  {$ELSE}
  RGBtoHSL(GetColorUnderCursor, H, S, L);
  {$ENDIF}
  Result := HSLToRGB(H, S, L);
end;

procedure THSColorPicker.Resize;
begin
  SetSelectedColor(FSelected);
  inherited;
end;

procedure THSColorPicker.SetHue(H: integer);
begin
  Clamp(H, 0, FMaxHue);
  FHue := H / FMaxHue;
  {$IFDEF USE_COLOR_TO_RGB}
  SetSelectedColor(HSLtoColor(FHue, FSat, FLum));
  {$ELSE}
  SetSelectedColor(HSLToRGB(FHue, FSat, FLum));
  {$ENDIF}
end;

// Sets the luminance value used for the display. It is not necessarily that
// of the selected color.
procedure THSColorPicker.SetLum(L: Integer);
begin
  Clamp(L, 0, FMaxLum);
  FLum := L / FMaxLum;
  CreateGradient;
  {$IFDEF USE_COLOR_TO_RGB}
  SetSelectedColor(HSLtoColor(FHue, FSat, FLum));
  {$ELSE}
  SetSelectedColor(HSLToRGB(FHue, FSat, FLum));
  {$ENDIF}
end;

procedure THSColorPicker.SetSat(S: integer);
begin
  Clamp(S, 0, FMaxSat);
  FSat := S;
  {$IFDEF USE_COLOR_TO_RGB}
  SetSelectedColor(HSLtoColor(FHue, FSat, FLum));
  {$ELSE}
  SetSelectedColor(HSLToRGB(FHue, FSat, FLum));
  {$ENDIF}
end;

procedure THSColorPicker.SetMaxHue(H: Integer);
begin
  if H = FMaxHue then
    exit;
  FMaxHue := H;
  FGradientWidth := FMaxHue + 1;
  CreateGradient;
  Invalidate;
end;

procedure THSColorPicker.SetMaxLum(L: Integer);
begin
  if L = FMaxLum then
    exit;
  FMaxLum := L;
  CreateGradient;
  Invalidate;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure THSColorPicker.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FMaxSat := S;
  FGradientHeight := FMaxSat + 1;
  CreateGradient;
  Invalidate;
end;

procedure THSColorPicker.SetSelectedColor(c: TColor);
var
  L: Double;
begin
  if WebSafe then c := GetWebSafe(c);
  {$IFDEF USE_COLOR_TO_RGB}
  ColorToHSL(c, FHue, FSat, L);
  {$ELSE}
  RGBtoHSL(c, FHue, FSat, L);
  {$ENDIF}
  FSelected := c;
  FManual := false;
  mxx := Round(FHue * Width);
  myy := Round((1.0 - FSat) * Height);
  Invalidate;
  if Assigned(OnChange) then OnChange(Self);
end;

end.
