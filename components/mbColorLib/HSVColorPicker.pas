unit HSVColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Math, Forms, Themes,
  RGBHSVUtils, Scanlines, HTMLColors, mbColorPickerControl;

type
  THSVColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FValue: Double;
    FMaxHue, FMaxSat, FMaxValue: Integer;
    FSatCircColor, FHueLineColor: TColor;
    FSelectedColor: TColor;
    FShowSatCirc: boolean;
    FShowHueLine: boolean;
    FShowSelCirc: boolean;
    function RadHue(New: integer): integer;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetValue: Integer;
    function GetRed: Integer;
    function GetGreen: Integer;
    function GetBlue: Integer;
    procedure SetMaxHue(h: Integer);
    procedure SetMaxSat(s: Integer);
    procedure SetMaxValue(v: Integer);
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetValue(V: integer);
    procedure SetRed(r: Integer);
    procedure SetGreen(g: Integer);
    procedure SetBlue(b: Integer);
    procedure SetSatCircColor(c: TColor);
    procedure SetHueLineColor(c: TColor);
    procedure DrawSatCirc;
    procedure DrawHueLine;
    procedure DrawMarker(x, y: integer);
    procedure SetShowSatCirc(s: boolean);
    procedure SetShowSelCirc(s: boolean);
    procedure SetShowHueLine(s: boolean);
    procedure UpdateCoords;
  protected
    procedure CreateGradient; override;
    procedure CreateWnd; override;
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SelectColor(x, y: integer);
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property Red: Integer read GetRed write SetRed;
    property Green: Integer read GetGreen write SetGreen;
    property Blue: Integer read GetBlue write SetBlue;
  published
    property SelectedColor default clRed;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 255;
    property Value: integer read GetValue write SetValue default 255;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxValue: Integer read FMaxValue write SetMaxValue default 255;
    property SaturationCircleColor: TColor read FSatCircColor write SetSatCircColor default clSilver;
    property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
    property ShowSaturationCircle: boolean read FShowSatCirc write SetShowSatCirc default true;
    property ShowHueLine: boolean read FShowHueLine write SetShowHueLine default true;
    property ShowSelectionCircle: boolean read FShowSelCirc write SetShowSelCirc default true;
    property MarkerStyle default msCrossCirc;
    property OnChange;
  end;

implementation

uses
  mbUtils;

{ THSVColorPicker }

constructor THSVColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  SetInitialBounds(0, 0, 204, 204);
  FMaxHue := 359;
  FMaxSat := 255;
  FMaxValue := 255;
  FHue := 0;
  FSat := 1.0;
  FValue := 1.0;
  FSatCircColor := clSilver;
  FHueLineColor := clGray;
  FSelectedColor := clRed;
  FManual := false;
  FShowSatCirc := true;
  FShowHueLine := true;
  FShowSelCirc := true;
  MarkerStyle := msCrossCirc;
end;

procedure THSVColorPicker.CreateGradient;
begin
  FGradientWidth := Min(Width, Height);
  FGradientHeight := FGradientWidth;
  inherited;
end;

procedure THSVColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THSVColorPicker.DrawSatCirc;
var
  delta: integer;
  radius: integer;
begin
  if not FShowSatCirc then
    exit;
  if (FSat > 0) and (FSat < 1.0) then
  begin
    radius := Min(Width, Height) div 2;
    Canvas.Pen.Color := FSatCircColor;
    Canvas.Brush.Style := bsClear;
    delta := round(radius * FSat);
    Canvas.Ellipse(radius - delta, radius - delta, radius + delta, radius + delta);
  end;
end;

procedure THSVColorPicker.DrawHueLine;
var
  angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
  if not FShowHueLine then
    exit;
  radius := Min(Width, Height) div 2;
  if (FHue >= 0) and (FHue <= 1.0) then
  begin
    angle := -FHue * 2 * pi;
    SinCos(angle, sinAngle, cosAngle);
    Canvas.Pen.Color := FHueLineColor;
    Canvas.MoveTo(radius, radius);
    Canvas.LineTo(radius + round(radius*cosAngle), radius + round(radius*sinAngle));
  end;
end;

procedure THSVColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  if not FShowSelCirc then
    exit;
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clGray;
  InternalDrawMarker(x, y, c);
end;

function THSVColorPicker.GetBlue: Integer;
begin
  Result := GetBValue(FSelectedColor);
end;

function THSVColorPicker.GetColorAtPoint(x, y: integer): TColor;
var
  angle: Double;
  dx, dy, r, radius: integer;
  h, s: double;
begin
  radius := Min(Width, Height) div 2;
  dx := x - Radius;
  dy := y - Radius;

  r := round(sqrt(sqr(dx) + sqr(dy)));
  if r <= radius then
  begin
    angle := 360 + 180 * arctan2(-dy, dx) / pi;
    if angle < 0 then
      angle := angle + 360
    else if angle > 360 then
      angle := angle - 360;
    h := angle / 360;
    s := r / radius;
    Result := HSVtoColor(h, s, FValue);
    if WebSafe then
      Result := GetWebSafe(Result);
  end else
    Result := clNone;
end;

{ Outer loop: Y, Inner loop: X }
function THSVColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
var
  dx, dy: Integer;
  dSq, radiusSq: Integer;
  radius, size: Integer;
  S, H, V: Double;
  q: TRGBQuad;
begin
  size := FGradientWidth;  // or Height, they are the same...
  radius := size div 2;
  radiusSq := sqr(radius);
  dx := X - radius;
  dy := Y - radius;
  dSq := sqr(dx) + sqr(dy);
  if dSq <= radiusSq then
  begin
    if radius <> 0 then
      S := sqrt(dSq) / radius
    else
      S := 0;
    H := 180 * (1 + arctan2(dx, dy) / pi);  // wp: order (x,y) is correct!
    H := H + 90;
    if H > 360 then H := H - 360;
    Result := HSVtoColor(H/360, S, FValue);
    if WebSafe then
      Result := GetWebSafe(Result);
  end else
    Result := GetDefaultColor(dctBrush);
end;

function THSVColorPicker.GetGreen: Integer;
begin
  Result := GetGValue(FSelectedColor);
end;

function THSVColorPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
end;

function THSVColorPicker.GetRed: Integer;
begin
  Result := GetRValue(FSelectedColor);
end;

function THSVColorPicker.GetSat: Integer;
begin
  Result := round(FSat * FMaxSat);
end;

function THSVColorPicker.GetSelectedColor: TColor;
begin
  if FSelectedColor <> clNone then
  begin
    Result := HSVtoColor(FHue, FSat, FValue);
    if WebSafe then
      Result := GetWebSafe(Result);
  end
  else
    Result := clNone;
end;

function THSVColorPicker.GetValue: Integer;
begin
  Result := round(FValue * FMaxValue);
end;

procedure THSVColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  delta := IfThen(ssCtrl in shift, 10, 1);

  case Key  of
    VK_LEFT  : SetHue(RadHue(GetHue() + delta));
    VK_RIGHT : SetHue(RadHue(GetHue() - delta));
    VK_UP    : SetSat(GetSat() + delta);
    VK_DOWN  : SetSat(GetSat() - delta);
    else       eraseKey := false;
  end;

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure THSVColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    exit;
  if (Button = mbLeft) and PointInCircle(Point(x, y), Min(Width, Height)) then
    SelectColor(X, Y);
  SetFocus;
end;

procedure THSVColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    exit;
  if (ssLeft in Shift) and PointInCircle(Point(x, y), Min(Width, Height)) then
    SelectColor(X, Y);
end;

procedure THSVColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    exit;
  if (Button = mbLeft) and PointInCircle(Point(x, y), Min(Width, Height)) then
    SelectColor(X, Y);
end;

procedure THSVColorPicker.Paint;
var
  rgn: HRGN;
  R: TRect;
begin
  PaintParentBack(Canvas);
  R := ClientRect;
  R.Right := R.Left + Min(Width, Height);
  R.Bottom := R.Top + Min(Width, Height);
  InflateRect(R, -1, -1);  // Avoid spurious black pixels at the border
  rgn := CreateEllipticRgnIndirect(R);
  SelectClipRgn(Canvas.Handle, rgn);
  Canvas.Draw(0, 0, FBufferBmp);
  DeleteObject(rgn);
  DrawSatCirc;
  DrawHueLine;
  DrawMarker(mx, my);
end;

function THSVColorPicker.RadHue(New: integer): integer;
begin
  if New < 0 then New := New + (FMaxHue + 1);
  if New > (FMaxHue + 1) then New := New - (FMaxHue + 1);
  Result := New;
end;

procedure THSVColorPicker.Resize;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THSVColorPicker.SelectColor(x, y: integer);
var
  angle: Double;
  dx, dy, r, radius: integer;
  H, S: Double;
begin
  mx := x;
  my := y;

  radius := Min(Width, Height) div 2;
  dx := x - radius;
  dy := y - radius;
  r := round(sqrt(sqr(dx) + sqr(dy)));

  if r > radius then  // point outside circle
  begin
    SetSelectedColor(clNone);
    exit;
  end;

  //FSelectedColor := clWhite;         // ????
  angle := 360 + 180*arctan2(-dy, dx) / pi;   // wp: "-y, x" correct? The others have "x, y"
  if angle < 0 then
    angle := angle + 360
  else if angle > 360 then
    angle := angle - 360;
  H := angle / 360;
  if r > radius then
    S := 1.0
  else
    S := r / radius;

  if (H = FHue) and (S = FSat) then
    exit;

  FHue := H;
  FSat := S;
  FSelectedColor := HSVToColor(FHue, FSat, FValue);
  UpdateCoords;
  Invalidate;
  DoChange;
end;

procedure THSVColorPicker.SetBlue(b: Integer);
var
  c: TColor;
begin
  Clamp(b, 0, 255);
  if b = GetBValue(FSelectedColor) then
    exit;
  c := RgbToColor(GetRValue(FSelectedColor), GetGValue(FSelectedColor), b);
  SetSelectedColor(c);
end;

procedure THSVColorPicker.SetGreen(g: Integer);
var
  c: TColor;
begin
  Clamp(g, 0, 255);
  if g = GetGValue(FSelectedColor) then
    exit;
  c := RgbToColor(GetRValue(FSelectedColor), g, GetBValue(FSelectedColor));
  SetSelectedColor(c);
end;

procedure THSVColorPicker.SetHue(h: integer);
begin
  if h > FMaxHue then h := h - (FMaxHue + 1);
  if h < 0 then h := h + (FMaxHue + 1);
  if GetHue() <> h then
  begin
    FHue := h / FMaxHue;
    FSelectedColor := HSVToColor(FHue, FSat, FValue);
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure THSVColorPicker.SetHueLineColor(c: TColor);
begin
  if FHueLineColor <> c then
  begin
    FHueLineColor := c;
    Invalidate;
  end;
end;

procedure THSVColorPicker.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure THSVColorPicker.SetMaxSat(s: Integer);
begin
  if s = FMaxSat then
    exit;
  FMaxSat := s;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure THSVColorPicker.SetMaxValue(v: Integer);
begin
  if v = FMaxValue then
    exit;
  FMaxValue := v;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure THSVColorPicker.SetRed(r: Integer);
var
  c: TColor;
begin
  Clamp(r, 0, 255);
  if r = GetRValue(FSelectedColor) then
    exit;
  c := RgbToColor(r, GetGValue(FSelectedColor), GetBValue(FSelectedColor));
  SetSelectedColor(c);
end;

procedure THSVColorPicker.SetSat(s: integer);
begin
  Clamp(s, 0, FMaxSat);
  if GetSat() <> s then
  begin
    FSat := s / FMaxSat;
    FSelectedColor := HSVToColor(FHue, FSat, FValue);
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure THSVColorPicker.SetSatCircColor(c: TColor);
begin
  if FSatCircColor <> c then
  begin
    FSatCircColor := c;
    Invalidate;
  end;
end;

procedure THSVColorPicker.SetSelectedColor(c: TColor);
var
  h, s, v: Double;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = FSelectedColor then
    exit;
  RGBtoHSV(GetRValue(c), GetGValue(c), GetBValue(c), FHue, FSat, FValue);
  FSelectedColor := c;
  UpdateCoords;
  Invalidate;
  DoChange;
end;

procedure THSVColorPicker.SetShowHueLine(s: boolean);
begin
  if FShowHueLine <> s then
  begin
    FShowHueLine := s;
    Invalidate;
  end;
end;

procedure THSVColorPicker.SetShowSatCirc(s: boolean);
begin
  if FShowSatCirc <> s then
  begin
    FShowSatCirc := s;
    Invalidate;
  end;
end;

procedure THSVColorPicker.SetShowSelCirc(s: boolean);
begin
  if FShowSelCirc <> s then
  begin
    FShowSelCirc := s;
    Invalidate;
  end;
end;

procedure THSVColorPicker.SetValue(V: integer);
begin
  Clamp(V, 0, FMaxValue);
  if GetValue() <> V then
  begin
    FValue := V / FMaxValue;
    FSelectedColor := HSVToColor(FHue, FSat, FValue);
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure THSVColorPicker.UpdateCoords;
var
  r, angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
  radius := Min(Width, Height) div 2;
  r := -FSat * radius;
  angle := -(FHue * 2 + 1) * pi;
  SinCos(angle, sinAngle, cosAngle);
  mx := round(cosAngle * r) + radius;
  my := round(sinAngle * r) + radius;
end;

end.
