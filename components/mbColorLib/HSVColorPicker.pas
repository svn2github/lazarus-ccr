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
    FChange: boolean;
    FDoChange: boolean;
    function RadHue(New: integer): integer;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetValue: Integer;
    procedure SetMaxHue(h: Integer);
    procedure SetMaxSat(s: Integer);
    procedure SetMaxValue(v: Integer);
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetValue(V: integer);
    procedure SetSatCircColor(c: TColor);
    procedure SetHueLineColor(c: TColor);
    procedure DrawSatCirc;
    procedure DrawHueLine;
    procedure DrawMarker(x, y: integer);
    procedure SelectionChanged(x, y: integer);
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
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
  published
    property Hue: integer read GetHue write SetHue;
    property Saturation: integer read GetSat write SetSat;
    property Value: integer read GetValue write SetValue;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxValue: Integer read FMaxValue write SetMaxValue default 255;
    property SaturationCircleColor: TColor read FSatCircColor write SetSatCircColor default clSilver;
    property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
    property SelectedColor default clNone;
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
  FSat := 0;
  FValue := 1.0;
  FSatCircColor := clSilver;
  FHueLineColor := clGray;
  FSelectedColor := clNone;
  FManual := false;
  FShowSatCirc := true;
  FShowHueLine := true;
  FShowSelCirc := true;
  FChange := true;
  FDoChange := false;
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

function THSVColorPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
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
  if ssCtrl in shift then
    delta := 10
  else
    delta := 1;

  case Key  of
    VK_LEFT:
      begin
        FChange := false;
        SetHue(RadHue(GetHue() + delta));
        FChange := true;
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    VK_RIGHT:
      begin
        FChange := false;
        SetHue(RadHue(GetHue() - delta));
        FChange := true;
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    VK_UP:
      begin
        FChange := false;
        SetSat(GetSat() + delta);
        FChange := true;
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    VK_DOWN:
      begin
        FChange := false;
        SetSat(GetSat() - delta);
        FChange := true;
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
  else
    eraseKey := false;
  end;

  if eraseKey then Key := 0;
  inherited;
end;

procedure THSVColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    exit;
  if (Button = mbLeft) and PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
  end;
  SetFocus;
end;

procedure THSVColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    exit;
  if (ssLeft in Shift) and PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
  end;
end;

procedure THSVColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    exit;
  if (Button = mbLeft) and PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
  end;
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
  DrawMarker(mdx, mdy);
  if FDoChange then
  begin
    if Assigned(FOnChange) then FOnChange(Self);
    FDoChange := false;
  end;
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

procedure THSVColorPicker.SelectionChanged(x, y: integer);
var
  angle: Double;
  dx, dy, r, radius: integer;
begin
  radius := Min(Width, Height) div 2;
  dx := x - radius;
  dy := y - radius;
  r := round(sqrt(sqr(dx) + sqr(dy)));

  if r > radius then  // point outside circle
  begin
    FChange := false;
    SetSelectedColor(clNone);
    FChange := true;
    exit;
  end;

  FSelectedColor := clWhite;
  angle := 360 + 180*arctan2(-dy, dx) / pi;   // wp: "-y, x" correct? The others have "x, y"
  if angle < 0 then
    angle := angle + 360
  else if angle > 360 then
    angle := angle - 360;
  FChange := false;
  FHue := angle / 360;
  if r > radius then
    FSat := 1.0
  else
    FSat := r / radius;
  FChange := true;

  Invalidate;
end;

procedure THSVColorPicker.SetHue(h: integer);
begin
  if h > FMaxHue then h := h - (FMaxHue + 1);
  if h < 0 then h := h + (FMaxHue + 1);
  if GetHue() <> h then
  begin
    FHue := h / FMaxHue;
    FManual := false;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
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
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure THSVColorPicker.SetMaxSat(s: Integer);
begin
  if s = FMaxSat then
    exit;
  FMaxSat := s;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure THSVColorPicker.SetMaxValue(v: Integer);
begin
  if v = FMaxValue then
    exit;
  FMaxValue := v;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure THSVColorPicker.SetSat(s: integer);
begin
  Clamp(s, 0, FMaxSat);
  if GetSat() <> s then
  begin
    FSat := s / FMaxSat;
    FManual := false;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
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
  changeSave: boolean;
  h, s, v: Double;
begin
  if WebSafe then c := GetWebSafe(c);
  changeSave := FChange;
  FManual := false;
  FChange := false;
  RGBtoHSV(GetRValue(c), GetGValue(c), GetBValue(c), FHue, FSat, FValue);
  FSelectedColor := c;
  UpdateCoords;
  Invalidate;
  FChange := changeSave;
  if FChange and Assigned(FOnChange) then FOnChange(Self);
  FChange := true;
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
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
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
  mdx := round(cosAngle * r) + radius;
  mdy := round(sinAngle * r) + radius;
end;

end.
