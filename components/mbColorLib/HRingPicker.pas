unit HRingPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Math, Forms,
  RGBHSVUtils, HTMLColors, mbColorPickerControl;

type
  THRingPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FValue: Double;
    FMaxHue, FMaxSat, FMaxValue: Integer;
    FHueLineColor: TColor;
    FSelectedColor: TColor;
    FManual: boolean;
    mx, my, mdx, mdy: integer;
    //FChange: boolean;
    FRadius: integer;
    FDoChange: boolean;
    FDragging: Boolean;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetValue: Integer;
    function RadHue(New: integer): integer;
    procedure SetMaxHue(h: Integer);
    procedure SetMaxSat(s: Integer);
    procedure SetMaxValue(v: Integer);
    procedure SetRadius(r: integer);
    procedure SetValue(v: integer);
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetHueLineColor(c: TColor);
    procedure DrawHueLine;
    procedure SelectionChanged(x, y: integer);
    procedure UpdateCoords;
  protected
    procedure CreateGradient; override;
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function MouseOnPicker(X, Y: Integer): Boolean;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property ColorUnderCursor;
  published
    property Hue: integer read GetHue write SetHue;
    property Saturation: integer read GetSat write SetSat;
    property Value: integer read GetValue write SetValue;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxValue: Integer read FMaxValue write SetMaxValue default 255;
    property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
    property Radius: integer read FRadius write SetRadius default 40;
    property SelectedColor default clNone;
    property OnChange;
  end;


implementation

uses
  mbUtils;

{ THRingPicker }

constructor THRingPicker.Create(AOwner: TComponent);
begin
  inherited;
  SetInitialBounds(0, 0, 204, 204);
  FMaxHue := 359;
  FMaxSat := 255;
  FMaxValue := 255;
  FValue := 1.0;
  FHue := 0.0;
  FSat := 1.0;
  FHueLineColor := clGray;
  FSelectedColor := clNone;
  FManual := false;
  FChange := true;
  FRadius := 40;
  FDoChange := false;
  HintFormat := 'Hue: %h (selected)';
  TabStop := true;
end;

procedure THRingPicker.CreateGradient;
begin
  FGradientWidth := Min(Width, Height);
  FGradientHeight := FGradientWidth;
  inherited;
end;

procedure THRingPicker.DrawHueLine;
var
  angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
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

function THRingPicker.GetColorAtPoint(x, y: integer): TColor;
var
  angle: Double;
  dx, dy, radius: integer;
  h: Double;
begin
  radius := Min(Width, Height) div 2;

  if PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    dx := x - Radius;
    dy := y - Radius;
    angle := 360 + 180 * arctan2(-dy, dx) / pi;
    if angle < 0 then
      angle := angle + 360
    else if angle > 360 then
      angle := angle - 360;
    h := angle / 360;
    Result := HSVtoColor(h, FSat, FValue);
    if WebSafe then
      Result := GetWebSafe(Result);
  end
  else
    Result := clNone;
end;

{ Outer loop: Y, Inner loop: X }
function THRingPicker.GetGradientColor2D(X, Y: Integer): TColor;
var
  dx, dy: Integer;
  dSq, rSq: Integer;
  radius, size: Integer;
  H: Double;
  q: TRGBQuad;
begin
  size := FGradientWidth;  // or Height, they are the same...
  radius := size div 2;
  rSq := sqr(radius);
  dx := X - radius;
  dy := Y - radius;
  dSq := sqr(dx) + sqr(dy);
  if dSq <= rSq then
  begin
    H := 180 * (1 + arctan2(dx, dy) / pi);  // wp: order (x,y) is correct!
    H := H + 90;
    if H > 360 then H := H - 360;
    Result := HSVtoColor(H/360, FSat, FValue);
    if WebSafe then
      Result := GetWebSafe(Result);
  end else
    Result := GetDefaultColor(dctBrush);
end;

function THRingPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
end;

function THRingPicker.GetSat: Integer;
begin
  Result := round(FSat * FMaxSat);
end;

function THRingPicker.GetSelectedColor: TColor;
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

function THRingPicker.GetValue: Integer;
begin
  Result := round(FValue * FMaxValue);
end;

procedure THRingPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  if ssCtrl in Shift then
    delta := 10
  else
    delta := 1;

  case Key of
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
      end
    else
      erasekey := false;
  end;

  if eraseKey then Key := 0;
  inherited;
end;

procedure THRingPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if (Button = mbLeft) and MouseOnPicker(X, Y)
  then begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
    FDragging := true;
  end;
  SetFocus;
end;

function THRingPicker.MouseOnPicker(X, Y: Integer): Boolean;
var
  diameter, r: Integer;
  P, ctr: TPoint;
begin
  diameter := Min(Width, Height);
  r := diameter div 2;      // outer radius
  P := Point(x, y);
  ctr := Point(r, r);
  Result := PtInCircle(P, ctr, r) and not PtInCircle(P, ctr, Radius);
end;

procedure THRingPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if (ssLeft in Shift) and FDragging then
  begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
  end;
end;

procedure THRingPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if (Button = mbLeft) and FDragging then
  begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
    FDragging := false;
  end;
end;

procedure THRingPicker.Paint;
var
  rgn, r1, r2: HRGN;
  r: TRect;
  size: Integer;
  ringwidth: Integer;
begin
  PaintParentBack(Canvas);
  size := Min(Width, Height);         // diameter of circle
  ringwidth := size div 2 - FRadius;  // FRadius is inner radius
  r := ClientRect;
  r.Right := R.Left + size;
  R.Bottom := R.Top + size;
  InflateRect(R, -1, -1);      // Remove spurious black pixels at the border
  r1 := CreateEllipticRgnIndirect(R);
  if ringwidth > 0 then
  begin
    rgn := r1;
    InflateRect(R, -ringwidth, - ringwidth);
    r2 := CreateEllipticRgnIndirect(R);
    CombineRgn(rgn, r1, r2, RGN_DIFF);
  end;
  SelectClipRgn(Canvas.Handle, rgn);
  Canvas.Draw(0, 0, FBufferBmp);
  DeleteObject(rgn);
  DrawHueLine;
  if FDoChange then
  begin
    if Assigned(FOnChange) then FOnChange(Self);
    FDoChange := false;
  end;
end;

function THRingPicker.RadHue(New: integer): integer;
begin
  if New < 0 then New := New + (FMaxHue + 1);
  if New > (FMaxHue + 1) then New := New - (FMaxHue + 1);
  Result := New;
end;

procedure THRingPicker.Resize;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THRingPicker.SelectionChanged(x, y: integer);
var
  angle, dx, dy, Radius: integer;
begin
  FSelectedColor := clWhite;
  radius := Min(Width, Height) div 2;
  dx := x - radius;
  dy := y - radius;
  angle := round(360 + 180*arctan2(-dy, dx) / pi);
  if angle < 0 then
    inc(angle, 360)
  else if angle > 360 then
    dec(angle, 360);
  FChange := false;
  SetHue(MulDiv(angle, FMaxHue + 1, 360));
  FChange := true;
  Invalidate;
end;

procedure THRingPicker.SetHue(h: integer);
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

procedure THRingPicker.SetHueLineColor(c: TColor);
begin
  if FHueLineColor <> c then
  begin
    FHueLineColor := c;
    Invalidate;
  end;
end;

procedure THRingPicker.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure THRingPicker.SetMaxSat(s: Integer);
begin
  if s = FMaxSat then
    exit;
  FMaxSat := s;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure THRingPicker.SetMaxValue(v: Integer);
begin
  if v = FMaxValue then
    exit;
  FMaxValue := v;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure THRingPicker.SetRadius(r: integer);
begin
  if FRadius <> r then
  begin
    FRadius := r;
    Invalidate;
  end;
end;

procedure THRingPicker.SetSat(s: integer);
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

procedure THRingPicker.SetSelectedColor(c: TColor);
var
  changeSave: boolean;
  h, s, v: Double;
begin
  if WebSafe then c := GetWebSafe(c);
  changeSave := FChange;
  FManual := false;
  FChange := false;
  RGBToHSV(GetRValue(c), GetGValue(c), GetBValue(c), FHue, FSat, FValue);
  FSelectedColor := c;
  UpdateCoords;
  Invalidate;
  FChange := changeSave;
  if FChange and Assigned(FOnChange) then FOnChange(Self);
  FChange := true;
end;

procedure THRingPicker.SetValue(v: integer);
begin
  Clamp(v, 0, FMaxValue);
  if GetValue() <> V then
  begin
    FValue := V / FMaxValue;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THRingPicker.UpdateCoords;
var
  r, angle: double;
  radius: integer;
  sinAngle, cosAngle: Double;
begin
  radius := Min(Width, Height) div 2;
  r := -radius * FSat;
  angle := -(FHue * 2 + 1) * pi;
  SinCos(angle, sinAngle, cosAngle);
  mdx := round(cosAngle * r) + radius;
  mdy := round(sinAngle * r) + radius;
end;

end.
