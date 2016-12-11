unit HRingPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Math, RGBHSVUtils,
  Forms, {IFDEF DELPHI_7_UP Themes, $ENDIF} HTMLColors, mbColorPickerControl;

type
  THRingPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FValue: integer;
    FHueLineColor: TColor;
    FSelectedColor: TColor;
    FManual: boolean;
    mx, my, mdx, mdy: integer;
    FChange: boolean;
    FRadius: integer;
    FDoChange: boolean;
    function RadHue(New: integer): integer;
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
    procedure SetSelectedColor(c: TColor); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CNKeyDown(var Message: {$IFDEF FPC}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF});
      message CN_KEYDOWN;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
  published
    property Hue: integer read FHue write SetHue default 0;
    property Saturation: integer read FSat write SetSat default 0;
    property Value: integer read FValue write SetValue default 255;
    property HueLineColor: TColor read FHueLineColor write SetHueLineColor default clGray;
    property SelectedColor default clNone;
    property Radius: integer read FRadius write SetRadius default 40;
    property OnChange;
  end;


implementation

uses
  mbUtils;

{ THRingPicker }

constructor THRingPicker.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF DELPHI}
  Width := 204;
  Height := 204;
  {$ELSE}
  SetInitialBounds(0, 0, 204, 204);
  {$ENDIF}
  FValue := 255;
  FHue := 0;
  FSat := 0;
  FHueLineColor := clGray;
  FSelectedColor := clNone;
  FManual := false;
  FChange := true;
  FRadius := 40;
  FDoChange := false;
end;

procedure THRingPicker.CreateGradient;
begin
  FGradientWidth := Min(Width, Height);
  FGradientHeight := FGradientWidth;
  inherited;
end;

{ Outer loop: Y, Inner loop: X }
function THRingPicker.GetGradientColor2D(X, Y: Integer): TColor;
var
  xcoord, ycoord: Integer;
  dSq, radiusSq: Integer;
  radius, size: Integer;
  S, H, V: Integer;
  q: TRGBQuad;
begin
  size := FGradientWidth;  // or Height, they are the same...
  radius := size div 2;
  radiusSq := sqr(radius);
  xcoord := X - radius;
  ycoord := Y - radius;
  dSq := sqr(xcoord) + sqr(ycoord);
  if dSq <= radiusSq then
  begin
    if radius <> 0 then
      S := round((255 * sqrt(dSq)) / radius)
    else
      S := 0;
    H := round( 180 * (1 + arctan2(xcoord, ycoord) / pi));  // wp: order (x,y) is correct!
    H := H + 90;
    if H > 360 then H := H - 360;
    Result := HSVtoColor(H, S, FValue);
    if WebSafe then
      Result := GetWebSafe(Result);
  end else
    Result := GetDefaultColor(dctBrush);
end;

procedure THRingPicker.Resize;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THRingPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THRingPicker.UpdateCoords;
var
  r, angle: real;
  radius: integer;
  sinAngle, cosAngle: Double;
begin
  radius := Min(Width, Height) div 2;
  r := -MulDiv(radius, FSat, 255);
  angle := -FHue * pi/180 - pi;
  SinCos(angle, sinAngle, cosAngle);
  mdx := round(cosAngle * r) + radius;
  mdy := round(sinAngle * r) + radius;
end;

procedure THRingPicker.SetHue(h: integer);
begin
  Clamp(h, 0, 360);
  if FHue <> h then
  begin
    FHue := h;
    FManual := false;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THRingPicker.SetSat(s: integer);
begin
  Clamp(s, 0, 255);
  if FSat <> s then
  begin
    FSat := s;
    FManual := false;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THRingPicker.SetValue(v: integer);
begin
  Clamp(v, 0, 255);
  if FValue <> V then
  begin
    FValue := V;
    FManual := false;
    CreateGradient;
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

procedure THRingPicker.SetRadius(r: integer);
begin
  if FRadius <> r then
  begin
    FRadius := r;
    Invalidate;
  end;
end;

procedure THRingPicker.DrawHueLine;
var
  angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
  radius := Min(Width, Height) div 2;
  if (FHue >= 0) and (FHue <= 360) then
  begin
    angle := -FHue*PI/180;
    SinCos(angle, sinAngle, cosAngle);
    Canvas.Pen.Color := FHueLineColor;
    Canvas.MoveTo(radius, radius);
    Canvas.LineTo(radius + round(radius*cosAngle), radius + round(radius*sinAngle));
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
  Canvas.Draw(0, 0, FGradientBmp);
  DeleteObject(rgn);
  DrawHueLine;
  if FDoChange then
  begin
    if Assigned(FOnChange) then FOnChange(Self);
    FDoChange := false;
  end;
end;

procedure THRingPicker.SelectionChanged(x, y: integer);
var
  angle, Distance, xDelta, yDelta, Radius: integer;
begin
  if not PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    FChange := false;
    SetSelectedColor(clNone);
    FChange := true;
    Exit;
  end
  else
    FSelectedColor := clWhite;
  Radius := Min(Width, Height) div 2;
  xDelta := x - Radius;
  yDelta := y - Radius;
  angle := round(360 + 180*arctan2(-yDelta, xDelta) / pi);
  if angle < 0 then
    Inc(angle, 360)
  else if angle > 360 then
    Dec(angle, 360);
  FChange := false;
  SetHue(angle);
  distance := round(sqrt(sqr(xDelta) + sqr(yDelta)));
  if distance >= radius then
    SetSat(255)
  else
    SetSat(MulDiv(distance, 255, radius));
  FChange := true;
end;

procedure THRingPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  {$IFDEF DELPHI}
  ClipCursor(nil);
  {$ENDIF}
  if csDesigning in ComponentState then Exit;
  if (Button = mbLeft) and PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
  end;
end;

procedure THRingPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if (Button = mbLeft) and PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    mdx := x;
    mdy := y;
    R := ClientRect;
    InflateRect(R, 1, 1);
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    {$IFDEF DELPHI}
    ClipCursor(@R);
    {$ENDIF}
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
  end;
  SetFocus;
end;

procedure THRingPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if (ssLeft in Shift) and PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    mdx := x;
    mdy := y;
    FDoChange := true;
    SelectionChanged(X, Y);
    FManual := true;
  end;
end;

function THRingPicker.GetSelectedColor: TColor;
begin
  if FSelectedColor <> clNone then
  begin
    if not WebSafe then
      Result := HSVtoColor(FHue, FSat, FValue)
    else
      Result := GetWebSafe(HSVtoColor(FHue, FSat, FValue));
  end
  else
    Result := clNone;
end;

function THRingPicker.GetColorAtPoint(x, y: integer): TColor;
var
  angle, distance, xDelta, yDelta, radius: integer;
  h, s: integer;
begin
  radius := Min(Width, Height) div 2;
  xDelta := x - Radius;
  yDelta := y - Radius;
  angle := round(360 + 180*arctan2(-yDelta, xDelta) / pi);
  if angle < 0 then
    Inc(angle, 360)
  else if angle > 360 then
    Dec(angle, 360);
  h := angle;
  distance := round(sqrt(sqr(xDelta) + sqr(yDelta)));
  if distance >= radius then
    s := 255
  else
    s := MulDiv(distance, 255, radius);
  if PointInCircle(Point(mx, my), Min(Width, Height)) then
  begin
    if not WebSafe then
      Result := HSVtoColor(h, s, FValue)
    else
      Result := GetWebSafe(HSVtoColor(h, s, FValue));
  end
  else
    Result := clNone;
end;

procedure THRingPicker.SetSelectedColor(c: TColor);
var
  changeSave: boolean;
begin
  if WebSafe then c := GetWebSafe(c);
  changeSave := FChange;
  FManual := false;
  Fchange := false;
  SetValue(GetVValue(c));
  SetHue(GetHValue(c));
  SetSat(GetSValue(c));
  FSelectedColor := c;
  FChange := changeSave;
  if FChange and Assigned(FOnChange) then FOnChange(Self);
  FChange := true;
end;

function THRingPicker.RadHue(New: integer): integer;
begin
  if New < 0 then New := New + 360;
  if New > 360 then New := New - 360;
  Result := New;
end;

procedure THRingPicker.CNKeyDown(
  var Message: {$IFDEF FPC}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF} );
var
 Shift: TShiftState;
 FInherited: boolean;
begin
 FInherited := false;
 Shift := KeyDataToShiftState(Message.KeyData);
 if not (ssCtrl in Shift) then
  case Message.CharCode of
   VK_LEFT:
     begin
      FChange := false;
      SetHue(RadHue(FHue + 1));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
     begin
      FChange := false;
      SetHue(RadHue(FHue - 1));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end
  else
   begin
    FInherited := true;
    inherited;
   end;
  end
 else
  case Message.CharCode of
   VK_LEFT:
     begin
      FChange := false;
      SetHue(RadHue(FHue + 10));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
     begin
      FChange := false;
      SetHue(RadHue(FHue - 10));
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end
  else
   begin
    FInherited := true;
    inherited;
   end;
  end;
 if not FInherited then
  if Assigned(OnKeyDown) then
   OnKeyDown(Self, Message.CharCode, Shift);
end;

end.
