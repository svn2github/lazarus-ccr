unit HSVColorPicker;

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
  SysUtils, Classes, Controls, Graphics, Math, RGBHSVUtils, Scanlines,
  Forms, {$IFDEF DELPHI_7_UP}Themes,{$ENDIF}
  HTMLColors, mbColorPickerControl;

type
  THSVColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FValue: integer;
    FSatCircColor, FHueLineColor: TColor;
    FSelectedColor: TColor;
    FShowSatCirc: boolean;
    FShowHueLine: boolean;
    FShowSelCirc: boolean;
    FChange: boolean;
    FDoChange: boolean;
    function RadHue(New: integer): integer;
    procedure SetValue(V: integer);
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
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
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    procedure SetSelectedColor(c: TColor); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function MouseOnPicker(X, Y: Integer): Boolean; override;
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
  {$IFDEF DELPHI}
  Width := 204;
  Height := 204;
  {$ELSE}
  SetInitialBounds(0, 0, 204, 204);
  {$ENDIF}
  FValue := 255;
  FHue := 0;
  FSat := 0;
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

procedure THSVColorPicker.CreateGradient;
begin
  FGradientWidth := Min(Width, Height);
  FGradientHeight := FGradientWidth;
  inherited;
end;

{ Outer loop: Y, Inner loop: X }
function THSVColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
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
      //S := trunc((255 * sqrt(dSq)) / radius)
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

procedure THSVColorPicker.Resize;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THSVColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure THSVColorPicker.UpdateCoords;
var
  r, angle: double;
  sinAngle, cosAngle: Double;
  radius: integer;
begin
  radius := Min(Width, Height) div 2;
  r := -MulDiv(radius, FSat, 255);
  angle := -FHue* pi / 180 - PI;
  SinCos(angle, sinAngle, cosAngle);
  mdx := round(cosAngle * r) + radius;
  mdy := round(sinAngle * r) + radius;
end;

procedure THSVColorPicker.SetHue(h: integer);
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

procedure THSVColorPicker.SetSat(s: integer);
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

procedure THSVColorPicker.SetValue(V: integer);
begin
  Clamp(V, 0, 255);
  if FValue <> V then
  begin
    FValue := V;
    FManual := false;
    CreateGradient;
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

procedure THSVColorPicker.SetHueLineColor(c: TColor);
begin
  if FHueLineColor <> c then
  begin
    FHueLineColor := c;
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

procedure THSVColorPicker.SetShowHueLine(s: boolean);
begin
  if FShowHueLine <> s then
  begin
    FShowHueLine := s;
    Invalidate;
  end;
end;

procedure THSVColorPicker.DrawSatCirc;
var
  delta: integer;
  radius: integer;
begin
  if not FShowSatCirc then
    exit;
  if (FSat > 0) and (FSat < 255) then
  begin
    radius := Min(Width, Height) div 2;
    Canvas.Pen.Color := FSatCircColor;
    Canvas.Brush.Style := bsClear;
    delta := MulDiv(radius, FSat, 255);
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
  if (FHue >= 0) and (FHue <= 360) then
  begin
    angle := -FHue * pi / 180;
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

procedure THSVColorPicker.SelectionChanged(x, y: integer);
var
  angle, distance, xDelta, yDelta, radius: integer;
begin
  if not PointInCircle(Point(x, y), Min(Width, Height)) then
  begin
    FChange := false;
    SetSelectedColor(clNone);
    FChange := true;
    exit;
  end
  else
    FSelectedColor := clWhite;
  radius := Min(Width, Height) div 2;
  xDelta := x - radius;
  yDelta := y - radius;
  angle := round(360 + 180*arctan2(-yDelta, xDelta) / pi);
  if angle < 0 then
    inc(angle, 360)
  else if angle > 360 then
    dec(angle, 360);
  FChange := false;
  SetHue(Angle);
  distance := round(sqrt(sqr(xDelta) + sqr(yDelta)));
  if distance >= radius then
    SetSat(255)
  else
    SetSat(MulDiv(distance, 255, radius));
  FChange := true;
end;

procedure THSVColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  {$IFDEF DELPHI}
  ClipCursor(nil);
  {$ENDIF}
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

procedure THSVColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if csDesigning in ComponentState then
    exit;
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

function THSVColorPicker.MouseOnPicker(X, Y: Integer): Boolean;
var
  diameter, r: Integer;
  P, ctr: TPoint;
begin
  diameter := Min(Width, Height);
  r := diameter div 2;
  P := Point(x, y);
  ctr := Point(r, r);
  Result := PtInCircle(P, ctr, r);
end;

function THSVColorPicker.GetSelectedColor: TColor;
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

function THSVColorPicker.GetColorAtPoint(x, y: integer): TColor;
var
  angle, distance, xDelta, yDelta, radius: integer;
  h, s: integer;
begin
  radius := Min(Width, Height) div 2;
  xDelta := x - Radius;
  yDelta := y - Radius;
  angle := round(360 + 180*arctan2(-yDelta, xDelta) / pi);
  if angle < 0 then
    inc(angle, 360)
  else if angle > 360 then
    dec(angle, 360);
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

procedure THSVColorPicker.SetSelectedColor(c: TColor);
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

function THSVColorPicker.RadHue(New: integer): integer;
begin
  if New < 0 then New := New + 360;
  if New > 360 then New := New - 360;
  Result := New;
end;

procedure THSVColorPicker.CNKeyDown(
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
     end;
   VK_UP:
     begin
      FChange := false;
      if FSat + 1 <= 255 then
       SetSat(FSat + 1);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
     begin
      FChange := false;
      if FSat - 1 >= 0  then
       SetSat(FSat - 1);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
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
     end;
   VK_UP:
     begin
      FChange := false;
      if FSat + 10 <= 255 then
       SetSat(FSat + 10);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
     begin
      FChange := false;
      if FSat - 10 >= 0 then
       SetSat(FSat - 10);
      FChange := true;
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
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
