unit SLColorPicker;

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
  SysUtils, Classes, Controls, Graphics, Math, Forms,
  mbColorPickerControl;

type
  TSLColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FLum: integer;
    FChange: boolean;
    procedure DrawMarker(x, y: integer);
    procedure SelectionChanged(x, y: integer);
    procedure UpdateCoords;
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetLum(l: integer);
  protected
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
    property Luminance: integer read FLum write SetLum default 255;
    property SelectedColor default clWhite;
    property MarkerStyle default msCircle;
    property OnChange;
  end;

implementation

uses
  ScanLines, RGBHSLUtils, HTMLColors, mbUtils;

{ TSLColorPicker }

constructor TSLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  {$IFDEF DELPHI}
  Width := 255;
  Height := 255;
  {$ELSE}
  SetInitialBounds(0, 0, 256, 256);
  {$ENDIF}
  MaxHue := 360;
  MaxSat := 255;
  MaxLum := 255;
  FHue := 0;
  FSat := 0;
  FLum := 255;
  FChange := true;
  MarkerStyle := msCircle;
end;

{ This picker has Saturation along the X and Luminance along the Y axis. }
function TSLColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
var
  q: TRGBQuad;
begin
  q := HSLtoRGBQuad(FHue, x, 255-y);
  Result := RGB(q.rgbRed, q.rgbGreen, q.rgbBlue);
end;

procedure TSLColorPicker.Resize;
begin
  inherited;
  UpdateCoords;
end;

procedure TSLColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure TSLColorPicker.UpdateCoords;
begin
  mdx := MulDiv(FSat, Width, 255);
  mdy := MulDiv(255-FLum, Height, 255);
end;

procedure TSLColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  c := not GetColorAtPoint(x, y);
  InternalDrawMarker(x, y, c);
end;

procedure TSLColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FGradientBMP);
  UpdateCoords;
  DrawMarker(mdx, mdy);
end;

procedure TSLColorPicker.SetHue(h: integer);
begin
  Clamp(h, 0, 360);
  if FHue <> h then
  begin
    FHue := h;
    FManual := false;
    CreateGradient;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetSat(s: integer);
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

procedure TSLColorPicker.SetLum(L: integer);
begin
  Clamp(L, 0, 255);
  if FLum <> L then
  begin
    FLum := L;
    FManual := false;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SelectionChanged(x, y: integer);
begin
  FChange := false;
//  SetSat(MulDiv(255, x, Width));
//  SetLum(MulDiv(255, Height - y, Height));
  SetSat(MulDiv(255, x, Width - 1));
  SetLum(MulDiv(255, Height - y -1, Height - 1));
  FChange := true;
end;

procedure TSLColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  {$IFDEF DELPHI}
  ClipCursor(nil);
  {$ENDIF}
  if csDesigning in ComponentState then Exit;
  if (Button = mbLeft) and PtInRect(ClientRect, Point(x, y)) then
  begin
    mdx := x;
    mdy := y;
    SelectionChanged(X, Y);
    FManual := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if (Button = mbLeft) and PtInRect(ClientRect, Point(x, y)) then
  begin
    mdx := x;
    mdy := y;
    R := ClientRect;
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    {$IFDEF DELPHI}
    ClipCursor(@R);
    {$ENDIF}
    SelectionChanged(X, Y);
    FManual := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
  SetFocus;
end;

procedure TSLColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if (ssLeft in Shift) and PtInRect(ClientRect, Point(x, y)) then
  begin
    mdx := x;
    mdy := y;
    SelectionChanged(X, Y);
    FManual := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetSelectedColor(c: TColor);
var
  h, s, l: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  FManual := false;
  FChange := false;
  RGBTripleToHSL(RGBtoRGBTriple(GetRValue(c), GetGValue(c), GetBValue(c)), h, s, l);
  SetHue(h);
  SetSat(s);
  SetLum(l);
  if FChange and Assigned(FOnChange) then FOnChange(Self);
  FChange := true;
end;

function TSLColorPicker.GetSelectedColor: TColor;
var
  triple: TRGBTriple;
begin
  triple := HSLToRGBTriple(FHue, FSat, FLum);
  if not WebSafe then
    Result := RGBTripleToTColor(triple)
  else
    Result := GetWebSafe(RGBTripleToTColor(triple));
end;

function TSLColorPicker.GetColorAtPoint(x, y: integer): TColor;
var
  triple: TRGBTriple;
begin
  triple := HSLToRGBTriple(FHue, MulDiv(255, x, Width), MulDiv(255, Height - y, Height));
  if not WebSafe then
    Result := RGBTripleToTColor(triple)
  else
    Result := GetWebSafe(RGBTripleToTColor(triple));
end;

procedure TSLColorPicker.CNKeyDown(
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
    if not (mdx - 1 < 0) then
     begin
      Dec(mdx, 1);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
    if not (mdx + 1 > Width) then
     begin
      Inc(mdx, 1);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_UP:
    if not (mdy - 1 < 0) then
     begin
      Dec(mdy, 1);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
    if not (mdy + 1 > Height) then
     begin
      Inc(mdy, 1);
      SelectionChanged(mdx, mdy);
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
    if not (mdx - 10 < 0) then
     begin
      Dec(mdx, 10);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
    if not (mdx + 10 > Width) then
     begin
      Inc(mdx, 10);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_UP:
    if not (mdy - 10 < 0) then
     begin
      Dec(mdy, 10);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
    if not (mdy + 10 > Height) then
     begin
      Inc(mdy, 10);
      SelectionChanged(mdx, mdy);
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
