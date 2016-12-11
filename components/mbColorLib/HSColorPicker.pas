unit HSColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages, Scanlines,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Math, Forms,
  RGBHSLUtils, HTMLColors, mbColorPickerControl;

type

  { THSColorPicker }

  THSColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSaturation, FLuminance: integer;
    FLum: integer;
    dx, dy, mxx, myy: integer;
    procedure SetHValue(h: integer);
    procedure SetSValue(s: integer);
  protected
    procedure CorrectCoords(var x, y: integer);
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    procedure SetSelectedColor(c: TColor); override;
    procedure CNKeyDown(var Message: {$IFDEF FPC}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF});
      message CN_KEYDOWN;
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
    property Lum: integer read FLum write FLum default 120;
  published
    property SelectedColor default clRed;
    property HueValue: integer read FHue write SetHValue default 0;
    property SaturationValue: integer read FSaturation write SetSValue default 240;
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
  FGradientWidth := 240;
  FGradientHeight := 241;
 {$IFDEF DELPHI}
  Width := 239;
  Height := 240;
 {$ELSE}
  SetInitialBounds(0, 0, 239, 240);
 {$ENDIF}
  HintFormat := 'H: %h S: %hslS'#13'Hex: %hex';
  FHue := 0;
  FSaturation := 240;
  FLuminance := 120;
  FSelected := clRed;
  FLum := 120;
  FManual := false;
  dx := 0;
  dy := 0;
  mxx := 0;
  myy := 0;
  MarkerStyle := msCross;
end;

procedure THSColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

function THSColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
begin
  Result := HSLRangeToRGB(x, FGradientBmp.Height - 1 - y, 120);
end;

procedure THSColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure THSColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  CorrectCoords(x, y);
  RGBtoHSLRange(FSelected, FHue, FSaturation, FLuminance);
  if Assigned(FOnChange) then
    FOnChange(Self);
  dx := x;
  dy := y;
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clWhite;
  InternalDrawMarker(x, y, c);
end;

procedure THSColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  RGBtoHSLRange(c, FHue, FSaturation, FLuminance);
  FSelected := c;
  FManual := false;
  mxx := Round(FHue*(Width/239));
  myy := Round((240-FSaturation)*(Height/240));
  Invalidate;
end;

procedure THSColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FGradientBmp);
  CorrectCoords(mxx, myy);
  DrawMarker(mxx, myy);
end;

procedure THSColorPicker.Resize;
begin
  SetSelectedColor(FSelected);
  inherited;
end;

procedure THSColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  mxx := x;
  myy := y;
  if Button = mbLeft then
  begin
    R := ClientRect;
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    {$IFDEF DELPHI}
    ClipCursor(@R);
    {$ENDIF}
    FSelected := GetColorAtPoint(x, y);
    FManual := true;
    Invalidate;
  end;
  SetFocus;
end;

procedure THSColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  {$IFDEF DELPHI}
  ClipCursor(nil);
  {$ENDIF}
  mxx := x;
  myy := y;
  FSelected := GetColorAtPoint(x, y);
  FManual := true;
  Invalidate;
end;

procedure THSColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := x;
    myy := y;
    FSelected := GetColorAtPoint(x, y);
    FManual := true;
    Invalidate;
  end;
end;

function THSColorPicker.PredictColor: TColor;
var
  FTHue, FTSat, FTLum: integer;
begin
  RGBtoHSLRange(GetColorUnderCursor, FTHue, FTSat, FTLum);
  Result := HSLRangeToRGB(FTHue, FTSat, FLum);
end;

procedure THSColorPicker.CNKeyDown(
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
     mxx := dx - 1;
     myy := dy;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_RIGHT:
    begin
     mxx := dx + 1;
     myy := dy;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_UP:
    begin
     mxx := dx;
     myy := dy - 1;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_DOWN:
    begin
     mxx := dx;
     myy := dy + 1;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
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
     mxx := dx - 10;
     myy := dy;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_RIGHT:
    begin
     mxx := dx + 10;
     myy := dy;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_UP:
    begin
     mxx := dx;
     myy := dy - 10;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
    end;
   VK_DOWN:
    begin
     mxx := dx;
     myy := dy + 10;
     Refresh;
     FSelected := GetColorAtPoint(mxx, myy);
     FManual := true;
     Invalidate;
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

procedure THSColorPicker.SetHValue(h: integer);
begin
  Clamp(h, 0, 239);
  FHue := h;
  SetSelectedColor(HSLRangeToRGB(FHue, FSaturation, 120));  // why hard-coded 120?
end;

procedure THSColorPicker.SetSValue(s: integer);
begin
  Clamp(s, 0, 240);
  FSaturation := s;
  SetSelectedColor(HSLRangeToRGB(FHue, FSaturation, 120));
end;

end.
