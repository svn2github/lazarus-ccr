unit HSColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$DEFINE USE COLOR_TO_RGB}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  RGBHSLUtils, HTMLColors, mbColorPickerControl;

type

  { THSColorPicker }

  THSColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FLum, FLumSel: Double;
    FMaxHue, FMaxSat, FMaxLum: Integer;
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
    procedure CreateWnd; override;
    procedure DrawMarker(x, y: integer);
    function GetGradientColor2D(x, y: Integer): TColor; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function PredictColor: TColor;
    procedure Resize; override;
    procedure SelectColor(x, y: Integer);
    procedure SetSelectedColor(c: TColor); override;
    procedure UpdateCoords;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
  published
    property SelectedColor default clRed;
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 240;
    property Luminance: Integer read GetLum write SetLum default 120;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 240;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum default 240;
    property MarkerStyle default msCross;
    property OnChange;
  end;

implementation

uses
  math, mbUtils;

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
  FHue := 0;
  FSat := 1.0;
  FLum := 0.5;
  FLumSel := 0.5;
  FSelected := clRed;
  CreateGradient;
  HintFormat := 'H: %h S: %hslS'#13'Hex: %hex';
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

  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clWhite;
  InternalDrawMarker(x, y, c);
end;

function THSColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  H, S: Double;
begin
  if InRange(x, 0, Width - 1) and InRange(y, 0, Height - 1) then
  begin
    H := x / (Width - 1);
    S := 1 - y / (Height - 1);
    {$IFDEF USE_COLOR_TO_RGB}
    Result := HSLToColor(H, S, FLumSel);
    {$ELSE}
    Result := HSLToRGB(H, S, FLumSel);
    {$ENDIF}
  end else
    Result := clNone;
end;

function THSColorPicker.GetGradientColor2D(x, y: Integer): TColor;
var
  H, S: Double;
begin
  H := x / FMaxHue;
  S := 1 - y / FMaxSat;
  {$IFDEF USE_COLOR_TO_RGB}
  Result := HSLToColor(H, S, FLum);
  {$ELSE}
  Result := HSLtoRGB(H, S, FLum);
  {$ENDIF}
end;

function THSColorPicker.GetHue: Integer;
begin
  Result := Round(FHue * (FMaxHue + 1));
end;

function THSColorPicker.GetLum: Integer;
begin
  Result := Round(FLum * FMaxLum);
end;

function THSColorPicker.GetSat: Integer;
begin
  Result := Round(FSat * FMaxSat);
end;

function THSColorPicker.GetSelectedColor: TColor;
begin
  {$IFDEF USE_COLOR_TO_RGB}
  Result := HSLToColor(FHue, FSat, FLumSel);
  {$ELSE}
  Result := HSLtoRGB(FHue, FSat, FLumSel);
  {$ENDIF}
end;

procedure THSColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  delta := IfThen(ssCtrl in Shift, 10, 1);

  case Key of
    VK_LEFT  : SelectColor(mx - delta, my);
    VK_RIGHT : SelectColor(mx + delta, my);
    VK_UP    : SelectColor(mx, my - delta);
    VK_DOWN  : SelectColor(mx, my + delta);
    else       eraseKey := false;
  end;
  {
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
  }

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure THSColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
  SetFocus;
end;

procedure THSColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    SelectColor(x, y);
end;

procedure THSColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    SelectColor(x, y);
end;

procedure THSColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mx, my);
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

procedure THSColorPicker.SelectColor(x, y: Integer);
var
  H, S, L: Double;
  c: TColor;
begin
  CorrectCoords(x, y);
  mx := x;
  my := y;
  c := GetColorAtPoint(x, y);
  if WebSafe then c := GetWebSafe(c);
  {$IFDEF USE_COLOR_TO_RGB}
  ColorToHSL(c, H, S, L);
  {$ELSE}
  RGBtoHSL(c, H, S, L);
  {$ENDIF}

  if (H = FHue) and (S = FSat) then
    exit;

  FHue := H;
  FSat := S;
  {$IFDEF USE_COLOR_TO_RGB}
  FSelected := ColorToHSL(FHue, FSat, FLumSel);
  {$ELSE}
  FSelected := HSLToRGB(FHue, FSat, FLumSel);
  {$ENDIF}

  Invalidate;
  DoChange;
end;

(*
  BeginUpdate;
  try
    mxx := x;
    myy := y;
    CorrectCoords(mxx, myy);
    c := GetColorAtPoint(mxx, myy);
    if WebSafe then c := GetWebSafe(c);
    {$IFDEF USE_COLOR_TO_RGB}
    ColorToHSL(c, FHue, FSat, L);
    {$ELSE}
    RGBtoHSL(c, FHue, FSat, L);
    {$ENDIF}
    FSelected := c;
    FManual := false;
    Invalidate;
  finally
    EndUpdate;
  end;
end;
*)

procedure THSColorPicker.SetHue(H: integer);
begin
  Clamp(H, 0, FMaxHue);
  if H = GetHue then
    exit;

  FHue := H / (FMaxHue + 1);
  {$IFDEF USE_COLOR_TO_RGB}
  FSelected := HSLtoColor(FHue, FSat, FLumSel);
  {$ELSE}
  FSelected := HSLToRGB(FHue, FSat, FLumSel);
  {$ENDIF}
  UpdateCoords;
  Invalidate;
  DoChange;
(*
  {$IFDEF USE_COLOR_TO_RGB}
  SetSelectedColor(HSLtoColor(FHue, FSat, FLumSel));
  {$ELSE}
  SetSelectedColor(HSLToRGB(FHue, FSat, FLumSel));
  {$ENDIF}
  *)
end;

// Sets the luminance value used for the display. It is not necessarily that
// of the selected color.
// The true luminance of the selected color is given by LumSel
procedure THSColorPicker.SetLum(L: Integer);
begin
  Clamp(L, 0, FMaxLum);
  if L = GetLum then
    exit;

  FLum := L / FMaxLum;
  CreateGradient;
  Invalidate;
  DoChange;
end;

procedure THSColorPicker.SetSat(S: integer);
begin
  Clamp(S, 0, FMaxSat);
  if S = GetSat then
    exit;

  FSat := S / FMaxSat;
  FSelected := HSLToRGB(FHue, FSat, FLumSel);
  UpdateCoords;
  Invalidate;
  DoChange;
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

// NOTE: In the picker display only the hue and the saturation of the input
// color are used, the luminance is replaced by the preset value of the picker.
// --> The selected color in the üicker display in general is different from the
//     input color.
procedure THSColorPicker.SetSelectedColor(c: TColor);
var
  H, S, L: Double;
begin
  if WebSafe then
    c := GetWebSafe(c);

  {$IFDEF USE_COLOR_TO_RGB}
  ColorToHSL(c, H, S, L);
  {$ELSE}
  RGBtoHSL(c, H, S, L);
  {$ENDIF}

  FSelected := c;
  if (H = FHue) and (S = FSat) then
    exit;

  FHue := H;
  FSat := S;
  FLumSel := L;

  UpdateCoords;
  Invalidate;
  DoChange;
end;

procedure THSCOlorPicker.UpdateCoords;
begin
  mx := Round(FHue * Width);
  my := Round((1.0 - FSat) * Height);
  CorrectCoords(mx, my);
end;

end.
