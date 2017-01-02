unit SLColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Controls, Graphics, Forms,
  mbColorPickerControl;

type
  TSLColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FLum: Double;
    FMaxHue, FMaxSat, FMaxLum: integer;
    procedure DrawMarker(x, y: integer);
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    procedure SetHue(H: integer);
    procedure SetLum(L: integer);
    procedure SetSat(S: integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure UpdateCoords;
  protected
    procedure CorrectCoords(var x, y: integer);
    procedure CreateWnd; override;
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure Paint; override;
    procedure SelectColor(x, y: integer);
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property ColorUnderCursor;
  published
    property Hue: integer read GetHue write SetHue default 0;
    property Saturation: integer read GetSat write SetSat default 0;
    property Luminance: integer read GetLum write SetLum default 240;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 240;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum default 240;
    property SelectedColor default clWhite;
    property MarkerStyle default msCircle;
    property OnChange;
  end;

implementation

uses
  Math,
  ScanLines, RGBHSLUtils, HTMLColors, mbUtils;

{ TSLColorPicker }

constructor TSLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FMaxHue := 359;
  FMaxSat := 240;
  FMaxLum := 240;
  FGradientWidth := FMaxSat + 1;       // x --> Saturation
  FGradientHeight := FMaxLum + 1;      // y --> Luminance
  SetInitialBounds(0, 0, FGradientWidth, FGradientHeight);
  FSelected := clWhite;
  RGBToHSL(FSelected, FHue, FSat, FLum);
  HintFormat := 'S: %hslS L: %l'#13'Hex: %hex';
  MarkerStyle := msCircle;
end;

procedure TSLColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TSLColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
  UpdateCoords;
end;

procedure TSLColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  c := not GetColorAtPoint(x, y);     // "not" --> invert color bits
  InternalDrawMarker(x, y, c);
end;

function TSLColorPicker.GetColorAtPoint(x, y: integer): TColor;
var
  S, L: Double;
begin
  S := x / (Width - 1);
  L := 1.0 - y / (Height - 1);
  Result := HSLToRGB(FHue, S, L);
//  Result := HSLToRGB(FHue, x/(Width - 1), (Height - 1 - y) / (Height - 1));
  if WebSafe then
    Result := GetWebSafe(Result);
end;

{ This picker has Saturation along the X and Luminance along the Y axis.

  NOTE: The HSL conversion (HSLtoColor) seems to be wrong
  but it produces the display seen elsewhere }
function TSLColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
begin
//  Result := HSLtoColor(FHue, x / FMaxSat, (FMaxLum - y) / FMaxLum);   // wrong formula
  Result := HSLtoRGB(FHue, x / FMaxSat, (FMaxLum - y) / FMaxLum);       // correct, but looks wrong...
end;

function TSLColorPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
end;

function TSLColorPicker.GetLum: Integer;
begin
  Result := round(FLum * FMaxLum);
end;

function TSLColorPicker.GetSat: Integer;
begin
  Result := round(FSat * FMaxSat);
end;

procedure TSLColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  delta := IfThen(ssCtrl in Shift, 10, 1);

  case Key of
    VK_LEFT  : SelectColor(mdx - delta, mdy);
    VK_RIGHT : SelectColor(mdx + delta, mdy);
    VK_UP    : SelectColor(mdx, mdy - delta);
    VK_DOWN  : SelectColor(mdx, mdy + delta);
    else       eraseKey := false;
  end;
  {
  case Key of
    VK_LEFT:
      if (mdx - delta >= 0) then
      begin
        Dec(mdx, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        DoChange;
      end;
    VK_RIGHT:
      if (mdx + delta < Width) then
      begin
        Inc(mdx, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        DoChange;
      end;
    VK_UP:
      if (mdy - delta >= 0) then
      begin
        Dec(mdy, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        DoChange;
      end;
    VK_DOWN:
      if (mdy + delta < Height) then
      begin
        Inc(mdy, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        DoChange;
      end;
    else
      eraseKey := false;
  end;
  }

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure TSLColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if (Button = mbLeft) then
    SelectColor(X, Y);
  SetFocus;
end;

procedure TSLColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if (ssLeft in Shift) then
    SelectColor(X, Y);
end;

procedure TSLColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if (Button = mbLeft)then
    SelectColor(X, Y);
end;

procedure TSLColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBMP);
  UpdateCoords;
  DrawMarker(mdx, mdy);
end;

procedure TSLColorPicker.Resize;
begin
  inherited;
  UpdateCoords;
end;

procedure TSLColorPicker.SelectColor(x, y: integer);
var
  S, L: Double;
begin
  CorrectCoords(x, y);
  S := x / (Width - 1);
  L := 1 - y / (Height - 1);
  if (S = FSat) and (L = FLum) then
    exit;

  FSat := S;
  FLum := L;
  FSelected := HSLtoRGB(FHue, FSat, FLum);
  Invalidate;
  UpdateCoords;
  DoChange;
end;

procedure TSLColorPicker.SetHue(H: integer);
begin
  Clamp(H, 0, FMaxHue);
  if GetHue() <> H then
  begin
    FHue := h / FMaxHue;
    FSelected := HSLtoRGB(FHue, FSat, FLum);
    CreateGradient;
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure TSLColorPicker.SetLum(L: integer);
begin
  Clamp(L, 0, FMaxLum);
  if GetLum() <> L then
  begin
    FLum := L / FMaxLum;
    FSelected := HSLtoRGB(FHue, FSat, FLum);
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure TSLColorPicker.SetMaxHue(H: Integer);
begin
  if H = FMaxHue then
    exit;
  FMaxHue := H;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSLColorPicker.SetMaxLum(L: Integer);
begin
  if L = FMaxLum then
    exit;
  FMaxLum := L;
  FGradientHeight := FMaxLum + 1;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSLColorPicker.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FMaxSat := S;
  FGradientWidth := FMaxSat + 1;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSLColorPicker.SetSat(S: integer);
begin
  Clamp(S, 0, FMaxSat);
  if GetSat() <> S then
  begin
    FSat := S / FMaxSat;
    FSelected := HSLtoRGB(FHue, FSat, FLum);
    UpdateCoords;
    Invalidate;
    DoChange;
  end;
end;

procedure TSLColorPicker.SetSelectedColor(c: TColor);
var
  H, S, L: Double;
  needNewGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = GetSelectedColor then
    exit;

  RGBToHSL(c, H, S, L);
//  ColorToHSL(c, H, S, L);
  needNewGradient := (FHue <> H);
  FHue := H;
  FSat := S;
  FLum := L;
  FSelected := c;
  UpdateCoords;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TSLColorPicker.UpdateCoords;
begin
  mdx := round(FSat * (Width - 1));
  mdy := round((1.0 - FLum) * (Height - 1));
end;


end.
