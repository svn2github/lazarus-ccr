unit SLColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Controls, Graphics, Math, Forms,
  mbColorPickerControl;

type
  TSLColorPicker = class(TmbColorPickerControl)
  private
    FHue, FSat, FLum: Double;
    FMaxHue, FMaxSat, FMaxLum: integer;
    //FChange: boolean;
    procedure DrawMarker(x, y: integer);
    procedure SelectionChanged(x, y: integer);
    procedure UpdateCoords;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    procedure SetHue(H: integer);
    procedure SetSat(S: integer);
    procedure SetLum(L: integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
  protected
    function GetGradientColor2D(X, Y: Integer): TColor; override;
    function GetSelectedColor: TColor; override;
    procedure SetSelectedColor(c: TColor); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; override;
    property ColorUnderCursor;
  published
    property Hue: integer read GetHue write SetHue;
    property Saturation: integer read GetSat write SetSat;
    property Luminance: integer read GetLum write SetLum;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 240;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum default 240;
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
  FMaxHue := 359;
  FMaxSat := 240;
  FMaxLum := 240;
  FGradientWidth := FMaxSat + 1;       // x --> Saturation
  FGradientHeight := FMaxLum + 1;      // y --> Luminance
  SetInitialBounds(0, 0, FGradientWidth, FGradientHeight);
  FHue := 0.0;
  FSat := 0.0;
  FLum := 1.0;
  FChange := true;
  MarkerStyle := msCircle;
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
begin
  Result := HSLToRGB(FHue, x/(Width - 1), (Height - 1 - y) / (Height - 1));
  if WebSafe then
    Result := GetWebSafe(Result);
end;

{ This picker has Saturation along the X and Luminance along the Y axis. }
function TSLColorPicker.GetGradientColor2D(X, Y: Integer): TColor;
begin
  Result := HSLtoColor(FHue, x / FMaxSat, (FMaxLum - y) / FMaxLum);
//  Result := HSLtoRGB(FHue, x / FMaxSat, (FMaxLum - y) / FMaxLum);
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

function TSLColorPicker.GetSelectedColor: TColor;
begin
  Result := HSLtoRGB(FHue, FSat, FLum);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

procedure TSLColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
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
      if (mdx - delta >= 0) then
      begin
        Dec(mdx, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    VK_RIGHT:
      if (mdx + delta < Width) then
      begin
        Inc(mdx, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    VK_UP:
      if (mdy - delta >= 0) then
      begin
        Dec(mdy, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    VK_DOWN:
      if (mdy + delta < Height) then
      begin
        Inc(mdy, delta);
        SelectionChanged(mdx, mdy);
        FManual := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end;
    else
      eraseKey := false;
  end;

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
  if (Button = mbLeft) and PtInRect(ClientRect, Point(x, y)) then
  begin
    mdx := x;
    mdy := y;
    SelectionChanged(X, Y);
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
  end;
end;

procedure TSLColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
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

procedure TSLColorPicker.SelectionChanged(x, y: integer);
begin
  FChange := false;
  FSat := x / (Width - 1);
  FLum := (Height - y - 1) / (Height - 1);
  FManual := false;
  UpdateCoords;
  Invalidate;
  if FChange and Assigned(FOnChange) then FOnChange(Self);
  FChange := true;
end;

procedure TSLColorPicker.SetHue(H: integer);
begin
  Clamp(H, 0, FMaxHue);
  if GetHue() <> H then
  begin
    FHue := h / FMaxHue;
    FManual := false;
    CreateGradient;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetLum(L: integer);
begin
  Clamp(L, 0, FMaxLum);
  if GetLum() <> L then
  begin
    FLum := L / FMaxLum;
    FManual := false;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetMaxHue(H: Integer);
begin
  if H = FMaxHue then
    exit;
  FMaxHue := H;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSLColorPicker.SetMaxLum(L: Integer);
begin
  if L = FMaxLum then
    exit;
  FMaxLum := L;
  FGradientHeight := FMaxLum + 1;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSLColorPicker.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FMaxSat := S;
  FGradientWidth := FMaxSat + 1;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSLColorPicker.SetSat(S: integer);
begin
  Clamp(S, 0, FMaxSat);
  if GetSat() <> S then
  begin
    FSat := S / FMaxSat;
    FManual := false;
    UpdateCoords;
    Invalidate;
    if FChange and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetSelectedColor(c: TColor);
var
  h, s, l: Double;
begin
  if WebSafe then c := GetWebSafe(c);
  FManual := false;
  FChange := false;
  ColorToHSL(c, FHue, FSat, FLum);
  FManual := false;
  UpdateCoords;
  Invalidate;
  if FChange and Assigned(FOnChange) then FOnChange(Self);
  FChange := true;
end;

procedure TSLColorPicker.UpdateCoords;
begin
  mdx := round(FSat * (Width - 1));
  mdy := round((1.0 - FLum) * (Height - 1));
end;


end.
