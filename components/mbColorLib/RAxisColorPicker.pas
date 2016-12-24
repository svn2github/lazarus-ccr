unit RAxisColorPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbColorPickerControl;

type
  TRAxisColorPicker = class(TmbColorPickerControl)
  private
    FR, FG, FB: integer;
    dx, dy, mxx, myy: integer;
    procedure SetRValue(r: integer);
    procedure SetGValue(g: integer);
    procedure SetBValue(b: integer);
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
    procedure Resize; override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: Integer): TColor; override;
  published
    property SelectedColor default clRed;
    property Red: integer read FR write SetRValue default 255;
    property Green: integer read FG write SetGValue default 0;
    property Blue: integer read FB write SetBValue default 0;
    property MarkerStyle default msCircle;
    property OnChange;
  end;


implementation

uses
  mbUtils;

{TRAxisColorPicker}

constructor TRAxisColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'G: %g B: %b'#13'Hex: %hex';
  FG := 0;
  FB := 0;
  FR := 255;
  FSelected := clRed;
  FManual := false;
  dx := 0;
  dy := 0;
  mxx := 0;
  myy := 0;
  MarkerStyle := msCircle;
end;

procedure TRAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TRAxisColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure TRAxisColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  CorrectCoords(x, y);
  FR := GetRValue(FSelected);
  FG := GetGValue(FSelected);
  FB := GetBValue(FSelected);
  dx := x;
  dy := y;
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clWhite;
  InternalDrawMarker(x, y, c);
end;

function TRAxisColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  g, b: Integer;
begin
  b := round(x / (Width - 1) * 255);
  g := 255 - round(y / (Height - 1) * 255);
  Result := RGBtoColor(FR, g, b);
end;

{ x is BLUE, y is GREEN }
function TRAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(FR, FBufferBmp.Height - 1 - y, x);
end;

procedure TRAxisColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  delta: Integer;
  eraseKey: Boolean;
begin
  eraseKey := true;
  if ssCtrl in Shift then
    delta := 10
  else
    delta := 1;

  case Key of
    VK_LEFT:
      begin
        mxx := dx - delta;
        if mxx < 0 then mxx := 0;
        myy := dy;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then FOnChange(self);
      end;
    VK_RIGHT:
      begin
        mxx := dx + delta;
        if mxx >= Width then mxx := Width - 1;
        myy := dy;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then FOnChange(self);
      end;
    VK_UP:
      begin
        mxx := dx;
        myy := dy - delta;
        if myy < 0 then myy := 0;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then FOnChange(self);
      end;
    VK_DOWN:
      begin
        mxx := dx;
        myy := dy + delta;
        if myy >= Height then
          myy := Height - 1;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then FOnChange(self);
      end;
    else
      eraseKey := false;
  end;

  if eraseKey then Key := 0;

  inherited;
end;

procedure TRAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  mxx := x;
  myy := y;
  if Button = mbLeft then
  begin
    mxx := x;
    myy := y;
    FSelected := GetColorAtPoint(x, y);
    FManual := true;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(self);
  end;
  SetFocus;
end;

procedure TRAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := x;
    myy := y;
    Clamp(mxx, 0, Width - 1);
    Clamp(myy, 0, Height - 1);
    FSelected := GetColorAtPoint(mxx, myy);
    FManual := true;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(self);
  end;
end;

procedure TRAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := x;
    myy := y;
    Clamp(mxx, 0, Width - 1);
    Clamp(myy, 0, Height - 1);
    FSelected := GetColorAtPoint(mxx, myy);
    FManual := true;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(self);
  end;
end;

procedure TRAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mxx, myy);
end;

procedure TRAxisColorPicker.Resize;
begin
  FManual := false;
  mxx := Round(FB * Width / 255);
  myy := Round((255 - FG) * Height / 255);
  inherited;
end;

procedure TRAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  FB := b;
  SetSelectedColor(RGBtoColor(FR, FG, FB));
end;

procedure TRAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  FG := g;
  SetSelectedColor(RGBtoColor(FR, FG, FB));
end;

procedure TRAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  if FR <> r then
  begin
    FR := r;
    CreateGradient;
    SetSelectedColor(RGBtoColor(FR, FG, FB));
  end;
end;

procedure TRAxisColorPicker.SetSelectedColor(c: TColor);
var
  r, g, b: Integer;
begin
  if WebSafe then c := GetWebSafe(c);
  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  if r <> FR then
    CreateGradient;
  FR := r;
  FG := g;
  FB := b;
  FSelected := c;
  FManual := false;
  mxx := Round(FB * Width / 255);            // BLUE on x
  myy := Round((255 - FG) * Height / 255);   // GREEN on y
  Invalidate;
  if Assigned(FOnChange) then FOnChange(self);
end;

end.
