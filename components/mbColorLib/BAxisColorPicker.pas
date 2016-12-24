unit BAxisColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics,
  HTMLColors, mbColorPickerControl;

type
  TBAxisColorPicker = class(TmbColorPickerControl)
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetSelectedColor(c: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: Integer): TColor; override;
  published
    property SelectedColor default clBlue;
    property Red: integer read FR write SetRValue default 0;
    property Green: integer read FG write SetGValue default 0;
    property Blue: integer read FB write SetBValue default 255;
    property MarkerStyle default msCircle;
    property OnChange;
  end;


implementation

uses
  mbUtils;


{TBAxisColorPicker}

constructor TBAxisColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 255, 255);
  HintFormat := 'R: %r G: %g'#13'Hex: %hex';
  FG := 0;
  FB := 255;
  FR := 0;
  FSelected := clBlue;
  FManual := false;
  dx := 0;
  dy := 0;
  mxx := 0;
  myy := 0;
  MarkerStyle := msCircle;
end;

procedure TBAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TBAxisColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure TBAxisColorPicker.DrawMarker(x, y: integer);
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

function TBAxisColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  r, g: Integer;
begin
  r := round(x / (Width - 1) * 255);
  g := 255 - round(y / (Height - 1) * 255);
  Result := RGBtoColor(r, g, FB);
end;

{ x is RED, y is GREEN }
function TBAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(x, FBufferBmp.Height - 1 - y, FB);
end;

procedure TBAxisColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
  delta: Integer;
begin
  eraseKey := true;
  if (ssCtrl in Shift) then delta := 10 else delta := 1;

  case Key of
    VK_LEFT:
      begin
        mxx := dx - delta;
        myy := dy;
        if mxx < 0 then mxx := 0;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then FOnChange(self);
      end;
    VK_RIGHT:
      begin
        mxx := dx + delta;
        myy := dy;
        if mxx >= Width then mxx := Width - 1;
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
        if myy >= Height then myy := Height - 1;
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

procedure TBAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
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

procedure TBAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TBAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TBAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  DrawMarker(mxx, myy);
end;

procedure TBAxisColorPicker.Resize;
begin
  FManual := false;
  mxx := round(FR * Width / 255);
  myy := round((255 - FG) * Height / 255);
  inherited;
end;

procedure TBAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  if b <> FB then
  begin
    FB := b;
    CreateGradient;
    SetSelectedColor(RGB(FR, FG, FB));
  end;
end;

procedure TBAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  FG := g;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TBAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  FR := r;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TBAxisColorPicker.SetSelectedColor(c: TColor);
var
  r, g, b: Integer;
begin
  if WebSafe then c := GetWebSafe(c);
  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  if b <> FB then
    CreateGradient;
  FR := r;
  FG := g;
  FB := b;
  FSelected := c;
  FManual := true;
  mxx := Round(FR * Width / 255);                  // RED is on x
  myy := Round((255 - FG) * Height / 255);         // GREEN is on y
  Invalidate;
  if Assigned(FOnChange) then FOnChange(self);
end;


end.
