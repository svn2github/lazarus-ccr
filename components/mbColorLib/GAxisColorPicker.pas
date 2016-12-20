unit GAxisColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLType, LCLIntf, LMessages, SysUtils, Classes, Controls, Graphics, Math, Forms,
  HTMLColors, mbColorPickerControl;

type
  TGAxisColorPicker = class(TmbColorPickerControl)
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
    function GetColorAtPoint(x, y: Integer): TColor; override;
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
  published
    property SelectedColor default clLime;
    property RValue: integer read FR write SetRValue default 0;
    property GValue: integer read FG write SetGValue default 255;
    property BValue: integer read FB write SetBValue default 0;
    property MarkerStyle default msCircle;
    property OnChange;
  end;


implementation

uses
  mbUtils;

{TGAxisColorPicker}

constructor TGAxisColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'R: %r B: %b'#13'Hex: %hex';
  FG := 255;
  FB := 0;
  FR := 0;
  FSelected := clLime;
  FManual := false;
  dx := 0;
  dy := 0;
  mxx := 0;
  myy := 0;
  MarkerStyle := msCircle;
end;

procedure TGAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width-1);
  Clamp(y, 0, Height-1);
end;

procedure TGAxisColorPicker.CreateWnd;
begin
 inherited;
 CreateGradient;
end;

procedure TGAxisColorPicker.DrawMarker(x, y: integer);
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

function TGAxisColorPicker.GetColorAtPoint(x, y: Integer): TColor;
var
  r, b: Integer;
begin
  b := round(x / (Width - 1) * 255);
  r := 255 - round(y / (Height - 1) * 255);
  Result := RGBtoColor(r, FG, b);
end;

// x is BLUE, y is RED
function TGAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(FBufferBmp.Height - 1 - y, FG, x);
end;

procedure TGAxisColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
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
        if Assigned(FOnChange) then
          FOnChange(Self);
      end;
    VK_RIGHT:
      begin
        mxx := dx + delta;
        myy := dy;
        if mxx >= Width then mxx := Width - 1;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then
          FOnChange(Self);
      end;
    VK_UP:
      begin
        mxx := dx;
        myy := dy - delta;
        if myy < 0 then myy := 0;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then
          FOnChange(Self);
      end;
    VK_DOWN:
      begin
        mxx := dx;
        myy := dy + delta;
        if myy >= Height then myy := Height - 1;
        FSelected := GetColorAtPoint(mxx, myy);
        FManual := true;
        Invalidate;
        if Assigned(FOnChange) then
          FOnChange(Self);
      end;
  else
    eraseKey := false;
  end;

  if eraseKey then Key := 0;
  inherited;
end;

procedure TGAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  mxx := x;
  myy := y;
  if Button = mbLeft then
  begin
    FSelected := GetColorAtPoint(x, y);
    FManual := true;
    Invalidate;
  end;
  SetFocus;
end;

procedure TGAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := X;
    myy := Y;
    FSelected := GetColorAtPoint(X, Y);
    FManual := true;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TGAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := X;
    myy := Y;
    FSelected := GetColorAtPoint(X, Y);
    FManual := true;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TGAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  CorrectCoords(mxx, myy);
  DrawMarker(mxx, myy);
end;

procedure TGAxisColorPicker.Resize;
begin
  FManual := false;
  myy := Round((255 - FR) * Height / 255);
  mxx := Round(FB * Width / 255);
  inherited;
end;

procedure TGAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  FB := b;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TGAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  FG := g;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TGAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  FR := r;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TGAxisColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FR := GetRValue(c);
  FG := GetGValue(c);
  FB := GetBValue(c);
  FSelected := c;
  FManual := false;
  mxx := Round(FB * Width / 255);            // BLUE is x
  myy := Round((255 - FR) * Height / 255);   // RED is y
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
