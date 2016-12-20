unit CIEBColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Controls, Graphics, Math, Forms,
  HTMLColors, RGBCIEUtils, mbColorPickerControl;

type

  { TCIEBColorPicker }

  TCIEBColorPicker = class(TmbColorPickerControl)
  private
    FL, FA, FB: integer;
    dx, dy, mxx, myy: integer;
    procedure SetLValue(l: integer);
    procedure SetAValue(a: integer);
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
  published
    property AValue: integer read FA write SetAValue default -128;
    property BValue: integer read FB write SetBValue default 127;
    property LValue: integer read FL write SetLValue default 100;
    property MarkerStyle default msCircle;
    property SelectedColor default clLime;
    property OnChange;
  end;


implementation

uses
  mbUtils;

{TCIEBColorPicker}

constructor TCIEBColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  SetInitialBounds(0, 0, 256, 256);
  HintFormat := 'L: %cieL A: %cieA'#13'Hex: %hex';
  FSelected := clLime;
  FL := 100;
  FA := -128;
  FB := 127;
  FManual := false;
  dx := 0;
  dy := 0;
  mxx := 0;
  myy := 0;
  MarkerStyle := msCircle;
end;

procedure TCIEBColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TCIEBColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

procedure TCIEBColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  CorrectCoords(x, y);
  FL := Round(GetCIELValue(FSelected));
  FA := Round(GetCIEAValue(FSelected));
  FB := Round(GetCIEBValue(FSelected));
  dx := x;
  dy := y;
  if Focused or (csDesigning in ComponentState) then
    c := clBlack
  else
    c := clWhite;
  InternalDrawMarker(x, y, c);
end;

{ In the original code: for L ... for A ... LabToRGB(Round(100-L*100/244), A-128, FB)
  --> x is A, y is L}
function TCIEBColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := LabToRGB(Round(100 - y*100/255), x - 128, FB);
end;

procedure TCIEBColorPicker.KeyDown(var Key: Word; Shift: TShiftState);
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
        if myy < 0 then myy := 0;
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
        if myy >= Width then myy := Width - 1;
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

procedure TCIEBColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TCIEBColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    {$IFDEF DELPHI}
    ClipCursor(nil);
    {$ENDIF}
    mxx := x;
    myy := y;
    FSelected := GetColorAtPoint(x, y);
    FManual := true;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TCIEBColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    mxx := x;
    myy := y;
    FSelected := GetColorAtPoint(x, y);
    FManual := true;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TCIEBColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  CorrectCoords(mxx, myy);
  DrawMarker(mxx, myy);
end;

procedure TCIEBColorPicker.Resize;
begin
  FManual := false;
  mxx := Round((FA + 128) * (Width / 255));
  myy := Round(((100 - FL) * 255 / 100) * (Height / 255));
  inherited;
end;

procedure TCIEBColorPicker.SetAValue(a: integer);
begin
  Clamp(a, -128, 127);
  FA := a;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEBColorPicker.SetBValue(b: integer);
begin
  Clamp(b, -128, 127);
  FB := b;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEBColorPicker.SetLValue(L: integer);
begin
  Clamp(L, 0, 100);
  FL := L;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIEBColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FL := Round(GetCIELValue(c));
  FA := Round(GetCIEAValue(c));
  FB := Round(GetCIEBValue(c));
  FSelected := c;
  FManual := false;
  mxx := Round((FA + 128) * Width / 255);
  myy := Round((100 - FL) * 255 / 100* Height / 255);
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
