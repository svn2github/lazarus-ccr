unit CIELColorPicker;

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
  HTMLColors, RGBCIEUtils, mbColorPickerControl;

type
  TCIELColorPicker = class(TmbColorPickerControl)
  private
    FL, FA, FB: integer;
    dx, dy, mxx, myy: integer;
    procedure SetLValue(l: integer);
    procedure SetAValue(a: integer);
    procedure SetBValue(b: integer);
  protected
    function GetGradientColor2D(x, y: Integer): TColor; override;
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
    procedure CorrectCoords(var x, y: integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SelectedColor default clAqua;
    property LValue: integer read FL write SetLValue default 100;
    property AValue: integer read FA write SetAValue default -128;
    property BValue: integer read FB write SetBValue default 127;
    property MarkerStyle default msCircle;
    property OnChange;
 end;


implementation

uses
  mbUtils;

{TCIELColorPicker}

constructor TCIELColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 256;
  {$IFDEF DELPHI}
  Width := 256;
  Height := 256;
  {$ELSE}
  SetInitialBounds(0, 0, 256, 256);
  {$ENDIF}
  HintFormat := 'A: %cieA B: %cieB'#13'Hex: %hex';
  FSelected := clAqua;
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

procedure TCIELColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

{ Original code: for A ... for B ---> LabToRGB(FL, A - 128, B - 128) }
function TCIELColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := LabToRGB(FL, y - 128, x - 128);
end;

procedure TCIELColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  clamp(y, 0, Height - 1);
end;

procedure TCIELColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  CorrectCoords(x, y);
  FL := Round(GetCIELValue(FSelected));
  FA := Round(GetCIEAValue(FSelected));
  FB := Round(GetCIEBValue(FSelected));
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

procedure TCIELColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FL := Round(GetCIELValue(c));
  FA := Round(GetCIEAValue(c));
  FB := Round(GetCIEBValue(c));
  FSelected := c;
  FManual := false;
  mxx := Round((FA+128)*(Width/255));
  myy := Round((255-(FB+128))*(Height/255));
  CreateGradient;
  Invalidate;
end;

procedure TCIELColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FGradientBmp);
  CorrectCoords(mxx, myy);
  DrawMarker(mxx, myy);
end;

procedure TCIELColorPicker.Resize;
begin
  FManual := false;
  mxx := Round((FA+128)*(Width/255));
  myy := Round((255-(FB+128))*(Height/255));
  inherited;
end;

procedure TCIELColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TCIELColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TCIELColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TCIELColorPicker.CNKeyDown(
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

procedure TCIELColorPicker.SetLValue(l: integer);
begin
  Clamp(L, 0, 100);
  FL := L;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIELColorPicker.SetAValue(a: integer);
begin
  Clamp(A, -128, 127);
  FA := a;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

procedure TCIELColorPicker.SetBValue(b: integer);
begin
  Clamp(b, -128, 127);
  FB := b;
  SetSelectedColor(LabToRGB(FL, FA, FB));
end;

end.
