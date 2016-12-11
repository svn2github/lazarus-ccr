unit RAxisColorPicker;

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
    property SelectedColor default clRed;
    property RValue: integer read FR write SetRValue default 255;
    property GValue: integer read FG write SetGValue default 0;
    property BValue: integer read FB write SetBValue default 0;
    property MarkerStyle default msCircle;
    property OnChange;
  end;

procedure Register;

implementation

{$IFDEF FPC}
  {$R RAxisColorPicker.dcr}
{$ENDIF}

uses
  mbUtils;

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TRAxisColorPicker]);
end;

{TRAxisColorPicker}

constructor TRAxisColorPicker.Create(AOwner: TComponent);
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

procedure TRAxisColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

{ x is BLUE, y is GREEN }
function TRAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(FR, FGradientBmp.Height - 1 - y, x);
end;

procedure TRAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TRAxisColorPicker.DrawMarker(x, y: integer);
var
  c: TColor;
begin
  CorrectCoords(x, y);
  FR := GetRValue(FSelected);
  FG := GetGValue(FSelected);
  FB := GetBValue(FSelected);
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

procedure TRAxisColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FR := GetRValue(c);
  FG := GetGValue(c);
  FB := GetBValue(c);
  FSelected := c;
  FManual := false;
  myy := Round((255-FG)*(Height/255));
  mxx := Round(FB*(Width/255));
  CreateGradient;
  Invalidate;
end;

procedure TRAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FGradientBmp);
  CorrectCoords(mxx, myy);
  DrawMarker(mxx, myy);
end;

procedure TRAxisColorPicker.Resize;
begin
  FManual := false;
  myy := Round((255-FG)*(Height/255));
  mxx := Round(FB*(Width/255));
  inherited;
end;

procedure TRAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TRAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TRAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TRAxisColorPicker.CNKeyDown(
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

procedure TRAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  FR := r;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TRAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  FG := g;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TRAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  FB := b;
  SetSelectedColor(RGB(FR, FG, FB));
end;

end.
