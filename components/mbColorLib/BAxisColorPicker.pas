unit BAxisColorPicker;

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
  TBAxisColorPicker = class(TmbColorPickerControl)
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
    property SelectedColor default clBlue;
    property RValue: integer read FR write SetRValue default 0;
    property GValue: integer read FG write SetGValue default 0;
    property BValue: integer read FB write SetBValue default 255;
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
  {$IFDEF DELPHI}
  Width := 256;
  Height := 256;
  {$ELSE}
  SetInitialBounds(0, 0, 255, 255);
  {$ENDIF}
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

procedure TBAxisColorPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

{ x is RED, y is GREEN }
function TBAxisColorPicker.GetGradientColor2D(x, y: Integer): TColor;
begin
  Result := RGB(x, FBufferBmp.Height - 1 - y, FB);
end;

procedure TBAxisColorPicker.CorrectCoords(var x, y: integer);
begin
  Clamp(x, 0, Width - 1);
  Clamp(y, 0, Height - 1);
end;

procedure TBAxisColorPicker.DrawMarker(x, y: integer);
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

procedure TBAxisColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FR := GetRValue(c);
  FG := GetGValue(c);
  FB := GetBValue(c);
  FSelected := c;
  FManual := false;
  mxx := Round(FR*(Width/255));
  myy := Round((255-FG)*(Height/255));
  CreateGradient;
  Invalidate;
end;

procedure TBAxisColorPicker.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBufferBmp);
  CorrectCoords(mxx, myy);
  DrawMarker(mxx, myy);
end;

procedure TBAxisColorPicker.Resize;
begin
  FManual := false;
  mxx := round(FR * (Width / 255));
  myy := round((255 - FG) * (Height / 255));
  inherited;
end;

procedure TBAxisColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
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

procedure TBAxisColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TBAxisColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TBAxisColorPicker.CNKeyDown(
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

procedure TBAxisColorPicker.SetRValue(r: integer);
begin
  Clamp(r, 0, 255);
  FR := r;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TBAxisColorPicker.SetGValue(g: integer);
begin
  Clamp(g, 0, 255);
  FG := g;
  SetSelectedColor(RGB(FR, FG, FB));
end;

procedure TBAxisColorPicker.SetBValue(b: integer);
begin
  Clamp(b, 0, 255);
  FB := b;
  SetSelectedColor(RGB(FR, FG, FB));
end;

end.
