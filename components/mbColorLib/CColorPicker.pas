unit CColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors;

type
  TCColorPicker = class(TmbTrackBarPicker)
  private
    FCyan, FMagenta, FYellow, FBlack: integer;
    function ArrowPosFromCyan(c: integer): integer;
    function CyanFromArrowPos(p: integer): integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(clr: TColor);
    procedure SetCyan(c: integer);
    procedure SetMagenta(m: integer);
    procedure SetYellow(y: integer);
    procedure SetBlack(k: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Cyan: integer read FCyan write SetCyan default 255;
    property Magenta: integer read FMagenta write SetMagenta default 0;
    property Yellow: integer read FYellow write SetYellow default 0;
    property Black: integer read FBlack write SetBlack default 0;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
    property Layout default lyVertical;
  end;

procedure Register;

implementation

{$IFDEF FPC}
  {$R CColorPicker.dcr}
{$ENDIF}

uses
  mbUtils;

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TCColorPicker]);
end;

{TCColorPicker}

constructor TCColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 12;
  SetInitialBounds(0, 0, 22, 267);
  //Width := 22;
  //Height := 267;
  Layout := lyVertical;
  FCyan := 255;
  FMagenta := 0;
  FYellow := 0;
  FBlack := 0;
  FArrowPos := ArrowPosFromCyan(255);
  FChange := false;
  SetCyan(255);
  HintFormat := 'Cyan: %value';
  FManual := false;
  FChange := true;
end;

function TCColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoTColor(AValue, FMagenta, FYellow, FBlack);
end;

procedure TCColorPicker.SetCyan(C: integer);
begin
  Clamp(c, 0, 255);
  if FCyan <> c then
  begin
    FCyan := c;
    FArrowPos := ArrowPosFromCyan(c);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TCColorPicker.SetMagenta(m: integer);
begin
  Clamp(m, 0, 255);
  if FMagenta <> m then
  begin
    FMagenta := m;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TCColorPicker.SetYellow(y: integer);
begin
  Clamp(y, 0, 255);
  if FYellow <> y then
  begin
    FYellow := y;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TCColorPicker.SetBlack(k: integer);
begin
  Clamp(k, 0, 255);
  if FBlack <> k then
  begin
    FBlack := k;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function TCColorPicker.ArrowPosFromCyan(c: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*c);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    c := 255 - c;
    a := Round(((Height - 12)/255)*c);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TCColorPicker.CyanFromArrowPos(p: integer): integer;
var
  c: integer;
begin
  if Layout = lyHorizontal then
    c := Round(p/((Width - 12)/255))
  else
    c := Round(255 - p/((Height - 12)/255));
  Clamp(c, 0, 255);
  Result := c;
end;

function TCColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack)
  else
    Result := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack));
end;

function TCColorPicker.GetSelectedValue: integer;
begin
  Result := FCyan;
end;

procedure TCColorPicker.SetSelectedColor(clr: TColor);
var
  c, m, y, k: integer;
begin
  if WebSafe then clr := GetWebSafe(clr);
  ColorToCMYK(clr, c, m, y, k);
  FChange := false;
  SetMagenta(m);
  SetYellow(y);
  SetBlack(k);
  SetCyan(c);
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function TCColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromCyan(FCyan);
end;

procedure TCColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetCyan(FCyan);
    TBA_MouseMove:
      FCyan := CyanFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FCyan := CyanFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FCyan := CyanFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetCyan(FCyan + Increment);
    TBA_WheelDown:
      SetCyan(FCyan - Increment);
    TBA_VKRight:
      SetCyan(FCyan + Increment);
    TBA_VKCtrlRight:
      SetCyan(255);
    TBA_VKLeft:
      SetCyan(FCyan - Increment);
    TBA_VKCtrlLeft:
      SetCyan(0);
    TBA_VKUp:
      SetCyan(FCyan + Increment);
    TBA_VKCtrlUp:
      SetCyan(255);
    TBA_VKDown:
      SetCyan(FCyan - Increment);
    TBA_VKCtrlDown:
      SetCyan(0);
    else
      inherited;
  end;
end;

end.
