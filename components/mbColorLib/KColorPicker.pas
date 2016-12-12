unit KColorPicker;

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
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors;

type
  TKColorPicker = class(TmbTrackBarPicker)
  private
    FCyan, FMagenta, FYellow, FBlack: integer;
    function ArrowPosFromBlack(k: integer): integer;
    function BlackFromArrowPos(p: integer): integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
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

implementation

uses
  mbUtils;

{TKColorPicker}

constructor TKColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 12;
  FCyan := 0;
  FMagenta := 0;
  FYellow := 0;
  FBlack := 255;
  FArrowPos := ArrowPosFromBlack(255);
  FChange := false;
  Layout := lyVertical;
  SetBlack(255);
  HintFormat := 'Black: %value (selected)';
  FManual := false;
  FChange := true;
end;

function TKColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoTColor(FCyan, FMagenta, FYellow, AValue);
end;

procedure TKColorPicker.SetBlack(k: integer);
begin
  Clamp(k, 0, 255);
  if FBlack <> k then
  begin
    FBlack := k;
    FArrowPos := ArrowPosFromBlack(k);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TKColorPicker.SetMagenta(m: integer);
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

procedure TKColorPicker.SetYellow(y: integer);
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

procedure TKColorPicker.SetCyan(c: integer);
begin
  Clamp(c, 0, 255);
  if FCyan <> c then
  begin
    FCyan := c;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function TKColorPicker.ArrowPosFromBlack(k: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*k);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    k := 255 - k;
    a := Round(((Height - 12)/255)*k);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TKColorPicker.BlackFromArrowPos(p: integer): integer;
var
  k: integer;
begin
  if Layout = lyHorizontal then
    k := Round(p/((Width - 12)/255))
  else
    k := Round(255 - p/((Height - 12)/255));
  Clamp(k, 0, 255);
  Result := k;
end;

function TKColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack)
  else
    Result := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack));
end;

function TKColorPicker.GetSelectedValue: integer;
begin
  Result := FBlack;
end;

procedure TKColorPicker.SetSelectedColor(c: TColor);
var
  cy, m, y, k: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  ColorToCMYK(c, cy, m, y, k);
  FChange := false;
  SetMagenta(m);
  SetYellow(y);
  SetCyan(cy);
  SetBlack(k);
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function TKColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromBlack(FBlack);
end;

procedure TKColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetBlack(FBlack);
    TBA_MouseMove:
      FBlack := BlackFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FBlack := BlackFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FBlack := BlackFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetBlack(FBlack + Increment);
    TBA_WheelDown:
      SetBlack(FBlack - Increment);
    TBA_VKRight:
      SetBlack(FBlack + Increment);
    TBA_VKCtrlRight:
      SetBlack(255);
    TBA_VKLeft:
      SetBlack(FBlack - Increment);
    TBA_VKCtrlLeft:
      SetBlack(0);
    TBA_VKUp:
      SetBlack(FBlack + Increment);
    TBA_VKCtrlUp:
      SetBlack(255);
    TBA_VKDown:
      SetBlack(FBlack - Increment);
    TBA_VKCtrlDown:
      SetBlack(0);
    else
      inherited;
  end;
end;

end.
