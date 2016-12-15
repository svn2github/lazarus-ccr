unit YColorPicker;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
  TYColorPicker = class(TmbTrackBarPicker)
  private
    FYellow, FMagenta, FCyan, FBlack: integer;
    function ArrowPosFromYellow(y: integer): integer;
    function YellowFromArrowPos(p: integer): integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
    procedure SetYellow(y: integer);
    procedure SetMagenta(m: integer);
    procedure SetCyan(c: integer);
    procedure SetBlack(k: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Yellow: integer read FYellow write SetYellow default 255;
    property Magenta: integer read FMagenta write SetMagenta default 0;
    property Cyan: integer read FCyan write SetCyan default 0;
    property Black: integer read FBlack write SetBlack default 0;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
    property Layout default lyVertical;
  end;

implementation

uses
  mbUtils;

{TYColorPicker}

constructor TYColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 255;
  FGradientHeight := 12;
  FYellow := 255;
  FMagenta := 0;
  FCyan := 0;
  FBlack := 0;
  FArrowPos := ArrowPosFromYellow(255);
  FChange := false;
  Layout := lyVertical;
  SetYellow(255);
  HintFormat := 'Yellow: %value (selected)';
  FManual := false;
  FChange := true;
end;

function TYColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoTColor(FCyan, FMagenta, AValue, FBlack);
end;

procedure TYColorPicker.SetYellow(y: integer);
begin
  Clamp(y, 0, 255);
  if FYellow <> y then
  begin
    FYellow := y;
    FArrowPos := ArrowPosFromYellow(y);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TYColorPicker.SetMagenta(m: integer);
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

procedure TYColorPicker.SetCyan(c: integer);
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

procedure TYColorPicker.SetBlack(k: integer);
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

function TYColorPicker.ArrowPosFromYellow(y: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*y);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    y := 255 - y;
    a := Round(((Height - 12)/255)*y);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TYColorPicker.YellowFromArrowPos(p: integer): integer;
var
 r: integer;
begin
  if Layout = lyHorizontal then
    r := Round(p/((Width - 12)/255))
  else
    r := Round(255 - p/((Height - 12)/255));
  Clamp(r, 0, 255);
  Result := r;
end;

function TYColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack)
  else
    Result := GetWebSafe(CMYKtoTColor(FCyan, FMagenta, FYellow, FBlack));
end;

function TYColorPicker.GetSelectedValue: integer;
begin
  Result := FYellow;
end;

procedure TYColorPicker.SetSelectedColor(c: TColor);
var
  cy, m, y, k: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  ColorToCMYK(c, cy, m, y, k);
  FChange := false;
  SetMagenta(m);
  SetCyan(cy);
  SetBlack(k);
  SetYellow(y);
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function TYColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromYellow(FYellow);
end;

procedure TYColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetYellow(FYellow);
    TBA_MouseMove:
      FYellow := YellowFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FYellow := YellowFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FYellow := YellowFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetYellow(FYellow + Increment);
    TBA_WheelDown:
      SetYellow(FYellow - Increment);
    TBA_VKRight:
      SetYellow(FYellow + Increment);
    TBA_VKCtrlRight:
      SetYellow(255);
    TBA_VKLeft:
      SetYellow(FYellow - Increment);
    TBA_VKCtrlLeft:
      SetYellow(0);
    TBA_VKUp:
      SetYellow(FYellow + Increment);
    TBA_VKCtrlUp:
      SetYellow(255);
    TBA_VKDown:
      SetYellow(FYellow - Increment);
    TBA_VKCtrlDown:
      SetYellow(0);
    else
      inherited;
  end;
end;

end.
