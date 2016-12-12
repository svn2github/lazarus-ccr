unit HColorPicker;

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
  RGBHSVUtils, mbTrackBarPicker, HTMLColors;

type
  THColorPicker = class(TmbTrackBarPicker)
  private
    FVal, FSat, FHue: integer;
    function ArrowPosFromHue(h: integer): integer;
    function HueFromArrowPos(p: integer): integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetValue(v: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Hue: integer read FHue write SetHue default 0;
    property Saturation: integer read FSat write SetSat default 255;
    property Value: integer read FVal write SetValue default 255;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  end;


implementation

uses
  mbUtils;

{THColorPicker}

constructor THColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 360;
  FGradientHeight := 12;
  FSat := 255;
  FVal := 255;
  FArrowPos := ArrowPosFromHue(0);
  FChange := false;
  SetHue(0);
  HintFormat := 'Hue: %value (selected)';
  FManual := false;
  FChange := true;
end;

function THColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  if Layout = lyVertical then AValue := 360 - AValue;
  Result := HSVtoColor(AValue, FSat, FVal);
end;

procedure THColorPicker.SetValue(v: integer);
begin
  Clamp(v, 0, 255);
  if FVal <> v then
  begin
    FVal := v;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure THColorPicker.SetHue(h: integer);
begin
  Clamp(h, 0, 360);
  if FHue <> h then
  begin
    FHue := h;
    FArrowPos := ArrowPosFromHue(h);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure THColorPicker.SetSat(s: integer);
begin
  Clamp(s, 0, 255);
  if FSat <> s then
  begin
    FSat := s;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function THColorPicker.ArrowPosFromHue(h: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/360)*h);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round(((Height - 12)/360)*h);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function THColorPicker.HueFromArrowPos(p: integer): integer;
var
  r: integer;
begin
  if Layout = lyHorizontal then
    r := Round(p/((Width - 12)/360))
  else
    r := Round(p/((Height - 12)/360));
  Clamp(r, 0, 360);
  Result := r;
end;

function THColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := HSVtoColor(FHue, FSat, FVal)
  else
    Result := GetWebSafe(HSVtoColor(FHue, FSat, FVal));
end;

function THColorPicker.GetSelectedValue: integer;
begin
  Result := FHue;
end;

procedure THColorPicker.SetSelectedColor(c: TColor);
var
  h, s, v: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  RGBToHSV(GetRValue(c), GetGValue(c), GetBValue(c), h, s, v);
  FChange := false;
  SetHue(h);
  SetSat(s);
  SetValue(v);
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function THColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromHue(FHue);
end;

procedure THColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetHue(FHue);
    TBA_MouseMove:
      FHue := HueFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FHue := HueFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FHue := HueFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetHue(FHue + Increment);
    TBA_WheelDown:
      SetHue(FHue - Increment);
    TBA_VKLeft:
      SetHue(FHue - Increment);
    TBA_VKCtrlLeft:
      SetHue(0);
    TBA_VKRight:
      SetHue(FHue + Increment);
    TBA_VKCtrlRight:
      SetHue(360);
    TBA_VKUp:
      SetHue(FHue - Increment);
    TBA_VKCtrlUp:
      SetHue(0);
    TBA_VKDown:
      SetHue(FHue + Increment);
    TBA_VKCtrlDown:
      SetHue(360);
    else
      inherited;
 end;
end;

end.
