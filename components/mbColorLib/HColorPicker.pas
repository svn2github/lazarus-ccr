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
    FVal, FSat, FHue: double;
    FMaxVal, FMaxSat, FMaxHue: Integer;
    function ArrowPosFromHue(h: integer): integer;
    function HueFromArrowPos(p: integer): integer;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetVal: Integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
    procedure SetHue(h: integer);
    procedure SetMaxHue(h: Integer);
    procedure SetMaxSat(s: Integer);
    procedure SetMaxVal(v: Integer);
    procedure SetSat(s: integer);
    procedure SetVal(v: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Hue: integer read GetHue write SetHue;
    property Saturation: integer read GetSat write SetSat;
    property Value: integer read GetVal write SetVal;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxValue: Integer read FMaxVal write SetMaxVal default 255;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  end;


implementation

uses
  mbUtils;

{THColorPicker}

constructor THColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FMaxHue := 359;
  FMaxSat := 255;
  FMaxVal := 255;
  FGradientWidth := FMaxHue + 1;
  FGradientHeight := 12;
  FSat := 1.0;
  FVal := 1.0;
  FChange := false;
  SetHue(0);
  HintFormat := 'Hue: %value (selected)';
  FManual := false;
  FChange := true;
end;

function THColorPicker.GetGradientColor(AValue: Integer): TColor;
var
  h: Double;
begin
  if Layout = lyVertical then AValue := (FMaxHue + 1) - AValue;
  h := AValue / (FMaxHue + 1);
  Result := HSVtoColor(h, FSat, FVal);
end;

function THColorPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
end;

function THColorPicker.GetSat: Integer;
begin
  Result := round(FSat * FMaxSat);
end;

function THColorPicker.GetVal: Integer;
begin
  Result := round(FVal * FMaxVal);
end;

procedure THColorPicker.SetHue(h: integer);
begin
  Clamp(h, 0, FMaxHue);
  if GetHue <> h then
  begin
    FHue := h / FMaxHue;
    FArrowPos := ArrowPosFromHue(h);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure THColorPicker.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  FGradientWidth := FMaxHue + 1;   // 0 .. FMaxHue  --> FMaxHue + 1 pixels
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure THColorPicker.SetMaxSat(s: Integer);
begin
  if s = FMaxSat then
    exit;
  FMaxSat := s;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure THColorPicker.SetMaxVal(v: Integer);
begin
  if v = FMaxVal then
    exit;
  FMaxVal := v;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure THColorPicker.SetSat(s: integer);
begin
  Clamp(s, 0, FMaxSat);
  if GetSat() <> s then
  begin
    FSat := s / FMaxSat;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure THColorPicker.SetVal(v: integer);
begin
  Clamp(v, 0, FMaxVal);
  if GetVal() <> v then
  begin
    FVal := v / FMaxVal;
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
    a := Round((Width - 12) * h / FMaxHue);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * h / FMaxHue);
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
    r := Round(p / (Width - 12) * FMaxHue)
  else
    r := Round(p / (Height - 12) * MaxHue);
  Clamp(r, 0, FMaxHue);
  Result := r;
end;

function THColorPicker.GetSelectedColor: TColor;
begin
  Result := HSVtoColor(FHue, FSat, FVal);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function THColorPicker.GetSelectedValue: integer;
begin
  Result := GetHue();
end;

procedure THColorPicker.SetSelectedColor(c: TColor);
var
  h, s, v: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  RGBToHSVRange(GetRValue(c), GetGValue(c), GetBValue(c), h, s, v);
  FChange := false;
  SetHue(h);
  SetSat(s);
  SetVal(v);
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function THColorPicker.GetArrowPos: integer;
begin
  if FMaxHue = 0 then
    Result := inherited GetArrowPos
  else
    Result := ArrowPosFromHue(GetHue());
end;

procedure THColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetHue(GetHue);
    TBA_MouseMove:
      Hue := HueFromArrowPos(FArrowPos);
    TBA_MouseDown:
      Hue := HueFromArrowPos(FArrowPos);
    TBA_MouseUp:
      Hue := HueFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetHue(GetHue() + Increment);
    TBA_WheelDown:
      SetHue(GetHue() - Increment);
    TBA_VKLeft:
      SetHue(GetHue() - Increment);
    TBA_VKCtrlLeft:
      SetHue(0);
    TBA_VKRight:
      SetHue(GetHue() + Increment);
    TBA_VKCtrlRight:
      SetHue(FMaxHue);
    TBA_VKUp:
      SetHue(GetHue() - Increment);
    TBA_VKCtrlUp:
      SetHue(0);
    TBA_VKDown:
      SetHue(GetHue() + Increment);
    TBA_VKCtrlDown:
      SetHue(FMaxHue);
    else
      inherited;
 end;
end;

end.
