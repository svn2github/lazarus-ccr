unit VColorPicker;

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
  SysUtils, Classes, Controls, Forms, Graphics,
  RGBHSVUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
  TVColorPicker = class(TmbTrackBarPicker)
  private
    FHue, FSat, FVal: Double;
    FMaxHue, FMaxSat, FMaxVal: Integer;
    function ArrowPosFromVal(v: integer): integer;
    function ValFromArrowPos(p: integer): integer;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetSelectedColor: TColor;
    function GetValue: Integer;
    procedure SetSelectedColor(c: TColor);
    procedure SetHue(h: integer);
    procedure SetMaxHue(h: Integer);
    procedure SetMaxSat(s: Integer);
    procedure SetMaxVal(v: Integer);
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
    property Hue: integer read GetHue write SetHue;
    property Saturation: integer read GetSat write SetSat;
    property Value: integer read GetValue write SetValue;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxValue: Integer read FMaxVal write SetMaxVal default 255;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  end;

implementation

{TVColorPicker}

constructor TVColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FMaxHue := 359;
  FMaxSat := 255;
  FMaxVal := 255;
  FGradientWidth := FMaxVal + 1;
  FGradientHeight := 12;
  FHue := 0;
  FSat := 0;
  FChange := false;
  SetValue(FMaxVal);
  HintFormat := 'Value: %value (selected)';
  FManual := false;
  FChange := true;
end;

function TVColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := HSVtoColor(FHue, FSat, AValue / FMaxVal);
end;

function TVColorPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
end;

function TVColorPicker.GetSat: Integer;
begin
  Result := round(FSat * FMaxSat);
end;

function TVColorPicker.GetValue: Integer;
begin
  Result := round(FVal * FMaxVal);
end;

procedure TVColorPicker.SetHue(h: integer);
begin
  if h > FMaxHue+1 then h := FMaxHue + 1;
  if h < 0 then h := 0;
  if GetHue() <> h then
  begin
    FHue := h / (FMaxHue + 1);
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TVColorPicker.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure TVColorPicker.SetMaxSat(s: Integer);
begin
  if s = FMaxSat then
    exit;
  FMaxSat := s;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure TVColorPicker.SetMaxVal(v: Integer);
begin
  if v = FMaxVal then
    exit;
  FMaxVal := v;
  FGradientWidth := FMaxVal + 1;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure TVColorPicker.SetSat(s: integer);
begin
  if s > FMaxSat then s := FMaxSat;
  if s < 0 then s := 0;
  if GetSat() <> s then
  begin
    FSat := s / FMaxSat;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function TVColorPicker.ArrowPosFromVal(v: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) * v / FMaxVal);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    v := FMaxVal - v;
    a := Round((Height - 12) * v / FMaxVal);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TVColorPicker.ValFromArrowPos(p: integer): integer;
var
  r: integer;
begin
  if Layout = lyHorizontal then
    r := Round(p / (Width - 12) * FMaxVal)
  else
    r := Round(FMaxVal - p / (Height - 12) * FMaxVal);
  if r < 0 then r := 0;
  if r > FMaxVal then r := FMaxVal;
  Result := r;
end;

procedure TVColorPicker.SetValue(V: integer);
begin
  if v < 0 then v := 0;
  if v > FMaxVal then v := FMaxVal;
  if GetValue() <> v then
  begin
    FVal := v / FMaxVal;
    FArrowPos := ArrowPosFromVal(v);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function TVColorPicker.GetSelectedColor: TColor;
begin
  Result := HSVtoColor(FHue, FSat, FVal);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TVColorPicker.GetSelectedValue: integer;
begin
  Result := GetValue();
end;

procedure TVColorPicker.SetSelectedColor(c: TColor);
var
  h, s, v: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  RGBToHSVRange(GetRValue(c), GetGValue(c), GetBValue(c), h, s, v);
  FChange := false;
  SetHue(h);
  SetSat(s);
  SetValue(v);
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function TVColorPicker.GetArrowPos: integer;
begin
  if FMaxVal = 0 then
    Result := inherited GetArrowPos
  else
    Result := ArrowPosFromVal(GetValue());
end;

procedure TVColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetValue(GetValue());
    TBA_MouseMove:
      SetValue(ValFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetValue(ValFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetValue(ValFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetValue(GetValue() + Increment);
    TBA_WheelDown:
      SetValue(GetValue() - Increment);
    TBA_VKRight:
      SetValue(GetValue() + Increment);
    TBA_VKCtrlRight:
      SetValue(FMaxVal);
    TBA_VKLeft:
      SetValue(GetValue() - Increment);
    TBA_VKCtrlLeft:
      SetValue(0);
    TBA_VKUp:
      SetValue(GetValue() + Increment);
    TBA_VKCtrlUp:
      SetValue(FMaxVal);
    TBA_VKDown:
      SetValue(GetValue() - Increment);
    TBA_VKCtrlDown:
      SetValue(0);
    else
      inherited;
  end;
end;

end.
