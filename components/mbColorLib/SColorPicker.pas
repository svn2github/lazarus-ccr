unit SColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBHSVUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
  TSColorPicker = class(TmbTrackBarPicker)
  private
    FVal, FHue, FSat: Double;
    FMaxVal, FMaxHue, FMaxSat: Integer;
    function ArrowPosFromSat(s: integer): integer;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetSelectedColor: TColor;
    function GetVal: Integer;
    function SatFromArrowPos(p: integer): integer;
    procedure SetHue(h: integer);
    procedure SetMaxHue(h: Integer);
    procedure SetMaxSat(s: Integer);
    procedure SetMaxVal(v: Integer);
    procedure SetSat(s: integer);
    procedure SetValue(v: integer);
    procedure SetSelectedColor(c: TColor);
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
    property Value: integer read GetVal write SetValue;
    property MaxHue: Integer read FMaxHue write SetMaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 255;
    property MaxValue: Integer read FMaxVal write SetMaxVal default 255;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  end;


implementation

uses
  mbUtils;

{ TSColorPicker }

constructor TSColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FMaxHue := 359;
  FMaxSat := 255;
  FMaxVal := 255;
  FGradientWidth := FMaxSat + 1;
  FGradientHeight := 1;
  FHue := 0;
  FVal := 1.0;
  SetSat(FMaxSat);
  HintFormat := 'Saturation: %value (selected)';
end;

function TSColorPicker.ArrowPosFromSat(s: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(s / FMaxSat * (Width - 12));
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((FMaxSat - s) / FMaxSat * (Height - 12));
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TSColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetSat(GetSat());
    TBA_MouseMove:
      SetSat(SatFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetSat(SatFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetSat(SatFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetSat(GetSat() + Increment);
    TBA_WheelDown:
      SetSat(GetSat() - Increment);
    TBA_VKLeft:
      SetSat(GetSat() - Increment);
    TBA_VKCtrlLeft:
      SetSat(0);
    TBA_VKRight:
      SetSat(GetSat() + Increment);
    TBA_VKCtrlRight:
      SetSat(FMaxSat);
    TBA_VKUp:
      SetSat(GetSat() + Increment);
    TBA_VKCtrlUp:
      SetSat(FMaxSat);
    TBA_VKDown:
      SetSat(GetSat() - Increment);
    TBA_VKCtrlDown:
      SetSat(0);
    else
      inherited;
  end;
end;

function TSColorPicker.GetArrowPos: integer;
begin
  if FMaxSat = 0 then
    Result := inherited GetArrowPos
  else
    Result := ArrowPosFromSat(GetSat());
end;

function TSColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := HSVtoColor(FHue, AValue/FMaxSat, FVal);
end;

function TSColorPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
end;

function TSColorPicker.GetSat: Integer;
begin
  Result := round(FSat * FMaxSat);
end;

function TSColorPicker.GetSelectedColor: TColor;
begin
  Result := HSVToColor(FHue, FSat, FVal);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TSColorPicker.GetSelectedValue: integer;
begin
  Result := GetSat();
end;

function TSColorPicker.GetVal: Integer;
begin
  Result := round(FVal * FMaxVal);
end;

function TSColorPicker.SatFromArrowPos(p: integer): integer;
var
  s: integer;
begin
  case Layout of
    lyHorizontal: s := Round(p / (Width - 12) * FMaxSat);
    lyVertical  : s := Round(FMaxSat - p / (Height - 12) * FMaxSat);
  end;
  Clamp(s, 0, FMaxSat);
  Result := s;
end;

procedure TSColorPicker.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSColorPicker.SetMaxSat(s: Integer);
begin
  if s = FMaxSat then
    exit;
  FMaxSat := s;
  FGradientWidth := FMaxSat + 1;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSColorPicker.SetMaxVal(v: Integer);
begin
  if v = FMaxVal then
    exit;
  FMaxVal := v;
  CreateGradient;
  //if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSColorPicker.SetHue(h: integer);
begin
  Clamp(h, 0, FMaxHue);
  if GetHue() <> h then
  begin
    FHue := h / FMaxHue;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

procedure TSColorPicker.SetSat(s: integer);
begin
  Clamp(s, 0, FMaxSat);
  if GetSat() <> s then
  begin
    FSat := s / FMaxSat;
    FArrowPos := ArrowPosFromSat(s);
    Invalidate;
    DoChange;
  end;
end;

procedure TSColorPicker.SetSelectedColor(c: TColor);
var
  h, s, v: integer;
  needNewGradient: Boolean;
begin
  if WebSafe then
    c := GetWebSafe(c);
  if c = GetSelectedColor then
    exit;

  RGBToHSVRange(GetRValue(c), GetGValue(c), GetBValue(c), h, s, v);
  needNewGradient := (h <> FHue) or (v <> FVal);
  FHue := h;
  FSat := s;
  FVal := v;
  if needNewGradient then
    CreateGradient;
  Invalidate;
  DoChange;
end;

procedure TSColorPicker.SetValue(v: integer);
begin
  Clamp(v, 0, FMaxVal);
  if GetVal() <> v then
  begin
    FVal := v / FMaxVal;
    CreateGradient;
    Invalidate;
    DoChange;
  end;
end;

end.
