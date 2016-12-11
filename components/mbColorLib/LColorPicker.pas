unit LColorPicker;

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
  RGBHSLUtils, mbTrackBarPicker, HTMLColors;

type
  TLColorPicker = class(TmbTrackBarPicker)
  private
    FHue, FSat, FLuminance: integer;
    function ArrowPosFromLum(l: integer): integer;
    function LumFromArrowPos(p: integer): integer;
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetLuminance(l: integer);
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Hue: integer read FHue write SetHue default 0;
    property Saturation: integer read FSat write SetSat default 240;
    property Luminance: integer read FLuminance write SetLuminance default 120;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  end;

implementation

uses
  mbUtils;

{TLColorPicker}

constructor TLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 12;
  FHue := 0;
  FSat := MaxSat;
  FArrowPos := ArrowPosFromLum(MaxLum div 2);
  FChange := false;
  SetLuminance(MaxLum div 2);
  HintFormat := 'Luminance: %value';
  FManual := false;
  FChange := true;
end;

function TLColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := HSLRangeToRGB(FHue, FSat, AValue);
end;

procedure TLColorPicker.SetHue(h: integer);
begin
  Clamp(h, 0, MaxHue);
  if FHue <> h then
  begin
    FHue := h;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TLColorPicker.SetSat(s: integer);
begin
  Clamp(s, 0, MaxSat);
  if FSat <> s then
  begin
    FSat := s;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function TLColorPicker.ArrowPosFromLum(l: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/MaxLum)*l);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    l := MaxLum - l;
    a := Round(((Height - 12)/MaxLum)*l);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TLColorPicker.LumFromArrowPos(p: integer): integer;
var
  L: integer;
begin
  if Layout = lyHorizontal then
    L := Round(p/((Width - 12)/MaxLum))
  else
    L := Round(MaxLum - p/((Height - 12)/MaxLum));
  Clamp(L, 0, MaxLum);
  Result := L;
end;

procedure TLColorPicker.SetLuminance(L: integer);
begin
  Clamp(L, 0, MaxLum);
  if FLuminance <> L then
  begin
    FLuminance := L;
    FArrowPos := ArrowPosFromLum(L);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function TLColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := HSLRangeToRGB(FHue, FSat, FLuminance)
  else
    Result := GetWebSafe(HSLRangeToRGB(FHue, FSat, FLuminance));
end;

function TLColorPicker.GetSelectedValue: integer;
begin
  Result := FLuminance;
end;

procedure TLColorPicker.SetSelectedColor(c: TColor);
var
  h1, s1, l1: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  RGBtoHSLRange(c, h1, s1, l1);
  Fchange := false;
  SetHue(h1);
  SetSat(s1);
  SetLuminance(l1);
  FChange := true;
  FManual := false;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

function TLColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromLum(FLuminance);
end;

procedure TLColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetLuminance(FLuminance);
    TBA_MouseMove:
      FLuminance := LumFromArrowPos(FArrowPos);
    TBA_MouseDown:
      Fluminance := LumFromArrowPos(FArrowPos);
    TBA_MouseUp:
      Fluminance := LumFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetLuminance(FLuminance + Increment);
    TBA_WheelDown:
      SetLuminance(FLuminance - Increment);
    TBA_VKRight:
      SetLuminance(FLuminance + Increment);
    TBA_VKCtrlRight:
      SetLuminance(MaxLum);
    TBA_VKLeft:
      SetLuminance(FLuminance - Increment);
    TBA_VKCtrlLeft:
      SetLuminance(0);
    TBA_VKUp:
      SetLuminance(FLuminance + Increment);
    TBA_VKCtrlUp:
      SetLuminance(MaxLum);
    TBA_VKDown:
      SetLuminance(FLuminance - Increment);
    TBA_VKCtrlDown:
      SetLuminance(0);
    else
      inherited;
  end;
end;

end.
