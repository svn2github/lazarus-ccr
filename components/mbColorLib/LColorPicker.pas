unit LColorPicker;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, RGBHSLUtils, mbTrackBarPicker;

type
  TLColorPicker = class(TmbTrackBarPicker)
  private
    FHue, FSat, FLuminance: Double;
    FMaxHue, FMaxSat, FMaxLum: Integer;
    function ArrowPosFromLum(L: integer): integer;
    function GetHue: Integer;
    function GetLuminance: Integer;
    function GetSat: Integer;
    function GetSelectedColor: TColor;
    function LumFromArrowPos(p: integer): integer;
    procedure SetHue(H: integer);
    procedure SetLuminance(L: integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetSat(S: integer);
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
    property Luminance: integer read GetLuminance write SetLuminance;
    property MaxHue: Integer read FMaxHue write SetmaxHue default 359;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat default 240;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum default 240;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  end;

implementation

uses
  mbUtils;

{TLColorPicker}

constructor TLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FMaxHue := 359;
  FMaxSat := 240;
  FMaxLum := 240;
  FGradientWidth := FMaxLum + 1;
  FGradientHeight := 1;
  FHue := 0;
  FSat := FMaxSat;
  FChange := false;
  SetLuminance(FMaxLum div 2);
  HintFormat := 'Luminance: %value (selected)';
  FManual := false;
  FChange := true;
end;

function TLColorPicker.ArrowPosFromLum(L: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) * L / FMaxLum);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * (FMaxLum - L) / FMaxLum);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TLColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetLuminance(GetLuminance());
    TBA_MouseMove:
      SetLuminance(LumFromArrowPos(FArrowPos));
    TBA_MouseDown:
      SetLuminance(LumFromArrowPos(FArrowPos));
    TBA_MouseUp:
      SetLuminance(LumFromArrowPos(FArrowPos));
    TBA_WheelUp:
      SetLuminance(GetLuminance() + Increment);
    TBA_WheelDown:
      SetLuminance(GetLuminance() - Increment);
    TBA_VKRight:
      SetLuminance(GetLuminance() + Increment);
    TBA_VKCtrlRight:
      SetLuminance(FMaxLum);
    TBA_VKLeft:
      SetLuminance(GetLuminance() - Increment);
    TBA_VKCtrlLeft:
      SetLuminance(0);
    TBA_VKUp:
      SetLuminance(GetLuminance() + Increment);
    TBA_VKCtrlUp:
      SetLuminance(FMaxLum);
    TBA_VKDown:
      SetLuminance(GetLuminance() - Increment);
    TBA_VKCtrlDown:
      SetLuminance(0);
    else
      inherited;
  end;
end;

function TLColorPicker.GetArrowPos: integer;
begin
  if FMaxLum = 0 then
    Result := inherited GetArrowPos
  else
    Result := ArrowPosFromLum(GetLuminance());
end;

function TLColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := HSLToRGB(FHue, FSat, AValue/FMaxLum);
end;

function TLColorPicker.GetHue: Integer;
begin
  Result := Round(FHue * FMaxHue);
end;

function TLColorPicker.GetLuminance: Integer;
begin
  Result := Round(FLuminance * FMaxLum);
end;

function TLColorPicker.GetSat: Integer;
begin
  Result := Round(FSat * FMaxSat);
end;

function TLColorPicker.GetSelectedColor: TColor;
begin
  Result := HSLToRGB(FHue, FSat, FLuminance);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TLColorPicker.GetSelectedValue: integer;
begin
  Result := GetLuminance();
end;

function TLColorPicker.LumFromArrowPos(p: integer): integer;
var
  L: integer;
begin
  if Layout = lyHorizontal then
    L := Round(p / (Width - 12) * FMaxLum)
  else
    L := Round(MaxLum - p /(Height - 12) * FMaxLum);
  Clamp(L, 0, FMaxLum);
  Result := L;
end;

procedure TLColorPicker.SetHue(H: integer);
begin
  Clamp(H, 0, FMaxHue);
  if GetHue() <> H then
  begin
    FHue := H / FMaxHue;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TLColorPicker.SetLuminance(L: integer);
begin
  Clamp(L, 0, FMaxLum);
  if GetLuminance() <> L then
  begin
    FLuminance := L / FMaxLum;
    FArrowPos := ArrowPosFromLum(L);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TLColorPicker.SetMaxHue(H: Integer);
begin
  if H = FMaxHue then
    exit;
  FMaxHue := H;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure TLColorPicker.SetMaxLum(L: Integer);
begin
  if L = FMaxLum then
    exit;
  FMaxLum := L;
  FGradientWidth := FMaxLum + 1;   // 0 .. FMaxHue  --> FMaxHue + 1 pixels
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure TLColorPicker.SetMaxSat(S: Integer);
begin
  if S = FMaxSat then
    exit;
  FMaxSat := S;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

procedure TLColorPicker.SetSat(S: integer);
begin
  Clamp(S, 0, FMaxSat);
  if GetSat() <> S then
  begin
    FSat := S / FMaxSat;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TLColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  ColortoHSL(c, FHue, FSat, FLuminance);
  FChange := false;
  FManual := false;
  CreateGradient;
  Invalidate;
  if FChange and Assigned(OnChange) then OnChange(Self);
end;

end.
