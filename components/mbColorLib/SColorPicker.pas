unit SColorPicker;

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
  RGBHSVUtils, mbTrackBarPicker, HTMLColors, Scanlines;

type
  TSColorPicker = class(TmbTrackBarPicker)
  private
    FVal, FHue, FSat: Double;
    FMaxVal, FMaxHue, FMaxSat: Integer;
    function ArrowPosFromSat(s: integer): integer;
    function SatFromArrowPos(p: integer): integer;
    function GetHue: Integer;
    function GetSat: Integer;
    function GetVal: Integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetValue(v: integer);
    procedure SetMaxHue(h: Integer);
    procedure SetMaxSat(s: Integer);
    procedure SetMaxVal(v: Integer);
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
  FChange := false;
  FHue := 0;
  FVal := 1.0;
  SetSat(FMaxSat);
  HintFormat := 'Saturation: %value (selected)';
  FManual := false;
  FChange := true;
end;

(*
procedure TSColorPicker.CreateSGradient;
var
 i,j: integer;
 row: pRGBQuadArray;
begin
 if FSBmp = nil then
  begin
   FSBmp := TBitmap.Create;
   FSBmp.PixelFormat := pf32bit;
  end;
 if Layout = lyHorizontal then
  begin
   FSBmp.width := 255;
   FSBmp.height := 12;
   for i := 0 to 254 do
    for j := 0 to 11 do
     begin
      row := FSBmp.Scanline[j];
      if not WebSafe then
       row[i] := RGBToRGBQuad(HSVtoColor(FHue, i, FVal))
//       FSBmp.Canvas.Pixels[i, j] := HSVtoColor(FHue, i, FVal)
      else
       row[i] := RGBToRGBQuad(GetWebSafe(HSVtoColor(FHue, i, FVal)));
//       FSBmp.Canvas.Pixels[i, j] := GetWebSafe(HSVtoColor(FHue, i, FVal));
     end;
  end
 else
  begin
   FSBmp.width := 12;
   FSBmp.height := 255;
   for i := 0 to 254 do
    begin
     row := FSBmp.Scanline[i];
     for j := 0 to 11 do
      if not WebSafe then
       row[j] := RGBToRGBQuad(HSVtoColor(FHue, 255-i, FVal))
//       FSBmp.Canvas.Pixels[j, i] := HSVtoColor(FHue, 255-i, FVal)
      else
       row[j] := RGBToRGBQuad(GetWebSafe(HSVtoColor(FHue, 255-i, FVal)));
//       FSBmp.Canvas.Pixels[j, i] := GetWebSafe(HSVtoColor(FHue, 255-i, FVal));
    end;
  end;
end;
 *)

function TSColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := HSVtoColor(FHue, AValue/FMaxSat, FVal);
end;

procedure TSColorPicker.SetValue(v: integer);
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

procedure TSColorPicker.SetHue(h: integer);
begin
  Clamp(h, 0, FMaxHue);
  if GetHue() <> h then
  begin
    FHue := h / FMaxHue;
    CreateGradient;
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TSColorPicker.SetSat(s: integer);
begin
  Clamp(s, 0, FMaxSat);
  if GetSat() <> s then
  begin
    FSat := s / FMaxSat;
    FManual := false;
    FArrowPos := ArrowPosFromSat(s);
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
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
    s := FMaxSat - s;
    a := Round(s / FMaxSat * (Height - 12));
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TSColorPicker.SatFromArrowPos(p: integer): integer;
var
  r: integer;
begin
  if Layout = lyHorizontal then
    r := Round(p / (Width - 12) * FMaxSat)
  else
    r := Round(FMaxSat - p / (Height - 12) * FMaxSat);
  Clamp(r, 0, FMaxSat);
  Result := r;
end;

function TSColorPicker.GetHue: Integer;
begin
  Result := round(FHue * FMaxHue);
end;

function TSColorPicker.GetSat: Integer;
begin
  Result := round(FSat * FMaxSat);
end;

function TSColorPicker.GetVal: Integer;
begin
  Result := round(FVal * FMaxVal);
end;

procedure TSColorPicker.SetMaxHue(h: Integer);
begin
  if h = FMaxHue then
    exit;
  FMaxHue := h;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSColorPicker.SetMaxSat(s: Integer);
begin
  if s = FMaxSat then
    exit;
  FMaxSat := s;
  FGradientWidth := FMaxSat + 1;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TSColorPicker.SetMaxVal(v: Integer);
begin
  if v = FMaxVal then
    exit;
  FMaxVal := v;
  CreateGradient;
  if FChange and Assigned(OnChange) then OnChange(Self);
  Invalidate;
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

procedure TSColorPicker.SetSelectedColor(c: TColor);
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

function TSColorPicker.GetArrowPos: integer;
begin
  if FMaxSat = 0 then
    Result := inherited GetArrowPos
  else
    Result := ArrowPosFromSat(GetSat());
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

end.
