unit RGBHSVUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Math,
  Scanlines;

procedure RGBtoHSV(R, G, B: Integer; out H, S, V: Double);
procedure RGBtoHSVRange(R, G, B: integer; out H, S, V: integer);

procedure HSVtoRGB(H, S, V: Double; out R, G, B: Integer);
procedure HSVtoRGBRange(H, S, V: Integer; out R, G, B: Integer);

function HSVtoRGBTriple(H, S, V: integer): TRGBTriple;
function HSVtoRGBQuad(H, S, V: integer): TRGBQuad;

function HSVtoColor(H, S, V: Double): TColor;
function HSVRangeToColor(H, S, V: Integer): TColor;

function GetHValue(Color: TColor): integer;
function GetVValue(Color: TColor): integer;
function GetSValue(Color: TColor): integer;


implementation

procedure RGBToHSVRange(R, G, B: integer; out H, S, V: integer);
var
  Delta, Min, H1, S1: double;
begin
  h1 := h;
  s1 := s;
  Min := MinIntValue([R, G, B]);
  V := MaxIntValue([R, G, B]);
  Delta := V - Min;
  if V =  0.0 then S1 := 0 else S1 := Delta / V;
  if S1  = 0.0 then
    H1 := 0
  else
  begin
    if R = V then
      H1 := 60.0 * (G - B) / Delta
    else if G = V then
      H1 := 120.0 + 60.0 * (B - R) / Delta
    else if B = V then
      H1 := 240.0 + 60.0 * (R - G) / Delta;
    if H1 < 0.0 then H1 := H1 + 360.0;
  end;
  h := round(h1);
  s := round(s1*255);
end;

procedure RGBToHSV(R, G, B: Integer; out H, S, V: Double);
var
  hh, ss, vv: Integer;
begin
  RGBtoHSVRange(R, G, B, hh, ss, vv);
  H := H / 360;
  S := S / 255;
  V := V / 255;
end;

procedure HSVtoRGB(H, S, V: Double; out R, G, B: Integer);
begin
  HSVtoRGBRange(round(H*360), round(S*255), round(V*255), R, G, B);
end;

procedure HSVtoRGBRange(H, S, V: Integer; out R, G, B: Integer);
var
  t: TRGBTriple;
begin
  t := HSVtoRGBTriple(H, S, V);
  R := t.rgbtRed;
  G := t.rgbtGreen;
  B := t.rgbtBlue;
end;

function HSVtoRGBTriple(H, S, V: integer): TRGBTriple;
const
  divisor: integer = 255*60;
var
  f, hTemp, p, q, t, VS: integer;
begin
  if H > 360 then H := H - 360;
  if H < 0 then H := H + 360;
  if s = 0 then
    Result := RGBtoRGBTriple(V, V, V)
  else
  begin
    if H = 360 then hTemp := 0 else hTemp := H;
    f := hTemp mod 60;
    hTemp := hTemp div 60;
    VS := V*S;
    p := V - VS div 255;
    q := V - (VS*f) div divisor;
    t := V - (VS*(60 - f)) div divisor;
    case hTemp of
      0: Result := RGBtoRGBTriple(V, t, p);
      1: Result := RGBtoRGBTriple(q, V, p);
      2: Result := RGBtoRGBTriple(p, V, t);
      3: Result := RGBtoRGBTriple(p, q, V);
      4: Result := RGBtoRGBTriple(t, p, V);
      5: Result := RGBtoRGBTriple(V, p, q);
    else Result := RGBtoRGBTriple(0,0,0)
    end;
  end;
end;

function HSVtoRGBQuad(H, S, V: integer): TRGBQuad;
const
  divisor: integer = 255*60;
var
  f, hTemp, p, q, t, VS: integer;
begin
  if H > 360 then H := H - 360;
  if H < 0 then H := H + 360;
  if s = 0 then
    Result := RGBtoRGBQuad(V, V, V)
  else
  begin
    if H = 360 then hTemp := 0 else hTemp := H;
    f := hTemp mod 60;
    hTemp := hTemp div 60;
    VS := V*S;
    p := V - VS div 255;
    q := V - (VS*f) div divisor;
    t := V - (VS*(60 - f)) div divisor;
    case hTemp of
      0: Result := RGBtoRGBQuad(V, t, p);
      1: Result := RGBtoRGBQuad(q, V, p);
      2: Result := RGBtoRGBQuad(p, V, t);
      3: Result := RGBtoRGBQuad(p, q, V);
      4: Result := RGBtoRGBQuad(t, p, V);
      5: Result := RGBtoRGBQuad(V, p, q);
    else Result := RGBtoRGBQuad(0,0,0)
    end;
  end;
end;

function HSVRangetoColor(H, S, V: integer): TColor;
begin
  Result := RGBTripleToColor(HSVtoRGBTriple(H, S, V));
end;

function HSVtoColor(H, S, V: Double): TColor;
begin
  Result := HSVRangeToColor(round(H*360), round(S*255), round(V*255));
end;

function GetHValue(Color: TColor): integer;
var
  s, v: integer;
begin
  RGBToHSVRange(GetRValue(Color), GetGValue(Color), GetBValue(Color), Result, s, v);
end;

function GetSValue(Color: TColor): integer;
var
  h, v: integer;
begin
  RGBToHSVRange(GetRValue(Color), GetGValue(Color), GetBValue(Color), h, Result, v);
end;

function GetVValue(Color: TColor): integer;
var
  h, s: integer;
begin
  RGBToHSVRange(GetRValue(Color), GetGValue(Color), GetBValue(Color), h, s, Result);
end;

end.
