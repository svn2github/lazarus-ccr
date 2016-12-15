unit mbUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf;

procedure Clamp(var AValue: Integer; AMin, AMax: Integer); overload;
procedure Clamp(var AValue: Double; AMin, AMax: Double); overload;
procedure DrawHorDottedLine(ACanvas: TCanvas; X1, X2, Y: Integer; AColor: TColor);
function PointInCircle(p: TPoint; Size: integer): boolean;
function PtInCircle(p, ctr: TPoint; Radius: Integer): Boolean;

function HighContrastColor(AColor: TColor): TColor;

function HeightOf(R: TRect): Integer;
function WidthOf(R: TRect): Integer;

implementation

procedure Clamp(var AValue: integer; AMin, AMax: integer);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;

procedure Clamp(var AValue: Double; AMin, AMax: Double);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;

procedure DrawHorDottedLine(ACanvas: TCanvas; X1, X2, Y: Integer; AColor: TColor);
begin
  while X1 <= X2 do begin
    ACanvas.Pixels[X1, Y] := AColor;
    inc(X1, 2);
  end;
end;

function PointInCircle(p: TPoint; Size: integer): boolean;
var
  r: integer;
begin
  r := size div 2;
  Result := (sqr(p.x - r) + sqr(p.y - r) <= sqr(r));
end;

function PtInCircle(p, ctr: TPoint; Radius: Integer): Boolean;
begin
  Result := sqr(p.x - ctr.x) + sqr(p.y - ctr.y) <= sqr(Radius);
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HighContrastColor(AColor: TColor): TColor;
begin
  if GetRValue(AColor) + GetGValue(AColor) + GetBValue(AColor) > 3*128 then
    Result := clBlack
  else
    Result := clWhite;
end;

end.

