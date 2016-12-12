unit mbUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

procedure Clamp(var AValue:Integer; AMin, AMax: Integer);
procedure DrawHorDottedLine(ACanvas: TCanvas; X1, X2, Y: Integer; AColor: TColor);
function PointInCircle(p: TPoint; Size: integer): boolean;
function PtInCircle(p, ctr: TPoint; Radius: Integer): Boolean;

implementation

procedure Clamp(var AValue: integer; AMin, AMax: integer);
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


end.

