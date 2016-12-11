unit mbUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Clamp(var AValue:Integer; AMin, AMax: Integer);
function PointInCircle(p: TPoint; Size: integer): boolean;

implementation

procedure Clamp(var AValue: integer; AMin, AMax: integer);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;

function PointInCircle(p: TPoint; Size: integer): boolean;
var
  r: integer;
begin
  r := size div 2;
  Result := (sqr(p.x - r) + sqr(p.y - r) <= sqr(r));
end;


end.

