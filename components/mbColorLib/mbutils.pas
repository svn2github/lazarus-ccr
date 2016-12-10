unit mbUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Clamp(var AValue:Integer; AMin, AMax: Integer);

implementation

procedure Clamp(var AValue: integer; AMin, AMax: integer);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;


end.

