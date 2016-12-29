unit ugenericcollection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TGenericCollection }

  generic TGenericCollection<T> = class(TCollection)
  private
    function GetItems(Index: integer): T;
    procedure SetItems(Index: integer; AValue: T);
  public
    constructor Create;
  public
    function Add: T;
  public
    property Items[Index: integer]: T read GetItems write SetItems; default;
  end;

implementation

{ TGenericCollection }

function TGenericCollection.GetItems(Index: integer): T;
begin
  Result := T(inherited Items[Index]);
end;

procedure TGenericCollection.SetItems(Index: integer; AValue: T);
begin
  Items[Index].Assign(AValue);
end;

constructor TGenericCollection.Create;
begin
  inherited Create(T);
end;

function TGenericCollection.Add: T;
begin
  Result := T(inherited Add);
end;

end.
