{ RxTextHolder unit

  Copyright (C) 2005-2018 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RxTextHolder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { TRxTextHolderItem }

  TRxTextHolderItem = class(TCollectionItem)
  private
    FCaption: string;
    FLines: TStrings;
    procedure SetCaption(AValue: string);
    procedure SetLines(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Caption:string read FCaption write SetCaption;
    property Lines:TStrings read FLines write SetLines;
  end;

  { TRxTextHolderItems }

  TRxTextHolderItems = class(TOwnedCollection)
  private
    function GetItems(AIndex: Integer): TRxTextHolderItem;
  public
    function Add(ACaption:string):TRxTextHolderItem;
    property Items[AIndex:Integer]:TRxTextHolderItem read GetItems; default;
  end;

  TRxTextHolder = class(TComponent)
  private
    FItems:TRxTextHolderItems;
    function GetText(ACaption: string): string;
    procedure SetItems(AValue: TRxTextHolderItems);
    procedure SetText(ACaption: string; AValue: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function IndexByName(ACaption:string):integer;
    property Text[ACaption:string]:string read GetText write SetText; default;
  published
    property Items:TRxTextHolderItems read FItems write SetItems;
  end;

implementation

{ TRxTextHolderItems }

function TRxTextHolderItems.GetItems(AIndex: Integer): TRxTextHolderItem;
begin
  Result:=TRxTextHolderItem(inherited Items[AIndex]);
end;

function TRxTextHolderItems.Add(ACaption: string): TRxTextHolderItem;
begin
  Result:=TRxTextHolderItem.Create(Self);
  Result.Caption:=ACaption;
end;

{ TRxTextHolder }

procedure TRxTextHolder.SetItems(AValue: TRxTextHolderItems);
begin
  FItems.Assign(AValue);
end;

function TRxTextHolder.GetText(ACaption: string): string;
var
  Itm: TRxTextHolderItem;
  I: Integer;
begin
  Result:='';
  I:=IndexByName(ACaption);
  if i<0 then Exit;
  Itm:=FItems[I];
  if Assigned(Itm) then
    Result:=Itm.Lines.Text;
end;

procedure TRxTextHolder.SetText(ACaption: string; AValue: string);
var
  I: Integer;
  Itm: TRxTextHolderItem;
begin
  I:=IndexByName(ACaption);
  if i < 0 then
    Itm:=Items.Add(ACaption)
  else
    Itm:=FItems[I];
  Itm.Lines.Text:=AValue;
end;

procedure TRxTextHolder.AssignTo(Dest: TPersistent);
begin
  if (Dest is TRxTextHolder) then
  begin
    TRxTextHolder(Dest).Items.Assign(Items);
  end
  else
    inherited AssignTo(Dest);
end;

constructor TRxTextHolder.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItems:=TRxTextHolderItems.Create(Self, TRxTextHolderItem);
end;

destructor TRxTextHolder.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TRxTextHolder.IndexByName(ACaption: string): integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to FItems.Count-1 do
    if FItems[i].Caption = ACaption then
      Exit(i);
end;

{ TRxTextHolderItem }

procedure TRxTextHolderItem.SetCaption(AValue: string);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TRxTextHolderItem.SetLines(AValue: TStrings);
begin
  FLines.Assign(AValue);
end;

function TRxTextHolderItem.GetDisplayName: string;
begin
  if Caption <> '' then
    Result:=Caption
  else
    Result:=inherited GetDisplayName;
end;

procedure TRxTextHolderItem.AssignTo(Dest: TPersistent);
begin
  if (Dest is TRxTextHolderItem) then
  begin
    TRxTextHolderItem(Dest).Lines.Assign(Lines);
    TRxTextHolderItem(Dest).Caption:=FCaption;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TRxTextHolderItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLines:=TStringList.Create;
end;

destructor TRxTextHolderItem.Destroy;
begin
  FreeAndNil(FLines);
  inherited Destroy;
end;

end.

