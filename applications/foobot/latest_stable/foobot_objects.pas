unit foobot_objects;
{ Objects for Foobot Interrogator

  Copyright (C)2016 Gordon Bamber minsadorada@charcodelvalle.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ugenericcollection, fpjsonrtti;

{TFoobotIdentities}
type
  TFoobotIdentities = class(TCollectionItem)
    // JSON fields here as properties
  private
    Fuuid: string;
    FuserId: integer;
    FMac: string;
    FName: string;
  public
  published
    property uuid: string read Fuuid write Fuuid;
    property userId: integer read FuserId write FuserId;
    property mac: string read FMac write FMac;
    property name: string read FName write FName;
  end;

  {TFoobotIdentityList}
  TFoobotIdentityList = specialize TGenericCollection<TFoobotIdentities>;

{TFoobotIdentityObject}
// Contains a list of TFoobotIdentities as a TCollection
type
  TFoobotIdentityObject = class(TPersistent)
  private
    FFoobotIdentityList: TFoobotIdentityList;
  public
    constructor Create;
    destructor Destroy; override;
    function SaveToFile(const AFilename: string): boolean;
    function LoadFromFile(const AFileName: string): boolean;
  published
    property FoobotIdentityList: TFoobotIdentityList
      read FFoobotIdentityList write FFoobotIdentityList;
  end;


type
  TFoobotDataObject = class(TPersistent)
    private
      FDataPoints:Variant;
      FSensors:TStrings;
      FUnits:TStrings;
      Fuuid:String;
      FStart:Int64;
      FEnd:Int64;
    public
      constructor Create;
      Destructor Destroy; override;
      function SaveToFile(const AFilename: string): boolean;
    published
      property uuid:String read Fuuid write Fuuid;
      property start:Int64 read FStart write FStart;
      property &end:Int64 read FEnd write FEnd;
      property sensors:TStrings
        read FSensors write FSensors;
      property units:TStrings
        read FUnits write FUnits;
      property datapoints : Variant read FDataPoints write FDataPoints;
  end;


implementation

constructor TFoobotDataObject.Create;
begin
  inherited;
  FSensors:=TStringList.Create;
  FUnits:=TstringList.Create;
end;

Destructor TFoobotDataObject.Destroy;
begin
  FSensors.Free;
  FUnits.Free;
  inherited Destroy;
end;

{TFoobotIdentityObject}
constructor TFoobotIdentityObject.Create;
begin
  inherited;
  FFoobotIdentityList := TFoobotIdentityList.Create;
end;

destructor TFoobotIdentityObject.Destroy;
var
  c: TCollectionItem;
begin
  for c in FFoobotIdentityList do
    c.Free;
  FFoobotIdentityList.Free;
  inherited Destroy;
end;

function TFoobotIdentityObject.LoadFromFile(const AFileName: string): boolean;
var
  DeStreamer: TJSONDeStreamer;
  s: TStringList;
begin
  Result := True;
  s := TStringList.Create;
  try
    s.LoadFromFile(AFileName);
    DeStreamer := TJSONDeStreamer.Create(nil);
    try
      DeStreamer.JSONToObject(s.Text, Self);
    except
      // Eat the exception
      On E: Exception do
        Result := False;
    end;
  finally
    DeStreamer.Free;
    s.Free;
  end;

end;

function TFoobotIdentityObject.SaveToFile(const AFilename: string): boolean;
var
  Streamer: TJSONStreamer;
  s: TStringList;
begin
  Result := True;
  s := TStringList.Create;
  try
    Streamer := TJSONStreamer.Create(nil);
    Streamer.Options := Streamer.Options + [jsoUseFormatString];
    s.AddText(Streamer.ObjectToJSONString(Self));
    try
      s.SaveToFile(AFileName);
    except
      // Eat the exception
      On E: Exception do
        Result := False;
    end;
  finally
    Streamer.Free;
    s.Free;
  end;
end;


function TFoobotDataObject.SaveToFile(const AFilename: string): boolean;
var
  Streamer: TJSONStreamer;
  s: TStringList;
begin
  Result := True;
  s := TStringList.Create;
  try
    Streamer := TJSONStreamer.Create(nil);
    Streamer.Options := Streamer.Options + [jsoUseFormatString];
    s.AddText(Streamer.ObjectToJSONString(Self));
    try
      s.SaveToFile(AFileName);
    except
      // Eat the exception
      On E: Exception do
        Result := False;
    end;
  finally
    Streamer.Free;
    s.Free;
  end;
end;

end.
