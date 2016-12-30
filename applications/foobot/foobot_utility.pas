unit foobot_utility;

{ Foobot Utilities

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

VERSION HISTORY
===============
* HighLow routines
* Use GetAppGonfigFile for IniFile location
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  foobot_httpclient, foobot_objects, fpjson, fpjsonrtti, base64, variants,
  DateUtils, INIFiles;

const
  FOOBOT_USER_URL = 'https://api.foobot.io/v2/user/%s/login/';
  FOOBOT_IDENTITY_URL = 'https://api.foobot.io/v2/owner/%s/device/';
  FOOBOT_DATA_LAST_URL = 'https://api.foobot.io/v2/device/%s/datapoint/%s/%s/%s/';
  FOOBOT_DATA_START_FINISH_URL =
    'https://api.foobot.io/v2/device/%s/datapoint/%s/%s/%s/';
  HIGHLOWMAX = 6;

type
  TDataFetchType = (dfLast, dfStartEnd);
  TSensorType = (st_time, st_pm, st_tmp, st_hum, st_co2, st_voc, st_allpollu);

function EncodeStringBase64(const s: string): string;
function FetchAuthenticationKey(aUsername, aUserPassword: string): boolean;

// Populates FoobotIdentityObject.TFoobotIdentityList collection
function FetchFoobotIdentity(aUsername, aSecretKey: string): boolean;

// Populates FoobotDataObject
function FetchFoobotData(DataFetchType: TDataFetchType = dfLast;
  iCurrentFoobot: integer = 0; iLastIntervalSeconds: integer = 3600;
  iLastAverageBySeconds: integer = 0; iStartTimeSeconds: int64 = 0;
  iEndTimeSeconds: int64 = 0; aSecretKey: string = 'unknown'): boolean;

// Populates datapoint arrays from FoobotIdentityObject for easy access
// - also populates HighLow arrays
function FoobotDataObjectToArrays: boolean;

// Utility functions
function ResetArrays: boolean;
function ResetObjects: boolean;
function ResetHighLows: boolean;
function SaveHighLows: boolean;
function LoadHighLows: boolean;

var
  HttpClient: TFPHTTPClient;
  FoobotIdentityObject: TFoobotIdentityObject;
  FoobotDataObject: TFoobotDataObject;
  sAuthenticationKey: string;
  SensorType: TSensorType;
  SaveLoadHighLows: boolean;
  TheCurrentFoobot: integer;
  HLINI: TIniFile;
  // Easier access to datapoints
  // Call FoobotDataObjectToArrays to populate them
  FoobotData_time: array of TDateTime;
  FoobotData_pm: array of double;
  FoobotData_tmp: array of double;
  FoobotData_hum: array of double;
  FoobotData_co2: array of integer;
  FoobotData_voc: array of integer;
  FoobotData_allpollu: array of double;
  // Set in FoobotDataObjectToArrays
  FoobotDataHighs: array[0..HIGHLOWMAX] of variant;
  FoobotDataLows: array[0..HIGHLOWMAX] of variant;
  FoobotDataHighTimes: array[0..HIGHLOWMAX] of variant;
  FoobotDataLowTimes: array[0..HIGHLOWMAX] of variant;

implementation

function SaveHighLows: boolean;
var
  sFoobotName: string;
begin
  if SaveLoadHighLows = False then
    Exit(False);
  sFoobotName := FoobotIdentityObject.FoobotIdentityList[TheCurrentFoobot].Name;
  if not Assigned(HLINI) then
    HLINI := TIniFile.Create(ChangeFileExt(GetAppConfigFile(False), '.ini'));
  // Store current Foobot info
  HLINI.WriteInteger('Foobot', 'CurrentFoobot', TheCurrentFoobot);
  HLINI.WriteString('Foobot', 'CurrentFoobotName', sFoobotName);

  // Particulates
  HLINI.WriteFloat(sFoobotName, 'pmHigh', double(FoobotDataHighs[1]));
  HLINI.WriteDateTime(sFoobotName, 'pmHighTime', TDateTime(FoobotDataHighTimes[1]));
  HLINI.WriteFloat(sFoobotName, 'pmLow', double(FoobotDataLows[1]));
  HLINI.WriteDateTime(sFoobotName, 'pmLowTime', TDateTime(FoobotDataLowTimes[1]));
  // Temp
  HLINI.WriteFloat(sFoobotName, 'tmpHigh', double(FoobotDataHighs[2]));
  HLINI.WriteDateTime(sFoobotName, 'tmpHighTime', TDateTime(FoobotDataHighTimes[2]));
  HLINI.WriteFloat(sFoobotName, 'tmpLow', double(FoobotDataLows[2]));
  HLINI.WriteDateTime(sFoobotName, 'tmpLowTime', TDateTime(FoobotDataLowTimes[2]));
  // Humidity
  HLINI.WriteFloat(sFoobotName, 'humHigh', double(FoobotDataHighs[3]));
  HLINI.WriteDateTime(sFoobotName, 'humHighTime', TDateTime(FoobotDataHighTimes[3]));
  HLINI.WriteFloat(sFoobotName, 'humLow', double(FoobotDataLows[3]));
  HLINI.WriteDateTime(sFoobotName, 'humLowTime', TDateTime(FoobotDataLowTimes[3]));
  // CO2
  HLINI.WriteInteger(sFoobotName, 'co2High', integer(FoobotDataHighs[4]));
  HLINI.WriteDateTime(sFoobotName, 'co2HighTime', TDateTime(FoobotDataHighTimes[4]));
  HLINI.WriteInteger(sFoobotName, 'co2Low', integer(FoobotDataLows[4]));
  HLINI.WriteDateTime(sFoobotName, 'co2LowTime', TDateTime(FoobotDataLowTimes[4]));
  //  Volatile Compounds
  HLINI.WriteInteger(sFoobotName, 'vocHigh', integer(FoobotDataHighs[5]));
  HLINI.WriteDateTime(sFoobotName, 'vocHighTime', TDateTime(FoobotDataHighTimes[5]));
  HLINI.WriteInteger(sFoobotName, 'vocLow', integer(FoobotDataLows[5]));
  HLINI.WriteDateTime(sFoobotName, 'vocLowTime', TDateTime(FoobotDataLowTimes[5]));
  // All Pollution
  HLINI.WriteFloat(sFoobotName, 'allpolluHigh', double(FoobotDataHighs[6]));
  HLINI.WriteDateTime(sFoobotName, 'allpolluHighTime', TDateTime(FoobotDataHighTimes[6]));
  HLINI.WriteFloat(sFoobotName, 'allpolluLow', double(FoobotDataLows[6]));
  HLINI.WriteDateTime(sFoobotName, 'allpolluLowTime', TDateTime(FoobotDataLowTimes[6]));
end;

function LoadHighLows: boolean;
var
  sFoobotName: string;
begin
  if SaveLoadHighLows = False then
    Exit(False);
  sFoobotName := FoobotIdentityObject.FoobotIdentityList[TheCurrentFoobot].Name;
  if not Assigned(HLINI) then
    HLINI := TIniFile.Create(ChangeFileExt(GetAppConfigFile(False), '.ini'));
  // Make sure the High-Lows are for the current Foobot
  if (HLINI.ReadString('Foobot', 'CurrentFoobotName', 'unknown') <> sFoobotName) then
    Exit(False);

  // Particulates
  FoobotDataHighs[1] := HLINI.ReadFloat(sFoobotName, 'pmHigh', 0);
  FoobotDataHighTimes[1] := HLINI.ReadDateTime(sFoobotName, 'pmHighTime', Now);
  FoobotDataLows[1] := HLINI.ReadFloat(sFoobotName, 'pmLow', 0);
  FoobotDataLowTimes[1] := HLINI.ReadDateTime(sFoobotName, 'pmLowTime', Now);
  // Temp
  FoobotDataHighs[2] := HLINI.ReadFloat(sFoobotName, 'tmpHigh', 0);
  FoobotDataHighTimes[2] := HLINI.ReadDateTime(sFoobotName, 'tmpHighTime', Now);
  FoobotDataLows[2] := HLINI.ReadFloat(sFoobotName, 'tmpLow', 0);
  FoobotDataLowTimes[2] := HLINI.ReadDateTime(sFoobotName, 'tmpLowTime', Now);
  // Humidity
  FoobotDataHighs[3] := HLINI.ReadFloat(sFoobotName, 'humHigh', 0);
  FoobotDataHighTimes[3] := HLINI.ReadDateTime(sFoobotName, 'humHighTime', Now);
  FoobotDataLows[3] := HLINI.ReadFloat(sFoobotName, 'humLow', 0);
  FoobotDataLowTimes[3] := HLINI.ReadDateTime(sFoobotName, 'humLowTime', Now);
  // CO2
  FoobotDataHighs[4] := HLINI.ReadInteger(sFoobotName, 'co2High', 0);
  FoobotDataHighTimes[4] := HLINI.ReadDateTime(sFoobotName, 'co2HighTime', Now);
  FoobotDataLows[4] := HLINI.ReadInteger(sFoobotName, 'co2Low', 0);
  FoobotDataLowTimes[4] := HLINI.ReadDateTime(sFoobotName, 'co2LowTime', Now);
  // Volatile Compounds
  FoobotDataHighs[5] := HLINI.ReadInteger(sFoobotName, 'vocHigh', 0);
  FoobotDataHighTimes[5] := HLINI.ReadDateTime(sFoobotName, 'vocHighTime', Now);
  FoobotDataLows[5] := HLINI.ReadInteger(sFoobotName, 'vocLow', 0);
  FoobotDataLowTimes[5] := HLINI.ReadDateTime(sFoobotName, 'vocLowTime', Now);
  // All Pollution
  FoobotDataHighs[6] := HLINI.ReadFloat(sFoobotName, 'allpolluHigh', 0);
  FoobotDataHighTimes[6] := HLINI.ReadDateTime(sFoobotName, 'allpolluHighTime', Now);
  FoobotDataLows[6] := HLINI.ReadFloat(sFoobotName, 'allpolluLow', 0);
  FoobotDataLowTimes[6] := HLINI.ReadDateTime(sFoobotName, 'allpolluLowTime', Now);
end;

// ToDo: Multiple Foobots?
function FoobotDataObjectToArrays: boolean;
var
  J, K: integer;
  Mydatapoint: variant;
  {
  dtDate, dtStart, dtEnd: TDateTime;
  sStart, sEnd: string;
  }
  iUnixSecs: int64;
  // ========= Internal routines start ===========
  procedure SetHigh(iMember: integer; aValue: variant; aDateTime: TDateTime);
  begin
    if aValue > FoobotDataHighs[iMember] then
    begin
      FoobotDataHighs[iMember] := aValue;
      FoobotDataHighTimes[iMember] := aDateTime;
    end;
  end;

  procedure SetLow(iMember: integer; aValue: variant; aDateTime: TDateTime);
  begin
    if (aValue < FoobotDataLows[iMember]) or (FoobotDataLows[iMember] = 0) then
    begin
      FoobotDataLows[iMember] := aValue;
      FoobotDataLowTimes[iMember] := aDateTime;
    end;
  end;
  // ========== Internal routines end =============
begin
  ResetArrays;
  Result := True;
  LoadHighLows;
  if FoobotIdentityObject.FoobotIdentityList.Count = 0 then
    Exit(False);
  if FooBotDataObject.sensors.Count = 0 then
    Exit(False);
  if FooBotDataObject.units.Count = 0 then
    Exit(False);
  // J=Column, K=Row
  for K := VarArrayLowBound(FoobotDataObject.datapoints, 1)
    to VarArrayHighBound(FoobotDataObject.datapoints, 1) do
  begin
    for J := VarArrayLowBound(FoobotDataObject.datapoints[K], 1)
      to VarArrayHighBound(FoobotDataObject.datapoints[K], 1) do
    begin
      Mydatapoint := FoobotDataObject.datapoints[K][J];
      case J of
        0: // First field is a DateTime
        begin
          iUnixSecs := int64(Mydatapoint);
          SetLength(FoobotData_time, K + 1);
          FoobotData_time[K] := UnixToDateTime(iUnixSecs);
        end;
        1: // Particulate matter
        begin
          SetLength(FoobotData_pm, K + 1);
          FoobotData_pm[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_pm[K], FoobotData_time[K]);
          SetLow(J, FoobotData_pm[K], FoobotData_time[K]);
        end;
        2: // Temperature
        begin
          SetLength(FoobotData_tmp, K + 1);
          FoobotData_tmp[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_tmp[K], FoobotData_time[K]);
          SetLow(J, FoobotData_tmp[K], FoobotData_time[K]);
        end;
        3: // Humidity
        begin
          SetLength(FoobotData_hum, K + 1);
          FoobotData_hum[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_hum[K], FoobotData_time[K]);
          SetLow(J, FoobotData_hum[K], FoobotData_time[K]);
        end;
        4: // CO2
        begin
          SetLength(FoobotData_co2, K + 1);
          FoobotData_co2[K] := integer(MyDataPoint);
          SetHigh(J, FoobotData_co2[K], FoobotData_time[K]);
          SetLow(J, FoobotData_co2[K], FoobotData_time[K]);
        end;
        5: // Volatile compounds
        begin
          SetLength(FoobotData_voc, K + 1);
          FoobotData_voc[K] := integer(MyDataPoint);
          SetHigh(J, FoobotData_voc[K], FoobotData_time[K]);
          SetLow(J, FoobotData_voc[K], FoobotData_time[K]);
        end;
        6: // All Pollution
        begin
          SetLength(FoobotData_allpollu, K + 1);
          FoobotData_allpollu[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_allpollu[K], FoobotData_time[K]);
          SetLow(J, FoobotData_allpollu[K], FoobotData_time[K]);
        end;
      end; // of Case
    end;
  end;
  SaveHighLows;
end;

function ResetHighLows: boolean;
var
  iCount: integer;
begin
  for iCount := 0 to HIGHLOWMAX do
  begin
    FoobotDataHighs[iCount] := 0;
    FoobotDataLows[iCount] := 0;
  end;
  Result := True;
end;

function ResetArrays: boolean;
begin
  Result := True;
  try
    SetLength(FoobotData_time, 0);
    SetLength(FoobotData_pm, 0);
    SetLength(FoobotData_tmp, 0);
    SetLength(FoobotData_hum, 0);
    SetLength(FoobotData_co2, 0);
    SetLength(FoobotData_voc, 0);
    SetLength(FoobotData_allpollu, 0);
  except
    Result := False;
    raise;
  end;
end;

function ResetObjects: boolean;
var
  J, K: integer;
begin
  Result := True;
  try
    for K := VarArrayLowBound(FoobotDataObject.datapoints, 1)
      to VarArrayHighBound(FoobotDataObject.datapoints, 1) do
      for J := VarArrayLowBound(FoobotDataObject.datapoints[K], 1)
        to VarArrayHighBound(FoobotDataObject.datapoints[K], 1) do
        FoobotDataObject.datapoints[K][J] := 0;
    FooBotDataObject.sensors.Clear;
    FooBotDataObject.units.Clear;
    FoobotIdentityObject.FoobotIdentityList.Clear;
  except
    Result := False;
    raise;
  end;
end;

function EncodeStringBase64(const s: string): string;

var
  outstream: TStringStream;
  encoder: TBase64EncodingStream;
begin
  outstream := TStringStream.Create('');
  try
    encoder := TBase64EncodingStream.Create(outstream);
    try
      encoder.Write(s[1], length(s));
    finally
      encoder.Free;
    end;
    outstream.position := 0;
    Result := outstream.readstring(outstream.size);
  finally
    outstream.Free;
  end;
end;

function FetchAuthenticationKey(aUsername, aUserPassword: string): boolean;
var
  sRequestURL: string;
  iCount: integer;
  JSON: TJSONStringType;
begin
  //  FOOBOT_USER_URL = 'http://api.foobot.io/v2/user/%s/login/';
  //  sAuthenticationKey
  //  Looking for "x-auth-token"
  Result := False;
  try
    with httpclient do
    begin
      ResponseHeaders.NameValueSeparator := ':';
      AddHeader('password', aUserPassword);
      // ShowMessage(EncodeURLElement(aUsername));
      sRequestURL := Format(FOOBOT_USER_URL, [EncodeURLElement(aUsername)]);
      try
        JSON := Get(sRequestURL);
        if ResponseStatusCode <> 200 then
        begin
          ShowMessageFmt('Failed - Foobot server refused with code %d',
            [ResponseStatusCode]);
          Exit(False);
        end;
      finally
        ShowMessage(JSON);
        for iCount := 0 to Pred(ResponseHeaders.Count) do
          ShowMessage(ResponseHeaders[iCount]);
      end;
      Result := True;
    end;
  finally
  end;

end;

function FetchFoobotIdentity(aUsername, aSecretKey: string): boolean;
var
  sUserNameURL: string;
  JSON: TJSONStringType;
  DeStreamer: TJSONDeStreamer;
begin
  Result := True; // Assume success: Look for failure
  sAuthenticationKey := aSecretKey;
  try
    with httpclient do
    begin
      DeStreamer := TJSONDeStreamer.Create(nil);
      DeStreamer.Options := [jdoIgnorePropertyErrors];
      sUserNameURL := Format(FOOBOT_IDENTITY_URL, [aUsername]);
      ResponseHeaders.NameValueSeparator := ':';
      AddHeader('Accept', 'application/json;charset=UTF-8');
      AddHeader('X-API-KEY-TOKEN', aSecretKey);
      JSON := Get(sUserNameURL);
      if (ResponseStatusCode <> 200) then
        case ResponseStatusCode of
          429:
          begin
            ShowMessageFmt('Cannot retieve data - too many requests to the Foobot server%s%s',
              [LineEnding, JSON]);
            Exit(False);
          end;
          else
          begin
            ShowMessageFmt('Cannot retieve data - Foobot server refused with code %d',
              [ResponseStatusCode]);
            Exit(False);
          end;
        end;
      try
        // Stream it to the object list
        DeStreamer.JSONToObject(JSON, FoobotIdentityObject.FoobotIdentityList);
      except
        On E: Exception do
          showmessagefmt('Cannot retieve data - Foobot server refused with code %s',
            [E.Message]);
        On E: Exception do
          Result := False;
      end;
    end;
  finally
    DeStreamer.Free;
  end;
end;

function FetchFoobotData(DataFetchType: TDataFetchType;
  iCurrentFoobot, iLastIntervalSeconds, iLastAverageBySeconds: integer;
  iStartTimeSeconds, iEndTimeSeconds: int64; aSecretKey: string): boolean;
var
  sUserNameURL: string;
  JSON: TJSONStringType;
  DeStreamer: TJSONDeStreamer;
  uuid: string;
  //FOOBOT_DATA_LAST_URL = 'http://api.foobot.io/v2/device/%s/datapoint/%s/%s/%s/';
  //FOOBOT_DATA_START_FINISH_URL = 'http://api.foobot.io/v2/device/%s/datapoint/%s/%s/%s/';
begin
  Result := True; // Assume success: Look for failure
  TheCurrentFoobot := iCurrentFoobot;
  // Checks for integrity
  if (FoobotIdentityObject.FoobotIdentityList.Count = 0) then
    Exit(False);
  if (DataFetchType = dfStartEnd) and ((iStartTimeSeconds = 0) or
    (iEndTimeSeconds = 0)) then
    Exit(False);
  if (aSecretKey = 'unknown') then
    Exit(False);

  try
    with httpclient do
    begin
      DeStreamer := TJSONDeStreamer.Create(nil);
      DeStreamer.Options := [jdoIgnorePropertyErrors];
      // secretkey := INI.ReadString('Foobot', 'Secret Key', '');
      uuid := FoobotIdentityObject.FoobotIdentityList.Items[iCurrentFoobot].uuid;
      case DataFetchType of
        dfLast:
          sUserNameURL := Format(FOOBOT_DATA_LAST_URL,
            [uuid, IntToStr(iLastIntervalSeconds), 'last',
            IntToStr(iLastAverageBySeconds)]);
        dfStartEnd:
          sUserNameURL := Format(FOOBOT_DATA_START_FINISH_URL,
            [uuid, IntToStr(iStartTimeSeconds), IntToStr(iEndTimeSeconds),
            IntToStr(iLastAverageBySeconds)]);
        else
        begin
          Result := False;
          Exit;
        end;
      end;
      ResponseHeaders.NameValueSeparator := ':';

      AddHeader('Accept', 'application/json;charset=UTF-8');
      AddHeader('X-API-KEY-TOKEN', aSecretKey);
      JSON := Get(sUserNameURL);
      if (ResponseStatusCode <> 200) then
        case ResponseStatusCode of
          429:
          begin
            ShowMessageFmt('Failed - Too many requests to the Foobot server%s%s',
              [LineEnding, JSON]);
            Exit(False);
          end;
          else
          begin
            ShowMessageFmt('Failed - Foobot server refused with code %d',
              [ResponseStatusCode]);
            Exit(False);
          end;
        end;
      try
        // Stream it to the object list
        DeStreamer.JSONToObject(JSON, FoobotDataObject);
      except
        On E: Exception do
          showmessagefmt('Failed - Foobot server refused with code %s', [E.Message]);
        On E: Exception do
          Result := False;
      end;
    end;
  finally
    DeStreamer.Free;
  end;
end;

initialization
  begin
    HttpClient := TFPHTTPClient.Create(nil);
    FoobotIdentityObject := TFoobotIdentityObject.Create;
    FoobotDataObject := TFoobotDataObject.Create;
    SaveLoadHighLows := True;
    TheCurrentFoobot := 0;
  end;

finalization
  begin
    if Assigned(HLINI) then
      FreeAndNil(HLINI);
    FreeAndNil(HttpClient);
    FreeAndNil(FoobotIdentityObject);
    FreeAndNil(FoobotDataObject);
    SetLength(FoobotData_time, 0);
    SetLength(FoobotData_pm, 0);
    SetLength(FoobotData_tmp, 0);
    SetLength(FoobotData_hum, 0);
    SetLength(FoobotData_co2, 0);
    SetLength(FoobotData_voc, 0);
    SetLength(FoobotData_allpollu, 0);
  end;

end.
