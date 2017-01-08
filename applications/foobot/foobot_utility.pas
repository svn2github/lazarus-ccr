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
  C_TIME = 0;
  C_PM = 1;
  C_TMP = 2;
  C_HUM = 3;
  C_CO2 = 4;
  C_VOC = 5;
  C_ALLPOLLU = 6;
  C_NONE = 7;

type
  TDataFetchType = (dfLast, dfStartEnd);
  TSensorType = (st_time, st_pm, st_tmp, st_hum, st_co2, st_voc, st_allpollu);
  TAlertType = (at_high, at_low);

  TAlertRec = record
    AlertTriggered: boolean;
    AlertTime: TDateTime;
    AlertType: TAlertType;
    AlertValue: variant;
  end;

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
  // Used to fetch server data
  HttpClient: TFPHTTPClient;
  // Holds identity values for multiple Foobots
  FoobotIdentityObject: TFoobotIdentityObject;
  // Holds data for current Foobot
  FoobotDataObject: TFoobotDataObject;

  sAuthenticationKey: string;
  SensorType: TSensorType; // st_time, st_pm, st_tmp etc.
  // Boolean to enable/disable serialisation of HighLows.  Default = TRUE
  SaveLoadHighLows: boolean;
  // Boolean to enable/disable Trigger functions
  UseTriggers: boolean;
  // Used in data fetch
  TheCurrentFoobot: integer;
  // INIFile located in GetAppConfig(false) folder
  HLINI: TIniFile;
  // Dynamic arrays - easier access to datapoints
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

  // [0=Low(at_low)..1=High(at_high), C_PM..C_ALLPOLLU]
  // Dynamic in case of future changes (e.g. more triggers)
  FooBotTriggerArray: array of array of variant;
  // Set by FoobotDataObjectToArrays
  AlertRec: array [C_TIME..C_ALLPOLLU] of TAlertRec;
  giCount: integer; // used in initialization

implementation

function SaveHighLows: boolean;
  // Save values to an INI data file
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
  HLINI.WriteFloat(sFoobotName, 'pmHigh', double(FoobotDataHighs[C_PM]));
  HLINI.WriteDateTime(sFoobotName, 'pmHighTime', TDateTime(FoobotDataHighTimes[C_PM]));
  HLINI.WriteFloat(sFoobotName, 'pmLow', double(FoobotDataLows[C_PM]));
  HLINI.WriteDateTime(sFoobotName, 'pmLowTime', TDateTime(FoobotDataLowTimes[C_PM]));
  // Temp
  HLINI.WriteFloat(sFoobotName, 'tmpHigh', double(FoobotDataHighs[C_TMP]));
  HLINI.WriteDateTime(sFoobotName, 'tmpHighTime', TDateTime(FoobotDataHighTimes[C_TMP]));
  HLINI.WriteFloat(sFoobotName, 'tmpLow', double(FoobotDataLows[C_TMP]));
  HLINI.WriteDateTime(sFoobotName, 'tmpLowTime', TDateTime(FoobotDataLowTimes[C_TMP]));
  // Humidity
  HLINI.WriteFloat(sFoobotName, 'humHigh', double(FoobotDataHighs[C_HUM]));
  HLINI.WriteDateTime(sFoobotName, 'humHighTime', TDateTime(FoobotDataHighTimes[C_HUM]));
  HLINI.WriteFloat(sFoobotName, 'humLow', double(FoobotDataLows[C_HUM]));
  HLINI.WriteDateTime(sFoobotName, 'humLowTime', TDateTime(FoobotDataLowTimes[C_HUM]));
  // CO2
  HLINI.WriteInteger(sFoobotName, 'co2High', integer(FoobotDataHighs[C_CO2]));
  HLINI.WriteDateTime(sFoobotName, 'co2HighTime', TDateTime(FoobotDataHighTimes[C_CO2]));
  HLINI.WriteInteger(sFoobotName, 'co2Low', integer(FoobotDataLows[C_CO2]));
  HLINI.WriteDateTime(sFoobotName, 'co2LowTime', TDateTime(FoobotDataLowTimes[C_CO2]));
  //  Volatile Compounds
  HLINI.WriteInteger(sFoobotName, 'vocHigh', integer(FoobotDataHighs[C_VOC]));
  HLINI.WriteDateTime(sFoobotName, 'vocHighTime', TDateTime(FoobotDataHighTimes[C_VOC]));
  HLINI.WriteInteger(sFoobotName, 'vocLow', integer(FoobotDataLows[C_VOC]));
  HLINI.WriteDateTime(sFoobotName, 'vocLowTime', TDateTime(FoobotDataLowTimes[C_VOC]));
  // All Pollution
  HLINI.WriteFloat(sFoobotName, 'allpolluHigh', double(FoobotDataHighs[C_ALLPOLLU]));
  HLINI.WriteDateTime(sFoobotName, 'allpolluHighTime',
    TDateTime(FoobotDataHighTimes[C_ALLPOLLU]));
  HLINI.WriteFloat(sFoobotName, 'allpolluLow', double(FoobotDataLows[C_ALLPOLLU]));
  HLINI.WriteDateTime(sFoobotName, 'allpolluLowTime',
    TDateTime(FoobotDataLowTimes[C_ALLPOLLU]));
  Result := True;
end;

function LoadHighLows: boolean;
  // Load values from an INI data file
var
  sFoobotName: string;
begin
  if SaveLoadHighLows = False then
  begin
    ShowMessage('Unable to load All-Time stats');
    Exit(False);
  end;
  sFoobotName := FoobotIdentityObject.FoobotIdentityList[TheCurrentFoobot].Name;
  if not Assigned(HLINI) then
    HLINI := TIniFile.Create(ChangeFileExt(GetAppConfigFile(False), '.ini'));
  // Make sure the High-Lows are for the current Foobot
  if (HLINI.ReadString('Foobot', 'CurrentFoobotName', 'unknown') <> sFoobotName) then
    Exit(False);

  // Particulates
  FoobotDataHighs[C_PM] := HLINI.ReadFloat(sFoobotName, 'pmHigh', 0);
  FoobotDataHighTimes[C_PM] := HLINI.ReadDateTime(sFoobotName, 'pmHighTime', Now);
  FoobotDataLows[C_PM] := HLINI.ReadFloat(sFoobotName, 'pmLow', 0);
  FoobotDataLowTimes[C_PM] := HLINI.ReadDateTime(sFoobotName, 'pmLowTime', Now);
  // Temp
  FoobotDataHighs[C_TMP] := HLINI.ReadFloat(sFoobotName, 'tmpHigh', 0);
  FoobotDataHighTimes[C_TMP] := HLINI.ReadDateTime(sFoobotName, 'tmpHighTime', Now);
  FoobotDataLows[C_TMP] := HLINI.ReadFloat(sFoobotName, 'tmpLow', 0);
  FoobotDataLowTimes[C_TMP] := HLINI.ReadDateTime(sFoobotName, 'tmpLowTime', Now);
  // Humidity
  FoobotDataHighs[C_HUM] := HLINI.ReadFloat(sFoobotName, 'humHigh', 0);
  FoobotDataHighTimes[C_HUM] := HLINI.ReadDateTime(sFoobotName, 'humHighTime', Now);
  FoobotDataLows[C_HUM] := HLINI.ReadFloat(sFoobotName, 'humLow', 0);
  FoobotDataLowTimes[C_HUM] := HLINI.ReadDateTime(sFoobotName, 'humLowTime', Now);
  // CO2
  FoobotDataHighs[C_CO2] := HLINI.ReadInteger(sFoobotName, 'co2High', 0);
  FoobotDataHighTimes[C_CO2] := HLINI.ReadDateTime(sFoobotName, 'co2HighTime', Now);
  FoobotDataLows[C_CO2] := HLINI.ReadInteger(sFoobotName, 'co2Low', 0);
  FoobotDataLowTimes[C_CO2] := HLINI.ReadDateTime(sFoobotName, 'co2LowTime', Now);
  // Volatile Compounds
  FoobotDataHighs[C_VOC] := HLINI.ReadInteger(sFoobotName, 'vocHigh', 0);
  FoobotDataHighTimes[C_VOC] := HLINI.ReadDateTime(sFoobotName, 'vocHighTime', Now);
  FoobotDataLows[C_VOC] := HLINI.ReadInteger(sFoobotName, 'vocLow', 0);
  FoobotDataLowTimes[C_VOC] := HLINI.ReadDateTime(sFoobotName, 'vocLowTime', Now);
  // All Pollution
  FoobotDataHighs[C_ALLPOLLU] := HLINI.ReadFloat(sFoobotName, 'allpolluHigh', 0);
  FoobotDataHighTimes[C_ALLPOLLU] :=
    HLINI.ReadDateTime(sFoobotName, 'allpolluHighTime', Now);
  FoobotDataLows[C_ALLPOLLU] := HLINI.ReadFloat(sFoobotName, 'allpolluLow', 0);
  FoobotDataLowTimes[C_ALLPOLLU] :=
    HLINI.ReadDateTime(sFoobotName, 'allpolluLowTime', Now);
  Result := True;
end;

// Function to make the Foobot data accessible
// Also sets the HighLow array values
// Also sets triggers
{
TAlertRec = Record
  Triggered:Boolean;
  AlertTime:TDateTime;
  AlertType:TAlertType;
  AlertValue:Variant;
end;
}
function FoobotDataObjectToArrays: boolean;
var
  J, K: integer;
  Mydatapoint: variant;
  iUnixSecs: int64;
  // ========= Internal routines start ===========
  procedure SetHigh(iMember: integer; aValue: variant; aDateTime: TDateTime);
  begin
    if aValue > FoobotDataHighs[iMember] then
    begin
      FoobotDataHighs[iMember] := aValue;
      FoobotDataHighTimes[iMember] := aDateTime;
      SaveHighLows;
    end;
    if ((UseTriggers = True) and (FooBotTriggerArray[1, iMember] <> 0)) then
    begin
      // Process High Trigger
      // Sets AlertRec record
      if (aValue > FooBotTriggerArray[1, iMember]) then
      begin
        AlertRec[iMember].AlertTriggered := True;
        AlertRec[iMember].AlertTime := aDateTime;
        AlertRec[iMember].AlertType := at_high;
        AlertRec[iMember].AlertValue := aValue;
      end
      else
        AlertRec[iMember].AlertTriggered := False;
    end;
  end;

  procedure SetLow(iMember: integer; aValue: variant; aDateTime: TDateTime);
  begin
    if (aValue < FoobotDataLows[iMember]) or (FoobotDataLows[iMember] = 0) then
    begin
      FoobotDataLows[iMember] := aValue;
      FoobotDataLowTimes[iMember] := aDateTime;
      SaveHighLows;
    end;
    if ((UseTriggers = True) and (FooBotTriggerArray[0, iMember] <> 0)) then
    begin
      // Process Low Trigger
      // Sets AlertRec record
      if (aValue < FooBotTriggerArray[1, iMember]) then
      begin
        AlertRec[iMember].AlertTriggered := True;
        AlertRec[iMember].AlertTime := aDateTime;
        AlertRec[iMember].AlertType := at_low;
        AlertRec[iMember].AlertValue := aValue;
      end
      else
        AlertRec[iMember].AlertTriggered := False;
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
        C_TIME: // First field is a DateTime
        begin
          iUnixSecs := int64(Mydatapoint);
          SetLength(FoobotData_time, K + 1);
          FoobotData_time[K] := UnixToDateTime(iUnixSecs);
        end;
        C_PM: // Particulate matter
        begin
          SetLength(FoobotData_pm, K + 1);
          FoobotData_pm[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_pm[K], FoobotData_time[K]);
          SetLow(J, FoobotData_pm[K], FoobotData_time[K]);
        end;
        C_TMP: // Temperature
        begin
          SetLength(FoobotData_tmp, K + 1);
          FoobotData_tmp[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_tmp[K], FoobotData_time[K]);
          SetLow(J, FoobotData_tmp[K], FoobotData_time[K]);
        end;
        C_HUM: // Humidity
        begin
          SetLength(FoobotData_hum, K + 1);
          FoobotData_hum[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_hum[K], FoobotData_time[K]);
          SetLow(J, FoobotData_hum[K], FoobotData_time[K]);
        end;
        C_CO2: // CO2
        begin
          SetLength(FoobotData_co2, K + 1);
          FoobotData_co2[K] := integer(MyDataPoint);
          SetHigh(J, FoobotData_co2[K], FoobotData_time[K]);
          SetLow(J, FoobotData_co2[K], FoobotData_time[K]);
        end;
        C_VOC: // Volatile compounds
        begin
          SetLength(FoobotData_voc, K + 1);
          FoobotData_voc[K] := integer(MyDataPoint);
          SetHigh(J, FoobotData_voc[K], FoobotData_time[K]);
          SetLow(J, FoobotData_voc[K], FoobotData_time[K]);
        end;
        C_ALLPOLLU: // All Pollution
        begin
          SetLength(FoobotData_allpollu, K + 1);
          FoobotData_allpollu[K] := double(MyDataPoint);
          SetHigh(J, FoobotData_allpollu[K], FoobotData_time[K]);
          SetLow(J, FoobotData_allpollu[K], FoobotData_time[K]);
        end;
        else raise Exception.Create('Error in FoobotDataObjectToArrays Case');
      end; // of Case
    end;
  end;

end;


function SetHighTrigger(const aSensor: TSensorType; const aValue: variant): boolean;
begin
  Result := False;
  if UseTriggers = False then
    Exit;
  if aValue <> FooBotTriggerArray[1, Ord(aSensor)] then
  begin
    FooBotTriggerArray[1, Ord(aSensor)] := aValue;
    Result := True;
  end;
end;

function SetLowTrigger(const aSensor: TSensorType; const aValue: variant): boolean;
begin
  Result := False;
  if UseTriggers = False then
    Exit;
  if aValue <> FooBotTriggerArray[0, Ord(aSensor)] then
  begin
    FooBotTriggerArray[0, Ord(aSensor)] := aValue;
    Result := True;
  end;
end;

// Data cleaning functions
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

// Authentication functions
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
    // Empty
  end;

end;

// This function must succeed before calling FetchFoobotData
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

// Function FetchFoobotIdentity must be called before this one
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
    TheCurrentFoobot := 0;
    SetLength(FooBotTriggerArray, 2, Succ(C_ALLPOLLU));
    SaveLoadHighLows := True; // Default
    UseTriggers := False; // Defaul
    for giCount := C_PM to C_ALLPOLLU do
    begin
      AlertRec[giCount].AlertTriggered := False;
      AlertRec[giCount].AlertTime := Now;
      AlertRec[giCount].AlertType := at_low;
      AlertRec[giCount].AlertValue := 0;
    end;
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
    SetLength(FooBotTriggerArray, 0, 0);
  end;

end.
