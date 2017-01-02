unit umainform;

{ Foobot Monitor

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
V0.0.1.0: Initial commit
V0.0.2.0: Trayicon added
V0.0.3.0: Added Help menu.  Updated Options menu
V0.0.4.0: Graph added
V0.0.5.0: ??
}
{$ifopt D+}
// Debug mode
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries,
  Sensors, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus,
  lclIntf, foobot_utility, uCryptIni, Variants, dateutils, uconfigform;

const
  // Timer milliseconds
  ONEMINUTE = 60000;
  ONEHOUR = ONEMINUTE * 60;
  TWOHOURS = ONEHOUR * 2;
  FOURHOURS = ONEHOUR * 4;
  EIGHTHOURS = ONEHOUR * 8;
  TWENTYFOURHOURS = ONEHOUR * 24;

  // Colours for guages and graph lines
  COL_PM = clGreen;
  COL_TMP = clRed;
  COL_HUM = clMaroon;
  COL_CO2 = clLime;
  COL_VOC = clBlue;
  COL_ALLPOLLU = clFuchsia;

  // Sensor Gauge MINMAX Values
  MIN_PM = 0;
  MAX_PM = 1000;
  MIN_TMP = 0;
  MAX_TMP = 40;
  MIN_HUM = 10;
  MAX_HUM = 100;
  MIN_CO2 = 450;
  MAX_CO2 = 3000;
  MIN_VOC = 125;
  MAX_VOC = 1000;
  MIN_ALLPOLLU = 0;
  MAX_ALLPOLLU = 700;


type

  { Tmainform }

  Tmainform = class(TForm)
    as_allpollu: TAnalogSensor;
    as_co2: TAnalogSensor;
    as_hum: TAnalogSensor;
    as_pm: TAnalogSensor;
    as_tmp: TAnalogSensor;
    as_voc: TAnalogSensor;
    Chart1: TChart;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    lineseries_allpollu: TLineSeries;
    lineseries_voc: TLineSeries;
    lineseries_co2: TLineSeries;
    lineseries_hum: TLineSeries;
    lineseries_tmp: TLineSeries;
    lineseries_pm: TLineSeries;
    grp_chart: TGroupBox;
    grp_pm: TGroupBox;
    grp_tmp: TGroupBox;
    grp_hum: TGroupBox;
    grp_co2: TGroupBox;
    grp_voc: TGroupBox;
    grp_allpollu: TGroupBox;
    grp_highlow: TGroupBox;
    grp_sensorDisplay: TGroupBox;
    lbl_pmhigh: TLabel;
    lbl_tmphigh: TLabel;
    lbl_humhigh: TLabel;
    lbl_co2high: TLabel;
    lbl_vochigh: TLabel;
    lbl_allpolluhigh: TLabel;
    lbl_pmlow: TLabel;
    lbl_tmplow: TLabel;
    lbl_humlow: TLabel;
    lbl_co2low: TLabel;
    lbl_voclow: TLabel;
    lbl_allpollulow: TLabel;
    MainMenu1: TMainMenu;
    mnu_optionsOnlineHelp: TMenuItem;
    mnu_optionsSeperator1: TMenuItem;
    mnu_helpAbout: TMenuItem;
    mnu_help: TMenuItem;
    mnupopup_fileRestore: TMenuItem;
    mnu_pupupClose: TMenuItem;
    mnu_optionsMinimiseToTray: TMenuItem;
    mnu_optionsSaveHighLows: TMenuItem;
    mnu_SampleEvery24Hours: TMenuItem;
    mnu_SampleEvery8Hours: TMenuItem;
    mnu_SampleEvery4Hours: TMenuItem;
    mnu_SampleEvery2Hours: TMenuItem;
    mnu_SampleEvery1Hour: TMenuItem;
    mnu_optionsSampleEvery: TMenuItem;
    mnu_optionsTakeReadingNow: TMenuItem;
    mnu_optionsShowMinimalDisplay: TMenuItem;
    mnu_options: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_file: TMenuItem;
    traypopup: TPopupMenu;
    tmr_foobot: TTimer;
    TrayIcon1: TTrayIcon;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure mnupopup_fileRestoreClick(Sender: TObject);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure mnu_optionsMinimiseToTrayClick(Sender: TObject);
    procedure mnu_optionsOnlineHelpClick(Sender: TObject);
    procedure mnu_optionsSaveHighLowsClick(Sender: TObject);
    procedure mnu_optionsShowMinimalDisplayClick(Sender: TObject);
    procedure mnu_optionsTakeReadingNowClick(Sender: TObject);
    procedure mnu_SampleEvery1HourClick(Sender: TObject);
    procedure mnu_SampleEvery24HoursClick(Sender: TObject);
    procedure mnu_SampleEvery2HoursClick(Sender: TObject);
    procedure mnu_SampleEvery4HoursClick(Sender: TObject);
    procedure mnu_SampleEvery8HoursClick(Sender: TObject);
    procedure tmr_foobotTimer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
  private
    sSecretKey, sFoobotUserName, sUUID: string;
    bShowMinimalDisplay: boolean;
    iFudgeFactor: integer;
    procedure SetUpSensorColours;
    procedure SetUpSensorMinMax;
    procedure DisplayReadings;
    procedure UpdateGuage(Sender: TAnalogSensor; SensorNumber: integer);
    procedure UpdateHighLow(SensorNumber: integer);
    procedure GraphHistory;
    procedure GraphCurrentReading;
  public
    INI: TCryptINIfile;
  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }

procedure Tmainform.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  Icon := Application.Icon;
  INI := TCryptINIfile.Create(GetAppConfigFile(False));
  if INI.IsVirgin then
  begin
    INI.WriteIdent('Gordon Bamber', '(c)2016', 'GPLV2',
      'minesadorada@charcodelvalle.com', True);
  end;
  if not INI.VerifyIdent('41d10218d247980fc5e871b6b7844483') then
  begin
    ShowMessage(Application.Title +
      ' has been tampered wth.  Please re-install from a trusted source.');
    FreeAndNil(INI);
    Application.Terminate;
  end;
  INI.SectionHashing := False;
  ResetHighLows;
  iFudgeFactor := 20; // only needed if height set here
  bShowMinimalDisplay := False;
  TrayIcon1.Icon := Application.Icon;
  TrayIcon1.Hint := Application.Title;
  DateTimeIntervalChartSource1.DateTimeFormat := 'hh:nn';
  SetUpSensorMinMax;
  SetUpSensorColours;
end;

procedure Tmainform.FormActivate(Sender: TObject);
var
  sTempFoobotUserName, sTempSecretKey: string;

begin
  ClientHeight := grp_sensorDisplay.Height + grp_highlow.Height + grp_chart.Height;

  // Allow user to enter values in INIFile
  sTempFoobotUserName := INI.ReadUnencryptedString('Config', 'Foobot User', 'unknown');
  sTempSecretKey := INI.ReadUnencryptedString('Config', 'Secret Key', 'unknown');
  if ((sTempFoobotUserName <> 'unknown') and (sTempSecretKey <> 'unknown')) then
  begin
    INI.WriteString('Foobot', 'Foobot User', sTempFoobotUserName);
    INI.DeleteKey('Config', 'Foobot User');
    INI.WriteString('Foobot', 'Secret Key', sTempSecretKey);
    INI.DeleteKey('Config', 'Secret Key');
  end;
  // Fetch Username and API_KEY
  sFoobotUserName := INI.ReadString('Foobot', 'Foobot User', 'unknown');
  sSecretKey := INI.ReadString('Foobot', 'Secret Key', 'unknown');
  if ((sFoobotUserName <> 'unknown') and (sSecretKey <> 'unknown')) then
  begin
    Hide;
    if FetchFoobotIdentity(sFoobotUserName, sSecretKey) then
    begin
      if FoobotIdentityObject.FoobotIdentityList.Count > 0 then
      begin
        sUUID := FoobotIdentityObject.FoobotIdentityList.Items[0].uuid;
        SaveLoadHighLows := INI.ReadBool('Foobot', 'SaveLoadHighLows', True);
        mnu_optionsSaveHighLows.Checked := SaveLoadHighLows;
        if SaveLoadHighLows then
          LoadHighLows;
        GraphHistory;
        {$IFNDEF DEBUGMODE}
        mnu_optionsTakeReadingNow.Click;
{$ENDIF}
        // Switch off for testing
        tmr_foobot.Interval := ONEHOUR;
        {$IFNDEF DEBUGMODE}
        tmr_foobot.Enabled := True;
{$ENDIF}
        Show;
      end;
    end
    else
      Close;
  end
  else
  begin
    // No valid cfg.  Show config form
    Hide;
    Application.ProcessMessages;
    configform.ShowModal;
    // If user quit without data, then bail out
    if not configform.bValid then
    begin
      Close;
    end;
    // Store encrypted Username and API_KEY
    INI.WriteString('Foobot', 'Foobot User', configform.FoobotUsername);
    INI.WriteString('Foobot', 'Secret Key', configform.FoobotSecretKey);
    //sFoobotUserName := INI.ReadString('Foobot', 'Foobot User', 'unknown');
    //sSecretKey := INI.ReadString('Foobot', 'Secret Key', 'unknown');
    ShowMessage('Click OK to store settings and close the app.' +
      LineEnding + 'New settings are applied on resart.');
    Close;
  end;
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  FreeAndNil(INI);
end;

procedure Tmainform.SetUpSensorColours;
begin
  as_pm.ColorFore := COL_PM;
  lineSeries_pm.SeriesColor := COL_PM;
  as_tmp.ColorFore := COL_TMP;
  lineSeries_tmp.SeriesColor := COL_TMP;
  as_hum.ColorFore := COL_HUM;
  lineSeries_hum.SeriesColor := COL_HUM;
  as_co2.ColorFore := COL_CO2;
  lineSeries_co2.SeriesColor := COL_CO2;
  as_voc.ColorFore := COL_VOC;
  lineSeries_voc.SeriesColor := COL_VOC;
  as_allpollu.ColorFore := COL_ALLPOLLU;
  lineSeries_allpollu.SeriesColor := COL_ALLPOLLU;
end;

procedure Tmainform.SetUpSensorMinMax;
begin
  as_pm.ValueMin := MIN_PM;
  as_pm.ValueMax := MAX_PM;
  as_tmp.ValueMin := MIN_TMP;
  as_tmp.ValueMax := MAX_TMP;
  as_hum.ValueMin := MIN_HUM;
  as_hum.ValueMax := MAX_HUM;
  as_co2.ValueMin := MIN_CO2;
  as_co2.ValueMax := MAX_CO2;
  as_voc.ValueMin := MIN_VOC;
  as_voc.ValueMax := MAX_VOC;
  as_allpollu.ValueMin := MIN_ALLPOLLU;
  as_allpollu.ValueMax := MAX_ALLPOLLU;
end;

procedure Tmainform.FormWindowStateChange(Sender: TObject);
begin
  if mainform.WindowState = wsMinimized then
  begin
    mainform.WindowState := wsNormal;
    mainform.Hide;
    mainform.ShowInTaskBar := stNever;
  end;
end;

procedure Tmainform.mnupopup_fileRestoreClick(Sender: TObject);
begin
  mainform.Show;
end;

procedure Tmainform.mnu_fileExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tmainform.mnu_helpAboutClick(Sender: TObject);
var
  s: string;
begin
  s := Application.Title + LineEnding;
  s += 'Version: ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_APPVERSION, '') +
    LineEnding + LineEnding;
  s += INI.ReadUnencryptedString('ProgramInfo', IDENT_COPYRIGHT, '');
  s += ' by ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_AUTHOR, '') + LineEnding;
  s += 'Licence: ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_LICENSE, '') +
    LineEnding;
  s += 'Made with LCL v ' + INI.ReadUnencryptedString('ProgramInfo',
    IDENT_LCLVERSION, '');
  s += ' FPC v ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_FPCVERSION, '') +
    LineEnding;
  s += 'Compiled ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_LASTCOMPILED, '') +
    LineEnding;
  s += ' for ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_TARGET, '');
  MessageDlg('About ' + Application.Title, s,
    mtInformation, [mbOK], 0);
end;

procedure Tmainform.mnu_optionsMinimiseToTrayClick(Sender: TObject);
begin
  mainform.WindowState := wsMinimized;
  mainform.FormWindowStateChange(Self);
end;

procedure Tmainform.mnu_optionsOnlineHelpClick(Sender: TObject);
begin
  OpenURL('http://wiki.freepascal.org/Foobot');
end;

procedure Tmainform.mnu_optionsSaveHighLowsClick(Sender: TObject);
begin
  SaveLoadHighLows := mnu_optionsSaveHighLows.Checked;
  INI.WriteBool('Foobot', 'SaveLoadHighLows', SaveLoadHighLows);
end;

procedure Tmainform.mnu_optionsShowMinimalDisplayClick(Sender: TObject);
begin
  if mnu_optionsShowMinimalDisplay.Checked then
    mainform.ClientHeight := grp_sensorDisplay.Height// + iFudgeFactor
  else
    mainform.ClientHeight := grp_sensorDisplay.Height + grp_chart.Height +
      grp_highlow.Height;// + iFudgeFactor;
  bShowMinimalDisplay := mnu_optionsShowMinimalDisplay.Checked;
end;

procedure Tmainform.mnu_optionsTakeReadingNowClick(Sender: TObject);
begin
  mainform.Cursor := crHourGlass;
  // Only Foobot #0
  if FetchFoobotData(dfLast, 0, 0, 0, 0, 0, sSecretKey) then
    DisplayReadings
  else
    ShowMessage('Sorry - no readings available');
  mainform.Cursor := crDefault;
end;

procedure Tmainform.mnu_SampleEvery1HourClick(Sender: TObject);
begin
  tmr_foobot.Enabled := False;
  tmr_foobot.Interval := ONEHOUR;
  tmr_foobot.Enabled := True;
end;

procedure Tmainform.mnu_SampleEvery24HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled := False;
  tmr_foobot.Interval := TWENTYFOURHOURS;
  tmr_foobot.Enabled := True;
end;

procedure Tmainform.mnu_SampleEvery2HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled := False;
  tmr_foobot.Interval := TWOHOURS;
  tmr_foobot.Enabled := True;
end;

procedure Tmainform.mnu_SampleEvery4HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled := False;
  tmr_foobot.Interval := FOURHOURS;
  tmr_foobot.Enabled := True;
end;

procedure Tmainform.mnu_SampleEvery8HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled := False;
  tmr_foobot.Interval := EIGHTHOURS;
  tmr_foobot.Enabled := True;
end;

procedure Tmainform.tmr_foobotTimer(Sender: TObject);
begin
  if FetchFoobotData(dfLast, 0, 0, 0, 0, 0, sSecretKey) then
    DisplayReadings;
end;

procedure Tmainform.TrayIcon1Click(Sender: TObject);
begin
  mainform.Show;
end;

procedure Tmainform.UpdateHighLow(SensorNumber: integer);
begin
  case SensorNumber of
    1:
    begin
      lbl_pmhigh.Caption := Format('High: %f %s',
        [double(FoobotDataHighs[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt',
        TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_pmLow.Caption := Format('Low: %f %s',
        [double(FoobotDataLows[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    2:
    begin
      lbl_tmphigh.Caption := Format('High: %f %s',
        [double(FoobotDataHighs[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt',
        TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_tmpLow.Caption := Format('Low: %f %s',
        [double(FoobotDataLows[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    3:
    begin
      lbl_humhigh.Caption := Format('High: %f %s',
        [double(FoobotDataHighs[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt',
        TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_humLow.Caption := Format('Low: %f %s',
        [double(FoobotDataLows[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    4:
    begin
      lbl_co2high.Caption := Format('High: %f %s',
        [double(FoobotDataHighs[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt',
        TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_co2Low.Caption := Format('Low: %f %s',
        [double(FoobotDataLows[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    5:
    begin
      lbl_vochigh.Caption := Format('High: %f %s',
        [double(FoobotDataHighs[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt',
        TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_vocLow.Caption := Format('Low: %f %s',
        [double(FoobotDataLows[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    6:
    begin
      lbl_allpolluhigh.Caption :=
        Format('High: %f %s', [double(FoobotDataHighs[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) + LineEnding + 'on ' +
        FormatDateTime('dd/mm tt', TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_allpollulow.Caption :=
        Format('Low: %f %s', [double(FoobotDataLows[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) + LineEnding + 'on ' +
        FormatDateTime('dd/mm tt', TDateTime(FoobotDataLowTimes[SensorNumber]));
    end;
  end;
end;

procedure Tmainform.UpdateGuage(Sender: TAnalogSensor; SensorNumber: integer);
begin
  with Sender do
  begin
    case SensorNumber of
      1:
      begin
        Value := FoobotData_pm[0];
        Caption := Format('PM (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      2:
      begin
        Value := FoobotData_tmp[0];
        Caption := Format('Temp (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      3:
      begin
        Value := FoobotData_hum[0];
        Caption := Format('Hum. (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      4:
      begin
        Value := FoobotData_co2[0];
        Caption := Format('CO2 (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      5:
      begin
        Value := FoobotData_voc[0];
        Caption := Format('VOC (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      6:
      begin
        Value := FoobotData_allpollu[0];
        Caption := Format('All (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
    end;
    if Value > ValueMax then
      ValueMax := Value;
    ValueYellow := ValueMax;
    if Value > ValueRed then
      ValueRed := Value;
  end;
end;

procedure Tmainform.DisplayReadings;
var
  iCount: integer;
begin
  if FoobotDataObjectToArrays = True then
  begin
    mainform.Caption := Format('Foobot "%s" - Last reading: ',
      [FoobotIdentityObject.FoobotIdentityList[0].Name]) +
      FormatDateTime('dd/mm/yyyy - tt', FoobotData_time[0]);
    UpdateGuage(as_pm, 1);
    UpdateGuage(as_tmp, 2);
    UpdateGuage(as_hum, 3);
    UpdateGuage(as_co2, 4);
    UpdateGuage(as_voc, 5);
    UpdateGuage(as_allpollu, 6);
    if not bShowMinimalDisplay then
    begin
      for iCount := 1 to 6 do
        UpdateHighLow(iCount);
    end;
    GraphCurrentReading;
  end;
end;

function AsPercent(aValue, aMin, aMax: double): double;
begin
  if aMax > 0 then
    Result := aValue / (aMax - aMin) * 100
  else
    Result := 0;
end;

procedure Tmainform.GraphCurrentReading;
begin
  lineseries_pm.AddXY(FoobotData_time[0],
    AsPercent(FoobotData_pm[0], as_pm.ValueMin, as_pm.ValueMax));
  lineseries_tmp.AddXY(FoobotData_time[0], AsPercent(FoobotData_tmp[0],
    as_tmp.ValueMin, as_tmp.ValueMax));
  lineseries_hum.AddXY(FoobotData_time[0],
    AsPercent(FoobotData_hum[0], as_hum.ValueMin, as_hum.ValueMax));
  lineseries_co2.AddXY(FoobotData_time[0],
    AsPercent(FoobotData_co2[0], as_co2.ValueMin, as_co2.ValueMax));
  lineseries_voc.AddXY(FoobotData_time[0],
    AsPercent(FoobotData_voc[0], as_voc.ValueMin, as_voc.ValueMax));
  lineseries_allpollu.AddXY(FoobotData_time[0],
    AsPercent(FoobotData_allpollu[0], as_allpollu.ValueMin, as_allpollu.ValueMax));
end;

procedure Tmainform.GraphHistory;
// Fetch Hourly readings for the previous 2 days (AverageBy=3600)
// Populate FoobotDataObjectToArrays
var
  iCount: integer;
  iStartSeconds, iEndSeconds: int64;
begin
  iEndSeconds := DateTimeToUnix(Now) - 3600;
  iStartSeconds := iEndSeconds - (2 * (24 * 3600)); // 49 hours before Now
  grp_chart.Caption:=Format('History from %s',[FormatDateTime('dd/mm/yyyy hh:nn',UnixToDateTime(iStartSeconds))]);
  if FetchFoobotData(dfStartEnd, 0, 0, 3600, iStartSeconds, iEndSeconds, sSecretKey) =
    False then
    exit;
  if FoobotDataObjectToArrays then
    for iCount := 0 to Pred(High(FoobotData_time)) do
    begin
      lineseries_pm.AddXY(FoobotData_time[iCount],
        AsPercent(FoobotData_pm[iCount], as_pm.ValueMin, as_pm.ValueMax));
      lineseries_tmp.AddXY(FoobotData_time[iCount],
        AsPercent(FoobotData_tmp[iCount], as_tmp.ValueMin, as_tmp.ValueMax));
      lineseries_hum.AddXY(FoobotData_time[iCount],
        AsPercent(FoobotData_hum[iCount], as_hum.ValueMin, as_hum.ValueMax));
      lineseries_co2.AddXY(FoobotData_time[iCount],
        AsPercent(FoobotData_co2[iCount], as_co2.ValueMin, as_co2.ValueMax));
      lineseries_voc.AddXY(FoobotData_time[iCount],
        AsPercent(FoobotData_voc[iCount], as_voc.ValueMin, as_voc.ValueMax));
      lineseries_allpollu.AddXY(FoobotData_time[iCount],
        AsPercent(FoobotData_allpollu[iCount], as_allpollu.ValueMin,
        as_allpollu.ValueMax));
    end;
  ResetArrays; // at end
end;

end.
