unit umainform;

{ Foobot Monitor

  Copyright (C)2016 Gordon Bamber minsadorada@charcodelvalle.com

  This project requires FPCv3.1.1+ and Lazarus V1.7+

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

REQUIREMENTS
============
* Custom sensors unit from Industrial package (foobot_sendors and foobot_sensors.res)
* Custom httpdclient unit (foobot_httpclient)
* CryptINI, TChart and Industrial components
* FPCv3.1.1+ and Lazarus V1.7+
* A connected Foobot

VERSION HISTORY
===============
V0.0.1.0: Initial commit
V0.0.2.0: Trayicon added
V0.0.3.0: Added Help menu.  Updated Options menu
V0.0.4.0: Graph added
V0.1.0.0: Save/Load Alltime High/Lows.  Reset values from menu
V0.1.1.0: Save/Load Colours, Min and Max values to cfg file
V0.2.1.0: Triggers,Multiple Foobots
V0.2.2.0: Trigger config form, SaveLoad recommended values
V0.2.4.0: Embedded help file added, replaced sensors.res with foobot_sensors.res
V0.2.5.0: ??
}
{$ifopt D+}
  {$DEFINE DEBUGMODE}
// Debug mode does not load data from web
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses // If Lazarus auto-inserts 'sensors' in the clause then delete it
  SysUtils, LazFileUtils, TAGraph, TAIntervalSources, TASeries, foobot_sensors,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus, lclIntf,
  ComCtrls, foobot_utility, uCryptIni, dateutils, uconfigform, utriggersform,
  Classes;

const
  // Timer milliseconds
  ONEMINUTE = 60000;
  HALFHOUR = ONEMINUTE * 30;
  ONEHOUR = ONEMINUTE * 60;
  TWOHOURS = ONEHOUR * 2;
  FOURHOURS = ONEHOUR * 4;
  EIGHTHOURS = ONEHOUR * 8;
  TWENTYFOURHOURS = ONEHOUR * 24;

  // Colours for guages and graph lines
  COL_PM = 'clGreen';
  COL_TMP = 'clRed';
  COL_HUM = 'clMaroon';
  COL_CO2 = 'clLime';
  COL_VOC = 'clBlue';
  COL_ALLPOLLU = 'clFuchsia';

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
    grp_health: TGroupBox;
    lbl_greenlighttmp: TLabel;
    lbl_greenlighthum: TLabel;
    lbl_greenlightco2: TLabel;
    lbl_greenlightvoc: TLabel;
    lbl_greenlightallpollu: TLabel;
    lbl_redlightpm: TLabel;
    lbl_redlighttmp: TLabel;
    lbl_redlighthum: TLabel;
    lbl_redlightco2: TLabel;
    lbl_redlightvoc: TLabel;
    lbl_redlightallpollu: TLabel;
    lbl_yellowlightpm: TLabel;
    lbl_greenlightpm: TLabel;
    lbl_yellowlighttmp: TLabel;
    lbl_yellowlighthum: TLabel;
    lbl_yellowlightco2: TLabel;
    lbl_yellowlightvoc: TLabel;
    lbl_yellowlightallpollu: TLabel;
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
    mnu_helpHelpHTML: TMenuItem;
    mnu_helpFoobotAPIPage: TMenuItem;
    mnu_options_triggersActivateTriggers: TMenuItem;
    mnu_options_triggersSetTriggers: TMenuItem;
    mnu_optionsFoobotTriggers: TMenuItem;
    mnu_foobot: TMenuItem;
    mnu_optionsDisplayRedLines: TMenuItem;
    mnu_optionsDisplayYellowLines: TMenuItem;
    mnu_optionsDisplayGuagesOnly: TMenuItem;
    mnu_SampleEveryHalfHour: TMenuItem;
    mnu_optionsResetHighsLows: TMenuItem;
    mnu_optionsHomePage: TMenuItem;
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
    mnu_optionsDisplay: TMenuItem;
    mnu_options: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_file: TMenuItem;
    pnl_healthpm: TPanel;
    pnl_healthallpollu: TPanel;
    pnl_healthvoc: TPanel;
    pnl_healthco2: TPanel;
    pnl_healthhum: TPanel;
    pnl_healthtmp: TPanel;
    sls_pm: TStopLightSensor;
    sls_allpollu: TStopLightSensor;
    sls_voc: TStopLightSensor;
    sls_co2: TStopLightSensor;
    sls_hum: TStopLightSensor;
    sls_tmp: TStopLightSensor;
    sts: TStatusBar;
    traypopup: TPopupMenu;
    tmr_foobot: TTimer;
    TrayIcon1: TTrayIcon;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure mnupopup_fileRestoreClick(Sender: TObject);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure mnu_helpFoobotAPIPageClick(Sender: TObject);
    procedure mnu_helpHelpHTMLClick(Sender: TObject);
    procedure mnu_optionsDisplayGuagesOnlyClick(Sender: TObject);
    procedure mnu_optionsDisplayRedLinesClick(Sender: TObject);
    procedure mnu_optionsDisplayYellowLinesClick(Sender: TObject);
    procedure mnu_optionsMinimiseToTrayClick(Sender: TObject);
    procedure mnu_optionsHomePageClick(Sender: TObject);
    procedure mnu_optionsResetHighsLowsClick(Sender: TObject);
    procedure mnu_optionsSaveHighLowsClick(Sender: TObject);
    procedure mnu_optionsTakeReadingNowClick(Sender: TObject);
    procedure mnu_options_triggersActivateTriggersClick(Sender: TObject);
    procedure mnu_options_triggersSetTriggersClick(Sender: TObject);
    procedure mnu_SampleEvery1HourClick(Sender: TObject);
    procedure mnu_SampleEvery24HoursClick(Sender: TObject);
    procedure mnu_SampleEvery2HoursClick(Sender: TObject);
    procedure mnu_SampleEvery4HoursClick(Sender: TObject);
    procedure mnu_SampleEvery8HoursClick(Sender: TObject);
    procedure mnu_SampleEveryHalfHourClick(Sender: TObject);
    procedure tmr_foobotTimer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure ShowHintInStatusBar(Sender:TObject);
  private
    sSecretKey, sFoobotUserName, sUUID: string;
    bDisplayGuagesOnly, bDisplayYellowLines, bDisplayRedLines: boolean;
    iFudgeFactor: integer;
    HighTriggerColor, LowTriggerColor: TColor;
    foobotmenuarray: array of TMenuItem;
    sStandardHintText:String;
    procedure DisplayReadings;
    procedure UpdateGuage(Sender: TAnalogSensor; SensorNumber: integer);
    procedure UpdateHighLow(SensorNumber: integer);
    procedure GraphHistory;
    procedure GraphCurrentReading;
    procedure SetYellowRecommendedLevels;
    procedure SetRedSessionMax;
    procedure UpdateHealth;
    procedure PopulateFoobotMenu;
    procedure ChangeCurrentFoobot(Sender: TObject);
    procedure SaveConfig;
    procedure LoadConfig;
    procedure SetMinMaxTriggers;
    procedure SetTrafficLightStats(const iSensorNum: integer; const HIGHLOW: integer);
    procedure DoHighTriggerAlert(const iSensorNum: integer; const aValue: variant);
    procedure DoLowTriggerAlert(const iSensorNum: integer; const aValue: variant);
    procedure RestoreNormalColour(const iSensorNum: integer);
    procedure PopulateHints; // for i8n use later
  public
    iCurrentFoobot: integer;
    INI: TCryptINIfile;
  end;

var
  mainform: Tmainform;
  // Used in extracting the help html file
  sHelpFilePath:String;
  S: TResourceStream;
  F: TFileStream;

implementation

uses uSplash;

{$R *.lfm}

{ Tmainform }
procedure Tmainform.PopulateHints;
// ToDo: i8n
begin
  // Traffic light controls
  sls_pm.Hint:='|Shows health of Particulates';
  sls_tmp.Hint:='|Shows health of Temperature';
  sls_hum.Hint:='|Shows health of Humidity';
  sls_co2.Hint:='|Shows health of Carbon Dioxide';
  sls_voc.Hint:='|Shows health of Volatile Organics';
  sls_allpollu.Hint:='|Shows health of All Pollution';
  // Traffic light labels
  lbl_greenlightpm.Hint:='|Shows green when below recommended value';
  lbl_greenlighttmp.Hint:='|Shows green when below recommended value';
  lbl_greenlighthum.Hint:='|Shows green when below recommended value';
  lbl_greenlightco2.Hint:='|Shows green when below recommended value';
  lbl_greenlightvoc.Hint:='|Shows green when below recommended value';
  lbl_greenlightallpollu.Hint:='|Shows green wne below recommended value';
  lbl_redlightpm.Hint:='|Shows red if High or Low value is triggered';
  lbl_redlighttmp.Hint:='|Shows red if High or Low value is triggered';
  lbl_redlighthum.Hint:='|Shows red if High or Low value is triggered';
  lbl_redlightco2.Hint:='|Shows red if High or Low value is triggered';
  lbl_redlightvoc.Hint:='|Shows red if High or Low value is triggered';
  lbl_redlightallpollu.Hint:='|Shows red if High or Low value is triggered';
  lbl_yellowlightpm.Hint:='|Shows yellow when above recommended value';
  lbl_yellowlighttmp.Hint:='|Shows yellow when above recommended value';
  lbl_yellowlighthum.Hint:='|Shows yellow when above recommended value';
  lbl_yellowlightco2.Hint:='|Shows yellow when above recommended value';
  lbl_yellowlightvoc.Hint:='|Shows yellow when above recommended value';
  lbl_yellowlightallpollu.Hint:='|Shows yellow when above recommended value';
  // All-time highs and lows
  lbl_pmhigh.Hint:='|All-time highest value recorded';
  lbl_tmphigh.Hint:='|All-time highest value recorded';
  lbl_humhigh.Hint:='|All-time highest value recorded';
  lbl_co2high.Hint:='|All-time highest value recorded';
  lbl_vochigh.Hint:='|All-time highest value recorded';
  lbl_allpolluhigh.Hint:='|All-time highest value recorded';
  lbl_pmlow.Hint:='|All-time lowest value recorded';
  lbl_tmplow.Hint:='|All-time lowest value recorded';
  lbl_humlow.Hint:='|All-time lowest value recorded';
  lbl_co2low.Hint:='|All-time lowest value recorded';
  lbl_voclow.Hint:='|All-time lowest value recorded';
  lbl_allpollulow.Hint:='|All-time lowest value recorded';
  // GroupBoxes
  grp_chart.Hint:='|' + sStandardHintText;
  grp_pm.Hint:='|' + sStandardHintText;
  grp_tmp.Hint:='|' + sStandardHintText;
  grp_hum.Hint:='|' + sStandardHintText;
  grp_co2.Hint:='|' + sStandardHintText;
  grp_voc.Hint:='|' + sStandardHintText;
  grp_allpollu.Hint:='|' + sStandardHintText;
  grp_highlow.Hint:='|' + sStandardHintText;
  grp_sensorDisplay.Hint:='|' + sStandardHintText;
  grp_health.Hint:='|' + sStandardHintText;
  // Sensors
  as_pm.Hint:='|Particulates level';
  as_tmp.Hint:='|Temperature';
  as_hum.Hint:='|Humidity';
  as_co2.Hint:='|Co2 level';
  as_voc.Hint:='|Volatile organics level';
  as_allpollu.Hint:='|All pollution level';
  Chart1.Hint:='|Rolling chart showing levels of all sensors';
end;

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
  iFudgeFactor := 20; // only needed if height set in form.create
  bDisplayGuagesOnly := False;
  INI.PlainTextMode := True;
  HighTriggerColor := clYellow;
  LowTriggerColor := clAqua;
  bDisplayYellowLines := INI.ReadBool('Config', 'DisplayYellowLines', False);
  mnu_optionsDisplayYellowLines.Checked := bDisplayYellowLines;
  bDisplayRedLines := INI.ReadBool('Config', 'DisplayRedLines', False);
  mnu_optionsDisplayRedLines.Checked := bDisplayRedLines;
  INI.PlainTextMode := False;
  SetYellowRecommendedLevels;
  SetRedSessionMax;
  TrayIcon1.Icon := Application.Icon;
  TrayIcon1.Hint := Application.Title;
  DateTimeIntervalChartSource1.DateTimeFormat := 'hh:nn';
  LoadConfig;
  {$IFDEF DEBUGMODE}
  UseTriggers := False;
  {$ENDIF}
  if UseTriggers then
  begin
    mnu_options_triggersActivateTriggers.Enabled := True;
    mnu_options_triggersActivateTriggers.Checked := True;
    mnu_options_triggersActivateTriggers.Caption := 'Set Triggers Off';
  end;
  sStandardHintText:='Welcome to ' + Application.Title;
  PopulateHints;
  sts.SimpleText:=sStandardHintText;
  Application.OnHint := @ShowHintInStatusBar;
end;

procedure Tmainform.FormActivate(Sender: TObject);
var
  sTempFoobotUserName, sTempSecretKey: string;
  iCount: integer;
begin
  ClientHeight := grp_sensorDisplay.Height + grp_highlow.Height +
    grp_health.Height + grp_chart.Height;
  Application.ProcessMessages;
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
    //Show;
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
        // Switch off for testing
        tmr_foobot.Interval := ONEHOUR;
        {$IFNDEF DEBUGMODE}
        tmr_foobot.Enabled := True;
        {$ENDIF}
        // Everything OK - lets go!
        iCurrentFoobot := 0;
        PopulateFoobotMenu;
        LoadTriggers; // This can only be done if we have a Foobot Identity
        // as each Foobot has its own trigger values
        SetMinMaxTriggers; // Adjust if necesarry for preset Guage High/Low limits
        LoadRecommendedLevels; // into RecommendedLevelsArray
        for iCount := C_PM to C_ALLPOLLU do
          SetTrafficLightStats(iCount, C_HIGH);
        Show;
        {$IFNDEF DEBUGMODE}
        mnu_optionsTakeReadingNow.Click;
        {$ENDIF}
        grp_sensorDisplay.Refresh;
        grp_highlow.Refresh;
        grp_health.Refresh;
        Update;
        Application.ProcessMessages;
        splashform.hide;
        Application.ProcessMessages;
      end
      else
      begin
        // Identity.Count = 0
      end;
    end
    else
    begin // Unable to fetch foobot identity
      ShowMessage('Cannot locate your Foobot.  Click OK to close the application');
      Close;
    end;
  end
  else
  begin
    // No valid cfg.  Show config form
    Hide;
    splashform.Hide;
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

procedure Tmainform.ChangeCurrentFoobot(Sender: TObject);
// Called from 'Foobot' TSubmenuitem.click
begin
  iCurrentFoobot := (Sender as TMenuItem).Tag;
  mnu_optionsTakeReadingNow.Click; // also triggers DoDisplay
end;

procedure Tmainform.SetTrafficLightStats(const iSensorNum: integer; const HIGHLOW: integer);
// Called via a loop in form.create with HIGHLOW=C_HIGH (all traffic light captions set)
// Called in DoHighTriggerAlert with HIGHLOW=C_HIGH (specific traffic light caption set)
// Called in DoLowTriggerAlert with HIGHLOW=C_LOW (specific traffic light caption set)
begin
  {$IFDEF DEBUGMODE}Exit;{$ENDIF}
  if iSensorNum = C_PM then
    if (HIGHLOW = C_HIGH) then
      lbl_redlightpm.Caption :=
        Format('> %.1f %s', [double(FooBotTriggerArray[C_HIGH, C_PM]),
        FoobotDataObject.Units[C_PM]])
    else
      lbl_redlightpm.Caption :=
        Format('< %.1f %s', [double(FooBotTriggerArray[C_LOW, C_PM]),
        FoobotDataObject.Units[C_PM]]);
  lbl_yellowlightpm.Caption :=
    Format('> %.1f %s', [RecommendedLevelsArray[C_PM], FoobotDataObject.Units[C_PM]]);
  lbl_greenlightpm.Caption :=
    Format('< %.1f %s', [RecommendedLevelsArray[C_PM], FoobotDataObject.Units[C_PM]]);

  if iSensorNum = C_TMP then
    if (HIGHLOW = C_HIGH) then
      lbl_redlighttmp.Caption :=
        Format('> %.1f %s', [double(FooBotTriggerArray[C_HIGH, C_TMP]),
        FoobotDataObject.Units[C_TMP]])
    else
      lbl_redlighttmp.Caption :=
        Format('< %.1f %s', [double(FooBotTriggerArray[C_LOW, C_TMP]),
        FoobotDataObject.Units[C_TMP]]);
  lbl_yellowlighttmp.Caption :=
    Format('> %.1f %s', [RecommendedLevelsArray[C_TMP], FoobotDataObject.Units[C_TMP]]);
  lbl_greenlighttmp.Caption :=
    Format('< %.1f %s', [RecommendedLevelsArray[C_TMP], FoobotDataObject.Units[C_TMP]]);

  if iSensorNum = C_HUM then
    if (HIGHLOW = C_HIGH) then
      lbl_redlighthum.Caption :=
        Format('> %.1f %s', [double(FooBotTriggerArray[C_HIGH, C_HUM]),
        FoobotDataObject.Units[C_HUM]])
    else
      lbl_redlighthum.Caption :=
        Format('< %.1f %s', [double(FooBotTriggerArray[C_LOW, C_HUM]),
        FoobotDataObject.Units[C_HUM]]);
  lbl_yellowlighthum.Caption :=
    Format('> %.1f %s', [RecommendedLevelsArray[C_HUM], FoobotDataObject.Units[C_HUM]]);
  lbl_greenlighthum.Caption :=
    Format('< %.1f %s', [RecommendedLevelsArray[C_HUM], FoobotDataObject.Units[C_HUM]]);

  if iSensorNum = C_CO2 then
    if (HIGHLOW = C_HIGH) then
      lbl_redlightco2.Caption :=
        Format('> %.0f %s', [double(FooBotTriggerArray[C_HIGH, C_CO2]),
        FoobotDataObject.Units[C_CO2]])
    else
      lbl_redlightco2.Caption :=
        Format('< %.0f %s', [double(FooBotTriggerArray[C_LOW, C_CO2]),
        FoobotDataObject.Units[C_CO2]]);
  lbl_yellowlightco2.Caption :=
    Format('> %.0f %s', [RecommendedLevelsArray[C_CO2], FoobotDataObject.Units[C_CO2]]);
  lbl_greenlightco2.Caption :=
    Format('< %.0f %s', [RecommendedLevelsArray[C_CO2], FoobotDataObject.Units[C_CO2]]);

  if iSensorNum = C_VOC then
    if (HIGHLOW = C_HIGH) then
      lbl_redlightvoc.Caption :=
        Format('> %.0f %s', [double(FooBotTriggerArray[C_HIGH, C_VOC]),
        FoobotDataObject.Units[C_VOC]])
    else
      lbl_redlightvoc.Caption :=
        Format('< %.0f %s', [double(FooBotTriggerArray[C_LOW, C_VOC]),
        FoobotDataObject.Units[C_VOC]]);
  lbl_yellowlightvoc.Caption :=
    Format('> %.0f %s', [RecommendedLevelsArray[C_VOC], FoobotDataObject.Units[C_VOC]]);
  lbl_greenlightvoc.Caption :=
    Format('< %.0f %s', [RecommendedLevelsArray[C_VOC], FoobotDataObject.Units[C_VOC]]);

  if iSensorNum = C_ALLPOLLU then
    if (HIGHLOW = C_HIGH) then
      lbl_redlightallpollu.Caption :=
        Format('> %.1f %s', [double(FooBotTriggerArray[C_HIGH, C_ALLPOLLU]),
        FoobotDataObject.Units[C_ALLPOLLU]])
    else
      lbl_redlightallpollu.Caption :=
        Format('< %.1f %s', [double(FooBotTriggerArray[C_LOW, C_ALLPOLLU]),
        FoobotDataObject.Units[C_ALLPOLLU]]);
  lbl_yellowlightallpollu.Caption :=
    Format('> %.1f %s', [RecommendedLevelsArray[C_ALLPOLLU], FoobotDataObject.Units[C_ALLPOLLU]]);
  lbl_greenlightallpollu.Caption :=
    Format('< %.1f %s', [RecommendedLevelsArray[C_ALLPOLLU], FoobotDataObject.Units[C_ALLPOLLU]]);
  grp_health.Refresh;
end;

procedure Tmainform.PopulateFoobotMenu;
// Uses dynamic foobotmenuarray
var
  iCount: integer;
begin
  if (FoobotIdentityObject.FoobotIdentityList.Count = 0) then
    Exit;
  SetLength(foobotmenuarray, FoobotIdentityObject.FoobotIdentityList.Count);
  for iCount := 0 to Pred(FoobotIdentityObject.FoobotIdentityList.Count) do
  begin
    foobotmenuarray[iCount] := TMenuItem.Create(MainMenu1);
    with foobotmenuarray[iCount] do
    begin
      Caption := FoobotIdentityObject.FoobotIdentityList[iCount].Name;
      AutoCheck := True;
      RadioItem := True;
      OnClick := @ChangeCurrentFoobot;
      Tag := iCount;
      if iCount = 0 then
        Checked := True;
    end;
  end;
  mnu_foobot.Add(foobotmenuarray);
end;

procedure Tmainform.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig; // to .cfg file
  if UseTriggers then
    SaveTriggers; // To .ini file
  SaveRecommendedLevels;
  CloseAction := caFree;
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  FreeAndNil(splashform);
  FreeAndNil(INI);
end;

procedure Tmainform.SaveConfig;
// For all Foobots
begin
  with INI do
  begin
    PlainTextMode := True;
    // Colours
    WriteString('Config', 'pmColour', ColorToString(as_pm.ColorFore));
    WriteString('Config', 'tmpColour', ColorToString(as_tmp.ColorFore));
    WriteString('Config', 'humColour', ColorToString(as_hum.ColorFore));
    WriteString('Config', 'co2Colour', ColorToString(as_co2.ColorFore));
    WriteString('Config', 'vocColour', ColorToString(as_voc.ColorFore));
    WriteString('Config', 'allpolluColour', ColorToString(as_allpollu.ColorFore));
    // Max and Min
    WriteFloat('Config', 'pmMinValue', as_pm.ValueMin);
    WriteFloat('Config', 'pmMaxValue', as_pm.ValueMax);
    WriteFloat('Config', 'tmpMinValue', as_tmp.ValueMin);
    WriteFloat('Config', 'tmpMaxValue', as_tmp.ValueMax);
    WriteFloat('Config', 'humMinValue', as_hum.ValueMin);
    WriteFloat('Config', 'humMaxValue', as_hum.ValueMax);
    WriteFloat('Config', 'co2MinValue', as_co2.ValueMin);
    WriteFloat('Config', 'co2MaxValue', as_co2.ValueMax);
    WriteFloat('Config', 'vocMinValue', as_voc.ValueMin);
    WriteFloat('Config', 'vocMaxValue', as_voc.ValueMax);
    WriteFloat('Config', 'allpolluMinValue', as_allpollu.ValueMin);
    WriteFloat('Config', 'allpolluMaxValue', as_allpollu.ValueMax);
    WriteBool('Config', 'DisplayYellowLines', bDisplayYellowLines);
    WriteBool('Config', 'DisplayRedLines', bDisplayRedLines);
    // Triggers
    WriteBool('Config', 'UseTriggers', UseTriggers);
    WriteString('Config', 'HighTriggerColour', ColorToString(HighTriggerColor));
    WriteString('Config', 'LowTriggerColour', ColorToString(LowTriggerColor));
    PlainTextMode := False;
  end;
end;

procedure Tmainform.SetMinMaxTriggers;
// Ensure Triggers are in range of High & Low guage values
begin
  if (UseTriggers = False) then
    exit;
  if as_pm.ValueMin > GetLowTrigger(C_PM) then
    SetLowTrigger(C_PM, as_pm.ValueMin);
  if as_pm.ValueMax < GetHighTrigger(C_PM) then
    SetHighTrigger(C_PM, as_pm.ValueMax);

  if as_tmp.ValueMin > GetLowTrigger(C_TMP) then
    SetLowTrigger(C_TMP, as_tmp.ValueMin);
  if as_tmp.ValueMax < GetHighTrigger(C_TMP) then
    SetHighTrigger(C_TMP, as_tmp.ValueMax);

  if as_hum.ValueMin > GetLowTrigger(C_HUM) then
    SetLowTrigger(C_HUM, as_hum.ValueMin);
  if as_hum.ValueMax < GetHighTrigger(C_HUM) then
    SetHighTrigger(C_HUM, as_hum.ValueMax);

  if as_co2.ValueMin > GetLowTrigger(C_CO2) then
    SetLowTrigger(C_CO2, as_co2.ValueMin);
  if as_co2.ValueMax < GetHighTrigger(C_CO2) then
    SetHighTrigger(C_CO2, as_co2.ValueMax);

  if as_voc.ValueMin > GetLowTrigger(C_VOC) then
    SetLowTrigger(C_VOC, as_voc.ValueMin);
  if as_voc.ValueMax < GetHighTrigger(C_VOC) then
    SetHighTrigger(C_VOC, as_voc.ValueMax);

  if as_allpollu.ValueMin > GetLowTrigger(C_ALLPOLLU) then
    SetLowTrigger(C_ALLPOLLU, as_allpollu.ValueMin);
  if as_allpollu.ValueMax < GetHighTrigger(C_ALLPOLLU) then
    SetHighTrigger(C_ALLPOLLU, as_allpollu.ValueMax);
end;

procedure Tmainform.LoadConfig;
// For all Foobots
begin
  INI.PlainTextMode := True;
  // Colours
  as_pm.ColorFore := StringToColor(INI.ReadString('Config', 'pmColour', COL_PM));
  as_tmp.ColorFore := StringToColor(INI.ReadString('Config', 'tmpColour', COL_TMP));
  as_hum.ColorFore := StringToColor(INI.ReadString('Config', 'humColour', COL_HUM));
  as_co2.ColorFore := StringToColor(INI.ReadString('Config', 'co2Colour', COL_CO2));
  as_voc.ColorFore := StringToColor(INI.ReadString('Config', 'vocColour', COL_VOC));
  as_allpollu.ColorFore := StringToColor(
    INI.ReadString('Config', 'allpolluColour', COL_ALLPOLLU));
  lineSeries_pm.SeriesColor := as_pm.ColorFore;
  lineSeries_tmp.SeriesColor := as_tmp.ColorFore;
  lineSeries_hum.SeriesColor := as_hum.ColorFore;
  lineSeries_co2.SeriesColor := as_co2.ColorFore;
  lineSeries_voc.SeriesColor := as_voc.ColorFore;
  lineSeries_allpollu.SeriesColor := as_allpollu.ColorFore;
  // Max and Min
  as_pm.ValueMin := INI.ReadFloat('Config', 'pmMinValue', MIN_PM);
  as_pm.ValueMax := INI.ReadFloat('Config', 'pmMaxValue', MAX_PM);
  as_tmp.ValueMin := INI.ReadFloat('Config', 'tmpMinValue', MIN_TMP);
  as_tmp.ValueMax := INI.ReadFloat('Config', 'tmpMaxValue', MAX_TMP);
  as_hum.ValueMin := INI.ReadFloat('Config', 'humMinValue', MIN_HUM);
  as_hum.ValueMax := INI.ReadFloat('Config', 'humMaxValue', MAX_HUM);
  as_co2.ValueMin := INI.ReadFloat('Config', 'co2MinValue', MIN_CO2);
  as_co2.ValueMax := INI.ReadFloat('Config', 'co2MaxValue', MAX_CO2);
  as_voc.ValueMin := INI.ReadFloat('Config', 'vocMinValue', MIN_VOC);
  as_voc.ValueMax := INI.ReadFloat('Config', 'vocMaxValue', MAX_VOC);
  as_allpollu.ValueMin := INI.ReadFloat('Config', 'allpolluMinValue', MIN_ALLPOLLU);
  as_allpollu.ValueMax := INI.ReadFloat('Config', 'allpolluMaxValue', MAX_ALLPOLLU);
  // Triggers
  UseTriggers := INI.ReadBool('Config', 'UseTriggers', False);
  HighTriggerColor := StringToColor(INI.ReadString('Config',
    'HighTriggerColour', 'clYellow'));
  LowTriggerColor := StringToColor(INI.ReadString('Config',
    'LowTriggerColour', 'clAqua'));
  INI.PlainTextMode := False;
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

procedure Tmainform.mnu_helpFoobotAPIPageClick(Sender: TObject);
begin
  OpenURL('http://api.foobot.io/apidoc/index.html');
end;

procedure Tmainform.mnu_helpHelpHTMLClick(Sender: TObject);
Var s:String;
begin
  If FileExists(sHelpFilePath) then
    OpenURL('file://' + sHelpFilePath)
  else ShowMessageFmt('Sorry, the help file %s is missing',[sHelpFilePath]);
end;

procedure Tmainform.mnu_optionsDisplayGuagesOnlyClick(Sender: TObject);
begin
  bDisplayGuagesOnly := mnu_optionsDisplayGuagesOnly.Checked;
  if bDisplayGuagesOnly then
    mainform.ClientHeight := grp_sensorDisplay.Height + grp_health.Height
  else
    mainform.ClientHeight := grp_sensorDisplay.Height + grp_health.Height +
      grp_chart.Height + grp_highlow.Height;
end;

procedure Tmainform.mnu_optionsDisplayRedLinesClick(Sender: TObject);
begin
  bDisplayRedLines := mnu_optionsDisplayRedLines.Checked;
  SetRedSessionMax;
end;

procedure Tmainform.mnu_optionsDisplayYellowLinesClick(Sender: TObject);
begin
  bDisplayYellowLines := mnu_optionsDisplayYellowLines.Checked;
  SetYellowRecommendedLevels;
end;

procedure Tmainform.mnu_optionsMinimiseToTrayClick(Sender: TObject);
begin
  mainform.WindowState := wsMinimized;
  mainform.FormWindowStateChange(Self);
end;

procedure Tmainform.mnu_optionsHomePageClick(Sender: TObject);
begin
  OpenURL('http://wiki.freepascal.org/Foobot');
end;

procedure Tmainform.mnu_optionsResetHighsLowsClick(Sender: TObject);
var
  iCount: integer;
begin
  if MessageDlg('This will erase the all-time high/low data permanently.  Are you sure?',
    mtConfirmation, [mbYes, mbCancel], 0, mbCancel) = mrCancel then
    exit;
  ResetHighLows;
  SaveHighLows;
  for iCount := C_PM to C_ALLPOLLU do
    UpdateHighLow(iCount);
end;

procedure Tmainform.mnu_optionsSaveHighLowsClick(Sender: TObject);
begin
  SaveLoadHighLows := mnu_optionsSaveHighLows.Checked;
  INI.WriteBool('Foobot', 'SaveLoadHighLows', SaveLoadHighLows);
end;

procedure Tmainform.mnu_optionsTakeReadingNowClick(Sender: TObject);
begin
  mainform.Cursor := crHourGlass;
  if FetchFoobotData(dfLast, iCurrentFoobot, 0, 0, 0, 0, sSecretKey) then
    DisplayReadings
  else
    ShowMessage('Sorry - no readings available');
  mainform.Cursor := crDefault;
end;

procedure Tmainform.mnu_options_triggersActivateTriggersClick(Sender: TObject);
var
  icount: integer;
begin
  mnu_options_triggersActivateTriggers.Checked :=
    not mnu_options_triggersActivateTriggers.Checked;
  UseTriggers := mnu_options_triggersActivateTriggers.Checked;
  if UseTriggers then
  begin
    mnu_options_triggersActivateTriggers.Caption := 'Set Triggers Off';
    LoadTriggers;
    SetMinMaxTriggers;
    DisplayReadings;
  end
  else
  begin
    mnu_options_triggersActivateTriggers.Caption := 'Set Triggers On';
    for iCount := C_PM to C_ALLPOLLU do
      RestoreNormalColour(iCount);
  end;
end;

procedure Tmainform.mnu_options_triggersSetTriggersClick(Sender: TObject);
Var iCount:Integer;
begin
  If FoobotIdentityObject.FoobotIdentityList.Count = 0 then Exit;;
  triggersform.ShowModal;
  // If cancel was clicked nothing was changed
  mnu_options_triggersActivateTriggers.Enabled := True;
  LoadTriggers; // This can only be done if we have a Foobot Identity
  // as each Foobot has its own trigger values
  SetMinMaxTriggers; // Adjust if necesarry for preset Guage High/Low limits
  // LoadRecommendedLevels; // into RecommendedLevelsArray
  for iCount := C_PM to C_ALLPOLLU do
      SetTrafficLightStats(iCount, C_HIGH);
   UpdateHealth;
   DisplayReadings;
   Update;
   {
   ShowMessageFmt('Current high trigger: %.1f %s',
    [double(FooBotTriggerArray[C_HIGH, C_TMP]), FoobotDataObject.Units[C_TMP]]);
   }
end;

procedure Tmainform.mnu_SampleEveryHalfHourClick(Sender: TObject);
begin
  tmr_foobot.Enabled := False;
  tmr_foobot.Interval := HALFHOUR;
  tmr_foobot.Enabled := True;
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
  if FetchFoobotData(dfLast, iCurrentFoobot, 0, 0, 0, 0, sSecretKey) then
    DisplayReadings
  else
    mainform.Caption := Format('Foobot "%s" - Read failure on %s',
      [FoobotIdentityObject.FoobotIdentityList[iCurrentFoobot].Name,
      FormatDateTime('dd/mm/yyyy - tt', Now)]);
end;

procedure Tmainform.TrayIcon1Click(Sender: TObject);
begin
  mainform.Show;
end;

procedure Tmainform.SetRedSessionMax;
// Sets red lines on guages
begin
  if bDisplayRedLines = True then
  begin
    with as_pm do
      if Value > ValueRed then
        ValueRed := Value;
    with as_tmp do
      if Value > ValueRed then
        ValueRed := Value;
    with as_hum do
      if Value > ValueRed then
        ValueRed := Value;
    with as_co2 do
      if Value > ValueRed then
        ValueRed := Value;
    with as_voc do
      if Value > ValueRed then
        ValueRed := Value;
    with as_allpollu do
      if Value > ValueRed then
        ValueRed := Value;
  end
  else
  begin
    as_pm.ValueRed := as_pm.ValueMin;
    as_tmp.ValueRed := as_tmp.ValueMin;
    as_hum.ValueRed := as_hum.ValueMin;
    as_co2.ValueRed := as_co2.ValueMin;
    as_voc.ValueRed := as_voc.ValueMin;
    as_allpollu.ValueRed := as_allpollu.ValueMin;
  end;
end;

procedure Tmainform.SetYellowRecommendedLevels;
// Sets yellow lines on guages
begin
  if bDisplayYellowLines = True then
  begin
    as_pm.ValueYellow := RecommendedLevelsArray[C_PM];
    as_tmp.ValueYellow := RecommendedLevelsArray[C_TMP];
    as_hum.ValueYellow := RecommendedLevelsArray[C_HUM];
    as_co2.ValueYellow := RecommendedLevelsArray[C_CO2];
    as_voc.ValueYellow := RecommendedLevelsArray[C_VOC];
    as_allpollu.ValueYellow := RecommendedLevelsArray[C_ALLPOLLU];
  end
  else
  begin
    as_pm.ValueYellow := as_pm.ValueMin;
    as_tmp.ValueYellow := as_tmp.ValueMin;
    as_hum.ValueYellow := as_hum.ValueMin;
    as_co2.ValueYellow := as_co2.ValueMin;
    as_voc.ValueYellow := as_voc.ValueMin;
    as_allpollu.ValueYellow := as_allpollu.ValueMin;
  end;
end;

procedure Tmainform.UpdateHighLow(SensorNumber: integer);
begin
  case SensorNumber of
    C_PM:
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
    C_TMP:
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
    C_HUM:
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
    C_CO2:
    begin
      lbl_co2high.Caption := Format('High: %d %s',
        [integer(FoobotDataHighs[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt',
        TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_co2Low.Caption := Format('Low: %d %s',
        [integer(FoobotDataLows[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    C_VOC:
    begin
      lbl_vochigh.Caption := Format('High: %d %s',
        [integer(FoobotDataHighs[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt',
        TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_vocLow.Caption := Format('Low: %d %s',
        [integer(FoobotDataLows[SensorNumber]), FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    C_ALLPOLLU:
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
    else
      Exception.Create('Error in UpdateHighLow Case statement');
  end;
end;

procedure Tmainform.UpdateGuage(Sender: TAnalogSensor; SensorNumber: integer);
begin
  with Sender do
  begin
    case SensorNumber of
      C_PM:
      begin
        Value := FoobotData_pm[iCurrentFoobot];
        Caption := Format('PM (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      C_TMP:
      begin
        Value := FoobotData_tmp[iCurrentFoobot];
        Caption := Format('Temp (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      C_HUM:
      begin
        Value := FoobotData_hum[iCurrentFoobot];
        Caption := Format('Hum. (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      C_CO2:
      begin
        Value := FoobotData_co2[iCurrentFoobot];
        Caption := Format('CO2 (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      C_VOC:
      begin
        Value := FoobotData_voc[iCurrentFoobot];
        Caption := Format('VOC (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
      C_ALLPOLLU:
      begin
        Value := FoobotData_allpollu[iCurrentFoobot];
        Caption := Format('All (%s): ', [FoobotDataObject.Units[SensorNumber]]);
      end;
    end;
    if Value > ValueMax then
      ValueMax := Value;
    if bDisplayRedLines then
      if Value > ValueRed then
        ValueRed := Value;
    if bDisplayYellowLines then
      SetYellowRecommendedLevels;
  end;
end;

procedure Tmainform.DoHighTriggerAlert(const iSensorNum: integer; const aValue: variant);
begin
  case iSensorNum of
    C_PM: as_pm.Color := HighTriggerColor;
    C_TMP: as_tmp.Color := HighTriggerColor;
    C_HUM: as_hum.Color := HighTriggerColor;
    C_CO2: as_co2.Color := HighTriggerColor;
    C_VOC: as_voc.Color := HighTriggerColor;
    C_ALLPOLLU: as_allpollu.Color := HighTriggerColor;
  end;
  SetTrafficLightStats(iSensorNum, C_HIGH);
end;

procedure Tmainform.DoLowTriggerAlert(const iSensorNum: integer; const aValue: variant);
begin
  case iSensorNum of
    C_PM: as_pm.Color := LowTriggerColor;
    C_TMP: as_tmp.Color := LowTriggerColor;
    C_HUM: as_hum.Color := LowTriggerColor;
    C_CO2: as_co2.Color := LowTriggerColor;
    C_VOC: as_voc.Color := LowTriggerColor;
    C_ALLPOLLU: as_allpollu.Color := LowTriggerColor;
  end;
  SetTrafficLightStats(iSensorNum, C_LOW);
end;

procedure Tmainform.RestoreNormalColour(const iSensorNum: integer);
begin
  case iSensorNum of
    C_PM: as_pm.Color := clDefault;
    C_TMP: as_tmp.Color := clDefault;
    C_HUM: as_hum.Color := clDefault;
    C_CO2: as_co2.Color := clDefault;
    C_VOC: as_voc.Color := clDefault;
    C_ALLPOLLU: as_allpollu.Color := clDefault;
  end;
end;

procedure Tmainform.UpdateHealth;
begin
  if (as_pm.Value >= RecommendedLevelsArray[C_PM]) then
    sls_pm.State := slYELLOW
  else
    sls_pm.State := slGREEN;

  if (as_tmp.Value >= RecommendedLevelsArray[C_TMP]) then
    sls_tmp.State := slYELLOW
  else
    sls_tmp.State := slGREEN;

  if (as_hum.Value >= RecommendedLevelsArray[C_HUM]) then
    sls_hum.State := slYELLOW
  else
    sls_hum.State := slGREEN;

  if (as_co2.Value >= RecommendedLevelsArray[C_CO2]) then
    sls_co2.State := slYELLOW
  else
    sls_co2.State := slGREEN;

  if (as_voc.Value >= RecommendedLevelsArray[C_VOC]) then
    sls_voc.State := slYELLOW
  else
    sls_voc.State := slGREEN;

  if (as_allpollu.Value >= RecommendedLevelsArray[C_ALLPOLLU]) then
    sls_allpollu.State := slYELLOW
  else
    sls_allpollu.State := slGREEN;

end;

procedure Tmainform.DisplayReadings;
var
  iCount: integer;
begin
  if FoobotDataObjectToArrays = True then
  begin
    mainform.Caption := Format('Foobot "%s" - Last reading: ',
      [FoobotIdentityObject.FoobotIdentityList[iCurrentFoobot].Name]) +
      FormatDateTime('dd/mm/yyyy - tt', FoobotData_time[0]);
    UpdateGuage(as_pm, C_PM);
    UpdateGuage(as_tmp, C_TMP);
    UpdateGuage(as_hum, C_HUM);
    UpdateGuage(as_co2, C_CO2);
    UpdateGuage(as_voc, C_VOC);
    UpdateGuage(as_allpollu, C_ALLPOLLU);
    UpdateHealth;
    // Process Trigger Alerts on each call to FoobotDataObjectToArrays
    if UseTriggers then
      try
        // Look for alerts in each sensor
        for iCount := C_PM to C_ALLPOLLU do
        begin
          if (AlertRec[iCount].AlertTriggered = True) then
          begin
            case iCount of
              C_PM: sls_pm.State := slRED;
              C_TMP: sls_tmp.State := slRED;
              C_HUM: sls_hum.State := slRED;
              C_CO2: sls_co2.State := slRED;
              C_VOC: sls_voc.State := slRED;
              C_ALLPOLLU: sls_allpollu.State := slRED;
            end;
            // Alert found.  High or low?
            if (AlertRec[iCount].AlertType = C_HIGH) then
            begin
              // A high alert - do something
              DoHighTriggerAlert(iCount, AlertRec[iCount].AlertValue);
            end
            else
            begin
              // A low alert - do something
              DoLowTriggerAlert(iCount, AlertRec[iCount].AlertValue);
            end;
          end
          else
            RestoreNormalColour(iCount);
        end;
      except
        raise Exception.Create('Unable to process triggers in DisplayReadings');
      end;
    if not bDisplayGuagesOnly then
    begin
      for iCount := 1 to 6 do
        UpdateHighLow(iCount);
    end;
    GraphCurrentReading;

  end
  else
    raise Exception.Create('FoobotDataObjectToArrays error in DisplayReadings');
end;

function AsPercent(aValue, aMin, aMax: double): double;
begin
  if aMax > 0 then
    Result := (aValue / (aMax - aMin) * 100)
  else
    Result := 0;
end;

procedure Tmainform.GraphCurrentReading;
begin
  {$IFDEF DEBUGMODE}
  Exit;
  {$ENDIF}
  try
    lineseries_pm.AddXY(FoobotData_time[iCurrentFoobot],
      AsPercent(FoobotData_pm[iCurrentFoobot], as_pm.ValueMin, as_pm.ValueMax));
    lineseries_tmp.AddXY(FoobotData_time[iCurrentFoobot],
      AsPercent(FoobotData_tmp[iCurrentFoobot], as_tmp.ValueMin, as_tmp.ValueMax));
    lineseries_hum.AddXY(FoobotData_time[iCurrentFoobot],
      AsPercent(FoobotData_hum[iCurrentFoobot], as_hum.ValueMin, as_hum.ValueMax));
    lineseries_co2.AddXY(FoobotData_time[iCurrentFoobot],
      AsPercent(FoobotData_co2[iCurrentFoobot], as_co2.ValueMin, as_co2.ValueMax));
    lineseries_voc.AddXY(FoobotData_time[iCurrentFoobot],
      AsPercent(FoobotData_voc[iCurrentFoobot], as_voc.ValueMin, as_voc.ValueMax));
    lineseries_allpollu.AddXY(FoobotData_time[iCurrentFoobot],
      AsPercent(FoobotData_allpollu[iCurrentFoobot], as_allpollu.ValueMin,
      as_allpollu.ValueMax));
  except
    raise Exception.Create('Unable to update graph in GraphCurrentReading');
  end;
end;

procedure Tmainform.GraphHistory;
// Fetch Hourly readings for the previous 2 days (AverageBy=3600)
// Populate FoobotDataObjectToArrays
var
  iCount: integer;
  iStartSeconds, iEndSeconds: int64;
  bTempUseTriggers: boolean;
begin
  {$IFDEF DEBUGMODE}
  Exit;
  {$ENDIF}
  // Turn off triggers (if on)
  bTempUseTriggers := UseTriggers;
  UseTriggers := False;

  iEndSeconds := DateTimeToUnix(Now) - 3600;
  iStartSeconds := iEndSeconds - (2 * (24 * 3600)); // 49 hours before Now
  grp_chart.Caption := Format('History since %s',
    [FormatDateTime('dd/mm/yyyy hh:nn', UnixToDateTime(iStartSeconds))]);
  if FetchFoobotData(dfStartEnd, iCurrentFoobot, 0, 3600, iStartSeconds,
    iEndSeconds, sSecretKey) = False then
    exit;
  try
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
  finally
    UseTriggers := bTempUseTriggers;
    ResetArrays; // at end
  end;
end;

procedure Tmainform.ShowHintInStatusBar(Sender:TObject);
begin
  sts.SimpleText := GetLongHint(Application.Hint);
end;
initialization
//sHelpFilePath:=AppendPathDelim(GetCurrentDir) + 'foobotmonitorhelp.htm';
sHelpFilePath:=AppendPathDelim(GetAppConfigDir(false)) + 'foobotmonitorhelp.htm';
//sHelpFilePath:=ExtractFileDir(GetAppConfigFile(False));
//sHelpFilePath:=AppendPathDelim(sHelpFilePath) + 'foobotmonitorhelp.htm';

// This uses a resource file added via Project/Options (Laz 1.7+)
if not FileExistsUTF8(sHelpFilePath) then
begin
  // create a resource stream which points to the po file
  S := TResourceStream.Create(HInstance, 'FOOBOTMONITORHELP', MakeIntResource(10));
  TRY
    try
      ForceDirectoriesUTF8(GetAppConfigDir(false));
      F := TFileStream.Create(sHelpFilePath, fmCreate);
      try
        F.CopyFrom(S, S.Size); // copy data from the resource stream to file stream
      finally
        F.Free; // destroy the file stream
      end;
    finally
      S.Free; // destroy the resource stream
    end;
  EXCEPT
    raise Exception.Create('Could not create ' + sHelpFilePath);
  end;
end;
end.
