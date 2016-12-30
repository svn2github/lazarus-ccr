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
V0.0.3.0: ??
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Sensors, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Menus, foobot_utility, uCryptIni, Variants, dateutils,
  uconfigform;

CONST
  ONEMINUTE = 60000;
  ONEHOUR = ONEMINUTE * 60;
  TWOHOURS = ONEHOUR * 2;
  FOURHOURS = ONEHOUR * 4;
  EIGHTHOURS = ONEHOUR * 8;
  TWENTYFOURHOURS = ONEHOUR * 24;


type

  { Tmainform }

  Tmainform = class(TForm)
    as_allpollu: TAnalogSensor;
    as_co2: TAnalogSensor;
    as_hum: TAnalogSensor;
    as_pm: TAnalogSensor;
    as_tmp: TAnalogSensor;
    as_voc: TAnalogSensor;
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
    mnu_optionsShowHighsAndLows: TMenuItem;
    mnu_options: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_file: TMenuItem;
    traypopup: TPopupMenu;
    tmr_foobot: TTimer;
    TrayIcon1: TTrayIcon;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure mnupopup_fileRestoreClick(Sender: TObject);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_optionsMinimiseToTrayClick(Sender: TObject);
    procedure mnu_optionsSaveHighLowsClick(Sender: TObject);
    procedure mnu_optionsShowHighsAndLowsClick(Sender: TObject);
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
    bShowHighsAndLows: boolean;
    iFudgeFactor: integer;
    iSampleInterval:Integer;
    procedure DisplayReadings;
    procedure UpdateGuage(Sender: TAnalogSensor; SensorNumber: integer);
    procedure UpdateHighLow(SensorNumber: integer);
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
  INI.SectionHashing:=FALSE;
  ResetHighLows;
  iFudgeFactor := 20;
  ClientHeight := grp_sensorDisplay.Height + grp_highlow.Height + iFudgeFactor;
  bShowHighsAndLows := True;
  TrayIcon1.Icon:=Application.Icon;
  TrayIcon1.Hint:=Application.Title;
end;

procedure Tmainform.FormActivate(Sender: TObject);
Var sTempFoobotUserName,sTempSecretKey:String;

begin
  // Allow user to enter values in INIFile
  sTempFoobotUserName:=INI.ReadUnencryptedString('Config','Foobot User','unknown');
  sTempSecretKey:=INI.ReadUnencryptedString('Config', 'Secret Key', 'unknown');
  if ((sTempFoobotUserName <> 'unknown') and (sTempSecretKey <> 'unknown')) then
  begin
    INI.WriteString('Foobot', 'Foobot User', sTempFoobotUserName);
    INI.DeleteKey('Config','Foobot User');
    INI.WriteString('Foobot', 'Secret Key', sTempSecretKey);
    INI.DeleteKey('Config','Secret Key');
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
        SaveLoadHighLows:=INI.ReadBool('Foobot','SaveLoadHighLows',TRUE);
        mnu_optionsSaveHighLows.Checked:=SaveLoadHighLows;
        If SaveLoadHighLows then LoadHighLows;
        mnu_optionsTakeReadingNow.Click;
        // Switch off for testing
        tmr_foobot.Interval:=ONEHOUR;
        tmr_foobot.Enabled:=TRUE;
        Show;
      end;
    end
    else Close;
  end
  else
  begin
    // No valid cfg.  Show config form
    Hide;
    Application.ProcessMessages;
    configform.ShowModal;
    // If user quit without data, then bail out
    If NOT configform.bValid then
    begin
      Close;
    end;
    // Store encrypted Username and API_KEY
    INI.WriteString('Foobot', 'Foobot User', configform.FoobotUsername);
    INI.WriteString('Foobot', 'Secret Key', configform.FoobotSecretKey);
    //sFoobotUserName := INI.ReadString('Foobot', 'Foobot User', 'unknown');
    //sSecretKey := INI.ReadString('Foobot', 'Secret Key', 'unknown');
    ShowMessage('Click OK to store settings and close the app.' + LineEnding + 'New settings are applied on resart.');
    Close;
  end;
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  FreeAndNil(INI);
end;

procedure Tmainform.FormShow(Sender: TObject);
begin
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
  mainform.show;
end;

procedure Tmainform.mnu_fileExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tmainform.mnu_optionsMinimiseToTrayClick(Sender: TObject);
begin
  mainform.WindowState:=wsMinimized;
  mainform.FormWindowStateChange(Self);
end;

procedure Tmainform.mnu_optionsSaveHighLowsClick(Sender: TObject);
begin
  SaveLoadHighLows:=mnu_optionsSaveHighLows.Checked;
  INI.WriteBool('Foobot','SaveLoadHighLows',SaveLoadHighLows);
end;

procedure Tmainform.mnu_optionsShowHighsAndLowsClick(Sender: TObject);
begin
  if mnu_optionsShowHighsAndLows.Checked then
    mainform.ClientHeight := grp_sensorDisplay.Height + grp_highlow.Height + iFudgeFactor
  else
    mainform.ClientHeight := grp_sensorDisplay.Height + iFudgeFactor;
  bShowHighsAndLows := mnu_optionsShowHighsAndLows.Checked;
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
  tmr_foobot.Enabled:=FALSE;
  tmr_foobot.Interval:=ONEHOUR;
  tmr_foobot.Enabled:=TRUE;
end;

procedure Tmainform.mnu_SampleEvery24HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled:=FALSE;
  tmr_foobot.Interval:=TWENTYFOURHOURS;
  tmr_foobot.Enabled:=TRUE;
end;

procedure Tmainform.mnu_SampleEvery2HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled:=FALSE;
  tmr_foobot.Interval:=TWOHOURS;
  tmr_foobot.Enabled:=TRUE;
end;

procedure Tmainform.mnu_SampleEvery4HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled:=FALSE;
  tmr_foobot.Interval:=FOURHOURS;
  tmr_foobot.Enabled:=TRUE;
end;

procedure Tmainform.mnu_SampleEvery8HoursClick(Sender: TObject);
begin
  tmr_foobot.Enabled:=FALSE;
  tmr_foobot.Interval:=EIGHTHOURS;
  tmr_foobot.Enabled:=TRUE;
end;

procedure Tmainform.tmr_foobotTimer(Sender: TObject);
begin
  if FetchFoobotData(dfLast, 0, 0, 0, 0, 0, sSecretKey) then
    DisplayReadings;
end;

procedure Tmainform.TrayIcon1Click(Sender: TObject);
begin
  mainform.show;
end;

procedure Tmainform.UpdateHighLow(SensorNumber: integer);
begin
  case SensorNumber of
    1:
    begin
      lbl_pmhigh.Caption := Format(
        'High: %f %s', [double(FoobotDataHighs[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) + LineEnding + 'on ' +
        FormatDateTime('dd/mm tt', TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_pmLow.Caption := Format(
        'Low: %f %s', [double(FoobotDataLows[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    2:
    begin
      lbl_tmphigh.Caption := Format(
        'High: %f %s', [double(FoobotDataHighs[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) + LineEnding + 'on ' +
        FormatDateTime('dd/mm tt', TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_tmpLow.Caption := Format(
        'Low: %f %s', [double(FoobotDataLows[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    3:
    begin
      lbl_humhigh.Caption := Format(
        'High: %f %s', [double(FoobotDataHighs[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) + LineEnding + 'on ' +
        FormatDateTime('dd/mm tt', TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_humLow.Caption := Format(
        'Low: %f %s', [double(FoobotDataLows[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    4:
    begin
      lbl_co2high.Caption := Format(
        'High: %f %s', [double(FoobotDataHighs[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) + LineEnding + 'on ' +
        FormatDateTime('dd/mm tt', TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_co2Low.Caption := Format(
        'Low: %f %s', [double(FoobotDataLows[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) +
        LineEnding + 'on ' + FormatDateTime('dd/mm tt', TDateTime(
        FoobotDataLowTimes[SensorNumber]));
    end;
    5:
    begin
      lbl_vochigh.Caption := Format(
        'High: %f %s', [double(FoobotDataHighs[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) + LineEnding + 'on ' +
        FormatDateTime('dd/mm tt', TDateTime(FoobotDataHighTimes[SensorNumber]));
      lbl_vocLow.Caption := Format(
        'Low: %f %s', [double(FoobotDataLows[SensorNumber]),
        FoobotDataObject.Units[SensorNumber]]) +
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
    mainform.Caption := Format('Foobot "%s" - ',
      [FoobotIdentityObject.FoobotIdentityList[0].Name]) +
      FormatDateTime('dd/mm/yyyy - tt', FoobotData_time[0]);
    UpdateGuage(as_pm, 1);
    UpdateGuage(as_tmp, 2);
    UpdateGuage(as_hum, 3);
    UpdateGuage(as_co2, 4);
    UpdateGuage(as_voc, 5);
    UpdateGuage(as_allpollu, 6);
    if bShowHighsAndLows then
      for iCount := 1 to 6 do
        UpdateHighLow(iCount);
  end;
end;

end.
