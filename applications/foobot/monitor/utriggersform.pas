unit utriggersform;
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
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type
  { Ttriggersform }

  Ttriggersform = class(TForm)
    cmd_cancel: TBitBtn;
    cmd_OK: TBitBtn;
    edt_newhightrigger_tmp: TEdit;
    edt_newhightrigger_hum: TEdit;
    edt_newhightrigger_co2: TEdit;
    edt_newhightrigger_voc: TEdit;
    edt_newhightrigger_allpollu: TEdit;
    edt_newlowtrigger_tmp: TEdit;
    edt_newlowtrigger_hum: TEdit;
    edt_newlowtrigger_co2: TEdit;
    edt_newlowtrigger_voc: TEdit;
    edt_newlowtrigger_allpollu: TEdit;
    edt_newrec_pm: TEdit;
    edt_newhightrigger_pm: TEdit;
    edt_newlowtrigger_pm: TEdit;
    edt_newrec_tmp: TEdit;
    edt_newrec_hum: TEdit;
    edt_newrec_co2: TEdit;
    edt_newrec_voc: TEdit;
    edt_newrec_allpollu: TEdit;
    grp_pm: TGroupBox;
    grp_tmp: TGroupBox;
    grp_hum: TGroupBox;
    grp_co2: TGroupBox;
    grp_voc: TGroupBox;
    grp_allpollu: TGroupBox;
    grp_main: TGroupBox;
    lbl_currenthightriggerunits_tmp: TLabel;
    lbl_currenthightriggerunits_hum: TLabel;
    lbl_currenthightriggerunits_co2: TLabel;
    lbl_currenthightriggerunits_voc: TLabel;
    lbl_currenthightriggerunits_allpollu: TLabel;
    lbl_currenthightrigger_tmp: TLabel;
    lbl_currenthightrigger_hum: TLabel;
    lbl_currenthightrigger_co2: TLabel;
    lbl_currenthightrigger_voc: TLabel;
    lbl_currenthightrigger_allpollu: TLabel;
    lbl_currentlowtriggerunits_tmp: TLabel;
    lbl_currentlowtriggerunits_hum: TLabel;
    lbl_currentlowtriggerunits_co2: TLabel;
    lbl_currentlowtriggerunits_voc: TLabel;
    lbl_currentlowtriggerunits_allpollu: TLabel;
    lbl_currentlowtrigger_tmp: TLabel;
    lbl_currentlowtrigger_hum: TLabel;
    lbl_currentlowtrigger_co2: TLabel;
    lbl_currentlowtrigger_voc: TLabel;
    lbl_currentlowtrigger_allpollu: TLabel;
    lbl_currenthightriggerunits_pm: TLabel;
    lbl_currentlowtriggerunits_pm: TLabel;
    lbl_currentrecunits_tmp: TLabel;
    lbl_currentrecunits_hum: TLabel;
    lbl_currentrecunits_co2: TLabel;
    lbl_currentrecunits_voc: TLabel;
    lbl_currentrecunits_allpollu: TLabel;
    lbl_currentrec_pm: TLabel;
    lbl_currenthightrigger_pm: TLabel;
    lbl_currentlowtrigger_pm: TLabel;
    lbl_currentrecunits_pm: TLabel;
    lbl_newhightrigger_tmp: TLabel;
    lbl_newhightrigger_hum: TLabel;
    lbl_newhightrigger_co2: TLabel;
    lbl_newhightrigger_voc: TLabel;
    lbl_newhightrigger_allpollu: TLabel;
    lbl_newlowtrigger_tmp: TLabel;
    lbl_newlowtrigger_hum: TLabel;
    lbl_newlowtrigger_co2: TLabel;
    lbl_newlowtrigger_voc: TLabel;
    lbl_newlowtrigger_allpollu: TLabel;
    lbl_newrec_pm: TLabel;
    lbl_currentrec_tmp: TLabel;
    lbl_currentrec_hum: TLabel;
    lbl_currentrec_co2: TLabel;
    lbl_currentrec_voc: TLabel;
    lbl_currentrec_allpollu: TLabel;
    lbl_newhightrigger_pm: TLabel;
    lbl_newlowtrigger_pm: TLabel;
    lbl_newrec_tmp: TLabel;
    lbl_newrec_hum: TLabel;
    lbl_newrec_co2: TLabel;
    lbl_newrec_voc: TLabel;
    lbl_newrec_allpollu: TLabel;
    procedure cmd_OKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnlyNumericKeyPressFloat(Sender: TObject; var Key: char);
    procedure OnlyNumericKeyPressInteger(Sender: TObject; var Key: char);
  private
    ErrorList: TStrings;
    procedure DisplayCurrentValues;
    procedure SetUpUnits;
    function AssignAndSaveTriggers: boolean;
    function AssignAndSaveRecommendedLevels: boolean;
    function AllInputsVerified: boolean;
  public

  end;

var
  triggersform: Ttriggersform;

implementation

uses umainform, foobot_utility;

{$R *.lfm}

{ Ttriggersform }

procedure Ttriggersform.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  ErrorList := TStringList.Create;
end;

procedure Ttriggersform.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ErrorList);
end;

procedure Ttriggersform.FormActivate(Sender: TObject);
begin
  SetUpUnits;
end;

procedure Ttriggersform.FormShow(Sender: TObject);
begin
  Caption := Application.Title +
    ' - Set Recommended values (for all Foobots) and Triggers (for ' +
    FoobotIdentityObject.FoobotIdentityList[mainform.iCurrentFoobot].Name + ')';
  DisplayCurrentValues;
  ErrorList.Clear;
  Update;
end;
// *****************************************************************************

procedure Ttriggersform.cmd_OKClick(Sender: TObject);
var
  s: string;
  iCount: integer;
begin
  // VerifyEveryThing then Save to inifiles;
  if  AllInputsVerified then
  begin
    AssignAndSaveTriggers;
    AssignAndSaveRecommendedLevels;
    ModalResult := mrClose;
    Close;
  end
  else
  begin
    ModalResult := mrNone;
    s := '';
    if (ErrorList.Count > 0) then
      for iCount := 0 to Pred(ErrorList.Count) do
        s := s + '* ' + ErrorList[iCount] + LineEnding;
    MessageDlg('Problems listed below', s, mtError, [mbOK], 0);
  end;
end;

procedure Ttriggersform.OnlyNumericKeyPressInteger(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9]) then
    Key := #0;
end;

procedure Ttriggersform.OnlyNumericKeyPressFloat(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', '.', #8, #9]) then
    Key := #0;
end;

function Ttriggersform.AllInputsVerified: boolean;
begin
  ErrorList.Clear;
  // Low triggers bigger than high triggers
  If StrToFloat(edt_newlowtrigger_pm.Text) > StrToFloat(edt_newhightrigger_pm.Text) then
    ErrorList.Add('Particulates low trigger is greater than the high trigger');
  If StrToFloat(edt_newlowtrigger_tmp.Text) > StrToFloat(edt_newhightrigger_tmp.Text) then
    ErrorList.Add('Temperature low trigger is greater than the high trigger');
  If StrToFloat(edt_newlowtrigger_hum.Text) > StrToFloat(edt_newhightrigger_hum.Text) then
    ErrorList.Add('Humidity low trigger is greater than the high trigger');
  If StrToFloat(edt_newlowtrigger_co2.Text) > StrToFloat(edt_newhightrigger_co2.Text) then
    ErrorList.Add('CO2 low trigger is greater than the high trigger');
  If StrToFloat(edt_newlowtrigger_voc.Text) > StrToFloat(edt_newhightrigger_voc.Text) then
    ErrorList.Add('Volatile compounds low trigger is greater than the high trigger');
  If StrToFloat(edt_newlowtrigger_allpollu.Text) > StrToFloat(edt_newhightrigger_allpollu.Text) then
    ErrorList.Add('All Pollution low trigger is greater than the high trigger');
  // Recommended levels bigger than high triggers
  If StrToFloat(edt_newrec_pm.Text) > StrToFloat(edt_newhightrigger_pm.Text) then
    ErrorList.Add('Particulates recommended level is greater than the high trigger');
  If StrToFloat(edt_newrec_tmp.Text) > StrToFloat(edt_newhightrigger_tmp.Text) then
    ErrorList.Add('Temperature recommended level is greater than the high trigger');
  If StrToFloat(edt_newrec_hum.Text) > StrToFloat(edt_newhightrigger_hum.Text) then
    ErrorList.Add('Humidity recommended level is greater than the high trigger');
  If StrToFloat(edt_newrec_co2.Text) > StrToFloat(edt_newhightrigger_co2.Text) then
    ErrorList.Add('CO2 recommended level is greater than the high trigger');
  If StrToFloat(edt_newrec_voc.Text) > StrToFloat(edt_newhightrigger_voc.Text) then
    ErrorList.Add('Volatile compounds recommended level is greater than the high trigger');
  If StrToFloat(edt_newrec_allpollu.Text) > StrToFloat(edt_newhightrigger_allpollu.Text) then
    ErrorList.Add('All Pollution recommended level is greater than the high trigger');
  // Recommended levels lower than low trigger
  If StrToFloat(edt_newrec_pm.Text) < StrToFloat(edt_newlowtrigger_pm.Text) then
    ErrorList.Add('Particulates recommended level is less than the low trigger');
  If StrToFloat(edt_newrec_tmp.Text) < StrToFloat(edt_newlowtrigger_tmp.Text) then
    ErrorList.Add('Temperature recommended level is less than the low trigger');
  If StrToFloat(edt_newrec_hum.Text) < StrToFloat(edt_newlowtrigger_hum.Text) then
    ErrorList.Add('Humidity recommended level is less than the low trigger');
  If StrToFloat(edt_newrec_co2.Text) < StrToFloat(edt_newlowtrigger_co2.Text) then
    ErrorList.Add('CO2 recommended level is less than the low trigger');
  If StrToFloat(edt_newrec_voc.Text) < StrToFloat(edt_newlowtrigger_voc.Text) then
    ErrorList.Add('Volatile compounds recommended level is less than the low trigger');
  If StrToFloat(edt_newrec_allpollu.Text) < StrToFloat(edt_newlowtrigger_allpollu.Text) then
    ErrorList.Add('All Pollution recommended level is less than the low trigger');

  // CO2 and Voc lowtrigger or rec level lower than sensor minimum
  If StrToFloat(edt_newlowtrigger_co2.Text) < 450 then
    ErrorList.Add('CO2 low trigger cannot be below 450');
  If StrToFloat(edt_newlowtrigger_voc.Text) < 125 then
    ErrorList.Add('Volatile compounds low trigger cannot be below 125');
  If StrToFloat(edt_newrec_co2.Text) < 450 then
    ErrorList.Add('CO2 recommended level cannot be below 450');
  If StrToFloat(edt_newrec_voc.Text) < 125 then
    ErrorList.Add('Volatile compounds recommended level cannot be below 125');

  If (ErrorList.Count > 0) then Result:=FALSE
  else Result := True;
end;

function Ttriggersform.AssignAndSaveTriggers: boolean;
begin
  Result := False;
  try
    FooBotTriggerArray[C_HIGH, C_PM] := StrToFloat(edt_newhightrigger_pm.Text);
    FooBotTriggerArray[C_LOW, C_PM] := StrToFloat(edt_newlowtrigger_pm.Text);
    FooBotTriggerArray[C_HIGH, C_TMP] := StrToFloat(edt_newhightrigger_tmp.Text);
    FooBotTriggerArray[C_LOW, C_TMP] := StrToFloat(edt_newlowtrigger_tmp.Text);
    FooBotTriggerArray[C_HIGH, C_HUM] := StrToFloat(edt_newhightrigger_hum.Text);
    FooBotTriggerArray[C_LOW, C_HUM] := StrToFloat(edt_newlowtrigger_hum.Text);
    FooBotTriggerArray[C_HIGH, C_CO2] := StrToInt(edt_newhightrigger_co2.Text);
    FooBotTriggerArray[C_LOW, C_CO2] := StrToInt(edt_newlowtrigger_co2.Text);
    FooBotTriggerArray[C_HIGH, C_VOC] := StrToInt(edt_newhightrigger_voc.Text);
    FooBotTriggerArray[C_LOW, C_VOC] := StrToInt(edt_newlowtrigger_voc.Text);
    FooBotTriggerArray[C_HIGH, C_ALLPOLLU] := StrToFloat(edt_newhightrigger_allpollu.Text);
    FooBotTriggerArray[C_LOW, C_ALLPOLLU] := StrToFloat(edt_newlowtrigger_allpollu.Text);
  except
    raise Exception.Create('Error in AssignAndSaveTriggers');
    Exit;
  end;
  if not SaveTriggers then
    ErrorList.Add('Unable to save new triggers to disk')
  else
    Result := True;
end;

function Ttriggersform.AssignAndSaveRecommendedLevels: boolean;
begin
  Result := False;
  try
    RecommendedLevelsArray[C_PM] := StrToFloat(edt_newrec_pm.Text);
    RecommendedLevelsArray[C_TMP] := StrToFloat(edt_newrec_tmp.Text);
    RecommendedLevelsArray[C_HUM] := StrToFloat(edt_newrec_hum.Text);
    RecommendedLevelsArray[C_CO2] := StrToFloat(edt_newrec_co2.Text);
    RecommendedLevelsArray[C_VOC] := StrToFloat(edt_newrec_voc.Text);
    RecommendedLevelsArray[C_ALLPOLLU] := StrToFloat(edt_newrec_allpollu.Text);
  except
    raise Exception.Create('Error in AssignAndSaveRecommendedLevels');
    Exit;
  end;
  if not SaveRecommendedLevels then
    ErrorList.Add('Unable to save new recommended levels to disk')
  else
    Result := True;
end;

procedure Ttriggersform.DisplayCurrentValues;
begin
   {$IFDEF DEBUGMODE}
  Exit;
{$ENDIF}
  // Recommended levels
  lbl_currentrec_pm.Caption :=
    Format('Current recommended level: %.1f %s',
    [RecommendedLevelsArray[C_PM], FoobotDataObject.Units[C_PM]]);
  lbl_currentrec_tmp.Caption :=
    Format('Current recommended level: %.1f %s',
    [RecommendedLevelsArray[C_TMP], FoobotDataObject.Units[C_TMP]]);
  lbl_currentrec_hum.Caption :=
    Format('Current recommended level: %.1f %s',
    [RecommendedLevelsArray[C_HUM], FoobotDataObject.Units[C_HUM]]);
  lbl_currentrec_co2.Caption :=
    Format('Current recommended level: %.0f %s',
    [RecommendedLevelsArray[C_CO2], FoobotDataObject.Units[C_CO2]]);
  lbl_currentrec_voc.Caption :=
    Format('Current recommended level: %.0f %s',
    [RecommendedLevelsArray[C_VOC], FoobotDataObject.Units[C_VOC]]);
  lbl_currentrec_allpollu.Caption :=
    Format('Current recommended level: %.1f %s',
    [RecommendedLevelsArray[C_ALLPOLLU], FoobotDataObject.Units[C_ALLPOLLU]]);
  // Trigger highs
  lbl_currenthightrigger_pm.Caption :=
    Format('Current high trigger: %.1f %s',
    [double(FooBotTriggerArray[C_HIGH, C_PM]), FoobotDataObject.Units[C_PM]]);
  lbl_currenthightrigger_tmp.Caption :=
    Format('Current high trigger: %.1f %s',
    [double(FooBotTriggerArray[C_HIGH, C_TMP]), FoobotDataObject.Units[C_TMP]]);
  lbl_currenthightrigger_hum.Caption :=
    Format('Current high trigger: %.1f %s',
    [double(FooBotTriggerArray[C_HIGH, C_HUM]), FoobotDataObject.Units[C_HUM]]);
  lbl_currenthightrigger_co2.Caption :=
    Format('Current high trigger: %.0f %s',
    [double(FooBotTriggerArray[C_HIGH, C_CO2]), FoobotDataObject.Units[C_CO2]]);
  lbl_currenthightrigger_voc.Caption :=
    Format('Current high trigger: %.0f %s',
    [double(FooBotTriggerArray[C_HIGH, C_VOC]), FoobotDataObject.Units[C_VOC]]);
  lbl_currenthightrigger_allpollu.Caption :=
    Format('Current high trigger: %.1f %s',
    [double(FooBotTriggerArray[C_HIGH, C_ALLPOLLU]),
    FoobotDataObject.Units[C_ALLPOLLU]]);
  // Trigger lows
  lbl_currentlowtrigger_pm.Caption :=
    Format('Current low trigger: %.1f %s',
    [double(FooBotTriggerArray[C_LOW, C_PM]), FoobotDataObject.Units[C_PM]]);
  lbl_currentlowtrigger_tmp.Caption :=
    Format('Current low trigger: %.1f %s',
    [double(FooBotTriggerArray[C_LOW, C_TMP]), FoobotDataObject.Units[C_TMP]]);
  lbl_currentlowtrigger_hum.Caption :=
    Format('Current low trigger: %.1f %s',
    [double(FooBotTriggerArray[C_LOW, C_HUM]), FoobotDataObject.Units[C_HUM]]);
  lbl_currentlowtrigger_co2.Caption :=
    Format('Current low trigger: %.0f %s',
    [double(FooBotTriggerArray[C_LOW, C_CO2]), FoobotDataObject.Units[C_CO2]]);
  lbl_currentlowtrigger_voc.Caption :=
    Format('Current low trigger: %.0f %s',
    [double(FooBotTriggerArray[C_LOW, C_VOC]), FoobotDataObject.Units[C_VOC]]);
  lbl_currentlowtrigger_allpollu.Caption :=
    Format('Current low trigger: %.1f %s',
    [double(FooBotTriggerArray[C_LOW, C_ALLPOLLU]), FoobotDataObject.Units[C_ALLPOLLU]]);

  // Assign Edit control values
  // Recommended
  edt_newrec_pm.Text := Format('%.1f', [RecommendedLevelsArray[C_PM]]);
  edt_newrec_tmp.Text := Format('%.1f', [RecommendedLevelsArray[C_TMP]]);
  edt_newrec_hum.Text := Format('%.1f', [RecommendedLevelsArray[C_HUM]]);
  edt_newrec_co2.Text := Format('%.0f', [RecommendedLevelsArray[C_CO2]]);
  edt_newrec_voc.Text := Format('%.0f', [RecommendedLevelsArray[C_VOC]]);
  edt_newrec_allpollu.Text := Format('%.1f', [RecommendedLevelsArray[C_ALLPOLLU]]);
  // HighTrigger
  edt_newhightrigger_pm.Text := Format('%.1f', [double(FooBotTriggerArray[C_HIGH, C_PM])]);
  edt_newhightrigger_tmp.Text :=
    Format('%.1f', [double(FooBotTriggerArray[C_HIGH, C_TMP])]);
  edt_newhightrigger_hum.Text :=
    Format('%.1f', [double(FooBotTriggerArray[C_HIGH, C_HUM])]);
  edt_newhightrigger_co2.Text :=
    Format('%.0f', [double(FooBotTriggerArray[C_HIGH, C_CO2])]);
  edt_newhightrigger_voc.Text :=
    Format('%.0f', [double(FooBotTriggerArray[C_HIGH, C_VOC])]);
  edt_newhightrigger_allpollu.Text :=
    Format('%.1f', [double(FooBotTriggerArray[C_HIGH, C_ALLPOLLU])]);
  // LowTrigger
  edt_newlowtrigger_pm.Text := Format('%.1f', [double(FooBotTriggerArray[C_LOW, C_PM])]);
  edt_newlowtrigger_tmp.Text := Format('%.1f', [double(FooBotTriggerArray[C_LOW, C_TMP])]);
  edt_newlowtrigger_hum.Text := Format('%.1f', [double(FooBotTriggerArray[C_LOW, C_HUM])]);
  edt_newlowtrigger_co2.Text := Format('%.0f', [double(FooBotTriggerArray[C_LOW, C_CO2])]);
  edt_newlowtrigger_voc.Text := Format('%.0f', [double(FooBotTriggerArray[C_LOW, C_VOC])]);
  edt_newlowtrigger_allpollu.Text :=
    Format('%.1f', [double(FooBotTriggerArray[C_LOW, C_ALLPOLLU])]);

end;

procedure Ttriggersform.SetUpUnits;
var
  s: string;
begin
  s := Format('%s', [FoobotDataObject.Units[C_PM]]);
  lbl_currentrecunits_pm.Caption := s;
  lbl_currenthightriggerunits_pm.Caption := s;
  lbl_currentlowtriggerunits_pm.Caption := s;

  s := Format('%s', [FoobotDataObject.Units[C_TMP]]);
  lbl_currentrecunits_tmp.Caption := s;
  lbl_currenthightriggerunits_tmp.Caption := s;
  lbl_currentlowtriggerunits_tmp.Caption := s;

  s := Format('%s', [FoobotDataObject.Units[C_HUM]]);
  lbl_currentrecunits_hum.Caption := s;
  lbl_currenthightriggerunits_hum.Caption := s;
  lbl_currentlowtriggerunits_hum.Caption := s;

  s := Format('%s', [FoobotDataObject.Units[C_CO2]]);
  lbl_currentrecunits_co2.Caption := s;
  lbl_currenthightriggerunits_co2.Caption := s;
  lbl_currentlowtriggerunits_co2.Caption := s;

  s := Format('%s', [FoobotDataObject.Units[C_VOC]]);
  lbl_currentrecunits_voc.Caption := s;
  lbl_currenthightriggerunits_voc.Caption := s;
  lbl_currentlowtriggerunits_voc.Caption := s;

  s := Format('%s', [FoobotDataObject.Units[C_ALLPOLLU]]);
  lbl_currentrecunits_allpollu.Caption := s;
  lbl_currenthightriggerunits_allpollu.Caption := s;
  lbl_currentlowtriggerunits_allpollu.Caption := s;
end;

end.
