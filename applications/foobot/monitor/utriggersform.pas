unit utriggersform;

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
    procedure OnlyNumericKeyPress(Sender: TObject; var Key: char);
  private
    ErrorList:TStrings;
    procedure DisplayCurrentValues;
    procedure SetUpUnits;
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
  ErrorList:=TStringList.Create;
end;

procedure Ttriggersform.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ErrorList);
end;

procedure Ttriggersform.FormActivate(Sender: TObject);
begin
  SetUpUnits;
end;

procedure Ttriggersform.cmd_OKClick(Sender: TObject);
begin
  // VerifyEveryThing then Save to inifiles;
end;

procedure Ttriggersform.OnlyNumericKeyPress(Sender: TObject; var Key: char);
begin
   if not (Key in ['0'..'9', '.', #8, #9]) then Key := #0;
end;

procedure Ttriggersform.FormShow(Sender: TObject);
begin
  Caption := Application.Title + ' - Set Recommended values (for all Foobots) and Triggers (for ' +
  FoobotIdentityObject.FoobotIdentityList[mainform.iCurrentFoobot].Name + ')';
  DisplayCurrentValues;
  ErrorList.Clear;
  Update;
end;

procedure Ttriggersform.DisplayCurrentValues;
begin
   {$IFDEF DEBUGMODE}
  Exit;
{$ENDIF}
  // Recommended levels
  lbl_currentrec_pm.Caption :=
    Format('Current recommended level: %.1f %s', [double(REC_PM),
    FoobotDataObject.Units[C_PM]]);
  lbl_currentrec_tmp.Caption :=
    Format('Current recommended level: %.1f %s', [double(REC_TMP),
    FoobotDataObject.Units[C_TMP]]);
  lbl_currentrec_hum.Caption :=
    Format('Current recommended level: %.1f %s', [double(REC_HUM),
    FoobotDataObject.Units[C_HUM]]);
  lbl_currentrec_co2.Caption :=
    Format('Current recommended level: %.0f %s', [double(REC_CO2),
    FoobotDataObject.Units[C_CO2]]);
  lbl_currentrec_voc.Caption :=
    Format('Current recommended level: %.0f %s', [double(REC_VOC),
    FoobotDataObject.Units[C_VOC]]);
  lbl_currentrec_allpollu.Caption :=
    Format('Current recommended level: %.1f %s',
    [double(REC_ALLPOLLU), FoobotDataObject.Units[C_ALLPOLLU]]);
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
    Format('Current high trigger: %.1f %s',
    [double(FooBotTriggerArray[C_HIGH, C_CO2]), FoobotDataObject.Units[C_CO2]]);
  lbl_currenthightrigger_voc.Caption :=
    Format('Current high trigger: %.1f %s',
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
    Format('Current low trigger: %.1f %s',
    [double(FooBotTriggerArray[C_LOW, C_CO2]), FoobotDataObject.Units[C_CO2]]);
  lbl_currentlowtrigger_voc.Caption :=
    Format('Current low trigger: %.1f %s',
    [double(FooBotTriggerArray[C_LOW, C_VOC]), FoobotDataObject.Units[C_VOC]]);
  lbl_currentlowtrigger_allpollu.Caption :=
    Format('Current low trigger: %.1f %s',
    [double(FooBotTriggerArray[C_LOW, C_ALLPOLLU]), FoobotDataObject.Units[C_ALLPOLLU]]);
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
