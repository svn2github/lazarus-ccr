unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MKnob,
  switches, A3nalogGauge;

type

  { TMainForm }

  TMainForm = class(TForm)
    AnalogGauge: TA3nalogGauge;
    Edit: TEdit;
    Knob: TmKnob;
    OnOffSwitch: TOnOffSwitch;
    procedure EditEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure KnobChange(Sender: TObject; AValue: Longint);
    procedure OnOffSwitchChange(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.EditEditingDone(Sender: TObject);
var
  s: String;
begin
  s := Edit.Text;
  while (s <> '') and not (s[Length(s)] in ['0'..'9']) do
    SetLength(s, Length(s)-1);
  Knob.Position := StrToInt(s);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Knob.Position := 40;
end;

procedure TMainForm.KnobChange(Sender: TObject; AValue: Longint);
begin
  AnalogGauge.Position := Knob.Position;
  Edit.Text := IntToStr(Knob.Position) + ' V';
end;

procedure TMainForm.OnOffSwitchChange(Sender: TObject);
begin
  Knob.AllowUserDrag := OnOffSwitch.Checked
end;

end.

