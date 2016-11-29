unit jvhtcontrolsreg;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

{$R ../resource/JvHTControlsReg.res}

uses
  Classes, JvHtControls, JvDBHTLabel, JvHint, JvHTHintForm, PropEdits, Controls;

procedure Register;
begin
  RegisterComponents('JvHTControls', [TJvHTLabel, TJvHTComboBox, TJvHTListBox,
    TJvDBHTLabel, TJvHint]);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
end;

end.

