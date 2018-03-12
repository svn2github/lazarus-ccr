program JvSpellCheckerDemo;

uses
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSpellCheckerForm in 'JvSpellCheckerForm.pas' {frmSpellChecker};

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSpellChecker, frmSpellChecker);
  Application.Run;
end.
