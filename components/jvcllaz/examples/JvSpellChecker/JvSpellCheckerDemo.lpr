program JvSpellCheckerDemo;

uses
  Forms, Interfaces, LCLVersion,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSpellCheckerForm in 'JvSpellCheckerForm.pas' {frmSpellChecker};

{$R *.res}

begin
  {$IF LCL_FullVersion >= 1080000}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSpellChecker, frmSpellChecker);
  Application.Run;
end.
