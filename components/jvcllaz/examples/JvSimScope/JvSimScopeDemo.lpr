program JvSimScopeDemo;

uses
  Forms, Interfaces, LclVersion,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  {$IF LCL_FullVersion >= 1080000}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
