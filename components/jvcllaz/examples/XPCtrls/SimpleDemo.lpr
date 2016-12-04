program SimpleDemo;

uses
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas', JvXPCtrlsLaz {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
