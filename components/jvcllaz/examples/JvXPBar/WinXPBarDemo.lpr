program WinXPBarDemo;

uses
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas', JvXPBarLaz {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
