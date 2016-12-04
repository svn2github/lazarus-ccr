program WinXPBarDemo;

uses
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas', JvXPCtrlsLaz;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
