program XPBarDemo;

{$MODE DELPHI}

uses
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
