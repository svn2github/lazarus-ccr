program JvFullColorCircleDialogPrj;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms,
  JvFullColorCircleDialogMainForm in 'JvFullColorCircleDialogMainForm.pas' {JvFullColorCircleDlgMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvFullColorCircleDlgMainFrm, JvFullColorCircleDlgMainFrm);
  Application.Run;
end.
