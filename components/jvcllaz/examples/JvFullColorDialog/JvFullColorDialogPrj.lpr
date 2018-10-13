program JvFullColorDialogPrj;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  JvFullColorDialogMainForm in 'JvFullColorDialogMainForm.pas' {JvFullColorDialogMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvFullColorDialogMainFrm, JvFullColorDialogMainFrm);
  Application.Run;
end.
