program SimpleTLTest1;

{$mode objfpc}{$H+}

uses
  Forms, datetimectrls, Interfaces,
  frmMemoEdit in 'frmMemoEdit.pas' {MemoEditFrm},
  TMTimeLineMainFormU in 'TMTimeLineMainFormU.pas' {TMTimeLineMainForm};

{$R *.RES}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TTMTimeLineMainForm, TMTimeLineMainForm);
  Application.Run;
end.
