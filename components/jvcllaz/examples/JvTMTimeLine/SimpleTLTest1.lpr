program SimpleTLTest1;

{$mode objfpc}{$H+}

uses
  Forms, datetimectrls, Interfaces, LCLVersion,
  frmMemoEdit in 'frmMemoEdit.pas' {MemoEditFrm},
  TMTimeLineMainFormU in 'TMTimeLineMainFormU.pas' {TMTimeLineMainForm};

{$R *.res}

begin
  {$IF LCL_FULLVERSION >= 1080000}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TTMTimeLineMainForm, TMTimeLineMainForm);
  Application.Run;
end.
