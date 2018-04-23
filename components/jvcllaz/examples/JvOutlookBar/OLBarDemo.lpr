program OLBarDemo;

uses
  Forms, Interfaces, LCLVersion,
  OLBarMainFormU in 'OLBarMainFormU.pas' {OLBarMainForm};

{$R *.res}

begin
  Application.Scaled:=True;   // Please remove this if Lazarus is older than 1.8
  Application.Initialize;
  Application.CreateForm(TOLBarMainForm, OLBarMainForm);
  Application.Run;
end.
