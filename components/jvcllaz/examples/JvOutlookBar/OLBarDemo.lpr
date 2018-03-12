program OLBarDemo;

uses
  Forms, Interfaces, LCLVersion,
  OLBarMainFormU in 'OLBarMainFormU.pas' {OLBarMainForm};

{$R *.res}

begin
  {$IF LCL_FullVersion >= 1080000}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TOLBarMainForm, OLBarMainForm);
  Application.Run;
end.
