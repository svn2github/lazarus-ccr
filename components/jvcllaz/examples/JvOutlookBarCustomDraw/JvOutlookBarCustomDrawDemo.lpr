program JvOutlookBarCustomDrawDemo;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, LCLVersion,
  JvOutlookBarCustomDrawDemoMainForm in 'JvOutlookBarCustomDrawDemoMainForm.pas' {JvOutlookBarCustomDrawDemoMainFrm};

{$R *.res}

begin
  {$IFDEF LCL_FullVersion >= 1080000}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TJvOutlookBarCustomDrawDemoMainFrm, JvOutlookBarCustomDrawDemoMainFrm);
  Application.Run;
end.
