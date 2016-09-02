program mormotdemo;

{ commandline parameters:
  -noserver ---> run without server, otherwise HostIP is localhost }

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, demoMain, laz_visualplanit, LCLTranslator, DefaultTranslator,
  printer4lazarus, mormotdatamodule;

{$R *.res}

begin
  Application.Title := 'TurboPower VisualPlanIt & mORMot Demo';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TDemoDM, DemoDM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

