program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, demoMain, laz_visualplanit, LCLTranslator, DefaultTranslator,
  bufdsdatamodule, printer4lazarus;

{$R *.res}

begin
  Application.Scaled := True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TDemoDM, DemoDM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

