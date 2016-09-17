program mormotdemo;

{ IMPORTANT NOTE:
  If compilation aborts with an error that libkernel32a, libgcc.a and libmsvcrt.a
  cannot be found then do a rebuild ("Run" / "Build"). Or add the corresponding
  folder to the library path of the project, the obj files are in folder
  mORMotSourceHere/fpc-win32, etc. }

{ commandline parameters:
  - noserver ---> run without server
  - localhost ---> server ip is localhost
  - empty ---> server is tvplanit demo server on Amazon Web Services
  - xxx.xxx.xxx.xxx  ---> ip address for server connection. }

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

