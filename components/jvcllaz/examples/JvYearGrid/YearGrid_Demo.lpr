program YearGrid_Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  LclVersion, Forms, main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  {$IF LCL_FULLVERSION >= 1080000}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

