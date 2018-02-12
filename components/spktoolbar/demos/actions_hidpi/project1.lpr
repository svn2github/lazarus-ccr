program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  LCLVersion, Forms, Unit1, Unit2
  { you can add units after this };

{$R *.res}

begin
  {$IF LCL_FullVersion < 1090000}
  *** This demo requires Laz version >= 1.9 ***
  {$ENDIF}
  Application.Scaled:=True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

