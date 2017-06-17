program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Unit1, Forms, lclversion, lazcolorpalette
  { you can add units after this };

{$R *.res}

begin
  {$IF LCL_FULLVERSION >= 1080000}
  Application.Scaled := True;
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

