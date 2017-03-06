program controlsample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmcontrolsample, 
uanimationcontrol, 
uanimationtypes;

{$R *.res}

begin
  Application.Title:='Animated Controls Sample';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmAnimationControls, frmAnimationControls);
  Application.Run;
end.

