program JvLEDDemo;

uses
  Interfaces, Forms,
  LEDMain in 'LEDMain.pas' {LEDDemoMain};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TLEDDemoMain, LEDDemoMain);
  Application.Run;
end.
