
{**********************************************************************
 Package industrial Lazarus
 
 This unit is part of Lazarus Project
***********************************************************************}

unit AllIndustrialRegister;

interface


 uses
  Classes, LResources, AdvLed, IndLed, LedNumber, Sensors, IndGnouMeter,
  A3nalogGauge;

procedure Register;

implementation

{$R industrial_icons.res}

//==========================================================
procedure Register;
begin
  RegisterComponents ('Industrial',[
    TAdvLed, TIndLed, TLedNumber, TStopLightSensor,
    TAnalogSensor, TA3nalogGauge, TindGnouMeter
  ]);

end;

end.
