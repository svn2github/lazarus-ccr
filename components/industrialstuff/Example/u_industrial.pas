unit u_industrial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IndLed, Sensors, LedNumber, IndGnouMeter, AdvLed,
  A3nalogGauge, Forms, Controls, Graphics, Dialogs, Arrow, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    A3nalogGauge1: TA3nalogGauge;
    AdvLed1: TAdvLed;
    AnalogSensor1: TAnalogSensor;
    Arrow1: TArrow;
    indGnouMeter1: TindGnouMeter;
    indLed1: TindLed;
    LEDNumber1: TLEDNumber;
    StopLightSensor1: TStopLightSensor;
    TrackBar1: TTrackBar;
    procedure TrackBar1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  A3nalogGauge1.Position := Trackbar1.Position;
  indGnouMeter1.Value := Trackbar1.Position;
  AnalogSensor1.Value := Trackbar1.Position;
end;

end.

