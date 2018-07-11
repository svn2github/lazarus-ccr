AntiAliased AnalogGauge component for Delphi
============================================
2002, Irnis Haliullin (irnis@irnis.net)

based on AnalogGauge component source
by Shteg Vyacheslav (slast@softhome.net),

use the supersampling algoritm source
by Nacho Urenda (nurenda@wanadoo.es)


Overview
--------------------------------------------
A3nalogGauge - simple component for displaying
floating point value as analog voltmeter. In
addition to AnalogGauge component, the A3nalogGauge 
allow to use the supersampling antialiasing algorithm
for improvement the image quality. Besides, the
A3nalogGauge contains additional properties for
more powerfull control of the own face. And also
others, less significant improvements.

Licenses
--------------------------------------------
This component is free for use in any purposes.

Contacts
--------------------------------------------
All questons, bugreports and suggestions 
please send by mail to: irnis@irnis.net


Reference
--------------------------------------------
The component contains fallowing properties:

 - these properties define the voltmeter elements colors:
  * MinColor: TColor - color of indicator of minimum range;
  * MidColor: TColor - color of indicator of normal range;
  * MaxColor: TColor - color of indicator of maximum range;
  * FaceColor: TColor - voltmeter scale background color;
  * TicksColor: TColor - voltmeter scale mark lines color;
  * ValueColor: TColor - color of values on mark lines;
  * CaptionColor: TColor - values unit name color;
  * ArrowColor: TColor - voltmeter pointer arrow color;
  * MarginColor: TColor - margin rectangle color;
  * CenterColor: TColor - color of a knob on an axis of the pointer arrow;
  * CircleColor: TColor - color of circles on mark lines;

 - these properties define the voltmeter elements sizes:
  * CenterRadius: Integer - radius of a knob on an axis of the pointer arrow;
  * CircleRadius: Integer - radius of circles on mark lines;
  * Angle: Integer - the angle of scale segment;
  * Margin: Integer - margin width;
  * ArrowWidth: Integer - width of pointer arrow;
  * LengthMainTicks: Integer - main (marked) tick lines length;
  * LengthSubTicks: Integer - additional tick lines length;
  * Style: TStyle read - the scale styles (left corner, on center and
                         right corner);
  * NumberMainTicks: Integer - number of main tick lines;
  * FaceOptions: TFaceOptions - defines what voltmeter elements will
                                be displayed;

  * Position: Single read - the value for display;
  * Scale: Integer - the maximal value for display (minimal is always 0);
  * IndMaximum: Integer - maximal value for normal range;
  * IndMinimum: Integer - minimal value for normal range;
  * Caption: string - this text will be displayed in the voltmeter centre;
  * AntiAliased: TAntialiased = (aaNone, aaBiline, aaTriline, aaQuadral) -
         Mode of smoothing. Accordingly: without smoothing, smoothing with
         cells of 2x2, 3x3 and 4x4 pixels;
  * OnOverMax: TNotifyEvent - this event is fired if the value reached
                              the maximum of normal range;
  * OnOverMin: TNotifyEvent - the same for a minimum;

 - if you have set a "TICKER" condition,  the component will calculate own
   performance:

{$IFDEF TICKER}
  * Frames: Integer - last repaint speed, frames per second (FPS);
  * OnFrames: TNotifyEvent - this event will fired when performance value
                             has been changed;
{$ENDIF}

