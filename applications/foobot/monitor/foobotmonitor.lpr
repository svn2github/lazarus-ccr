program foobotmonitor;
{ Foobot Monitor

  Copyright (C)2016 Gordon Bamber minsadorada@charcodelvalle.com

  This project requires FPCv3.1.1+ and Lazarus V1.7+

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

REQUIREMENTS
============
* Custom sensors unit from Industrial package (foobot_sendors and foobot_sensors.res)
* Custom httpdclient unit (foobot_httpclient)
* CryptINI, TChart and Industrial components
* FPCv3.1.1+ and Lazarus V1.7+
* A connected Foobot
* Compiles successfully in Windows32/64, Linux 32/64 Windows GTK2
* Untested in Darwin and other OS's
}
{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$ifdef CPUARM}
      //if GUI, then uncomment
      //{$linklib GLESv2}
    {$endif}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, usplash, tachartlazaruspkg, umainform, uconfigform,
  utriggersform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Foobot monitor';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  SplashForm := TSplashForm.Create(Application);
  SplashForm.Show;
  Application.ProcessMessages; // process splash paint message
  Application.CreateForm(Tmainform, mainform);
  Application.CreateForm(Tconfigform, configform);
  Application.CreateForm(Ttriggersform, triggersform);
  Application.Run;
end.

