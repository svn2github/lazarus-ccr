program foobotmonitor;
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
  Forms, tachartlazaruspkg, umainform, uconfigform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Foobot monitor';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.CreateForm(Tconfigform, configform);
  Application.Run;
end.

