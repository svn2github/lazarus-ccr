program contributors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uanimationbasic, 
uanimationcontributors, 
ufrmcontributors, uanimationtypes;

{$R *.res}

begin
  Application.Title:='Lazarus Contributors Animation';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmContributors, frmContributors);
  Application.Run;
end.

