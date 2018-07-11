program demo;

uses
  Interfaces, Forms,
  main in 'main.pas' {MainForm};

{$R *.RES}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
