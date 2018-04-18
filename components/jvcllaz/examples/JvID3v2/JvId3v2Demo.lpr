program JvId3v2Demo;

uses
  Interfaces,
  Forms,
  JvId3v2MainFormU in 'JvId3v2MainFormU.pas' {JvId3v2MainForm},
  JvId3v2EditFormU in 'JvId3v2EditFormU.pas' {JvId3v2EditForm};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TJvId3v2MainForm, JvId3v2MainForm);
  Application.Run;
end.
