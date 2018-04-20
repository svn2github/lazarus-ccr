program JvId3v2Demo;

uses
  Interfaces, LCLVersion,
  Forms,
  JvId3v2MainFormU in 'JvId3v2MainFormU.pas' {JvId3v2MainForm},
  JvId3v2EditFormU in 'JvId3v2EditFormU.pas' {JvId3v2EditForm};

{$R *.res}

begin
  {$IF LCL_FullVersion >= 1080000}
  Application.Scaled:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TJvId3v2MainForm, JvId3v2MainForm);
  Application.Run;
end.
