program JvFormatEditDemo;

uses
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas' {frmValidateEditDemo};

{$R *.RES}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmValidateEditDemo, frmValidateEditDemo);
  Application.Run;
end.

