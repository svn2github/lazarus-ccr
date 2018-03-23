program JvThumbnailDemo;

uses
  Forms, Interfaces, JvThumbnailMainFormU, JvThumbnailChildFormU, JvThumbnailDatamodule {JvThumbnailMainForm};

{$R *.RES}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TJvThumbnailMainForm, JvThumbnailMainForm);
//  Application.CreateForm(TJvThumbnailChildForm, JvThumbnailChildForm);
  Application.Run;
end.
