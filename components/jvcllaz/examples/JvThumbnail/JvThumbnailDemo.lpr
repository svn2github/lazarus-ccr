program JvThumbnailDemo;

uses
  Forms, Interfaces, JvThumbnailMainFormU, JvThumbnailChildFormU {JvThumbnailMainForm};

{$R *.RES}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TJvThumbnailMainForm, JvThumbnailMainForm);
//  Application.CreateForm(TJvThumbnailChildForm, JvThumbnailChildForm);
  Application.Run;
end.
