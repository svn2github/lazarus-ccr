program BmpAnimDemo;

uses
  Interfaces,
  Forms,
  BmpAnimMainFormU in 'BmpAnimMainFormU.pas' {BmpAnimMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBmpAnimMainForm, BmpAnimMainForm);
  Application.Run;
end.
