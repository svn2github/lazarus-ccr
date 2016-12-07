program ExCust;

uses
  Forms, Interfaces,
  ExCustu in 'ExCustu.pas', lazff2 {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
