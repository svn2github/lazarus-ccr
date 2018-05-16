unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, rxctrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    SecretPanel1: TSecretPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SecretPanel1StartPlay(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses rxlclutils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SecretPanel1.Cycled:=CheckBox1.Checked;
  SecretPanel1.Play;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SecretPanel1.Stop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SecretPanel1StartPlay(nil);
end;

procedure TForm1.SecretPanel1StartPlay(Sender: TObject);
begin
  Button1.Enabled:=not SecretPanel1.Active;
  CheckBox1.Enabled:=not SecretPanel1.Active;
  Button2.Enabled:=SecretPanel1.Active;
end;

end.

