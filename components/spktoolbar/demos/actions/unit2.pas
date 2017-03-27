unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BtnClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Label3Click(Sender: TObject);
    procedure Label3MouseEnter(Sender: TObject);
    procedure Label3MouseLeave(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf;

{ TAboutForm }

procedure TAboutForm.Label3Click(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TAboutForm.Label3MouseEnter(Sender: TObject);
begin
  Label3.Font.Style := [fsUnderline];
end;

procedure TAboutForm.Label3MouseLeave(Sender: TObject);
begin
  Label3.Font.Style := [];
end;

end.

