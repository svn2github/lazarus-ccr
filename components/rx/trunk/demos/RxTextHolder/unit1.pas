unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, StrHolder,
  RxHistoryNavigator, RxTextHolder, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    RxTextHolder1: TRxTextHolder;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Text:=RxTextHolder1['Line 1'];
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Text:=RxTextHolder1['Line 2'];
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Text:=RxTextHolder1['Line 3'];
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Text:=RxTextHolder1['Line 4'];
end;

end.

