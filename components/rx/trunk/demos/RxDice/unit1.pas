unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, rxdice,
  rxswitch;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    RxDice1: TRxDice;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RxDice1Start(Sender: TObject);
    procedure RxDice1Stop(Sender: TObject);
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
  RxDice1.Interval:=SpinEdit1.Value;
  RxDice1.AutoStopInterval:=SpinEdit2.Value * 10;
  RxDice1.Rotate:=true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RxDice1.RandomValue;
  RxDice1Stop(nil);
end;

procedure TForm1.RxDice1Start(Sender: TObject);
begin
  SpinEdit1.Enabled:=not RxDice1.Rotate;
  SpinEdit2.Enabled:=not RxDice1.Rotate;
  Label1.Enabled:=not RxDice1.Rotate;
  Label2.Enabled:=not RxDice1.Rotate;
  Label4.Enabled:=not RxDice1.Rotate;
  Button1.Enabled:=not RxDice1.Rotate;
end;

procedure TForm1.RxDice1Stop(Sender: TObject);
begin
  RxDice1Start(nil);
  Label4.Caption:=Format('Current value : %d', [RxDice1.Value]);
end;

end.

