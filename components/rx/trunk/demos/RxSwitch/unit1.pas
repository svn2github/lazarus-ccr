unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  rxswitch;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    RxSwitch1: TRxSwitch;
    RxSwitch2: TRxSwitch;
    procedure RadioGroup1Click(Sender: TObject);
    procedure RxSwitch1Off(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
const
  SwithcStateStr:array [TSwithState] of string = ('OFF', 'ON');

procedure TForm1.RxSwitch1Off(Sender: TObject);
begin
  Label1.Caption:=SwithcStateStr[RxSwitch1.StateOn];
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  RxSwitch1.Style:=TSwithStyle(RadioGroup1.ItemIndex);
end;

end.

