unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses rxAppUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  R: TRxMsgBeepStyle;
begin
  R:=TRxMsgBeepStyle(RadioGroup1.ItemIndex);
  RxMessageBeep(R);
end;

end.

