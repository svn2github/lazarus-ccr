unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxspin, rxtooledit, RxRangeSel, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls, ComCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    RxRangeSelector1: TRxRangeSelector;
    procedure RadioGroup1Click(Sender: TObject);
    procedure RxRangeSelector1Change(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation
uses Themes;

{$R *.lfm}

{ TForm1 }

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  RxRangeSelector1.Style:=TRxRangeSelectorStyle(RadioGroup1.ItemIndex);
end;

procedure TForm1.RxRangeSelector1Change(Sender: TObject);
begin
  Label1.Caption:=FloatToStr(RxRangeSelector1.SelectedStart) + ' - ' + FloatToStr(RxRangeSelector1.SelectedEnd);
end;

end.

