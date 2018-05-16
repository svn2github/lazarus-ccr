unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ColorBox, ComCtrls, Buttons, rxPopupNotifier;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    ColorBox1: TColorBox;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    RxPopupNotifier1: TRxPopupNotifier;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RxPopupNotifier1NotifiClick(Sender: TRxPopupNotifier;
      AItem: TRxPopupNotifierItem);
  private
    FRClose: TRxPopupNotifierItem;
    FR: TRxPopupNotifierItem;
    FCurID:integer;
  public

  end;

var
  Form1: TForm1;

implementation
uses rxAppUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Assigned(FR) then
    FR.Active:=true
  else
    FR:=RxPopupNotifier1.AddNotifyItem('Information', 'Static text information');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  R: TRxPopupNotifierItem;
begin
  if RxPopupNotifier1.Items.Count>0 then
  begin
    R:=RxPopupNotifier1.Items[0];
    R.Active:=true;

  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  R1: TRxPopupNotifierItem;
begin
  Inc(FCurID);
  R1:=RxPopupNotifier1.AddNotifyItem('Warning', 'Error message № ' + IntToStr(FCurID));

  if TrackBar1.Position < 255 then
  begin
    R1.AlphaBlend:=true;
    R1.AlphaBlendValue:=TrackBar1.Position;
  end;

  R1.ShowCloseTimer:=CheckBox1.Checked;
  R1.Color:=ColorBox1.Selected;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if Assigned(FRClose) then
    FRClose.Active:=true
  else
  begin
    FRClose:=RxPopupNotifier1.AddNotifyItem('Information', 'Static text information without close');
    FRClose.ShowCloseTimer:=false;
  end;

  RxMessageBeep(mbsIconExclamation);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  RxPopupNotifier1.MessageCorner:=TRxPopupNotifierCorner(RadioGroup1.ItemIndex);
//  Hint:=;
end;

procedure TForm1.RxPopupNotifier1NotifiClick(Sender: TRxPopupNotifier;
  AItem: TRxPopupNotifierItem);
begin
  ShowMessage('Click');
end;

end.

