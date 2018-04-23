unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  PopupNotifier, ExtCtrls, ColorBox, MRUList, rxPopupNotifier, rxtooledit, DB;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ColorBox1: TColorBox;
    Label1: TLabel;
    RxPopupNotifier1: TRxPopupNotifier;
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Assigned(FR) then
    FR.Active:=true
  else
    FR:=RxPopupNotifier1.AddNotifyItem('Information', 'Static text information');
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  R1: TRxPopupNotifierItem;
begin
  Inc(FCurID);
  R1:=RxPopupNotifier1.AddNotifyItem('Warning', 'Error message № ' + IntToStr(FCurID));
  R1.ShowCloseTimer:=true;
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
  end
end;

procedure TForm1.RxPopupNotifier1NotifiClick(Sender: TRxPopupNotifier;
  AItem: TRxPopupNotifierItem);
begin
  ShowMessage('Click');
end;

end.

