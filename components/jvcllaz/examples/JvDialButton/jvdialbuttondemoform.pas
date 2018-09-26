unit JvDialButtonDemoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, JvDialButton,
  JvLED;

type

  { TForm1 }

  TForm1 = class(TForm)
    cmbPointerShape: TComboBox;
    cmbBorderStyle: TComboBox;
    JvDialButton1: TJvDialButton;
    JvLED1: TJvLED;
    JvLED10: TJvLED;
    JvLED2: TJvLED;
    JvLED3: TJvLED;
    JvLED4: TJvLED;
    JvLED5: TJvLED;
    JvLED6: TJvLED;
    JvLED7: TJvLED;
    JvLED8: TJvLED;
    JvLED9: TJvLED;
    lblPointerShape: TLabel;
    lblBorderStyle: TLabel;
    procedure cmbBorderStyleChange(Sender: TObject);
    procedure cmbPointerShapeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvDialButton1Change(Sender: TObject);
  private
    FLEDs: array[0..9] of TJvLED;
    FReady: Boolean;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.JvDialButton1Change(Sender: TObject);
var
  i: Integer;
begin
  if not FReady then
    exit;
  for i := Low(FLEDs) to High(FLEDs) do
    FLEDs[i].Status := JvDialButton1.Position > i*10;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLEDs[0] := JvLED10;
  FLEDs[1] := JvLED9;
  FLEDs[2] := JvLED8;
  FLEDs[3] := JvLED7;
  FLEDs[4] := JvLED6;
  FLEDs[5] := JvLED5;
  FLEDs[6] := JvLED4;
  FLEDs[7] := JvLED3;
  FLEDs[8] := JvLED2;
  FLEDs[9] := JVLED1;
  FReady := true;

  cmbPointerShapeChange(nil);
end;

procedure TForm1.cmbPointerShapeChange(Sender: TObject);
begin
  JvDialButton1.PointerShape := TJvDialPointerShape(cmbPointerShape.ItemIndex);
end;

procedure TForm1.cmbBorderStyleChange(Sender: TObject);
begin
  case cmbBorderStyle.ItemIndex of
    0: JvDialButton1.BorderStyle := bsNone;
    1: JvDialButton1.BorderStyle := bsSingle;
  end;
end;

end.

