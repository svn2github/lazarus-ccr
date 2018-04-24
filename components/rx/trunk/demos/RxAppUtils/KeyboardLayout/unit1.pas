unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses LazUTF8, rxAppUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label2.Caption:='  '+UTF8UpperCase(UTF8Copy(RxGetKeyboardLayoutName, 1, 2)) + '  ';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1Timer(nil);
end;

end.

