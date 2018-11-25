unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  JvFormAnimatedIcon, JvAppAnimatedIcon, JvAnimTitle;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    IconImages: TImageList;
    JvAnimTitle1: TJvAnimTitle;
    JvAppAnimatedIcon1: TJvAppAnimatedIcon;
    JvFormAnimatedIcon1: TJvFormAnimatedIcon;
    Label1: TLabel;
    ListView1: TListView;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  FORM_CAPTION = 'JvFormAnimations Demo';

procedure TForm1.FormCreate(Sender: TObject);

  procedure AddItem(AIndex: Integer);
  begin
    with Listview1.Items.Add do begin
      Caption := 'Image' + IntToStr(AIndex);
      ImageIndex := AIndex;
    end;
  end;

var
  i: Integer;
begin
  JvAppAnimatedIcon1.Active := true;
  for i:=0 to IconImages.Count-1 do
    AddItem(i);

  Checkbox1Change(nil);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  JvAnimTitle1.Enabled := Checkbox1.Checked;
  JvAnimTitle1.Title := FORM_CAPTION;
  if not JvAnimTitle1.Enabled then Caption := FORM_CAPTION;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  JvFormAnimatedIcon1.Active := Checkbox2.Checked;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  JvAppAnimatedIcon1.Active := Checkbox3.Checked;
end;

end.

