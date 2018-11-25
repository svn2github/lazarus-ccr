unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  JvFormAnimatedIcon, JvAppAnimatedIcon;

type

  { TForm1 }

  TForm1 = class(TForm)
    IconImages: TImageList;
    JvAppAnimatedIcon1: TJvAppAnimatedIcon;
    JvFormAnimatedIcon1: TJvFormAnimatedIcon;
    Label1: TLabel;
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

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
end;

end.

