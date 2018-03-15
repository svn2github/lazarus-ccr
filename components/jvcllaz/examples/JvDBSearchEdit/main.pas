unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, DBGrids, Buttons, JvDBSearchEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    BufDataset1Birthdate: TDateField;
    BufDataset1Name: TStringField;
    CheckBox1: TCheckBox;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    JvDBSearchEdit1: TJvDBSearchEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function RandomBirthDate: TDate;
begin
  Result := Date() - Random(20*365) - 10;
end;

{ TForm1 }

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  JvDBSearchEdit1.ClearOnEnter := Checkbox1.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with BufDataset1 do
  begin
    CreateDataset;

    // Add records
    Append;
    BufDataset1Name.AsString := 'Johnny';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'Tom';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'Dick';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'John';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'Jack';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'Michael';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'Robert';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'Harry';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    Append;
    BufDataset1Name.AsString := 'Henry';
    BufDataset1BirthDate.AsDateTime := RandomBirthdate;
    Post;

    // Index
    IndexFieldNames := 'Name';
  end;
end;

end.

