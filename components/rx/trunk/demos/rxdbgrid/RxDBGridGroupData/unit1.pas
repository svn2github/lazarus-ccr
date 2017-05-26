unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxdbgrid, rxmemds, Forms, Controls, Graphics,
  Dialogs, StdCtrls, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    dsData: TDataSource;
    rxDataAAA: TStringField;
    rxDataGROUP_ID: TLongintField;
    rxDataID: TLongintField;
    rxDataSUM: TCurrencyField;
    rxDataTEXT: TStringField;
    RxDBGrid1: TRxDBGrid;
    rxData: TRxMemoryData;
    procedure CheckBox1Change(Sender: TObject);
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
var
  i: Integer;
begin
  rxData.Open;
  for i:=1 to 200 do
    rxData.AppendRecord([i, ((i-1) div 4) + 1, Random * 1000, Format('Test string %d', [i])]);

  RxDBGrid1.GroupItems.GroupFieldName:='GROUP_ID';
  RxDBGrid1.ColumnByFieldName('SUM').GroupParam.ValueType:=fvtSum;
  RxDBGrid1.ColumnByFieldName('SUM').GroupParam.Alignment:=taRightJustify;

  RxDBGrid1.ColumnByFieldName('GROUP_ID').GroupParam.ValueType:=fvtCount;

  RxDBGrid1.ColumnByFieldName('ID').GroupParam.ValueType:=fvtStaticText;
  RxDBGrid1.ColumnByFieldName('ID').GroupParam.StaticText:='Группа:';

  RxDBGrid1.ColumnByFieldName('AAA').GroupParam.ValueType:=fvtFieldValue;
  RxDBGrid1.ColumnByFieldName('AAA').GroupParam.Alignment:=taCenter;
  RxDBGrid1.ColumnByFieldName('AAA').GroupParam.Color:=clRed;
  RxDBGrid1.GroupItems.Color:=clSkyBlue;

  rxData.First;

  CheckBox1Change(nil);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  RxDBGrid1.GroupItems.Active:=CheckBox1.Checked;
  RxDBGrid1.FooterOptions.Active:=CheckBox2.Checked;
  RxDBGrid1.ReadOnly:=CheckBox3.Checked;
end;

end.

