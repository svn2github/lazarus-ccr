unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxdbgrid, rxmemds, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    dsData: TDataSource;
    Label1: TLabel;
    Panel1: TPanel;
    rxDataCODE: TLongintField;
    rxDataDATE: TDateTimeField;
    rxDataNAME: TStringField;
    RxDBGrid1: TRxDBGrid;
    rxData: TRxMemoryData;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rxDataAfterScroll(DataSet: TDataSet);
  private
    procedure RxDBGridMergeCellsEvent(Sender: TObject; ACol: Integer; Column: TRxColumn;
      var ALeft, ARight: Integer);
  public

  end;

var
  Form1: TForm1;

implementation
uses Grids;

{$R *.lfm}

{ TForm1 }

type
  THackDataGrid = class(TRxDBGrid);

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  rxData.Open;
  for i:=1 to 30 do
    rxData.AppendRecord([i, Date - i, 'Line '+IntToStr(i)]);
  rxData.First;

  RxDBGrid1.OnMergeCells:=@RxDBGridMergeCellsEvent;
end;

procedure TForm1.rxDataAfterScroll(DataSet: TDataSet);
begin
  Label1.Caption:=Format('Datalink.ActiveRecord=%d, Row = %d', [THackDataGrid(RxDBGrid1).Datalink.ActiveRecord, TDrawGrid(RxDBGrid1).Row]);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx + [rdgColSpanning]
  else
    RxDBGrid1.OptionsRx:=RxDBGrid1.OptionsRx - [rdgColSpanning];
end;

procedure TForm1.RxDBGridMergeCellsEvent(Sender: TObject; ACol: Integer;
  Column: TRxColumn; var ALeft, ARight: Integer);
begin
   if rxDataCODE.AsInteger mod 10 = 1 then
   begin
     ALeft:=1;
     ARight:=3;
   end;
end;

end.

