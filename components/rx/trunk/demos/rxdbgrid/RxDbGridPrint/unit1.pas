unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Desgn, rxmemds, rxdbgrid, RxDBGridExportPdf,
  RxDBGridPrintGrid, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Spin, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    DataSource1: TDataSource;
    Edit1: TEdit;
    frDesigner1: TfrDesigner;
    ImageList1: TImageList;
    Label1: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RxDBGrid1: TRxDBGrid;
    RxDBGridPrint1: TRxDBGridPrint;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1Country: TStringField;
    RxMemoryData1FLAG: TLongintField;
    RxMemoryData1ID: TAutoIncField;
    RxMemoryData1NAME: TStringField;
    RxMemoryData1PDATE: TDateField;
    RxMemoryData1Sity: TStringField;
    RxMemoryData1SUM: TCurrencyField;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FontDirList: TStrings;
    procedure ShowInfo(AText:string; AParams : array of const);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses fpTTF, LazFileUtils, LR_Class;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ProcedureInitDesigner;

  RxDBGridPrint1.ShowSetupForm:=true;
  PageControl1.ActivePageIndex:=0;
  RxMemoryData1.Open;
  RxMemoryData1.AppendRecord([1, 'Строка с длинным текстом 1', now, 100, 'Россия', 'Москва', 0]);
  RxMemoryData1.AppendRecord([2, 'Строка с длинным текстом 2', now - 1, 100, 'Россия', 'Ставрополь', 1]);
  RxMemoryData1.AppendRecord([3, 'Строка с длинным текстом 3', now - 2, 110, 'Россия', 'Калининград', 2]);
  RxMemoryData1.AppendRecord([4, 'Строка с длинным текстом 4', now - 3, 5000, 'Россия', 'Владивасток', 0]);
  RxMemoryData1.AppendRecord([5, 'Строка с длинным текстом 5', now - 4, 123.31, 'USA', 'New-York', 0]);
  RxMemoryData1.AppendRecord([6, 'Строка с длинным текстом 6', now, 100, 'Россия', 'Москва', 0]);
  RxMemoryData1.AppendRecord([7, 'Строка с длинным текстом 7', now - 1, 100, 'Россия', 'Ставрополь', 2]);
  RxMemoryData1.AppendRecord([8, 'Строка с длинным текстом 8', now - 2, 110, 'Россия', 'Калининград', 1]);
  RxMemoryData1.AppendRecord([9, 'Строка с длинным текстом 9', now - 3, 5000, 'Россия', 'Владивасток', 0]);
  RxMemoryData1.AppendRecord([10,'Строка с длинным текстом 10', now - 4, 123.31, 'USA', 'New-York', 3]);
  RxMemoryData1.AppendRecord([11,'Строка с длинным текстом 11', now, 100, 'Россия', 'Москва', 2]);
  RxMemoryData1.AppendRecord([12,'Строка с длинным текстом 12', now - 1, 100, 'Россия', 'Ставрополь', 1]);
  RxMemoryData1.AppendRecord([13,'Строка с длинным текстом 13', now - 2, 110, 'Россия', 'Калининград', 0]);
  RxMemoryData1.AppendRecord([14,'Строка с длинным текстом 14', now - 3, 5000, 'Россия', 'Владивасток', 3]);
  RxMemoryData1.AppendRecord([15,'Строка с длинным текстом 15', now - 4, 123.31, 'USA', 'New-York', 2]);
  RxMemoryData1.AppendRecord([16,'Строка с длинным текстом 16', now, 100, 'Россия', 'Москва', 1]);
  RxMemoryData1.AppendRecord([17,'Строка с длинным текстом 17', now - 1, 100, 'Россия', 'Ставрополь', 0]);
  RxMemoryData1.AppendRecord([18,'Строка с длинным текстом 18', now - 2, 110, 'Россия', 'Калининград', 3]);
  RxMemoryData1.AppendRecord([19,'Строка с длинным текстом 19', now - 3, 5000, 'Россия', 'Владивасток', 2]);
  RxMemoryData1.AppendRecord([20,'Строка с длинным текстом 20', now - 4, 123.31, 'USA', 'New-York', 1]);
  RxMemoryData1.First;
  CheckBox2Change(nil);
end;


procedure TForm1.ShowInfo(AText: string; AParams: array of const);
begin
  Memo1.Lines.Add(Format(AText, AParams));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CheckBox2Change(nil);
  RxDBGridPrint1.ShowSetupForm:=false;
  RxDBGridPrint1.Execute;
  RxDBGridPrint1.ShowSetupForm:=true;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  RxDBGridPrint1.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
var
  O: TRxDBGridPrintOptions;
begin
  O:=RxDBGridPrint1.Options;
  if CheckBox2.Checked then
    O:=O + [rxpoShowTitle]
  else
    O:=O - [rxpoShowTitle];

  if CheckBox3.Checked then
    O:=O + [rxpoShowFooter]
  else
    O:=O - [rxpoShowFooter];

  if CheckBox4.Checked then
    O:=O + [rxpoShowGridColor]
  else
    O:=O - [rxpoShowGridColor];

  if CheckBox5.Checked then
    O:=O + [rxpoShowFooterColor]
  else
    O:=O - [rxpoShowFooterColor];

  if CheckBox6.Checked then
    O:=O + [rxpoShowReportTitle]
  else
    O:=O - [rxpoShowReportTitle];

  if CheckBox7.Checked then
    O:=O + [rxpoHideZeroValues]
  else
    O:=O - [rxpoHideZeroValues];

  if CheckBox8.Checked then
    O:=O + [rxpoColSpanning]
  else
    O:=O - [rxpoColSpanning];


  if CheckBox9.Checked then
    O:=O + [rxpoShowPreview]
  else
    O:=O - [rxpoShowPreview];

  RxDBGridPrint1.Options:=O;

  RxDBGridPrint1.ReportTitle:=Edit1.Text;
  RxDBGridPrint1.ModifyPrepared:=CheckBox10.Checked;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FontDirList);
end;

end.


