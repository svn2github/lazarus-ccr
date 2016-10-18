unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, EditBtn, ExtCtrls, ComCtrls, fpspreadsheetchart,
  fpspreadsheetgrid, TAGraph, TASeries, TypInfo,
  // FPSpreadsheet and supported formats
  fpstypes, fpspreadsheet, fpsallformats
  ;

type
  
  { Tlazfpsmainform }

  Tlazfpsmainform = class(TForm)
    btnLoadSpreadsheet: TButton;
    buttonReadCellInfo: TButton;
    editSourceFile: TFileNameEdit;
    labelCurCell: TLabel;
    Label2: TLabel;
    memoCellData: TMemo;
    pagesSheets: TPageControl;
    Panel1: TPanel;
    procedure btnLoadSpreadsheetClick(Sender: TObject);
    procedure buttonReadCellInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    procedure HandleSelectionChanged(Sender: TObject; aCol, aRow: Integer);
  public
    { public declarations }
    Worksheets: array of TsWorksheetGrid;
    Workbook: TsWorkbook;
    procedure DeleteAllSheets();
  end; 

var
  lazfpsmainform: Tlazfpsmainform;

implementation

{$R *.lfm}

uses
  fpsUtils;


{ Tlazfpsmainform }

procedure Tlazfpsmainform.btnLoadSpreadsheetClick(Sender: TObject);
var
  lWorksheetCount: Cardinal;
  lCurPage: TTabSheet;
  lCurWorksheet: TsWorksheet;
  i: Integer;
begin
  if editSourceFile.Text = '' then Exit;

  Workbook.ReadFromFile(editSourceFile.Text);

  DeleteAllSheets();

  lWorksheetCount := Workbook.GetWorksheetCount();
  SetLength(Worksheets, lWorksheetCount);
  for i := 0 to lWorksheetCount-1 do
  begin
    pagesSheets.AddTabSheet();
    lCurPage := pagesSheets.Pages[i];
    lCurWorksheet := Workbook.GetWorksheetByIndex(i);

    Worksheets[i] := TsWorksheetGrid.Create(nil);
    Worksheets[i].Parent := lCurPage;
    Worksheets[i].Align := alClient;
    //Worksheets[i].Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goSmoothScroll]
    Worksheets[i].LoadFromWorkbook(Workbook, i); //LoadFromWorksheet(lCurWorksheet);
    Worksheets[i].OnSelection := @HandleSelectionChanged;
    lCurPage.Caption := lCurWorksheet.Name;
  end;
end;

procedure Tlazfpsmainform.buttonReadCellInfoClick(Sender: TObject);
var
  lX, lY, lCurTab: LongInt;
  lCurWorksheet: TsWorksheet;
  lCurCell: PCell;
  fmt: PsCellFormat;
begin
  lCurTab := pagesSheets.TabIndex;
  lX := Worksheets[lCurTab].Selection.Left - 1; // -1 for fixed rows/cols
  lY := Worksheets[lCurTab].Selection.Top - 1;
  lCurWorksheet := Workbook.GetWorksheetByIndex(lCurTab);
  lCurCell := lCurWorksheet.GetCell(lY, lX);
  fmt := Workbook.GetPointerToCellFormat(lCurCell^.FormatIndex);
  memoCellData.Lines.Text := '';
  memoCellData.Lines.Add(Format('Row: %d Col: %d (zero-based)', [lY, lX]));
  memoCellData.Lines.Add(Format('ContentType: %s', [GetEnumName(TypeInfo(TCellContentType), integer(lCurCell^.ContentType))]));
  memoCellData.Lines.Add(Format('NumberValue: %f', [lCurCell^.NumberValue]));
  memoCellData.Lines.Add(Format('UTF8StringValue: %s', [lCurCell^.UTF8StringValue]));
  //memoCellData.Lines.Add(Format('DateTimeValue: %s', [lCurCell^.DateTimeValue]));
  //memoCellData.Lines.Add(Format('UsedFormattingFields: %f', [lCurCell^.NumberValue]));
  memoCellData.Lines.Add(Format('TextRotation: %s', [GetEnumName(TypeInfo(TsTextRotation), integer(fmt^.TextRotation))]));
  //memoCellData.Lines.Add(Format('Border: %f', [lCurCell^.NumberValue]));
  memoCellData.Lines.Add(Format('BackgroundColor: %s', [GetColorName(fmt^.Background.BgColor)]));
  memoCellData.Lines.Add('');
  memoCellData.Lines.Add(Format('ReadAsText(): %s', [lCurWorksheet.ReadAsText(lY, lX)]));
end;

procedure Tlazfpsmainform.DeleteAllSheets;
var
  i: Integer;
begin
  for i := 0 to Length(Worksheets)-1 do
  begin
    Worksheets[i].Free;
    pagesSheets.Pages[i].Free;
  end;
  SetLength(Worksheets, 0);
end;

procedure Tlazfpsmainform.FormCreate(Sender: TObject);
begin
  Workbook := TsWorkbook.Create;
  editSourceFile.InitialDir := ExtractFilePath(ParamStr(0));
end;

procedure Tlazfpsmainform.FormDestroy(Sender: TObject);
begin
  Workbook.Free;
end;

procedure Tlazfpsmainform.HandleSelectionChanged(Sender: TObject; aCol,
  aRow: Integer);
begin
  labelCurCell.Caption := Format('Current Cell: Row=%d Col=%d', [ARow, ACol]);
end;

end.

