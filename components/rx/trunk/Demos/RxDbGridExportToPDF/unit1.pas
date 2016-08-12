unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxmemds, rxdbgrid,
  RxDBGridExportPdf, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    DataSource1: TDataSource;
    Edit1: TEdit;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RxDBGrid1: TRxDBGrid;
    RxDBGridExportPDF1: TRxDBGridExportPDF;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1Country: TStringField;
    RxMemoryData1ID: TAutoIncField;
    RxMemoryData1NAME: TStringField;
    RxMemoryData1PDATE: TDateField;
    RxMemoryData1Sity: TStringField;
    RxMemoryData1SUM: TCurrencyField;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FontDirList: TStrings;
    procedure InitFonts;
    procedure ShowInfo(AText:string; AParams : array of const);
    procedure DebugFonts;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses EasyLazFreeType, LazFreeTypeFontCollection,
  LazFileUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  RxMemoryData1.Open;
  RxMemoryData1.AppendRecord([1, 'Строка с длинным текстом 1', now, 100, 'Россия', 'Москва']);
  RxMemoryData1.AppendRecord([2, 'Строка с длинным текстом 2', now - 1, 100, 'Россия', 'Ставрополь']);
  RxMemoryData1.AppendRecord([3, 'Строка с длинным текстом 3', now - 2, 110, 'Россия', 'Калининград']);
  RxMemoryData1.AppendRecord([4, 'Строка с длинным текстом 4', now - 3, 5000, 'Россия', 'Владивасток']);
  RxMemoryData1.AppendRecord([5, 'Строка с длинным текстом 5', now - 4, 123.31, 'USA', 'New-York']);
  RxMemoryData1.AppendRecord([6, 'Строка с длинным текстом 6', now, 100, 'Россия', 'Москва']);
  RxMemoryData1.AppendRecord([7, 'Строка с длинным текстом 7', now - 1, 100, 'Россия', 'Ставрополь']);
  RxMemoryData1.AppendRecord([8, 'Строка с длинным текстом 8', now - 2, 110, 'Россия', 'Калининград']);
  RxMemoryData1.AppendRecord([9, 'Строка с длинным текстом 9', now - 3, 5000, 'Россия', 'Владивасток']);
  RxMemoryData1.AppendRecord([10,'Строка с длинным текстом 10', now - 4, 123.31, 'USA', 'New-York']);
  RxMemoryData1.AppendRecord([11,'Строка с длинным текстом 11', now, 100, 'Россия', 'Москва']);
  RxMemoryData1.AppendRecord([12,'Строка с длинным текстом 12', now - 1, 100, 'Россия', 'Ставрополь']);
  RxMemoryData1.AppendRecord([13,'Строка с длинным текстом 13', now - 2, 110, 'Россия', 'Калининград']);
  RxMemoryData1.AppendRecord([14,'Строка с длинным текстом 14', now - 3, 5000, 'Россия', 'Владивасток']);
  RxMemoryData1.AppendRecord([15,'Строка с длинным текстом 15', now - 4, 123.31, 'USA', 'New-York']);
  RxMemoryData1.AppendRecord([16,'Строка с длинным текстом 16', now, 100, 'Россия', 'Москва']);
  RxMemoryData1.AppendRecord([17,'Строка с длинным текстом 17', now - 1, 100, 'Россия', 'Ставрополь']);
  RxMemoryData1.AppendRecord([18,'Строка с длинным текстом 18', now - 2, 110, 'Россия', 'Калининград']);
  RxMemoryData1.AppendRecord([19,'Строка с длинным текстом 19', now - 3, 5000, 'Россия', 'Владивасток']);
  RxMemoryData1.AppendRecord([20,'Строка с длинным текстом 20', now - 4, 123.31, 'USA', 'New-York']);

  DebugFonts;
end;

procedure TForm1.InitFonts;
procedure CreateFontDirList;
var
  s: String;
begin
  FontDirList := TStringList.Create;
 {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(20); // CSIDL_FONTS = 20
  if s <> '' then
    FontDirList.Add(s);
 {$ENDIF}
 {$IFDEF linux}
  FontDirList.Add('/usr/share/cups/fonts/');
  FontDirList.Add('/usr/share/fonts/');
  FontDirList.Add('/usr/local/lib/X11/fonts/');
  FontDirList.Add(GetUserDir + '.fonts/');
 {$ENDIF}
end;


  { Duplicates functionality in FontCollection.AddFolder in order to be able to
    ignore exceptions due to font read errors (occur on Linux Mint with font
    NanumMyeongjo.ttf }
  procedure AddFolder(AFolder: string);
  var
    files: TStringList;
    i: integer;
  begin
    AFolder := ExpandFileName(AFolder);
    if (length(AFolder) <> 0) and (AFolder[length(AFolder)] <> PathDelim) then
      AFolder += PathDelim;
    files := TStringList.Create;
    FontCollection.BeginUpdate;
    try
      FindAllFiles(files, AFolder, '*.ttf', true);
      files.Sort;
      for i := 0 to files.Count-1 do
        try
          FontCollection.AddFile(files[i]);
        except
        end;
    finally
      FontCollection.EndUpdate;
      files.Free;
    end;
  end;

var
  i: Integer;
begin
  if FontDirList = nil then
    CreateFontDirList;

  for i:=0 to FontDirList.Count-1 do
    AddFolder(FontDirList[i]);
end;

procedure TForm1.ShowInfo(AText: string; AParams: array of const);
begin
  Memo1.Lines.Add(Format(AText, AParams));
end;

procedure TForm1.DebugFonts;

procedure DumpFamaly(AFontFamely:string);
var
  FFM: TCustomFamilyCollectionItem;
  I: Integer;
  FFI: TCustomFontCollectionItem;
begin
  FFM:=FontCollection.Family[AFontFamely];
  if not Assigned(FFM) then
  begin
    ShowInfo('Font Family %s NOT FOUND!', [AFontFamely]);
    exit;
  end;


  ShowInfo('In Family %s count fonts : %d', [AFontFamely, FFM.FontCount]);
  for I:=0 to FFM.FontCount-1 do
  begin
    FFI:=FFM.Font[i];
    ShowInfo('Font in file %s - NAME: %s. Styles = %s', [FFI.Filename, FFI.Information[ftiFullName], FFI.Styles]);
  end;

  FFI:=FFM.GetFont('Regular');
  if Assigned(FFI) then
    ShowInfo('REGULAR Font in file %s - NAME: %s', [FFI.Filename, FFI.Information[ftiFullName]])
  else
    ShowInfo('Regular font not found', []);
end;

begin
  InitFonts;
  Memo1.Lines.Clear;
  if Assigned(FontCollection) then
  begin
    ShowInfo('FontCollection.FontFileCount = %d', [FontCollection.FontFileCount]);
    ShowInfo('FontCollection.FamilyCount = %d', [FontCollection.FamilyCount]);
    DumpFamaly('Arial');
    DumpFamaly('Sans');
    DumpFamaly('Serif');
    DumpFamaly('Liberation Sans');
  end
  else
    Memo1.Text:='FontCollection not assigned';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RxDBGridExportPDF1.Execute;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    RxMemoryData1.Filter:=Edit1.Text;
  end;
  RxMemoryData1.Filtered:=CheckBox1.Checked;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FontDirList);
end;

end.

{
'Conakry'
'DejaVu Sans'
'DejaVu Sans Condensed'
'DejaVu Sans Light'
'DejaVu Sans Mono'
'DejaVu Serif'
'DejaVu Serif Condensed'

'Denemo'

'FreeSans'
'Caladea'
'Carlito'
}
