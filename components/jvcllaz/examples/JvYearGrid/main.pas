unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls, Grids,
  ExtCtrls, JvYearGrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    CbDayNamesAlignment: TComboBox;
    CbMonthNamesAlignment: TComboBox;
    CbDaysAlignment: TComboBox;
    CbFlat: TCheckBox;
    JvYearGrid1: TJvYearGrid;
    EdLeftMargin: TSpinEdit;
    EdRightMargin: TSpinEdit;
    EdTopMargin: TSpinEdit;
    EdBottomMargin: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    procedure CbDayNamesAlignmentChange(Sender: TObject);
    procedure CbDaysAlignmentChange(Sender: TObject);
    procedure CbFlatChange(Sender: TObject);
    procedure CbMonthNamesAlignmentChange(Sender: TObject);
    procedure EdBottomMarginChange(Sender: TObject);
    procedure EdLeftMarginChange(Sender: TObject);
    procedure EdRightMarginChange(Sender: TObject);
    procedure EdTopMarginChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CbDayNamesAlignmentChange(Sender: TObject);
begin
  JvYearGrid1.DayNamesAlignment := TAlignment(CbDayNamesAlignment.ItemIndex);
end;

procedure TForm1.CbDaysAlignmentChange(Sender: TObject);
begin
  JvYearGrid1.DaysAlignment := TAlignment(CbDaysAlignment.ItemIndex);
end;

procedure TForm1.CbFlatChange(Sender: TObject);
begin
  JvYearGrid1.Flat := CbFlat.Checked;
end;

procedure TForm1.CbMonthNamesAlignmentChange(Sender: TObject);
begin
  JvYearGrid1.MonthNamesAlignment := TAlignment(CbMonthNamesAlignment.ItemIndex);
end;

procedure TForm1.EdBottomMarginChange(Sender: TObject);
begin
  JvYearGrid1.CellMargins.Bottom := EdBottomMargin.Value;
end;

procedure TForm1.EdLeftMarginChange(Sender: TObject);
begin
  JvYearGrid1.CellMargins.Left := EdLeftMargin.Value;
end;

procedure TForm1.EdRightMarginChange(Sender: TObject);
begin
  JvYearGrid1.CellMargins.Right := EdRightMargin.Value;
end;

procedure TForm1.EdTopMarginChange(Sender: TObject);
begin
  JvYearGrid1.CellMargins.Top := EdTopMargin.Value;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdLeftMargin.Value := JvYearGrid1.CellMargins.Left;
  EdRightMargin.Value := JvYearGrid1.CellMargins.Right;
  EdTopMargin.Value := JvYearGrid1.CellMargins.Top;
  EdBottomMargin.Value := JvYearGrid1.CellMargins.Bottom;

  CbDayNamesAlignment.ItemIndex := ord(JvYearGrid1.DayNamesAlignment);
  CbMonthNamesAlignment.ItemIndex := ord(JvYearGrid1.MonthNamesAlignment);
  CbDaysAlignment.ItemIndex := ord(JvYearGrid1.DaysAlignment);

  CbFlat.Checked := JvYearGrid1.Flat;
end;

initialization
  {$IFDEF WINDOWS}
  GetLocaleFormatSettings(1033, FormatSettings);
  {$ENDIF}

end.

