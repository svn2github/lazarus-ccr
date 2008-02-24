{
xlsbiff2.pas

Writes an Excel 2.x file

Excel 2.x files support only one Worksheet per Workbook, so only the first
will be written.

An Excel file consists of a number of subsequent records.
To ensure a properly formed file, the following order must be respected:

1st record:        BOF
2nd to Nth record: Any record
Last record:       EOF

Excel file format specification obtained from:

http://sc.openoffice.org/excelfileformat.pdf

AUTHORS: Felipe Monteiro de Carvalho
}
unit xlsbiff2;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils,
  fpspreadsheet;
  
type

  { TsSpreadBIFF2Writer }

  TsSpreadBIFF2Writer = class(TsCustomSpreadWriter)
  public
    { General writing methods }
    procedure WriteToStream(AStream: TStream; AData: TsWorkbook); override;
    { Record writing methods }
    procedure WriteBOF(AStream: TStream);
    procedure WriteEOF(AStream: TStream);
    procedure WriteFormula(AStream: TStream; const ARow, ACol: Word; const AFormula: TRPNFormula); override;
    procedure WriteLabel(AStream: TStream; const ARow, ACol: Word; const AValue: string); override;
    procedure WriteNumber(AStream: TStream; const ARow, ACol: Cardinal; const AValue: double); override;
  end;

implementation

const
  { Excel record IDs }
  INT_EXCEL_ID_NUMBER     = $0003;
  INT_EXCEL_ID_LABEL      = $0004;
  INT_EXCEL_ID_FORMULA    = $0006;
  INT_EXCEL_ID_BOF        = $0009;
  INT_EXCEL_ID_EOF        = $000A;

  { Cell Addresses constants }
  MASK_EXCEL_ROW          = $3FFF;
  MASK_EXCEL_RELATIVE_ROW = $4000;
  MASK_EXCEL_RELATIVE_COL = $8000;

  { BOF record constants }
  INT_EXCEL_SHEET         = $0010;
  INT_EXCEL_CHART         = $0020;
  INT_EXCEL_MACRO_SHEET   = $0040;

{
  Endianess helper functions

  Excel files are all written with Little Endian byte order,
  so it's necessary to swap the data to be able to build a
  correct file on big endian systems.
}

function WordToLE(AValue: Word): Word;
begin
  {$IFDEF BIG_ENDIAN}
    Result := ((AValue shl 8) and $FF00) or ((AValue shr 8) and $00FF);
  {$ELSE}
    Result := AValue;
  {$ENDIF}
end;

{ TsSpreadBIFF2Writer }

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteToStream ()
*
*  DESCRIPTION:    Writes an Excel 2 file to a stream
*
*                  Excel 2.x files support only one Worksheet per Workbook,
*                  so only the first will be written.
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteToStream(AStream: TStream; AData: TsWorkbook);
begin
  WriteBOF(AStream);

  WriteCellsToStream(AStream, AData.GetFirstWorksheet.FCells);

  WriteEOF(AStream);
end;

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteBOF ()
*
*  DESCRIPTION:    Writes an Excel 2 BOF record
*
*                  This must be the first record on an Excel 2 stream
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteBOF(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_BOF));
  AStream.WriteWord(WordToLE($0004));

  { Unused }
  AStream.WriteWord($0000);

  { Data type }
  AStream.WriteWord(WordToLE(INT_EXCEL_SHEET));
end;

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteEOF ()
*
*  DESCRIPTION:    Writes an Excel 2 EOF record
*
*                  This must be the last record on an Excel 2 stream
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteEOF(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_EOF));
  AStream.WriteWord($0000);
end;

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteFormula ()
*
*  DESCRIPTION:    Writes an Excel 2 FORMULA record
*
*                  To input a formula to this method, first convert it
*                  to RPN, and then list all it's members in the
*                  AFormula array
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteFormula(AStream: TStream; const ARow,
  ACol: Word; const AFormula: TRPNFormula);
var
  FormulaResult: double;
  i: Integer;
  RPNLength: Word;
  TokenArraySizePos, RecordSizePos, FinalPos: Cardinal;
begin
  RPNLength := 0;
  FormulaResult := 0.0;

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_FORMULA));
  RecordSizePos := AStream.Position;
  AStream.WriteWord(WordToLE(17 + RPNLength));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { BIFF2 Attributes }
  AStream.WriteByte($0);
  AStream.WriteByte($0);
  AStream.WriteByte($0);

  { Result of the formula in IEE 754 floating-point value }
  AStream.WriteBuffer(FormulaResult, 8);

  { 0 = Do not recalculate
    1 = Always recalculate }
  AStream.WriteByte($1);

  { Formula }

  { The size of the token array is written later,
    because it's necessary to calculate if first,
    and this is done at the same time it is written }
  TokenArraySizePos := AStream.Position;
  AStream.WriteByte(RPNLength);

  { Formula data (RPN token array) }
  for i := 0 to Length(AFormula) - 1 do
  begin
    { Token identifier }
    AStream.WriteByte(AFormula[i].TokenID);
    Inc(RPNLength);

    { Additional data }
    case AFormula[i].TokenID of

    { binary operation tokens }

    INT_EXCEL_TOKEN_TADD, INT_EXCEL_TOKEN_TSUB, INT_EXCEL_TOKEN_TMUL,
     INT_EXCEL_TOKEN_TDIV, INT_EXCEL_TOKEN_TPOWER: begin end;

    INT_EXCEL_TOKEN_TNUM:
    begin
      AStream.WriteBuffer(AFormula[i].DoubleValue, 8);
      Inc(RPNLength, 8);
    end;

    INT_EXCEL_TOKEN_TREFR, INT_EXCEL_TOKEN_TREFV, INT_EXCEL_TOKEN_TREFA:
    begin
      AStream.WriteWord(AFormula[i].Row and MASK_EXCEL_ROW);
      AStream.WriteByte(AFormula[i].Col);
      Inc(RPNLength, 3);
    end;

    end;
  end;

  { Write sizes in the end, after we known them }
  FinalPos := AStream.Position;
  AStream.position := TokenArraySizePos;
  AStream.WriteByte(RPNLength);
  AStream.Position := RecordSizePos;
  AStream.WriteWord(WordToLE(17 + RPNLength));
  AStream.position := FinalPos;
end;

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteLabel ()
*
*  DESCRIPTION:    Writes an Excel 2 LABEL record
*
*                  Writes a string to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteLabel(AStream: TStream; const ARow,
  ACol: Word; const AValue: string);
var
  L: Byte;
begin
  L := Length(AValue);

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_LABEL));
  AStream.WriteWord(WordToLE(8 + L));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { BIFF2 Attributes }
  AStream.WriteByte($0);
  AStream.WriteByte($0);
  AStream.WriteByte($0);

  { String with 8-bit size }
  AStream.WriteByte(L);
  AStream.WriteBuffer(AValue[1], L);
end;

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteNumber ()
*
*  DESCRIPTION:    Writes an Excel 2 NUMBER record
*
*                  Writes a number (64-bit IEE 754 floating point) to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteNumber(AStream: TStream; const ARow,
  ACol: Cardinal; const AValue: double);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_NUMBER));
  AStream.WriteWord(WordToLE(15));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { BIFF2 Attributes }
  AStream.WriteByte($0);
  AStream.WriteByte($0);
  AStream.WriteByte($0);

  { IEE 754 floating-point value }
  AStream.WriteBuffer(AValue, 8);
end;

{*******************************************************************
*  Initialization section
*
*  Registers this reader / writer on fpSpreadsheet
*
*******************************************************************}
initialization

  RegisterSpreadFormat(TsCustomSpreadReader, TsSpreadBIFF2Writer, sfExcel2);

end.

