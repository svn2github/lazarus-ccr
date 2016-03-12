unit fpsPageLayout;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpsTypes;

type
  {@@ Class defining parameters for printing by the Office applications }
  TsPageLayout = class
  private
    FWorksheet: pointer;
    FOrientation: TsPageOrientation;
    FPageWidth: Double;
    FPageHeight: Double;
    FLeftMargin: Double;
    FRightMargin: Double;
    FTopMargin: Double;
    FBottomMargin: Double;
    FHeaderMargin: Double;
    FFooterMargin: Double;
    FStartPageNumber: Integer;
    FScalingFactor: Integer;
    FFitWidthToPages: Integer;
    FFitHeightToPages: Integer;
    FCopies: Integer;
    FOptions: TsPrintOptions;
    FHeaders: array[0..2] of String;
    FFooters: array[0..2] of String;
    FHeaderImages: TsHeaderFooterImages;
    FFooterImages: TsHeaderFooterImages;
    FRepeatedCols: TsRowColRange;
    FRepeatedRows: TsRowColRange;
    FPrintRanges: TsCellRangeArray;

    function GetFooterImages(ASection: TsHeaderFooterSectionIndex): TsHeaderFooterImage;
    function GetFooters(AIndex: Integer): String;
    function GetHeaderImages(ASection: TsHeaderFooterSectionIndex): TsHeaderFooterImage;
    function GetHeaders(AIndex: Integer): String;
    procedure SetFitHeightToPages(AValue: Integer);
    procedure SetFitWidthToPages(AValue: Integer);
    procedure SetFooters(AIndex: Integer; const AValue: String);
    procedure SetHeaders(AIndex: Integer; const AValue: String);
    procedure SetScalingFactor(AValue: Integer);
    procedure SetStartPageNumber(AValue: Integer);

  protected
    function JoinHeaderFooterText(const ALeft, ACenter, ARight: String): String;
    procedure SplitHeaderFooterText(const AText: String; out ALeft, ACenter, ARight: String);

  public
    constructor Create(AWorksheet: pointer);
    procedure Assign(ASource: TsPageLayout);

    { Images embedded in header and/or footer }
    procedure AddHeaderImage(AHeaderIndex: Integer;
      ASection: TsHeaderFooterSectionIndex; const AFilename: String);
    procedure AddFooterImage(AFooterIndex: Integer;
      ASection: TsHeaderFooterSectionIndex; const AFilename: String);
    procedure GetImageSections(out AHeaderTags, AFooterTags: String);
    function HasHeaderFooterImages: Boolean;

    { Repeated rows and columns }
    function HasRepeatedCols: Boolean;
    function HasRepeatedRows: Boolean;
    procedure SetRepeatedCols(AFirstCol, ALastCol: Cardinal);
    procedure SetRepeatedRows(AFirstRow, ALastRow: Cardinal);

    { print ranges }
    function AddPrintRange(ARow1, ACol1, ARow2, ACol2: Cardinal): Integer; overload;
    function AddPrintRange(const ARange: TsCellRange): Integer; overload;
    function GetPrintRange(AIndex: Integer): TsCellRange;
    function NumPrintRanges: Integer;
    procedure RemovePrintRange(AIndex: Integer);

    {@@ Page orientation, portrait or landscape. Use spoPortrait or spoLandscape}
    property Orientation: TsPageOrientation
      read FOrientation write FOrientation;
    {@@ Page width, in millimeters.
      Defined for non-rotated orientation, mostly Portrait }
    property PageWidth: Double
      read FPageWidth write FPageWidth;
    {@@ Page height, in millimeters.
      Defined for non-rotated orientation, mostly Portrait }
    property PageHeight: Double
      read FPageHeight write FPageHeight;
    {@@ Left page margin, in millimeters }
    property LeftMargin: Double
      read FLeftMargin write FLeftMargin;
    {@@ Right page margin, in millimeters }
    property RightMargin: Double
      read FRightMargin write FRightMargin;
    {@@ Top page margin, in millimeters }
    property TopMargin: Double
      read FTopMargin write FTopMargin;
    {@@ Bottom page margin, in millimeters }
    property BottomMargin: Double
      read FBottomMargin write FBottomMargin;
    {@@ Margin reserved for the header region, in millimeters }
    property HeaderMargin: Double
      read FHeaderMargin write FHeaderMargin;
    {@@ Margin reserved for the footer region, in millimeters }
    property FooterMargin: Double
      read FFooterMargin write FFooterMargin;
    {@@ Page number to be printed on the first page, default: 1 }
    property StartPageNumber: Integer
      read FStartPageNumber write SetStartPageNumber;
    {@@ Scaling factor of the page, in percent.
      PrintOptions must not contain poFitPages (is removed automatically) }
    property ScalingFactor: Integer
      read FScalingFactor write SetScalingFactor;
    {@@ Count of pages scaled such they they fon on a single page height.
      0 means: use as many pages as needed.
      Options must contain poFitPages (is set automatically) }
    property FitHeightToPages: Integer
      read FFitHeightToPages write SetFitHeightToPages;
    {@@ Count of pages scaled such that they fit on one page width.
      0 means: use as many pages as needed.
      Options must contain poFitPages (is set automatically) }
    property FitWidthToPages: Integer
      read FFitWidthToPages write SetFitWidthToPages;
    {@@ Number of copies to be printed }
    property Copies: Integer
      read FCopies write FCopies;

    {@@ A variety of options controlling the output - see TsPrintOption }
    property Options: TsPrintOptions
      read FOptions write FOptions;

    {@@ Headers and footers are in Excel syntax:
      - left/center/right sections begin with &L / &C / &R
      - page number: &P
      - page count: &N
      - current date: &D
      - current time:  &T
      - sheet name: &A
      - file name without path: &F
      - file path without file name: &Z
      - image: &G (filename must be provided by "AddHeaderImage" or "AddFooterImage")
      - bold/italic/underlining/double underlining/strike out/shadowed/
        outlined/superscript/subscript on/off:
          &B / &I / &U / &E / &S / &H
          &O / &X / &Y
      There can be three headers/footers, for first ([0]) page and
      odd ([1])/even ([2]) page numbers.
      This is activated by Options poDifferentOddEven and poDifferentFirst.
      Array index 1 contains the strings if these options are not used. }
    property Headers[AIndex: Integer]: String
      read GetHeaders write SetHeaders;
    property Footers[AIndex: Integer]: String
      read GetFooters write SetFooters;

    {@@ Count of worksheet columns repeated at the left of the printed page }
    property RepeatedCols: TsRowColRange
      read FRepeatedCols;
    {@@ Count of worksheet rows repeated at the top of the printed page }
    property RepeatedRows: TsRowColRange
      read FRepeatedRows;

    {@@ TsCellRange record of the print range with the specified index. }
    property PrintRange[AIndex: Integer]: TsCellRange
      read GetPrintRange;

    {@@ Images inserted into footer }
    property FooterImages[ASection: TsHeaderFooterSectionIndex]: TsHeaderFooterImage
      read GetFooterImages;
    {@@ Images inserted into header }
    property HeaderImages[ASection: TsHeaderFooterSectionIndex]: TsHeaderFooterImage
      read GetHeaderImages;
  end;


implementation

uses
  Math,
  fpsUtils, fpsHeaderFooterParser, fpSpreadsheet;

constructor TsPageLayout.Create(AWorksheet: Pointer);
var
  sec: TsHeaderFooterSectionIndex;
  i: Integer;
begin
  inherited Create;
  FWorksheet := AWorksheet;

  FOrientation := spoPortrait;
  FPageWidth := 210;
  FPageHeight := 297;
  FLeftMargin := InToMM(0.7);
  FRightMargin := InToMM(0.7);
  FTopMargin := InToMM(0.78740157499999996);
  FBottomMargin := InToMM(0.78740157499999996);
  FHeaderMargin := InToMM(0.3);
  FFooterMargin := InToMM(0.3);
  FStartPageNumber := 1;
  FScalingFactor := 100;   // Percent
  FFitWidthToPages := 0;   // use as many pages as needed
  FFitHeightToPages := 0;
  FCopies := 1;

  FOptions := [];

  for i:=0 to 2 do FHeaders[i] := '';
  for i:=0 to 2 do FFooters[i] := '';

  for sec in TsHeaderFooterSectionIndex do
  begin
    InitHeaderFooterImageRecord(FHeaderImages[sec]);
    InitHeaderFooterImageRecord(FFooterImages[sec]);
  end;

  FRepeatedRows.FirstIndex := UNASSIGNED_ROW_COL_INDEX;
  FRepeatedRows.LastIndex := UNASSIGNED_ROW_COL_INDEX;
  FRepeatedCols.FirstIndex := UNASSIGNED_ROW_COL_INDEX;
  FRepeatedCols.LastIndex := UNASSIGNED_ROW_COL_INDEX;
end;

{@@ Copies the data from the provided PageLayout ASource }
procedure TsPageLayout.Assign(ASource: TsPageLayout);
var
  i: Integer;
  sec: TsHeaderFooterSectionIndex;
begin
  FOrientation := ASource.Orientation;
  FPageWidth := ASource.PageWidth;
  FPageHeight := ASource.PageHeight;
  FLeftMargin := ASource.LeftMargin;
  FRightMargin := ASource.RightMargin;
  FTopMargin := ASource.TopMargin;
  FBottomMargin := ASource.BottomMargin;
  FHeaderMargin := ASource.HeaderMargin;
  FFooterMargin := ASource.FooterMargin;
  FStartPageNumber := ASource.StartPageNumber;
  FScalingFactor := ASource.ScalingFactor;
  FFitWidthToPages := ASource.FitWidthToPages;
  FFitHeightToPages := ASource.FitHeightToPages;
  FCopies := ASource.Copies;
  FOptions := ASource.Options;
  for i:=0 to 2 do
  begin
    FHeaders[i] := ASource.Headers[i];
    FFooters[i] := ASource.Footers[i];
  end;
  for sec in TsHeaderFooterSectionIndex do
  begin
    FHeaderImages[sec] := ASource.HeaderImages[sec];
    FFooterImages[sec] := ASource.FooterImages[sec];
  end;
  FRepeatedCols := ASource.RepeatedCols;
  FRepeatedRows := ASource.RepeatedRows;
  SetLength(FPrintRanges, Length(ASource.FPrintRanges));
  for i:=0 to High(FPrintRanges) do
    FPrintranges[i] := ASource.FPrintRanges[i];
end;

procedure TsPageLayout.AddHeaderImage(AHeaderIndex: Integer;
  ASection: TsHeaderFooterSectionIndex; const AFilename: String);
var
  book: TsWorkbook;
  idx: Integer;
  s: Array[TsHeaderFooterSectionIndex] of String;
begin
  if FWorksheet = nil then
    raise Exception.Create('[TsPageLayout.AddHeaderImage] Worksheet is nil.');
  book := TsWorksheet(FWorksheet).Workbook;
  idx := book.FindEmbeddedStream(AFilename);
  if idx = -1 then
  begin
    idx := book.AddEmbeddedStream(AFilename);
    book.GetEmbeddedStream(idx).LoadFromFile(AFileName);
  end;
  FHeaderImages[ASection].Index := idx;
  SplitHeaderFooterText(FHeaders[AHeaderIndex], s[hfsLeft], s[hfsCenter], s[hfsRight]);
  s[ASection] := s[ASection] + '&G';
  FHeaders[AHeaderIndex] := JoinHeaderFooterText(s[hfsLeft], s[hfsCenter], s[hfsRight]);
end;

procedure TsPageLayout.AddFooterImage(AFooterIndex: Integer;
  ASection: TsHeaderFooterSectionIndex; const AFileName: String);
var
  book: TsWorkbook;
  idx: Integer;
  s: Array[TsHeaderFooterSectionIndex] of String;
begin
  if FWorksheet = nil then
    raise Exception.Create('[TsPageLayout.AddFooterImage] Worksheet is nil.');
  book := TsWorksheet(FWorksheet).Workbook;
  idx := book.FindEmbeddedStream(AFilename);
  if idx = -1 then
  begin
    idx := book.AddEmbeddedStream(AFilename);
    book.GetEmbeddedStream(idx).LoadFromFile(AFileName);
  end;
  FFooterImages[ASection].Index := idx;
  SplitHeaderFooterText(FHeaders[AFooterIndex], s[hfsLeft], s[hfsCenter], s[hfsRight]);
  s[ASection] := s[ASection] + '&G';
  FHeaders[AFooterIndex] := JoinHeaderFooterText(s[hfsLeft], s[hfsCenter], s[hfsRight]);
end;

{@@ ----------------------------------------------------------------------------
  Adds a print range defined by the row/column indexes of its corner cells.
-------------------------------------------------------------------------------}
function TsPageLayout.AddPrintRange(ARow1, ACol1, ARow2, ACol2: Cardinal): Integer;
begin
  Result := Length(FPrintRanges);
  SetLength(FPrintRanges, Result + 1);
  with FPrintRanges[Result] do
  begin
    if ARow1 < ARow2 then
    begin
      Row1 := ARow1;
      Row2 := ARow2;
    end else
    begin
      Row1 := ARow2;
      Row2 := ARow1;
    end;
    if ACol1 < ACol2 then
    begin
      Col1 := ACol1;
      Col2 := ACol2;
    end else
    begin
      Col1 := ACol2;
      Col2 := ACol1;
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Adds a print range defined by a TsCellRange record
-------------------------------------------------------------------------------}
function TsPageLayout.AddPrintRange(const ARange: TsCellRange): Integer;
begin
  Result := AddPrintRange(ARange.Row1, ARange.Col1, ARange.Row2, ARange.Col2);
end;

function TsPageLayout.GetFooterImages(
  ASection: TsHeaderFooterSectionIndex): TsHeaderFooterImage;
begin
  Result := FFooterImages[ASection];
end;

function TsPageLayout.GetFooters(AIndex: Integer): String;
begin
  if InRange(AIndex, 0, High(FFooters)) then
    Result := FFooters[AIndex]
  else
    raise Exception.Create('[TsPageLayout.GetFooters] Illegal index.');
end;

{@@ ----------------------------------------------------------------------------
  Checks all sections of the headers and footers for images.
  Creates a 3-character-string for the header and the footer containing an "L"
  at the first position if any of the three header texts contains a "%G" in
  the left section.
  Dto. for the other sections.
  Only one image per section is allowed! Sections violating this are marked by "x"
-------------------------------------------------------------------------------}
procedure TsPageLayout.GetImageSections(out AHeaderTags, AFooterTags: String);

  procedure Process(AText: String; var ATags: String);
  var
    hfp: TsHeaderFooterParser;
  begin
    hfp := TsHeaderFooterParser.Create(AText); //, booknil, nil);
    try
      if hfp.IsImageInSection(hfsLeft) then
        ATags[1] := IfThen(ATags[1] = ' ', 'L', 'x');
      if hfp.IsImageInSection(hfsCenter) then
        ATags[2] := IfThen(ATags[2] = ' ', 'C', 'x');
      if hfp.IsImageInSection(hfsRight) then
        ATags[3] := IfThen(ATags[3] = ' ', 'R', 'x');
    finally
      hfp.Free;
    end;
  end;

var
  i: Integer;
begin
  AHeaderTags := '   ';
  for i:=0 to 2 do
    Process(FHeaders[i], AHeaderTags);

  AFooterTags := '   ';
  for i:=0 to 2 do
    Process(FFooters[i], AFooterTags);
end;

function TsPageLayout.GetHeaderImages(
  ASection: TsHeaderFooterSectionIndex): TsHeaderFooterImage;
begin
  Result := FHeaderImages[ASection];
end;

function TsPageLayout.GetHeaders(AIndex: Integer): String;
begin
  if InRange(AIndex, 0, High(FHeaders)) then
    Result := FHeaders[AIndex]
  else
    raise Exception.Create('[TsPageLayout.GetHeaders] Illegal index.');
end;

{@@ ----------------------------------------------------------------------------
  Returns the TsCellRange record of the print range with the specified index.
-------------------------------------------------------------------------------}
function TsPageLayout.GetPrintRange(AIndex: Integer): TsCellRange;
begin
  if InRange(AIndex, 0, High(FPrintRanges)) then
    Result := FPrintRanges[AIndex]
  else
    raise Exception.Create('[TsPageLayout.GetPrintRange] Illegal index.');
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the header or footer of the worksheet contains embedded images
-------------------------------------------------------------------------------}
function TsPageLayout.HasHeaderFooterImages: Boolean;
var
  sec: TsHeaderFooterSectionIndex;
begin
  Result := true;
  for sec in TsHeaderFooterSectionIndex do
  begin
    if FHeaderImages[sec].Index >= 0 then Exit;
    if FFooterImages[sec].Index >= 0 then Exit;
  end;
  Result := false;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the worksheet defines columns to be printed repeatedly at the
  left of each printed page
-------------------------------------------------------------------------------}
function TsPageLayout.HasRepeatedCols: Boolean;
begin
  Result := Cardinal(FRepeatedCols.FirstIndex) <> Cardinal(UNASSIGNED_ROW_COL_INDEX);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the worksheet defines rows to be printed repeatedly at the
  top of each printed page
-------------------------------------------------------------------------------}
function TsPageLayout.HasRepeatedRows: Boolean;
begin
  Result := Cardinal(FRepeatedRows.FirstIndex) <> Cardinal(UNASSIGNED_ROW_COL_INDEX);
end;

{@@ ----------------------------------------------------------------------------
  Combines the three strings for the left, center and right header/footer
  sections to a valid header/footer string. Inserts "&L", "&C" and "&R" codes.
-------------------------------------------------------------------------------}
function TsPageLayout.JoinHeaderFooterText(
  const ALeft, ACenter, ARight: String): String;
begin
  Result := '';
  if (ALeft = '') and (ARight = '') then
  begin
    Result := ACenter;
    exit;
  end;
  if (ALeft <> '') then Result := '&L' + ALeft;
  if (ACenter <> '') then Result := Result + '&C' + ACenter;
  if (ARight <> '') then Result := Result + '&R' + ARight;
end;

{@@ ----------------------------------------------------------------------------
  Returns the count of print ranges defined for this worksheet
-------------------------------------------------------------------------------}
function TsPageLayout.NumPrintRanges: Integer;
begin
  Result := Length(FPrintRanges);
end;

{@@ ----------------------------------------------------------------------------
  Removes the print range specified by the index
-------------------------------------------------------------------------------}
procedure TsPageLayout.RemovePrintRange(AIndex: Integer);
var
  i: Integer;
begin
  if not InRange(AIndex, 0, High(FPrintRanges)) then exit;
  for i := AIndex + 1 to High(FPrintRanges) do
    FPrintRanges[i - 1] := FPrintRanges[i];
  SetLength(FPrintRanges, Length(FPrintRanges)-1);
end;

procedure TsPageLayout.SetFitHeightToPages(AValue: Integer);
begin
  FFitHeightToPages := AValue;
  Include(FOptions, poFitPages);
end;

procedure TsPageLayout.SetFitWidthToPages(AValue: Integer);
begin
  FFitWidthToPages := AValue;
  Include(FOptions, poFitPages);
end;

procedure TsPageLayout.SetFooters(AIndex: Integer; const AValue: String);
begin
  FFooters[AIndex] := AValue;
end;

procedure TsPageLayout.SetHeaders(AIndex: Integer; const AValue: String);
begin
  FHeaders[AIndex] := AValue;
end;

procedure TsPageLayout.SetRepeatedCols(AFirstCol, ALastCol: Cardinal);
begin
  if AFirstCol < ALastCol then
  begin
    FRepeatedCols.FirstIndex := AFirstCol;
    FRepeatedCols.LastIndex := ALastCol;
  end else
  begin
    FRepeatedCols.FirstIndex := ALastCol;
    FRepeatedCols.LastIndex := AFirstCol;
  end;
end;

procedure TsPageLayout.SetRepeatedRows(AFirstRow, AlastRow: Cardinal);
begin
  if AFirstRow < ALastRow then
  begin
    FRepeatedRows.FirstIndex := AFirstRow;
    FRepeatedRows.LastIndex := ALastRow;
  end else
  begin
    FRepeatedRows.FirstIndex := ALastRow;
    FRepeatedRows.LastIndex := AFirstRow;
  end;
end;

procedure TsPageLayout.SetScalingFactor(AValue: Integer);
begin
  FScalingFactor := AValue;
  Exclude(FOptions, poFitPages);
end;

procedure TsPageLayout.SetStartPageNumber(AValue: Integer);
begin
  FStartPageNumber := AValue;
  Include(FOptions, poUseStartPageNumber);
end;

procedure TsPageLayout.SplitHeaderFooterText(const AText: String;
  out ALeft, ACenter, ARight: String);
var
  pL, pC, pR: Integer;
  P, PStart: PChar;
begin
  ALeft := '';
  ACenter := '';
  ARight := '';
  if AText = '' then
    exit;

  P := PChar(AText);
  PStart := P;
  pL := 0;
  pC := 0;
  pR := 0;
  while (P^ <> #0) do begin
    if P^ = '&' then
    begin
      inc(P);
      if (P^ = 'L') or (P^ = 'l') then
        pL := PtrUInt(P) - PtrUInt(PStart)
      else
      if (P^ = 'C') or (P^ = 'c') then
        pC := PtrUInt(P) - PtrUInt(PStart)
      else
      if (P^ = 'R') or (P^ = 'r') then
        pR := PtrUInt(P) - PtrUInt(PStart);
    end;
    inc(P);
  end;
  if (pL > 0) then
  begin
    if pC > 0 then
      ALeft := Copy(AText, pL+2, pC - pL - 1)
    else
    if pR > 0 then
      ARight := Copy(AText, pL, pR - pL - 1)
    else
      ALeft := Copy(AText, pL+2, MaxInt);
    exit;
  end;
  if (pC > 0) then
  begin
    if pR > 0 then
      ACenter := Copy(AText, pC+2, pR - pC - 1)
    else
      ACenter := Copy(AText, pC+2, MaxInt);
    exit;
  end;
  if (pR > 0) then
    ARight := Copy(AText, pR+2, MaxInt);
end;

end.