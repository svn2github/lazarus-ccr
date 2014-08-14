unit beBIFFUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function BOFName(ACode: Word): String;
function CodePageName(AID: Word): String;
function PaperSizeName(ACode: Word): String;
function RecTypeName(ARecType: Word): String;
function SheetFuncName(AIndex: Word): String;
function ErrorCodeName(ACode: Byte): String;


implementation

function BOFName(ACode: Word): String;
begin
  case ACode of
    $0005: Result := 'Workbook globals';
    $0006: Result := 'Visual Basic module';
    $0010: Result := 'Sheet or dialog';
    $0020: Result := 'Chart';
    $0040: Result := 'Macro sheet';
    $0100: Result := 'Workspace';
    else   Result := '';
  end;
end;

function CodePageName(AID: Word): String;
begin
  case AID of
    $0016: Result := 'ASCII';
    $01B5: Result := 'IBM PC CP-437 (US)';
    $02D0: Result := 'IBM PC CP-720 (OEM Arabic)';
    $02E1: Result := 'IBM PC CP-737 (Greek)';
    $0307: Result := 'IBM PC CP-775 (Baltic)';
    $0352: Result := 'IBM PC CP-850 (Latin I)';
    $0354: Result := 'IBM PC CP-852 (Latin II (Central European))';
    $0357: Result := 'IBM PC CP-855 (Cyrillic)';
    $0359: Result := 'IBM PC CP-857 (Turkish)';
    $035A: Result := 'IBM PC CP-858 (Multilingual Latin I with Euro)';
    $035C: Result := 'IBM PC CP-860 (Portuguese)';
    $035D: Result := 'IBM PC CP-861 (Icelandic)';
    $035E: Result := 'IBM PC CP-862 (Hebrew)';
    $035F: Result := 'IBM PC CP-863 (Canadian (French))';
    $0360: Result := 'IBM PC CP-864 (Arabic)';
    $0361: Result := 'IBM PC CP-865 (Nordic)';
    $0362: Result := 'IBM PC CP-866 (Cyrillic (Russian))';
    $0365: Result := 'IBM PC CP-869 (Greek (Modern))';
    $036A: Result := 'Windows CP-874 (Thai)';
    $03A4: Result := 'Windows CP-932 (Japanese Shift-JIS)';
    $03A8: Result := 'Windows CP-936 (Chinese Simplified GBK)';
    $03B5: Result := 'Windows CP-949 (Korean (Wansung))';
    $03B6: Result := 'Windows CP-950 (Chinese Traditional BIG5)';
    $04B0: Result := 'UTF-16 (BIFF8)';
    $04E2: Result := 'Windows CP-1250 (Latin II) (Central European)';
    $04E3: Result := 'Windows CP-1251 (Cyrillic)';
    $04E4: Result := 'Windows CP-1252 (Latin I) (BIFF4-BIFF5)';
    $04E5: Result := 'Windows CP-1253 (Greek)';
    $04E6: Result := 'Windows CP-1254 (Turkish)';
    $04E7: Result := 'Windows CP-1255 (Hebrew)';
    $04E8: Result := 'Windows CP-1256 (Arabic)';
    $04E9: Result := 'Windows CP-1257 (Baltic)';
    $04EA: Result := 'Windows CP-1258 (Vietnamese)';
    $0551: Result := 'Windows CP-1361 (Korean (Johab))';
    $2710: Result := 'Apple Roman';
    $8000: Result := 'Apple Roman';
    $8001: Result := 'Windows CP-1252 (Latin I) (BIFF2-BIFF3)';
    else   Result := '';
  end;
end;

function ErrorCodeName(ACode: Byte): String;
begin
  case ACode of
    $00: Result := '#NULL! - Intersection of two cell ranges is empty';
    $07: Result := '#DIV/0! - Division by zero';
    $0F: Result := '#VALUE! - Wrong type of operand';
    $17: Result := '#REF! - Illegal or deleted cell reference';
    $1D: Result := '#NAME? - Wrong function or range name';
    $24: Result := '#NUM! - Value range overflow';
    $2A: Result := '#N/A - Argument or function not available';
    else Result := '(unknown)';
  end;
end;


function PaperSizeName(ACode: Word): String;
begin
  case ACode of
    0: Result := 'Undefined';
    1: Result := 'Letter (8.5" x 11")';
    2: Result := 'Letter small (8.5" x 11")';
    3: Result := 'Tabloid (11" x 17")';
    4: Result := 'Ledger (17" x 11")';
    5: Result := 'Legal (8.5" x 14")';
    6: Result := 'Statement (5.5" x 8.5")';
    7: Result := 'Executive (7.25" x 10.5")';
    8: Result := 'A3 (297mm x 420mm)';
    9: Result := 'A4 (210mm x 297mm)';
   10: Result := 'A4 small (210mm x 297mm)';
   11: Result := 'A5 (148mm x 210mm)';
   12: Result := 'B4 (JIS) (257mm x 364mm)';
   13: Result := 'B5 (JIS) (182mm x 257mm)';
   14: Result := 'Folio (8.5" x 13")';
   15: Result := 'Quarto (215mm x 275mm)';
   16: Result := '10x14 (10" x 14")';
   17: Result := '11x17 (11" x 17")';
   18: Result := 'Note (8.5" x 11")';
   19: Result := 'Envelope #9 (3 7/8" x 8 7/8")';
   20: Result := 'Envelope #10 (4 1/8" x 9 1/2")';
   21: Result := 'Envelope #11 (4 1/2" x 10 3/8")';
   22: Result := 'Envelope #12 (4 3/4" x 11")';
   23: Result := 'Envelope #14 (5" x 11.5")';
   24: Result := 'C (17" x 22")';
   25: Result := 'D (22" x 34")';
   26: Result := 'E (34" x 44")';
   27: Result := 'Envelope DL (110mm x 220mm)';
   28: Result := 'Envelope C5 (162mm x 229mm)';
   29: Result := 'Envelope C3 (324mm x 458mm)';
   30: Result := 'Envelope C4 (229mm x 324mm)';
   31: Result := 'Envelope C6 (114mm x 162mm)';
   32: Result := 'Envelope C6/C5 (114mm x 229mm)';
   33: Result := 'B4 (ISO) (250mm x 353mm)';
   34: Result := 'B5 (ISO) (176mm x 250mm)';
   35: Result := 'B6 (ISO) (125mm x 176mm)';
   36: Result := 'Envelope Italy (110mm x 230mm)';
   37: Result := 'Envelope Monarch (3 7/8" x 7 1/2")';
   38: Result := '6 3/4 Envelope (3 5/8" x 6 1/2")';
   39: Result := 'US Standard Fanfold (14 7/8" x 11")';
   40: Result := 'German Std. Fanfold (8.5" x 12")';
   41: Result := 'German Legal Fanfold (8.5" x 13")';
   42: Result := 'B4 (ISO) (250mm x 353mm)';
   43: Result := 'Japanese Postcard (100mm x 148mm)';
   44: Result := '9x11 (9" x 11")';
   45: Result := '10x11 (10" x 11")';
   46: Result := '15x11 (15" x 11")';
   47: Result := 'Envelope Invite (220mm x 220mm)';
   48: Result := 'Undefined';
   49: Result := 'Undefined';
   50: Result := 'Letter Extra (9.5" x 12")';
   51: Result := 'Legal Extra (9.5" x 15")';
   52: Result := 'Tabloid Extra (11 11/16" x 18")';
   53: Result := 'A4 Extra (235mm x 322mm)';
   54: Result := 'Letter Transverse (8.5" x 11")';
   55: Result := 'A4 Transverse (210mm x 297mm)';
   56: Result := 'Letter Extra Transv. (9.5" x 12")';
   57: Result := 'Super A/A4 (227mm x 356mm)';
   58: Result := 'Super B/A3 (305mm x 487mm)';
   59: Result := 'Letter Plus (8.5" x 12 11/16")';
   60: Result := 'A4 Plus (210mm x 330mm)';
   61: Result := 'A5 Transverse (148mm x 210mm)';
   62: Result := 'B5 (JIS) Transverse (182mm x 257mm)';
   63: Result := 'A3 Extra (322mm x 445mm)';
   64: Result := 'A5 Extra (174mm x 235mm)';
   65: Result := 'B5 (ISO) Extra (201mm x 276mm)';
   66: Result := 'A2 (420mm s 594mm)';
   67: Result := 'A3 Transverse (297mm x 420mm)';
   68: Result := 'A3 Extra Transverse (322mm x 445mm)';
   69: Result := 'Dbl. Japanese Postcard (200mm x 148mm)';
   70: Result := 'A6 (105mm x 148mm)';
   75: Result := 'Letter Rotated (11" x 8.5")';
   76: Result := 'A3 Rotated (420mm x 297mm)';
   77: Result := 'A4 Rotated (297mm x 210mm)';
   78: Result := 'A5 Rotated (210mm x 148mm)';
   79: Result := 'B4 (JIS) Rotated (364mm x 257mm)';
   80: Result := 'B5 (JIS) Rotated (257mm x 182mm)';
   81: Result := 'Japanese Postcard Rot. (148mm x 100mm)';
   82: Result := 'Dbl. Jap. Postcard Rot. (148mm x 200mm)';
   83: Result := 'A6 Rotated (148mm x 105mm)';
   88: Result := 'B6 (JIS) (128mm x 182mm)';
   89: Result := 'B6 (JIS) Rotated (182mm x 128mm)';
   90: Result := '2x11 (12" x 11")';
   else Result := '(unknown)';
  end;
end;

function RecTypeName(ARecType: Word): String;
begin
  case ARecType of
    $0000: Result := 'DIMENSION';
    $0001: Result := 'BLANK';
    $0002: Result := 'INTEGER';
    $0003: Result := 'NUMBER';
    $0004: Result := 'LABEL';
    $0005: Result := 'BoolErr';
    $0006: Result := 'FORMULA';
    $0007: Result := 'STRING';
    $0008: Result := 'ROW';
    $0009: Result := 'BOF';
    $000A: Result := 'EOF: End of file';
    $000B: Result := 'INDEX';
    $000C: Result := 'CALCCOUNT: Iteration count';
    $000D: Result := 'CALCMODE: Calculation mode';
    $000E: Result := 'PRECISION: Precision';
    $000F: Result := 'REFMODE: Reference mode';
    $0010: Result := 'DELTA: Iteration increment';
    $0011: Result := 'ITERATION: Iteration mode';
    $0012: Result := 'PROTECT: Protection flag';
    $0013: Result := 'PASSWORD: Protection password';
    $0014: Result := 'HEADER: Print header on each page';
    $0015: Result := 'FOOTER: Print footer on each page';
    $0016: Result := 'EXTERNCOUNT: Number of external references';
    $0017: Result := 'EXTERNSHEET: External reference';
    $0018: Result := 'DefinedName';
    $0019: Result := 'WINDOWPROTECT: Windows are protected';
    $001A: Result := 'VERTICALPAGEBREAKS: Explicit column page breaks';
    $001B: Result := 'HORIZONALPAGEBREAKS: Explicit row page breaks';
    $001C: Result := 'NOTE: Comment associated with a cell';
    $001D: Result := 'SELECTION: Current selection';
    $001E: Result := 'FORMAT: Number format record';
    $001F: Result := 'FORMATCOUNT: Count of number formats';
    $0020: Result := 'ColumnDefault';
    $0021: Result := 'Array';
    $0022: Result := '1904: 1904 date system';
    $0023: Result := 'ExternalName';
    $0024: Result := 'COLWIDTH';
    $0025: Result := 'DefaultRowHeight';
    $0026: Result := 'LEFTMARGIN: Left margin measurement';
    $0027: Result := 'RIGHTMARGIN: Right margin measurement';
    $0028: Result := 'TOPMARGIN: Top margin measurement';
    $0029: Result := 'BOTTOMMARGIN: Bottom margin measurement';
    $002A: Result := 'PRINTHEADERS: Print row/column labels';
    $002B: Result := 'PRINTGRIDLINES: Print gridlines flag';
    $002F: Result := 'FILEPASS: File is password-protected';
    $0031: Result := 'FONT: Font and font formatting information';
    $0032: Result := 'FONT2';
    $0033: Result := 'PRINTSIZE: Printed size of chart';
    $0036: Result := 'DataTable';
    $0037: Result := 'DateTable2';
    $003C: Result := 'CONTINUE: Continues long records';
    $003D: Result := 'WINDOW1: Window information';
    $003E: Result := 'WINDOW2';
    $0040: Result := 'BACKUP: Save backup version of the file';
    $0041: Result := 'PANE: Number of panes and their position';
    $0042: Result := 'CODEPAGE: Default code page';   // also: CODENAME: VBE object name ???
    $0043: Result := 'XF: Extended format';
    $0044: Result := 'IXFE';
    $0045: Result := 'FONTCOLOR';
    $004D: Result := 'PLS: Environment-specific print record';
    $0050: Result := 'DCON: Data consolidation information';
    $0051: Result := 'DCONREF: Data consolidation references';
    $0055: Result := 'DEFCOLWIDTH: Default width for columns';
    $0056: Result := 'BuiltInFmtCount';
    $0059: Result := 'XCT: CRN record count';
    $005A: Result := 'CRN: Non-resident operands';
    $005B: Result := 'FILESHARING: File-sharing information';
    $005C: Result := 'WRITEACCESS: Write access user name';
    $005D: Result := 'OBJ';
    $005E: Result := 'UNCALCED: Recalculation status';
    $005F: Result := 'SAVERECALC: Recalculate before saving';
    $0060: Result := 'TEMPLATE: Workbook is a template';
    $0063: Result := 'OBJPROTECT: Objects are protected';
    $007D: Result := 'COLINFO: Column formatting information';
    $007E: Result := 'RK: Cell value, RK number';
    $007F: Result := 'IMDATA: Image data';
    $0080: Result := 'GUTS: Size of row and column gutters';
    $0081: Result := 'SHEETPR: Additional workspace information';
    $0082: Result := 'GRIDSET: State change of Gridlines option';
    $0083: Result := 'HCENTER: Center between horizontal margins';
    $0084: Result := 'VCENTER: Center between vertical margins';
    $0085: Result := 'BOUNDSSHEET: Sheet information';
    $0086: Result := 'WRITEPROT: Workbook is write-protected';
    $0087: Result := 'ADDIN: Workbook is an add-in macro';
    $0088: Result := 'EDG: Edition globals';
    $0089: Result := 'PUB: Publisher';
    $008C: Result := 'COUNTRY: Default country and WIN.INI country';
    $008D: Result := 'HIDEOBJ: Object display options';
    $0090: Result := 'SORT: Sorting options';
    $0091: Result := 'SUB: Subscriber';
    $0092: Result := 'PALETTE: Color palette definition';
    $0094: Result := 'LHRECORD: .WK? file conversion information';
    $0095: Result := 'LHNGRAPH: Named graph information';
    $0096: Result := 'SOUND: Sound note';
    $0098: Result := 'LPR: Sheet was printed using LINE.PRINT()';
    $0099: Result := 'STANDARDWIDTH: Standard column width';
    $009A: Result := 'FNGROUPNAME: Function group name';
    $009B: Result := 'FILTERMODE: Sheet contains filtered list';
    $009C: Result := 'FNGROUPCOUNT: Built-in function group count';
    $009D: Result := 'AUTOFILTERINFO: Drop-down arrow count';
    $009E: Result := 'AUTOFILTER: AutoFilter data';
    $00A0: Result := 'SCL: Window zoom magnification';
    $00A1: Result := 'PAGESETUP: PageSetup';
    $00A9: Result := 'COORDLIST: Polygon object vertex coordinates';
    $00AB: Result := 'GCW: Global column width flags';
    $00AE: Result := 'SCENMAN: Scenario output data';
    $00AF: Result := 'SCENARIO: Scenario data';
    $00B0: Result := 'SXVIEW: View definition';
    $00B1: Result := 'SXVD: View fields';
    $00B2: Result := 'SXVI: View item';
    $00B4: Result := 'SXIVD: Row/column field IDs';
    $00B5: Result := 'SXLI: Line item array';
    $00B6: Result := 'SXPI: Page item';
    $00B8: Result := 'DOCROUTE: Routing slip information';
    $00B9: Result := 'RECIPNAME: Recipient name';
    $00BC: Result := 'SHRFMLA: Shared formula';
    $00BD: Result := 'MULRK: Multiple RK cells';
    $00BE: Result := 'MULBLANK: Multiple blank cells';
    $00C1: Result := 'MMS: ADDMENU/DELMENU record group count';
    $00C2: Result := 'ADDMENU: Menu addition';
    $00C3: Result := 'DELMENU: Menu deletion';
    $00C5: Result := 'SXDI: Data item';
    $00C6: Result := 'SXDB: Pivot table cache data';
    $00CD: Result := 'SXSTRING: String';
    $00D0: Result := 'SXTBL: Multiple consolidation source info';
    $00D1: Result := 'SXBRGIITM: Page item name count';
    $00D2: Result := 'SXTBPG: Page item indexex';
    $00D3: Result := 'OBPROJ: Visual Basic project';
    $00D5: Result := 'SXIDSTM: Stream ID';
    $00D6: Result := 'RSTRING: Cell with character formatting';
    $00D7: Result := 'DBCELL: Stream offsets';
    $00DA: Result := 'BOOKBOOL: Workbook option flag';
    $00DC: Result := 'PARAMQRY: Query parameters';      // also: SXEXT: External source information
    $00DD: Result := 'SCENPROTECT: Scenario Protection';
    $00DE: Result := 'OLESIZE: Size of OLE object';
    $00DF: Result := 'UDDESC: Description format for chart autoformat';
    $00E0: Result := 'XF: Extended format';
    $00E1: Result := 'INTERFACEHDR: Beginning of user interface records';
    $00E2: Result := 'INTERFACEEND: End of user interface records';
    $00E3: Result := 'SXVS: View source';
    $00E5: Result := 'MEGECELLS: Merged cells';
    $00E9: Result := 'Bitmap';
    $00EA: Result := 'TABIDCONF: Sheet Tab ID of conflict history';
    $00EB: Result := 'MSODRAWINGGROUP: Microsoft Office drawing group';
    $00EC: Result := 'MSODRAWING: Microsoft Office drawing';
    $00ED: Result := 'MSODRAWINGSELECTION: Microsoft Office drawing selection';
    $00EF: Result := 'PhoneticPR';
    $00F0: Result := 'SXRULE: PivotTable rule data';
    $00F1: Result := 'SXEX: PivotTable view extended information';
    $00F2: Result := 'SXFILT: PivotTable rule filter';
    $00F4: Result := 'SXDXF: PivotTable formatting';
    $00F5: Result := 'SXITM: PivotTable item indexes';
    $00F6: Result := 'SXNAME: PivotTable name';
    $00F7: Result := 'SXSELECT: PivotTable selection information';
    $00F8: Result := 'SXPAIR: PivotTable name pair';
    $00F9: Result := 'SXFMLA: PivotTable parsed expression';
    $00FB: Result := 'SXFORMAT: PivotTable format record';
    $00FC: Result := 'SST: Shared string table';
    $00FD: Result := 'LABELSST: Cell value, string constant/SST';
    $00FF: Result := 'EXTSST: extended shared string table';
    $013D: Result := 'TABID: Sheet tab index array';
    $015F: Result := 'LabelRanges';
    $0160: Result := 'USESELFS: Natural language formulas flag';
    $0161: Result := 'DSF: Double stream file';
    $0162: Result := 'XL5MODIFY: Flag for DSF';
    $01AE: Result := 'SUPBOOK: Supporting workbook';
    $01AF: Result := 'PROT4REV: Shared workbook protection flag';
    $01B0: Result := 'CONDFMT: Conditional formatting range information';
    $01B1: Result := 'CF: Conditional formatting conditions';
    $01B2: Result := 'DVAL: Data validation information';
    $01B5: Result := 'DCONBIN: Data consolidation information';
    $01B6: Result := 'TXO: Text object';
    $01B7: Result := 'REFRESHALL: Refresh flag';
    $01B8: Result := 'HLINK: Hyperlink';
    $01BA: Result := 'CODENAME: Name of the workbook object';
    $01BB: Result := 'SXFDBTYPE: SQL datatype identifier';
    $01BC: Result := 'PROT4REVPASS: Shared workbook protection password';
    $01BE: Result := 'DV: Data validation criteria';
    $01C0: Result := 'EXCEL9FILE: Excel 9 file';
    $01C1: Result := 'RECALCID: Recalc information';
    $0200: Result := 'DIMENSIONS: Cell table size';
    $0201: Result := 'BLANK: Cell Value, blank cell';
    $0203: Result := 'NUMBER: Cell value, floating-point cell';
    $0204: Result := 'LABEL: Cell value, string constant';
    $0205: Result := 'BOOLERR: Cell Value, boolean or error';
    $0206: Result := 'Formula';
    $0207: Result := 'STRING: String value of a formula';
    $0208: Result := 'ROW: Describes a row';
    $0209: Result := 'BOF';
    $020B: Result := 'INDEX: Index record';
    $0218: Result := 'NAME: Defined name';
    $0221: Result := 'ARRAY: Array-entered formula';
    $0223: Result := 'EXTERNNAME: Externally referenced name';
    $0225: Result := 'DEFAULTROWHEIGHT: Default row height';
    $0231: Result := 'FONT: Font description';
    $0236: Result := 'TABLE: Data table';
    $023E: Result := 'WINDOW2: Sheet window information';
    $0243: Result := 'XF: Extended format';
    $027E: Result := 'RK: Cell value, RK number';
    $0293: Result := 'STYLE: Style information';
    $0406: Result := 'FORMULA: Cell formula';
    $0409: Result := 'BOF';
    $041E: Result := 'FORMAT: Number format';
    $0443: Result := 'XF';
    $04BC: Result := 'SHAREDFMLA: Shared formula';
    $0800: Result := 'HLINKTOOLTOP: Hyperlink tooltip';
    $0801: Result := 'WEBPUB: Web publish item';
    $0802: Result := 'QSISXTAG: PivotTable and query table extensions';
    $0803: Result := 'DBQUERYEXT: Database query extensions';
    $0804: Result := 'EXTSTRING: FRT string';
    $0805: Result := 'TXTQUERY: Text query information';
    $0806: Result := 'QSIR: Query table formatting';
    $0807: Result := 'QSIF: Query table field formatting';
    $0809: Result := 'BOF: Beginning of file';
    $080A: Result := 'OLEDBCONN: OLE database connection';
    $080B: Result := 'WOPT: Web options';
    $080C: Result := 'SXVIEWEX: Pivot table OLAP extensions';
    $080D: Result := 'SXTH: Pivot table OLAP hierarchy';
    $080E: Result := 'SXPIEX: OLAP page item extensions';
    $080F: Result := 'SXVDTEX: View dimension OLAP extensions';
    $0810: Result := 'SXVIEWX9: Pivot table extensions';
    $0812: Result := 'CONTINUEFRT: Continued FRT';
    $0813: Result := 'REALTIMEDATA: Real-time data (RTD)';
    $0850: Result := 'CHARTFRTINFO: Future record identifiers';
    $0852: Result := 'STARTBLOCK: Beginning of a collection of records';
    $0853: Result := 'ENDBLOCK: End of a collection of records';
    $0862: Result := 'SHEETEXT: Extra sheet info';
    $0863: Result := 'BOOKEXT: Extra book info';
    $0864: Result := 'SXADDL: Pivot table additional info';
    $0865: Result := 'CRASHRECERR: Crash recovery error';
    $0866: Result := 'HFPICTURE: Header/footer picture';
    $0867: Result := 'FEATHEADR: Shared feature header';
    $0868: Result := 'FEAT: Shared feature record';
    $086A: Result := 'DATALABEXT: Chart data label extension';
    $086B: Result := 'DATALABEXTCONTENTS: Chart data label extension contents';
    $086C: Result := 'CELLWATCH: Cell watch';
    $086D: Result := 'FEATINFO: Shared feature info record';
    $0871: Result := 'FEATHEADR11: Shared feature header 11';
    $0872: Result := 'FEAT11: Shared feature 11 record';
    $0873: Result := 'FEATINFO11: Shared feature info 11 record';
    $0874: Result := 'DROPDOWNOBJIDS: Drop down opbject';
    $0875: Result := 'CONTINUEFRT11: Continue FRT 11';
    $0876: Result := 'DCONN: Data connection';
    $0877: Result := 'LIST12: Extra table data introduced in Excel 2007';
    $0878: Result := 'FEAT12: Shared feature 12 record';
    $0879: Result := 'CONDFMT12: Conditional formatting range information 12';
    $087A: Result := 'CF12: Conditional formatting condition 12';
    $087B: Result := 'CFEX: Conditional formatting extension';
    $087C: Result := 'XFCRC: XF extension checksum';
    $087D: Result := 'XFEXT: XF extension';
    $087E: Result := 'EZFILTER12: Autofilter data introduced in Excel 2007';
    $087F: Result := 'CONTINUEFRT12: Continue FRT 12';
    $0881: Result := 'SXADDL12: Additional workbook connections information';
    $0884: Result := 'MDTINFO: Information about a metadata type';
    $0885: Result := 'MDXSTR: MDX metadata string';
    $0886: Result := 'MDXTUPLE: Tuple MDX metadata';
    $0887: Result := 'MDXSET: Set MDX metadata';
    $0888: Result := 'MDXPROP: Member property MDX metadata';
    $0889: Result := 'MDXKPI: Key performance indicator MDX metadata';
    $088A: Result := 'MDTB: Block of metadata records';
    $088B: Result := 'PLV: Page layout view settings in Excel 2007';
    $088C: Result := 'COMPAR12: Compatibility checker 12';
    $088D: Result := 'DXF: Differential XF';
    $088E: Result := 'TABLESTYLES: Table styles';
    $088F: Result := 'TABLESTYLE: Table style';
    $0890: Result := 'TABLESTYLEELEMENT: Table style element';
    $0892: Result := 'STYLEEXT: Named cell style extension';
    $0893: Result := 'NAMEPUBLISH: Publish to Excel server data for name';
    $0894: Result := 'NAMECMT: Name comment';
    $0895: Result := 'SORTDATA12: Sort data 12';
    $0896: Result := 'THEME: Theme';
    $0897: Result := 'GUIDTYPELIB: VB project typelib GUID';
    $0898: Result := 'FNGRP12: Function group';
    $0899: Result := 'NAMEFNGRP12: Extra function group';
    $089A: Result := 'MTRSETTINGS: Multi-threaded calculation settings';
    $089B: Result := 'COMPRESSPICTURES: Automatic picture compression mode';
    $089C: Result := 'HEADERFOOTER: Header footer';
    $089E: Result := 'CRTMLFRT: Additional properties for chart elements';
    $08A3: Result := 'FORCEFULLCALCULATION: Force full calculation settings';
    $08A4: Result := 'SHAPEPROPSSTREAM: Shape formatting properties for chart elements';
    $08A5: Result := 'TEXTPROPSSTREAM: Additional text properties for text in entire chart';
    $08A7: Result := 'CRTLAYOUT12A: Layout information for a plot area';
    $08C1: Result := 'LISTOBJ: List object';
    $08C2: Result := 'LISTFIELD: List field';
    $08C3: Result := 'LISTDV: List data validation';
    $08C4: Result := 'LISTCONDFMT: List conditional formatting';
    $08C5: Result := 'LIST CF: List cell formatting';
    $08C6: Result := 'FMQRY: Filemaker queries';
    $08C7: Result := 'FMSQRY: Filemaker queries';
    $08C8: Result := 'PLV: Page layout view in Mac Excel 11';
    $08C9: Result := 'LNEXT: Extenstion information for borders in Mac Office 11';
    $08CA: Result := 'MKREXT: Extension information for markers in Mac Office 11';
    $08CB: Result := 'CRTCOOPT: Color options for chart series in Mac Office 11';
    $1001: Result := 'UNITS: ignored';
    $1002: Result := 'CHART: Position and size of chart area';
    $1003: Result := 'SERIES: Properties of the data for a series, a trendline, or error bars';
    $1004: Result := 'CHSOURCELINK: Source of data series';
    $1006: Result := 'DATAFORMAT: Formatting properties for data point or series';
    $1007: Result := 'CHLINEFORMAT: Formatting attributes of line or border';
    $1009: Result := 'MARKERFORMAT: Color, size, and shape of data markers';
    $100A: Result := 'AREAFORMAT: Patterns and colors in filled chart region';
    $100B: Result := 'PIEFORMAT: Distance of a data point(s) from pie';
    $100C: Result := 'ATTACHEDLABEL: Properties of series data label';
    $100D: Result := 'CHSTRING: Category name of series, or text for text box in chart';
    $1014: Result := 'CHARTFORMAT: Properties of a chart group';
    $1015: Result := 'LEGEND: Properties of a legend';
    $1016: Result := 'SERIESLIST: Specifies the series for the chart';
    $1017: Result := 'BAR: identifies a bar/column chart group';
    $1018: Result := 'LINE: identifies a line chart group';
    $1019: Result := 'PIE: identifies a pie/doughnut chart group';
    $101A: Result := 'AREA: identifies an area chart group';
    $101B: Result := 'SCATTER: identifies scatter or bubble chart group';
    $101D: Result := 'AXIS: Properties of an axis';
    $101E: Result := 'TICK: Attributes of axis labels and ticks';
    $101F: Result := 'VALUERANGE: Properties of value axis';
    $1021: Result := 'CHAXISLINE: Part of the axis specified by the LINEFORMAT record';
    $1022: Result := 'CRTLINK: not used';
    $1024: Result := 'DEFAULTTEXT: Text elements formatted by TEXT record';
    $1025: Result := 'TEXT: Properties of an attached label';
    $1026: Result := 'FONTX: Font for a given text element';
    $1027: Result := 'OBJECTLINK: specifies object on chart, or entire chart, to which TEXT record is linked.';
    $1032: Result := 'CHFRAME: Border and area formatting of chart';
    $1033: Result := 'CHBEGIN: Indicates begin of a chart record block';
    $1034: Result := 'CHEND: Indicates end of a chart record block';
    $1035: Result := 'PLOTAREA: empty --> see FRAME record specifying plot area properties';
    $103A: Result := 'CHART3D: plot area of the chart group is rendered in a 3-D';
    $103C: Result := 'PICF: Layout of a picture attached to a picture-filled chart elemen';
    $103D: Result := 'DROPBAR: attributes of the up/down bars between multiple series of line chart group';
    $103E: Result := 'RADAR: identifies a radar chart group';
    $103F: Result := 'SURF: identifies a surface chart group';
    $1040: Result := 'RADARAREA: identifies a filled radar chart group';
    $1041: Result := 'AXISPARENT: Properties of an axis group';
    $1043: Result := 'LEGENDEXCEPTION: Information on legend item changed from default';
    $1044: Result := 'SHTPROPS: Chart properties defined by the Chart Sheet Substream ABNF';
    $1045: Result := 'SERTOCRT: Specifies chart group for the current series';
    $1046: Result := 'AXESUSED: Number of axis groups on the chart';
    $1048: Result := 'SBASEREF: Location of a PivotTable view referenced by a chart';
    $104A: Result := 'SERPARENT: Series to which the current trendline or error bar corresponds';
    $104B: Result := 'SERAUXTREND: Specifies a trendline';
    $104E: Result := 'IFMTRECORD: Number format to use for the text on an axis';
    $104F: Result := 'POS: Size/position for legend, attached label, or plot area';
    $1050: Result := 'ALRUNS: Rich Text Formatting within chart titles, trendline, and data labels';
    $1051: Result := 'BRAI: Reference to data used in chart';
    $105B: Result := 'SERAUXERRBAR: Error bar properties';
    $105C: Result := 'CLRTCLIENT: Custom color palette for chart';
    $105D: Result := 'SERFMT: Properties of series data points, markers, or lines';
    $105F: Result := 'CHART3DBARSHAPE: Shape of the data points in bar or column chart group';
    $1060: Result := 'FBI: Scalable font information (chart)';
    $1061: Result := 'BOPPOP: Chart group is a bar or a pie of pie chart';
    $1062: Result := 'AXCEXT: Additional extension properties of a date axis';
    $1063: Result := 'DAT: Chart Sheet Substream ABNF for data table within chart area';
    $1064: Result := 'PLOTGROWTH: Scale factors for font scaling';
    $1065: Result := 'SIINDEX: Specifies data of a chart';
    $1066: Result := 'GELFRAME: Properties of a fill pattern for parts of a chart';
    $1067: Result := 'BOPPOPCUSTOM: Series data points contained in the secondary bar/pie';
    $1068: Result := 'FBI2: Scalable font information (chart)';
  else
    Result := '<unknown>';
  end;
end;

function SheetFuncName(AIndex: Word): String;
begin
  case AIndex of
    0   : result := 'COUNT';
    1   : Result := 'IF';
    2   : Result := 'ISNA';
    3   : Result := 'ISERROR';
    4   : Result := 'SUM';
    5   : Result := 'AVERAGE';
    6   : Result := 'MIN';
    7   : Result := 'MAX';
    8   : Result := 'ROW';
    9   : Result := 'COLUMN';
    10  : Result := 'NA';
    11  : Result := 'NPV';
    12  : Result := 'STDEV';
    13  : Result := 'DOLLAR';
    14  : Result := 'FIXED';
    15  : Result := 'SIN';
    16  : Result := 'COS';
    17  : Result := 'TAN';
    18  : Result := 'ATAN';
    19  : Result := 'PI';
    20  : Result := 'SQRT';
    21  : Result := 'EXP';
    22  : Result := 'LN';
    23  : Result := 'LOG10';
    24  : Result := 'ABS';
    25  : Result := 'INT';
    26  : Result := 'SIGN';
    27  : Result := 'ROUND';
    28  : Result := 'LOOKUP';
    29  : Result := 'INDEX';
    30  : Result := 'REPT';
    31  : Result := 'MID';
    32  : Result := 'LEN';
    33  : Result := 'VALUE';
    34  : Result := 'TRUE';
    35  : Result := 'FALSE';
    36  : Result := 'AND';
    37  : Result := 'OR';
    38  : Result := 'NOT';
    39  : Result := 'MOD';
    40  : Result := 'DCOUNT';
    41  : Result := 'DSUM';
    42  : Result := 'DAVERAGE';
    43  : Result := 'DMIN';
    44  : Result := 'DMAX';
    45  : Result := 'DSTDEV';
    46  : Result := 'VAR';
    47  : Result := 'DVAR';
    48  : Result := 'TEXT';
    49  : Result := 'LINEST';
    50  : Result := 'TREND';
    51  : Result := 'LOGEST';
    52  : Result := 'GROWTH';
    56  : Result := 'PV';
    57  : Result := 'FV';
    58  : Result := 'NPER';
    59  : Result := 'PMT';
    60  : Result := 'RATE';
    61  : Result := 'MIRR';
    62  : Result := 'IRR';
    63  : Result := 'RAND';
    64  : Result := 'MATCH';
    65  : Result := 'DATE';
    66  : Result := 'TIME';
    67  : Result := 'DAY';
    68  : Result := 'MONTH';
    69  : Result := 'YEAR';
    70  : Result := 'WEEKDAY';
    71  : Result := 'HOUR';
    72  : Result := 'MINUTE';
    73  : Result := 'SECOND';
    74  : Result := 'NOW';
    75  : Result := 'AREAS';
    76  : Result := 'ROWS';
    77  : Result := 'COLUMNS';
    78  : Result := 'OFFSET';
    82  : Result := 'SEARCH';
    83  : Result := 'TRANSPOSE';
    86  : Result := 'TYPE';
    97  : Result := 'ATAN2';
    98  : Result := 'ASIN';
    99  : Result := 'ACOS';
    100 : Result := 'CHOOSE';
    101 : Result := 'HLOOKUP';
    102 : Result := 'VLOOKUP';
    105 : Result := 'ISREF';
    109 : Result := 'LOG';
    111 : Result := 'CHAR';
    112 : Result := 'LOWER';
    113 : Result := 'UPPER';
    114 : Result := 'PROPER';
    115 : Result := 'LEFT';
    116 : Result := 'RIGHT';
    117 : Result := 'EXACT';
    118 : Result := 'TRIM';
    119 : Result := 'REPLACE';
    120 : Result := 'SUBSTITUTE';
    121 : Result := 'CODE';
    124 : Result := 'FIND';
    125 : Result := 'CELL';
    126 : Result := 'ISERR';
    127 : Result := 'ISTEXT';
    128 : Result := 'ISNUMBER';
    129 : Result := 'ISBLANK';
    130 : Result := 'T';
    131 : Result := 'N';
    140 : Result := 'DATEVALUE';
    141 : Result := 'TIMEVALUE';
    142 : Result := 'SLN';
    143 : Result := 'SYD';
    144 : Result := 'DDB';
    148 : Result := 'INDIRECT';
    162 : Result := 'CLEAN';
    163 : Result := 'MDETERM';
    164 : Result := 'MINVERSE';
    165 : Result := 'MMULT';
    167 : Result := 'IPMT';
    168 : Result := 'PPMT';
    169 : Result := 'COUNTA';
    183 : Result := 'PRODUCT';
    184 : Result := 'FACT';
    189 : Result := 'DPRODUCT';
    190 : Result := 'ISNONTEXT';
    193 : Result := 'STDEVP';
    194 : Result := 'VARP';
    195 : Result := 'DSTDEVP';
    196 : Result := 'DVARP';
    197 : Result := 'TRUNC';
    198 : Result := 'ISLOGICAL';
    199 : Result := 'DCOUNTA';
    204 : Result := 'YEN/USDOLLAR';
    205 : Result := 'FINDB';
    206 : Result := 'SEARCHB';
    207 : Result := 'REPLACEB';
    208 : Result := 'LEFTB';
    209 : Result := 'RIGHTB';
    210 : Result := 'MIDB';
    211 : Result := 'LENB';
    212 : Result := 'ROUNDUP';
    213 : Result := 'ROUNDDOWN';
    214 : Result := 'ASC';
    215 : Result := 'JIS / DBCS';
    216 : Result := 'RANK';
    219 : Result := 'ADDRESS';
    220 : Result := 'DAYS360';
    221 : Result := 'TODAY';
    222 : Result := 'VDB';
    227 : Result := 'MEDIAN';
    228 : Result := 'SUMPRODUCT';
    229 : Result := 'SINH';
    230 : Result := 'COSH';
    231 : Result := 'TANH';
    232 : Result := 'ASINH';
    233 : Result := 'ACOSH';
    234 : Result := 'ATANH';
    235 : Result := 'DGET';
    244 : Result := 'INFO';
    247 : Result := 'DB';
    252 : Result := 'FREQUENCY';
    261 : Result := 'ERROR.TYPE';
    269 : Result := 'AVEDEV';
    270 : Result := 'BETADIST';
    271 : Result := 'GAMMALN';
    272 : Result := 'BETAINV';
    273 : Result := 'BINOMDIST';
    274 : Result := 'CHIDIST';
    275 : Result := 'CHIINV';
    276 : Result := 'COMBIN';
    277 : Result := 'CONFIDENCE';
    278 : Result := 'CRITBINOM';
    279 : Result := 'EVEN';
    280 : Result := 'EXPONDIST';
    281 : Result := 'FDIST';
    282 : Result := 'FINV';
    283 : Result := 'FISHER';
    284 : Result := 'FISHERINV';
    285 : Result := 'FLOOR';
    286 : Result := 'GAMMADIST';
    287 : Result := 'GAMMAINV';
    288 : Result := 'CEILING';
    289 : Result := 'HYPGEOMDIST';
    290 : Result := 'LOGNORMDIST';
    291 : Result := 'LOGINV';
    292 : Result := 'NEGBINOMDIST';
    293 : Result := 'NORMDIST';
    294 : Result := 'NORMSDIST';
    295 : Result := 'NORMINV';
    296 : Result := 'NORMSINV';
    297 : Result := 'STANDARDIZE';
    298 : Result := 'ODD';
    299 : Result := 'PERMUT';
    300 : Result := 'POISSON';
    301 : Result := 'TDIST';
    302 : Result := 'WEIBULL';
    303 : Result := 'SUMXMY2';
    304 : Result := 'SUMX2MY2';
    305 : Result := 'SUMX2PY2';
    306 : Result := 'CHITEST';
    307 : Result := 'CORREL';
    308 : Result := 'COVAR';
    309 : Result := 'FORECAST';
    310 : Result := 'FTEST';
    311 : Result := 'INTERCEPT';
    312 : Result := 'PEARSON';
    313 : Result := 'RSQ';
    314 : Result := 'STEYX';
    315 : Result := 'SLOPE';
    316 : Result := 'TTEST';
    317 : Result := 'PROB';
    318 : Result := 'DEVSQ';
    319 : Result := 'GEOMEAN';
    320 : Result := 'HARMEAN';
    321 : Result := 'SUMSQ';
    322 : Result := 'KURT';
    323 : Result := 'SKEW';
    324 : Result := 'ZTEST';
    325 : Result := 'LARGE';
    326 : Result := 'SMALL';
    327 : Result := 'QUARTILE';
    328 : Result := 'PERCENTILE';
    329 : Result := 'PERCENTRANK';
    330 : Result := 'MODE';
    331 : Result := 'TRIMMEAN';
    332 : Result := 'TINV';
    336 : Result := 'CONCATENATE';
    337 : Result := 'POWER';
    342 : Result := 'RADIANS';
    343 : Result := 'DEGREES';
    344 : Result := 'SUBTOTAL';
    345 : Result := 'SUMIF';
    346 : Result := 'COUNTIF';
    347 : Result := 'COUNTBLANK';
    350 : Result := 'ISPMT';
    351 : Result := 'DATEDIF';
    352 : Result := 'DATESTRING';
    353 : Result := 'NUMBERSTRING';
    354 : Result := 'ROMAN';
    358 : Result := 'GETPIVOTDATA';
    359 : Result := 'HYPERLINK';
    360 : Result := 'PHONETIC';
    361 : Result := 'AVERAGEA';
    362 : Result := 'MAXA';
    363 : Result := 'MINA';
    364 : Result := 'STDEVPA';
    365 : Result := 'VARPA';
    366 : Result := 'STDEVA';
    367 : Result := 'VARA';
    else  Result := 'unknown';
  end;
end;

end.

