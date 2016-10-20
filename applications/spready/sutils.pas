unit sutils;

{$mode objfpc}{$H+}

interface

uses
  fpstypes, fpspreadsheet;

function GetCellFormatAsString(AWorkbook: TsWorkbook; AIndex: Integer): String;
function GetColorName(AColor: TsColor): String;
function GetFontAsString(AFont: TsFont): String;


implementation

{@@ ----------------------------------------------------------------------------
  Determines the name of a color from its rgb value
-------------------------------------------------------------------------------}
function GetColorName(AColor: TsColor): string;
var
  rgba: TRGBA absolute AColor;
begin
  case AColor of
    scAqua       : Result := rsAqua;
    scBeige      : Result := rsBeige;
    scBlack      : Result := rsBlack;
    scBlue       : Result := rsBlue;
    scBlueGray   : Result := rsBlueGray;
    scBrown      : Result := rsBrown;
    scCoral      : Result := rsCoral;
    scCyan       : Result := rsCyan;
    scDarkBlue   : Result := rsDarkBlue;
    scDarkGreen  : Result := rsDarkGreen;
    scDarkPurple : Result := rsDarkPurple;
    scDarkRed    : Result := rsDarkRed;
    scDarkTeal   : Result := rsDarkTeal;
    scGold       : Result := rsGold;
    scGray       : Result := rsGray;
    scGray10pct  : Result := rsGray10pct;
    scGray20pct  : Result := rsGray20pct;
    scGray40pct  : Result := rsGray40pct;
    scGray80pct  : Result := rsGray80pct;
    scGreen      : Result := rsGreen;
    scIceBlue    : Result := rsIceBlue;
    scIndigo     : Result := rsIndigo;
    scIvory      : Result := rsIvory;
    scLavander   : Result := rsLavander;
    scLightBlue  : Result := rsLightBlue;
    scLightGreen : Result := rsLightGreen;
    scLightOrange: Result := rsLightOrange;
    scLightTurquoise: Result := rsLightTurquoise;
    scLightYellow: Result := rsLightYellow;
    scLime       : Result := rsLime;
    scMagenta    : Result := rsMagenta;
    scNavy       : Result := rsNavy;
    scOceanBlue  : Result := rsOceanBlue;
    scOlive      : Result := rsOlive;
    scOliveGreen : Result := rsOliveGreen;
    scOrange     : Result := rsOrange;
    scPaleBlue   : Result := rsPaleBlue;
    scPeriwinkle : Result := rsPeriwinkle;
    scPink       : Result := rsPink;
    scPlum       : Result := rsPlum;
    scPurple     : Result := rsPurple;
    scRed        : Result := rsRed;
    scRose       : Result := rsRose;
    scSeaGreen   : Result := rsSeaGreen;
    scSilver     : Result := rsSilver;
    scSkyBlue    : Result := rsSkyBlue;
    scTan        : Result := rsTan;
    scTeal       : Result := rsTeal;
    scVeryDarkGreen: Result := rsVeryDarkGreen;
//    scViolet     : Result := rsViolet;
    scWheat      : Result := rsWheat;
    scWhite      : Result := rsWhite;
    scYellow     : Result := rsYellow;
    scTransparent: Result := rsTransparent;
    scNotDefined : Result := rsNotDefined;
    else
      case rgba.a of
        $00:
          Result := Format('R%d G%d B%d', [rgba.r, rgba.g, rgba.b]);
        scPaletteIndexMask shr 24:
          Result := Format(rsPaletteIndex, [AColor and $00FFFFFF]);
        else
          Result := '';
      end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Returns a string describing the cell format with the specified index.
-------------------------------------------------------------------------------}
function GetCellFormatAsString(AWorkbook: TsWorkbook; AIndex: Integer): String;
var
  fmt: PsCellFormat;
  cb: TsCellBorder;
  s: String;
  numFmt: TsNumFormatParams;
begin
  Result := '';
  fmt := GetPointerToCellFormat(AIndex);
  if fmt = nil then
    exit;

  if (uffFont in fmt^.UsedFormattingFields) then
    Result := Format('%s; Font%d', [Result, fmt^.FontIndex]);
  if (uffBackground in fmt^.UsedFormattingFields) then begin
    Result := Format('%s; Bg %s', [Result, GetColorName(fmt^.Background.BgColor)]);
    Result := Format('%s; Fg %s', [Result, GetColorName(fmt^.Background.FgColor)]);
    Result := Format('%s; Pattern %s', [Result, GetEnumName(TypeInfo(TsFillStyle), ord(fmt^.Background.Style))]);
  end;
  if (uffHorAlign in fmt^.UsedFormattingfields) then
    Result := Format('%s; %s', [Result, GetEnumName(TypeInfo(TsHorAlignment), ord(fmt^.HorAlignment))]);
  if (uffVertAlign in fmt^.UsedFormattingFields) then
    Result := Format('%s; %s', [Result, GetEnumName(TypeInfo(TsVertAlignment), ord(fmt^.VertAlignment))]);
  if (uffWordwrap in fmt^.UsedFormattingFields) then
    Result := Format('%s; Word-wrap', [Result]);
  if (uffNumberFormat in fmt^.UsedFormattingFields) then
  begin
    numFmt := GetNumberFormat(fmt^.NumberFormatIndex);
    if numFmt <> nil then
      Result := Format('%s; %s (%s)', [Result,
        GetEnumName(TypeInfo(TsNumberFormat), ord(numFmt.NumFormat)),
        numFmt.NumFormatStr
      ])
    else
      Result := Format('%s; %s', [Result, 'nfGeneral']);
  end else
    Result := Format('%s; %s', [Result, 'nfGeneral']);
  if (uffBorder in fmt^.UsedFormattingFields) then
  begin
    s := '';
    for cb in fmt^.Border do
      if s = '' then s := GetEnumName(TypeInfo(TsCellBorder), ord(cb))
        else s := s + '+' + GetEnumName(TypeInfo(TsCellBorder), ord(cb));
    Result := Format('%s; %s', [Result, s]);
  end;
  if (uffBiDi in fmt^.UsedFormattingFields) then
    Result := Format('%s; %s', [Result, GetEnumName(TypeInfo(TsBiDiMode), ord(fmt^.BiDiMode))]);
  if Result <> '' then Delete(Result, 1, 2);
end;

{@@ ----------------------------------------------------------------------------
  Returns a string which identifies the font.

  @param  AIndex    Index of the font
  @return String with font name, font size etc.
-------------------------------------------------------------------------------}
function GetFontAsString(AFont: TsFont): String;
begin
  if AFont <> nil then begin
    Result := Format('%s; size %.1g; %s', [
      AFont.FontName, AFont.Size, GetColorName(AFont.Color)]);
    if (fssBold in AFont.Style) then Result := Result + '; bold';
    if (fssItalic in AFont.Style) then Result := Result + '; italic';
    if (fssUnderline in AFont.Style) then Result := Result + '; underline';
    if (fssStrikeout in AFont.Style) then result := Result + '; strikeout';
    if AFont.Position = fpSubscript then Result := Result + '; subscript';
    if AFont.Position = fpSuperscript then Result := Result + '; superscript';
  end else
    Result := '';
end;

end.
