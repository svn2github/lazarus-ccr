unit fpeMakerNote;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

{$I fpexif.inc}

interface

uses
  Sysutils, Classes,
  fpeGlobal, fpeTags, fpeExifReadWrite;


type
  TCanonMakerNoteReader = class(TMakerNoteReader)
  protected
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; override;
  end;

  TCasioMakerNoteReader = class(TMakerNoteReader)
  protected
    FVersion: Integer;
    function Prepare(AStream: TStream): Boolean; override;
  end;

  TMinoltaMakerNoteReader = class(TMakerNoteReader)
  protected
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; override;
  end;

  TOlympusMakerNoteReader = class(TMinoltaMakerNoteReader)
  protected
    FVersion: Integer;
    function Prepare(AStream: TStream): Boolean; override;
  end;

procedure BuildCanonTagDefs(AList: TTagDefList);
procedure BuildCasio1TagDefs(AList: TTagDefList);
procedure BuildCasio2TagDefs(AList: TTagDefList);
procedure BuildEpsonTagDefs(AList: TTagDefList);
procedure BuildFujiTagDefs(AList: TTagDefList);
procedure BuildMinoltaTagDefs(AList: TTagDefList);
procedure BuildNikon1TagDefs(AList: TTagDefList);
procedure BuildNikon2TagDefs(AList: TTagDefList);
procedure BuildOlympusTagDefs(AList: TTagDefList);
procedure BuildSanyoTagDefs(AList: TTagDefList);


implementation

uses
  fpeStrConsts, fpeUtils, fpeExifData;


//==============================================================================
//                         TCanonMakerNoteReader
//==============================================================================

function TCanonMakerNoteReader.AddTag(AStream: TStream;
  const AIFDRecord: TIFDRecord; const AData: TBytes; AParent: TTagID): Integer;
var
  tagDef: TTagDef;
  w: array of Word;
  n,i: Integer;
begin
  Result := -1;

  tagDef := FindTagDef(AIFDRecord.TagID or AParent);
  if (tagDef = nil) then
    exit;

  Result := inherited AddTag(AStream, AIFDRecord, AData, AParent);

  // We only handle 16-bit integer types here for further processing
  if not (tagDef.TagType in [ttUInt16, ttSInt16]) then
    exit;

  // Put binary data into a word array and fix endianness
  n := Length(AData) div TagElementSize[ord(tagDef.TagType)];
  if FBigEndian then
    for i:=0 to n-1 do AData[i] := BEtoN(AData[i])
  else
    for i:=0 to n-1 do AData[i] := LEtoN(AData[i]);
  SetLength(w, n);
  Move(AData[0], w[0], Length(AData));

  // This is a special treatment of array tags which will be added as
  // separate "MakerNote" tags.
  case AIFDRecord.TagID of
    1:   // Exposure Info 1
      with FImgInfo.ExifData do begin
        AddMakerNoteTag(1, 1, 'Macro mode',          w[1],  rsCanonMacroLkup);
        AddMakerNoteTag(1, 2, 'Self-timer',          w[2]/10, '%2:.1f s');
        AddMakerNoteTag(1, 3, 'Quality',             w[3],  rsCanonQualityLkup);
        AddMakerNoteTag(1, 4, 'Flash mode',          w[4],  rsCanonFlashLkup);
        AddMakerNoteTag(1, 5, 'Drive mode',          w[5],  rsSingleContinuous);
        AddMakerNoteTag(1, 7, 'Focus mode',          w[7],  rsCanonFocusLkup);
        AddMakerNoteTag(1, 9, 'Record mode',         w[9],  rsCanonRecLkup);
        AddMakerNoteTag(1,10, 'Image size',          w[10], rsCanonSizeLkup);
        AddMakerNoteTag(1,11, 'Easy shoot',          w[11], rsCanonEasyLkup);
        AddMakerNoteTag(1,12, 'Digital zoom',        w[12], rsCanonZoomLkup);
        AddMakerNoteTag(1,13, 'Contrast',            w[13], rsCanonGenLkup);
        AddMakerNoteTag(1,14, 'Saturation',          w[14], rsCanonGenLkup);
        AddMakerNoteTag(1,15, 'Sharpness',           w[15], rsCanonGenLkup);
        AddMakerNoteTag(1,16, 'CCD ISO',             w[16], rsCanonISOLkup);
        AddMakerNoteTag(1,17, 'Metering mode',       w[17], rsCanonMeterLkup);
        AddMakerNoteTag(1,18, 'Focus type',          w[18], rsCanonFocTypeLkup);
        AddMakerNoteTag(1,19, 'AFPoint',            w[19], rsCanonAFLkup);
        AddMakerNoteTag(1,20, 'Exposure mode',       w[20], rsCanonExposeLkup);
        AddMakerNoteTag(1,24, 'Long focal',          w[24]);
        AddMakerNoteTag(1,25, 'Short focal',         w[25]);
        AddMakerNoteTag(1,26, 'Focal units',         w[26]);
        AddMakerNoteTag(1,28, 'Flash activity',      w[28], rsCanonFlashActLkup);
        AddMakerNoteTag(1,29, 'Flash details',       w[29]);
        AddMakerNoteTag(1,32, 'Focus mode',          w[32], rsSingleContinuous);
        AddMakerNoteTag(1,33, 'AESetting',          w[33], rsCanonAELkup);
        AddMakerNoteTag(1,34, 'Image stabilization', w[34], rsSingleContinuous);
      end;
    2:  // Focal length
      with FImgInfo.ExifData do begin
        AddMakerNoteTag(2, 0, 'FocalType',           w[0],  rsCanonFocalTypeLkup);
        AddMakerNoteTag(2, 1, 'FocalLength',         w[1]);
      end;
    4:  // ExposureInfo2
      with FImgInfo.ExifData do begin
        AddMakerNoteTag(4, 7, 'WhiteBalance',        w[7], rsCanonWhiteBalLkup);
        AddMakerNoteTag(4, 8, 'Slow shutter',        w[8], rsCanonSloShuttLkup);
        AddMakerNoteTag(4, 9, 'SequenceNumber',      w[9]);
        AddMakerNoteTag(4,11, 'OpticalZoomStep',     w[11]);
        AddMakerNoteTag(4,12, 'Camera temperature',  w[12]);
        AddMakerNoteTag(4,14, 'AFPoint',            w[14]);
        AddMakerNoteTag(4,15, 'FlashBias',           w[15], rsCanonBiasLkup);
        AddMakerNoteTag(4,19, 'Distance',            w[19]);
        AddMakerNoteTag(4,21, 'FNumber',             w[21]);
        AddMakerNoteTag(4,22, 'Exposure time',       w[22]);
        AddMakerNoteTag(4,23, 'Measured EV2',        w[23]);
        AddMakerNoteTag(4,24, 'Bulb duration',       w[24]);
        AddMakerNoteTag(4,26, 'Camera type',         w[26], rsCanonCamTypeLkup);
        AddMakerNoteTag(4,27, 'Auto rotation',       w[27], rsCanonAutoRotLkup);
        AddMakerNoteTag(4,28, 'NDFilter',           w[28], rsCanonGenLkup);
      end;
    5:  // Panorma
      with FImgInfo.ExifData do begin
        AddMakerNoteTag(5, 2, 'Panorama frame number', w[2]);
        AddMakerNoteTag(5, 5, 'Panorama direction',    w[5], rsCanonPanDirLkup);
      end;
  end;
end;

//==============================================================================
//                            TCasioMakerNoteReader
//==============================================================================
function TCasioMakerNoteReader.Prepare(AStream: TStream): Boolean;
var
  p: Int64;
  hdr: Array[0..5] of ansichar;
begin
  Result := false;

  p := AStream.Position;
  AStream.Read({%H-}hdr[0], SizeOf(hdr));
  if (hdr[0] = 'Q') and (hdr[1] = 'V') and (hdr[2] = 'C') and
     (hdr[3] = #0)  and (hdr[4] = #0)  and (hdr[5] = #0)
  then begin
    FVersion := 2;
    BuildCasio2TagDefs(FTagDefs);
    AStream.Position := p + SizeOf(hdr);
  end else
  begin
    FVersion := 1;
    BuildCasio1TagDefs(FTagDefs);
    AStream.Position := p;
  end;

  FBigEndian := true;
  Result := true;
end;


//==============================================================================
//                          TMinoltaMakerNoteReader
//==============================================================================
function TMinoltaMakerNoteReader.AddTag(AStream: TStream;
  const AIFDRecord: TIFDRecord; const AData: TBytes; AParent: TTagID): Integer;
var
  tagDef: TTagDef;
  v: array of DWord;
  n, i: Integer;
  t: TTagID;
  d: Integer;
  isDiMAGE7Hi: Boolean;
  //p: PByte;
begin
  Result := -1;

  tagDef := FindTagDef(AIFDRecord.TagID or AParent);
  if (tagDef = nil) then
    exit;

  Result := inherited AddTag(AStream, AIFDRecord, AData, AParent);

  // This is a special treatment of array tags which will be added as
  // separate "MakerNote" tags.
  // Ref: https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Minolta.html#CameraSettings
  t := AIFDRecord.TagID;
  case AIFDRecord.TagID of
    $0001,
    $0003:   // Minolta camera settings tags
      // Contains an array of ULong values encoded in big-endian style,
      // regardless of the byte order in the picture (i.e., even if the
      // JPEG or TIFF itself is little-endian).
      begin
        // Put binary data into a DWord array and fix endianness
        // ASSUMING HERE THAT DATA ARE ULONG HERE!
        n := Length(AData) div TagElementSize[ord(ttUInt32)];
        SetLength(v, n);
        Move(AData[0], v[0], Length(AData));
        for i:=0 to n-1 do
          v[i] := BEtoN(v[i]);
        // Fix problem with DiMAGE7Hi (http://www.dalibor.cz/software/minolta-makernote)
        isDiMAGE7Hi := FModel = 'DiMAGE7Hi';
        if isDiMAGE7Hi then d := 1 else d := 0;
        with FImgInfo.ExifData do begin
          AddMakerNoteTag(t, 1, 'Exposure mode',       v[1],  rsMinoltaExposureModeLkup, '', ttUInt32);
          AddMakerNoteTag(t, 2, 'Flash mode',          v[2],  rsMinoltaFlashModeLkup, '', ttUInt32);
          AddMakerNoteTag(t, 3, 'White balance',       v[3],  '', '', ttUInt32);
          AddMakerNoteTag(t, 4, 'Minolta image size',  v[4],  rsMinoltaImageSizeLkup1, '', ttUInt32);
          AddMakerNoteTag(t, 5, 'Minolta quality',     v[5],  rsMinoltaQualityLkup, '', ttUInt32);
          AddMakerNoteTag(t, 6, 'Drive mode',          v[6],  rsMinoltaDriveModeLkup, '', ttUInt32);
          AddMakerNoteTag(t, 7, 'Metering mode',       v[7],  rsMinoltaMeteringModeLkup, '', ttUInt32);
          AddMakerNoteTag(t, 8, 'ISO',                 v[8],  '', '', ttUInt32);
          AddMakerNoteTag(t, 9, 'Exposure time',       v[9],  '', '', ttUInt32);
          AddMakerNoteTag(t,10, 'F number',            v[10], '', '', ttUInt32);
          AddMakerNoteTag(t,11, 'Macro mode',          v[11], rsOffOn, '', ttUInt32);
          AddMakerNoteTag(t,12, 'Digital zoom',        v[12], rsMinoltaDigitalZoomLkup, '', ttUInt32);
          AddMakerNoteTag(t,13, 'Exposure compensation', v[13], '', '', ttUInt32);
          AddMakerNoteTag(t,14, 'Bracket step',        v[14], rsMinoltaBracketStepLkup, '', ttUInt32);
          AddMakerNoteTag(t,16, 'Interval length',     v[16], '', '', ttUInt32);
          AddMakerNoteTag(t,17, 'Interval number',     v[17], '', '', ttUInt32);
          AddMakerNoteTag(t,18, 'Focal length',        v[18], '', '', ttUInt32);   // crashes
          AddMakerNoteTag(t,19, 'Focus distance',      v[19], '', '', ttUInt32);
          AddMakerNoteTag(t,20, 'Flash fired',         v[20], rsNoYes, '', ttUInt32);
          AddMakerNoteTag(t,21, 'Minolta date',        v[21], '', '', ttUInt32);
          AddMakerNoteTag(t,22, 'Minolta time',        v[22], '', '', ttUInt32);
          AddMakerNoteTag(t,23, 'Max aperture',        v[23], '', '', ttUInt32);
          AddMakerNoteTag(t,26, 'File number memory',  v[26], rsOffOn, '', ttUInt32);
          AddMakerNoteTag(t,27, 'Last file number',    v[27], '', '', ttUInt32);
          AddMakerNoteTag(t,28, 'Color balance red',   v[28], '', '', ttUInt32);
          AddMakerNoteTag(t,29, 'Color balance green', v[29], '', '', ttUInt32);
          AddMakerNoteTag(t,30, 'Color balance blue',  v[30], '', '', ttUInt32);
          AddMakerNoteTag(t,31, 'Saturation',          v[31], '', '', ttUInt32);
          AddMakerNoteTag(t,32, 'Contrast',            v[32], '', '', ttUInt32);
          AddMakerNoteTag(t,33, 'Sharpness',           v[33], rsMinoltaSharpnessLkup, '', ttUInt32);
          AddMakerNoteTag(t,34, 'Subject program',     v[34], rsMinoltaSubjectProgramLkup, '', ttUInt32);
          AddMakerNoteTag(t,35, 'Flash exposure compensation', v[35], '', '', ttUInt32);
          AddMakerNoteTag(t,36, 'AE setting',          v[36], rsMinoltaIsoSettingLkup, '', ttUInt32);
          AddMakerNoteTag(t,37, 'Minolta model ID',    v[37], rsMinoltaModelIDLkup, '', ttUInt32);
          AddMakerNoteTag(t,38, 'Interval mode',       v[38], rsMinoltaIntervalModeLkup, '', ttUInt32);
          AddMakerNoteTag(t,39, 'Folder name',         v[39], rsMinoltaFolderNameLkup, '', ttUInt32);
          AddMakerNoteTag(t,40, 'Color mode',          v[40], rsMinoltaColorModeLkup, '', ttUInt32);
          AddMakerNoteTag(t,41, 'Color filter',        v[41], '', '', ttUInt32);
          AddMakerNoteTag(t,42, 'BW filter',           v[42], '', '', ttUInt32);
          AddMakerNoteTag(t,43, 'Internal flash',      v[43], rsMinoltaInternalFlashLkup, '', ttUInt32);
          AddMakerNoteTag(t,44, 'Brightness',          v[44], '', '', ttUInt32);
          AddMakerNoteTag(t,45, 'Spot focus point X',  v[45], '', '', ttUInt32);
          AddMakerNoteTag(t,46, 'Spot focus point Y',  v[46], '', '', ttUInt32);
          AddMakerNoteTag(t,47, 'Wide focus zone',     v[47], rsMinoltaWideFocusZoneLkup, '', ttUInt32);
          AddMakerNoteTag(t,48, 'Focus mode',          v[48], rsMinoltaFocusModeLkup, '', ttUInt32);
          AddMakerNoteTag(t,49, 'Focus area',          v[49], rsMinoltaFocusAreaLkup, '', ttUInt32);
          AddMakerNoteTag(t,50, 'DEC position',        v[50], rsMinoltaDECPositionLkup, '', ttUInt32);
          if isDiMAGE7Hi then
            AddMakerNoteTag(t,51, 'Color profile',     v[51], rsMinoltaColorProfileLkup, '', ttUInt32);
          AddMakerNoteTag(t,51+d, 'Data imprint',      v[52], rsMinoltaDataImprintLkup, '', ttUInt32);
          AddMakerNoteTag(t,63+d, 'Flash metering',    v[63], rsMinoltaFlashMeteringLkup, '', ttUInt32);  // or is the index 53?
        end;
      end;
    $0010:  // CameraInfoA100
      begin
        //p := @AData[0];
        //... conversion stopped due to unclear documentation on
        // https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/Minolta.html#CameraInfoA100
        // --- Is there an index 0?
      end;
  end;
end;


//==============================================================================
//                        TOlympusMakerNoteReader
//==============================================================================

{ Read the header and determine the version of the olympus makernotes:
  - version 1: header OLYMP#0#1+0, offsets relative to EXIF
  - version 2: header OLYMP#0#2#0, offsets relative to EXIF
  - version 3: header OLYMPUS#0 + BOM (II or MM) + version (#3#0)
               offsets relative to maker notes !!!! }
function TOlympusMakerNoteReader.Prepare(AStream: TStream): Boolean;
var
  p: Int64;
  hdr: packed array[0..11] of ansichar;
begin
  Result := false;

  // Remember begin of makernotes tag.
  p := AStream.Position;

  // Read header
  AStream.Read(hdr{%H-}, 12);

  // The first 5 bytes must be 'OLYMP'; this is common to all versions
  if not ((hdr[0] = 'O') and (hdr[1] = 'L') and (hdr[2] = 'Y') and (hdr[3] = 'M') and (hdr[4] = 'P')) then
    exit;

  FVersion := 0;
  // Version 1 or 2 if a #0 follows after the 'OLYMP'
  if (hdr[5] = #0) then begin
    if (hdr[6] = #1) and (hdr[7] = #0) then
      FVersion := 1
    else
    if (hdr[6] = #2) and (hdr[7] = #0) then
      FVersion := 2;
  end else
  // Version 3 if the first 8 bytes are 'OLYMPUS'#0
  if (hdr[5] = 'U') and (hdr[6] = 'S') and (hdr[7] = #0) then begin
    // Endianness marker, like in standard EXIF: 'II' or 'MM'
    if (hdr[8] = 'I') and (hdr[9] = 'I') then
      FBigEndian := false
    else
    if (hdr[8] = 'M') and (hdr[9] = 'M') then
      FBigEndian := true;
    if (hdr[10] = #3) then
      FVersion := 3;
    FStartPosition := p;  // Offsets are relative to maker notes
  end;

  // Jump to begin of IFD
  case FVersion of
    1, 2: AStream.Position := p + 8;
    3   : AStream.Position := p + 12;
    else  exit;
  end;

  BuildOlympusTagDefs(FTagDefs);
  Result := true;
end;


    (*
{ Read the header and determine the version of the olympus makernotes:
  - version 1: header OLYMP#0#1+0, offsets relative to EXIF
  - version 2: header OLYMP#0#2#0, offsets relative to EXIF
  - version 3: header OLYMPUS#0 + BOM (II or MM) + version (#3#0)
               offsets relative to maker notes !!!! }
procedure TOlympusMakerNoteReader.ReadIFD(AStream: TStream; AParent: TTagID);
//procedure TOlympusMakerNoteReader.ReadIFD(AStream: TStream; AGroup: TTagGroup);
var
  p: Int64;
  hdr: packed array[0..11] of ansichar;
begin
  if TTagIDRec(AParent).Parent = TAG_MAKERNOTE then
  begin
//  if AGroup = tgExifMakerNote then
    // Remember begin of makernotes tag.
    p := AStream.Position;

    // Read header
    AStream.Read(hdr, 12);

    // The first 5 bytes must be 'OLYMP'; this is common to all versions
    if not ((hdr[0] = 'O') and (hdr[1] = 'L') and (hdr[2] = 'Y') and (hdr[3] = 'M') and (hdr[4] = 'P')) then
      exit;

    FVersion := 0;
    // Version 1 or 2 if a #0 follows after the 'OLYMP'
    if (hdr[5] = #0) then begin
      if (hdr[6] = #1) and (hdr[7] = #0) then
        FVersion := 1
      else
      if (hdr[6] = #2) and (hdr[7] = #0) then
        FVersion := 2;
    end else
    // Version 3 if the first 8 bytes are 'OLYMPUS'#0
    if (hdr[5] = 'U') and (hdr[6] = 'S') and (hdr[7] = #0) then begin
      // Endianness marker, like in standard EXIF: 'II' or 'MM'
      if (hdr[8] = 'I') and (hdr[9] = 'I') then
        FBigEndian := false
      else
      if (hdr[8] = 'M') and (hdr[9] = 'M') then
        FBigEndian := true;
      if (hdr[10] = #3) then
        FVersion := 3;
      FStartPosition := p;  // Offsets are relative to maker notes
    end;

    // Jump to begin of IFD
    case FVersion of
      1, 2: AStream.Position := p + 8;
      3   : AStream.Position := p + 12;
      else  exit;
    end;

    BuildOlympusTagDefs(FTagDefs)
  end;

  inherited;
end;
  *)

//==============================================================================
//                        Tag definition lists
//==============================================================================

const
   M = DWord(TAGPARENT_MAKERNOTE);

procedure BuildCanonTagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0001, 'ExposureInfo1');
    AddUShortTag(M+$0002, 'Panorama');
    AddUShortTag(M+$0004, 'ExposureInfo2');
    AddStringTag(M+$0006, 'ImageType');
    AddStringTag(M+$0007, 'FirmwareVersion');
    AddULongTag (M+$0008, 'ImageNumber');
    AddStringTag(M+$0009, 'OwnerName');
    AddULongTag (M+$000C, 'CameraSerialNumber');
    AddUShortTag(M+$000F, 'CustomFunctions');
  end;
end;

{ Casio Type 1
  Standard TIFF IFD Data using Casio Type 1 Tags but always uses
  Motorola (Big-Endian) byte alignment
  This makernote has no header - the IFD starts immediately
  Ref.: http://www.ozhiker.com/electronics/pjmt/jpeg_info/casio_mn.html }
procedure BuildCasio1TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0001, 'RecordingMode',  1, '', rsCasioRecordingModeLkup);
    AddUShortTag(M+$0002, 'Quality',        1, '', rsEconomyNormalFine1);
    AddUShortTag(M+$0003, 'FocusingMode',   1, '', rsCasioFocusingModeLkup);
    AddUShortTag(M+$0004, 'FlashMode',      1, '', rsCasioFlashModeLkup);
    AddUShortTag(M+$0005, 'FlashIntensity', 1, '', rsCasioFlashIntensityLkup);
    AddULongTag (M+$0006, 'ObjectDistance', 1, '', '', '%d mm');
    AddUShortTag(M+$0007, 'WhiteBalance',   1, '', rsCasioWhiteBalanceLkup);
    AddULongTag (M+$000A, 'DigitalZoom',    1, '', rsCasioDigitalZoomLkup);
    AddUShortTag(M+$000B, 'Sharpness',      1, '', rsNormalSoftHard);
    AddUShortTag(M+$000C, 'Contrast',       1, '', rsNormalLowHigh);
    AddUShortTag(M+$000D, 'Saturation',     1, '', rsNormalLowHigh);
    AddUShortTag(M+$000A, 'DigitalZoom',    1, '', rsCasioDigitalZoomLkup);
    AddUShortTag(M+$0014, 'CCDSensitivity', 1, '', rsCasioCCDSensitivityLkup);
  end;
end;

{ Case Type 2
  Header: 6 Bytes "QVC\x00\x00\x00"
  IFD Data: Standard TIFF IFD Data using Casio Type 2 Tags but always uses
  Motorola (Big-Endian) Byte Alignment.
  All EXIF offsets are relative to the start of the TIFF header at the beginning of the EXIF segment
  Ref.: http://www.ozhiker.com/electronics/pjmt/jpeg_info/casio_mn.html
        http://www.exiv2.org/tags-casio.html
        https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Casio.html#Type2
}
procedure BuildCasio2TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag   (M+$0002, 'PreviewImageSize', 2);   // width and height, in pixels
    AddULongTag    (M+$0003, 'PreviewImageLength');
    AddULongTag    (M+$0004, 'PreviewImageStart');
    AddUShortTag   (M+$0008, 'QualityMode', 1, '', rsEconomyNormalFine);
    AddUShortTag   (M+$0009, 'ImageSize', 1, '', rsCasioImageSize2Lkup);
    AddUShortTag   (M+$000D, 'FocusMode', 1, '', rsCasioFocusMode2Lkup);
    AddUShortTag   (M+$0014, 'ISOSpeed', 1, '', rsCasioISOSpeed2Lkup);
    AddUShortTag   (M+$0019, 'WhiteBalance', 1, '', rsCasioWhiteBalance2Lkup);
    AddURationalTag(M+$001D, 'FocalLength');
    AddUShortTag   (M+$001F, 'Saturation', 1, '', rsLowNormalHigh);
    AddUShortTag   (M+$0020, 'Contrast', 1, '', rsLowNormalHigh);
    AddUShortTag   (M+$0021, 'Sharpness', 1, '', rsCasioSharpness2Lkup);
    AddBinaryTag   (M+$0E00, 'PrintIM');
    AddBinaryTag   (M+$2000, 'PreviewImage');
    AddStringTag   (M+$2001, 'FirwareDate', 18);
    AddUShortTag   (M+$2011, 'WhiteBalanceBias', 2);
    AddUShortTag   (M+$2012, 'WhiteBalance2', 2, '', rsCasioWhiteBalance22Lkup);
    AddUShortTag   (M+$2021, 'AFPointPosition', 4);
    AddULongTag    (M+$2022, 'ObjectDistance');
    AddUShortTag   (M+$2034, 'FlashDistance');
    AddByteTag     (M+$2076, 'SpecialEffectMode', 3);  // to do: array lkup - should be: '0 0 0' = Off,'1 0 0' = Makeup,'2 0 0' = Mist Removal,'3 0 0' = Vivid Landscape
    AddBinaryTag   (M+$2089, 'FaceInfo');
    AddByteTag     (M+$211C, 'FacesDetected');
    AddUShortTag   (M+$3000, 'RecordMode', 1, '', rsCasioRecordMode2Lkup);
    AddUShortTag   (M+$3001, 'ReleaseMode', 1, '', rsCasioReleaseMode2Lkup);
    AddUShortTag   (M+$3002, 'Quality', 1, '', rsEconomyNormalFine1);
    AddUShortTag   (M+$3003, 'FocusMode2', 1, '', rsCasioFocusMode2Lkup);
    AddStringTag   (M+$3006, 'HometownCity');
    AddUShortTag   (M+$3007, 'BestShotMode');  // Lkup depends severly on camera model
    AddUShortTag   (M+$3008, 'AutoISO', 1, '', rsCasioAutoIso2Lkup);
    AddUShortTag   (M+$3009, 'AFMode', 1, '', rsCasioAFMode2Lkup);
    AddBinaryTag   (M+$3011, 'Sharpness2');
    AddBinaryTag   (M+$3012, 'Contrast2');
    AddBinaryTag   (M+$3013, 'Saturation2');
    AddUShortTag   (M+$3014, 'ISO');
    AddUShortTag   (M+$3015, 'ColorMode', 1, '', rsCasioColorMode2Lkup);
    AddUShortTag   (M+$3016, 'Enhancement', 1, '', rsCasioEnhancement2Lkup);
    AddUShortTag   (M+$3017, 'ColorFilter', 1, '', rsCasioColorFilter2Lkup);
    AddUShortTag   (M+$301B, 'ArtMode', 1, '', rsCasioArtMode2Lkup);
    AddUShortTag   (M+$301C, 'SequenceNumber');
    AddUShortTag   (M+$301D, 'BracketSequence', 2);
    AddUShortTag   (M+$3020, 'ImageStabilization', 1, '', rsCasioImageStabilization2Lkup);
    AddUShortTag   (M+$302A, 'LightingMode', 1, '', rsCasioLightingMode2Lkup);
    AddUShortTag   (M+$302B, 'PortraitRefiner', 1, '', rsCasioPortraitRefiner2Lkup);
    AddUShortTag   (M+$3030, 'SpecialEffectLevel');
    AddUShortTag   (M+$3031, 'SpecialEffectSetting', 1, '', rsCasioSpecialEffectSetting2Lkup);
    AddUShortTag   (M+$3103, 'DriveMode', 1, '', rsCasioDriveMode2Lkup);
    AddBinaryTag   (M+$310B, 'ArtModeParameters', 3);
    AddUShortTag   (M+$4001, 'CaptureFrameRate');
    AddUShortTag   (M+$4003, 'VideoQuality', 1, '', rsCasioVideoQuality2Lkup);

    // to do...
  end;
end;

procedure BuildEpsonTagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0200, 'SpecialMode');
    AddUShortTag(M+$0201, 'JpegQuality');
    AddUShortTag(M+$0202, 'Macro');
    AddUShortTag(M+$0204, 'DigitalZoom');
    AddUShortTag(M+$0209, 'CameraID');
    AddStringTag(M+$020A, 'Comments');
    AddUShortTag(M+$020B, 'Width');
    AddUShortTag(M+$020C, 'Height');
    AddUShortTag(M+$020D, 'SoftRelease');
  end;
end;

procedure BuildFujiTagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddBinaryTag   (M+$0000, 'Version');
    AddStringTag   (M+$1000, 'Quality');
    AddUShortTag   (M+$1001, 'Sharpness',             1, '', rsFujiSharpnessLkup);
    AddUShortTag   (M+$1002, 'WhiteBalance',          1, '', rsFujiWhiteBalLkup);
    AddUShortTag   (M+$1003, 'Saturation',            1, '', rsFujiSaturationLkup);
    AddUShortTag   (M+$1004, 'Contrast',              1, '', rsFujiContrastLkup);
    AddUShortTag   (M+$1005, 'ColorTemperature');
    AddUShortTag   (M+$1006, 'Contrast',              1, '', rsFujiContrastLkup1);
    AddURationalTag(M+$100A, 'WhiteBalanceFineTune');
    AddUShortTag   (M+$100B, 'NoiseReduction',        1, '', rsFujiNoiseReductionLkup);
    AddUShortTag   (M+$100E, 'HighISONoiseReduction', 1, '', rsFujiHighIsoNoiseReductionLkup);
    AddUShortTag   (M+$1010, 'FlashMode',             1, '', rsFujiFlashModeLkup);
    AddURationalTag(M+$1011, 'FlashStrength');
    AddUShortTag   (M+$1020, 'Macro',                 1, '', rsOffOn);
    AddUShortTag   (M+$1021, 'FocusMode',             1, '', rsAutoManual);
    AddUShortTag   (M+$1030, 'SlowSync',              1, '', rsOffOn);
    AddUShortTag   (M+$1031, 'PictureMode',           1, '', rsFujiPictureModeLkup);
    AddUShortTag   (M+$1032, 'ExposureCount');
    AddUShortTag   (M+$1033, 'EXRAuto',               1, '', rsAutoManual);
    AddUShortTag   (M+$1034, 'EXRMode',               1, '', rsFujiEXRModeLkup);
    AddSLongTag    (M+$1040, 'ShadowTone',            1, '', rsFujiShadowHighlightLkup);
    AddSLongTag    (M+$1041, 'HighlightTone',         1, '', rsFujiShadowHighlightLkup);
    AddULongTag    (M+$1044, 'DigitalZoom');
    AddUShortTag   (M+$1050, 'ShutterType',           1, '', rsFujiShutterTypeLkup);
    AddUShortTag   (M+$1100, 'AutoBracketing',        1, '', rsFujiAutoBracketingLkup);
    AddUShortTag   (M+$1101, 'SequenceNumber');
    AddUShortTag   (M+$1153, 'PanoramaAngle');
    AddUShortTag   (M+$1154, 'PanoramaDirection',     1, '', rsFujiPanoramaDirLkup);
    AddULongTag    (M+$1201, 'AdvancedFilter',        1, '', rsFujiAdvancedFilterLkup);
    AddUShortTag   (M+$1210, 'ColorMode',             1, '', rsFujiColorModeLkup);
    AddUShortTag   (M+$1300, 'BlurWarning',           1, '', rsFujiBlurWarningLkup);
    AddUShortTag   (M+$1301, 'FocusWarning',          1, '', rsFujiFocusWarningLkup);
    AddUShortTag   (M+$1302, 'ExposureWarning',       1, '', rsFujiExposureWarningLkup);
    AddUShortTag   (M+$1400, 'DynamicRange',          1, '', rsFujiDynamicRangeLkup);
    AddURationalTag(M+$1404, 'MinFocalLength');
    AddURationalTag(M+$1405, 'MaxFocalLength');
    AddURationalTag(M+$1406, 'MaxApertureAtMinFocal');
    AddURationalTag(M+$1407, 'MaxApertureAtMaxFocal');
    AddUShortTag   (M+$140B, 'AutoDynamicRange');
    AddUShortTag   (M+$1422, 'ImageStabilization',    3);
    AddUShortTag   (M+$1425, 'SceneRecognition',      1, '', rsFujiSceneRecognLkup);
    AddUShortTag   (M+$1431, 'Rating');
    AddStringTag   (M+$8000, 'FileSource');
    AddULongTag    (M+$8002, 'OrderNumber');
    AddUShortTag   (M+$8003, 'FrameNumber');
  end;
end;

{ The Minolta MakerNote can be quite long, about 12 kB. In the beginning
  of this tag there is a normal tag directory in usual format.
  References:
  - http://www.dalibor.cz/software/minolta-makernote
  - https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Minolta.html }
procedure BuildMinoltaTagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    { This tag stores the string 'MLT0', not zero-terminated, as an identifier }
    AddBinaryTag   (M+$0000, 'Version',                 4, '', '', '', TVersionTag);

    { Stores all settings which were in effect when taking the picture.
      Details depend on camera. }
    AddBinaryTag   (M+$0001, 'MinoltaCameraSettingsOld');  // Camera D5, D7, S304, S404
    AddBinaryTag   (M+$0003, 'MinoltaCameraSettings');     // Camera D7u, D7i, D7Hi

    // this is the size of the JPEG (compressed) or TIFF or RAW file.
    AddULongTag    (M+$0040, 'CompressedImageSize');

    { Stores the thumbnail image (640×480). It is in normal JFIF format but the
      first byte should be changed to 0xFF. Beware! Sometimes the thumbnail
      is not stored in the file and this tag points beyond the end of the file. }
    AddBinaryTag   (M+$0081, 'ReviewImage');

    { The cameras D7u, D7i and D7Hi no longer store the thumbnail inside the tag.
      It has instead two tags describing the position of the thumbnail in the
      file and its size }
    AddULongTag    (M+$0088, 'PreviewImageStart');
    AddULongTag    (M+$0089, 'PreviewImageLength');

    AddULongTag    (M+$0100, 'SceneMode',               1, '', rsMinoltaSceneModeLkup);
    AddULongTag    (M+$0101, 'ColorMode',               1, '', rsMinoltaColorModeLkup);
    AddULongtag    (M+$0102, 'Quality',                 1, '', rsMinoltaQualityLkup);
    AddULongTag    (M+$0103, 'ImageSize',               1, '', rsMinoltaImageSizeLkup);
    AddSRationalTag(M+$0104, 'FlashExposureComp');
    AddULongTag    (M+$0105, 'TeleConverter',           1, '', rsMinoltaTeleconverterLkup);
    AddULongTag    (M+$0107, 'ImageStabilization',      1, '', rsMinoltaImageStabLkup);
    AddULongTag    (M+$0109, 'RawAndJpegRecording',     1, '', rsOffOn);
    AddULongTag    (M+$010A, 'ZoneMatching',            1, '', rsMinoltaZoneMatchingLkup);
    AddULongTag    (M+$010B, 'ColorTemperature',        1);
    AddULongTag    (M+$010C, 'LensType',                1);
    AddSLongTag    (M+$0111, 'ColorCompensationFilter', 1);
    AddULongTag    (M+$0112, 'WhiteBalanceFileTune',    1);
    AddULongTag    (M+$0113, 'ImageStabilization',      1, '', rsOffOn);
    AddULongTag    (M+$0115, 'WhiteBalance',            1, '', rsMinoltaWhiteBalanceLkup);
    AddBinaryTag   (M+$0E00, 'PrintPIM');
  end;
end;

// not tested
procedure BuildNikon1TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0002, 'FamilyID');
    AddUShortTag(M+$0003, 'Quality',         1, '', rsNikonQualityLkup);
    AddUShortTag(M+$0004, 'ColorMode',       1, '', rsNikonColorModeLkup);
    AddUShortTag(M+$0005, 'ImageAdjustment', 1, '', rsNikonImgAdjLkup);
    AddUShortTag(M+$0006, 'ISOSpeed',        1, '', rsNikonISOLkup);
    AddUShortTag(M+$0007, 'WhiteBalance',    1, '', rsNikonWhiteBalanceLkup);
    AddUShortTag(M+$0008, 'Focus');
    AddUShortTag(M+$000A, 'DigitalZoom');
    AddUShortTag(M+$000B, 'Converter',       1, '', rsNikonConverterLkup);
  end;
end;

{ for Nikon D1, E880, E885, E990, E995, E2500, E5000
  Ref http://www.tawbaware.com/990exif.htm
      https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Nikon.html }
procedure BuildNikon2TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddBinaryTag   (M+$0001, 'Version',      4, '', '', '', TVersionTag);
    AddUShortTag   (M+$0002, 'ISO',          2);
    AddStringTag   (M+$0003, 'ColorMode');
    AddStringTag   (M+$0004, 'Quality');
    AddStringTag   (M+$0005, 'WhiteBalance');
    AddStringtag   (M+$0006, 'ImageSharpening');
    AddStringTag   (M+$0007, 'FocusMode');
    AddStringTag   (M+$0008, 'FlashSetting');
    AddStringTag   (M+$0009, 'FlashType');
    AddURationalTag(M+$000A, 'UNKNOWN');
    AddStringTag   (M+$000F, 'ISOSelection');
    AddStringTag   (M+$0080, 'ImageAdjustment');
    AddStringTag   (M+$0081, 'ToneComp');
    AddStringTag   (M+$0082, 'AuxiliaryLens');
    AddURationalTag(M+$0085, 'ManualFocusDistance');
    AddURationalTag(M+$0086, 'DigitalZoom');
    AddBinaryTag   (M+$0088, 'AFInfo');
    AddStringTag   (M+$008D, 'ColorHue');
    AddStringTag   (M+$008F, 'SceneMode');
    AddStringTag   (M+$0090, 'LightSource');
    AddBinaryTag   (M+$0010, 'DataDump');
  end;
end;

// Most from https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Olympus.html
// some from dExif

const
   E = $2010 shl 16;  // Equipment version
   C = $2011 shl 16;  // Camera settings

procedure BuildOlympusTagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddBinaryTag   (M+$0000, 'Version', 4, '', '', '', TVersionTag);

    { Stores all settings which were in effect when taking the picture.
      Details depend on camera. }
    AddBinaryTag   (M+$0001, 'MinoltaCameraSettingsOld'); //, $FFFF, '', '', '', TSubIFDTag, true);
    AddBinaryTag   (M+$0003, 'MinoltaCameraSettings'); //, $FFFF, '', '', '', TSubIFDTag, false);

    // this is the size of the JPEG (compressed) or TIFF or RAW file.
    AddULongTag    (M+$0040, 'CompressedImageSize');

    { Stores the thumbnail image (640×480). It is in normal JFIF format but the
      first byte should be changed to 0xFF. Beware! Sometimes the thumbnail
      is not stored in the file and this tag points beyond the end of the file. }
    AddBinaryTag   (M+$0081, 'ReviewImage');

    { The cameras D7u, D7i and D7Hi no longer store the thumbnail inside the tag.
      It has instead two tags describing the position of the thumbnail in the
      file and its size }
    AddULongTag    (M+$0088, 'PreviewImageStart');
    AddULongTag    (M+$0089, 'PreviewImageLength');

    AddULongTag    (M+$0200, 'SpecialMode',      3);
    AddUShortTag   (M+$0201, 'JpegQuality',      1, '', rsOlympusJpegQualLkup);
    AddUShortTag   (M+$0202, 'Macro',            1, '', rsOlympusMacroLkup);
    AddURationalTag(M+$0204, 'DigitalZoom');
//    AddUShortTag   (M+$0207, 'Firmware');
    AddStringTag   (M+$9207, 'CameraType');
    AddStringTag   (M+$0208, 'PictureInfo');
    AddStringTag   (M+$0209, 'CameraID');
    AddUShortTag   (M+$020B, 'EpsonImageWidth');
    AddUShortTag   (M+$020C, 'EpsonImageHeight');
    AddStringTag   (M+$020D, 'EpsonSoftware');
    AddUShortTag   (M+$0403, 'SceneMode',        1, '', rsOlympusSceneModeLkup);
    AddStringTag   (M+$0404, 'SerialNumber');
    AddStringTag   (M+$0405, 'Firmware');
    AddSRationalTag(M+$1000, 'ShutterSpeedValue');
    AddSRationalTag(M+$1001, 'ISOValue');
    AddSRationalTag(M+$1002, 'ApertureValue');
    AddSRationalTag(M+$1003, 'BrightnessValue');
    AddUShortTag   (M+$1004, 'FlashMode',        1, '', rsOlympusFlashModeLkup);
    AddUShortTag   (M+$1005, 'FlashDevice',      1, '', rsOlympusFlashDevLkup);
    AddURationalTag(M+$1006, 'Bracket');
    AddSShortTag   (M+$1007, 'SensorTemperature');
    AddSShortTag   (M+$1008, 'LensTemperature');
    AddUShortTag   (M+$100B, 'FocusMode',        1, '', rsAutoManual);
    AddURationalTag(M+$100C, 'FocusDistance');
    AddUShortTag   (M+$100D, 'ZoomStepCount');
    AddUShortTag   (M+$100E, 'FocusStepCount');
    AddUShortTag   (M+$100F, 'Sharpness',        1, '', rsOlympusSharpnessLkup);
    AddUShortTag   (M+$1010, 'FlashChargeLevel');
    AddUShortTag   (M+$1011, 'ColorMatrix',      9);
    AddUShortTag   (M+$1012, 'BlackLevel',       4);
    AddUShortTag   (M+$1015, 'WhiteBalanceMode', 2);
    AddUShortTag   (M+$1017, 'RedBalance',       2);
    AddUShortTag   (M+$1018, 'BlueBalance',      2);
    AddStringTag   (M+$101A, 'SerialNumber');
    AddURationalTag(M+$1023, 'FlashBias');
    AddUShortTag   (M+$1029, 'Contrast',         1, '', rsOlympusContrastLkup);
    AddUShortTag   (M+$102A, 'SharpnessFactor');
    AddUShortTag   (M+$102B, 'ColorControl',     6);
    AddUShortTag   (M+$102C, 'ValidBits',        2);
    AddUShortTag   (M+$102D, 'CoringFilter');
    AddULongTag    (M+$102E, 'FinalWidth');
    AddULongTag    (M+$102F, 'FinalHeight');
    AddUShortTag   (M+$1030, 'SceneDetect');
    AddULongTag    (M+$1031, 'SceneArea',        8);
    AddURationalTag(M+$1034, 'CompressionRatio');
    AddUShortTag   (M+$1038, 'AFResult');
    AddUShortTag   (M+$1039, 'CCDScanMode',      1, '', rsOlympusCCDScanModeLkup);
    AddUShortTag   (M+$103A, 'NoiseReduction',   1, '', rsOffOn);
    AddUShortTag   (M+$103B, 'FocusStepInfinity');
    AddUShortTag   (M+$103C, 'FocusStepNear');
    AddSRationalTag(M+$103D, 'LightValueCenter');
    AddSRationalTag(M+$103E, 'LightValuePeriphery');
    AddIFDTag      (M+$2010, 'Equipment',        '', TSubIFDTag);
    AddIFDTag      (M+$2011, 'CameraSettings',   '', TSubIFDTag);

    // Olympus Equipment Tags
    AddBinaryTag   (E+$0000, 'EquipmentVersion', 4, '', '', '', TVersionTag);
    AddStringTag   (E+$0100, 'CameraType', 6);
    AddStringTag   (E+$0101, 'SerialNumber', 32);
    AddStringTag   (E+$0102, 'InternalSerialNumber', 32);
    AddURationalTag(E+$0103, 'FocalPlaneDiagonal');
    AddULongTag    (E+$0104, 'BodyFirmwareVersion');
    AddByteTag     (E+$0201, 'LensType', 6);
    AddStringTag   (E+$0202, 'LensSerialNumber', 32);
    AddStringTag   (E+$0203, 'LensModel');
    AddULongTag    (E+$0204, 'LensFirmwareVersion');
    AddUShortTag   (E+$0205, 'MaxApertureAtMinFocal');
    AddUShortTag   (E+$0206, 'MaxApertureAtMaxFocal');
    AddUShortTag   (E+$0207, 'MinFocalLength');
    AddUShortTag   (E+$0208, 'MaxFocalLength');
    AddUShortTag   (E+$020A, 'MaxAperture');
    AddUShortTag   (E+$020B, 'LensProperties');
    AddByteTag     (E+$0301, 'Extender', 6);
    AddStringTag   (E+$0302, 'ExtenderSerialNumber', 32);
    AddStringTag   (E+$0303, 'ExtenderModel');
    AddULongTag    (E+$0304, 'ExtenderFirmwareVersion');
    AddStringTag   (E+$0403, 'ConversionLens');
    AddUShortTag   (E+$1000, 'FlashType', 1, '', rsOlympusFlashTypeLkup);
    AddUShortTag   (E+$1001, 'FlashModel', 1, '', rsOlympusFlashModelLkup);
    AddULongTag    (E+$1002, 'FlashFirmwareVersion');
    AddStringTag   (E+$1003, 'FlashSerialNumber', 32);

    // Olympus camera settings tags
    AddBinaryTag   (C+$0000, 'CameraSettingsVersion', 4, '', '', '', TVersionTag);
    AddULongTag    (C+$0100, 'PreviewImageValid', 1, rsOlympusPreviewImgValid, rsOffOn);
    AddULongTag    (C+$0101, 'PreviewImageStart', 1, rsOlympusPreviewImgStart);
    AddULongTag    (C+$0102, 'PreviewImageLength', 1, rsOlympusPreviewImgLength);

  end;
end;

// from dExif.
procedure BuildSanyoTagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddULongTag    (M+$0200, 'SpecialMode', 3, rsSanyoSpecialMode);
    AddUShortTag   (M+$0201, 'Quality',     1, rsQuality, rsSanyoQualityLkup);
    AddUShortTag   (M+$0202, 'Macro',       1, rsMacro, rsSanyoMacroLkup);
    AddURationalTag(M+$0204, 'DigitalZoom', 1, rsDigitalZoom);
  end;
end;


initialization
  RegisterMakerNoteReader(TCanonMakerNoteReader,   'Canon',   '');
  RegisterMakerNoteReader(TCanonMakerNoteReader,   'Casio',   '');
  RegisterMakerNoteReader(TMinoltaMakerNoteReader, 'Minolta', '');
  RegisterMakerNoteReader(TOlympusMakerNoteReader, 'Olympus', '');

end.
