unit fpeMetadata;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

{$I fpexif.inc}

interface

uses
  Classes, SysUtils,
 {$IFDEF FPC}
  LazUTF8,
 {$ENDIF}
  fpeGlobal,
  fpeExifData, fpeIptcData;

type
  TImgInfo = class;

  { TBasicMetadataReaderWriter }
  TBasicMetadataReaderWriter = class
  protected
    FImgInfo: TImgInfo;
    FImgFormat: TImgFormat;
    procedure Warning(const AMsg: String);
  public
    constructor Create(AImgInfo: TImgInfo); virtual;
  end;

  { TBasicMetadataReader }
  TBasicMetadataReader = class(TBasicMetadataReaderWriter)
  protected
    procedure Error(const AMsg: String); virtual;
  public
    procedure ReadFromStream(AStream: TStream; AImgFormat: TImgFormat); virtual;
  end;

  { TBasicMetadataWriter }
  TBasicMetadataWriter = class(TBasicMetadataReaderWriter)
  protected
    procedure Error(const AMsg: String); virtual;
    procedure UpdateSegmentSize(AStream: TStream; ASegmentStartPos: Int64);
  public
    procedure WriteToStream(AStream: TStream; AImgFormat: TImgFormat); virtual;
  end;

  { TImgInfo }
  TImgInfo = class
  private
    FFileName: String;
    FFileDate: TDateTime;
    FFileSize: Int64;
    FImgFormat: TImgFormat;
    FImgWidth: Integer;
    FImgHeight: Integer;
    FWarnings: TStrings;
    FMetadataKinds: TMetadataKinds;
    FHeaderSegment: TBytes;
    FComment: String;
  private
    FExifData: TExifData;
    FIptcData: TIptcData;
    function GetComment: String;
    function GetWarnings: String;
    procedure SetComment(const AValue: String);
  protected
    procedure Error(const AMsg: String);
    function ExtractImgFormat(AStream: TStream): TImgFormat;
    procedure MergeToJpegStream(AInputStream, AOutputStream: TStream);
    procedure ReadJpeg(AStream: TStream);
    procedure ReadTiff(AStream: TStream);
    procedure StoreFileInfo(const AFileName: String);
    procedure WriteJpeg(AStream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromStream(AStream: TStream);
    procedure Save;
    procedure SaveToFile(const AFileName: String; AImgFile: String = '');

    function CreateExifData(ABigEndian: Boolean = false): TExifData;
    function CreateIptcData: TIptcData;

    function HasComment: Boolean;
    function HasExif: Boolean;
    function HasIptc: Boolean;
    function HasThumbnail: Boolean;
    function HasWarnings: boolean;

    { Comment stored in the Jpeg COM segment }
    property Comment: String read GetComment write SetComment;
    { Name of the file processed }
    property FileName: String read FFileName;
    { Date when the file was created }
    property FileDate: TDateTime read FFileDate;
    { Size of the file in bytes }
    property FileSize: Int64 read FFileSize;
    { Image format, jpeg or tiff }
    property ImgFormat: TImgFormat read FImgFormat;
    { Image width }
    property ImgWidth: Integer read FImgWidth;
    { Image height }
    property ImgHeight: Integer read FImgHeight;
    { Selects which kind of metadata will be loaded }
    property MetadataKinds: TMetadataKinds read FMetadataKinds write FMetadataKinds default mdkAll;
    { Warning message - NOTE: Reading of warnings is erasing the warnings list! }
    property Warnings: String read GetWarnings;

    property ExifData: TExifData read FExifData;
    property IptcData: TIptcData read FIptcData;  // to do: rename to IptcData
  end;


implementation

uses
  Variants,
  fpeStrConsts, fpeUtils, fpeExifReadWrite, fpeIptcReadWrite;

type
  TJpegJFIFSegment = packed record
    Identifier: packed array[0..4] of AnsiChar;    // 'JFIF'#0
    JFIFVersion: packed array[0..1] of Byte;       // 01 02
    DensityUnit: Byte;               // 0: aspect ratio, 1: inches, 2: cm
    XDensity: Word;
    YDensity: Word;
    ThumbnailWidth: Byte;            // Pixel count of thumbnail width...
    ThumbnailHeight: Byte;           // ... and height
  end;
  PJpegJFIFSegment = ^TJpegJFIFSegment;

  TJpegSOF0Segment = packed record
    DataPrecision: Byte;
    ImageHeight: Word;
    ImageWidth: Word;
    // and more..., not needed here.
  end;
  PJpegSOF0Segment = ^TJpegSOF0Segment;

const
  { JPEG markers consist of one or more $FF bytes, followed by a marker code
    byte (which is not an FF). Here are the marker codes needed by fpExif: }
  M_SOF0 = $C0;         // Start Of Frame 0
  M_SOI  = $D8;         // Start Of Image (beginning of datastream)
  M_EOI  = $D9;         // End Of Image (end of datastream)
  M_SOS  = $DA;         // Start Of Scan (begins compressed data)
  M_JFIF = $E0;         // Jfif marker                             224
  M_EXIF = $E1;         // Exif marker                             225
  M_IPTC = $ED;         // IPTC - Photoshop                        237
  M_COM  = $FE;         // Comment                                 254


//==============================================================================
//                         TBasicMetaDataWriter
//==============================================================================

constructor TBasicMetadataReaderWriter.Create(AImgInfo: TImgInfo);
begin
  FImgInfo := AImgInfo;
end;

procedure TBasicMetadataReaderWriter.Warning(const AMsg: String);
begin
  FImgInfo.FWarnings.Add(AMsg);
end;


//==============================================================================
//                          TBasicMetaDataReader
//==============================================================================

procedure TBasicMetadataReader.Error(const AMsg: String);
begin
  raise EFpExifReader.Create(AMsg);
end;

procedure TBasicMetadataReader.ReadFromStream(AStream: TStream;
  AImgFormat: TImgFormat);
begin
  Assert(AStream <> nil);
  FImgFormat := AImgFormat;
end;


//==============================================================================
//                        TBasicMetaDataWriter
//==============================================================================

procedure TBasicMetadataWriter.Error(const AMsg: String);
begin
  raise EFpExifWriter.Create(AMsg);
end;

procedure TBasicMetadataWriter.UpdateSegmentSize(AStream: TStream;
  ASegmentStartPos: Int64);
var
  startPos: Int64;
  segmentSize: Word;
  w: Word;
begin
  // If the metadata structure is part of a jpeg file (e.g.) then the start
  // position of the corresponding metadata segment has been stored in
  // ASegmentStartPos. In other cases ASegmentStartPos is -1.
  // This means: if ASegmentStartPos is > -1 then the segment size must be
  // written to the segment start position.
  if (ASegmentStartPos < 0) then
    exit;

  // From the current stream position (at the end) and the position where
  // the segment size must be written, we calculate the size of the segment
  startPos := ASegmentStartPos + SizeOf(word);
  segmentSize := AStream.Position - startPos;

  // Move the stream to where the segment size must be written...
  AStream.Position := startPos;

  // ... and write the segment size.
  w := BEToN(segmentSize);
  AStream.WriteBuffer(w, SizeOf(w));

  // Rewind stream to the end
  AStream.Seek(0, soFromEnd);
end;

procedure TBasicMetadataWriter.WriteToStream(AStream: TStream;
  AImgFormat: TImgFormat);
begin
  Assert(AStream <> nil);
  FImgFormat := AImgFormat;
end;


//==============================================================================
//                               TImgInfo
//==============================================================================

constructor TImgInfo.Create;
begin
  FMetadataKinds := mdkAll;
  FWarnings := TStringList.Create;
end;

destructor TImgInfo.Destroy;
begin
  FWarnings.Free;
  FExifData.Free;
  FIptcData.Free;
  inherited;
end;

function TImgInfo.CreateExifData(ABigEndian: Boolean = false): TExifData;
begin
  FWarnings.Clear;
  FExifData.Free;
  FExifData := TExifData.Create(ABigEndian);
  Result := FExifData;
end;

function TImgInfo.CreateIptcData: TIptcData;
begin
  FWarnings.Clear;
  FIptcData.Free;
  FIptcData := TIptcData.Create;
  Result := FIptcData;
end;

procedure TImgInfo.Error(const AMsg: String);
begin
  raise EFpExif.Create(AMsg);
end;

function TImgInfo.ExtractImgFormat(AStream: TStream): TImgFormat;
var
  p: Int64;
  hdr: array[0..SizeOf(TTiffHeader)-1] of byte;
  tiffHdr: TTiffHeader absolute hdr;
begin
  p := AStream.Position;
  try
    AStream.Read({%H-}hdr[0], SizeOf(hdr));
    // Test for jpeg signature
    if (hdr[0] = $FF) and (hdr[1] = $D8) then begin
      Result := ifJpeg;
      exit;
    end;
    // Test for TIFF header
    if (tiffHdr.BOM[0]='I') and (tiffHdr.BOM[1]='I') and (LEtoN(tiffHdr.Signature) = 42)
    then begin
      Result := ifTiff;
      exit;
    end;
    if (tiffHdr.BOM[0]='M') and (tiffHdr.BOM[1]='M') and (BEtoN(tiffHdr.signature) = 42)
    then begin
      Result := ifTiff;
      exit;
    end;
    Result := ifUnknown;
  finally
    AStream.Position := p;
  end;
end;

function TImgInfo.GetComment: String;
begin
  Result := FComment;
end;

function TImgInfo.GetWarnings: String;
begin
  Result := FWarnings.Text;
  FWarnings.Clear;
end;

function TImgInfo.HasComment: Boolean;
begin
  Result := FComment <> '';
end;

function TImgInfo.HasExif: Boolean;
begin
  Result := (FExifData <> nil) and (FExifData.TagCount > 0);
end;

function TImgInfo.HasIptc: Boolean;
begin
  Result := (FIptcData <> nil) and (FIptcData.TagCount > 0);
end;

function TImgInfo.HasThumbnail: boolean;
begin
  Result := (FExifData <> nil) and FExifData.HasThumbnail;
end;

function TImgInfo.HasWarnings: boolean;
begin
  Result := FWarnings.Count > 0;
end;

procedure TImgInfo.LoadFromFile(const AFileName: String);
var
  stream: TStream;
begin
  if not FileExists(AFileName) then
    Error(Format(rsFileNotFoundError, [AFileName]));

  FWarnings.Clear;
  StoreFileInfo(AFileName);
  stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TImgInfo.LoadFromStream(AStream: TStream);
begin
  FWarnings.Clear;
  FImgFormat := ExtractImgFormat(AStream);
  if FImgFormat = ifUnknown then
    Error(rsUnknownImageFormat);

  case FImgFormat of
    ifJpeg:
      ReadJpeg(AStream);
    ifTiff:
      ReadTiff(AStream);
    else
      Error('TImgInfo.LoadFromStream: ' + rsImageFormatNotSupported);
  end;
end;

{ Reads the image data from AInputstream and replaces the meta data segments
  by those of TImgInfo }
procedure TImgInfo.MergeToJpegStream(AInputStream, AOutputStream: TStream);
type
  TSegmentHeader = packed record
    Key: byte;
    Marker: byte;
    Size: Word;
  end;
var
  header: TSegmentHeader;
  n, count: Int64;
  savedPos: Int64;
begin
  // Write the header segment and all metadata segments stored in TImgInfo
  // to the beginning of the stream
  AOutputStream.Position := 0;
  WriteJpeg(AOutputStream);

  // Now write copy all other segments.
  AInputStream.Position := 0;
  while AInputStream.Position < AInputStream.Size do begin
    savedPos := AInputStream.Position;  // just for debugging
    n := AInputStream.Read(header{%H-}, SizeOf(header));
    if n <> Sizeof(header) then
      Error(rsIncompleteJpegSegmentHeader);
    if header.Key <> $FF then
      Error(rsJpegSegmentMarkerExpected);
    header.Size := BEToN(header.Size);

    // Save stream position before segment size value.
    savedPos := AInputStream.Position - 2;
    case header.Marker of
      M_SOI:
        header.Size := 0;
      M_JFIF, M_EXIF, M_IPTC, M_COM:  // these segments were already written by WriteJpeg
        ;
      M_SOS:
        begin
          // this is the last segment before compressed data which don't have a marker
          // --> just copy the rest of the file
          count := AInputStream.Size - savedPos;
          AInputStream.Position := savedPos;
          AOutputStream.WriteBuffer(header, 2);
          n := AOutputStream.CopyFrom(AInputStream, count);
          if n <> count then
            Error(rsJpegCompressedDataWriting);
          break;
        end;
      else
        AInputStream.Position := AInputStream.Position - 4;  // go back to where the segment begins
        n := AOutputStream.CopyFrom(AInputStream, Int64(header.Size) + 2);
        if n <> Int64(header.Size) + 2 then
          Error(rsJpegReadWriteErrorInSegment);
    end;
    AInputStream.Position := savedPos + header.Size;
  end;
end;

procedure TImgInfo.ReadJpeg(AStream: TStream);
var
  marker: Byte;
  size: Word;
  streamsize: Int64;
  p: Int64;
  buf: TBytes;
  reader: TBasicMetadataReader;
  bigEndian: Boolean;
 {$IFNDEF FPC}
  sa: ansistring;
 {$ENDIF}
begin
  p := AStream.Position;
  streamsize := AStream.Size;

  if not ((ReadByte(AStream) = $FF) and (ReadByte(AStream) = M_SOI)) then
    exit;

  while p < streamsize do begin
    // The basic structure of the jpeg segments is
    //   $FF ..... identifier (sometimes repeated)
    //   marker .. segment identifier (1 byte)
    //   size .... size of the segment in bytes (2 bytes), including size field
    //   data .... data of the segment, (size)-2 bytes.
    repeat
      marker := ReadByte(AStream);
    until marker <> $FF;
    size := BEtoN(ReadWord(AStream)) - 2;
    p := AStream.Position;
    case marker of
      M_EXIF:
        if FMetaDataKinds * [mdkExif, mdkExifNoMakerNotes] <> [] then begin
          reader := TExifReader.Create(self);
          try
            if not TExifReader(reader).ReadExifHeader(AStream) then
              exit;
            if not TExifReader(reader).ReadTiffHeader(AStream, bigEndian) then
              exit;
            FExifData := CreateExifData(bigEndian);
            try
              reader.ReadFromStream(AStream, ifJpeg);
            except
              FreeAndNil(FExifData);
              raise;
            end;
          finally
            reader.Free;
          end;
        end;
      M_IPTC:
        if (mdkIPTC in FMetadataKinds) then begin
          reader := TIptcReader.Create(self);
          try
            FIptcData := CreateIptcData;
            try
              reader.ReadFromStream(AStream, ifJpeg);
            except
              FreeAndNil(FIptcData);
              raise;
            end;
          except
            reader.Free;
          end;
        end;
      M_COM:
        if (mdkComment in FMetadataKinds) and (size > 0) then
        begin
          // JFIF comment is encoded as UTF8 according to
          // http://mail.kde.org/pipermail/digikam-devel/2006-May/005000.html
         {$IFDEF FPC}
          SetLength(FComment, size);
          AStream.Read(FComment[1], size);
         {$ELSE}
          SetLength(sa, size);
          AStream.Read(sa[1], size);
          {$IFDEF UNITCODE}
          FComment := UTF8Decode(sa);
          {$ELSE}
          FComment := Utf8ToAnsi(sa);
          {$ENDIF}
         {$ENDIF}
        end;
      M_JFIF:
        begin
          SetLength(FHeaderSegment, size);
          AStream.Read(FHeaderSegment[0], size);
          with PJpegJFIFSegment(@FHeaderSegment[0])^ do begin
            if not (
              (Identifier[0]='J') and (Identifier[1]='F') and
              (Identifier[2]='I') and (Identifier[3]='F') and
              (Identifier[4]=#0) )
            then
              exit;
            if (JFIFVersion[0] <> 1) then
              exit;
          end;
        end;
      M_SOF0:
        begin
          SetLength(buf, size);
          AStream.Read(buf[0], size);
          with PJpegSOF0Segment(@buf[0])^ do begin
            FImgHeight := BEtoN(ImageHeight);
            FImgWidth := BEtoN(ImageWidth);
          end;
          SetLength(buf, 0);
        end;
      M_EOI, M_SOS:
        break;
    end;
    AStream.Position := p + size;
  end;
end;

procedure TImgInfo.ReadTiff(AStream: TStream);
var
  reader: TExifReader;
  bigEndian: Boolean;
begin
  reader := TExifReader.Create(self);
  try
    if not TExifReader(reader).ReadTiffHeader(AStream, bigEndian) then
      exit;
    FExifData := CreateExifData(bigEndian);
    try
      reader.ReadFromStream(AStream, ifTiff);
    except
      FreeAndNil(FExifData);
      raise;
    end;
  finally
    reader.Free;
  end;
end;

procedure TImgInfo.Save;
begin
  SaveToFile(FFileName);
end;

procedure TImgInfo.SaveToFile(const AFileName: String; AImgFile: String = '');
var
  ms: TMemoryStream;
  srcStream: TFileStream;
begin
  if (AImgFile = '') then
    AImgFile := FFileName;

  if AImgFile = '' then
    Error(rsImageDataFileNotSpecified);

  if not FileExists(AImgFile) then
    Error(Format(rsImageDataFileNotExisting, [AImgFile]));

  FWarnings.Clear;
  ms := TMemoryStream.Create;
  try
    srcstream := TFileStream.Create(AImgFile, fmOpenRead + fmShareDenyNone);
    try
      if FImgFormat = ifUnknown then begin
        FimgFormat := ExtractImgFormat(srcstream);
        if FImgFormat = ifUnknown then
          Error(rsCannotSaveToUnknownFileFormat);
      end;
      case FImgFormat of
        ifJpeg: MergeToJpegStream(srcstream, ms);
        ifTiff: Error(Format(rsWritingNotImplemented, ['TIFF']));
        else    Error(rsImageFormatNotSupported);
      end;
    finally
      // Destroy the srcStream before saving the memorystream to file to prevent
      // an error if AImgFile = AFileName
      srcStream.Free;
    end;
    ms.SaveToFile(AFileName)
  finally
    ms.Free;
  end;
end;

procedure TImgInfo.SetComment(const AValue: String);
begin
  FComment := AValue;
end;

procedure TImgInfo.StoreFileInfo(const AFileName: String);
var
  rec: TSearchRec;
  res: word;
begin
  res := FindFirst(AFilename, faAnyFile, rec);
  if res = 0 then
  begin
    FFilename := AFilename;
    FFileDate := FileDateToDateTime(rec.Time);
    FFileSize := rec.Size;
  end;
  FindClose(rec);
end;

{ Writes all metadata-related segments to a stream. Note image data must be
  written separately. }
procedure TImgInfo.WriteJpeg(AStream: TStream);
const
  SOI_MARKER: array[0..1] of byte = ($FF, $D8);
  COM_MARKER: array[0..1] of byte = ($FF, $FE);
  JFIF_MARKER: array[0..1] of byte = ($FF, $E0);
  JFIF: ansistring = 'JFIF'#0;
var
  jfifSegment: TJpegJFIFSegment;
  writer: TBasicMetadataWriter;
  {$IFNDEF FPC}
  sa: ansistring;
  {$ENDIF}
begin
  // Write Start-of-image segment (SOI)
  AStream.WriteBuffer(SOI_MARKER, SizeOf(SOI_MARKER));

  // No Exif --> write an APP0 segment
  if not HasExif or (FMetadataKinds * [mdkExif, mdkExifNoMakerNotes] = []) then begin
    if Length(FHeaderSegment) = 0 then begin
      Move(JFIF[1], {%H-}JFIFSegment.Identifier[0], Length(JFIF));
      JFIFSegment.JFIFVersion[0] := 1;
      JFIFSegment.JFIFVersion[1] := 2;
      JFIFSegment.DensityUnit := 1;       // inch
      JFIFSegment.XDensity := NtoBE(72);  // 72 ppi
      JFIFSegment.YDensity := NtoBE(72);
      JFIFSegment.ThumbnailWidth := 0;    // no thumbnail in APP0 segment
      JFIFSegment.ThumbnailHeight := 0;
      AStream.WriteBuffer(JFIF_MARKER, SizeOf(JFIF_MARKER));
      WriteWord(AStream, NtoBE(Word(SizeOf(JFIFSegment) + 2)));
      AStream.WriteBuffer(JFIFSegment, SizeOf(JFIFSegment));
    end else
    begin
      AStream.WriteBuffer(JFIF_MARKER, SizeOf(JFIF_MARKER));
      WriteWord(AStream, NtoBE(Word(Length(FHeaderSegment) + 2)));
      AStream.WriteBuffer(FHeaderSegment[0], Length(FHeaderSegment));
    end;
  end else
  begin
    // Exif --> Write APP1 segment
    writer := TExifWriter.Create(Self);
    try
      TExifWriter(writer).BigEndian:= FExifData.BigEndian;
      writer.WriteToStream(AStream, ifJpeg);
    finally
      writer.Free;
    end;
  end;

  // Write IPTCSegment (APP13)
  if (mdkIPTC in FMetadataKinds) and HasIPTC then begin
    writer := TIptcWriter.Create(Self);
    try
      TIptcWriter(writer).WriteToStream(AStream, ifJpeg);
    finally
      writer.Free;
    end;
  end;

  // Write comment segment
  if (mdkComment in FMetadataKinds) and HasComment then begin
    // JFIF Comment is encoded as utf8
    // according to http://mail.kde.org/pipermail/digikam-devel/2006-May/005000.html
    AStream.WriteBuffer(COM_MARKER, SizeOf(COM_MARKER));
   {$IFDEF FPC}
    WriteWord(AStream, NtoBE(Word(Length(FComment) + 2)));
    AStream.WriteBuffer(FComment[1], Length(FComment));
   {$ELSE}
    {$IFDEF UNICODE}
    sa := UTF8Encode(FComment);
    {$ELSE}
    sa := AnsiToUTF8(FComment);
    {$ENDIF}
    WriteWord(AStream, NtoBE(Word(Length(sa) + 2)));
    AStream.WriteBuffer(sa[1], Length(sa));
   {$ENDIF}
  end;
end;


end.

