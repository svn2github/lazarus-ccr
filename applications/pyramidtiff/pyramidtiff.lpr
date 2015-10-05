{ Author: Mattias Gaertner mattias@freepascal.org

  License:
    Modified LGPL2 like FCL
}
program pyramidtiff;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, math, LazFileUtils, AvgLvlTree, CustApp,
  FPimage, FPReadJPEG, FPReadPNG, FPReadBMP, FPImgCanv, FPCanvas, MTProcs,
  PyTiGraphics, FPReadTiff, FPTiffCmn, FPWriteTiff;

const
  Version = '1.2';
type

  { TFilenameToStringTree }

  TFilenameToStringTree = class(TStringToStringTree)
  public
    constructor Create;
  end;

  { TPyramidTiffer }

  TPyramidTiffer = class(TCustomApplication)
  private
    FInputPath: string;
    FMinSize: Word;
    FOutputPath: string;
    FQuiet: boolean;
    FTileHeight: Word;
    FTileWidth: Word;
    FVerbose: boolean;
    procedure LoadTiff(out Img: TFPCompactImgBase;
      Reader: TFPReaderTiff; InStream: TMemoryStream;
      var ErrorMsg: string);
    procedure LoadOther(out Img: TFPCompactImgBase;
      Reader: TFPCustomImageReader; InStream: TMemoryStream);
    function ShrinkImage(LastImg: TFPCompactImgBase): TFPCompactImgBase;
    procedure TiffReaderCreateImage(Sender: TFPReaderTiff; IFD: TTiffIFD);
  protected
    procedure DoRun; override;
    procedure ParamError(const Msg: string);
    procedure ReadConfig;
    function CheckIfFileIsPyramidTiled(Filename: string; out ErrorMsg: string): boolean;
    function CheckIfStreamIsPyramidTiled(s: TStream; out ErrorMsg: string): boolean;
    function GetReaderClass(Filename: string): TFPCustomImageReaderClass; // worker thread
    function ConvertDir(InputDir, OutputDir: string; out ErrorMsg: string): boolean; // main thread
    procedure ConvertFilesParallel(Index: PtrInt; Data: Pointer; {%H-}Item: TMultiThreadProcItem); // worker thread
    function ConvertFile(InputFilename, OutputFilename: string; out ErrorMsg: string): boolean; // worker thread
    function Convert(Img: TFPCompactImgBase; OutputFilename: string; out ErrorMsg: string): boolean; // worker thread
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(WithHeader: boolean); virtual;

    property TileWidth: Word read FTileWidth write FTileWidth;
    property TileHeight: Word read FTileHeight write FTileHeight;
    property MinSize: Word read FMinSize write FMinSize;
    property Verbose: boolean read FVerbose write FVerbose;
    property Quiet: boolean read FQuiet write FQuiet;
    property InputPath: string read FInputPath write FInputPath;
    property OutputPath: string read FOutputPath write FOutputPath;
  end;

  { TMyCanvas }

  TMyCanvas = class(TFPImageCanvas)
  protected
    procedure DoCopyRect({%H-}x, {%H-}y: integer; {%H-}canvas: TFPCustomCanvas;
      const {%H-}SourceRect: TRect); override;
    procedure DoDraw({%H-}x, {%H-}y: integer; const {%H-}anImage: TFPCustomImage); override;
  end;

function CompareIFDForSize(i1, i2: Pointer): integer;
var
  IFD1: TTiffIFD absolute i1;
  IFD2: TTiffIFD absolute i2;
  Size1: Int64;
  Size2: Int64;
begin
  Size1:=IFD1.ImageWidth*IFD1.ImageHeight;
  Size2:=IFD2.ImageWidth*IFD2.ImageHeight;
  if Size1>Size2 then Result:=1
  else if Size1<Size2 then Result:=-1
  else Result:=0;
end;

function StringToList(const LongOpts: string): TStrings;
const
  SepChars = ' '#10#13#9;
var
  L : TStringList;
  Len,I,J : Integer;
begin
  l:=TStringList.Create;
  I:=1;
  Len:=Length(LongOpts);
  while I<=Len do begin
    while Isdelimiter(SepChars,LongOpts,I) do
      Inc(I);
    J:=I;
    while (J<=Len) and Not IsDelimiter(SepChars,LongOpts,J) do
      Inc(J);
    if (I<=J) then
      L.Add(Copy(LongOpts,I,(J-I)));
    I:=J+1;
  end;
  Result:=l;
end;

function CompareFilenameAndFilenameToStringTreeItem(Key, Data: Pointer
  ): integer;
begin
  Result:=CompareFilenames(String(Key),PStringToStringItem(Data)^.Name);
end;

function CompareFilenameToStringItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(PStringToStringItem(Data1)^.Name,
                           PStringToStringItem(Data2)^.Name);
end;

{ TFilenameToStringTree }

constructor TFilenameToStringTree.Create;
begin
  SetCompareFuncs(@CompareFilenameToStringItems,
                  @CompareFilenameAndFilenameToStringTreeItem,false);
end;

{ TMyCanvas }

procedure TMyCanvas.DoCopyRect(x, y: integer; canvas: TFPCustomCanvas;
  const SourceRect: TRect);
begin

end;

procedure TMyCanvas.DoDraw(x, y: integer; const anImage: TFPCustomImage);
begin

end;

{ TMyApplication }

procedure TPyramidTiffer.TiffReaderCreateImage(Sender: TFPReaderTiff;
  IFD: TTiffIFD);
var
  Desc: TFPCompactImgDesc;
begin
  // free old image
  FreeAndNil(IFD.Img);

  Desc.HasAlpha:=IFD.AlphaBits>0;
  Desc.Gray:=IFD.PhotoMetricInterpretation in [0,1];
  Desc.Depth:=Max(Max(Max(IFD.RedBits,
                          IFD.GreenBits),
                          IFD.BlueBits),
                          IFD.GrayBits);
  IFD.Img:=CreateFPCompactImg(Desc,IFD.ImageWidth,IFD.ImageHeight);
end;

function TPyramidTiffer.ShrinkImage(LastImg: TFPCompactImgBase): TFPCompactImgBase;

  function Half(i: integer): integer;
  begin
    Result:=(i+1) div 2;
    if Result<1 then Result:=1;
  end;

var
  ImgCanvas: TFPImageCanvas;
begin
  Result:=TFPCompactImgBase(CreateFPCompactImg(LastImg.Desc, Half(LastImg.Width), Half(
    LastImg.Height)));
  ImgCanvas:=TMyCanvas.create(Result);
  ImgCanvas.Interpolation:=TLinearInterpolation.Create;
  ImgCanvas.StretchDraw(0, 0, Result.Width, Result.Height, LastImg);
  ImgCanvas.Interpolation.Free;
  ImgCanvas.Free;
end;

procedure TPyramidTiffer.LoadTiff(out Img: TFPCompactImgBase;
  Reader: TFPReaderTiff; InStream: TMemoryStream; var ErrorMsg: string);
begin
  Reader.OnCreateImage:=@TiffReaderCreateImage;
  {$ifdef FPC_Debug_Image}
  Reader.Debug:=true;
  {$endif}
  Reader.LoadFromStream(InStream);
  if Reader.ImageCount=0 then begin
    ErrorMsg:='tiff has no image';
    exit;
  end;
  Img:=Reader.GetBiggestImage.Img as TFPCompactImgBase;
end;

procedure TPyramidTiffer.LoadOther(out Img: TFPCompactImgBase;
  Reader: TFPCustomImageReader; InStream: TMemoryStream);
begin
  Img:=TFPCompactImgRGB8Bit.Create(0, 0);
  Reader.ImageRead(InStream, Img);
  Img:=GetMinimumFPCompactImg(Img, true) as TFPCompactImgBase;
end;

procedure TPyramidTiffer.DoRun;
var
  ErrorMsg: string;
begin
  if GetCurrentDir='' then
    SetCurrentDir(GetEnvironmentVariable('PWD'));

  ReadConfig;

  if HasOption('min-size') then begin
    MinSize:=StrToInt(GetOptionValue('min-size'));
    if (MinSize<4) or (MinSize>32768) then
      ParamError('min-size out of range (4..32768): '+IntToStr(MinSize));
  end;
  if HasOption('width') then begin
    TileWidth:=StrToInt(GetOptionValue('width'));
    if (TileWidth<4) or (TileWidth>32768) then
      ParamError('width out of range (4..32768): '+IntToStr(TileWidth));
  end;
  if HasOption('height') then begin
    TileHeight:=StrToInt(GetOptionValue('height'));
    if (TileHeight<4) or (TileHeight>32768) then
      ParamError('height out of range (4..32768): '+IntToStr(TileHeight));
  end;

  if HasOption('c') then begin
    // only check
    if HasOption('i') then
      ParamError('can not combine option -c and -i');
    if HasOption('o') then
      ParamError('can not combine option -c and -o');
    InputPath:=CleanAndExpandFilename(GetOptionValue('c'));
    if not FileExistsUTF8(InputPath) then
      ParamError('check file not found: '+InputPath);
    if CheckIfFileIsPyramidTiled(InputPath,ErrorMsg) then begin
      if not Quiet then
        writeln('ok');
    end else begin
      if not Quiet then
        writeln('not ok: ',ErrorMsg);
      ExitCode:=1;
    end;
  end else begin
    // convert
    if not HasOption('i') then
      ParamError('missing parameter -i');
    if not HasOption('o') then
      ParamError('missing parameter -o');

    GetCurrentDirUTF8;
    InputPath:=CleanAndExpandFilename(GetOptionValue('i'));
    OutputPath:=CleanAndExpandFilename(GetOptionValue('o'));
    if DirPathExists(InputPath) then begin
      // convert whole directory
      if not DirPathExists(OutputPath) then
        ParamError('output directory not found: '+OutputPath);
      if not ConvertDir(InputPath,OutputPath,ErrorMsg) then begin
        if not Quiet then
          writeln('ERROR: ',ErrorMsg);
        ExitCode:=1;
      end;
    end else begin
      // convert single file
      if not FileExistsUTF8(InputPath) then
        ParamError('input file not found: '+InputPath);
      if not DirectoryExistsUTF8(ExtractFilePath(OutputPath)) then
        ParamError('output directory not found: '+ExtractFilePath(OutputPath));
      if DirPathExists(OutputPath) then
        ParamError('output is a directory, but input is a file');
      if not ConvertFile(InputPath,OutputPath,ErrorMsg) then begin
        if not Quiet then
          writeln('ERROR: ',ErrorMsg);
        ExitCode:=1;
      end;
    end;
  end;

  // stop program loop
  Terminate;
end;

procedure TPyramidTiffer.ParamError(const Msg: string);
begin
  writeln('Error: ',Msg);
  writeln;
  WriteHelp(false);
  Halt(2);
end;

procedure TPyramidTiffer.ReadConfig;
const
  ShortOpts = 'hc:i:o:qvV';
  LongOpts = 'help width height min-size quiet verbose version';
var
  LongOptions: TStrings;

  procedure CheckOpts;
  var
    Opts,NonOpts: TStrings;
    ErrorMsg: String;
    i: Integer;
  begin
    Opts:=TStringList.Create;
    NonOpts:=TStringList.Create;
    try
      ErrorMsg:=CheckOptions(ShortOpts,LongOptions,Opts,NonOpts);
      if ErrorMsg<>'' then begin
        ShowException(Exception.Create(ErrorMsg));
        Halt(1);
      end;
      for i:=0 to NonOpts.Count-1 do
        if NonOpts[i]<>'' then
          ParamError('invalid parameter "'+NonOpts[i]+'"');
    finally
      Opts.Free;
      NonOpts.Free;
    end;
    Verbose:=HasOption('v','verbose');
    Quiet:=HasOption('q','quiet');
  end;

begin
  LongOptions:=StringToList(LongOpts);
  try
    CheckOpts;

    // parse parameters
    if HasOption('h','help') then begin
      WriteHelp(true);
      Halt(1);
    end;

    // parse parameters
    if HasOption('V','version') then begin
      writeln(Version);
      Halt(0);
    end;
  finally
    LongOptions.Free;
  end;
end;

function TPyramidTiffer.CheckIfFileIsPyramidTiled(Filename: string; out
  ErrorMsg: string): boolean;
var
  ms: TMemoryStream;
begin
  Result:=false;
  ErrorMsg:='';
  try
    if Verbose then
      writeln('Checking file "',Filename,'"');
    ms:=TMemoryStream.Create;
    try
      ms.LoadFromFile(Filename);
      ms.Position:=0;
      Result:=CheckIfStreamIsPyramidTiled(ms,ErrorMsg);
    finally
      ms.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
    end;
  end;
end;

function TPyramidTiffer.CheckIfStreamIsPyramidTiled(s: TStream; out
  ErrorMsg: string): boolean;
var
  Reader: TFPReaderTiff;
  i: Integer;
  Img: TTiffIFD;
  SmallerImg: TTiffIFD;
begin
  Result:=false;
  ErrorMsg:='';
  try
    Reader:=TFPReaderTiff.Create;
    try
      ErrorMsg:='this is not a tiff file: ';
      Reader.LoadHeaderFromStream(s);
      ErrorMsg:='error in tiff file: ';
      Reader.LoadIFDsFromStream;
      if Reader.ImageCount<1 then begin
        ErrorMsg:='no images found in tif';
        exit;
      end;
      // sort ascending
      Reader.ImageList.Sort(@CompareIFDForSize);
      SmallerImg:=nil;
      for i:=0 to Reader.ImageCount-1 do begin
        Img:=Reader.Images[i];
        if Verbose then
          writeln('  ',i,'/',Reader.ImageCount,' ',Img.ImageWidth,'x',Img.ImageHeight);
        if (Img.TileWidth<1) or (Img.TileLength<1) then begin
          ErrorMsg:='image '+IntToStr(i)+' is not tiled';
          exit;
        end;
        if SmallerImg=nil then begin
          // this is the smallest image
          if (Img.ImageWidth>MinSize*2)
          or (Img.ImageHeight>MinSize*2) then begin
            ErrorMsg:='missing small scale step. min-size='+IntToStr(MinSize)+'.'
              +' Smallest image: '+IntToStr(Img.ImageWidth)+'x'+IntToStr(Img.ImageHeight);
            exit;
          end;
        end else begin
          if (SmallerImg.ImageWidth*2+1)<Img.ImageWidth then begin
            ErrorMsg:='missing scale step between ImageWidth='
              +IntToStr(SmallerImg.ImageWidth)+' and '+IntToStr(Img.ImageWidth);
            exit;
          end;
          if (SmallerImg.ImageHeight*2+1)<Img.ImageHeight then begin
            ErrorMsg:='missing scale step between ImageHeight='
              +IntToStr(SmallerImg.ImageHeight)+' and '+IntToStr(Img.ImageHeight);
            exit;
          end;
        end;
        SmallerImg:=Img;
      end;
      Result:=true;
    finally
      Reader.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=ErrorMsg+E.Message;
    end;
  end;
end;

function TPyramidTiffer.GetReaderClass(Filename: string
  ): TFPCustomImageReaderClass;
var
  Ext: String;
  i: Integer;
begin
  Result:=nil;
  Ext:=lowercase(ExtractFileExt(Filename));
  Delete(Ext,1,1); // delete '.'
  if (Ext='tif') or (Ext='tiff') then
    Result:=TFPReaderTiff
  else begin
    for i:=0 to ImageHandlers.Count-1 do begin
      if Pos(Ext,ImageHandlers.{$IF FPC_FULLVERSION>=20701}Extensions{$ELSE}Extentions{$ENDIF}[ImageHandlers.TypeNames[i]])<1
      then continue;
      Result:=ImageHandlers.ImageReader[ImageHandlers.TypeNames[i]];
    end;
  end;
end;

function TPyramidTiffer.ConvertDir(InputDir, OutputDir: string; out
  ErrorMsg: string): boolean;
var
  FileInfo: TSearchRec;
  Files: TStringToStringTree;
  Item: PStringToStringItem;
  InputFile: String;
  OutputFile: String;
  FileList: TStringList;
begin
  Result:=false;
  ErrorMsg:='';
  InputDir:=AppendPathDelim(InputDir);
  OutputDir:=AppendPathDelim(OutputDir);
  Files:=TStringToStringTree.Create(false);
  FileList:=TStringList.Create;
  try
    if FindFirstUTF8(InputDir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // skip special files
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        if GetReaderClass(FileInfo.Name)=nil then continue;
        Files[FileInfo.Name]:='1';
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
    if CompareFilenames(InputDir,OutputDir)=0 then begin
      // input and out dir are the same
      // => remove output files from input list
      for Item in Files do begin
        InputFile:=Item^.Name;
        if (CompareFileExt(InputFile,'tif',false)=0) then continue;
        OutputFile:=ChangeFileExt(InputFile,'.tif');
        Files.Remove(OutputFile);
      end;
    end;
    // convert
    for Item in Files do
      FileList.Add(Item^.Name);
    try
      ProcThreadPool.DoParallel(@ConvertFilesParallel,0,FileList.Count-1,FileList);
      Result:=true;
    except
      on E: Exception do
        ErrorMsg:=E.Message;
    end;
  finally
    FileList.Free;
    Files.Free;
  end;
end;

procedure TPyramidTiffer.ConvertFilesParallel(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  Files: TStringList;
  InputFilename: String;
  OutputFilename: String;
  ErrorMsg: string;
begin
  Files:=TStringList(Data);
  InputFilename:=AppendPathDelim(InputPath)+Files[Index];
  OutputFilename:=AppendPathDelim(OutputPath)+ChangeFileExt(Files[Index],'.tif');
  if not ConvertFile(InputFilename,OutputFilename,ErrorMsg) then
    raise Exception.Create(ErrorMsg+',input='+InputFilename+',output='+OutputFilename);
end;

function TPyramidTiffer.ConvertFile(InputFilename, OutputFilename: string; out
  ErrorMsg: string): boolean;
var
  InStream: TMemoryStream;
  ReaderClass: TFPCustomImageReaderClass;
  Reader: TFPCustomImageReader;
  Img: TFPCompactImgBase;
begin
  Result:=false;
  ErrorMsg:='';
  try
    if Verbose then
      writeln('Reading file "',InputFilename,'"');
    InStream:=TMemoryStream.Create;
    Reader:=nil;
    Img:=nil;
    try
      // load file
      InStream.LoadFromFile(InputFilename);
      InStream.Position:=0;

      // get the right image type reader
      ReaderClass:=GetReaderClass(InputFilename);
      if ReaderClass=nil then begin
        ErrorMsg:='unknown file extension "'+ExtractFileExt(InputFilename)+'"';
        exit;
      end;
      Reader:=ReaderClass.Create;

      // parse image
      if Reader is TFPReaderTiff then begin
        LoadTiff(Img, TFPReaderTiff(Reader), InStream, ErrorMsg);
      end else begin
        LoadOther(Img, Reader, InStream);
      end;
      // free memory early
      FreeAndNil(InStream);
      FreeAndNil(Reader);

      // convert
      Result:=Convert(Img,OutputFilename,ErrorMsg);
    finally
      InStream.Free;
      Reader.Free;
      Img.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
    end;
  end;
end;

function TPyramidTiffer.Convert(Img: TFPCompactImgBase; OutputFilename: string; out
  ErrorMsg: string): boolean;

  procedure AddTiff(Writer: TFPWriterTiff; Img: TFPCustomImage;
    PageNumber, PageCount, TileWidth, TileHeight: integer;
    Desc: TFPCompactImgDesc);
  begin
    Img.Extra[TiffPageNumber]:=IntToStr(PageNumber);
    Img.Extra[TiffPageCount]:=IntToStr(PageCount);
    Img.Extra[TiffTileWidth]:=IntToStr(TileWidth);
    Img.Extra[TiffTileLength]:=IntToStr(TileHeight);
    SetFPImgExtraTiff(Desc,Img,false);
    Img.Extra[TiffCompression]:=IntToStr(TiffCompressionDeflateZLib);
    Writer.AddImage(Img);
  end;

var
  OutStream: TMemoryStream;
  Writer: TFPWriterTiff;
  Size: Int64;
  Count: Integer;
  Index: Integer;
  LastImg: TFPCompactImgBase;
  NewImg: TFPCompactImgBase;
begin
  Result:=false;
  try
    // compute the number of images
    Count:=1;
    Size:=Int64(Img.Width)*Img.Height;
    while Size>4096 do begin
      Size:=Size div 4;
      inc(Count);
    end;

    // create images
    if Verbose then
      writeln('Creating file "',OutputFilename,'"');
    OutStream:=TMemoryStream.Create;
    Writer:=nil;
    LastImg:=nil;
    NewImg:=nil;
    try
      Writer:=TFPWriterTiff.Create;
      Index:=0;
      AddTiff(Writer,Img,Index,Count,TileWidth,TileHeight,Img.Desc);
      // add smaller images
      LastImg:=Img;
      while Index+1<Count do begin
        Index+=1;
        // create next image with half the width and height
        NewImg:=ShrinkImage(LastImg);
        AddTiff(Writer,NewImg,Index,Count,TileWidth,TileHeight,NewImg.Desc);
        // free last step
        if LastImg<>Img then
          FreeAndNil(LastImg);
        LastImg:=NewImg;
        NewImg:=nil;
      end;
      // free memory early
      FreeAndNil(LastImg);

      // create stream
      Writer.SaveToStream(OutStream);
      OutStream.Position:=0;

      // save to file
      OutStream.SaveToFile(OutputFilename);
      if Verbose then
        writeln('Saved file "',OutputFilename,'"');
      Result:=true;
    finally
      if LastImg<>Img then
        LastImg.Free;
      NewImg.Free;
      Writer.Free;
      OutStream.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
    end;
  end;
end;

constructor TPyramidTiffer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  TileWidth:=256;
  TileHeight:=256;
  MinSize:=32;
end;

destructor TPyramidTiffer.Destroy;
begin
  inherited Destroy;
end;

procedure TPyramidTiffer.WriteHelp(WithHeader: boolean);
var
  ImgType: String;
  i: Integer;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  if WithHeader then begin
    writeln('Version ',Version);
    writeln;
    writeln('pyramidtiff creates a tiff containing multiple images:');
    writeln('the original image,');
    writeln('the image with half the width and half the height (rounded up),');
    writeln('the image with quartered width/height (rounded up),');
    writeln('... and so forth.');
    writeln;
  end;
  writeln('-i <input file>');
  write('   Input image file can be a:');
  for i:=0 to ImageHandlers.Count-1 do begin
    ImgType:=ImageHandlers.TypeNames[i];
    write(' ',ImageHandlers.{$IF FPC_FULLVERSION>=20701}Extensions{$ELSE}Extentions{$ENDIF}[ImgType]);
  end;
  writeln;
  writeln('   If input file is a directory then the -o must be a directory too.');
  writeln('   All image files in the directory will be converted.');
  writeln('-o <output file>');
  writeln('   Output image file. It will always be a tif file, no matter what');
  writeln('   extension it has.');
  writeln('-c <input file>');
  writeln('   Check if file is a pyramid, tiled tif. 0 = yes, 1 = no.');
  writeln('   You can not use both -c and -i');
  writeln('--width=<tilewidth>');
  writeln('   In pixel. Default=',TileWidth);
  writeln('--height=<tileheight>');
  writeln('   In pixel. Default=',TileHeight);
  writeln('--min-size=<min size>');
  writeln('   Create no images with a smaller width or height than this value in pixel.');
  writeln('   Default=',MinSize);
  //writeln('--skip-check');
  //writeln('   Skip check if output file is already a pyramid tiled tif.');
  writeln('-h or --help');
  writeln('   Write this help');
  writeln('-q or --quiet');
  writeln('   Be less verbose');
  writeln('-v or --verbose');
  writeln('   Be more verbose');
  writeln('-V or --version');
  writeln('   Write version.');
  writeln;
  writeln('Examples:');
  writeln('  Convert input.jpg into output.tif:');
  writeln('  ',ExeName,' -i input.jpg -o output.tif');
  writeln;
  writeln('  Check if file.tif is already a pyramid, tiled tif:');
  writeln('  ',ExeName,' -c file.tif');
  writeln;
end;

var
  Application: TPyramidTiffer;
begin
  Application:=TPyramidTiffer.Create(nil);
  Application.Run;
  Application.Free;
end.

