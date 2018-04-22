{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThumbImage.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer att Excite dott com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

Changes form the previous Version:

Converted the rotation Functions to use scanlines for faster results
  I have converted the movement from an array of TRGBTriple to an
  an array of bytes. Right now it must rotate the following formats
  without big speed differences and problems pf8bit,pf24bit,pf32bit
  the pf4bit,pf1bit is converted to pf8bit.
  The Pfdevice,pfcustom is converted into pf24bit.
  all the Color conversions do not revert to the primary state after the
  rotation

Added the Mirror routines
Removed the 180 degree rotation and replaced by the mirror(mtBoth) call.
 this let the GDI engine to make the rotation and it is faster than any
 rotation I have tested until now I have tested this routine with
 and image of 2300x3500x24bit without any problems on Win2K.
 I must test it on Win98 before release.
-----------------------------------------------------------------------------}
// $Id$

{$MODE objfpc}{$H+}

unit JvThumbImage;

interface

uses
  LCLIntf, LCLType,
  Classes, Controls, ExtCtrls, SysUtils, Graphics, Forms, Dialogs, IntfGraphics,
  JvBaseThumbnail;

type
  TAngle = (AT0, AT90, AT180, AT270);

  // (rom) renamed elements
  TMirror = (mtHorizontal, mtVertical, mtBoth);

  TCurveArray = array [0..255] of Byte;
  TRotateNotify = procedure(Sender: TObject; Percent: Byte; var Cancel: Boolean) of object;
  TFilterEmpty = function: Byte;
  TFilterArray = array [1..9] of Byte;

  TJvTransformProc = procedure (AIntfImage: TLazIntfImage;
    ARedData, AGreenData, ABlueData: Pointer);

  TJvTransformProc2 = procedure (ASourceIntfImage, ADestIntfImage: TLazIntfImage;
    ARedData, AGreenData, ABlueData: Pointer);

  { TJvThumbImage }

  TJvThumbImage = class(TJvBaseThumbImage)
  private
    FAngle: TAngle;
    FModified: Boolean;
    FOnRotate: TRotateNotify;
    FZoom: Word;
    FOnLoad: TNotifyEvent;
    FFileName: string;
    FClass: TGraphicClass;
    FOnInvalidImage: TInvalidImageEvent;
    procedure SetAngle(AAngle: TAngle);
    function GetModify: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Mirror(MirrorType: TMirror);
    procedure ChangeRGB(R, G, B: Longint);
    procedure ChangeRGBCurves(R, G, B: TCurveArray);
    procedure ScaleDown(MaxW, MaxH: Longint);
    procedure LoadFromFile(AFile: string); //virtual;
    procedure LoadFromStream(AStream: TStream; AType: TGRFKind); // needs more tests
    procedure SaveToStream(AStream: TStream; AType: TGRFKind); // testing it
    procedure SaveToFile(AFile: string);
    procedure Save;
    procedure Transform(TransformProc: TJvTransformProc; ARedData: Pointer = nil;
      AGreenData: Pointer = nil; ABlueData: Pointer = nil); overload;
    procedure Transform(TransformProc: TJvTransformProc2; ARedData: Pointer = nil;
      AGreenData: Pointer = nil; ABlueData: Pointer = nil); overload;
    procedure BitmapNeeded;
    //    Procedure FilterFactory(Filter: TFilterArray; Divider: Byte);
    procedure Invert;
    procedure Contrast(const Percent: TPercent);
    procedure Lightness(const Percent: TPercent);
    procedure Grayscale;
    procedure Rotate(AAngle: TAngle);
    function GetFilter: string;
    property FileName: String read FFileName;
    //property JpegScale: TJPegScale read vJPegScale write vJpegScale;
  published
    property Angle: TAngle read FAngle write SetAngle;
    property Modified: Boolean read FModified;
    //Property OnRelease : TdestroyNotify read EVonrelease write Evonrelease;
    property CanModify: Boolean read GetModify;
    property Zoom: Word read FZoom write FZoom;
    // (rom) should be called in the implementation more often
    property OnRotate: TRotateNotify read FOnRotate write FOnRotate;
    property OnLoaded: TNotifyEvent read FOnLoad write FOnLoad;
    property OnInvalidImage: TInvalidImageEvent read FOnInvalidImage write FOnInvalidImage;
  end;


implementation

uses
  FPImage,
  JvThumbnails, JvTypes, JvResources;

procedure GrayScaleProc(AImg: TLazIntfImage; ARedData, AGreenData, ABlueData: Pointer);
var
  r, c: Integer;
  clr: TColor;
  col: TFPColor;
  intens: Integer;
begin
  for r := 0 to AImg.Height - 1 do
    for c := 0 to AImg.Width - 1 do begin
      col := AImg.Colors[c, r];
      intens := (integer(col.Red) + col.Green + col.Blue) div 3;
      AImg.Colors[c, r] := FPColor(intens, intens, intens, col.Alpha);
    end;
end;

procedure InvertProc(AImg: TLazIntfImage; ARedData, AGreenData, ABlueData: Pointer);
const
  MX: word = $FFFF;
var
  r, c: Integer;
  col: TFPColor;
  a: Word;
begin
  for r := 0 to AImg.Height - 1 do
    for c := 0 to AImg.Width - 1 do begin
      col := AImg.Colors[c, r];
      a := col.Alpha;
      AImg.Colors[c, r] := FPColor(MX-col.Red, MX-col.Green, MX-col.Blue, a);
    end;
end;

procedure MirrorHorProc(AImg: TLazIntfImage; ARedData, AGreenData, ABlueData: Pointer);
var
  r, c, w, h: Integer;
  col1, col2: TFPColor;
begin
  w := AImg.Width;
  h := AImg.Height;
  for r := 0 to h - 1 do
    for c := 0 to w div 2 do begin
      col1 := AImg.Colors[c, r];
      col2 := AImg.Colors[w-1-c, r];
      AImg.Colors[c, r] := col2;
      AImg.Colors[w-1-c, r] := col1;
    end;
end;

procedure MirrorVertProc(AImg: TLazIntfImage; ARedData, AGreenData, ABlueData: Pointer);
var
  r, c, w, h: Integer;
  col1, col2: TFPColor;
begin
  w := AImg.Width;
  h := AImg.Height;
  for c := 0 to w - 1 do
    for r := 0 to h div 2 do begin
      col1 := AImg.Colors[c, r];
      col2 := AImg.Colors[c, h-1-r];
      AImg.Colors[c, r] := col2;
      AImg.Colors[c, h-1-r] := col1;
    end;
end;

procedure Rotate90Proc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c, w, h: Integer;
  col: TFPColor;
begin
  w := ASrcImg.Width;
  h := ASrcImg.Height;
  ADestImg.SetSize(h, w);
  for r := 0 to h - 1 do
    for c := 0 to w - 1 do begin
      col := ASrcImg.Colors[c, r];
      ADestImg.Colors[r, w-1-c] := col;
    end;
end;

procedure Rotate180Proc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c, w, h: Integer;
  col: TFPColor;
begin
  w := ASrcImg.Width;
  h := ASrcImg.Height;
  for r := 0 to h - 1 do
    for c := 0 to w - 1 do begin
      col := ASrcImg.Colors[c, r];
      ADestImg.Colors[w-1-c, h-1-r] := col;
    end;
end;

procedure Rotate270Proc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c, w, h: Integer;
  col: TFPColor;
begin
  w := ASrcImg.Width;
  h := ASrcImg.Height;
  ADestImg.SetSize(h, w);
  for r := 0 to h - 1 do
    for c := 0 to w - 1 do begin
      col := ASrcImg.Colors[c, r];
      ADestImg.Colors[h-1-r, c] := col;
    end;
end;

procedure RGBProc(AImg: TLazIntfImage; ARedData, AGreenData, ABlueData: Pointer);
var
  r, c: Integer;
  clr: TColor;
  col: TFPColor;
  a: Word;
  rVal, gVal, bVal: Byte;
  deltaR, deltaG, deltaB: Integer;
begin
  deltaR := PtrUInt(ARedData);
  deltaG := PtrUInt(AGreenData);
  deltaB := PtrUInt(ABlueData);
  for r := 0 to AImg.Height - 1 do
    for c := 0 to AImg.Width - 1 do begin
      a := AImg.Colors[c, r].Alpha;
      clr := AImg.TColors[c, r];
      rVal := BoundByte(0, 255, GetBValue(clr) + deltaR);
      gVal := BoundByte(0, 255, GetGValue(clr) + deltaG);
      bVal := BoundByte(0, 255, GetBValue(clr) + deltaB);
      col := FPColor(rval shl 8, gval shl 8, bval shl 8, a);
      AImg.Colors[c, r] := col;
    end;
end;

procedure RGBCurveProc(AImg: TLazIntfImage; ARedData, AGreenData, ABlueData: Pointer);
var
  r, c: Integer;
  clr: TColor;
  rVal, gVal, bVal: Byte;
  a: Word;
  col: TFPColor;
begin
  for r := 0 to AImg.Height - 1 do
    for c := 0 to AImg.Width - 1 do begin
      a := AImg.Colors[c, r].Alpha;
      clr := AImg.TColors[c, r];
      rVal := TCurveArray(ARedData^)[GetRValue(clr)];
      gVal := TCurveArray(AGreenData^)[GetGValue(clr)];
      bVal := TCurveArray(ABlueData^)[GetBValue(clr)];
      col := FPColor(rVal shl 8, gVal shl 8, bVal shl 8, a);
      AImg.Colors[c, r] := col;
    end;
end;


{ TJvThumbImage }

constructor TJvThumbImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAngle := AT0;
//  FClass := Graphics.TBitmap;
  FModified := False;
end;

destructor TJvThumbImage.Destroy;
begin
  inherited Destroy;
end;

procedure TJvThumbImage.Lightness(const Percent: TPercent);
var
  Amount: Integer;
  RCurve: TCurveArray;
  I: Integer;
begin
  Amount := Round((255 / 100) * Percent);
  if Amount > 0 then
    for I := 0 to 255 do
      RCurve[I] := BoundByte(0, 255, I + ((Amount * (I xor 255)) shr 8))
  else
    for I := 0 to 255 do
      RCurve[I] := BoundByte(0, 255, I - ((Abs(Amount) * I) shr 8));
  ChangeRGBCurves(RCurve, RCurve, RCurve);
end;

procedure TJvThumbImage.Rotate(AAngle: TAngle);
begin
  case AAngle of
    AT90:
      Transform(@Rotate90Proc);
    AT180:
      Transform(@Rotate180Proc);
    AT270:
      Transform(@Rotate270Proc);
  end;
end;

function TJvThumbImage.GetFilter: string;
var
  //  a: string;
  P: Longint;
begin
  Result := Graphics.GraphicFilter(TGraphic);
  // (rom) better clean that up
  P := Pos('(', Result);
  InsertStr(Result, RsPcxTga, P);
  P := Pos('|', Result);
  InsertStr(Result, RsPcxTga, P);
  Result := Result + RsFileFilters;
    //Graphics.GraphicFilter(TGraphic)+'|PCX File|*.PCX|Targa File|*.TGA';
  { TODO : Add in the filter the rest of the images we support but are not registered to the Graphics unit }
end;

procedure TJvThumbImage.Contrast(const Percent: TPercent);
var
  Amount: Integer;
  Counter: Integer;
  Colors: TCurveArray;
begin
  Amount := Round((256 / 100) * Percent);
  for Counter := 0 to 127 do
    Colors[Counter] := BoundByte(0, 255, Counter - ((Abs(128 - Counter) * Amount) div 256));
  for Counter := 127 to 255 do
    Colors[Counter] := BoundByte(0, 255, Counter + ((Abs(128 - Counter) * Amount) div 256));
  ChangeRGBCurves(Colors, Colors, Colors);
end;

procedure TJvThumbImage.LoadFromStream(AStream: TStream; AType: TGRFKind);
var
  Bmp: Graphics.TBitmap;
  Jpg: TJpegImage;
  Ico: TIcon;
  (*********** NOT CONVERTED ***
  Wmf: TMetafile;
  ****************************)
begin
  //testing the stream load capabilities;
  // (rom) deactivated because LoadFromStream is not defined that way
  //AStream.Seek(0, soFromBeginning); //most of the stream error are generated because this is not at the proper position
  case AType of
    grBMP:
      begin
        Bmp := Graphics.TBitmap.Create;
        try
          Bmp.LoadFromStream(AStream);
          Bmp.PixelFormat := pf24bit;
          Picture.Assign(Bmp);
        finally
          FreeAndNil(Bmp);
        end;
      end;
    grJPG:
      begin
        Jpg := TJpegImage.Create;
        try
          Jpg.LoadFromStream(AStream);
          Picture.Assign(Jpg);
        finally
          FreeAndNil(Jpg);
        end;
      end;
    (**************** NOT CONVERTED ***
    grWMF, grEMF:
      begin
        Wmf := Graphics.TMetafile.Create;
        try
          Wmf.LoadFromStream(AStream);
          Picture.Assign(Wmf);
        finally
          FreeAndNil(Wmf);
        end;
      end;
    ******************************)
    grICO:
      begin
        Ico := Graphics.TIcon.Create;
        try
          Ico.LoadFromStream(AStream);
          Picture.Assign(Ico);
        finally
          FreeAndNil(Ico);
        end;
      end;
  end;
end;

procedure TJvThumbImage.SaveToStream(AStream: TStream; AType: TGRFKind);
var
  Bmp: Graphics.TBitmap;
  Jpg: TJpegImage;
  Ico: TIcon;
  (******************** NOT CONVERTED ***
  Wmf: TMetafile;
  **************************************)
begin
  //testing the stream Save capabilities;
  // (rom) deactivated because SaveToStream is not defined that way
  //AStream.Seek(0, soFromBeginning); //most of the stream error are generated because this is not at the proper position
  case AType of
    grBMP:
      begin
        Bmp := Graphics.TBitmap.Create;
        // (rom) secured
        try
          Bmp.Assign(Picture.Graphic);
          Bmp.PixelFormat := pf24bit;
          Bmp.SaveToStream(AStream);
        finally
          FreeAndNil(Bmp);
        end;
      end;
    grJPG:
      begin
        Jpg := TJpegImage.Create;
        try
          Jpg.Assign(Picture.Graphic);
          Jpg.SaveToStream(AStream);
        finally
          FreeAndNil(Jpg);
        end;
      end;
    (******************************* NOT CONVERTED ***
    grWMF, grEMF:
      begin
        Wmf := Graphics.TMetafile.Create;
        try
          Wmf.Assign(Picture.Graphic);
          Wmf.SaveToStream(AStream);
        finally
          FreeAndNil(Wmf);
        end;
      end;
    **********************************************)
    grICO:
      begin
        Ico := Graphics.TIcon.Create;
        try
          Ico.Assign(Picture.Graphic);
          Ico.SaveToStream(AStream);
        finally
          FreeAndNil(Ico);
        end;
      end;
  end;
end;

procedure TJvThumbImage.LoadFromFile(AFile: string);
var
  JpegImage: TJpegImage;
  Fl: TFileStream;
begin
  try
    if UpperCase(ExtractFileExt(AFile)) = '.JPG' then
    begin
      JpegImage := TJpegImage.Create;

      {************************ NOT CONVERTED *************
      if Parent is TJvThumbnail then
      begin
        Fl := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
        // (rom) this is idiotic
        try
          case Fl.Size of
            0..1000000:
              JpegImage.Scale := jsFullSize;
            1000001..4000000:
              JpegImage.Scale := jsHalf;
            4000001..7000000:
              JpegImage.Scale := jsQuarter;
          else
            JpegImage.Scale := jsEighth;
          end;
        finally
          Fl.Free;
        end;
      end
      else
        JpegImage.Scale := jsFullSize;
      *********************************************************}
      JpegImage.LoadFromFile(AFile);
      // Picture.Bitmap := Graphics.TBitmap.Create;
      with Picture.Bitmap do
      begin
        Width := JpegImage.Width;
        Height := JpegImage.Height;
        Picture.Bitmap.Canvas.Draw(0, 0, JpegImage);
        Self.FClass := TJpegImage;
      end;
      FreeAndNil(JpegImage);
    end
    else
    begin
      try
        Picture.LoadFromFile(AFile);
      except
        if Assigned(FOnInvalidImage) then
        begin
          FOnInvalidImage(Self, AFile);
          Exit;
        end
        else
          raise;
      end;
      Self.FClass := TGraphicClass(Picture.Graphic.ClassType);
    end;
    FFileName := AFile;
    FAngle := AT0;
    if Assigned(FOnLoad) then
      FOnLoad(Self);
  except
    on E: Exception do
    begin
      FFileName := '';
      Self.FClass := nil;
      raise;
    end;
  end;
end;

procedure TJvThumbImage.SaveToFile(AFile: string);
var
  Ext: string;
  Jpg: TJpegImage;
  Bmp: TBitmap;
  png: TPortableNetworkGraphic;
  {*************** NOT CONVERTED ***
  Wmf: TMetafile;
  ********************************}
begin
  // (rom) enforcing a file extension is bad style
  Ext := LowerCase(ExtractFileExt(AFile));
  if (Ext = '.jpg') or (Ext = '.jpeg') then
    try
      Jpg := TJpegImage.Create;
      Jpg.Assign(Picture.Graphic);
      Jpg.CompressionQuality := 75;
      { *************** NOT CONVERTED ***
      Jpg.Compress;
      **********************************}
      Jpg.SaveToFile(AFile);
    finally
      Jpg.Free;
    end
  else
  if Ext = '.bmp' then
    try
      Bmp := Graphics.TBitmap.Create;
      Bmp.Assign(Picture.Graphic);
      Bmp.Canvas.Draw(0, 0, Picture.Graphic);
      Bmp.SaveToFile(AFile);
    finally
      Bmp.Free;
    end
  else
  if Ext = '.png' then
    try
      png := TPortableNetworkGraphic.Create;
      png.Assign(Picture.Graphic);
      png.Canvas.Draw(0, 0, Picture.Graphic);
      png.SaveToFile(AFile);
    finally
      png.Free;
    end
  { ********************** NOT CONVERTED ***
  else
  if Ext = '.WMF' then
    try
      Wmf := TMetafile.Create;
      Wmf.Assign(Picture.Graphic);
      Wmf.Enhanced := False;
      Wmf.SaveToFile(AFile);
    finally
      FreeAndNil(Wmf);
    end
  else
  if Ext = '.EMF' then
    try
      Wmf := Graphics.TMetafile.Create;
      Wmf.Assign(Picture.Graphic);
      Wmf.Enhanced := True;
      Wmf.SaveToFile(AFile);
    finally
      FreeAndNil(Wmf);
    end
    ***************************************}
  else
    raise EJVCLException.CreateResFmt(@RsEUnknownFileExtension, [Ext]);
end;

procedure TJvThumbImage.Save;
var
  Temp: TGraphic;
begin
  if FClass <> nil then
  begin
    Temp := FClass.Create;
    Temp.Assign(Self.Picture.Graphic);
    Temp.SaveToFile(FFileName);
    FreeAndNil(Temp);
  end
  else
    SaveToFile(FFileName);
end;

procedure TJvThumbImage.BitmapNeeded;
var
  Bmp: Graphics.TBitmap;
begin
  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    //    Bmp.PixelFormat := pf24Bit;
    //    Bmp.Width := Picture.Graphic.Width;
    //    Bmp.Height := Picture.Graphic.Height;
    //    Bmp.Canvas.Draw(0,0,Picture.Graphic);
    Bmp.Assign(Picture.Graphic);
    Picture.Graphic.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TJvThumbImage.ScaleDown(MaxW, MaxH: Longint);
var
  NewSize: TPoint;
  Bmp: Graphics.TBitmap;
begin
  NewSize := ProportionalSize(Point(Picture.Width, Picture.Height), Point(MaxW, MaxH));
  if (NewSize.X > Picture.Width) and (NewSize.Y > Picture.Height) then
    Exit;
  // SomeTimes when the resize is bigger than 1600% then the strechDraw
  // doesn't produce any results at all so do it more than once to make
  // absolutly sure the will have an image in any case.
  if ((Picture.Width div NewSize.X) > 16) or ((Picture.Height div NewSize.Y) > 16) then
    ScaleDown(2 * MaxW, 2 * MaxH);
  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.Width := NewSize.X;
    Bmp.Height := NewSize.Y;
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Canvas.StretchDraw(Rect(0, 0, Bmp.Width, Bmp.Height), Picture.Graphic);
    Picture.Assign(Bmp);
    { wp
    Picture.Bitmap.Dormant;
    Picture.Bitmap.FreeImage;
    }
  finally
    FreeAndNil(Bmp);
  end;
  FModified := True;
end;

function TJvThumbImage.GetModify: Boolean;
begin
  Result := False;
  if not Assigned(Picture) or not Assigned(Picture.Graphic) then
    Exit;
  if Picture.Graphic.Empty then
    Result := False
  { ********************* NOT CONVERTED *************
  else
  if Picture.Graphic is Graphics.TMetafile then
    Result := False
  *************************************************}
  else
    Result := not (Picture.Graphic is Graphics.TIcon);
end;

procedure TJvThumbImage.GrayScale;
begin
  Transform(@GrayscaleProc);
end;

procedure TJvThumbImage.Invert;
begin
  Transform(@InvertProc);
end;

{ This procedure substitutes the values of R,G,B acordinally to the arrays the
  user passes in it. This is the simplest way to change the curve of a Color
  depending on an algorithm created by the user.
  The substitute value of a red 0 is the value which lies in the R[0] position.
  for a simple example have a look at the invert procedure above. }
procedure TJvThumbImage.ChangeRGBCurves(R, G, B: TCurveArray);
begin
  Transform(@RGBCurveProc, @R, @G, @B);
end;
(*
var
  MemBmp: TBitmap;
  Row, Col: Word;
  IntfImg: TLazIntfImage;
  clr: TColor;
  cr, cg, cb: Byte;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  if CanModify then
  begin
    IntfImg := TLazIntfImage.Create(0, 0);
    MemBmp := TBitmap.Create;
    try
      MemBmp.PixelFormat := pf32bit;
      MemBmp.SetSize(Picture.Width, Picture.Height);
      MemBmp.Canvas.Brush.Color := clWhite;
      MemBmp.Canvas.FillRect(0, 0, MemBmp.Width, MemBmp.Height);;
      MemBmp.Assign(Picture);
      IntfImg.LoadFromBitmap(MemBmp.Handle, MemBmp.MaskHandle);
      for Row := 0 to IntfImg.Height-1 do
        for Col := 0 to IntfImg.Width - 1 do begin
          clr := IntfImg.TColors[Col, Row];
          cr := R[GetRValue(clr)];
          cg := G[GetGValue(clr)];
          cb := B[GetBValue(clr)];
          IntfImg.TColors[Col, Row] := RGBToColor(cr, cg, cb);
        end;
      IntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle);
      MemBmp.Handle := ImgHandle;
      MemBmp.MaskHandle := ImgMaskHandle;
      if Picture.Graphic is TJpegImage then
        TJpegImage(Picture.Graphic).Assign(MemBmp)
      else if Picture.Graphic is Graphics.TBitmap then
        Picture.Bitmap.Assign(MemBmp);
      Invalidate;
    finally
      MemBmp.Free;
      IntfImg.Free;
    end;
  end;
end;
*)

procedure TJvThumbImage.Mirror(MirrorType: TMirror);
begin
  if Assigned(Picture.Graphic) and CanModify then begin
    case MirrorType of
      mtHorizontal: Transform(@MirrorHorProc);
      mtVertical  : Transform(@MirrorVertProc);
      mtBoth      : Transform(@Rotate180Proc);
    end;
    Invalidate;
    {
    RotateByDelta(ord(AAngle) - ord(FAngle));
    FAngle := AAngle;
    FModified := FAngle <> AT0;
    }
  end;
end;

{ Just a simple procedure to increase or decrease the values of the each channel
  in the image independendly from each other. E.g., lets say the R,G,B variables
  have the values of 5, -3, 7. This means that the red channel should be
  increased by 5 points in the entire image, the green value will be decreased
  by 3 points and the blue value will be increased by 7 points.
  This will happen to the entire image by the same value. Color luminosity is
  not preserved or values calculations depending on the current channel values. }
procedure TJvThumbImage.ChangeRGB(R, G, B: Longint);
begin
  Transform(@RGBProc, Pointer(PtrUInt(R)), Pointer(PtrUInt(G)), Pointer(PtrUInt(B)));
end;

{ General bitmap transformation method using LazIntfImages. The operation is
  specified by the procedure pointer TransformProc. }
procedure TJvThumbImage.Transform(TransformProc: TJvTransformProc;
  ARedData: Pointer = nil; AGreenData: Pointer = nil; ABlueData: Pointer = nil);
var
  IntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  if Assigned(Picture.Graphic) and CanModify then begin
    IntfImg := TPortableNetworkGraphic(Picture.Graphic).CreateIntfImage;
    try
      TransformProc(IntfImg, ARedData, AGreenData, ABlueData);
      IntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle);
      Picture.Bitmap.LoadFromIntfImage(IntfImg);
      FModified := true;
    finally
      IntfImg.Free;
    end;
  end;
end;

{ General bitmap transformation method using LazIntfImages. The operation is
  specified by the procedure pointer TransformProc. }
procedure TJvThumbImage.Transform(TransformProc: TJvTransformProc2;
  ARedData: Pointer = nil; AGreenData: Pointer = nil; ABlueData: Pointer = nil);
var
  SrcIntfImg, DestIntfImg: TLazIntfImage;
  DestImgHandle, DestImgMaskHandle: HBitmap;
begin
  if Assigned(Picture.Graphic) and CanModify then begin
    SrcIntfImg := TPortableNetworkGraphic(Picture.Graphic).CreateIntfImage;
    DestIntfImg := TPortableNetworkGraphic(Picture.Graphic).CreateIntfImage;
    try
      TransformProc(SrcIntfImg, DestIntfImg, ARedData, AGreenData, ABlueData);
      DestIntfImg.CreateBitmaps(DestImgHandle, DestImgMaskHandle);
      Picture.Bitmap.LoadFromIntfImage(DestIntfImg);
      FModified := true;
    finally
      DestIntfImg.Free;
      SrcIntfImg.Free;
    end;
  end;
end;

{ Procedure to actually decide what should be the rotation in conjuction with the
  image's physical Angle}
procedure TJvThumbImage.SetAngle(AAngle: TAngle);

  procedure RotateByDelta(ADiff: integer);
  begin
    if ADiff < 0 then inc(ADiff, 4);
    case TAngle(ADiff mod 4) of
      AT90:
        begin
          Transform(@Rotate90Proc);
          if Parent is TJvThumbnail then
            SendMessage(TJvThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
        end;
      AT180:
        Transform(@Rotate180Proc);
      AT270:
        begin
          Transform(@Rotate270Proc);
          if Parent is TJvThumbnail then
            SendMessage(TJvThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
        end;
    end;
  end;

begin
  if Assigned(Picture.Graphic) and CanModify and (AAngle <> FAngle) then begin
    RotateByDelta(ord(AAngle) - ord(FAngle));
    FAngle := AAngle;
    FModified := FAngle <> AT0;
  end;
end;


end.
