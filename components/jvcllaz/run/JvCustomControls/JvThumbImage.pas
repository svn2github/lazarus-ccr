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

  TJvTransformProc = procedure (ASourceIntfImage, ADestIntfImage: TLazIntfImage;
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
//    procedure Rotate90;
//    procedure Rotate270;
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
      AGreenData: Pointer = nil; ABlueData: Pointer = nil);
    procedure BitmapNeeded;
    //    Procedure FilterFactory(Filter: TFilterArray; Divider: Byte);
    procedure Invert;
    procedure Contrast(const Percent: TPercent);
    procedure Lightness(const Percent: TPercent);
    procedure Grayscale;
    procedure Rotate(AAngle: TAngle);
    function GetFilter: string;
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

procedure GrayScaleProc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c: Integer;
  clr: TColor;
  intens: Integer;
begin
  for r := 0 to ASrcImg.Height-1 do
    for c := 0 to ASrcImg.Width-1 do begin
      clr := ASrcImg.TColors[c, r];
      intens := (GetRValue(clr) + GetGValue(clr) + GetBValue(clr)) div 3;
      ADestImg.TColors[c, r] := RGBToColor(intens, intens, intens);
    end;
end;

procedure InvertProc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c: Integer;
  clr: TColor;
begin
  for r := 0 to ASrcImg.Height - 1 do
    for c := 0 to ASrcImg.Width - 1 do begin
      clr := ASrcImg.TColors[c, r];
      ADestImg.TColors[c, r] := RGBToColor(255 - GetRValue(clr), 255 - GetGValue(clr), 255 - GetBValue(clr));
    end;
end;

procedure Rotate90Proc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c, w, h: Integer;
  clr: TColor;
begin
  w := ASrcImg.Width;
  h := ASrcImg.Height;
  ADestImg.SetSize(h, w);
  for r := 0 to h - 1 do
    for c := 0 to w - 1 do begin
      clr := ASrcImg.TColors[c, r];
      ADestImg.TColors[r, w-1-c] := clr;
    end;
end;

procedure Rotate270Proc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c, w, h: Integer;
  clr: TColor;
begin
  w := ASrcImg.Width;
  h := ASrcImg.Height;
  ADestImg.SetSize(h, w);
  for r := 0 to h - 1 do
    for c := 0 to w - 1 do begin
      clr := ASrcImg.TColors[c, r];
      ADestImg.TColors[h-1-r, c] := clr;
    end;
end;

procedure RGBProc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c: Integer;
  clr: TColor;
  rVal, gVal, bVal: Byte;
  deltaR, deltaG, deltaB: Integer;
begin
  deltaR := PtrUInt(ARedData);
  deltaG := PtrUInt(AGreenData);
  deltaB := PtrUInt(ABlueData);
  for r := 0 to ASrcImg.Height - 1 do
    for c := 0 to ASrcImg.Width - 1 do begin
      clr := ASrcImg.TColors[c, r];
      rVal := BoundByte(0, 255, GetBValue(clr) + deltaR);
      gVal := BoundByte(0, 255, GetGValue(clr) + deltaG);
      bVal := BoundByte(0, 255, GetBValue(clr) + deltaB);
      ADestImg.TColors[c, r] := RGBToColor(rVal, gVal, bVal);
    end;
end;

procedure RGBCurveProc(ASrcImg, ADestImg: TLazIntfImage;
  ARedData, AGreenData, ABlueData: Pointer);
var
  r, c: Integer;
  clr: TColor;
  rVal, gVal, bVal: Byte;
begin
  for r := 0 to ASrcImg.Height - 1 do
    for c := 0 to ASrcImg.Width - 1 do begin
      clr := ASrcImg.TColors[c, r];
      rVal := TCurveArray(ARedData^)[GetRValue(clr)];
      gVal := TCurveArray(AGreenData^)[GetGValue(clr)];
      bVal := TCurveArray(ABlueData^)[GetBValue(clr)];
      ADestImg.TColors[c, r] := RGBToColor(rVal, gVal, bVal);
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
      Mirror(mtBoth);
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
  {*************** NOT CONVERTED ***
  Wmf: TMetafile;
  ********************************}
begin
  // (rom) enforcing a file extension is bad style
  Ext := UpperCase(ExtractFileExt(AFile));
  if (Ext = '.JPG') or (Ext = '.JPEG') then
    try
      Jpg := TJpegImage.Create;
      Jpg.Assign(Picture.Graphic);
      Jpg.CompressionQuality := 75;
      { *************** NOT CONVERTED ***
      Jpg.Compress;
      **********************************}
      Jpg.SaveToFile(AFile);
    finally
      FreeAndNil(Jpg);
    end
  else
  if Ext = '.BMP' then
    try
      Bmp := Graphics.TBitmap.Create;
      Bmp.Assign(Picture.Graphic);
      Bmp.Canvas.Draw(0, 0, Picture.Graphic);
      Bmp.SaveToFile(AFile);
    finally
      FreeAndNil(Bmp);
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
var
  MemBmp: Graphics.TBitmap;
  Dest: TRect;
begin
  if Assigned(Picture.Graphic) then
    if CanModify then
    begin
      MemBmp := Graphics.TBitmap.Create;
      try
        MemBmp.PixelFormat := pf32bit;
        MemBmp.HandleType := bmDIB;
        MemBmp.Width := Self.Picture.Graphic.Width;
        MemBmp.Height := Self.Picture.Height;
        MemBmp.Canvas.Draw(0, 0, Picture.Graphic);
        case MirrorType of
          mtHorizontal:
            begin
              Dest.Left := MemBmp.Width;
              Dest.Top := 0;
              Dest.Right := -MemBmp.Width;
              Dest.Bottom := MemBmp.Height;
            end;
          mtVertical:
            begin
              Dest.Left := 0;
              Dest.Top := MemBmp.Height;
              Dest.Right := MemBmp.Width;
              Dest.Bottom := -MemBmp.Height;
            end;
          mtBoth:
            begin
              Dest.Left := MemBmp.Width;
              Dest.Top := MemBmp.Height;
              Dest.Right := -MemBmp.Width;
              Dest.Bottom := -MemBmp.Height;
            end;
        end;
        StretchBlt(MemBmp.Canvas.Handle, Dest.Left, Dest.Top, Dest.Right, Dest.Bottom,
          MemBmp.Canvas.Handle, 0, 0, MemBmp.Width, MemBmp.Height, SRCCOPY);
        Picture.Graphic.Assign(MemBmp);
        Invalidate;
      finally
        FreeAndNil(MemBmp);
      end;
    end;
end;

{ Just a simple procedure to increase or decrease the values of the each channel
  in the image idependendly from each other. E.G.
  lets say the R,G,B vars have the values of 5,-3,7 this means that the red
  channel should be increased buy 5 points in all the image the green value will
  be decreased by 3 points and the blue value will be increased by 7 points.
  This will happen to all the image by the same value no Color limunocity is
  been preserved or values calculations depenting on the current channel values. }
procedure TJvThumbImage.ChangeRGB(R, G, B: Longint);
begin
  Transform(@RGBProc, Pointer(PtrUInt(R)), Pointer(PtrUInt(G)), Pointer(PtrUInt(B)));
end;

procedure TJvThumbImage.Transform(TransformProc: TJvTransformProc;
  ARedData: Pointer = nil; AGreenData: Pointer = nil; ABlueData: Pointer = nil);
var
  Bmp: TBitmap;
  SrcIntfImg, DestIntfImg: TLazIntfImage;
  DestImgHandle, DestImgMaskHandle: HBitmap;
  w, h: Integer;
begin
  if Assigned(Picture.Graphic) then
    if CanModify then
    begin
      w := Picture.Width;
      h := Picture.Height;
      SrcIntfImg := TLazIntfImage.Create(0, 0);
      DestIntfImg := TLazIntfImage.Create(0, 0);
      Bmp := TBitmap.Create;
      try
        Bmp.PixelFormat := pf32bit;
        Bmp.SetSize(w, h);
        Bmp.Canvas.Brush.Color := clWhite;
        Bmp.Canvas.FillRect(0, 0, w, h);
        Bmp.Assign(Picture.Graphic);
        SrcIntfImg.LoadFromBitmap(Bmp.Handle, Bmp.MaskHandle);
        DestIntfImg.LoadFromBitmap(Bmp.Handle, Bmp.MaskHandle);
        TransformProc(SrcIntfImg, DestIntfImg, ARedData, AGreenData, ABlueData);
        DestIntfImg.CreateBitmaps(DestImgHandle, DestImgMaskHandle);
        Bmp.Handle := DestImgHandle;
        Bmp.MaskHandle := DestImgMaskHandle;
        Picture.Graphic.Clear;
        if Picture.Graphic is TJpegImage then
          TJpegImage(Picture.Graphic).Assign(Bmp)
        else if Picture.Graphic is Graphics.TBitmap then
          Picture.Bitmap.Assign(Bmp);
        Invalidate;
      finally
        Bmp.Free;
        SrcIntfImg.Free;
        DestIntfImg.Free;
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
        Mirror(mtBoth);
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
