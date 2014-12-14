{ Graphic functions for pyramidtiff.

  Copyright (C) 2008  Mattias Gaertner  mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit PyTiGraphics;

{$mode objfpc}{$H+}

{$inline on}

interface

uses
  Math, sysutils, Classes, FPimage,
  LazLogger, FPCanvas, FPWriteTiff, FPTiffCmn;

type
  TCreateCompatibleMemImgEvent = procedure(Sender: TObject; Img: TFPCustomImage;
          NewWidth, NewHeight: integer; out NewImage: TFPCustomImage) of object;

  { TLinearInterpolation }

  TLinearInterpolation = class(TFPCustomInterpolation)
  private
    procedure CreatePixelWeights(OldSize, NewSize: integer;
      out Entries: Pointer; out EntrySize: integer; out Support: integer);
  protected
    procedure Execute(x,y,w,h: integer); override;
    function Filter(x: double): double; virtual;
    function MaxSupport: double; virtual;
  end;

procedure SetFPImgExtraTiff(const Desc: TFPCompactImgDesc; Img: TFPCustomImage;
  ClearTiffExtras: boolean);

function dbgs(const Desc: TFPCompactImgDesc): string; overload;

implementation

procedure SetFPImgExtraTiff(const Desc: TFPCompactImgDesc; Img: TFPCustomImage;
  ClearTiffExtras: boolean);
begin
  if ClearTiffExtras then
    FPTiffCmn.ClearTiffExtras(Img);
  if Desc.Gray then begin
    Img.Extra[TiffPhotoMetric]:='1';
    Img.Extra[TiffGrayBits]:=IntToStr(Desc.Depth);
  end else begin
    Img.Extra[TiffPhotoMetric]:='2';
    Img.Extra[TiffRedBits]:=IntToStr(Desc.Depth);
    Img.Extra[TiffGreenBits]:=IntToStr(Desc.Depth);
    Img.Extra[TiffBlueBits]:=IntToStr(Desc.Depth);
  end;
  if Desc.HasAlpha then
    Img.Extra[TiffAlphaBits]:=IntToStr(Desc.Depth)
  else
    Img.Extra[TiffAlphaBits]:='0';
end;

function dbgs(const Desc: TFPCompactImgDesc): string;
begin
  Result:='Depth='+dbgs(Desc.Depth)
    +',Gray='+dbgs(Desc.Gray)
    +',HasAlpha='+dbgs(Desc.HasAlpha);
end;

function ColorRound (c : double) : word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;

{ TLinearInterpolation }

procedure TLinearInterpolation.CreatePixelWeights(OldSize, NewSize: integer;
  out Entries: Pointer; out EntrySize: integer; out Support: integer);
// create an array of #NewSize entries. Each entry starts with an integer
// for the StartIndex, followed by #Support singles for the pixel weights.
// The sum of weights for each entry is 1.
var
  Entry: Pointer;

  procedure SetSupport(NewSupport: integer);
  begin
    Support:=NewSupport;
    EntrySize:=SizeOf(integer)+SizeOf(Single)*Support;
    Getmem(Entries,EntrySize*NewSize);
    Entry:=Entries;
  end;

var
  i: Integer;
  Factor: double;
  StartPos: Double;
  StartIndex: Integer;
  j: Integer;
  FirstValue: Double;
  //Sum: double;
begin
  if NewSize=OldSize then begin
    SetSupport(1);
    for i:=0 to NewSize-1 do begin
      // 1:1
      PInteger(Entry)^:=i;
      inc(Entry,SizeOf(Integer));
      PSingle(Entry)^:=1.0;
      inc(Entry,SizeOf(Single));
    end;
  end else if NewSize<OldSize then begin
    // shrink
    SetSupport(Max(2,(OldSize+NewSize-1) div NewSize));
    Factor:=double(OldSize)/double(NewSize);
    for i:=0 to NewSize-1 do begin
      StartPos:=Factor*i;
      StartIndex:=Floor(StartPos);
      PInteger(Entry)^:=StartIndex;
      inc(Entry,SizeOf(Integer));
      // first pixel
      FirstValue:=(1.0-(StartPos-double(StartIndex)));
      PSingle(Entry)^:=FirstValue/Factor;
      inc(Entry,SizeOf(Single));
      // middle pixel
      for j:=1 to Support-2 do begin
        PSingle(Entry)^:=1.0/Factor;
        inc(Entry,SizeOf(Single));
      end;
      // last pixel
      PSingle(Entry)^:=(Factor-FirstValue-(Support-2))/Factor;
      inc(Entry,SizeOf(Single));
    end;
  end else begin
    // enlarge
    if OldSize=1 then begin
      SetSupport(1);
      for i:=0 to NewSize-1 do begin
        // nothing to interpolate
        PInteger(Entry)^:=0;
        inc(Entry,SizeOf(Integer));
        PSingle(Entry)^:=1.0;
        inc(Entry,SizeOf(Single));
      end;
    end else begin
      SetSupport(2);
      Factor:=double(OldSize-1)/double(NewSize);
      for i:=0 to NewSize-1 do begin
        StartPos:=Factor*i+Factor/2;
        StartIndex:=Floor(StartPos);
        PInteger(Entry)^:=StartIndex;
        inc(Entry,SizeOf(Integer));
        // first pixel
        FirstValue:=(1.0-(StartPos-double(StartIndex)));
        // convert linear distribution
        FirstValue:=Min(1.0,Max(0.0,Filter(FirstValue/MaxSupport)));
        PSingle(Entry)^:=FirstValue;
        inc(Entry,SizeOf(Single));
        // last pixel
        PSingle(Entry)^:=1.0-FirstValue;
        inc(Entry,SizeOf(Single));
      end;
    end;
  end;
  if Entry<>Entries+EntrySize*NewSize then
    raise Exception.Create('TSimpleInterpolation.Execute inconsistency');

  {WriteLn('CreatePixelWeights Old=',OldSize,' New=',NewSize,' Support=',Support,' EntrySize=',EntrySize,' Factor=',FloatToStr(Factor));
  Entry:=Entries;
  for i:=0 to NewSize-1 do begin
    StartIndex:=PInteger(Entry)^;
    inc(Entry,SizeOf(Integer));
    write(i,' Start=',StartIndex);
    Sum:=0;
    for j:=0 to Support-1 do begin
      FirstValue:=PSingle(Entry)^;
      inc(Entry,SizeOf(Single));
      write(' ',FloatToStr(FirstValue));
      Sum:=Sum+FirstValue;
    end;
    writeln(' Sum=',FloatToStr(Sum));
  end;}
end;

procedure TLinearInterpolation.Execute(x, y, w, h: integer);
// paint Image on Canvas at x,y,w*h
var
  dy: Integer;
  dx: Integer;
  HorzResized: PFPColor;
  xEntries: Pointer; // size:integer,weight1:single,weight2:single,...
  xEntrySize: integer;
  xSupport: integer;// how many horizontal pixel are needed to created one new pixel
  yEntries: Pointer; // size:integer,weight1:single,weight2:single,...
  yEntrySize: integer;
  ySupport: integer;// how many vertizontal pixel are needed to created one new pixel
  NewSupportLines: LongInt;
  yEntry: Pointer;
  SrcStartY: LongInt;
  LastSrcStartY: LongInt;
  sy: Integer;
  xEntry: Pointer;
  sx: LongInt;
  cx: Integer;
  f: Single;
  NewCol: TFPColor;
  Col: TFPColor;
  CurEntry: Pointer;
  NewRed, NewGreen, NewBlue, NewAlpha: Single;
begin
  //WriteLn('TSimpleInterpolation.Execute Src=',image.width,'x',image.Height,' Dest=',x,',',y,',',w,'x',h);
  if (w<=0) or (h<=0) or (image.Width=0) or (image.Height=0) then exit;

  xEntries:=nil;
  yEntries:=nil;
  HorzResized:=nil;
  try
    CreatePixelWeights(image.Width,w,xEntries,xEntrySize,xSupport);
    CreatePixelWeights(image.Height,h,yEntries,yEntrySize,ySupport);
    //WriteLn('TSimpleInterpolation.Execute xSupport=',xSupport,' ySupport=',ySupport);
    // create temporary buffer for the horizontally resized pixel for the current
    // y line
    GetMem(HorzResized,w*ySupport*SizeOf(TFPColor));

    SrcStartY:=0;
    for dy:=0 to h-1 do begin
      if dy=0 then begin
        yEntry:=yEntries;
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=ySupport;
      end else begin
        LastSrcStartY:=SrcStartY;
        inc(yEntry,yEntrySize);
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=SrcStartY-LastSrcStartY;
        //WriteLn('TSimpleInterpolation.Execute dy=',dy,' SrcStartY=',SrcStartY,' LastSrcStartY=',LastSrcStartY,' NewSupportLines=',NewSupportLines);
        // move lines up
        if (NewSupportLines>0) and (ySupport>NewSupportLines) then
          System.Move(HorzResized[NewSupportLines*w],
                      HorzResized[0],
                      (ySupport-NewSupportLines)*w*SizeOf(TFPColor));
      end;

      // compute new horizontally resized line(s)
      for sy:=ySupport-NewSupportLines to ySupport-1 do begin
        xEntry:=xEntries;
        for dx:=0 to w-1 do begin
          sx:=PInteger(xEntry)^;
          inc(xEntry,SizeOf(integer));
          NewRed:=0.0;
          NewGreen:=0.0;
          NewBlue:=0.0;
          NewAlpha:=0.0;
          for cx:=sx to sx+xSupport-1 do begin
            f:=PSingle(xEntry)^;
            inc(xEntry,SizeOf(Single));
            Col:=image.Colors[cx,SrcStartY+sy];
            NewRed:=NewRed+Col.red*f;
            NewGreen:=NewGreen+Col.green*f;
            NewBlue:=NewBlue+Col.blue*f;
            NewAlpha:=NewAlpha+Col.alpha*f;
          end;
          NewCol.red:=Min(round(NewRed),$ffff);
          NewCol.green:=Min(round(NewGreen),$ffff);
          NewCol.blue:=Min(round(NewBlue),$ffff);
          NewCol.alpha:=Min(round(NewAlpha),$ffff);
          HorzResized[dx+sy*w]:=NewCol;
        end;
      end;

      // compute new vertically resized line
      for dx:=0 to w-1 do begin
        CurEntry:=yEntry+SizeOf(integer);
        NewRed:=0.0;
        NewGreen:=0.0;
        NewBlue:=0.0;
        NewAlpha:=0.0;
        for sy:=0 to ySupport-1 do begin
          f:=PSingle(CurEntry)^;
          inc(CurEntry,SizeOf(Single));
          Col:=HorzResized[dx+sy*w];
          NewRed:=NewRed+Col.red*f;
          NewGreen:=NewGreen+Col.green*f;
          NewBlue:=NewBlue+Col.blue*f;
          NewAlpha:=NewAlpha+Col.alpha*f;
        end;
        NewCol.red:=Min(round(NewRed),$ffff);
        NewCol.green:=Min(round(NewGreen),$ffff);
        NewCol.blue:=Min(round(NewBlue),$ffff);
        NewCol.alpha:=Min(round(NewAlpha),$ffff);
        Canvas.Colors[x+dx,y+dy]:=NewCol;
      end;
    end;
  finally
    if xEntries<>nil then FreeMem(xEntries);
    if yEntries<>nil then FreeMem(yEntries);
    if HorzResized<>nil then FreeMem(HorzResized);
  end;
end;

function TLinearInterpolation.Filter(x: double): double;
begin
  Result:=x;
end;

function TLinearInterpolation.MaxSupport: double;
begin
  Result:=1.0;
end;

end.

