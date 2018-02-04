unit SpkGUITools;

{$mode ObjFpc}
{$H+}
{$DEFINE SPKGUITOOLS}
{.$define EnhancedRecordSupport}
//the fpcbugworkaround is only necessary when using inline for DrawRoundRect
{.$define FpcBugWorkAround}

interface

{$MESSAGE HINT 'Every rect in this module are exact rectanges (not like in WINAPI without right and bottom)'}

uses
  LCLType, Graphics, SysUtils, Classes, Controls, StdCtrls, SpkGraphTools, SpkMath;

type
  TCornerPos = (cpLeftTop, cpRightTop, cpLeftBottom, cpRightBottom);
  TCornerKind = (cpRound, cpNormal);
  TBackgroundKind = (bkSolid, bkVerticalGradient, bkHorizontalGradient,
                    bkConcave);

  TSpkButtonState = (bsIdle,
    bsBtnHottrack, bsBtnPressed, bsDropdownHottrack, bsDropdownPressed);

  TSpkCheckboxStyle = (cbsCheckbox, cbsRadioButton);
//  TSpkCheckboxState = (cbsIdle, cbsHotTrack, cbsPressed, cbsDisabled);

  TGUITools = class(TObject)
  protected
    class procedure FillGradientRectangle(ACanvas: TCanvas; Rect: T2DIntRect;
      ColorFrom, ColorTo: TColor; GradientKind: TBackgroundKind);
    class procedure SaveClipRgn(DC: HDC; var OrgRgnExists: boolean; var OrgRgn: HRGN);
    class procedure RestoreClipRgn(DC: HDC; OrgRgnExists: boolean; var OrgRgn: HRGN);
  public
    // *** Lines ***

    // Performance:
    // w/ClipRect:  Bitmap is faster (2x)
    // wo/ClipRect: Canvas is faster (a little)
    class procedure DrawHLine(ABitmap : TBitmap;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor); overload;
    class procedure DrawHLine(ABitmap : TBitmap;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;
    class procedure DrawHLine(ACanvas : TCanvas;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor); overload;
    class procedure DrawHLine(ACanvas : TCanvas;
                             x1, x2 : integer;
                             y : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;


    // Performance:
    // w/ClipRect:  Bitmap is faster (2x)
    // wo/ClipRect: Canvas is faster (a little)
    class procedure DrawVLine(ABitmap : TBitmap;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor); overload;
    class procedure DrawVLine(ABitmap : TBitmap;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;
    class procedure DrawVLine(ACanvas : TCanvas;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor); overload;
    class procedure DrawVLine(ACanvas : TCanvas;
                             x : integer;
                             y1, y2 : integer;
                             Color : TColor;
                             ClipRect : T2DIntRect); overload;

    // *** Background and frame tools ***

    // Performance:
    // w/ClipRect:  Bitmap is faster (extremely)
    // wo/ClipRect: Bitmap is faster (extremely)
    class procedure DrawAARoundCorner(ABitmap : TBitmap;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor); overload;
    class procedure DrawAARoundCorner(ABitmap : TBitmap;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor;
                                     ClipRect : T2DIntRect); overload;
    class procedure DrawAARoundCorner(ACanvas : TCanvas;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor); overload;
    class procedure DrawAARoundCorner(ACanvas : TCanvas;
                                     Point : T2DIntVector;
                                     Radius : integer;
                                     CornerPos : TCornerPos;
                                     Color : TColor;
                                     ClipRect : T2DIntRect); overload;

    // Performance:
    // w/ClipRect:  Bitmap is faster (extremely)
    // wo/ClipRect: Bitmap is faster (extremely)
    class procedure DrawAARoundFrame(ABitmap : TBitmap;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor); overload;
    class procedure DrawAARoundFrame(ABitmap : TBitmap;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor;
                                    ClipRect : T2DIntRect); overload;
    class procedure DrawAARoundFrame(ACanvas : TCanvas;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor); overload;
    class procedure DrawAARoundFrame(ACanvas : TCanvas;
                                    Rect : T2DIntRect;
                                    Radius : integer;
                                    Color : TColor;
                                    ClipRect : T2DIntRect); overload;

    class procedure RenderBackground(ABuffer : TBitmap;
                                    Rect : T2DIntRect;
                                    Color1, Color2 : TColor;
                                    BackgroundKind : TBackgroundKind);

    class procedure CopyRoundCorner(ABuffer : TBitmap;
                                   ABitmap : TBitmap;
                                   SrcPoint : T2DIntVector;
                                   DstPoint : T2DIntVector;
                                   Radius : integer;
                                   CornerPos : TCornerPos;
                                   Convex : boolean = true); overload;
    class procedure CopyRoundCorner(ABuffer : TBitmap;
                                   ABitmap : TBitmap;
                                   SrcPoint : T2DIntVector;
                                   DstPoint : T2DIntVector;
                                   Radius : integer;
                                   CornerPos : TCornerPos;
                                   ClipRect : T2DIntRect;
                                   Convex : boolean = true); overload;

    class procedure CopyCorner(ABuffer : TBitmap;
                              ABitmap: TBitmap;
                              SrcPoint : T2DIntVector;
                              DstPoint: T2DIntVector;
                              Radius: integer); overload; inline;
    class procedure CopyCorner(ABuffer : TBitmap;
                              ABitmap: TBitmap;
                              SrcPoint : T2DIntVector;
                              DstPoint: T2DIntVector;
                              Radius: integer;
                              ClipRect : T2DIntRect); overload; inline;

    class procedure CopyRectangle(ABuffer : TBitmap;
                                 ABitmap: TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint: T2DIntVector;
                                 Width: integer;
                                 Height : integer); overload;
    class procedure CopyRectangle(ABuffer : TBitmap;
                                 ABitmap : TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint : T2DIntVector;
                                 Width : integer;
                                 Height : integer;
                                 ClipRect : T2DIntRect); overload;
    class procedure CopyMaskRectangle(ABuffer : TBitmap;
                                     AMask : TBitmap;
                                     ABitmap : TBitmap;
                                     SrcPoint : T2DIntVector;
                                     DstPoint : T2DIntVector;
                                     Width : integer;
                                     Height : integer); overload;
    class procedure CopyMaskRectangle(ABuffer : TBitmap;
                                     AMask : TBitmap;
                                     ABitmap : TBitmap;
                                     SrcPoint : T2DIntVector;
                                     DstPoint : T2DIntVector;
                                     Width : integer;
                                     Height : integer;
                                     ClipRect : T2DIntRect); overload;

    // Performance (RenderBackground + CopyRoundRect vs DrawRoundRect):
    // w/ClipRect  : Bitmap faster for smaller radiuses, Canvas faster for larger
    // wo/ClipRect : Bitmap faster for smaller radiuses, Canvas faster for larger
    class procedure CopyRoundRect(ABuffer : TBitmap;
                                 ABitmap : TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint : T2DIntVector;
                                 Width, Height : integer;
                                 Radius : integer;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;
    class procedure CopyRoundRect(ABuffer : TBitmap;
                                 ABitmap : TBitmap;
                                 SrcPoint : T2DIntVector;
                                 DstPoint : T2DIntVector;
                                 Width, Height : integer;
                                 Radius : integer;
                                 ClipRect : T2DIntRect;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;


    class procedure DrawRoundRect(ACanvas : TCanvas;
                                 Rect : T2DIntRect;
                                 Radius : integer;
                                 ColorFrom : TColor;
                                 ColorTo : TColor;
                                 GradientKind : TBackgroundKind;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;
    class procedure DrawRoundRect(ACanvas : TCanvas;
                                 Rect : T2DIntRect;
                                 Radius : integer;
                                 ColorFrom : TColor;
                                 ColorTo : TColor;
                                 GradientKind : TBackgroundKind;
                                 ClipRect : T2DIntRect;
                                 LeftTopRound : boolean = true;
                                 RightTopRound : boolean = true;
                                 LeftBottomRound : boolean = true;
                                 RightBottomRound : boolean = true); overload;

    class procedure DrawRegion(ACanvas : TCanvas;
                              Region : HRGN;
                              Rect : T2DIntRect;
                              ColorFrom : TColor;
                              ColorTo : TColor;
                              GradientKind : TBackgroundKind); overload;
    class procedure DrawRegion(ACanvas : TCanvas;
                              Region : HRGN;
                              Rect : T2DIntRect;
                              ColorFrom : TColor;
                              ColorTo : TColor;
                              GradientKind : TBackgroundKind;
                              ClipRect : T2DIntRect); overload;

    // Imagelist tools
    class procedure DrawImage(ABitmap : TBitmap;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector); overload; inline;
    class procedure DrawImage(ABitmap : TBitmap;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector;
                             ClipRect : T2DIntRect); overload; inline;
    class procedure DrawImage(ACanvas : TCanvas;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector); overload; inline;
    class procedure DrawImage(ACanvas : TCanvas;
                             Imagelist : TImageList;
                             ImageIndex : integer;
                             Point : T2DIntVector;
                             ClipRect : T2DIntRect); overload;

    class procedure DrawDisabledImage(ABitmap : TBitmap;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector); overload; inline;
    class procedure DrawDisabledImage(ABitmap : TBitmap;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector;
                                     ClipRect : T2DIntRect); overload; inline;
    class procedure DrawDisabledImage(ACanvas : TCanvas;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector); overload;
    class procedure DrawDisabledImage(ACanvas : TCanvas;
                                     Imagelist : TImageList;
                                     ImageIndex : integer;
                                     Point : T2DIntVector;
                                     ClipRect : T2DIntRect); overload; inline;

    // Checkbox
    class procedure DrawCheckbox(ACanvas: TCanvas;
                                 x,y: Integer;
                                 AState: TCheckboxState;
                                 AButtonState: TSpkButtonState;
                                 AStyle: TSpkCheckboxStyle); overload;
    class procedure DrawCheckbox(ACanvas: TCanvas;
                                 x,y: Integer;
                                 AState: TCheckboxState;
                                 AButtonState: TSpkButtonState;
                                 AStyle: TSpkCheckboxStyle;
                                 ClipRect: T2DIntRect); overload;

    // Text tools
    class procedure DrawText(ABitmap : TBitmap;
                        x, y : integer;
                        const AText : string;
                        TextColor: TColor); overload;
    class procedure DrawText(ABitmap : TBitmap;
                        x, y : integer;
                        const AText : string;
                        TextColor : TColor;
                        ClipRect: T2DIntRect); overload;
    class procedure DrawMarkedText(ACanvas : TCanvas;
                                  x, y : integer;
                                  const AText, AMarkPhrase : string;
                                  TextColor : TColor;
                                  CaseSensitive : boolean = false); overload;
    class procedure DrawMarkedText(ACanvas : TCanvas;
                                  x, y : integer;
                                  const AText, AMarkPhrase : string;
                                  TextColor : TColor;
                                  ClipRect : T2DIntRect;
                                  CaseSensitive : boolean = false); overload;
    class procedure DrawText(ACanvas : TCanvas;
                        x, y : integer;
                        const AText : string;
                        TextColor : TColor); overload;
    class procedure DrawText(ACanvas : TCanvas;
                        x, y : integer;
                        const AText : string;
                        TextColor : TColor;
                        ClipRect : T2DIntRect); overload;
    class procedure DrawFitWText(ABitmap : TBitmap;
                                x1, x2 : integer;
                                y : integer;
                                const AText : string;
                                TextColor : TColor;
                                Align : TAlignment); overload;
    class procedure DrawFitWText(ACanvas : TCanvas;
                                x1, x2 : integer;
                                y : integer;
                                const AText : string;
                                TextColor : TColor;
                                Align : TAlignment); overload;

    class procedure DrawOutlinedText(ABitmap : TBitmap;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor); overload;
    class procedure DrawOutlinedText(ABitmap : TBitmap;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor;
                                    ClipRect : T2DIntRect); overload;
    class procedure DrawOutlinedText(ACanvas : TCanvas;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor); overload;
    class procedure DrawOutlinedText(ACanvas : TCanvas;
                                    x, y : integer;
                                    const AText : string;
                                    TextColor : TColor;
                                    OutlineColor : TColor;
                                    ClipRect : T2DIntRect); overload;
    class procedure DrawFitWOutlinedText(ABitmap: TBitmap;
                                        x1, x2 : integer;
                                        y: integer;
                                        const AText: string;
                                        TextColor,
                                        OutlineColor: TColor;
                                        Align: TAlignment); overload;
    class procedure DrawFitWOutlinedText(ACanvas: TCanvas;
                                        x1, x2 : integer;
                                        y: integer;
                                        const AText: string;
                                        TextColor,
                                        OutlineColor: TColor;
                                        Align: TAlignment); overload;
end;

implementation

uses
 types, LCLIntf, IntfGraphics, Math, Themes;

{ TSpkGUITools }

class procedure TGUITools.CopyRoundCorner(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Radius: integer; CornerPos: TCornerPos;
  ClipRect: T2DIntRect; Convex: boolean);
var
  BufferRect, BitmapRect, TempRect: T2DIntRect;
  OrgSrcRect, UnClippedDstRect, OrgDstRect: T2DIntRect;
  SrcRect: T2DIntRect;
  Offset: T2DIntVector;
  Center: T2DIntVector;
  y: Integer;
  SrcLine: Pointer;
  DstLine: Pointer;
  SrcPtr, DstPtr : PByte;
  x: Integer;
  Dist : double;
  SrcImg, DestImg: TLazIntfImage;
begin
  if (ABuffer.PixelFormat <> pf24Bit) or (ABitmap.PixelFormat <> pf24Bit) then
    raise Exception.Create('TSpkGUITools.CopyRoundCorner: Only 24-bit bitmaps are accepted!');

  // Validation
  if Radius < 1 then
    exit;

  if (ABuffer.Width = 0) or (ABuffer.Height = 0) or
     (ABitmap.Width = 0) or (ABitmap.Height = 0) then exit;

  //todo minimize use of temps here
  {$ifdef EnhancedRecordSupport}
  BufferRect := T2DIntRect.Create(0, 0, ABuffer.width-1, ABuffer.height-1);
  if not BufferRect.IntersectsWith(
      T2DIntRect.Create(SrcPoint.x, SrcPoint.y, SrcPoint.x+Radius-1, SrcPoint.y+Radius-1),
      OrgSrcRect
    )
  then
    exit;
  {$else}
  BufferRect.Create(0, 0, ABuffer.Width-1, ABuffer.Height-1);
  TempRect.Create(SrcPoint.x, SrcPoint.y, SrcPoint.x+Radius-1, SrcPoint.y+Radius-1);
  if not BufferRect.IntersectsWith(TempRect, OrgSrcRect) then
    exit;
  {$endif}

  {$ifdef EnhancedRecordSupport}
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(
      T2DIntRect.Create(DstPoint.x, DstPoint.y, DstPoint.x+Radius-1, DstPoint.y+Radius-1),
      UnClippedDstRect
    )
  then
    exit;
  {$else}
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  //todo: calling create twice
  TempRect.Create(DstPoint.x, DstPoint.y, DstPoint.x+Radius-1, DstPoint.y+Radius-1);
  if not(BitmapRect.IntersectsWith(TempRect, UnClippedDstRect)) then
    exit;
  {$endif}

  if not(ClipRect.IntersectsWith(UnClippedDstRect, OrgDstRect)) then
    exit;

  Offset := DstPoint - SrcPoint;

  if not(OrgSrcRect.IntersectsWith(OrgDstRect - Offset, SrcRect)) then
    exit;

  // We position the center of the arc
  {$ifdef EnhancedRecordSupport}
  case CornerPos of
    cpLeftTop:
      Center := T2DIntVector.Create(SrcPoint.x + Radius - 1, SrcPoint.y + Radius - 1);
    cpRightTop:
      Center := T2DIntVector.Create(SrcPoint.x, SrcPoint.y + Radius - 1);
    cpLeftBottom:
      Center := T2DIntVector.Create(SrcPoint.x + Radius - 1, SrcPoint.y);
    cpRightBottom:
      Center := T2DIntVector.Create(SrcPoint.x, SrcPoint.y);
  end;
  {$else}
  case CornerPos of
    cpLeftTop:
      Center.Create(SrcPoint.x + Radius - 1, SrcPoint.y + Radius - 1);
    cpRightTop:
      Center.Create(SrcPoint.x, SrcPoint.y + Radius - 1);
    cpLeftBottom:
      Center.Create(SrcPoint.x + Radius - 1, SrcPoint.y);
    cpRightBottom:
      Center.Create(SrcPoint.x, SrcPoint.y);
  end;
  {$endif}

  // Is there anything to be processed?
  if Convex then
  begin
    //todo: remove the check since is not necessary
    if (SrcRect.Left <= SrcRect.right) and (SrcRect.Top <= SrcRect.Bottom) then
    begin
      SrcImg := ABuffer.CreateIntfImage;
      DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.Top to SrcRect.Bottom do
      begin
        SrcLine := SrcImg.GetDataLineStart(y);
        DstLine := DestImg.GetDataLineStart(y+Offset.y);
        SrcPtr := pointer(PtrInt(SrcLine) + 3*SrcRect.Left);
        DstPtr := pointer(PtrInt(DstLine) + 3*(SrcRect.Left + Offset.x));
        for x := SrcRect.Left to SrcRect.Right do
        begin
          {$ifdef EnhancedRecordSupport}
          Dist := Center.DistanceTo(T2DIntVector.Create(x, y));
          {$else}
          Dist := Center.DistanceTo(x, y);
          {$endif}
          if Dist <= (Radius-1) then
            Move(SrcPtr^, DstPtr^, 3);
          inc(SrcPtr, 3);
          inc(DstPtr, 3);
        end;
      end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
    end;
  end else
  begin
    if (SrcRect.Left <= SrcRect.Right) and (SrcRect.Top <= SrcRect.Bottom) then
    begin
      SrcImg := ABuffer.CreateIntfImage;
      DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.Top to SrcRect.Bottom do
      begin
        SrcLine := SrcImg.GetDataLineStart(y);
        DstLine := DestImg.GetDataLineStart(y+Offset.y);
        SrcPtr := pointer(PtrInt(SrcLine) + 3*SrcRect.Left);
        DstPtr := pointer(PtrInt(DstLine) + 3*(SrcRect.Left + Offset.x));
        for x := SrcRect.Left to SrcRect.Right do
        begin
          {$ifdef EnhancedRecordSupport}
          Dist := Center.DistanceTo(T2DIntVector.Create(x, y));
          {$else}
          Dist := Center.DistanceTo(x, y);
          {$endif}
          if Dist >= (Radius-1) then
            Move(SrcPtr^, DstPtr^, 3);
          inc(SrcPtr,3);
          inc(DstPtr,3);
        end;
      end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
    end;
  end;
end;

class procedure TGUITools.CopyRoundRect(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height, Radius: integer; ClipRect: T2DIntRect;
  LeftTopRound, RightTopRound, LeftBottomRound, RightBottomRound: boolean);
begin
  if (ABuffer.PixelFormat <> pf24Bit) or (ABitmap.PixelFormat <> pf24Bit) then
   raise Exception.Create('TSpkGUITools.CopyBackground: Only 24 bit bitmaps are accepted!');

  if Radius < 0 then
    exit;

  if (Radius > Width div 2) or (Radius > Height div 2) then
    exit;

  if (ABuffer.Width = 0) or (ABuffer.Height = 0) or
     (ABitmap.Width = 0) or (ABitmap.Height = 0) then exit;

  {$REGION 'We fill the rectangles'}
  // Mountain /????
  // Góra
  CopyRectangle(ABuffer,
               ABitmap,
               {$ifdef EnhancedRecordSupport}
               T2DIntPoint.Create(SrcPoint.x + Radius, SrcPoint.y),
               T2DIntPoint.Create(DstPoint.x + Radius, DstPoint.y),
               {$else}
               Create2DIntPoint(SrcPoint.x + Radius, SrcPoint.y),
               Create2DIntPoint(DstPoint.x + Radius, DstPoint.y),
               {$endif}
               Width - 2*Radius,
               Radius,
               ClipRect);
  // Down
  // Dó³
  CopyRectangle(ABuffer,
               ABitmap,
               {$IFDEF EnhancedRecordSupport}
               T2DIntPoint.Create(SrcPoint.x + Radius, SrcPoint.y + Height - Radius),
               T2DIntPoint.Create(DstPoint.x + Radius, DstPoint.y + Height - Radius),
               {$ELSE}
               Create2DIntPoint(SrcPoint.x + Radius, SrcPoint.y + Height - Radius),
               Create2DIntPoint(DstPoint.x + Radius, DstPoint.y + Height - Radius),
               {$ENDIF}
               Width - 2*Radius,
               Radius,
               ClipRect);
  // Agent (???)
  // Œrodek
  CopyRectangle(ABuffer,
               ABitmap,
               {$IFDEF EnhancedRecordSupport}
               T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Radius),
               T2DIntPoint.Create(DstPoint.x, DstPoint.y + Radius),
               {$ELSE}
               Create2DIntPoint(SrcPoint.x, SrcPoint.y + Radius),
               Create2DIntPoint(DstPoint.x, DstPoint.y + Radius),
               {$ENDIF}
               Width,
               Height - 2*Radius,
               ClipRect);
{$ENDREGION}

  // We fill the corners
{$REGION 'Left upper'}
  if LeftTopRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                              T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                              Create2DIntPoint(DstPoint.x, DstPoint.y),
                              {$ENDIF}
                              Radius,
                              cpLeftTop,
                              ClipRect,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                         T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                         Create2DIntPoint(DstPoint.x, DstPoint.y),
                         {$ENDIF}
                         Radius,
                         ClipRect);
{$ENDREGION}

{$REGION 'Right upper'}
  if RightTopRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                              T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                              Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                              {$ENDIF}
                              Radius,
                              cpRightTop,
                              ClipRect,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                         T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                         Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                         {$ENDIF}
                         Radius,
                         ClipRect);
{$ENDREGION}

{$REGION 'Left bottom'}
  if LeftBottomRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                              T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                              Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                              {$ENDIF}
                              Radius,
                              cpLeftBottom,
                              ClipRect,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                         T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                         Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                         {$ENDIF}
                         Radius,
                         ClipRect);
{$ENDREGION}

{$REGION 'Right bottom'}
  if RightBottomRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                              T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                              Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                              {$ENDIF}
                              Radius,
                              cpRightBottom,
                              ClipRect,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                         T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                         Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                         {$ENDIF}
                         Radius,
                         ClipRect);
{$ENDREGION'}
end;

class procedure TGUITools.CopyRoundRect(ABuffer : TBitmap; ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height, Radius: integer; LeftTopRound,
  RightTopRound, LeftBottomRound, RightBottomRound: boolean);
begin
  if (ABuffer.PixelFormat <> pf24bit) or (ABitmap.PixelFormat <> pf24bit) then
    raise exception.create('TSpkGUITools.CopyBackground: Only 24 bit bitmaps are accepted!');

  if Radius < 0 then
    exit;

  if (Radius > Width div 2) or (Radius > Height div 2) then
    exit;

  if (ABuffer.Width = 0) or (ABuffer.Height = 0) or
     (ABitmap.Width = 0) or (ABitmap.Height = 0) then exit;

{$REGION 'We fill the rectangles'}
  // Góra
  CopyRectangle(ABuffer,
                ABitmap,
                {$IFDEF EnhancedRecordSupport}
                T2DIntPoint.Create(SrcPoint.x + Radius, SrcPoint.y),
                T2DIntPoint.Create(DstPoint.x + Radius, DstPoint.y),
                {$ELSE}
                Create2DIntPoint(SrcPoint.x + Radius, SrcPoint.y),
                Create2DIntPoint(DstPoint.x + Radius, DstPoint.y),
                {$ENDIF}
                Width - 2*Radius,
                Radius);
  // Dó³
  CopyRectangle(ABuffer,
                ABitmap,
                {$IFDEF EnhancedRecordSupport}
                T2DIntPoint.Create(SrcPoint.x + Radius, SrcPoint.y + Height - radius),
                T2DIntPoint.Create(DstPoint.x + Radius, DstPoint.y + Height - radius),
                {$ELSE}
                Create2DIntPoint(SrcPoint.x + Radius, SrcPoint.y + Height - radius),
                Create2DIntPoint(DstPoint.x + Radius, DstPoint.y + Height - radius),
                {$ENDIF}
                Width - 2*Radius,
                Radius);
  // Œrodek
  CopyRectangle(ABuffer,
               ABitmap,
               {$IFDEF EnhancedRecordSupport}
               T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Radius),
               T2DIntPoint.Create(DstPoint.x, DstPoint.y + Radius),
               {$ELSE}
               Create2DIntPoint(SrcPoint.x, SrcPoint.y + Radius),
               Create2DIntPoint(DstPoint.x, DstPoint.y + Radius),
               {$ENDIF}
               Width,
               Height - 2*Radius);
{$ENDREGION}

  // We fill the corners
{$REGION 'Left upper'}
  if LeftTopRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                              T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                              Create2DIntPoint(DstPoint.x, DstPoint.y),
                              {$ENDIF}
                              Radius,
                              cpLeftTop,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x, SrcPoint.y),
                         T2DIntPoint.Create(DstPoint.x, DstPoint.y),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x, SrcPoint.y),
                         Create2DIntPoint(DstPoint.x, DstPoint.y),
                         {$ENDIF}
                         Radius);
{$ENDREGION}

{$REGION 'Right upper'}
  if RightTopRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                              T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                              Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                              {$ENDIF}
                              Radius,
                              cpRightTop,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y),
                         T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y),
                         Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y),
                         {$ENDIF}
                         Radius);
{$ENDREGION}

{$REGION 'Left bottom'}
  if LeftBottomRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                              T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                              Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                              {$ENDIF}
                              Radius,
                              cpLeftBottom,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x, SrcPoint.y + Height - Radius),
                         T2DIntPoint.Create(DstPoint.x, DstPoint.y + Height - Radius),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x, SrcPoint.y + Height - Radius),
                         Create2DIntPoint(DstPoint.x, DstPoint.y + Height - Radius),
                         {$ENDIF}
                         Radius);
{$ENDREGION}

{$REGION 'Right bottom'}
  if RightBottomRound then
    TGUITools.CopyRoundCorner(ABuffer,
                              ABitmap,
                              {$IFDEF EnhancedRecordSupport}
                              T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                              T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                              {$ELSE}
                              Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                              Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                              {$ENDIF}
                              Radius,
                              cpRightBottom,
                              true)
  else
    TGUITools.CopyCorner(ABuffer,
                         ABitmap,
                         {$IFDEF EnhancedRecordSupport}
                         T2DIntPoint.Create(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                         T2DIntPoint.Create(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                         {$ELSE}
                         Create2DIntPoint(SrcPoint.x + Width - Radius, SrcPoint.y + Height - Radius),
                         Create2DIntPoint(DstPoint.x + Width - Radius, DstPoint.y + Height - Radius),
                         {$ENDIF}
                         Radius);
{$ENDREGION}
end;

class procedure TGUITools.CopyRectangle(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height: integer);
var
  BufferRect, BitmapRect: T2DIntRect;
  SrcRect, DstRect: T2DIntRect;
  ClippedSrcRect: T2DIntRect;
  Offset: T2DIntVector;
  y: Integer;
  SrcLine: Pointer;
  DstLine: Pointer;
  SrcImg: TLazIntfImage;
  DestImg: TLazIntfImage;
begin
  if (ABuffer.PixelFormat <> pf24Bit) or (ABitmap.PixelFormat <> pf24Bit) then
    raise exception.create('TSpkGUITools.CopyRoundCorner: Only 24 bit bitmaps are accepted!');

  // Validation
  if (Width < 1) or (Height < 1) then
    exit;

  if (ABuffer.Width = 0) or (ABuffer.Height = 0) or
     (ABitmap.Width = 0) or (ABitmap.Height = 0) then exit;

  {$IFDEF EnhancedRecordSupport}
  // Truncate the source rect to the source bitmap
  BufferRect := T2DIntRect.Create(0, 0, ABuffer.Width-1, ABuffer.Height-1);
  if not BufferRect.IntersectsWith(
      T2DIntRect.Create(SrcPoint.x, SrcPoint.y, SrcPoint.x+Width-1, SrcPoint.y+Height-1),
      SrcRect
    )
  then
    exit;

  // We cut the target rect to the target bitmap
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(
      T2DIntRect.Create(DstPoint.x, DstPoint.y, DstPoint.x+Width-1, DstPoint.y+Height-1),
      DstRect
    )
  then
    exit;
  {$ELSE}
  // Truncate the source rect to the source bitmap
  BufferRect.Create(0, 0, ABuffer.Width-1, ABuffer.Height-1);
  if not BufferRect.IntersectsWith(
      Create2DIntRect(SrcPoint.x, SrcPoint.y, SrcPoint.x+Width-1, SrcPoint.y+Height-1),
      SrcRect
    )
  then
    exit;

  // We cut the target rect to the target bitmap
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(
      Create2DIntRect(DstPoint.x, DstPoint.y, DstPoint.x+Width-1, DstPoint.y+Height-1),
      DstRect
    )
  then
    exit;
{$ENDIF}

  // We are counting the source offset to the target recta
  Offset := DstPoint - SrcPoint;

  // Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
  // offset maj¹ jak¹œ czêœæ wspóln¹
  // Google-translated:
  // Verify that the rectangular overhead: source and target shifted by offset have some common
  if not SrcRect.IntersectsWith(DstRect - Offset, ClippedSrcRect) then
    exit;

  // If there is anything to process, do the operation
  if (ClippedSrcRect.Left <= ClippedSrcRect.Right) and
     (ClippedSrcRect.Top <= ClippedSrcRect.Bottom) then
  begin
    SrcImg := ABuffer.CreateIntfImage;
    DestImg := ABitmap.CreateIntfImage;
    for y := ClippedSrcRect.Top to ClippedSrcRect.Bottom do
    begin
      SrcLine := SrcImg.GetDataLineStart(y);
      DstLine := DestImg.GetDataLineStart(y+Offset.y);
      Move(pointer(PtrInt(SrcLine) + 3*ClippedSrcRect.Left)^,
           pointer(PtrInt(DstLine) + 3*(ClippedSrcRect.Left + Offset.x))^,
           3*ClippedSrcRect.Width);
    end;
    ABitmap.LoadFromIntfImage(DestImg);
    SrcImg.Destroy;
    DestImg.Destroy;
  end;
end;

class procedure TGUITools.CopyCorner(ABuffer, ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Radius: integer);
begin
  CopyRectangle(ABuffer, ABitmap, SrcPoint, DstPoint, Radius, Radius);
end;

class procedure TGUITools.CopyCorner(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Radius: integer; ClipRect: T2DIntRect);
begin
  CopyRectangle(ABuffer, ABitmap, SrcPoint, DstPoint, Radius, Radius, ClipRect);
end;

class procedure TGUITools.CopyMaskRectangle(ABuffer, AMask, ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Width, Height: integer);
var
  BufferRect, BitmapRect: T2DIntRect;
  SrcRect, DstRect: T2DIntRect;
  ClippedSrcRect: T2DIntRect;
  Offset: T2DIntVector;
  y: Integer;
  SrcLine: Pointer;
  MaskLine: Pointer;
  DstLine: Pointer;
  SrcImg: TLazIntfImage;
  MaskImg: TLazIntfImage;
  DestImg: TLazIntfImage;
  i: Integer;
begin
  if (ABuffer.PixelFormat <> pf24Bit) or (ABitmap.PixelFormat <> pf24Bit) then
    raise Exception.Create('TSpkGUITools.CopyRoundCorner: Only 24 bit bitmaps are accepted!');

 if (AMask.PixelFormat <> pf8bit) then
   raise Exception.Create('TSpkGUITools.CopyRoundCorner: Only 8-bit masks are accepted!');

  // Validation
  if (Width < 1) or (Height < 1) then
    exit;

  if (ABuffer.Width = 0) or (ABuffer.Height = 0) or
     (ABitmap.Width = 0) or (ABitmap.Height = 0) then exit;

  if (ABuffer.Width <> AMask.Width) or (ABuffer.Height <> AMask.Height) then
    exit;

  {$IFDEF EnhancedRecordSupport}
  // Truncate the source rect to the source bitmap
  BufferRect := T2DIntRect.Create(0, 0, ABuffer.Width-1, ABuffer.Height-1);
  if not BufferRect.IntersectsWith(
      T2DIntRect.Create(SrcPoint.x, SrcPoint.y, SrcPoint.x+Width-1, SrcPoint.y+Height-1),
      SrcRect
    ) then
    exit;
  // We cut the target rect to the target bitmap
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(
      T2DIntRect.Create(DstPoint.x, DstPoint.y, DstPoint.x+Width-1, DstPoint.y+Height-1),
      DstRect
    ) then
    exit;
  {$ELSE}
  // Truncate the source rect to the source bitmap
  BufferRect.Create(0, 0, ABuffer.Width-1, ABuffer.Height-1);
  if not BufferRect.IntersectsWith(
      Create2DIntRect(SrcPoint.x, SrcPoint.y, SrcPoint.x+Width-1, SrcPoint.y+Height-1),
      SrcRect
    ) then
    exit;
  // Trim the target rect to the target bitmap
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(
      Create2DIntRect(DstPoint.x, DstPoint.y, DstPoint.x+Width-1, DstPoint.y+Height-1),
      DstRect
    ) then
    exit;
  {$ENDIF}

  // We are counting the source offset to the target recta
  Offset := DstPoint - SrcPoint;

  // Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
  // offset maj¹ jak¹œ czêœæ wspóln¹
  // Google-translated:
  // We check that the rectangles that are superimposed on each other:
  // source and target shifted by offset have some common
  if not(SrcRect.IntersectsWith(DstRect - Offset, ClippedSrcRect)) then
    exit;

  // If there is anything to process, do the operation
  if (ClippedSrcRect.Left <= ClippedSrcRect.Right) and
     (ClippedSrcRect.Top <= ClippedSrcRect.Bottom) then
  begin
    SrcImg := ABuffer.CreateIntfImage;
    DestImg := ABitmap.CreateIntfImage;
    MaskImg := AMask.CreateIntfImage;
    for y := ClippedSrcRect.Top to ClippedSrcRect.Bottom do
    begin
      SrcLine := SrcImg.GetDataLineStart(y);
      SrcLine := pointer(PtrInt(SrcLine) + 3 * ClippedSrcRect.left);
      MaskLine := MaskImg.GetDataLineStart(y);
      MaskLine := pointer(PtrInt(MaskLine) + ClippedSrcRect.left);
      DstLine := DestImg.GetDataLineStart(y+Offset.y);
      DstLine := pointer(PtrInt(DstLine) + 3 * (ClippedSrcRect.left + Offset.x));
      for i := 0 to ClippedSrcRect.Width - 1 do
      begin
        if PByte(MaskLine)^ < 128 then
          Move(SrcLine^, DstLine^, 3);
        SrcLine := pointer(PtrInt(SrcLine)+3);
        DstLine := pointer(PtrInt(DstLine)+3);
        MaskLine := pointer(PtrInt(MaskLine)+1);
      end;
    end;
    ABitmap.LoadFromIntfImage(DestImg);
    DestImg.Destroy;
    SrcImg.Destroy;
    MaskImg.Destroy;
  end;
end;

class procedure TGUITools.CopyMaskRectangle(ABuffer, AMask, ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Width, Height: integer;
  ClipRect: T2DIntRect);
var
  BufferRect, BitmapRect: T2DIntRect;
  SrcRect, DstRect: T2DIntRect;
  ClippedSrcRect, ClippedDstRect: T2DIntRect;
  Offset: T2DIntVector;
  y: Integer;
  SrcImg: TLazIntfImage;
  MaskImg: TLazIntfImage;
  DestImg: TLazIntfImage;
  SrcLine: Pointer;
  DstLine: Pointer;
  i: Integer;
  MaskLine: Pointer;
begin
  if (ABuffer.PixelFormat <> pf24Bit) or (ABitmap.PixelFormat <> pf24Bit) then
    raise Exception.Create('TSpkGUITools.CopyMaskRectangle: Only 24 bit bitmaps are accepted!');
  if AMask.PixelFormat<>pf8bit then
    raise Exception.Create('TSpkGUITools.CopyMaskRectangle: Only 8-bit masks are accepted!');

  // Validation
  if (Width < 1) or (Height < 1) then
    exit;

  if (ABuffer.Width = 0) or (ABuffer.Height = 0) or
     (ABitmap.Width = 0) or (ABitmap.Height = 0) then exit;

  if (ABuffer.Width <> AMask.Width) or
     (ABuffer.Height <> AMask.Height)
  then
    raise Exception.Create('TSpkGUITools.CopyMaskRectangle: The mask has incorrect dimensions!');

  {$IFDEF EnhancedRecordSupport}
  // Truncate the source rect to the source bitmap
  BufferRect := T2DIntRect.Create(0, 0, ABuffer.Width-1, ABuffer.Height-1);
  if not BufferRect.IntersectsWith(
      T2DIntRect.Create(SrcPoint.x, SrcPoint.y, SrcPoint.x+Width-1, SrcPoint.y+Height-1),
      SrcRect
    )
  then
    exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ELSE}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ENDIF}

// Dodatkowo przycinamy docelowy rect
if not(DstRect.IntersectsWith(ClipRect, ClippedDstRect)) then
   Exit;

// Liczymy offset Ÿród³owego do docelowego recta
Offset:=DstPoint - SrcPoint;

// Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
// offset maj¹ jak¹œ czêœæ wspóln¹
if not(SrcRect.IntersectsWith(ClippedDstRect - Offset, ClippedSrcRect)) then exit;

// Jeœli jest cokolwiek do przetworzenia, wykonaj operacjê
if (ClippedSrcRect.left<=ClippedSrcRect.right) and (ClippedSrcRect.top<=ClippedSrcRect.bottom) then
begin
  SrcImg := ABuffer.CreateIntfImage;
  DestImg := ABitmap.CreateIntfImage;
  MaskImg := ABitmap.CreateIntfImage;
   for y := ClippedSrcRect.top to ClippedSrcRect.bottom do
       begin
       SrcLine:=SrcImg.GetDataLineStart(y);
       SrcLine:=pointer(PtrInt(SrcLine) + 3 * ClippedSrcRect.left);

       MaskLine:=MaskImg.GetDataLineStart(y);
       MaskLine:=pointer(PtrInt(MaskLine) + ClippedSrcRect.left);

       DstLine:=DestImg.GetDataLineStart(y+Offset.y);
       DstLine:=pointer(PtrInt(DstLine) + 3 * (ClippedSrcRect.left + Offset.x));

       for i := 0 to ClippedSrcRect.width - 1 do
           begin
           if PByte(MaskLine)^<128 then
              Move(SrcLine^, DstLine^, 3);

           SrcLine:=pointer(PtrInt(SrcLine)+3);
           DstLine:=pointer(PtrInt(DstLine)+3);
           MaskLine:=pointer(PtrInt(MaskLine)+1);
           end;
       end;
   ABitmap.LoadFromIntfImage(DestImg);
   SrcImg.Destroy;
   DestImg.Destroy;
   MaskImg.Destroy;
end;
end;

class procedure TGUITools.CopyRectangle(ABuffer, ABitmap: TBitmap; SrcPoint,
  DstPoint: T2DIntVector; Width, Height: integer; ClipRect: T2DIntRect);

var BufferRect, BitmapRect : T2DIntRect;
    SrcRect, DstRect : T2DIntRect;
    ClippedSrcRect, ClippedDstRect : T2DIntRect;
    Offset : T2DIntVector;
    y: Integer;
    DestImg: TLazIntfImage;
    SrcImg: TLazIntfImage;
    SrcLine: Pointer;
    DstLine: Pointer;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if (Width<1) or (Height<1) then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

{$IFDEF EnhancedRecordSupport}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ELSE}
// Przycinamy Ÿród³owy rect do obszaru Ÿród³owej bitmapy
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Width-1,
                                                   SrcPoint.y+Height-1),
                                 SrcRect)) then exit;

// Przycinamy docelowy rect do obszaru docelowej bitmapy
BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Width-1,
                                                   DstPoint.y+Height-1),
                                 DstRect)) then exit;
{$ENDIF}

// Dodatkowo przycinamy docelowy rect
if not(DstRect.IntersectsWith(ClipRect, ClippedDstRect)) then
   Exit;

// Liczymy offset Ÿród³owego do docelowego recta
Offset:=DstPoint - SrcPoint;

// Sprawdzamy, czy na³o¿one na siebie recty: Ÿród³owy i docelowy przesuniêty o
// offset maj¹ jak¹œ czêœæ wspóln¹
if not(SrcRect.IntersectsWith(ClippedDstRect - Offset, ClippedSrcRect)) then exit;

// Jeœli jest cokolwiek do przetworzenia, wykonaj operacjê
if (ClippedSrcRect.left<=ClippedSrcRect.right) and (ClippedSrcRect.top<=ClippedSrcRect.bottom) then
begin
  SrcImg := ABuffer.CreateIntfImage;
  DestImg := ABitmap.CreateIntfImage;
   for y := ClippedSrcRect.top to ClippedSrcRect.bottom do
       begin
       SrcLine:=SrcImg.GetDataLineStart(y);
       DstLine:=DestImg.GetDataLineStart(y+Offset.y);

       Move(pointer(PtrInt(SrcLine) + 3*ClippedSrcRect.left)^,
            pointer(PtrInt(DstLine) + 3*(ClippedSrcRect.left + Offset.x))^,
            3*ClippedSrcRect.Width);
       end;
  ABitmap.LoadFromIntfImage(DestImg);
  DestImg.Destroy;
  SrcImg.Destroy;
end;
end;

class procedure TGUITools.CopyRoundCorner(ABuffer: TBitmap; ABitmap: TBitmap;
  SrcPoint, DstPoint: T2DIntVector; Radius: integer; CornerPos: TCornerPos;
  Convex: boolean);

var BufferRect, BitmapRect : T2DIntRect;
    OrgSrcRect, OrgDstRect : T2DIntRect;
    SrcRect : T2DIntRect;
    Offset : T2DIntVector;
    Center: T2DIntVector;
    y: Integer;
    SrcImg: TLazIntfImage;
    DestImg: TLazIntfImage;
    SrcLine: Pointer;
    DstLine: Pointer;
    SrcPtr, DstPtr : PByte;
    x: Integer;
    Dist : double;

begin
if (ABuffer.PixelFormat<>pf24bit) or (ABitmap.PixelFormat<>pf24bit) then
   raise exception.create('TSpkGUITools.CopyRoundCorner: Tylko 24-bitowe bitmapy s¹ akceptowane!');

// Sprawdzanie poprawnoœci
if Radius<1 then
   exit;

if (ABuffer.width=0) or (ABuffer.height=0) or
   (ABitmap.width=0) or (ABitmap.height=0) then exit;

{$IFDEF EnhancedRecordSupport}
BufferRect:=T2DIntRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(T2DIntRect.create(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Radius-1,
                                                   SrcPoint.y+Radius-1),
                                 OrgSrcRect)) then exit;

BitmapRect:=T2DIntRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(T2DIntRect.create(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Radius-1,
                                                   DstPoint.y+Radius-1),
                                 OrgDstRect)) then exit;
{$ELSE}
BufferRect.create(0, 0, ABuffer.width-1, ABuffer.height-1);
if not(BufferRect.IntersectsWith(Create2DIntRect(SrcPoint.x,
                                                   SrcPoint.y,
                                                   SrcPoint.x+Radius-1,
                                                   SrcPoint.y+Radius-1),
                                 OrgSrcRect)) then exit;

BitmapRect.create(0, 0, ABitmap.width-1, ABitmap.height-1);
if not(BitmapRect.IntersectsWith(Create2DIntRect(DstPoint.x,
                                                   DstPoint.y,
                                                   DstPoint.x+Radius-1,
                                                   DstPoint.y+Radius-1),
                                 OrgDstRect)) then exit;
{$ENDIF}

Offset:=DstPoint - SrcPoint;

if not(OrgSrcRect.IntersectsWith(OrgDstRect - Offset, SrcRect)) then exit;

// Ustalamy pozycjê œrodka ³uku

{$IFDEF EnhancedRecordSupport}
case CornerPos of
     cpLeftTop: Center:=T2DIntVector.create(SrcPoint.x + radius - 1, SrcPoint.y + Radius - 1);
     cpRightTop: Center:=T2DIntVector.create(SrcPoint.x, SrcPoint.y + Radius - 1);
     cpLeftBottom: Center:=T2DIntVector.Create(SrcPoint.x + radius - 1, SrcPoint.y);
     cpRightBottom: Center:=T2DIntVector.Create(SrcPoint.x, SrcPoint.y);
end;
{$ELSE}
case CornerPos of
     cpLeftTop: Center.create(SrcPoint.x + radius - 1, SrcPoint.y + Radius - 1);
     cpRightTop: Center.create(SrcPoint.x, SrcPoint.y + Radius - 1);
     cpLeftBottom: Center.Create(SrcPoint.x + radius - 1, SrcPoint.y);
     cpRightBottom: Center.Create(SrcPoint.x, SrcPoint.y);
end;
{$ENDIF}

// Czy jest cokolwiek do przetworzenia?
if Convex then
   begin
   if (SrcRect.left<=SrcRect.right) and (SrcRect.top<=SrcRect.bottom) then
   begin
     SrcImg := ABuffer.CreateIntfImage;
     DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.top to SrcRect.bottom do
          begin
          SrcLine:=SrcImg.GetDataLineStart(y);
          DstLine:=DestImg.GetDataLineStart(y+Offset.y);

          SrcPtr:=pointer(PtrInt(SrcLine) + 3*SrcRect.left);
          DstPtr:=pointer(PtrInt(DstLine) + 3*(SrcRect.left + Offset.x));
          for x := SrcRect.left to SrcRect.right do
              begin
              {$IFDEF EnhancedRecordSupport}
              Dist:=Center.DistanceTo(T2DVector.create(x, y));
              {$ELSE}
              Dist:=Center.DistanceTo(x, y);
              {$ENDIF}
              if Dist <= (Radius-1) then
                 Move(SrcPtr^,DstPtr^,3);

              inc(SrcPtr,3);
              inc(DstPtr,3);
              end;
          end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
    end;
   end
else
   begin
   if (SrcRect.left<=SrcRect.right) and (SrcRect.top<=SrcRect.bottom) then
   begin
     SrcImg := ABuffer.CreateIntfImage;
     DestImg := ABitmap.CreateIntfImage;
      for y := SrcRect.top to SrcRect.bottom do
          begin
          SrcLine:=SrcImg.GetDataLineStart(y);
          DstLine:=DestImg.GetDataLineStart(y+Offset.y);

          SrcPtr:=pointer(PtrInt(SrcLine) + 3*SrcRect.left);
          DstPtr:=pointer(PtrInt(DstLine) + 3*(SrcRect.left + Offset.x));
          for x := SrcRect.left to SrcRect.right do
              begin
              {$IFDEF EnhancedRecordSupport}
              Dist:=Center.DistanceTo(T2DVector.create(x, y));
              {$ELSE}
              Dist:=Center.DistanceTo(x, y);
              {$ENDIF}
              if Dist >= (Radius-1) then
                 Move(SrcPtr^,DstPtr^,3);

              inc(SrcPtr,3);
              inc(DstPtr,3);
              end;
          end;
      ABitmap.LoadFromIntfImage(DestImg);
      SrcImg.Destroy;
      DestImg.Destroy;
    end;
   end;
end;

class procedure TGUITools.DrawAARoundCorner(ABitmap: TBitmap; Point: T2DIntVector;
  Radius: integer; CornerPos: TCornerPos; Color: TColor);
var
  CornerRect: T2DIntRect;
  Center: T2DIntVector;
  Line: PByte;
  Ptr: PByte;
  colorR, colorG, colorB: byte;
  x, y: integer;
  RadiusDist: double;
  OrgCornerRect: T2DIntRect;
  BitmapRect: T2DIntRect;
  cr, cg, cb: Byte;
begin
  if ABitmap.PixelFormat <> pf24bit then
    raise Exception.Create('TSpkGUITools.DrawAARoundCorner: Bitmapa musi byæ w trybie 24-bitowym!');

  // Sprawdzamy poprawnoœæ
  if Radius < 1 then
    exit;
  if (ABitmap.Width=0) or (ABitmap.Height=0) then
    exit;

  {$IFDEF EnhancedRecordSupport}
  // �?ród³owy rect...
  OrgCornerRect := T2DIntRect.Create(Point.x,
                                     Point.y,
                                     Point.x + radius - 1,
                                     Point.y + radius - 1);

  // ...przycinamy do rozmiarów bitmapy
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ELSE}
  // �?ród³owy rect...
  OrgCornerRect.Create(Point.x,
                       Point.y,
                       Point.x + radius - 1,
                       Point.y + radius - 1);

  // ...przycinamy do rozmiarów bitmapy
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ENDIF}

  if not BitmapRect.intersectsWith(OrgCornerRect, CornerRect) then
    exit;

  // Jeœli nie ma czego rysowaæ, wychodzimy
  if (CornerRect.Left > CornerRect.Right) or (CornerRect.Top > CornerRect.Bottom) then
    exit;

  // Szukamy œrodka ³uku - zale¿nie od rodzaju naro¿nika
  {$IFDEF EnhancedRecordSupport}
  case CornerPos of
    cpLeftTop:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center := T2DIntVector.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center := T2DIntVector.Create(Point.x, Point.y);
  end;
  {$ELSE}
  case CornerPos of
    cpLeftTop:
      Center.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center.Create(Point.x, Point.y);
  end;
  {$ENDIF}

  Color := ColorToRGB(Color);

  colorR := GetRValue(Color);
  colorG := GetGValue(Color);
  colorB := GetBValue(Color);

  for y := CornerRect.Top to CornerRect.Bottom do
  begin
    for x := CornerRect.Left to CornerRect.Right do
    begin
      {$IFDEF EnhancedRecordSupport}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(T2DIntVector.create(x, y)));
      {$ELSE}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(x, y));
      {$ENDIF}
      if RadiusDist > 0 then
      begin
        RedGreenBlue(ColorToRGB(ABitmap.Canvas.Pixels[x,y]), cr, cg, cb);
        cb := round(cb + (ColorB - cb) * RadiusDist);
        cg := round(cg + (ColorG - cg) * RadiusDist);
        cr := round(cr + (ColorR - cr) * RadiusDist);
        ABitmap.Canvas.Pixels[x,y] := RGBToColor(cr,cg,cb);
      end;
    end;
  end;
end;

class procedure TGUITools.DrawAARoundCorner(ABitmap: TBitmap;
  Point: T2DIntVector; Radius: integer; CornerPos: TCornerPos; Color: TColor;
  ClipRect: T2DIntRect);
var
  CornerRect: T2DIntRect;
  Center: T2DIntVector;
  Line: PByte;
  Ptr: PByte;
  colorR, colorG, colorB: byte;
  x, y: integer;
  RadiusDist: double;
  OrgCornerRect: T2DIntRect;
  UnClippedCornerRect : T2DIntRect;
  BitmapRect: T2DIntRect;
  cr,cb,cg: byte;
begin
  if ABitmap.PixelFormat<>pf24bit then
    raise Exception.Create('TSpkGUITools.DrawAARoundCorner: Bitmap must be in 24-bit mode!');

  if Radius < 1 then
    exit;
  if (ABitmap.Width = 0) or (ABitmap.Height = 0) then
    exit;

  {$IFDEF EnhancedRecordSupport}
  // Source rect...
  OrgCornerRect := T2DIntRect.Create(
    Point.x, Point.y, Point.x + radius - 1, Point.y + radius - 1
  );
  // ... cut to size bitmap
  BitmapRect := T2DIntRect.Create(
    0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ELSE}
  // Source rect...
  OrgCornerRect.Create(Point.x,
                       Point.y,
                       Point.x + radius - 1,
                       Point.y + radius - 1);

  // ... cut to size bitmap
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  {$ENDIF}

  if not BitmapRect.IntersectsWith(OrgCornerRect, UnClippedCornerRect) then
    exit;

  // ClipRect
  if not UnClippedCornerRect.IntersectsWith(ClipRect, CornerRect) then
    exit;

  // If there is nothing to draw, we leave
  if (CornerRect.Left > CornerRect.Right) or
     (CornerRect.Top > CornerRect.Bottom)
  then
    exit;

  // We seek the center of the arc - depending on the type of corner
  {$IFDEF EnhancedRecordSupport}
  case CornerPos of
    cpLeftTop:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center := T2DIntVector.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center := T2DIntVector.Create(Point.x, Point.y);
  end;
  {$ELSE}
  case CornerPos of
    cpLeftTop:
      Center.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center.Create(Point.x, Point.y);
  end;
  {$ENDIF}

  Color := ColorToRGB(Color);

  colorR := GetRValue(Color);
  colorG := GetGValue(Color);
  colorB := GetBValue(Color);

  for y := CornerRect.Top to CornerRect.Bottom do
  begin
    for x := CornerRect.Left to CornerRect.Right do
    begin
      {$IFDEF EnhancedRecordSupport}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(T2DIntVector.create(x, y)));
      {$ELSE}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(x, y));
      {$ENDIF}
      if RadiusDist > 0 then
      begin
        RedGreenBlue(ColorToRGB(ABitmap.Canvas.Pixels[x,y]), cr, cg, cb);
        cb := round(cb + (ColorB - cb) * RadiusDist);
        cg := round(cg + (ColorG - cg) * RadiusDist);
        cr := round(cr + (ColorR - cr) * RadiusDist);
        ABitmap.Canvas.Pixels[x,y] := RGBToColor(cr,cg,cb);
      end;
    end;
  end;
end;

class procedure TGUITools.DrawAARoundCorner(ACanvas: TCanvas;
  Point: T2DIntVector; Radius: integer; CornerPos: TCornerPos; Color: TColor);
var
  Center: T2DIntVector;
  OrgColor: TColor;
  x, y: integer;
  RadiusDist: double;
  CornerRect: T2DIntRect;
begin
  if Radius<1 then
    exit;

  {$IFDEF EnhancedRecordSupport}
  // Source rect
  CornerRect := T2DIntRect.Create(
    Point.x, Point.y, Point.x + radius - 1, Point.y + radius - 1
  );
  // We seek the center of the arc - depending on the type of corner
  case CornerPos of
    cpLeftTop:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center := T2DIntVector.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center := T2DIntVector.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center := T2DIntVector.Create(Point.x, Point.y);
  end;
  {$ELSE}
  // Source rect
  CornerRect.Create(Point.x, Point.y, Point.x + radius - 1, Point.y + radius - 1);
  // We seek the center of the arc - depending on the type of corner
  case CornerPos of
    cpLeftTop:
      Center.Create(Point.x + radius - 1, Point.y + Radius - 1);
    cpRightTop:
      Center.Create(Point.x, Point.y + Radius - 1);
    cpLeftBottom:
      Center.Create(Point.x + radius - 1, Point.y);
    cpRightBottom:
      Center.Create(Point.x, Point.y);
  end;
  {$ENDIF}

  Color := ColorToRGB(Color);

  for y := CornerRect.Top to CornerRect.Bottom do
  begin
    for x := CornerRect.Left to CornerRect.Right do
    begin
      {$IFDEF EnhancedRecordSupport}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(T2DIntVector.Create(x, y)));
      {$ELSE}
      RadiusDist := 1 - abs((Radius - 1) - Center.DistanceTo(x, y));
      {$ENDIF}
      if RadiusDist > 0 then
      begin
        OrgColor := ACanvas.Pixels[x, y];
        ACanvas.Pixels[x, y] := TColorTools.Shade(OrgColor, Color, RadiusDist);
      end;
    end;
  end;
end;

class procedure TGUITools.DrawAARoundCorner(ACanvas: TCanvas;
  Point: T2DIntVector; Radius: integer; CornerPos: TCornerPos; Color: TColor;
  ClipRect: T2DIntRect);
var
  UseOrgClipRgn: boolean;
  ClipRgn: HRGN;
  OrgRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.Left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawAARoundCorner(ACanvas, Point, Radius, CornerPos, Color);

  // Restores previous ClipRgn and removes used regions
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawAARoundFrame(ABitmap: TBitmap; Rect: T2DIntRect;
  Radius: integer; Color: TColor; ClipRect: T2DIntRect);
begin
  if ABitmap.PixelFormat <> pf24Bit then
   raise Exception.Create('TGUITools.DrawAARoundFrame: Bitmap must be in 24-bit mode!');

  if Radius < 1 then
    exit;

  if (Radius > Rect.Width div 2) or (Radius > Rect.Height div 2) then
    exit;

  // DrawAARoundCorner is protected against drawing outside the area
  {$IFDEF EnhancedRecordSupport}
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Left, Rect.Top), Radius, cpLeftTop, Color, ClipRect);
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Right - Radius + 1, Rect.Top), Radius, cpRightTop, Color, ClipRect);
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Left, Rect.Bottom - Radius + 1), Radius, cpLeftBottom, Color, ClipRect);
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color, ClipRect);
  {$ELSE}
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Left, Rect.Top), Radius, cpLeftTop, Color, ClipRect);
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Right - Radius + 1, Rect.Top), Radius, cpRightTop, Color, ClipRect);
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Left, Rect.Bottom - Radius + 1), Radius, cpLeftBottom, Color, ClipRect);
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color, ClipRect);
  {$ENDIF}

  ABitmap.Canvas.Pen.Color := Color;
  ABitmap.Canvas.pen.Style := psSolid;

  // Draw*Line is protected against drawing outside the area
  DrawVLine(ABitmap, Rect.Left, Rect.top + Radius, Rect.Bottom - Radius, Color, ClipRect);
  DrawVLine(ABitmap, Rect.Right, Rect.top + Radius, Rect.Bottom - Radius, Color, ClipRect);
  DrawHLine(ABitmap, Rect.Left + Radius, Rect.Right - Radius, Rect.Top, Color, ClipRect);
  DrawHLine(ABitmap, Rect.Left + Radius, Rect.Right - Radius, Rect.Bottom, Color, ClipRect);
end;

class procedure TGUITools.DrawAARoundFrame(ABitmap: TBitmap; Rect: T2DIntRect;
  Radius: integer; Color: TColor);
begin
  if ABitmap.PixelFormat <> pf24Bit then
    raise Exception.Create('TGUITools.DrawAARoundFrame: Bitmap must be in 24-bit mode!');

  if Radius < 1 then
    exit;

  if (Radius > Rect.Width div 2) or (Radius > Rect.Height div 2) then
    exit;

  // DrawAARoundCorner is protected against drawing outside the area
  {$IFDEF EnhancedRecordSupport}
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Left, Rect.Top), Radius, cpLeftTop, Color);
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Right - Radius + 1, Rect.Top), Radius, cpRightTop, Color);
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Left, Rect.Bottom - Radius + 1), Radius, cpLeftBottom, Color);
  DrawAARoundCorner(ABitmap, T2DIntVector.Create(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
  {$ELSE}
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Left, Rect.Top), Radius, cpLeftTop, Color);
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Right - Radius + 1, Rect.top), Radius, cpRightTop, Color);
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Left, Rect.Bottom - Radius + 1), Radius, cpLeftBottom, Color);
  DrawAARoundCorner(ABitmap, Create2DIntVector(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
  {$ENDIF}

  ABitmap.Canvas.Pen.Color := Color;
  ABitmap.Canvas.pen.Style := psSolid;

  // Draw*Line is protected against drawing outside the area
  DrawVLine(ABitmap, Rect.Left, Rect.Top + Radius, Rect.bottom - Radius, Color);
  DrawVLine(ABitmap, Rect.Right, Rect.Top + Radius, Rect.bottom - Radius, Color);
  DrawHLine(ABitmap, Rect.Left + Radius, Rect.Right - Radius, Rect.Top, Color);
  DrawHLine(ABitmap, Rect.Left + Radius, Rect.Right - Radius, Rect.Bottom, Color);
end;

class procedure TGUITools.DrawFitWText(ABitmap: TBitmap; x1, x2, y: integer;
  const AText: string; TextColor: TColor; Align : TAlignment);
var
  tw: integer;
  s: string;
begin
  with ABitmap.Canvas do
  begin
    s := AText;
    tw := TextWidth(s);
    if tw <= x2-x1+1 then
    case Align of
      taLeftJustify  : TextOut(x1,y,AText);
      taRightJustify : TextOut(x2-tw+1,y,AText);
      taCenter       : TextOut(x1 + ((x2-x1 - tw) div 2), y, AText);
    end else
    begin
      while (s <> '') and (tw > x2-x1+1) do
      begin
        Delete(s, Length(s), 1);
        tw := TextWidth(s+'...');
      end;
      if tw <= x2-x1+1 then
        TextOut(x1, y, s+'...');
    end;
  end;
end;

class procedure TGUITools.DrawHLine(ACanvas: TCanvas; x1, x2, y: integer;
  Color: TColor);
begin
  EnsureOrder(x1, x2);
  ACanvas.Pen.Color := Color;
  ACanvas.MoveTo(x1, y);
  ACanvas.LineTo(x2+1,y);
end;

class procedure TGUITools.DrawHLine(ACanvas: TCanvas; x1, x2, y: integer;
  Color: TColor; ClipRect: T2DIntRect);
var
  UseOrgClipRgn: boolean;
  ClipRgn: HRGN;
  OrgRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawHLine(ACanvas, x1, x2, y, Color);

  // Restores previous ClipRgn and removes used regions
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawImage(ABitmap: TBitmap; Imagelist: TImageList;
  ImageIndex: integer; Point : T2DIntVector; ClipRect: T2DIntRect);
begin
  DrawImage(ABitmap.Canvas, ImageList, ImageIndex, Point, ClipRect);
end;

class procedure TGUITools.DrawImage(ABitmap: TBitmap; Imagelist: TImageList;
  ImageIndex: integer; Point: T2DIntVector);
begin
  DrawImage(ABitmap.Canvas, ImageList, ImageIndex, Point);
end;

class procedure TGUITools.DrawImage(ACanvas: TCanvas; Imagelist: TImageList;
  ImageIndex: integer; Point : T2DIntVector; ClipRect: T2DIntRect);
var
  UseOrgClipRgn: Boolean;
  OrgRgn: HRGN;
  ClipRgn: HRGN;
  //ImageIcon: TIcon;  // wp: no longer needed -- see below
  ImageBitmap: TBitmap;
begin
  // Storing original ClipRgn and applying a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  { wp: Next part fixes issue https://sourceforge.net/p/lazarus-ccr/bugs/35/ }
  ImageBitmap := TBitmap.Create;
  ImageList.GetBitmap(ImageIndex, ImageBitmap);
  ACanvas.Draw(Point.x, Point.y, ImageBitmap);
  ImageBitmap.Free;

{ wp: The following lines were removed and replaced by the "ImageBitmap" lines
  above in order to fix the "handle leak" of
  https://sourceforge.net/p/lazarus-ccr/bugs/35/
  Not daring to touch the ImageList.Draw which would have worked as well. }
{
  // avoid exclusive draw. draw with local canvas itself.
  //ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex);
  {$IfDef LCLWin32}
  ImageIcon := TIcon.Create;
  ImageList.GetIcon(ImageIndex, ImageIcon);
  ACanvas.Draw(Point.x, Point.y, ImageIcon);
  ImageIcon.Free;
  {$Else}
  ImageBitmap := TBitmap.Create;
  ImageList.GetBitmap(ImageIndex, ImageBitmap);
  ACanvas.Draw(Point.x, Point.y, ImageBitmap);
  ImageBitmap.Free;
  {$EndIf}
}
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawMarkedText(ACanvas: TCanvas; x, y: integer; const AText,
  AMarkPhrase: string; TextColor : TColor; ClipRect: T2DIntRect; CaseSensitive: boolean);
var
  UseOrgClipRgn: Boolean;
  OrgRgn: HRGN;
  ClipRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawMarkedText(ACanvas, x, y, AText, AMarkPhrase, TextColor, CaseSensitive);

  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawMarkedText(ACanvas: TCanvas; x, y: integer; const AText,
  AMarkPhrase: string; TextColor : TColor; CaseSensitive : boolean);
var
  TextToDraw: string;
  BaseText: string;
  MarkText: string;
  MarkPos: Integer;
  x1: integer;
  s: string;
  MarkTextLength: Integer;
begin
  TextToDraw := AText;
  if CaseSensitive then
  begin
    BaseText := AText;
    MarkText := AMarkPhrase;
  end else
  begin
    BaseText := AnsiUpperCase(AText);
    MarkText := AnsiUpperCase(AMarkPhrase);
  end;

  x1 := x;
  MarkTextLength := Length(MarkText);

  ACanvas.Font.Color := TextColor;
  ACanvas.Brush.Style := bsClear;

  MarkPos := pos(MarkText, BaseText);
  while MarkPos > 0 do
  begin
    if MarkPos > 1 then
    begin
      // Drawing text before highlighted
      ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
      s := copy(TextToDraw, 1, MarkPos-1);

      ACanvas.TextOut(x1, y, s);
      inc(x1, ACanvas.TextWidth(s)+1);

      Delete(TextToDraw, 1, MarkPos-1);
      Delete(BaseText, 1, MarkPos-1);
    end;

    // Drawing highlighted text
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
    s := copy(TextToDraw, 1, MarkTextLength);

    ACanvas.TextOut(x1, y, s);
    inc(x1, ACanvas.TextWidth(s)+1);

    Delete(TextToDraw, 1, MarkTextLength);
    Delete(BaseText, 1, MarkTextLength);

    MarkPos := pos(MarkText, BaseText);
  end;

  if Length(BaseText) > 0 then
  begin
    ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
    ACanvas.TextOut(x1, y, TextToDraw);
  end;
end;

class procedure TGUITools.DrawImage(ACanvas: TCanvas; Imagelist: TImageList;
  ImageIndex: integer; Point: T2DIntVector);
begin
  ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex);
end;

class procedure TGUITools.DrawOutlinedText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor);
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Font.Color := OutlineColor;
    TextOut(x-1, y-1, AText);
    TextOut(x, y-1, AText);
    TextOut(x+1, y-1, AText);
    TextOut(x-1, y, AText);
    TextOut(x+1, y, AText);
    TextOut(x-1, y+1, AText);
    TextOut(x, y+1, AText);
    TextOut(x+1, y+1, AText);

    Font.Color := TextColor;
    TextOut(x, y, AText);
  end;
end;

class procedure TGUITools.DrawOutlinedText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor; ClipRect: T2DIntRect);
var
  WinAPIClipRect: TRect;
begin
  WinAPIClipRect := ClipRect.ForWinAPI;
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Font.Color := OutlineColor;
    TextRect(WinAPIClipRect, x-1, y-1, AText);
    TextRect(WinAPIClipRect, x, y-1, AText);
    TextRect(WinAPIClipRect, x+1, y-1, AText);
    TextRect(WinAPIClipRect, x-1, y, AText);
    TextRect(WinAPIClipRect, x+1, y, AText);
    TextRect(WinAPIClipRect, x-1, y+1, AText);
    TextRect(WinAPIClipRect, x, y+1, AText);
    TextRect(WinAPIClipRect, x+1, y+1, AText);

    Font.Color := TextColor;
    TextRect(WinAPIClipRect, x, y, AText);
  end;
end;

class procedure TGUITools.DrawHLine(ABitmap: TBitmap; x1, x2, y: integer;
  Color: TColor);
var
  LineRect: T2DIntRect;
  BitmapRect: T2DIntRect;
begin
  if ABitmap.PixelFormat <> pf24Bit then
    raise Exception.Create('TGUITools.DrawHLine: Bitmap must be in 24-bit mode');
  EnsureOrder(x1, x2);

  {$IFDEF EnhancedRecordSupport}
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(T2DIntRect.Create(x1, y, x2, y), LineRect) then
    exit;
  {$ELSE}
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(Create2DIntRect(x1, y, x2, y), LineRect) then
    exit;
  {$ENDIF}

  ABitmap.Canvas.Pen.Color := Color;
  ABitmap.Canvas.Pen.Style := psSolid;
  ABitmap.Canvas.MoveTo(LineRect.Left, LineRect.Top);
  ABitmap.canvas.LineTo(LineRect.Right+1, LineRect.Top);
end;

class procedure TGUITools.DrawHLine(ABitmap: TBitmap; x1, x2, y: integer;
  Color: TColor; ClipRect: T2DIntRect);
var
  OrgLineRect: T2DIntRect;
  LineRect: T2DIntRect;
  BitmapRect: T2DIntRect;
begin
  if ABitmap.PixelFormat<>pf24bit then
    raise Exception.Create('TGUITools.DrawHLine: Bitmap must be in 24-bit mode!');
  EnsureOrder(x1, x2);

  {$IFDEF EnhancedRecordSupport}
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(T2DIntRect.Create(x1, y, x2, y), OrgLineRect) then
    exit;
  {$ELSE}
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not BitmapRect.IntersectsWith(Create2DIntRect(x1, y, x2, y), OrgLineRect) then
    exit;
  {$ENDIF}

  if not OrgLineRect.IntersectsWith(ClipRect, LineRect) then
    exit;

  ABitmap.Canvas.Pen.Color := Color;
  ABitmap.Canvas.Pen.Style := psSolid;
  ABitmap.Canvas.MoveTo(LineRect.Left, LineRect.Top);
  ABitmap.Canvas.LineTo(LineRect.Right+1, LineRect.Top);
end;

class procedure TGUITools.DrawOutlinedText(ABitmap: TBitmap; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor; ClipRect: T2DIntRect);
var
  WinAPIClipRect: TRect;
begin
  WinAPIClipRect := ClipRect.ForWinAPI;
  with ABitmap.canvas do
  begin
    Brush.Style := bsClear;
    Font.Color := OutlineColor;
    TextRect(WinAPIClipRect, x-1, y-1, AText);
    TextRect(WinAPIClipRect, x, y-1, AText);
    TextRect(WinAPIClipRect, x+1, y-1, AText);
    TextRect(WinAPIClipRect, x-1, y, AText);
    TextRect(WinAPIClipRect, x+1, y, AText);
    TextRect(WinAPIClipRect, x-1, y+1, AText);
    TextRect(WinAPIClipRect, x, y+1, AText);
    TextRect(WinAPIClipRect, x+1, y+1, AText);

    Font.Color := TextColor;
    TextRect(WinAPIClipRect, x, y, AText);
  end;
end;

class procedure TGUITools.DrawRegion(ACanvas: TCanvas; Region: HRGN;
  Rect : T2DIntRect; ColorFrom, ColorTo: TColor; GradientKind: TBackgroundKind);
var
  UseOrgClipRgn: Boolean;
  OrgRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  SelectClipRgn(ACanvas.Handle, Region);

  FillGradientRectangle(ACanvas, Rect, ColorFrom, ColorTo, GradientKind);

  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
end;

class procedure TGUITools.DrawRegion(ACanvas: TCanvas; Region: HRGN;
  Rect : T2DIntRect; ColorFrom, ColorTo: TColor; GradientKind: TBackgroundKind;
  ClipRect: T2DIntRect);
var
  UseOrgClipRgn: boolean;
  ClipRgn: HRGN;
  OrgRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawRegion(ACanvas, Region, Rect, ColorFrom, ColorTo, GradientKind);

  // Restores previous ClipRgn and removes used regions
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawRoundRect(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; ColorFrom, ColorTo: TColor; GradientKind: TBackgroundKind;
  ClipRect: T2DIntRect; LeftTopRound, RightTopRound, LeftBottomRound,
  RightBottomRound: boolean);
var
  UseOrgClipRgn: boolean;
  ClipRgn: HRGN;
  OrgRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(
    ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1
  );
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawRoundRect(ACanvas, Rect, Radius, ColorFrom, ColorTo, GradientKind,
    LeftTopRound, RightTopRound, LeftBottomRound, RightBottomRound);

  // Restores previous ClipRgn and removes used regions
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor: TColor);
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Font.Color := TextColor;
    TextOut(x, y, AText);
  end;
end;

class procedure TGUITools.DrawText(ACanvas: TCanvas; x, y: integer;
  const AText: string; TextColor: TColor; ClipRect: T2DIntRect);
var
  WinAPIClipRect: TRect;
begin
  WinAPIClipRect := ClipRect.ForWinAPI;
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Font.Color := TextColor;
    TextRect(WinAPIClipRect, x, y, AText);
  end;
end;

class procedure TGUITools.DrawRoundRect(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; ColorFrom, ColorTo: TColor; GradientKind: TBackgroundKind;
  LeftTopRound, RightTopRound, LeftBottomRound, RightBottomRound: boolean);
var
  RoundRgn: HRGN;
  TmpRgn: HRGN;
  OrgRgn: HRGN;
  UseOrgClipRgn: Boolean;
begin
  if Radius < 0 then
    exit;

  if Radius > 0 then
  begin
    //WriteLn('Radius: ', Radius, ' Rect.Width: ', Rect.Width, ' Rect.Height: ', Rect.Height);

    // There's a bug in fpc that evaluates the expression below erroneous when using inline
    // Radius = 3 and Rect.Width >= 128 and <= 261 will evaluate to true
    {$ifdef FpcBugWorkAround}
    if (CompareValue(Radius*2, Rect.width) > 0) and (CompareValue(Radius*2, Rect.Height) > 0) then
      exit;
    {$else}
    if (Radius*2 > Rect.Width) or (Radius*2 > Rect.Height) then
      exit;
    {$endif}

    // Zapamiêtywanie oryginalnego ClipRgn i ustawianie nowego
    SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

    if not(LeftTopRound) and
       not(RightTopRound) and
       not(LeftBottomRound) and
       not (RightBottomRound) then
    begin
      RoundRgn := CreateRectRgn(Rect.Left, Rect.Top, Rect.Right + 1, Rect.Bottom + 1);
    end
    else
    begin
      RoundRgn := CreateRoundRectRgn(Rect.Left, Rect.Top, Rect.Right +2, Rect.Bottom + 2, Radius*2, Radius*2);

      if not LeftTopRound then
      begin
        TmpRgn := CreateRectRgn(Rect.Left, Rect.Top, Rect.Left + Radius, Rect.Top + Radius);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;

      if not RightTopRound then
      begin
        TmpRgn := CreateRectRgn(Rect.Right - Radius + 1, Rect.Top, Rect.Right + 1, Rect.Top + Radius);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;

      if not LeftBottomRound then
      begin
        TmpRgn := CreateRectRgn(Rect.Left, Rect.Bottom - Radius + 1, Rect.Left + Radius, Rect.Bottom + 1);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;

      if not RightBottomRound then
      begin
        TmpRgn := CreateRectRgn(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1, Rect.Right + 1, Rect.Bottom + 1);
        CombineRgn(RoundRgn, RoundRgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;
    end;

    if UseOrgClipRgn then
      CombineRgn(RoundRgn, RoundRgn, OrgRgn, RGN_AND);

    SelectClipRgn(ACanvas.Handle, RoundRgn);
  end;  // if Radius > 0

  ColorFrom := ColorToRGB(ColorFrom);
  ColorTo := ColorToRGB(ColorTo);

  FillGradientRectangle(ACanvas, Rect, ColorFrom, ColorTo, GradientKind);

  if Radius > 0 then
  begin
    // Restores previous ClipRgn and removes used regions
    RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
    DeleteObject(RoundRgn);
  end;
end;

class procedure TGUITools.DrawOutlinedText(ABitmap: TBitmap; x, y: integer;
  const AText: string; TextColor, OutlineColor: TColor);
begin
  with ABitmap.Canvas do
  begin
    Brush.Style := bsClear;
    Font.Color := OutlineColor;
    TextOut(x-1, y-1, AText);
    TextOut(x, y-1, AText);
    TextOut(x+1, y-1, AText);
    TextOut(x-1, y, AText);
    TextOut(x+1, y, AText);
    TextOut(x-1, y+1, AText);
    TextOut(x, y+1, AText);
    TextOut(x+1, y+1, AText);

    Font.Color := TextColor;
    TextOut(x, y, AText);
  end;
end;

class procedure TGUITools.DrawText(ABitmap: TBitmap; x, y: integer;
  const AText: string; TextColor: TColor; ClipRect: T2DIntRect);
var
  WinAPIClipRect : TRect;
begin
  WinAPIClipRect := ClipRect.ForWinAPI;
  with ABitmap.Canvas do
  begin
    Brush.Style := bsClear;
    Font.Color := TextColor;
    TextRect(WinAPIClipRect, x, y, AText);
  end;
end;

class procedure TGUITools.DrawFitWOutlinedText(ABitmap: TBitmap;
  x1, x2, y: integer; const AText: string; TextColor, OutlineColor: TColor;
  Align: TAlignment);
var
  tw: integer;
  s: string;
begin
  with ABitmap.Canvas do
  begin
    s := AText;
    tw := TextWidth(s) + 2;
    if tw <= x2 - x1 + 1 then
      case Align of
        taLeftJustify:
          TGUITools.DrawOutlinedText(ABitmap,x1, y, AText, TextColor, OutlineColor);
        taRightJustify:
          TGUITools.DrawOutlinedText(ABitmap,x2-tw+1, y, AText, TextColor, OutlineColor);
        taCenter:
          TGUITools.DrawOutlinedText(ABitmap,x1 + ((x2-x1 - tw) div 2), y, AText, TextColor, OutlineColor);
      end
    else
    begin
      while (s <> '') and (tw > x2 - x1 + 1) do
      begin
        Delete(s, Length(s), 1);
        tw := TextWidth(s + '...') + 2;
      end;
      if tw <= x2 - x1 + 1 then
        TGUITools.DrawOutlinedText(ABitmap, x1, y, s+'...', TextColor, OutlineColor);
    end;
  end;
end;

class procedure TGUITools.DrawFitWOutlinedText(ACanvas: TCanvas;
  x1, x2, y: integer; const AText: string; TextColor, OutlineColor: TColor;
  Align: TAlignment);
var
  tw: integer;
  s: string;
begin
  with ACanvas do
  begin
    s := AText;
    tw := TextWidth(s) + 2;
    if tw <= x2 - x1 + 1 then
      case Align of
        taLeftJustify:
          TGUITools.DrawOutlinedText(ACanvas,x1, y, AText, TextColor, OutlineColor);
        taRightJustify:
          TGUITools.DrawOutlinedText(ACanvas,x2-tw+1, y, AText, TextColor, OutlineColor);
        taCenter:
          TGUITools.DrawOutlinedText(ACanvas,x1 + (x2-x1 - tw) div 2, y, AText, TextColor, OutlineColor);
       end
    else
    begin
      while (s <> '') and (tw > x2 - x1 + 1) do
      begin
        Delete(s, Length(s), 1);
        tw := TextWidth(s + '...') + 2;
      end;
      if tw <= x2 - x1 + 1 then
        TGUITools.DrawOutlinedText(ACanvas, x1, y, s+'...', TextColor, OutlineColor);
    end;
  end;
end;

class procedure TGUITools.FillGradientRectangle(ACanvas: TCanvas;
  Rect: T2DIntRect; ColorFrom: TColor; ColorTo: TColor;
  GradientKind: TBackgroundKind);
var
  Mesh: array of GRADIENTRECT;
  GradientVertice: array of TRIVERTEX;
  ConcaveColor: TColor;
begin
  case GradientKind of
    bkSolid:
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.brush.color := ColorFrom;
        ACanvas.fillrect(Rect.ForWinAPI);
      end;
    bkVerticalGradient, bkHorizontalGradient:
      begin
        SetLength(GradientVertice, 2);
        with GradientVertice[0] do
        begin
          x := Rect.left;
          y := Rect.top;
          Red := GetRValue(ColorFrom) shl 8;
          Green := GetGValue(ColorFrom) shl 8;
          Blue := GetBValue(ColorFrom) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[1] do
        begin
          x := Rect.Right + 1;
          y := Rect.bottom + 1;
          Red := GetRValue(ColorTo) shl 8;
          Green := GetGValue(ColorTo) shl 8;
          Blue := GetBValue(ColorTo) shl 8;
          Alpha := 255 shl 8;
        end;
        SetLength(Mesh, 1);
        Mesh[0].UpperLeft := 0;
        Mesh[0].LowerRight := 1;
        if GradientKind = bkVerticalGradient then
          GradientFill(ACanvas.Handle, @GradientVertice[0], 2, @Mesh[0], 1, GRADIENT_FILL_RECT_V)
        else
          GradientFill(ACanvas.Handle, @GradientVertice[0], 2, @Mesh[0], 1, GRADIENT_FILL_RECT_H);
      end;
    bkConcave:
      begin
        ConcaveColor:=TColorTools.Brighten(ColorFrom, 20);
        SetLength(GradientVertice, 4);
        with GradientVertice[0] do
        begin
          x := Rect.left;
          y := Rect.top;
          Red := GetRValue(ColorFrom) shl 8;
          Green := GetGValue(ColorFrom) shl 8;
          Blue := GetBValue(ColorFrom) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[1] do
        begin
          x := Rect.Right + 1;
          y := Rect.Top + (Rect.height) div 4;
          Red := GetRValue(ConcaveColor) shl 8;
          Green := GetGValue(ConcaveColor) shl 8;
          Blue := GetBValue(ConcaveColor) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[2] do
        begin
          x := Rect.left;
          y := Rect.Top + (Rect.height) div 4;
          Red := GetRValue(ColorTo) shl 8;
          Green := GetGValue(ColorTo) shl 8;
          Blue := GetBValue(ColorTo) shl 8;
          Alpha := 255 shl 8;
        end;
        with GradientVertice[3] do
        begin
          x := Rect.Right + 1;
          y := Rect.bottom + 1;
          Red := GetRValue(ColorFrom) shl 8;
          Green := GetGValue(ColorFrom) shl 8;
          Blue := GetBValue(ColorFrom) shl 8;
          Alpha := 255 shl 8;
        end;
        SetLength(Mesh, 2);
        Mesh[0].UpperLeft := 0;
        Mesh[0].LowerRight := 1;
        Mesh[1].UpperLeft := 2;
        Mesh[1].LowerRight := 3;
        GradientFill(ACanvas.Handle, @GradientVertice[0], 4, @Mesh[0], 2, GRADIENT_FILL_RECT_V);
      end;
  end;
end;

class procedure TGUITools.DrawFitWText(ACanvas: TCanvas; x1, x2, y: integer;
  const AText: string; TextColor: TColor; Align: TAlignment);
var
  tw: integer;
  s: string;
begin
  with ACanvas do
  begin
    s := AText;
    tw := TextWidth(s);
    // We draw if the text is changed
    if tw <= x2 - x1 + 1 then
      case Align of
        taLeftJustify  : TextOut(x1, y, AText);
        taRightJustify : TextOut(x2-tw+1, y, AText);
        taCenter       : TextOut(x1 + (x2-x1 - tw) div 2, y, AText);
      end
    else
    begin
      while (s <> '') and (tw > x2 - x1 + 1) do
      begin
        Delete(s, Length(s), 1);
        tw := TextWidth(s + '...');
      end;
      if tw <= x2 - x1 + 1 then
        TextOut(x1, y, s + '...');
    end;
  end;
end;

class procedure TGUITools.RenderBackground(ABuffer: TBitmap;
  Rect: T2DIntRect; Color1, Color2: TColor; BackgroundKind: TBackgroundKind);
var
  TempRect: T2DIntRect;
begin
  if ABuffer.PixelFormat<>pf24bit then
    raise Exception.Create('TGUITools.RenderBackground: Bitmap must be in 24-bit mode');
  if (Rect.Left > Rect.Right) or (Rect.Top > Rect.Bottom) then
    exit;

  // Both the FillRect method and the WinAPI gradient drawing are
  // protected from drawing outside the canvas area.
  case BackgroundKind of
    bkSolid:
      begin
        ABuffer.Canvas.Brush.Color := Color1;
        ABuffer.Canvas.Brush.Style := bsSolid;
        ABuffer.Canvas.FillRect(Rect.ForWinAPI);
      end;
    bkVerticalGradient:
      TGradientTools.VGradient(ABuffer.Canvas, Color1, Color2, Rect.ForWinAPI);
    bkHorizontalGradient:
      TGradientTools.HGradient(ABuffer.Canvas, Color1, Color2, Rect.ForWinAPI);
   bkConcave:
     begin
       {$IFDEF EnhancedRecordSupport}
       TempRect := T2DIntRect.Create(
       {$ELSE}
       TempRect.Create(
       {$ENDIF}
         Rect.Left, Rect.Top, Rect.Right, Rect.Top + (Rect.Bottom - Rect.Top) div 4
       );
       TGradientTools.VGradient(ABuffer.Canvas, Color1, TColorTools.Shade(Color1, Color2, 20), TempRect.ForWinAPI);
       {$IFDEF EnhancedRecordSupport}
       TempRect := T2DIntRect.Create(
       {$ELSE}
       TempRect.Create(
       {$ENDIF}
         Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top) div 4 + 1, Rect.Right, Rect.Bottom
       );
       TGradientTools.VGradient(ABuffer.Canvas, Color2, Color1, TempRect.ForWinAPI);
     end;
  end;  // case
end;

class procedure TGUITools.RestoreClipRgn(DC: HDC; OrgRgnExists: boolean;
  var OrgRgn: HRGN);
begin
  if OrgRgnExists then
    SelectClipRgn(DC, OrgRgn) else
    SelectClipRgn(DC, 0);
  DeleteObject(OrgRgn);
end;

class procedure TGUITools.SaveClipRgn(DC: HDC; var OrgRgnExists: boolean;
  var OrgRgn: HRGN);
var
  i: integer;
begin
  OrgRgn := CreateRectRgn(0, 0, 1, 1);
  i := GetClipRgn(DC, OrgRgn);
  OrgRgnExists := (i=1);
end;

class procedure TGUITools.DrawText(ABitmap: TBitmap; x, y: integer; const AText: string;
  TextColor: TColor);
begin
  with ABitmap.Canvas do
  begin
    Brush.Style := bsClear;
    Font.Color:= TextColor;
    TextOut(x, y, AText);
  end;
end;

class procedure TGUITools.DrawVLine(ABitmap: TBitmap; x, y1, y2: integer;
  Color: TColor);
var
  LineRect: T2DIntRect;
  BitmapRect: T2DIntRect;
begin
  if ABitmap.PixelFormat <> pf24bit then
    raise Exception.Create('TGUITools.DrawHLine: Bitmap must be in 24-bit mode!');

  EnsureOrder(y1, y2);

  {$IFDEF EnhancedRecordSupport}
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not (BitmapRect.IntersectsWith(T2DIntRect.Create(x, y1, x, y2), LineRect)) then
    exit;
  {$ELSE}
  BitmapRect.create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not (BitmapRect.IntersectsWith(Create2DIntRect(x, y1, x, y2), LineRect)) then
    exit;
  {$ENDIF}

  ABitmap.Canvas.Pen.color := Color;
  ABitmap.Canvas.Pen.style := psSolid;
  ABitmap.Canvas.Moveto(LineRect.Left, LineRect.Top);
  ABitmap.Canvas.Lineto(LineRect.Left, Linerect.Bottom+1);
end;

class procedure TGUITools.DrawVLine(ABitmap: TBitmap; x, y1, y2: integer;
  Color: TColor; ClipRect: T2DIntRect);
var
  OrgLineRect: T2DIntRect;
  LineRect: T2DIntRect;
  BitmapRect: T2DIntRect;
begin
  if ABitmap.PixelFormat <> pf24bit then
    raise Exception.Create('TGUITools.DrawHLine: Bitmap must be in 24-bit mode!');

  EnsureOrder(y1, y2);

  {$IFDEF EnhancedRecordSupport}
  BitmapRect := T2DIntRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not(BitmapRect.IntersectsWith(T2DIntRect.Create(x, y1, x, y2), OrgLineRect)) then
    exit;
  {$ELSE}
  BitmapRect.Create(0, 0, ABitmap.Width-1, ABitmap.Height-1);
  if not(BitmapRect.IntersectsWith(Create2DIntRect(x, y1, x, y2), OrgLineRect)) then
    exit;
  {$ENDIF}

  if not(OrgLineRect.IntersectsWith(ClipRect, LineRect)) then
    exit;

  ABitmap.Canvas.Pen.Color := Color;
  ABitmap.Canvas.Pen.Style := psSolid;
  ABitmap.Canvas.Moveto(LineRect.Left, LineRect.Top);
  ABitmap.Canvas.Lineto(LineRect.Left, Linerect.Bottom+1);
end;

class procedure TGUITools.DrawVLine(ACanvas: TCanvas; x, y1, y2: integer;
  Color: TColor);
begin
  EnsureOrder(y1, y2);
  ACanvas.Pen.Color := Color;
  ACanvas.Moveto(x, y1);
  ACanvas.Lineto(x, y2+1);
end;

class procedure TGUITools.DrawVLine(ACanvas: TCanvas; x, y1, y2: integer;
  Color: TColor; ClipRect: T2DIntRect);
var
  UseOrgClipRgn: boolean;
  ClipRgn: HRGN;
  OrgRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawVLine(ACanvas, x, y1, y2, Color);

  // Restores previous ClipRgn and removes used regions
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawAARoundFrame(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; Color: TColor);
begin
  if Radius < 1 then
    exit;

  if (Radius > Rect.Width div 2) or (Radius > Rect.Height div 2) then
    exit;

  // DrawAARoundCorner is protected against drawing outside the area
  {$IFDEF EnhancedRecordSupport}
  DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.Left, Rect.Top), Radius, cpLeftTop, Color);
  DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.Right - Radius + 1, Rect.Top), Radius, cpRightTop, Color);
  DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.Left, Rect.Bottom - Radius + 1), Radius, cpLeftBottom, Color);
  DrawAARoundCorner(ACanvas, T2DIntVector.create(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
  {$ELSE}
  DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.Left, Rect.Top), Radius, cpLeftTop, Color);
  DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.Right - Radius + 1, Rect.Top), Radius, cpRightTop, Color);
  DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.Left, Rect.Bottom - Radius + 1), Radius, cpLeftBottom, Color);
  DrawAARoundCorner(ACanvas, Create2DIntVector(Rect.Right - Radius + 1, Rect.Bottom - Radius + 1), Radius, cpRightBottom, Color);
  {$ENDIF}

  ACanvas.Pen.color := Color;
  ACanvas.pen.style := psSolid;

  // Draw * Line is protected against drawing outside the area
  DrawVLine(ACanvas, Rect.Left, Rect.Top + Radius, Rect.Bottom - Radius, Color);
  DrawVLine(ACanvas, Rect.Right, Rect.Top + Radius, Rect.Bottom - Radius, Color);
  DrawHLine(ACanvas, Rect.Left + Radius, Rect.Right - Radius, Rect.Top, Color);
  DrawHLine(ACanvas, Rect.Left + Radius, Rect.Right - Radius, Rect.Bottom, Color);
end;

class procedure TGUITools.DrawAARoundFrame(ACanvas: TCanvas; Rect: T2DIntRect;
  Radius: integer; Color: TColor; ClipRect: T2DIntRect);
var
  UseOrgClipRgn: boolean;
  ClipRgn: HRGN;
  OrgRgn: HRGN;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  DrawAARoundFrame(ACanvas, Rect, Radius, Color);

  // Restores previous ClipRgn and removes used regions
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawDisabledImage(ABitmap: TBitmap;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector;
  ClipRect: T2DIntRect);
begin
  DrawDisabledImage(ABitmap.Canvas, ImageList, ImageIndex, Point, ClipRect);
end;

class procedure TGUITools.DrawDisabledImage(ABitmap: TBitmap;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector);
begin
  DrawDisabledImage(ABitmap.Canvas, ImageList, ImageIndex, Point);
end;

class procedure TGUITools.DrawDisabledImage(ACanvas: TCanvas;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector;
  ClipRect: T2DIntRect);
var
  UseOrgClipRgn: Boolean;
  OrgRgn: HRGN;
  ClipRgn: HRGN;
  DCStackPos : integer;
begin
  // Store the original ClipRgn and set a new one
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);

  SelectClipRgn(ACanvas.Handle, ClipRgn);

  // Hack fixes the bug in ImageList.Draw which does not restore the previous one  /???
  // Font color for canvas
  DcStackPos := SaveDC(ACanvas.Handle);
  ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex, false);
  RestoreDC(ACanvas.Handle, DcStackPos);

  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);

  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawDisabledImage(ACanvas: TCanvas;
  Imagelist: TImageList; ImageIndex: integer; Point: T2DIntVector);
var
  DCStackPos : integer;
begin
  //todo: see if is necessary to save the DC
  DcStackPos := SaveDC(ACanvas.Handle);
  ImageList.Draw(ACanvas, Point.x, Point.y, ImageIndex, false);
  RestoreDC(ACanvas.Handle, DcStackPos);
end;

class procedure TGUITools.DrawCheckbox(ACanvas:TCanvas; x,y: Integer;
  AState: TCheckboxState; AButtonState:TSpkButtonState;
  AStyle: TSpkCheckboxStyle; ClipRect:T2DIntRect);
var
  UseOrgClipRgn: Boolean;
  OrgRgn: HRGN;
  ClipRgn: HRGN;
  te: TThemedElementDetails;
  Rect: TRect;
begin
  SaveClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  ClipRgn := CreateRectRgn(ClipRect.left, ClipRect.Top, ClipRect.Right+1, ClipRect.Bottom+1);
  if UseOrgClipRgn then
    CombineRgn(ClipRgn, ClipRgn, OrgRgn, RGN_AND);
  SelectClipRgn(ACanvas.Handle, ClipRgn);
  DrawCheckbox(ACanvas, x,y, AState, AButtonState, AStyle);
  RestoreClipRgn(ACanvas.Handle, UseOrgClipRgn, OrgRgn);
  DeleteObject(ClipRgn);
end;

class procedure TGUITools.DrawCheckbox(ACanvas: TCanvas; x,y: Integer;
  AState: TCheckboxState; AButtonState: TSpkButtonState;
  AStyle: TSpkCheckboxStyle);
const
  NOT_USED = tbCheckboxCheckedNormal;
const
  UNTHEMED_FLAGS: array [TSpkCheckboxStyle, TCheckboxState] of Integer = (
    (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED, DFCS_BUTTONCHECK or DFCS_BUTTON3STATE),
    (DFCS_BUTTONRADIO, DFCS_BUTTONRADIO or DFCS_CHECKED, DFCS_BUTTONRADIO or DFCS_BUTTON3STATE)
  );
  THEMED_FLAGS: array [TSpkCheckboxStyle, TCheckboxState, TSpkButtonState] of TThemedButton = (
    ( (tbCheckboxUncheckedNormal, tbCheckboxUncheckedHot, tbCheckboxUncheckedPressed, tbCheckboxUncheckedDisabled, NOT_USED),
      (tbCheckboxCheckedNormal, tbCheckboxCheckedHot, tbCheckboxCheckedPressed, tbCheckboxCheckedDisabled, NOT_USED),
      (tbCheckboxMixedNormal, tbCheckboxMixedHot, tbCheckboxMixedPressed, tbCheckboxMixedDisabled, NOT_USED)
    ),
    ( (tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled, NOT_USED),
      (tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled, NOT_USED),
      (tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled, NOT_USED)
    )
  );
var
  R: TRect;
  w: Integer;
  sz: TSize;
  te: TThemedElementDetails;
begin
  if ThemeServices.ThemesEnabled then begin
    te := ThemeServices.GetElementDetails(THEMED_FLAGS[AStyle, AState, AButtonState]);
    sz := ThemeServices.GetDetailSize(te);
    R := Bounds(x, y, sz.cx, sz.cy);
    InflateRect(R, 1, 1);
    ThemeServices.DrawElement(ACanvas.Handle, te, R);
  end else begin
    w := GetSystemMetrics(SM_CYMENUCHECK);
    R := Bounds(x, y, w, w);
    DrawFrameControl(
      ACanvas.Handle, R, DFC_BUTTON, UNTHEMED_FLAGS[AStyle, AState]);
  end;
end;

end.
