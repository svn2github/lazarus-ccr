unit A3nalogGauge;

{.$DEFINE TICKER}

{$MODE DELPHI}

{$IFNDEF WINDOWS}
 {$UNDEF TICKER}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LCLProc, Types,
  {$IFDEF TICKER} Windows,{$ENDIF}      // for QueryPerformanceCounter
  SysUtils, Classes, Graphics, Controls;

type
  TAntialiased = (aaNone, aaBiline, aaTriline, aaQuadral);

  TStyle = (agsLeftStyle, agsRightStyle, agsCenterStyle);

  TFaceOption = (
    foShowMargin, foShowCircles, foShowMainTicks, foShowSubTicks,
    foShowIndicatorMin, foShowIndicatorMid, foShowIndicatorMax,
    foShowValues, foShowCenter, foShowFrame, foShow3D, foShowCaption
  );
  TFaceOptions = set of TFaceOption;

const
  DEFAULT_FACE_OPTIONS = [
    foShowMainTicks, foShowSubTicks, foShowIndicatorMax,
    foShowValues, foShowCenter, foShowFrame, foShow3D, foShowCaption
  ];

type
  TA3nalogGauge = class(TCustomControl)
  private
    // face elements colors
    FMinColor: TColor;
    FMidColor: TColor;
    FMaxColor: TColor;
    FFaceColor: TColor;
    FTicksColor: TColor;
    FValueColor: TColor;
    FCaptionColor: TColor;
    FArrowColor: TColor;
    FMarginColor: TColor;
    FCenterColor: TColor;
    FCircleColor: TColor;
    FFrameColor: TColor;
    // face elements sizes, etc.
    FCenterRadius: Integer;
    FCircleRadius: Integer;
    FScaleAngle: Integer;
    FMargin: Integer;
    FStyle: TStyle;
    FArrowWidth: Integer;
    FNumMainTicks: Integer;
    FLengthMainTicks: Integer;
    FLengthSubTicks: Integer;
    FFaceOptions: TFaceOptions;
    FCaptionFont: TFont;
    // values
    FPosition: Single;
    FScaleValue: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FCaption: string;
    // event handlers
    FOverMax: TNotifyEvent;
    FOverMin: TNotifyEvent;
    // anti-aliasing mode
    FAntiAliased: TAntialiased;
    // internal bitmaps
    FBackBitmap: TBitmap;
    FFaceBitmap: TBitmap;
    FAABitmap: TBitmap;
    FBitmapsValid: Boolean;
   {$IFDEF TICKER}
    // performance tracking
    FTicker: Int64;
    FPeriod: Int64;
    FFrames: Integer;
    FOnFrames: TNotifyEvent;
   {$ENDIF}
    // set properties
    procedure SetFrameColor(C: TColor);
    procedure SetMinColor(C: TColor);
    procedure SetMidColor(C: TColor);
    procedure SetMaxColor(C: TColor);
    procedure SetFaceColor(C: TColor);
    procedure SetTicksColor(C: TColor);
    procedure SetValueColor(C: TColor);
    procedure SetCaptionColor(C: TColor);
    procedure SetArrowColor(C: TColor);
    procedure SetMarginColor(C: TColor);
    procedure SetCenterColor(C: TColor);
    procedure SetCircleColor(C: TColor);
    procedure SetCenterRadius(I: Integer);
    procedure SetCircleRadius(I: Integer);
    procedure SetScaleAngle(I: Integer);
    procedure SetMargin(I: Integer);
    procedure SetStyle(S: TStyle);
    procedure SetArrowWidth(I: Integer);
    procedure SetNumMainTicks(I: Integer);
    procedure SetLengthMainTicks(I: Integer);
    procedure SetLengthSubTicks(I: Integer);
    procedure SetFaceOptions(O: TFaceOptions);
    procedure SetPosition(V: Single);
    procedure SetScaleValue(I: Integer);
    procedure SetMaximum(I: Integer);
    procedure SetMinimum(I: Integer);
    procedure SetCaption(const S: string);
    procedure SetAntiAliased(V: TAntialiased);
    procedure SetCaptionFont(AValue: TFont);
    function GetAAMultiplier: Integer;

  protected
    procedure CaptionFontChanged(Sender: TObject);
    procedure DrawScale(Bitmap: TBitmap; K: Integer);
    procedure DrawArrow(Bitmap: TBitmap; K: Integer);
    procedure FastAntiAliasPicture;
    procedure Loaded; override;
    procedure RedrawArrow;
    procedure RedrawScale;
    procedure Paint; override;
    procedure Resize; override;
    procedure FontChanged(Sender: TObject); override;
    class function GetControlClassDefaultSize: TSize; override;
    //procedure CMFontChanged(var Msg: TMessage); message CM_FontChanged;
    //procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Angle: Integer
      read FScaleAngle write SetScaleAngle default 120;
    property AntiAliased: TAntialiased
      read FAntiAliased write SetAntiAliased default aaNone;
    property ArrowColor: TColor
      read FArrowColor write SetArrowColor default clBlack;
    property ArrowWidth: Integer
      read FArrowWidth write SetArrowWidth default 1;
    property Caption: string
      read FCaption write SetCaption;
    property CaptionColor: TColor
      read FCaptionColor write SetCaptionColor default clBlack;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property CenterColor: TColor
      read FCenterColor write SetCenterColor default clDkGray;
    property CenterRadius: Integer
      read FCenterRadius write SetCenterRadius default 8;
    property CircleColor: TColor
      read FCircleColor write SetCircleColor default clBlue;
    property CircleRadius: Integer
      read FCircleRadius write SetCircleRadius default 3;
    property FaceColor: TColor
      read FFaceColor write SetFaceColor default clBtnFace;
    property FaceOptions: TFaceOptions
      read FFaceOptions write SetFaceOptions default DEFAULT_FACE_OPTIONS;
    property FrameColor: TColor
      read FFrameColor write SetFramecolor default clBtnShadow;
    property IndMaximum: Integer
      read FMaximum write SetMaximum default 80;
    property IndMinimum: Integer
      read FMinimum write SetMinimum default 20;
    property LengthMainTicks: Integer
      read FLengthMainTicks write SetLengthMainTicks default 15;
    property LengthSubTicks: Integer
      read FLengthSubTicks write SetLengthSubTicks default 8;
    property Margin: Integer
      read FMargin write SetMargin default 10;
    property MarginColor: TColor
      read FMarginColor write SetMarginColor default clSilver;
    property MaxColor: TColor
      read FMaxColor write SetMaxColor default clRed;
    property MidColor: TColor
      read FMidColor write SetMidColor default clYellow;
    property MinColor: TColor
      read FMinColor write SetMinColor default clGreen;
    property NumberMainTicks: Integer
      read FNumMainTicks write SetNumMainTicks default 5;
    property Position: Single
      read FPosition write SetPosition;
    property Scale: Integer
      read FScaleValue write SetScaleValue default 100;
    property Style: TStyle
      read FStyle write SetStyle default agsCenterStyle;
    property TicksColor: TColor
      read FTicksColor write SetTicksColor default clBlack;
    property ValueColor: TColor
      read FValueColor write SetValueColor default clBlack;
    property OnOverMax: TNotifyEvent read FOverMax write FOverMax;
    property OnOverMin: TNotifyEvent read FOverMin write FOverMin;
   {$IFDEF TICKER}
    property OnFrames: TNotifyEvent read FOnFrames write FOnFrames;
    property Frames: Integer read FFrames;
   {$ENDIF}

    property Align;
    property Anchors;
    property BorderSpacing;
    property Font;
  end;

procedure Register;

implementation

uses
  IntfGraphics, fpimage,
  Math;


{ TA3nalogGauge }

constructor TA3nalogGauge.Create(AOwner: TComponent);
var
  w, h: Integer;
begin
  inherited;
  FBackBitmap := TBitmap.Create;
  FFaceBitmap := TBitmap.Create;
  FAABitmap := nil;
  //*****************************defaults:****************************************
  with GetControlClassDefaultSize do begin
    SetInitialBounds(0, 0, CX, CY);
    w := CX;
    h := CY;
  end;
  Width := w;
  Height := h;
  FBackBitmap.Width := w;
  FBackBitmap.Height := h;
  FBackBitmap.Canvas.Brush.Style := bsClear;
  FBackBitmap.Canvas.Brush.Color := Self.Color;
  FFaceBitmap.Width := w;
  FFaceBitmap.Height := h;
  FFaceColor := clBtnFace;
  FFrameColor := clBtnShadow;
  FTicksColor := clBlack;
  FValueColor := clBlack;
  FCaptionColor := clBlack;
  FArrowColor := clBlack;
  FMarginColor := clSilver; //Black;
  FCenterColor := clDkGray;
  FCircleColor := clBlue;
  FMinColor := clGreen;
  FMidColor := clYellow;
  FMaxColor := clRed;
  FArrowWidth := 1;
  FPosition := 0;
  FMargin := 10;
  FStyle := agsCenterStyle;
  FScaleValue := 100;
  FMaximum := 80;
  FMinimum := 20;
  FScaleAngle := 120;
  FCircleRadius := 3;
  FCenterRadius := 8;
  FNumMainTicks := 5;
  FLengthMainTicks := 15;
  FLengthSubTicks := 8;
  FCaption := '';
  FFaceOptions := DEFAULT_FACE_OPTIONS;
  FAntiAliased := aaNone;
  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := CaptionFontChanged;
{$IFDEF TICKER}
  FTicker := -1;
  FFrames := 0;
  if not QueryPerformanceFrequency(FPeriod) then
    FPeriod := 0;
{$ENDIF}
end;

destructor TA3nalogGauge.Destroy;
begin
  FBackBitmap.Free;
  FFaceBitmap.Free;
  FAABitmap.Free;
  FCaptionFont.Free;
  inherited;
end;

{ ------------------------------------------------------------------------- }

procedure SetPenStyles(Pen: TPen; Width: Integer; Color: TColor);
begin
  Pen.Width := Width;
  Pen.Color := Color;
end;

procedure TA3nalogGauge.CaptionFontChanged(Sender: TObject);
begin
  RedrawScale;
end;

procedure TA3nalogGauge.DrawScale(Bitmap: TBitmap; K: Integer);
var
  I, J, X, Y, N, M, W, H, R: Integer;
  Max, Min: Int64;
  A, C, dA: Single;
  hFnt, hCapFnt, hTxt, wTxt: Integer;
  cosA, sinA: Extended;
  cosB, sinB: Extended;
  tm: TTextMetric;
  pt: TPoint;
  txt: String;
  txtDist: Integer;
begin
  W := Bitmap.Width;
  H := Bitmap.Height;
  Max := FMaximum;
  Min := FMinimum;
  N := FNumMainTicks*5;
  M := FMargin * K;
  R := FCircleRadius * K;
  txtDist := 10;

  with Bitmap do begin
    Canvas.Brush.Color := FFaceColor;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Font.Assign(Font);
    GetTextMetrics(Canvas.Handle, tm{%H-});
    hfnt := tm.tmHeight * K;
    Canvas.Font.Height := hFnt;

    { draw frame }
    if foShowFrame in FFaceOptions then begin
      if foShow3D in FFaceOptions then begin
        Canvas.Pen.Width := 2*K;
        Canvas.Pen.Color := clBtnShadow;
        Canvas.MoveTo(W, 0);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(0, H);
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.LineTo(W, H);
        Canvas.LineTo(W, 0);
      end else begin
        Canvas.Pen.Width := K;
        Canvas.Pen.Color := FFrameColor;
        Canvas.Rectangle(0, 0, W, H);
      end;
    end;

    { draw margins }
    if foShowMargin in FFaceOptions then begin
      Canvas.Pen.Color := FMarginColor;
      Canvas.Pen.Width := K;
      Canvas.Rectangle(M, M, W - M, H - M);
    end;

    { calculate center of scale }
    case fStyle of
      agsRightStyle:
        begin
          A := 0;
          C := W - M;
          X := W - M;
          Y := H - M;
          if H > W then
            J := W - 2*M
          else
            J := H - 2*M;
          if FScaleAngle > 90 then FScaleAngle := 90;
        end;
      agsLeftStyle:
        begin
          A := 90;
          C := M;
          X := M;
          Y := H - M;
          if H > W then
            J := W - 2*M
          else
            J := H - 2*M;
          if FScaleAngle > 90 then FScaleAngle := 90;
        end;
      agsCenterStyle:
        begin
          X := W div 2;
          A := (180 - fScaleAngle)/2;
          C := W/2;
          if FScaleAngle >= 180 then begin
            J := (W - 2*M) div 2;
            Y := H div 2;
          end else begin
            J := Round(((W - 2*M)/2) / cos(A*pi/180));
            if J > H - 2*M then J := H - 2*M;
            Y := (H - J) div 2 + J;
          end;
        end;
      else
        raise Exception.Create('Style unknown.');
    end;{case}

    { Draw caption }
    if (foShowCaption in FFaceOptions) then begin
      SinCos(DegToRad(A + FScaleAngle/2), sinA, cosA);
      Canvas.Font.Assign(FCaptionFont);
      GetTextMetrics(Canvas.Handle, tm);
      hCapFnt := tm.tmHeight * K;
      Canvas.Font.Height := hcapFnt;
      Canvas.Font.Color := FCaptionColor;
      pt := Point(Round(C - J/2 * cosA), Round(Y - J/2 * sinA));
      Canvas.TextOut(pt.X - Canvas.TextWidth(FCaption) div 2, pt.Y, FCaption);
    end;

    { Draw min/max indicator arcs }
    if (foShowIndicatorMax in FFaceOptions) then begin
      SetPenStyles(Canvas.Pen, 4 * K, FMaxColor);
      SinCos(DegToRad(A + FScaleAngle), sinA, cosA);
      SinCos(DegToRad(A + Max*FScaleAngle/FScaleValue), sinB, cosB);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
        Round(C - J * cosA),
        Round(Y - J * sinA),
        Round(C - J * cosB),
        Round(Y - J * sinB)
      );
    end;
    if (foShowIndicatorMid in FFaceOptions) and (FMinimum < FMaximum) then begin
      SetPenStyles(Canvas.Pen, 4 * K, FMidColor);
      SinCos(DegToRad(A + Max*FScaleAngle/FScaleValue), sinA, cosA);
      SinCos(DegToRad(A + Min*FScaleAngle/FScaleValue), sinB, cosB);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
        Round(C - J * cosA),
        Round(Y - J * sinA),
        Round(C - J * cosB),
        Round(Y - J * sinB)
      );
    end;
    if (foShowIndicatorMin in FFaceOptions) then begin
      SinCos(DegToRad(A + Min*FScaleAngle/FScaleValue), sinA, cosA);
      SinCos(DegToRad(A), sinB, cosB);
      SetPenStyles(Canvas.Pen, 4 * K, FMinColor);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
        Round(C - J * cosA),
        Round(Y - J * sinA),
        Round(C - J * cosB),
        Round(Y - J * sinB)
      );
    end;
    Canvas.Pen.Color := FTicksColor;
    Canvas.Pen.Width := K;

    { Draw subticks }
    if foShowSubTicks in fFaceOptions then
      for I := 0 to N do begin
        SinCos(DegToRad(A + I*FScaleAngle/N), sinA, cosA);
        Canvas.MoveTo(Round(C-(J-FLengthSubTicks*K)*cosA), Round(Y-(J-FLengthSubTicks*K)*sinA));
        Canvas.LineTo(round(C-(J-K)*cosA), round(Y-(J-K)*sinA));
      end;

    { Draw main ticks }
    for I := 0 to FNumMainTicks do begin
      dA := I * FScaleAngle / FNumMainTicks;
      if foShowMainTicks in fFaceOptions then begin
        SinCos(DegToRad(A + dA), sinA, cosA);
        Canvas.MoveTo(Round(C-(J-FLengthMainTicks*K)*cosA), Round(Y-(J-FLengthMainTicks*K)*sinA));
        Canvas.LineTo(Round(C-(J-K)*cosA), Round(Y-(J-K)*sinA));
      end;

      { Draw circles }
      if foShowCircles in fFaceOptions then begin
        SinCos(DegToRad(A + dA), sinA, cosA);
        Canvas.Brush.Color := FCircleColor;
        pt := Point(Round(C - J*cosA), Round(Y - J*sinA));
        Canvas.Ellipse(pt.X - R, pt.Y - R, pt.X + R, pt.Y + R);
      end;

      { Draw main tick values }
      if foShowValues in fFaceOptions then begin
        Canvas.Font.Assign(Self.Font);
        Canvas.Font.Height := hFnt;
        hTxt := Canvas.TextHeight('Tg');
        Canvas.Brush.Style := bsClear;
        SinCos(DegToRad(A + dA), sinA, cosA);
        txt := FormatFloat('0', I * fScaleValue div fNumMainTicks);
        wTxt := Canvas.TextWidth(txt);
        Canvas.TextOut(
          Round(C-(J-(FLengthMainTicks+txtDist)*K-I)*cosA) - wTxt div 2,
          Round(Y-(J-(FLengthMainTicks+txtDist)*K)*sinA) - hTxt div 2,
          txt
        );
      end;
    end;
  end
end;

procedure TA3nalogGauge.DrawArrow(Bitmap: TBitmap; K: Integer);
var
  J, X, Y, M, W, H, R: Integer;
  A, C: Single;
  cosA, sinA: Extended;
begin
  M := FMargin * K;
  R := FCenterRadius * K;
  W := Bitmap.Width;
  H := Bitmap.Height;

  with Bitmap do begin
    case FStyle of
      agsRightStyle:
        begin
          A := 0;
          C := W - M;
          X := W - M;        // X, Y: position of center circle
          Y := H - M;
          if H > W then
            J := W - 2*M     // J: Arrow length
          else
            J := H - 2*M;
          if FScaleAngle > 90 then FScaleAngle := 90;
        end;
      agsLeftStyle:
        begin
          A := 90;
          C := M;
          X := M;
          Y := H - M;
          if H > W then
            J := W - 2*M
          else
            J := H - 2*M;
          if FScaleAngle > 90 then FScaleAngle := 90;
        end;
      agsCenterStyle:
        begin
          X := W div 2;
          A := (180 - fScaleAngle)/2;
          C := W/2;
          if FScaleAngle >= 180 then begin
            J := (W - 2*M) div 2;
            Y := H div 2;
          end else begin
            J := Round(((W - 2*M)/2)/cos(A*pi/180));
            if J > H - 2*M then J := H - 2*M;
            Y := (H - J) div 2 + J;
          end;
        end;
      else
        raise Exception.Create('Style unknown');
    end;{case}

    SinCos((A + FPosition*FScaleAngle/FScaleValue)*pi/180, sinA, cosA);
    Canvas.Pen.Width := FArrowWidth * K;
    Canvas.Pen.Color := FArrowColor;
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(Round(C - J*cosA), Round(Y - J*sinA));

    { Draw center }
    if foShowCenter in FFaceOptions then begin
      Canvas.Brush.Color := FCenterColor;
      Canvas.Pen.Width := 1;
      Canvas.Ellipse(X - R, Y - R, X + R, Y + R);
    end;
  end;
end;

procedure TA3nalogGauge.RedrawArrow;
{$IFDEF TICKER}
var
  F: Integer;
  ticker: Int64;
begin
  if FTicker < 0 then
    if FPeriod = 0 then
      FTicker := GetTickCount64
    else
      QueryPerformanceCounter(FTicker);
{$ELSE}
begin
{$ENDIF}
  BitBlt(
    FFaceBitmap.Canvas.Handle, 0, 0,
    FBackBitmap.Width,
    FBackBitmap.Height,
    FBackBitmap.Canvas.Handle, 0, 0,
    SRCCOPY
  );
  DrawArrow(FFaceBitmap, GetAAMultiplier);

  if FAntialiased <> aaNone then
    FastAntiAliasPicture;

 {$IFNDEF LCL}
  Paint;
 {$ENDIF}

 {$IFDEF TICKER}
  if FPeriod = 0 then begin
    ticker := GetTickCount64;
    if ticker < FTicker then ticker := ticker + $100000000;
    F := 1000 div (ticker - FTicker)
  end else begin
    QueryPerformanceCounter(ticker);
    F := FPeriod div (ticker - FTicker)
  end;
  if F <> FFrames then begin
    FFrames := F;
    if Assigned(FOnFrames) then FOnFrames(Self)
  end;
  FTicker := -1;
 {$ENDIF}
 Invalidate;
end;

procedure TA3nalogGauge.RedrawScale;
begin
 {$IFDEF TICKER}
  if FPeriod = 0 then
    FTicker := GetTickCount64
  else
    QueryPerformanceCounter(FTicker);
 {$ENDIF}
  DrawScale(FBackBitmap, GetAAMultiplier);
  RedrawArrow;
  FBitmapsValid := true;
end;

procedure TA3nalogGauge.FastAntiAliasPicture;
var
  intfImgAA: TLazIntfImage;
  intfImgFace: TLazIntfImage;
  totR, totG, totB: Integer;
  x, dx, cx: Integer;
  y, dy, cy: Integer;
  k, k2: Integer;
  imgHandle, imgMaskHandle: HBitmap;
  clr: TFPColor;
begin
  intfImgAA := TLazIntfImage.Create(FAABitmap.Width, FAABitmap.Height);
  intfImgFace := TLazIntfImage.Create(FFaceBitmap.Width, FFaceBitmap.Height);
  try
    intfImgAA.LoadFromBitmap(FAABitmap.Handle, FAABitmap.MaskHandle);
    intfImgFace.LoadFromBitmap(FFaceBitmap.Handle, FFaceBitmap.MaskHandle);
    k := GetAAMultiplier;
    k2 := k * k;
    y := 0;
    while y < intfImgAA.Height do begin
      cy := y * k;
      x := 0;
      while x < intfImgAA.Width do begin
        cx := x * k;
        totR := 0;
        totG := 0;
        totB := 0;
        for dy := 0 to k-1 do begin
          for dx := 0 to k-1 do begin
            clr := intfImgFace.Colors[cx+dx, cy+dy];
            totR := totR + clr.Red;
            totG := totG + clr.Green;
            totB := totB + clr.Blue;
          end;
        end;
        clr := FPColor(totR div k2, totG div k2, totB div k2);
        intfImgAA.Colors[x, y] := clr;
        inc(x);
      end;
      inc(y);
    end;
    intfimgAA.CreateBitmaps(imgHandle, imgMaskHandle, false);
    FAABitmap.Handle := imgHandle;
    FAABitmap.MaskHandle := imgMaskHandle;
  finally
    intfImgAA.Free;
    intfImgFace.Free;
  end;
end;

// This code is faster than the version above, but crashes after a few seconds.
  {
procedure TA3nalogGauge.FastAntiAliasPicture;
const
  MaxPixelCount = MaxInt div SizeOf(TRGBTriple);
type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount-1] of TRGBTriple;
var
  intfImgAA: TLazIntfImage;
  intfImgFace: TLazIntfImage;
  totR, totG, totB: Integer;
  x, cx: Integer;
  y, cy: Integer;
  i, ci: Integer;
  k, k2: Integer;
  imgHandle, imgMaskHandle: HBitmap;
  Row1, Row2, Row3, Row4, DestRow: PRGBArray;
begin
  intfImgAA := TLazIntfImage.Create(FAABitmap.Width, FAABitmap.Height);
  intfImgFace := TLazIntfImage.Create(FFaceBitmap.Width, FFaceBitmap.Height);
  try
    intfImgAA.LoadFromBitmap(FAABitmap.Handle, FAABitmap.MaskHandle);
    FAABitmap.Clear; //ReleaseHandle;
    intfImgFace.LoadFromBitmap(FFaceBitmap.Handle, FFaceBitmap.MaskHandle);
    k := GetAAMultiplier;
    k2 := k * k;
    for y := 0 to intfImgAA.Height - 1 do begin
      // We compute samples of k x k pixels
      cy := y * k;
      // Get pointer to rows in supersampled image
      Row1 := intfImgFace.GetDataLineStart(cy);
      if k > 1 then Row2 := intfImgFace.GetDataLineStart(cy + 1);
      if k > 2 then Row3 := intfImgFace.GetDataLineStart(cy + 2);
      if k > 3 then Row4 := intfImgFace.GetDataLineStart(cy + 3);
      // Get a pointer to destination row in output image
      DestRow := intfImgAA.GetDataLineStart(y);
      // For each column...
      for x := 0 to intfImgAA.Width - 1 do begin
        // We compute samples of k x k pixels
        cx := x * k;
        // Initialize result colur
        totR := 0;
        totG := 0;
        totB := 0;
        if k > 3 then begin
          for i := 0 to 3 do begin
            ci := cx + i;
            inc(totR, Row1[ci].rgbtRed + Row2[ci].rgbtRed + Row3[ci].rgbtRed + Row4[ci].rgbtRed);
            inc(totG, Row1[ci].rgbtGreen + Row2[ci].rgbtGreen + Row3[ci].rgbtGreen + Row4[ci].rgbtGreen);
            inc(totB, Row1[ci].rgbtBlue + Row2[ci].rgbtBlue + Row3[ci].rgbtBlue + Row4[ci].rgbtBlue);
          end;
        end else
        if k > 2 then begin
          for i := 0 to 2 do begin
            ci := cx + i;
            inc(totR, Row1[ci].rgbtRed + Row2[ci].rgbtRed + Row3[ci].rgbtRed);
            inc(totG, Row1[ci].rgbtGreen + Row2[ci].rgbtGreen + Row3[ci].rgbtGreen);
            inc(totB, Row1[ci].rgbtBlue + Row2[ci].rgbtBlue + Row3[ci].rgbtBlue);
          end;
        end else
        if k > 1 then begin
          for i := 0 to 1 do begin
            ci := cx + i;
            inc(totR, Row1[ci].rgbtRed + Row2[ci].rgbtRed);
            inc(totG, Row1[ci].rgbtGreen + Row2[ci].rgbtGreen);
            inc(totB, Row1[ci].rgbtBlue + Row2[ci].rgbtBlue);
          end;
        end;
        DestRow[x].rgbtRed := totR div k2;
        DestRow[x].rgbtGreen := totG div k2;
        DestRow[x].rgbtBlue := totB div k2;
      end;
    end;
    intfimgAA.CreateBitmaps(imgHandle, imgMaskHandle, false);
    FAABitmap.Handle := imgHandle;
    FAABitmap.MaskHandle := imgMaskHandle;
  except
    intfImgAA.Free;
    intfImgFace.Free;
  end;
end;
}

procedure TA3nalogGauge.Loaded;
begin
  inherited;
  RedrawScale;
end;

procedure TA3nalogGauge.Paint;
begin
  if not FBitmapsValid then
    RedrawScale;

  if FAntiAliased = aaNone then
    Canvas.Draw(0, 0, FFaceBitmap)
  else
    Canvas.Draw(0, 0, FAABitmap);
end;

procedure TA3nalogGauge.FontChanged(Sender: TObject);
begin
  inherited;
  RedrawScale;
end;

class function TA3nalogGauge.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 280;
  Result.CY := 180;
end;

procedure TA3nalogGauge.Resize;
var
  K: Integer;
begin
  if Width < 60 then
    Width := 60;
  if Height < 50 then
    Height := 50;

  if FAntiAliased = aaNone then begin
    FBackBitmap.Width := Width;
    FBackBitmap.Height := Height;
    FFaceBitmap.Width := Width;
    FFaceBitmap.Height := Height;
  end else begin
    K := GetAAMultiplier;
    FBackBitmap.Width := Width * K;
    FBackBitmap.Height := Height * K;
    FFaceBitmap.Width := Width * K;
    FFaceBitmap.Height := Height * K;
    FAABitmap.Width := Width;
    FAABitmap.Height := Height;
  end;
  FBitmapsValid := false;
  inherited;
end;

procedure TA3nalogGauge.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TA3nalogGauge.SetFrameColor(C: TColor);
begin
  if C <> FFrameColor then begin
    FFrameColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetMinColor(C: TColor);
begin
  if C <> FMinColor then begin
    FMinColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetMidColor(C: TColor);
begin
  if C <> FMidColor then begin
    FMidColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetMaxColor(C: TColor);
begin
  if C <> FMaxColor then begin
    FMaxColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetFaceColor(C: TColor);
begin
  if C <> FFaceColor then begin
    FFaceColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetTicksColor(C: TColor);
begin
  if C <> FTicksColor then begin
    FTicksColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetValueColor(C: TColor);
begin
  if C <> FValueColor then begin
    FValueColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetCaptionColor(C: TColor);
begin
  if C <> FCaptionColor then begin
    FCaptionColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetArrowColor(C: TColor);
begin
  if C <> FArrowColor then begin
    FArrowColor := C;
    RedrawArrow;
  end;
end;

procedure TA3nalogGauge.SetMarginColor(C: TColor);
begin
  if C <> FMarginColor then begin
    FMarginColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetCenterColor(C: TColor);
begin
  if C <> FCenterColor then begin
    FCenterColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetCircleColor(C: TColor);
begin
  if C <> FCircleColor then begin
    FCircleColor := C;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetCenterRadius(I: Integer);
begin
  if I <> FCenterRadius then begin
    FCenterRadius := I;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetCircleRadius(I: Integer);
begin
  if I <> FCircleRadius then begin
    FCircleRadius := I;
    RedrawScale;
  end
end;

procedure TA3nalogGauge.SetScaleAngle(I: Integer);
begin
  if I <> FScaleAngle then begin
    if (I > 10) and (I <= 360) then
      FScaleAngle := I;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetMargin(I: Integer);
begin
  if I <> FMargin then begin
    FMargin := I;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetStyle(S: TStyle);
begin
  if S <> FStyle then begin
    FStyle := S;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetArrowWidth(I: Integer);
begin
  if I <> FArrowWidth then begin
    if I < 1 then
      FArrowWidth := 1
    else
    if I > 5 then
      FArrowWidth := 5
    else
      FArrowWidth := i;
    RedrawArrow;
  end
end;

procedure TA3nalogGauge.SetNumMainTicks(I: Integer);
begin
  if I <> FNumMainTicks then begin
    FNumMainTicks := I;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetLengthMainTicks(I: Integer);
begin
  if I <> FLengthMainTicks then begin
    FLengthMainTicks := I;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetLengthSubTicks(I: Integer);
begin
  if I <> FLengthSubTicks then begin
    FLengthSubTicks := I;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetFaceOptions(O: TFaceOptions);
begin
  if O <> FFaceOptions then begin
    FFaceOptions := O;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetPosition(V: Single);
begin
  if V <> FPosition then begin
    FPosition := V;
    if (FPosition > fMaximum) and Assigned(FOverMax) then OnOverMax(Self);
    if (FPosition < fMinimum) and Assigned(FOverMin) then OnOverMin(Self);
    RedrawArrow;
  end
end;

procedure TA3nalogGauge.SetScaleValue(I: Integer);
begin
  if I <> FScaleValue then begin
    if I > 1 then begin
      FScaleValue := I;
      if FMaximum >= FScaleValue then FMaximum := FScaleValue - 1;
      if FMinimum > FScaleValue - FMaximum then FMinimum := FScaleValue - fMaximum;
    end;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetMaximum(I: Integer);
begin
  if I <> FMaximum then begin
    if (I > 0) and (I < FScaleValue) then
      FMaximum := I;
    RedrawScale;
  end;
end;

procedure TA3nalogGauge.SetMinimum(I: Integer);
begin
  if I <> FMinimum then begin
    if (I > 0) and (I < FScaleValue) then
      FMinimum := I;
    RedrawScale;
  end
end;

procedure TA3nalogGauge.SetCaption(const S: string);
begin
  if S <> FCaption then begin
    Canvas.Font := Font;
    FCaption := S;
    RedrawScale;
  end
end;

procedure TA3nalogGauge.SetAntiAliased(V: TAntialiased);
var
  K: Integer;
begin
  if V <> FAntiAliased then begin
    FAntiAliased := V;
    if FAntiAliased = aaNone then begin
      FreeAndNil(FAABitmap);
      FreeAndNil(FBackBitmap);
      FreeAndNil(FFaceBitmap);
      FBackBitmap := TBitmap.Create;
      FFaceBitmap := TBitmap.Create;
      FBackBitmap.Width := Width;
      FFaceBitmap.Width := Width;
      FBackBitmap.Height := Height;
      FFaceBitmap.Height := Height;
    end else begin
      K := GetAAMultiplier;
      FBackBitmap.PixelFormat := pf24bit;
      FFaceBitmap.PixelFormat := pf24bit;
      FBackBitmap.Width := Width * K;
      FFaceBitmap.Width := Width * K;
      FBackBitmap.Height := Height * K;
      FFaceBitmap.Height := Height * K;
      if not Assigned(FAABitmap) then
        FAABitmap := TBitmap.Create;
      FAABitmap.PixelFormat := pf24bit;
      FAABitmap.Width := Width;
      FAABitmap.Height := Height;
    end;
    RedrawScale;
  end
end;

function TA3nalogGauge.GetAAMultiplier: Integer;
begin
  Result := ord(FAntiAliased) + 1;
end;


{ Register }

procedure Register;
begin
  RegisterComponents('Industrial', [TA3nalogGauge]);
end;

end.


