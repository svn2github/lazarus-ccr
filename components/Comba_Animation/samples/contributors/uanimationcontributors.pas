unit uanimationcontributors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, uanimationbasic,LCLIntf;

type

  { TAnimationTranslateLinear }

  TAnimationTranslateLinear=class(TAnimationItem)
  private
  protected
  public
    FStartPoint: TPoint;
    FEndPoint: TPoint;
  end;

  { TTextLineAnimation }

  TTextLineAnimation=class(TAnimationTranslateLinear)
  private
  protected
    FText: string;
    FLXTime: int64;
    FTextWidth: integer;
    FHighLight: Boolean;
    procedure DoPerform; override;
  public
    property Text: string read FText write FText;
    constructor Create(const aOrigin: TPoint; const aTarget: TPoint; const aDuration: int64); reintroduce;
  end;

  { TBackgroundAnimation }

  TBackgroundAnimation=class(TAnimationItem)
  private
  protected
    FBackground: TBitmap;
    procedure DoPerform; override;
  public
    constructor Create(const aBitmap: TBitMap);
    procedure LoadBackgroundFromFile;
    procedure LoadFromBitmap(const aBitmap: TBitmap);
    destructor Destroy; override;
  end;

  { TAboutTextZoomAnimation }

  TAboutTextZoomAnimation=class(TAnimationItem)
  private
  protected
    FText: string;
    FFinalXY: TPoint;
    FHidden: Boolean;
    procedure DoPerform; override;
    procedure DoFinalizeAnimation; override;
  public
  end;

  { TAboutAnimation }

  TAboutAnimation=class (TAnimationQueue)
  private
  protected
    FX: integer;
    FTarget: TBitmap;
    FPaintBuffer: TBitmap;
    FRefreshObject: TControl;
    FTextLines: TStringList;
    FViewPort: TRect;
    FAboutZoomEffect: TAboutTextZoomAnimation;
    FFullAnimationTime: int64;
    FMousePoint: TPoint;
    FCalculatedFontSize: integer;
    //Object to check upper and lower boundary animations
    FFirstLine: TTextLineAnimation;
    procedure PrepareLinesObjects;
  public
    property  PaintBuffer: TBitmap read FPaintBuffer;
    property  RefreshObject: TControl read FRefreshObject write FRefreshObject;
    property  MousePosition: TPoint read FMousePoint write FMousePoint;

    constructor Create(const aTargetImage: TBitmap);
    destructor Destroy; override;
    procedure Animate; override;
  end;

implementation

const
  EACHLINE_HEIGHT=20;
  EACHLINE_TIME=600; //Milliseconds
  INITIAL_TEXT_FONT_SIZE=13;
  TEXT_ZOOM_SIZE=80;
  COLOR_TEXT=clBlack;
  COLOR_TEXT_HIGHLIGHT=$FF0000;
  COLOR_OUTLINE=$E0E0E0;
  COLOR_ZOOM_TEXT=$C0C0C0;
  OUTLINE_OFFSET=1;

{ TAboutTextZoomAnimation }

procedure TAboutTextZoomAnimation.DoPerform;
var
  Factor: Single;
  Distance: integer;
  TheBMP: TBitmap;
  PreserveSize: integer;
begin
  inherited DoPerform;
  If FHidden then exit;
  if FText='' then Exit;
  Factor:=GetElapsedMilliseconds / Duration;
  Distance:=TEXT_ZOOM_SIZE-TAboutAnimation(FQueue).FCalculatedFontSize;
  Distance:=Distance-Trunc(Distance*Factor);
  TheBmp:=TAboutAnimation(FQueue).PaintBuffer;
  PreserveSize:=TheBMP.Canvas.Font.Size;
  TheBMP.Canvas.Font.Size:=Distance+TAboutAnimation(FQueue).FCalculatedFontSize;
  TheBMP.Canvas.Font.Color:=COLOR_ZOOM_TEXT;
  TheBMP.Canvas.TextOut(TheBmp.Canvas.Width div 2 - TheBMP.Canvas.TextWidth(FText) div 2,FFinalXY.y-distance*2,FText);
  TheBMP.Canvas.Font.Size:=PreserveSize;
  thebmp.Canvas.Font.Color:=clBlack;
end;

procedure TAboutTextZoomAnimation.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  FHidden:=true;
end;

{ TBackgroundAnimation }

procedure TBackgroundAnimation.LoadBackgroundFromFile;
var
  BackgroundLoad: TPicture;
begin
  TAboutAnimation(FQueue).FTarget.Canvas.Clear;
  BackgroundLoad:=TPicture.Create;
  try
  BackgroundLoad.LoadFromFile('..\images\back_400.jpg');
  except
  end;
  FBackground:=TBitmap.Create;
  FBackground.Assign(TAboutAnimation(FQueue).FTarget);
  //R.Left:=0;
  //R.Top:=0;
  //R.Right:=FBackground.Width;
  //R.Bottom:=FBackground.Height;
  //FBackground.Canvas.AntialiasingMode:=amOn;
  // Use StretchDraw if target does not matches source dimensions
  // FBackground.Canvas.StretchDraw(R,BackgroundLoad.Bitmap);
  FBackground.Canvas.Draw(0,0,BackgroundLoad.Bitmap);
  BackgroundLoad.Free;
end;

procedure TBackgroundAnimation.LoadFromBitmap(const aBitmap: TBitmap);
begin
  FBackground:=TBitmap.Create;
  FBackground.SetSize(aBitmap.Width,aBitmap.Height);
  FBackground.Canvas.Draw(0,0,aBitmap);
end;

procedure TBackgroundAnimation.DoPerform;
var
  TheBMP: TBitmap;
begin
  if not Assigned(FBackground) then begin
    LoadBackgroundFromFile;
  end;
  TheBmp:=TAboutAnimation(FQueue).PaintBuffer;
  TheBMP.Canvas.Draw(0,0,FBackground);
end;

constructor TBackgroundAnimation.Create(const aBitmap: TBitMap);
begin
  LoadFromBitmap(aBitmap);
end;

destructor TBackgroundAnimation.Destroy;
begin
  FreeAndNil(FBackground);
  inherited Destroy;
end;

{ TAboutAnimation }

procedure TAboutAnimation.PrepareLinesObjects;
var
  j: integer;
  EachLine: TTextLineAnimation;
  StartOffSet: integer;
  StartPoint,EndPoint: TPoint;
  EachLineDuration: int64;
  BlankZone: int64;
begin
  StartOffSet:=FTarget.Height;
  BlankZone:=int64(StartOffSet div EACHLINE_HEIGHT)*EACHLINE_TIME;
  EachLineDuration:=FTextLines.Count*int64(EACHLINE_TIME)+BlankZone;
  for j := 0 to FTextLines.Count-1 do begin
    StartPoint.x:=0;
    StartPoint.y:=j*EACHLINE_HEIGHT+StartOffSet;
    EndPoint.x:=0;
    EndPoint.y:=StartPoint.y+(FTextLines.Count*EACHLINE_HEIGHT)+StartOffSet;
    EachLine:=TTextLineAnimation.Create(StartPoint,EndPoint,EachLineDuration);
    EachLine.FreeWithQueue:=true;
    if Length(FTextLines[j])>0 then begin
      if FTextLines[j][1]='#' then begin
        EachLine.FHighLight:=true;
        FTextLines[j]:=copy(FTextLines[j],2,Length(FTextLines[j])-1);
      end;
    end;
    EachLine.Text:=FTextLines[j];
    EachLine.FTextWidth:=FPaintBuffer.Canvas.TextWidth(FTextLines[j]);
    Self.Add(EachLine);
    if j=0 then begin
      FFirstLine:=EachLine;
    end;
  end;
  FFullAnimationTime:=EachLineDuration;
end;

constructor TAboutAnimation.Create(const aTargetImage: TBitmap);
var
  AnimBackground: TBackgroundAnimation;
  AnimTextZoom: TAboutTextZoomAnimation;
  TextWidth: integer;
begin
  FTarget:=aTargetImage;
  FPaintBuffer:=TBitmap.Create;
  with FPaintBuffer do begin
    Assign(FTarget);
    Canvas.Brush.Color:=clWhite;
    Canvas.Brush.Style:=bsSolid;
    Canvas.Pen.Color:=clBlack;
    Canvas.FillRect(0,0,Width,Height);
  end;
  inherited Create;

  FTextLines:=TStringList.Create;
  try
    FTextLines.LoadFromFile('Contributors.txt');
  except
  end;

  if FTextLines.Count=0 then FTextLines.Add('Missing contributors.txt file.');

  FPaintBuffer.Canvas.Font.Size:=INITIAL_TEXT_FONT_SIZE+1;
  repeat
    FPaintBuffer.Canvas.Font.Size:=FPaintBuffer.Canvas.Font.Size-1;
    TextWidth:=FPaintBuffer.Canvas.TextWidth(FTextLines[0]);
  until TextWidth<FPaintBuffer.Canvas.Width;
  FCalculatedFontSize:=FPaintBuffer.Canvas.Font.Size;

  AnimBackground:=TBackgroundAnimation.Create(aTargetImage);
  aTargetImage.Canvas.Clear;
  AnimBackground.FreeWithQueue:=true;
  Self.Add(AnimBackground);

  AnimTextZoom:=TAboutTextZoomAnimation.Create;
  AnimTextZoom.Duration:=EACHLINE_TIME;
  AnimTextZoom.Repeats:=1;
  AnimTextZoom.FreeWithQueue:=true;
  AnimTextZoom.FText:='This is a zooming text cache';
  Self.Add(AnimTextZoom);
  Self.FAboutZoomEffect:=AnimTextZoom;

  PrepareLinesObjects;
end;

destructor TAboutAnimation.Destroy;
begin
  FTextLines.Free;
  FreeAndNil(FPaintBuffer);
  inherited Destroy;
end;

procedure TAboutAnimation.Animate;
var
  R1: TRect;
begin
  if FState<>eAnimationQueueStarted then exit;
  if Reversed then begin
    if FFirstLine.GetElapsedMilliseconds<0 then begin
      Reverse;
    end;
  end else begin
    if FFirstLine.GetElapsedMilliseconds>FFullAnimationTime then begin
      Start;
    end;
  end;
  R1.Top:=0;R1.Left:=0;
  FTarget.GetSize(R1.Right,R1.Bottom);
  FViewPort:=R1;
  // Is "Draw" faster than CopyRect ?
  // FTarget.Canvas.CopyRect(R1,FPaintBuffer.Canvas,R1);
  FTarget.Canvas.Draw(0,0,FPaintBuffer);
  if Assigned(FRefreshObject) then begin
    FRefreshObject.Refresh;
  end;
  inherited Animate;
end;

{ TTextLineAnimation }

constructor TTextLineAnimation.Create(const aOrigin: TPoint;
  const aTarget: TPoint; const aDuration: int64);
begin
  Duration:=aDuration;
  FStartPoint:=aOrigin;
  FEndPoint:=aTarget;
end;

procedure TTextLineAnimation.DoPerform;
var
  TheBMP: TBitmap;
  Factor: single;
  Distance: integer;
  NP: integer;
  LX: integer;
  LT: integer;
  bInBottonLine: Boolean;
  bIsMouseOver: Boolean=false;
  TextRect: TRect;
begin
  Factor:=GetElapsedMilliseconds / Duration;
  if Factor>1.0 then exit;
  Distance:=FEndPoint.y-FStartPoint.y;
  NP:=Trunc(Distance * Factor);
  NP:=FStartPoint.y-NP;
  if NP<TAboutAnimation(FQueue).FViewPort.Top-EACHLINE_HEIGHT then exit;
  if NP>TAboutAnimation(FQueue).FViewPort.Bottom then exit;
  LT:=(TAboutAnimation(FQueue).FViewPort.Right div 2) - (FTextWidth div 2);
  if (NP>TAboutAnimation(FQueue).FViewPort.Bottom-EACHLINE_HEIGHT) and (NP<=TAboutAnimation(FQueue).FViewPort.Bottom) then begin
    bInBottonLine:=true;
    if TAboutAnimation(FQueue).FAboutZoomEffect.FText<>FText then begin
      TAboutAnimation(FQueue).FAboutZoomEffect.FText:=FText;
      TAboutAnimation(FQueue).FAboutZoomEffect.FFinalXY.x:=LT;
      TAboutAnimation(FQueue).FAboutZoomEffect.FFinalXY.Y:=TAboutAnimation(FQueue).FViewPort.Bottom-EACHLINE_HEIGHT;
      TAboutAnimation(FQueue).FAboutZoomEffect.Start;
    end;
    if FLXTime=0 then FLXTime:=GetElapsedMilliseconds;
    LX:=TAboutAnimation(FQueue).FViewPort.Right;
    Distance:=LX-LT+10; // 10 stop pixels...
    Factor:=(GetElapsedMilliseconds - FLXTime) / EACHLINE_TIME;
    Factor:=Sin(Factor*pi/2);
    Distance:=Trunc(Distance * Factor);
    if Distance>(LX-LT) then Distance:=LX-LT;
    LX:=LX-Distance;
    NP:=TAboutAnimation(FQueue).FViewPort.Bottom-EACHLINE_HEIGHT;
  end else begin
    bInBottonLine:=false;
    LX:=LT;
  end;
  TextRect:=rect(LX,NP,FTextWidth+LX,NP+EACHLINE_HEIGHT);
  TheBmp:=TAboutAnimation(FQueue).PaintBuffer;

  TheBMP.Canvas.Brush.Style:=bsClear;
  if not bInBottonLine then begin
    TheBMP.Canvas.Font.Color:=COLOR_OUTLINE; //Very light gray
    TheBMP.Canvas.AntialiasingMode:=amOff;
    if OUTLINE_OFFSET>0 then begin
      TheBMP.Canvas.TextOut(LX+OUTLINE_OFFSET,NP+OUTLINE_OFFSET,FText);
      TheBMP.Canvas.TextOut(LX-OUTLINE_OFFSET,NP+OUTLINE_OFFSET,FText);
      TheBMP.Canvas.TextOut(LX+OUTLINE_OFFSET,NP-OUTLINE_OFFSET,FText);
      TheBMP.Canvas.TextOut(LX-OUTLINE_OFFSET,NP-OUTLINE_OFFSET,FText);
    end;
  end;

  if PtInRect(TextRect,(TAboutAnimation(FQueue).MousePosition)) then begin
    bIsMouseOver:=true;
  end;

  if FHighLight or bIsMouseOver then begin
    TheBMP.Canvas.Font.Color:=COLOR_TEXT_HIGHLIGHT;
    TheBMP.Canvas.Font.Style:=[fsUnderline];
  end else begin
    TheBMP.Canvas.Font.Color:=COLOR_TEXT;
  end;
//  TheBMP.Canvas.AntialiasingMode:=amOn;
  if not bInBottonLine then begin
    TheBMP.Canvas.TextOut(LX,NP,FText);
  end;
  if FHighLight or bInBottonLine or bIsMouseOver then begin
    TheBMP.Canvas.Font.Style:=[];
  end;
end;

end.

