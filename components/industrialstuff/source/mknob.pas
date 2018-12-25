unit MKnob;

{ TmKnob : Marco Caselli's Knob Control rel. 1.0
 This component emulate the volume knob you could find on some HiFi devices;
**********************************************************************
* Feel free to use or give away this software as you see fit.        *
* Please leave the credits in place if you alter the source.         *
*                                                                    *
* This software is delivered to you "as is",                         *
* no guarantees of any kind.                                         *
*                                                                    *
* If you find any bugs, please let me know, I will try to fix them.  *
* If you modify the source code, please send me a copy               *
*                                                                    *
* If you like this component,  and also if you dislike it ;), please *
* send me an E-mail with your comment                                *
* Marco Caselli                                                      *
* Web site : http://members.tripod.com/dartclub                      *
* E-mail   : mcaselli@iname.com                                      *
*                                                                    *
* Thank to guy at news://marcocantu.public.italian.delphi            *
* for some math code. Check the site http://www.marcocantu.com       *
**********************************************************************
*** Sorry for my bad english ...............
 Properties :
      AllowUserDrag : Boolean; Specify if user can or not drag the control
                               to a new value using mouse;
      FaceColor : TColor;      Color of knob face;
      TickColor : TColor;      Color of tick mark;
      Position : Longint;      Current position of the knob;
      MarkStyle: TMarkStyle;   Specify style of the tick mark ( actually only
                               line or filled circle;
      RotationEffect:Boolean;    If True, the knob will shake emulating a rotation
                                  visual effect.
      Position:Longint;        Current value of knob;
      Max : Longint;           Upper limit value for Position;
      Min : Longint;           Lower limit value for Position;

 Events:
    property OnChange :        This event is triggered every time you change the
                               knob value;

 Lazarus port by W.Pamler
*******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  LclIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, math, ComCtrls;

const
  DEFAULT_KNOB_FACE_COLOR = $00B5CCBD;
  DEFAULT_KNOB_MARK_SIZE = 6;

type
  TKnobAngleRange = (
    arTop270, arTop180, arTop120, arTop90,
    arBottom270, arBottom180, arBottom120, arBottom90,
    arLeft270, arLeft180, arLeft120, arLeft90,
    arRight270, arRight180, arRight120, arRight90
  );
  TKnobChangeEvent = procedure(Sender: TObject; AValue: Longint) of object;
  TKnobMarkStyle = (msLine, msCircle, msTriangle);

  TmKnob = class(TCustomControl)
  private
    { Private declarations }
    FMaxValue: Integer;
    FMinValue: Integer;
    FCurValue: Integer;
    FFaceColor: TColor;
    FTickColor: TColor;
    FAllowDrag: Boolean;
    FOnChange: TKnobChangeEvent;
    FFollowMouse: Boolean;
    FMarkSize: Integer;
    FMarkStyle: TKnobMarkStyle;
    FAngleRange: TKnobAngleRange;
    FRotationEffect: Boolean;
    FTransparent: Boolean;
    function GetAngleOrigin: Double;
    function GetAngleRange: Double;
    procedure SetAllowDrag(AValue: Boolean);
    procedure SetAngleRange(AValue: TKnobAngleRange);
    procedure SetCurValue(AValue: Integer);
    procedure SetFaceColor(AColor: TColor);
    procedure SetMarkSize(AValue: Integer);
    procedure SetMarkStyle(AValue: TKnobMarkStyle);
    procedure SetMaxValue(AValue: Integer);
    procedure SetMinValue(AValue: Integer);
    procedure SetTickColor(AValue: TColor);
    procedure SetTransparent(AValue: Boolean);
    procedure UpdatePosition(X, Y: Integer);

  protected  { Protected declarations }
    procedure KnobChange;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

  published
    { Published declarations }
    property Align;
    property AllowUserDrag: Boolean read FAllowDrag write SetAllowDrag default True;
    property AngleRange: TKnobAngleRange read FAngleRange write SetAngleRange default arTop270;
    property BorderSpacing;
    property Color;
    property FaceColor: TColor read FFaceColor write SetFaceColor default DEFAULT_KNOB_FACE_COLOR;
    property TickColor: TColor read FTickColor write SetTickColor default clBlack;
    property Position: Integer read FCurValue write SetCurValue;
    property RotationEffect: Boolean read FRotationEffect write FRotationEffect default false;
    property Enabled;
    property MarkSize: Integer read FMarkSize write SetMarkSize default DEFAULT_KNOB_MARK_SIZE;
    property MarkStyle: TKnobMarkStyle read FMarkStyle write SetMarkStyle default msLine;
    property Max: Integer read FMaxValue write SetMaxValue default 100;
    property Min: Integer read FMinValue write SetMinvalue default 0;
    property OnChange: TKnobChangeEvent read FOnChange write FOnChange;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property Visible;
  end;

implementation

function Rotate(P, Center: TPoint; SinAngle, CosAngle: Double): TPoint;
begin
  P.X := P.X - Center.X;
  P.Y := P.Y - Center.Y;
  Result.X := round(cosAngle * P.X - sinAngle * P.Y) + Center.X;
  Result.Y := round(sinAngle * P.X + cosAngle * P.Y) + Center.Y;
end;

constructor TmKnob.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 60;
  Height := 60;
  FMaxValue := 100;
  FMinValue := 0;
  FCurValue := 0;
  FRotationEffect := false;
  FMarkStyle := msLine;
  FMarkSize := DEFAULT_KNOB_MARK_SIZE;
  FTickColor := clBlack;
  FFaceColor := DEFAULT_KNOB_FACE_COLOR;
  FFollowMouse := false;
  FAllowDrag := true;
  FAngleRange := arTop270;
  FTransparent := true;
end;

function TmKnob.GetAngleOrigin: Double;
const
  ORIGIN: array[TKnobAngleRange] of Double = (
      0,   0,   0,   0,
    180, 180, 180, 180,
     90,  90,  90,  90,
    270, 270, 270, 270
  );
begin
  Result := DegToRad(ORIGIN[FAngleRange]);
end;

function TmKnob.GetAngleRange: Double;
const
  ANGLE: array[TKnobAngleRange] of Double = (
    270, 180, 120, 90,
    270, 180, 120, 90,
    270, 180, 120, 90,
    270, 180, 120, 90
    );
begin
  Result := DegToRad(ANGLE[FAngleRange]);
end;

procedure TmKnob.KnobChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self, FCurValue);
end;

procedure TmKnob.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FAllowDrag then
  begin
    FFollowMouse := True;
    UpdatePosition(X,Y);
    Refresh;
  end;
end;

procedure TmKnob.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FFollowMouse := False;
end;

procedure TmKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FFollowMouse then
    UpdatePosition(X,Y)
end;

                        (*
procedure TmKnob.Paint;
var	R             : TRect;
       	Bm            : TBitMap;
        Co,Si,Angle   : Double;
        X, Y,W, H     : Integer;
        dx,dy,gx,gy   : Integer;
        OuterPoint    : TPoint;
begin
	{ Initialize offscreen BitMap }
   Bm := TBitMap.Create;
   if ( csdesigning in componentstate) then
      if Height < Width then
         Height:=Width
      else
         Width:=Height;

   Bm.Width := Width;
   Bm.Height := Height;
   Bm.Canvas.Brush.Color := clBTNFACE; //clWindow;
   R.Left := 0;
   R.Top := 0;
   R.Right := Width ;//- 1;
   R.Bottom := Height;// - 1;
   W := R.Right - R.Left -4;
   H := R.Bottom - R.Top -4;
   { This weird thing make knob "shake", emulating a rotation effect.
      Not so pretty, but I like it..............}
   if fRotationEffect then
      if (position mod 2) <> 0 then
         inc(h);

   Bm.Canvas.FillRect(R);

   with Bm.Canvas do
    begin

    Brush.Color := FaceColor;
    Pen.Width := 2;
    Pen.Color := Cl3dlight;
    ellipse(1, 1, W-1, H-1);
    Pen.Color := Clbtnshadow;
    ellipse(3, 3, W+2, H+2);

    Pen.Color := Clbtnface;
    Pen.Width := 1;
    RoundRect(2,2,w,h,w,h);
    Pen.Width := 3;
    Pen.Color := TickColor;

    if Position >= 0 then
    begin
      Brush.Color := FaceColor;
      X := W div 2;
      Y := H div 2;
      dX := W div 6;
      dY := H div 6;
      gX := W div 32;
      gY := H div 32;

      Angle:=(Position - (Min + Max)/2 ) / (Max - Min) * 5 ;

      Si:=Sin(Angle);
      Co:=Cos(Angle);
      OuterPoint.X:=Round(X + Si * (X-dx));
      OuterPoint.Y:=Round(Y - Co * (Y-dy));
      MoveTo(OuterPoint.X,OuterPoint.y);

      case MarkStyle of
        msLine   : LineTo(Round(X + Si * (X-gx)),Round(Y - Co * (Y-gy)));
  { this implementation of circle style is very poor but for my needing is enough}
        msCircle : begin
                   Brush.Color := TickColor;
                   RoundRect(OuterPoint.X-3, OuterPoint.Y-3,
                             OuterPoint.X+3, OuterPoint.Y+3,
                             OuterPoint.X+3, OuterPoint.Y+3);
                   end;
      end;
    end;
   end;

   Canvas.CopyMode := cmSrcCopy;
   Canvas.Draw(0, 0, Bm);
   bm.Destroy;
end;
             *)

procedure TmKnob.Paint;
const
  cPENWIDTH = 1;
  cMARGIN = 4*cPENWIDTH;
var
  R: TRect;
  bmp: TBitmap;
  Angle, sinAngle, cosAngle: Double;
  //X, Y,
  W, H: Integer;
  i: Integer;
  P: array[0..3] of TPoint;
  margin: Integer;
  markerSize: Integer;
  radius: Integer;
  ctr: TPoint;
  penwidth: Integer;
begin
  margin := Scale96ToFont(cMARGIN);
  penwidth := Scale96ToFont(cPENWIDTH);

  { Initialize offscreen BitMap }
  bmp := TBitmap.Create;
  try
    bmp.Width := Width;
    bmp.Height := Height;
    if FTransparent then
    begin
      bmp.Transparent := true;
      bmp.TransparentColor := clForm;
      bmp.Canvas.Brush.Color := bmp.TransparentColor;
    end else
    begin
      bmp.Transparent := false;
      if Color = clDefault then
        bmp.Canvas.Brush.Color := clForm
      else
        bmp.Canvas.Brush.Color := Color;
    end;
    ctr := Point(Width div 2, Height div 2);
    R := Rect(0, 0, Width, Height);
    W := R.Right - R.Left - margin;
    H := R.Bottom - R.Top - margin;
    if H < W then
      radius := H div 2
    else
      radius := W div 2;

    { This weird thing make knob "shake", emulating a rotation effect.
      Not so pretty, but I like it..............}
    if FRotationEffect and (Position mod 2 <> 0) then
      inc(H);

    with bmp.Canvas do
    begin
      FillRect(R);

      Brush.Color := FaceColor;
      Pen.Color := cl3dLight;
      Pen.Width := penwidth * 2;
      Pen.Style := psSolid;
      R := Rect(ctr.X, ctr.Y, ctr.X, ctr.Y);
      InflateRect(R, radius - penwidth, radius - penwidth);
      OffsetRect(R, -penwidth, -penwidth);
      Ellipse(R);

      Pen.Color := clBtnShadow;
      OffsetRect(R, 3*penwidth, 3*penwidth);
      Ellipse(R);

      Pen.Color := clBtnFace;
      Pen.Width := 1;
      OffsetRect(R, -2*penwidth, -2*penwidth);
      Ellipse(R);

      if Position >= 0 then
      begin
        markersize := radius * FMarkSize div 100;
        if markersize < 5 then markersize := 5;

        Angle := (Position - (Min + Max)/2 ) / (Max - Min) * GetAngleRange + GetAngleOrigin;
        SinCos(Angle, sinAngle, cosAngle);

        case MarkStyle of
          msLine:
            begin
              Pen.Width := 3;
              Pen.Color := TickColor;
              P[0] := Point(ctr.X, markersize);
              P[1] := Point(P[0].X, P[0].Y + markersize);
              for i:=0 to 1 do
                P[i] := Rotate(P[i], ctr, sinAngle, cosAngle);
              MoveTo(P[0].X, P[0].Y);
              LineTo(P[1].X, P[1].Y);
            end;
          msCircle:
            begin
              Brush.Color := TickColor;
              Pen.Style := psClear;
              P[0] := Rotate(Point(ctr.X, MARGIN + markersize + H div 32), ctr, sinAngle, cosAngle);
              R := Rect(P[0].X, P[0].Y, P[0].X, P[0].Y);
              InflateRect(R, markersize, markersize);
              Ellipse(R);
            end;
          msTriangle:
            begin
              Brush.Color := TickColor;
              Pen.Style := psClear;
              P[0] := Point(ctr.X, H div 32);
              P[1] := Point(P[0].X - markersize, P[0].Y + markersize*2);
              P[2] := Point(P[0].X + markersize, P[0].Y + markersize*2);
              P[3] := P[0];
              for i:=0 to High(P) do
                P[i] := Rotate(P[i], ctr, sinAngle, cosAngle);
              Polygon(P);
            end;
        end;
      end;
    end;

    Canvas.CopyMode := cmSrcCopy;
    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TmKnob.SetAllowDrag(AValue: Boolean);
begin
  if AValue <> FAllowDrag then
  begin
    FAllowDrag := AValue;
    Invalidate;
  end;
end;

procedure TmKnob.SetAngleRange(AValue: TKnobAngleRange);
begin
  if AValue <> FAngleRange then
  begin
    FAngleRange := AValue;
    Invalidate;
  end;
end;

procedure TmKnob.SetCurValue(AValue: Integer);
var
  tmp: Integer;
begin
  if AValue <> FCurValue then
  begin
    if FMinValue > FMaxValue then begin
      tmp := FMinValue;
      FMinValue := FMaxValue;
      FMaxValue := tmp;
    end;
    FCurValue := EnsureRange(AValue, FMinValue, FMaxValue);
    Invalidate;
    KnobChange;
  end;
end;

procedure TmKnob.SetFaceColor(AColor: TColor);
begin
  if FFaceColor <> AColor then begin
    FFaceColor := AColor;
    Invalidate;
  end;
end;

procedure TmKnob.SetMarkSize(AValue: Integer);
begin
  if AValue <> FMarkSize then
  begin
    FMarkSize := AValue;
    Invalidate;
  end;
end;

procedure TmKnob.SetMarkStyle(AValue: TKnobMarkStyle);
begin
  if AValue <> FMarkStyle then
  begin
    FMarkStyle := AValue;
    Invalidate;
  end;
end;

procedure TmKnob.SetMaxValue(AValue: Integer);
begin
  if AValue <> FMaxValue then
  begin
    FMaxValue := AValue;
    Invalidate;
  end;
end;

procedure TmKnob.SetMinValue(AValue: Integer);
begin
  if AValue <> FMinValue then
  begin
    FMinValue := AValue;
    Invalidate;
  end;
end;

procedure TmKnob.SetTickColor(AValue: TColor);
begin
  if AValue <> FTickColor then
  begin
    FTickColor := AValue;
    Invalidate;
  end;
end;

procedure TmKnob.SetTransparent(AValue: Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  Invalidate;
end;


procedure TmKnob.UpdatePosition(X, Y: Integer);
var
  CX, CY: integer;
  R: double;
  Angle: double;
begin
  CX := Width div 2;
  CY := Height div 2;
  R := Round(sqrt(sqr(CX-X) + sqr(CY-Y)));
  if R = 0 then R := 0.0001;

  if Y < CY then
    Angle := arcsin((X-CX)/R)
  else
  begin
    Angle := arcsin((CX-X)/R);
    if X > CX then
      Angle := Angle + Pi
    else
      Angle := Angle - Pi;
  end;
  Position := Round((Angle - GetAngleOrigin) * (Max - Min) / GetAngleRange + (Min + Max) / 2);
  Refresh;
end;


end.
