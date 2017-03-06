unit uanimationcontrol;

(*
  Comba - Animation controls module
  ---------------------------------
  @Licence: (c) 2017 José Mejuto // joshyfun at gmail.com
  @Licence: LGPL when compiled with FPC (Free Pascal), GNU GPL V3 in other cases.
  @Links:
     GPL:  https://www.gnu.org/licenses/gpl-3.0.en.html
     LGPL: https://www.gnu.org/licenses/lgpl-3.0.en.html

  @Description:

  This file implements an animation base class for LCL visual controls
  animation and implements some basic animations.

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms
  , uanimationbasic
  , uanimationtypes
  ;

type

  TAnimationZoomMode=(eAnimationZoomModeAll,eAnimationZoomModeWidthLeft,eAnimationZoomModeWidthRight,eAnimationZoomModeHeightTop,eAnimationZoomModeHeightBottom,eAnimationZoomModeWidth,eAnimationZoomModeHeight);
  TAnimationAnchorLocation=(eAnimationAnchorLocationLeftTop,eAnimationAnchorLocationLeft,eAnimationAnchorLocationTop,eAnimationAnchorLocationCenter);

  { TAnimationCustomControl }

  TAnimationCustomControl=class(TAnimationItem)
  private
  protected
    FControl: TControl;
    procedure DoFinalizeAnimation; override;
    function GetControl: TControl;
    property Control: TControl read FControl;
  public
    constructor Create(const aControl: TControl);
  end;

  TAnimationControl=class(TAnimationCustomControl)
  public
    property Control;
  end;

  { TAnimationCustomControlDimensions }

  TAnimationCustomControlDimensions=class(TAnimationControl)
  private
  protected
    FInitialRect: TAnimationRect;
    FFinalRect: TAnimationRect;
    procedure DoFinalizeAnimation; override;
    procedure DoMoveControlPosition(const aX,aY: integer); virtual;
    procedure DoMoveControlAsRect(const aRect: TRect); virtual;
  public
    constructor Create(const aControl: TControl);
    procedure SetInitialPosition(const aInitialPosition: TPoint); virtual;
    procedure SetFinalPosition(const aFinalPosition: TPoint); virtual;
    procedure SwapOriginWithFinal; virtual;
  end;

  { TAnimationControlTranslate }

  TAnimationControlTranslate=class(TAnimationCustomControlDimensions)
  protected
    FInitialCenter: TPoint;
    FFinalCenter: TPoint;
    FAnchorLocation: TAnimationAnchorLocation;
    FAnchors: TAnchors;
    FRemoveAnchors: Boolean;
    procedure DoPerform; override;
    procedure DoInitialize; override;
    procedure DoFinalizeAnimation; override;
  public
    procedure SetInitialPosition(const aInitialPosition: TPoint); override;
    procedure SetFinalPosition(const aFinalPosition: TPoint); override;
    property  RemoveAnchors: Boolean read FRemoveAnchors write FRemoveAnchors;
    property  AnchorLocation: TAnimationAnchorLocation read FAnchorLocation write FAnchorLocation;
    property  TransitionMode;
    property  FinalizeBehavior;
  end;

  { TAnimationControlZoom }

  TAnimationControlZoom=class(TAnimationCustomControlDimensions)
  protected
    FAutoSized: Boolean;
    FAnchors: TAnchors;
    FFinalZoom: single;
    FZoomMode: TAnimationZoomMode;
    FZoomByRectangle: Boolean;
    FRemoveAnchors: Boolean;
    procedure DoPerform; override;
    procedure DoInitialize; override;
    procedure DoFinalizeAnimation; override;
  public
    procedure SetFinalZoom(const aFinalZoom: Single);
    procedure SetFinalSize(const aRect: TRect);
    property  ZoomMode: TAnimationZoomMode read FZoomMode write FZoomMode;
    property  TransitionMode;
    property  FinalizeBehavior;
    property  RemoveAnchors: Boolean read FRemoveAnchors write FRemoveAnchors;
  end;

  { TAnimationControlCaptionCapital }

  TAnimationControlCaptionCapital=class(TAnimationControl)
  private
  protected
    FOriginalCaption: string;
    procedure DoInitialize; override;
    procedure DoPerform; override;
    procedure DoFinalizeAnimation; override;
  public
    property  TransitionMode;
  end;

  { TAnimationCustomControlColor }

  TAnimationCustomControlColor=class(TAnimationControl)
  private
  protected
    FInitialColor: integer;
    FFinalColor: integer;
    procedure DoPerform; override;
    procedure DoFinalizeAnimation; override;
    procedure DoChangeColor(const aNewColor: TColor); virtual; abstract;
    property InitialColor: integer read FInitialColor write FInitialColor;
    property FinalColor: integer read FFinalColor write FFinalColor;
  public
  end;

  { TAnimationControlColor }

  TAnimationControlColor=class(TAnimationCustomControlColor)
  private
  protected
    procedure DoChangeColor(const aNewColor: TColor); override;
  public
    property InitialColor;
    property FinalColor;
    property TransitionMode;
    property FinalizeBehavior;
  end;

  { TAnimationCustomControlRotator }

  TAnimationCustomControlRotator=class(TAnimationControl)
  private
  protected
    FInitialAngle: Single;
    FFinalAngle: Single;
    procedure DoPerform; override;
    procedure DoFinalizeAnimation; override;
    procedure DoRotate(const aAngle: Single); virtual; abstract;
    property InitialAngle: Single read FInitialAngle write FInitialAngle;
    property FinalAngle: Single read FFinalAngle write FFinalAngle;
  public
  end;

  { TAnimationControlFontRotator }

  TAnimationControlFontRotator=class(TAnimationCustomControlRotator)
  private
  protected
    procedure DoRotate(const aAngle: Single); override;
  public
    property InitialAngle;
    property FinalAngle;
    property TransitionMode;
  end;

  { TAnimationCustomControlTextChange }

  TAnimationCustomControlTextChange=class(TAnimationControl)
  private
    FSameLenInitialText: string;
    FSameLenFinalText: string;
    FMaxLength: integer;
  protected
    FInitialText: String;
    FFinalText: String;
    FCursorString: string;
    procedure DoPerform; override;
    procedure DoInitialize; override;
    procedure DoFinalizeAnimation; override;
    procedure DoChangeText(const aNewText: String); virtual; abstract;
    property  InitialText: String read FInitialText write FInitialText;
    property  FinalText: String read FFinalText write FFinalText;
    property  CursorString: String read FCursorString write FCursorString;
  public
  end;

  { TAnimationControlCaptionReplace }

  TAnimationControlCaptionReplace=class(TAnimationCustomControlTextChange)
  private
  protected
    procedure DoChangeText(const aNewText: String); override;
  public
    constructor Create(const aControl: TControl);
    property InitialText;
    property FinalText;
    property CursorString;
    property TransitionMode;
  end;

  { TAnimationCustomControlInteger }

  TAnimationCustomControlInteger=class(TAnimationControl)
  private
  protected
    FInitialInteger: Integer;
    FFinalInteger: Integer;
    procedure DoPerform; override;
    procedure DoFinalizeAnimation; override;
    procedure DoSetValue(const aValue: Integer); virtual; abstract;
    property InitialInteger: Integer read FInitialInteger write FInitialInteger;
    property FinalInteger: Integer read FFinalInteger write FFinalInteger;
  public
  end;

  { TAnimationControlAlphaBlend }

  TAnimationControlAlphaBlend=class(TAnimationCustomControlInteger)
  private
    function GetFinalBlend: Byte;
    function GetInitialBlend: Byte;
    procedure SetFinalBlend(AValue: Byte);
    procedure SetInitialBlend(AValue: Byte);
  protected
    procedure DoSetValue(const aValue: Integer); override;
    procedure DoInitialize; override;
  public
    constructor Create(const aForm: TCustomForm);
    property InitialBlend: Byte read GetInitialBlend write SetInitialBlend;
    property FinalBlend: Byte read GetFinalBlend write SetFinalBlend;
    property TransitionMode;
    property FinalizeBehavior;
  end;

implementation

{ TAnimationControlAlphaBlend }

function TAnimationControlAlphaBlend.GetFinalBlend: Byte;
begin
  Result:=Byte(FFinalInteger);
end;

function TAnimationControlAlphaBlend.GetInitialBlend: Byte;
begin
  Result:=Byte(FInitialInteger);
end;

procedure TAnimationControlAlphaBlend.SetFinalBlend(AValue: Byte);
begin
  if FFinalInteger=AValue then Exit;
  FFinalInteger:=AValue;
end;

procedure TAnimationControlAlphaBlend.SetInitialBlend(AValue: Byte);
begin
  if FInitialInteger=AValue then Exit;
  FInitialInteger:=AValue;
end;

procedure TAnimationControlAlphaBlend.DoSetValue(const aValue: Integer);
begin
  TCustomForm(FControl).AlphaBlendValue:=aValue;
end;

procedure TAnimationControlAlphaBlend.DoInitialize;
begin
  inherited DoInitialize;
  TCustomForm(FControl).AlphaBlend:=true;
end;

constructor TAnimationControlAlphaBlend.Create(const aForm: TCustomForm);
begin
  inherited Create(aForm);
  FInitialInteger:=aForm.AlphaBlendValue;
end;

{ TAnimationCustomControlInteger }

procedure TAnimationCustomControlInteger.DoPerform;
var
  NZ: Integer;
begin
  inherited DoPerform;
  NZ:=CalculateLinearPosition(FInitialInteger,FFinalInteger,GetElapsedMilliseconds,Duration);
  DoSetValue(NZ);
end;

procedure TAnimationCustomControlInteger.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  case FinalizeBehavior of
    eAnimationFinalizeBehaviorCurrent:
    begin
      // Do nothing
    end;
    eAnimationFinalizeBehaviorInitial:
    begin
      DoSetValue(FInitialInteger);
    end;
    eAnimationFinalizeBehaviorFinal:
    begin
      DoSetValue(FFinalInteger);
    end;
  end;
end;

{ TAnimationCustomControlTextChange }

procedure TAnimationCustomControlTextChange.DoPerform;
var
  NZ: Integer;
begin
  inherited DoPerform;
  NZ:=CalculateLinearPosition(1,FMaxLength,GetElapsedMilliseconds,Duration);
  // This is not UTF8 aware!!!
  DoChangeText(copy(FSameLenFinalText,1,NZ-1)+CursorString+copy(FSameLenInitialText,NZ));
end;

procedure TAnimationCustomControlTextChange.DoInitialize;
begin
  inherited DoInitialize;
  if Length(FInitialText)>Length(FFinalText) then begin
    FMaxLength:=Length(FInitialText);
  end else begin
    FMaxLength:=Length(FFinalText);
  end;
  FSameLenInitialText:=FInitialText+StringOfChar(' ',FMaxLength-Length(FInitialText));
  FSameLenFinalText:=FFinalText+StringOfChar(' ',FMaxLength-Length(FFinalText));
end;

procedure TAnimationCustomControlTextChange.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  case FinalizeBehavior of
    eAnimationFinalizeBehaviorCurrent:
    begin
      // Do nothing
    end;
    eAnimationFinalizeBehaviorInitial:
    begin
      DoChangeText(FInitialText);
    end;
    eAnimationFinalizeBehaviorFinal:
    begin
      DoChangeText(FFinalText);
    end;
  end;
end;

{ TAnimationControlCaptionReplace }

procedure TAnimationControlCaptionReplace.DoChangeText(const aNewText: String);
begin
  FControl.Caption:=aNewText;
end;

constructor TAnimationControlCaptionReplace.Create(const aControl: TControl);
begin
  inherited Create(aControl);
  FInitialText:=aControl.Caption;
  FFinalText:=aControl.Caption;
  // It proceduces a quite good eraser effect (8 spaces).
  FCursorString:=StringOfChar(' ',8);
end;

{ TAnimationControlFontRotator }

procedure TAnimationControlFontRotator.DoRotate(const aAngle: Single);
begin
  FControl.Font.Orientation:=trunc(aAngle*10);
end;

{ TAnimationCustomControlRotator }

procedure TAnimationCustomControlRotator.DoPerform;
var
  lDistance: Single;
begin
  inherited DoPerform;
  lDistance:=CalculateLinearPosition(FInitialAngle,FFinalAngle,GetElapsedMilliseconds,Duration);
  DoRotate(lDistance);
end;

procedure TAnimationCustomControlRotator.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  case FinalizeBehavior of
    eAnimationFinalizeBehaviorCurrent:
    begin
      // Do nothing
    end;
    eAnimationFinalizeBehaviorInitial:
    begin
      DoRotate(FInitialAngle);
    end;
    eAnimationFinalizeBehaviorFinal:
    begin
      DoRotate(FFinalAngle);
    end;
  end;
end;

{ TAnimationCustomColor }

procedure TAnimationCustomControlColor.DoPerform;
var
  tmpBaseColor: integer;
  tmpColor1: integer;
  tmpColor2: integer;
  NewColor: integer;
  NZ: Single;
  j: integer;
begin
  inherited DoPerform;
  NewColor:=0;
  NZ:=CalculateLinearPosition(1.0,0,GetElapsedMilliseconds,Duration);
  // Loop works in the RGB 24 bits colors (0..2)
  for j := 0 to 2 do begin
    tmpBaseColor:=(FFinalColor shr (8*j)) and 255;
    tmpColor2:=(FInitialColor shr (8*j)) and 255;
    tmpColor1:=trunc((tmpColor2-tmpBaseColor) * NZ);
    tmpColor1:=tmpBaseColor+tmpColor1;
    NewColor:=NewColor or (tmpColor1 shl (8*j));
  end;
  DoChangeColor(NewColor);
end;

procedure TAnimationCustomControlColor.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  case FinalizeBehavior of
    eAnimationFinalizeBehaviorCurrent:
    begin
      // Do nothing, is current
    end;
    eAnimationFinalizeBehaviorInitial:
    begin
      DoChangeColor(FInitialColor);
    end;
    eAnimationFinalizeBehaviorFinal:
    begin
      DoChangeColor(FFinalColor);
    end;
  end;
end;

{ TAnimationCustomControlDimensions }

procedure TAnimationCustomControlDimensions.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  case FinalizeBehavior of
    eAnimationFinalizeBehaviorCurrent:
    begin
      // Do nothing, it is current
    end;
    eAnimationFinalizeBehaviorInitial:
    begin
      DoMoveControlAsRect(FInitialRect.GetAsRect);
    end;
    eAnimationFinalizeBehaviorFinal:
    begin
      DoMoveControlAsRect(FFinalRect.GetAsRect);
    end;
  end;
  // When animated positions sometimes the control
  // ends with some fails in paint, so refresh it.
  FControl.Repaint;
end;

procedure TAnimationCustomControlDimensions.DoMoveControlPosition(const aX, aY: integer);
begin
  FControl.SetBounds(aX,aY,FControl.Width,FControl.Height);
end;

procedure TAnimationCustomControlDimensions.DoMoveControlAsRect(
  const aRect: TRect);
begin
  FControl.BoundsRect:=aRect;
end;

constructor TAnimationCustomControlDimensions.Create(const aControl: TControl);
begin
  inherited Create(aControl);
  FInitialRect.SetFromRect(aControl.BoundsRect);
  FFinalRect:=FInitialRect;
end;

procedure TAnimationCustomControlDimensions.SetInitialPosition(
  const aInitialPosition: TPoint);
begin
  FInitialRect.MoveTo(aInitialPosition);
end;

procedure TAnimationCustomControlDimensions.SetFinalPosition(
  const aFinalPosition: TPoint);
begin
  FFinalRect.MoveTo(aFinalPosition);
end;

procedure TAnimationCustomControlDimensions.SwapOriginWithFinal;
var
  Temp: TAnimationRect;
begin
  Temp:=FInitialRect;
  FInitialRect:=FFinalRect;
  FFinalRect:=Temp;
end;

{ TAnimationCustomControl }

procedure TAnimationCustomControl.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
end;

function TAnimationCustomControl.GetControl: TControl;
begin
  Result:=FControl;
end;

constructor TAnimationCustomControl.Create(const aControl: TControl);
begin
  inherited Create;
  FControl:=aControl;
  GroupID:=aControl;
end;

{ TAnimationControlCaptionCapital }

procedure TAnimationControlCaptionCapital.DoInitialize;
begin
  inherited DoInitialize;
  FOriginalCaption:=FControl.Caption;
end;

procedure TAnimationControlCaptionCapital.DoPerform;
var
  NZ: Integer;
begin
  inherited DoPerform;
  NZ:=CalculateLinearPosition(integer(1),integer(Length(FOriginalCaption)),GetElapsedMilliseconds,Duration);
  // This is not UTF8 aware!!!
  FControl.Caption:=copy(FOriginalCaption,1,NZ-1)+Uppercase(FOriginalCaption[NZ])+copy(FOriginalCaption,NZ+1);
end;

procedure TAnimationControlCaptionCapital.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  FControl.Caption:=FOriginalCaption;
end;

{ TAnimationControlCustomTranslate }

procedure TAnimationControlTranslate.DoPerform;
var
  NX,NY: integer;
begin
  inherited DoPerform;
  case FAnchorLocation of
    eAnimationAnchorLocationLeft:
      begin
        NX:=CalculateLinearPosition(FInitialRect.Left,FFinalRect.Left,GetElapsedMilliseconds,Duration);
        DoMoveControlPosition(NX,FControl.Top);
      end;
    eAnimationAnchorLocationTop:
      begin
        NY:=CalculateLinearPosition(FInitialRect.Top,FFinalRect.Top,GetElapsedMilliseconds,Duration);
        DoMoveControlPosition(FControl.Left,NY);
      end;
    eAnimationAnchorLocationLeftTop:
      begin
        NX:=CalculateLinearPosition(FInitialRect.Left,FFinalRect.Left,GetElapsedMilliseconds,Duration);
        NY:=CalculateLinearPosition(FInitialRect.Top,FFinalRect.Top,GetElapsedMilliseconds,Duration);
        DoMoveControlPosition(NX,NY);
      end;
    eAnimationAnchorLocationCenter:
      begin
        NX:=CalculateLinearPosition(FInitialCenter.x,FFinalCenter.x,GetElapsedMilliseconds,Duration);
        NX:=NX-FControl.Width div 2;
        NY:=CalculateLinearPosition(FInitialCenter.y,FFinalCenter.y,GetElapsedMilliseconds,Duration);
        NY:=NY-FControl.Height div 2;
        DoMoveControlPosition(NX,NY);
      end;
  end;
end;

procedure TAnimationControlTranslate.DoInitialize;
begin
  inherited DoInitialize;
  FInitialCenter:=FInitialRect.GetCenter;
  FFinalCenter:=FFinalRect.GetCenter;
  //Anchors could be a problem sometimes
  FAnchors:=FControl.Anchors;
  if FRemoveAnchors then begin
    FControl.Anchors:=[];
  end;
end;

procedure TAnimationControlTranslate.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  if FRemoveAnchors then begin
    FControl.Anchors:=FAnchors;
  end;
end;

procedure TAnimationControlTranslate.SetInitialPosition(
  const aInitialPosition: TPoint);
begin
  inherited SetInitialPosition(aInitialPosition);
  FInitialCenter:=FInitialRect.GetCenter;
end;

procedure TAnimationControlTranslate.SetFinalPosition(
  const aFinalPosition: TPoint);
begin
  inherited SetFinalPosition(aFinalPosition);
  FFinalCenter:=FFinalRect.GetCenter;
end;

{ TAnimationControlZoom }

procedure TAnimationControlZoom.DoPerform;
var
  NZ: Single;
  NRect: TAnimationRect;
  NewBottom: integer;
  Center: TPoint;
  AnimRect: TAnimationRect;
  TR: TAnimationRect;
begin
  inherited DoPerform;
  if not FZoomByRectangle then begin
    NZ:=CalculateLinearPosition(1.0,FFinalZoom,GetElapsedMilliseconds,Duration);
    case FZoomMode of
      eAnimationZoomModeAll:
        begin
          AnimRect.SetFromRect(FControl.BoundsRect);
          Center:=AnimRect.GetCenter;
          TR.Left:=Center.x-Round(FInitialRect.Width*NZ) div 2;
          TR.Right:=TR.Left+Round(FInitialRect.Width*NZ);
          TR.Top:=Center.y-Round(FInitialRect.Height*NZ) div 2;
          TR.Bottom:=TR.Top+Round(FInitialRect.Height*NZ);
          DoMoveControlAsRect(TR.GetAsRect);
        end;
      eAnimationZoomModeWidth:
        begin
          TR.SetFromRect(FControl.BoundsRect);
          Center:=TR.GetCenter;
          TR.Left:=Center.x-Round(FInitialRect.Width*NZ) div 2;
          TR.Width:=Round(FInitialRect.Width*NZ);
          DoMoveControlAsRect(TR.GetAsRect);
        end;
      eAnimationZoomModeHeight:
        begin
          TR.SetFromRect(FControl.BoundsRect);
          Center:=TR.GetCenter;
          TR.Top:=Center.y-Round(FInitialRect.Height*NZ) div 2;
          TR.Height:=Round(FInitialRect.Height*NZ);
          DoMoveControlAsRect(TR.GetAsRect);
        end;
      eAnimationZoomModeWidthLeft:
        begin
          TR.SetFromRect(FControl.BoundsRect);
          TR.Width:=Round(FInitialRect.Width*NZ);
          DoMoveControlAsRect(TR.GetAsRect);
        end;
      eAnimationZoomModeWidthRight:
        begin
          TR.SetFromRect(FControl.BoundsRect);
          TR.Left:=TR.Right-integer(Round(FInitialRect.Width*NZ));
          TR.Width:=Round(FInitialRect.Width*NZ);
          DoMoveControlAsRect(TR.GetAsRect);
        end;
      eAnimationZoomModeHeightTop:
        begin
          TR.SetFromRect(FControl.BoundsRect);
          TR.Height:=Round(FInitialRect.Height*NZ);
          DoMoveControlAsRect(TR.GetAsRect);
        end;
      eAnimationZoomModeHeightBottom:
        begin
          TR.SetFromRect(FControl.BoundsRect);
          NewBottom:=FControl.Top+FControl.Height;
          TR.Top:=NewBottom - Round(FInitialRect.Height*NZ);
          TR.Height:=Round(FInitialRect.Height*NZ);
          DoMoveControlAsRect(TR.GetAsRect);
        end;
    end;
  end else begin
    NRect.Left:=CalculateLinearPosition(FInitialRect.Left,FFinalRect.Left,GetElapsedMilliseconds,Duration);
    NRect.Top:=CalculateLinearPosition(FInitialRect.Top,FFinalRect.Top,GetElapsedMilliseconds,Duration);
    NRect.Width:=CalculateLinearPosition(FInitialRect.Width,FFinalRect.Width,GetElapsedMilliseconds,Duration);
    NRect.Height:=CalculateLinearPosition(FInitialRect.Height,FFinalRect.Height,GetElapsedMilliseconds,Duration);
    DoMoveControlAsRect(NRect.GetAsRect);
  end;
end;

procedure TAnimationControlZoom.DoInitialize;
begin
  inherited DoInitialize;
  if FControl.AutoSize then begin
    FAutoSized:=true;
    //Autosize and zoom effect do not work well together ;)
    FControl.AutoSize:=false;
  end else begin
    FAutoSized:=false;
  end;
  //Anchors can influence zoom in some cases
  FAnchors:=FControl.Anchors;
  if FRemoveAnchors then begin
    FControl.Anchors:=[];
  end;
end;

procedure TAnimationControlZoom.DoFinalizeAnimation;
begin
  inherited DoFinalizeAnimation;
  if FAutoSized then FControl.AutoSize:=true;
  if FRemoveAnchors then begin
    FControl.Anchors:=FAnchors;
  end;
end;

procedure TAnimationControlZoom.SetFinalZoom(const aFinalZoom: Single);
begin
  FFinalZoom:=aFinalZoom;
  FZoomByRectangle:=false;
end;

procedure TAnimationControlZoom.SetFinalSize(const aRect: TRect);
begin
  FFinalRect.SetFromRect(aRect);
  FZoomByRectangle:=true;
end;

{ TAnimationControlColor }

procedure TAnimationControlColor.DoChangeColor(const aNewColor: TColor);
begin
  FControl.Color:=aNewColor;
end;

end.

