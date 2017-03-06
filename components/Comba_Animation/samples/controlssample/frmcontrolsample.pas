unit frmcontrolsample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, uanimationbasic, uanimationcontrol
  (* Still not working
  , uanimationbitmap
  *)
  ;

type

  { TfrmAnimationControls }

  TfrmAnimationControls = class(TForm)
    BitBtn1: TBitBtn;
    butSliding: TBitBtn;
    butReverse: TButton;
    butPause: TButton;
    butBounceCaption: TButton;
    butDropDown: TButton;
    butAlphaBlend: TButton;
    butZoomTest: TButton;
    CheckBox1: TCheckBox;
    chkMetronome: TCheckBox;
    chkLabelFlashing: TCheckBox;
    chkMarquee: TCheckBox;
    grpButtons: TGroupBox;
    imgFlareMarquee: TImage;
    Label1: TLabel;
    lblFPS: TLabel;
    lblFlashingLabel: TLabel;
    lblDropDownMessage: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblMarqueeText: TLabel;
    lbSampleEntries: TListBox;
    Panel1: TPanel;
    pnlMarquee: TPanel;
    pnlAttached: TPanel;
    pnlDropDownContents: TPanel;
    pnlDropDownHeader: TPanel;
    pnlDropDown: TPanel;
    pnlText1: TPanel;
    pnlCheckbox: TPanel;
    Shape1: TShape;
    shpTri1: TShape;
    shpTri2: TShape;
    shpSliding: TShape;
    tmrAnimatedCadence: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure butAlphaBlendClick(Sender: TObject);
    procedure butBounceCaptionClick(Sender: TObject);
    procedure butDropDownClick(Sender: TObject);
    procedure butReverseClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure butSlidingClick(Sender: TObject);
    procedure butZoomTestClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure chkMarqueeChange(Sender: TObject);
    procedure chkMetronomeChange(Sender: TObject);
    procedure chkLabelFlashingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tmrAnimatedCadenceTimer(Sender: TObject);
  private
    { private declarations }
    AnimationQueue: TAnimationQueue;
    FDropDownExpandedHeight: Integer;
    FAnimateTri1: TAnimationControlTranslate;
    FAnimateTri2: TAnimationControlTranslate;
    procedure DropPanelEndAnimationCollapse(Sender: TAnimationItem);
    procedure MetronomeEndAnimation(Sender: TAnimationItem);
  public
    { public declarations }
  end;

  { TAnimationSampleBrushColor }

  TAnimationSampleBrushColor=class(TAnimationCustomControlColor)
  protected
    procedure DoChangeColor(const aNewColor: integer); override;
  public
    property InitialColor;
    property FinalColor;
    property TransitionMode;
  end;

var
  frmAnimationControls: TfrmAnimationControls;

implementation

{$R *.lfm}

{ TAnimationSampleBrushColor }

procedure TAnimationSampleBrushColor.DoChangeColor(const aNewColor: integer);
begin
  // Brush color instead Control.Color
  (Self.FControl as TShape).Brush.Color:=aNewColor;
end;

{ TfrmAnimationControls }

procedure TfrmAnimationControls.butBounceCaptionClick(Sender: TObject);
var
  Ef: TAnimationControlCaptionCapital;
begin
  AnimationQueue.RemoveGroupID(butBounceCaption);
  Ef:=TAnimationControlCaptionCapital.Create(butBounceCaption);
  {$PUSH}{$HINTS OFF} // Hide hint about use int64
  Ef.Duration:=50*Length(butBounceCaption.Caption);
  {$POP}
  ef.Repeats:=2;
  ef.AutoReverse:=true;
  ef.TransitionMode:=eAnimationTransitionBallisticBoth;
  EF.AnimationOnEndAction:=eAnimationOnEndFree;
  AnimationQueue.Add(Ef);
  EF.Start;
end;

procedure TfrmAnimationControls.butAlphaBlendClick(Sender: TObject);
var
  lAlphaBlend: TAnimationControlAlphaBlend;
begin
  AnimationQueue.RemoveGroupID(Self);
  lAlphaBlend:=TAnimationControlAlphaBlend.Create(Self);
  lAlphaBlend.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
  lAlphaBlend.Duration:=1000;
  lAlphaBlend.Repeats:=2;
  lAlphaBlend.AutoReverse:=true;
  lAlphaBlend.TransitionMode:=TAnimationTransitionMode.eAnimationTransitionLinear;
  lAlphaBlend.InitialBlend:=255;
  lAlphaBlend.FinalBlend:=0;
  lAlphaBlend.FinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
  AnimationQueue.Add(lAlphaBlend);
  lAlphaBlend.Start;
end;

procedure TfrmAnimationControls.BitBtn1Click(Sender: TObject);
(* Still not working
var
  lAnimBitmap: TAnimationControlBitBtnPicture;
*)
begin
  (* Still not working
  AnimationQueue.RemoveGroupID(BitBtn1);
  lAnimBitmap:=TAnimationControlBitBtnPicture.Create(BitBtn1);
  lAnimBitmap.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
  lAnimBitmap.Duration:=2000;
  lAnimBitmap.Repeats:=1;
  lAnimBitmap.InitialAngle:=0;
  lAnimBitmap.FinalAngle:=360;

  AnimationQueue.Add(lAnimBitmap);
  lAnimBitmap.Start;
  *)
end;

procedure TfrmAnimationControls.butDropDownClick(Sender: TObject);
var
  lDropAnimation: TAnimationControlZoom;
  lDropRotate: TAnimationControlFontRotator;
  lDropText: TAnimationControlCaptionReplace;
  lFinalRect: TRect;
  lList: TFPList;
  procedure ReverseList(aList: TFPList);
  var
    j: integer;
  begin
    for j := 0 to Pred(aList.Count) do begin
      if not TAnimationItem(lList[j]).Reverse then begin
        //writeln('Can''t reverse now');
      end;
    end;
  end;

begin
  lList:=AnimationQueue.FindGroupID(pnlDropDown);
  try
    if lList.Count>0 then begin
      // Reverse all the related controls
      ReverseList(lList);
      lList.Free;
      lList:=AnimationQueue.FindGroupID(lbSampleEntries);
      ReverseList(lList);
      lList.Free;
      lList:=AnimationQueue.FindGroupID(lblDropDownMessage);
      ReverseList(lList);
    end else begin
      AnimationQueue.RemoveGroupID(pnlDropDown);
      AnimationQueue.RemoveGroupID(lbSampleEntries);
      AnimationQueue.RemoveGroupID(lblDropDownMessage);

      lDropAnimation:=TAnimationControlZoom.Create(pnlDropDown);
      lDropAnimation.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
      lDropAnimation.Duration:=500;
      lDropAnimation.Repeats:=1;
      lDropAnimation.TransitionMode:=TAnimationTransitionMode.eAnimationTransitionBallisticBoth;

      lDropRotate:=TAnimationControlFontRotator.Create(lbSampleEntries);
      lDropRotate.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
      lDropRotate.Duration:=lDropAnimation.Duration;
      lDropRotate.Repeats:=lDropAnimation.Repeats;
      lDropRotate.TransitionMode:=TAnimationTransitionMode.eAnimationTransitionLinear;

      lDropText:=TAnimationControlCaptionReplace.Create(lblDropDownMessage);
      lDropText.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
      lDropText.Duration:=lDropAnimation.Duration;
      lDropText.Repeats:=lDropAnimation.Repeats;
      lDropText.TransitionMode:=TAnimationTransitionMode.eAnimationTransitionLinear;

      if butDropDown.Tag=0 then begin
        //writeln('Collapse');
        // DropDown panel is expanded
        // Store current height to recover when expand
        if FDropDownExpandedHeight=0 then begin
          FDropDownExpandedHeight:=pnlDropDown.Height;
        end;
        butDropDown.Caption:='_';
        lFinalRect:=pnlDropDown.BoundsRect;
        lFinalRect.Bottom:=pnlDropDownHeader.Height+pnlDropDown.Top;

        lDropAnimation.SetFinalSize(lFinalRect);
        lDropAnimation.OnAnimationEnd:=@DropPanelEndAnimationCollapse;

        lDropRotate.InitialAngle:=0;
        lDropRotate.FinalAngle:=90;

        lDropText.FinalText:='Now press the button again to expand the panel.';

      end else begin
        //writeln('Expand');
        butDropDown.Caption:='^';
        lFinalRect:=pnlDropDown.BoundsRect;
        lFinalRect.Bottom:=FDropDownExpandedHeight+pnlDropDown.Top;
        lDropAnimation.SetFinalSize(lFinalRect);
        lDropAnimation.OnAnimationEnd:=@DropPanelEndAnimationCollapse;

        lDropRotate.InitialAngle:=90;
        lDropRotate.FinalAngle:=0;

        pnlDropDownContents.Enabled:=true;

        lDropText.FinalText:='Press the button to collapse the panel below.';

      end;
      AnimationQueue.Add(lDropAnimation);
      AnimationQueue.Add(lDropRotate);
      AnimationQueue.Add(lDropText);
      lDropAnimation.Start;
      lDropRotate.Start;
      lDropText.Start;
    end;
  finally
    lList.Free;
  end;
end;

procedure TfrmAnimationControls.DropPanelEndAnimationCollapse(
  Sender: TAnimationItem);
begin
  // Disable contents so controls will not be reached by tabstop even when
  // its visual size is zero.
  if Sender.Reversed then begin
    if butDropDown.Tag=1 then begin
      // Reversed expansion, disable again
      //writeln('End reversed expand');
      pnlDropDownContents.Enabled:=false;
    end else begin
      // Reversed collapse, do nothing
      //writeln('End reversed collapse');
    end;
  end else begin
    if butDropDown.Tag=1 then begin
      // Finished expansion
      //writeln('End expand');
      butDropDown.Tag:=0;
    end else begin
      // Finished collapse
      //writeln('End collapse');
      butDropDown.Tag:=1;
      pnlDropDownContents.Enabled:=false;
    end;
  end;
end;

procedure TfrmAnimationControls.butReverseClick(Sender: TObject);
begin
  AnimationQueue.Reversed:=not AnimationQueue.Reversed;
end;

procedure TfrmAnimationControls.butPauseClick(Sender: TObject);
begin
  AnimationQueue.Paused:=not AnimationQueue.Paused;
end;

procedure TfrmAnimationControls.butSlidingClick(Sender: TObject);
var
  Button: TAnimationControlTranslate;
  Zoom: TAnimationControlZoom;
  lList: TFPList;
begin
  lList:=AnimationQueue.FindGroupID(butSliding);
  try
    if lList.Count>0 then begin
      // Do not start new animation until current finish.
      exit;
    end;
    Button:=TAnimationControlTranslate.Create(butSliding);
    Button.Duration:=1500;
    Button.Repeats:=4;
    Button.AnimationOnEndAction:=eAnimationOnEndFree;
    Button.AutoReverse:=true;
    Button.SetFinalPosition(Point(shpSliding.Left+shpSliding.Width-butSliding.Width,butSliding.Top));
    Button.AnchorLocation:=eAnimationAnchorLocationLeft;
    Button.FreeWithQueue:=true;
    Button.TransitionMode:=eAnimationTransitionBallisticBoth;
    Button.FinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
    AnimationQueue.Add(Button);

    Zoom:=TAnimationControlZoom.Create(butSliding);
    Zoom.Duration:=750;
    Zoom.Repeats:=2;
    Zoom.AutoReverse:=true;
    Zoom.AnimationOnEndAction:=eAnimationOnEndFree;
    Zoom.SetFinalZoom(0.1);
    Zoom.FreeWithQueue:=true;
    Zoom.ZoomMode:=eAnimationZoomModeHeightBottom;
    Zoom.FinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
    Zoom.RemoveAnchors:=true;
    Button.AddSyncStartStopAnimation(Zoom); //Synchronize restart/reverse action for Zoom when button restarts.
                                            //This can cause problems with Repeats counter
    AnimationQueue.Add(Zoom);

    Button.Start;
    Zoom.Start;
  finally
    lList.Free;
  end;
end;

procedure TfrmAnimationControls.butZoomTestClick(Sender: TObject);
var
  butAnim: TAnimationControlZoom;
begin
  // All animation references to butZoomTest must be removed
  // before as in the creation process we capture the current
  // dimensions. Removing ID before creation of new animation
  // first finalizes control and then captures original dimensions.
  // If not done this way starting butZoomTest dimensions will be
  // the current Zoom factor which is almost always small.
  AnimationQueue.RemoveGroupID(butZoomTest);
  butAnim:=TAnimationControlZoom.Create(butZoomTest);
  butAnim.SetFinalZoom(0.25);
  butAnim.ZoomMode:=TAnimationZoomMode.eAnimationZoomModeAll;
  butAnim.TransitionMode:=TAnimationTransitionMode.eAnimationTransitionLinear;
  butAnim.AutoReverse:=true;
  butAnim.Repeats:=2;
  butAnim.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
  butAnim.Duration:=100;
  butAnim.GroupID:=butZoomTest;
  AnimationQueue.Add(butAnim,true);
  butAnim.Start;
end;

procedure TfrmAnimationControls.CheckBox1Change(Sender: TObject);
var
  CheckboxAnim: TAnimationControlZoom;
  LightAnim: TAnimationControlColor;
begin
  AnimationQueue.RemoveGroupID(CheckBox1);

  CheckboxAnim:=TAnimationControlZoom.Create(CheckBox1);
  CheckboxAnim.SetFinalZoom(0.25);
  CheckboxAnim.ZoomMode:=TAnimationZoomMode.eAnimationZoomModeWidthLeft;
  CheckboxAnim.TransitionMode:=TAnimationTransitionMode.eAnimationTransitionLinear;
  CheckboxAnim.AutoReverse:=true;
  CheckboxAnim.Repeats:=2;
  CheckboxAnim.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
  CheckboxAnim.Duration:=250;
  AnimationQueue.Add(CheckboxAnim,true);
  CheckboxAnim.Start;


  LightAnim:=TAnimationControlColor.Create(pnlCheckbox);
  LightAnim.InitialColor:=ColorToRGB(clBtnFace);
  LightAnim.FinalColor:=$00FF00;
  LightAnim.TransitionMode:=TAnimationTransitionMode.eAnimationTransitionBallisticOut;
  LightAnim.AutoReverse:=true;
  LightAnim.Repeats:=2;
  LightAnim.AnimationOnEndAction:=TAnimationOnEndAction.eAnimationOnEndFree;
  LightAnim.Duration:=150;
  LightAnim.FinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
  AnimationQueue.Add(LightAnim);
  LightAnim.Start;
end;

procedure TfrmAnimationControls.chkMarqueeChange(Sender: TObject);
var
  lMarqueeBackground: TAnimationControlTranslate;
  lList: TFPList;
begin
  lList:=AnimationQueue.FindGroupID(imgFlareMarquee);
  try
    if lList.Count=0 then exit;
    lMarqueeBackground:=TObject(lList[0]) as TAnimationControlTranslate;
    if chkMarquee.Checked then begin
      if lMarqueeBackground.State=eAnimationStatePaused then begin
        lMarqueeBackground.Pause;
      end else begin
        lMarqueeBackground.Start;
      end;
    end else begin
      lMarqueeBackground.Pause;
    end;
  finally
    lList.Free;
  end;
end;

procedure TfrmAnimationControls.chkMetronomeChange(Sender: TObject);
var
  TriangleColor1,TriangleColor2: TAnimationSampleBrushColor;
begin
  if not chkMetronome.Checked then begin
    AnimationQueue.RemoveGroupID(shpTri1);
    AnimationQueue.RemoveGroupID(shpTri2);
  end else begin
    FAnimateTri1:=TAnimationControlTranslate.Create(shpTri1);
    FAnimateTri1.Duration:=1000;
    FAnimateTri1.AnimationOnEndAction:=eAnimationOnEndStop;
    FAnimateTri1.AutoReverse:=true;
    FAnimateTri1.Repeats:=2;
    FAnimateTri1.SetInitialPosition(Point((Self.Width div 2)-shpTri1.Width,shpTri1.Top));
    FAnimateTri1.SetFinalPosition(Point(0,shpTri1.Top));
    FAnimateTri1.FreeWithQueue:=true;
    FAnimateTri1.TransitionMode:=eAnimationTransitionBallisticIn;
    FAnimateTri1.FinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
    FAnimateTri1.OnAnimationEnd:=@MetronomeEndAnimation;
    AnimationQueue.Add(FAnimateTri1);

    TriangleColor1:=TAnimationSampleBrushColor.Create(shpTri1);
    TriangleColor1.Duration:=2000;
    TriangleColor1.AnimationOnEndAction:=eAnimationOnEndStop;
    TriangleColor1.AutoReverse:=false;
    TriangleColor1.Repeats:=1;
    TriangleColor1.InitialColor:=ColorToRGB(clRed);
    TriangleColor1.FinalColor:=ColorToRGB(clWhite);
    TriangleColor1.FreeWithQueue:=true;
    TriangleColor1.TransitionMode:=eAnimationTransitionBallisticOut;
    AnimationQueue.Add(TriangleColor1);

    FAnimateTri2:=TAnimationControlTranslate.Create(shpTri2);
    FAnimateTri2.Duration:=1000;
    FAnimateTri2.AnimationOnEndAction:=eAnimationOnEndStop;
    FAnimateTri2.AutoReverse:=true;
    FAnimateTri2.Repeats:=2;
    FAnimateTri2.SetInitialPosition(Point((Self.Width div 2),shpTri2.Top));
    FAnimateTri2.SetFinalPosition(Point(Self.Width-shpTri2.Width,shpTri2.Top));
    FAnimateTri2.FreeWithQueue:=true;
    FAnimateTri2.TransitionMode:=eAnimationTransitionBallisticIn;
    FAnimateTri2.FinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
    FAnimateTri2.OnAnimationEnd:=@MetronomeEndAnimation;
    AnimationQueue.Add(FAnimateTri2);

    TriangleColor2:=TAnimationSampleBrushColor.Create(shpTri2);
    TriangleColor2.Duration:=2000;
    TriangleColor2.AnimationOnEndAction:=eAnimationOnEndStop;
    TriangleColor2.AutoReverse:=false;
    TriangleColor2.Repeats:=1;
    TriangleColor2.InitialColor:=ColorToRGB(clRed);
    TriangleColor2.FinalColor:=ColorToRGB(clWhite);
    TriangleColor2.FreeWithQueue:=true;
    TriangleColor2.TransitionMode:=eAnimationTransitionBallisticOut;
    AnimationQueue.Add(TriangleColor2);

    TriangleColor1.Start;
    FAnimateTri1.Start;
  end;
end;

procedure TfrmAnimationControls.chkLabelFlashingChange(Sender: TObject);
var
  lFlashing: TAnimationControlColor;
  lList: TFPList;
begin
  if TCheckBox(Sender).Checked then begin
    AnimationQueue.RemoveGroupID(lblFlashingLabel);
    lFlashing:=TAnimationControlColor.Create(lblFlashingLabel);
    lFlashing.Duration:=1000;
    lFlashing.InitialColor:=ColorToRGB(clBtnFace);
    lFlashing.FinalColor:=ColorToRGB(TColor($000000FF));
    lFlashing.AutoReverse:=true;
    lFlashing.Repeats:=0; // infinite
    lFlashing.FinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
    AnimationQueue.Add(lFlashing);
    lFlashing.Start;
  end else begin
    lList:=AnimationQueue.FindGroupID(lblFlashingLabel);
    try
      if lList.Count>0 then begin
        lFlashing:=TObject(lList[0]) as TAnimationControlColor;
        lFlashing.FinalizeASAP(true);
      end;
    finally
      lList.Free;
    end;
  end;
end;

procedure TfrmAnimationControls.FormCreate(Sender: TObject);
var
  Button: TAnimationControlTranslate;
  MarqueeBackground: TAnimationControlTranslate;
begin
  AnimationQueue:=TAnimationQueue.Create;

  Button:=TAnimationControlTranslate.Create(butPause);
  Button.Duration:=2000;
  Button.AnimationOnEndAction:=eAnimationOnEndFree;
  Button.AutoReverse:=false;
  Button.Repeats:=1;
  Button.SetFinalPosition(Point(0,butPause.Top));
  Button.AnchorLocation:=eAnimationAnchorLocationCenter;
  Button.SwapOriginWithFinal;
  Button.FreeWithQueue:=true;
  Button.TransitionMode:=eAnimationTransitionBallisticIn;
  AnimationQueue.Add(Button);

  AnimationQueue.Start(true);

  MarqueeBackground:=TAnimationControlTranslate.Create(imgFlareMarquee);
  MarqueeBackground.Duration:=Trunc(imgFlareMarquee.Parent.Width*10);
  MarqueeBackground.AnimationOnEndAction:=eAnimationOnEndFree;
  MarqueeBackground.AutoReverse:=false;
  MarqueeBackground.Repeats:=0;
  MarqueeBackground.SetInitialPosition(Point(imgFlareMarquee.Width*-1,0));
  MarqueeBackground.SetFinalPosition(Point(imgFlareMarquee.Parent.Width,0));
  MarqueeBackground.AnchorLocation:=eAnimationAnchorLocationCenter;
  MarqueeBackground.FreeWithQueue:=true;
  MarqueeBackground.TransitionMode:=eAnimationTransitionLinear;
  AnimationQueue.Add(MarqueeBackground);
  MarqueeBackground.Stop;

end;

procedure TfrmAnimationControls.FormDestroy(Sender: TObject);
begin
  FreeAndNil(AnimationQueue);
end;

procedure TfrmAnimationControls.FormResize(Sender: TObject);
var
  lAnimations: TFPList;
  lFlareAnimation: TAnimationControlTranslate;
begin
  // Find the animation related to imgFlareMarquee
  lAnimations:=AnimationQueue.FindGroupID(imgFlareMarquee);
  try
    if lAnimations.Count=0 then begin
      // Not found ? Exit
      exit;
    end;
    try
      lFlareAnimation:=TObject(lAnimations[0]) as TAnimationControlTranslate;
      // Now readjust final position and animation duration
      // Changing duration we get a constant speed, but better is constant time in this case.
      //lFlareAnimation.Duration:=Trunc(lFlareAnimation.Control.Parent.Width*10);
      lFlareAnimation.SetFinalPosition(Point(lFlareAnimation.Control.Parent.Width,lFlareAnimation.Control.Top));
    except
      // Eat exceptions, so program will not break if something
      // goes really wrong.
    end;
  finally
    lAnimations.Free;
  end;
end;

procedure TfrmAnimationControls.MetronomeEndAnimation(Sender: TAnimationItem);
var
  lList: TFPList=nil;
  j: integer;
  A: TAnimationItem;
begin
  try
    if Sender=FAnimateTri1 then begin
      lList:=AnimationQueue.FindGroupID(shpTri2);
    end else begin
      lList:=AnimationQueue.FindGroupID(shpTri1);
    end;
    for j := 0 to Pred(lList.Count) do begin
      A:=TAnimationItem(lList[j]);
      A.Start;
    end;
  finally
    lList.Free;
  end;
end;

procedure TfrmAnimationControls.tmrAnimatedCadenceTimer(Sender: TObject);
begin
  Self.DisableAlign;
  AnimationQueue.Animate;
  Self.EnableAlign;
  // Adding Self.Invalidate improves smooth animation but can make
  // the form less responsive in low power machines.
  // When used removes some animation artifacts in controls translation.
  //Self.Invalidate;
  lblFPS.Caption:=format('FPS: %.2f',[AnimationQueue.AverageFPS]);
end;

end.

