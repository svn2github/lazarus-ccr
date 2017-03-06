unit uanimationbasic;

(*
  Comba - Animation main module
  -----------------------------
  @Licence: (c) 2017 José Mejuto // joshyfun at gmail.com
  @Licence: LGPL when compiled with FPC (Free Pascal), GNU GPL V3 in other cases.
  @Links:
     GPL:  https://www.gnu.org/licenses/gpl-3.0.en.html
     LGPL: https://www.gnu.org/licenses/lgpl-3.0.en.html

  @Description:

  This file implements an animation queue and animation item base class.

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, math;

type

  TAnimationTransitionMode=(eAnimationTransitionLinear,eAnimationTransitionBallisticIn,eAnimationTransitionBallisticOut,eAnimationTransitionBallisticBoth,eAnimationTransitionBallisticEdge);
  TAnimationFinalizeBehavior=(eAnimationFinalizeBehaviorCurrent,eAnimationFinalizeBehaviorInitial,eAnimationFinalizeBehaviorFinal);

  TAnimationOnEndAction=(eAnimationOnEndStop,eAnimationOnEndFree);
  TAnimationState=(eAnimationStateStopped,eAnimationStateStarted,eAnimationStatePaused,eAnimationStateToBeFreed);

  TAnimationItem=class;

  TAnimationOnPerform=procedure (const aElapedMilliseconds: int64) of object;
  TAnimationOnAnimationEnd=procedure (Sender: TAnimationItem) of object;
  TAnimationQueue=class;
  TAnimationException=class(Exception);

  { TAnimationItem }

  TAnimationItem=class(TObject)
  private
    FFinalizeASAPMode: integer;
    FUserData: Pointer;
    FOnPerform: TAnimationOnPerform;
    FOnAnimationEnd: TAnimationOnAnimationEnd;
    FFreeWithQueue: Boolean;
    FDuration: int64;
    FGroupID: pointer;
    FAnimationOnEndAction: TAnimationOnEndAction;
    FTransitionMode: TAnimationTransitionMode;
    FReverse: Boolean;
    FRepeats: integer;
    FAutoReverse: Boolean;
    FFinalizeBehavior: TAnimationFinalizeBehavior;
    procedure SetReverse(aValue: Boolean);
  protected
    FState: TAnimationState;
    FStartTick: int64;
    FPauseTick: int64;
    FQueue: TAnimationQueue;
    FStartStopSynchAnimationList: TFPList;
    FRepeated: integer;
    FFinalizeAnimation: Boolean;
    (* TransitionMode: (TAnimationTransitionMode) Linear or "curved" transition between animation values
          eAnimationTransitionLinear: Transition is linear in time.
          eAnimationTransitionBallisticIn: Fast -> Slow
          eAnimationTransitionBallisticOut: Slow -> Fast
          eAnimationTransitionBallisticBoth: Slow -> Fast -> Slow
          eAnimationTransitionBallisticEdge: Fast -> Slow -> Fast

          Calculations are done in CalculateLinearPosition.

          @Note: This transitions are not fine right now as some transitios
                 are quite difficult to distinguish from others. It will be
                 fixed in the future.
    *)
    property  TransitionMode: TAnimationTransitionMode read FTransitionMode write FTransitionMode;
    (* Calculate linear position is the default functions to calculate current
       animation value based in a source and a target value using the elapsed
       time. Derived classes can use it or define a new ones with different
       signatures and names *)
    function  CalculateLinearPosition(const aSource,aTarget: integer; const aElapsedMilliseconds,aDuration: int64): integer; overload;
    function  CalculateLinearPosition(const aSource,aTarget: Single; const aElapsedMilliseconds,aDuration: int64): Single; overload;

    (* Overridable
       DoInitialize: This method is called when animation starts or restarts.
    *)
    procedure DoInitialize; virtual;
    (* Overridable
       DoPerform: In this method derived classes must perform the calculations
                  needed for the animation
    *)
    procedure DoPerform; virtual;
    (* Overridable
       DoFinalizeAnimation: Method called when animation is about to be finished.
                            Useful to set EXACT values for final animation based on
                            TAnimationFinalizeBehavior
    *)
    procedure DoFinalizeAnimation; virtual;
    (* Animate: Do some calcs and call DoPerform *)
    procedure Animate;
    (* ProcessAnimationEnd: Adjust some values to end an animation *)
    procedure ProcessAnimationEnd;
    (* FinalizeBehavior: (TAnimationFinalizeBehavior) Which value is expected when animation ends ?
          eAnimationFinalizeBehaviorCurrent: Respects actual values, whichever it be
          eAnimationFinalizeBehaviorFinal: (default) At end it is the final value
          eAnimationFinalizeBehaviorInitial: At end the values is initial value.

          This set is quite important when playing animations in with AutoReverse and when
          reversing animations using Reverse()
    *)
    property  FinalizeBehavior: TAnimationFinalizeBehavior read FFinalizeBehavior write FFinalizeBehavior;
  public
    constructor Create;
    destructor Destroy; override;
    (* UserData: carries a pointer to user data *)
    property  UserData: Pointer read FUserData write FUserData;
    (* OnPerform: This event will be called instead class
                  perform if assigned *)
    property  OnPerform: TAnimationOnPerform read FOnPerform write FOnPerform;
    (* OnAnimationEnd: This event will be called when animation ends *)
    property  OnAnimationEnd: TAnimationOnAnimationEnd read FOnAnimationEnd write FOnAnimationEnd;
    (* FreeWithQueue: The TAnimationItem will be freed when the Queue is destroyed *)
    property  FreeWithQueue: Boolean read FFreeWithQueue write FFreeWithQueue;
    (* Duration: How many milliseconds will the animation take *)
    property  Duration: int64 read FDuration write FDuration;
    (* Reversed: Is animation being played in reverse time ? *)
    property  Reversed: Boolean read FReverse write SetReverse;
    (*
       AnimationOnEndAction: What happends when animation is finished:
            eAnimationOnEndStop: (Default) The animation just finish
            eAnimationOnEndFree: The animation finished and then the object is freed
    *)
    property  AnimationOnEndAction: TAnimationOnEndAction read FAnimationOnEndAction write FAnimationOnEndAction;
    (* AutoReverse: When animation reaches the end it will be played in reverse
                    order if there are enought Repeats to be played *)
    property  AutoReverse: Boolean read FAutoReverse write FAutoReverse;
    (* Repeats: How many times the animation should be repeated.
                0 = means infinite *)
    property  Repeats: integer read FRepeats write FRepeats;
    (* GroupID: Pointer with an identifier to select a group of animations
                from the queue. This is useful in example when animating controls
                as all animations related to a control will share the same GroupID
                which could be the memory address of the control *)
    property  GroupID: pointer read FGroupID write FGroupID;
    (*
       State: The animation play state (TAnimationState)
              eAnimationStateStopped: Animation in stopped state
              eanimationStateStarted: Animation is running
              eAnimationStatePaused: Animation is paused
              eAnimationStateToBeFreed: Animation is stopped and waiting to be freed
    *)
    property  State: TAnimationState read FState;
    (* GetElapsedMilliseconds: Returns the animation elapsed milliseconds *)
    function  GetElapsedMilliseconds: int64;
    (*
       FinalizeASAP: Finalizes an animation as soon as possible but waiting the
                     needed time to complete the programmed animation time.
       @Parameters:
           aReverseIfNeeded: If True and animation is running in forward mode, it
                             will wait to be run in reverse mode to stop it.
    *)
    procedure FinalizeASAP(const aReverseIfNeeded: Boolean=true);
    (*
       Reverse: Reverses current animation direction.
       @Parameters:
         aReverseFinalBehavior: If True Initial and Final behavior will be interchanged.
    *)
    function  Reverse(const aReverseFinalBehavior: Boolean=true): Boolean;
    (* Perform: Just perform the animation *)
    procedure Perform;
    (* Start: Starts animation *)
    procedure Start;
    (* Pause: Pauses animation *)
    procedure Pause;
    (* Stop: Stops animation *)
    procedure Stop;
    (* AddSyncStartStopAnimation: When this objects reaches the animation end, in forward
                                  or reverse mode, animations added with this function will
                                  be reversed or restarted at the same time. This is useful
                                  to synchronize some animations for large repeats *)
    procedure AddSyncStartStopAnimation(const aAnimationItem: TAnimationItem);
  end;

  { TAnimationQueue }

  TAnimationQueue=class(TObject)
  private
    FTick: int64;
    function  GetAverageFPS: single;
    function GetPaused: Boolean;
    procedure SetPaused(AValue: Boolean);
    procedure SetReverse(aValue: Boolean);
  protected
    type
      TAnimationQueueState=(eAnimationQueueStopped,eAnimationQueueStarted,eAnimationQueuePaused);
  protected
    FState: TAnimationQueueState;
    FFramesCounter: integer;
    FStartTick: int64;
    FReverse: Boolean;
    FPauseElapsed: int64;
    FAnimationItems: TFPList;
    function  GetAnimationTick: int64;
    function  intfGetTickCount64: int64; inline;
  public
    constructor Create;
    destructor Destroy; override;
    (* Start: Starts the animation

       @Parameters:
         aStartQueuedAnimations: If True it starts all animations already in the queue

       @Note: There is no Stop procedure because not calling "Animate" is just a stop.    *)
    procedure Start(const aStartQueuedAnimations: Boolean=false);
    (* Pause: Pauses animation. It can be unpaused calling it again *)
    procedure Pause;
    (* Reverse: Reverses all animations in queue *)
    procedure Reverse;
    (* Animate: Animate all objects in the queue. It is the main heartbeat *)
    procedure Animate; virtual;
    (* Paint: Animate just only the supplied animation
       @Parameters:
         aPerformItem: The animation to be animated :-)
    *)
    procedure Paint(const aPerformItem: TAnimationItem);
    (* Add: Adds one animation to the queue
       @Parameters:
         aAnimationItem: The animation to be added
         aRemoveSameID: Before adding it removes all animations in the queue
                        with the same GroupID, if GroupID is different than nil
    *)
    procedure Add(const aAnimationItem: TAnimationItem; const aRemoveSameGroupID: Boolean=false); virtual;
    (* Remove: Removes one animation from the queue
       @Parameters:
         aAnimationItem: The animation to be removed. If animation to be removed
                         has been selected to be freed on stop it will be automatically freed.
    *)
    procedure Remove(const aAnimationItem: TAnimationItem); virtual;
    (* GetAbsoluteAnimationClock: Milliseconds since animation start *)
    function  GetAbsoluteAnimationClock: int64;
    (* RemoveGroupID: Removes all animations with a given GroupID
       @Parameters:
         aClass: It can filter and remove only GroupID animations of the given class
    *)
    function  RemoveGroupID(const aID: pointer; const aClass: TClass=nil): integer;
    (* FindGroupID: Returns a TFPList containing all animations of a given GroupID

       @Parameters:
         aGroupID: The ID of the group to be returned.

       @Note: Programmer must free the TFPList returned
    *)
    function  FindGroupID(const aGroupID: pointer): TFPList;
    (* AverageFPS: Returns average animations per second since animation start *)
    property  AverageFPS: single read GetAverageFPS;
    (* Reversed: Is the animation being played in reverse time ? *)
    property  Reversed: Boolean read FReverse write SetReverse;
    (* Paused: Is animation in pause state ? *)
    property  Paused: Boolean read GetPaused write SetPaused;
  end;

implementation

{$IFDEF WINDOWS}
Uses windows; //For QueryPerformanceCounter only and it is not a must.
{$ENDIF}

resourcestring
  rs_AnimationInOtherQueue='The animation is already in other queue.';

const
  TFINALIZEASAPMODE_NONE=0;
  TFINALIZEASAPMODE_NOW=1;
  TFINALIZEASAPMODE_NEXT=2;
  TFINALIZEASAPMODE_REVERSED=3;

type
  TSplinePoint=record
    x,y: single;
  end;

function SplinePoint(x,y: single): TSplinePoint;
begin
  Result.x:=x;
  Result.y:=y;
end;

function CalculateSpline(const aPoint0, aPoint3, aPoint1, aPoint2: TSplinePoint; t: single ): TSplinePoint;
begin
  Result.x := power(t,3)*(aPoint3.x+3*(aPoint1.x-aPoint2.x)-aPoint0.x)+3*power(t,2)*(aPoint0.x-2*aPoint1.x+aPoint2.x)+3*t*(aPoint1.x-aPoint0.x)+aPoint0.x;
  Result.y := power(t,3)*(aPoint3.y+3*(aPoint1.y-aPoint2.y)-aPoint0.y)+3*power(t,2)*(aPoint0.y-2*aPoint1.y+aPoint2.y)+3*t*(aPoint1.y-aPoint0.y)+aPoint0.y;
end;

function CalculateElastic(const aFactor: single; const aDistance: integer; const aIntensity: single=1.5): integer;
begin
  Result:=trunc(Power(2,10*(aFactor-1))*cos(20*PI*aIntensity/3*aFactor)*aDistance)
end;


{ TAnimationItem }

procedure TAnimationItem.SetReverse(aValue: Boolean);
begin
  if FReverse=aValue then Exit;
  if FDuration=0 then begin
    FReverse:=aValue;
    exit;
  end;
  if Assigned(FQueue) then begin
    if aValue then begin
      FStartTick:=FQueue.GetAbsoluteAnimationClock-(FDuration-GetElapsedMilliseconds);
    end else begin
      FStartTick:=FQueue.GetAbsoluteAnimationClock-GetElapsedMilliseconds;
    end;
  end;
  FReverse:=aValue;
end;

function TAnimationItem.CalculateLinearPosition(const aSource,
  aTarget: integer; const aElapsedMilliseconds, aDuration: int64): integer;
var
  Factor: Single;
  Distance: integer;
begin
  if aDuration=0 then begin
    Result:=aSource;
    exit;
  end;
  Factor:=aElapsedMilliseconds / aDuration;
  if Factor>1.0 then Factor:=1.0;
  if Factor<0.0 then Factor:=0.0;
  case FTransitionMode of
    eAnimationTransitionLinear:
      begin
      end;
    eAnimationTransitionBallisticIn:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(0.9,1),SplinePoint(0.999,1),Factor).x;
      end;
    eAnimationTransitionBallisticOut:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(0.001,1),SplinePoint(0.1,1),Factor).x;
      end;
    eAnimationTransitionBallisticBoth:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(0,1),SplinePoint(1,1),Factor).x;
      end;
    eAnimationTransitionBallisticEdge:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(1,0),SplinePoint(0,1),Factor).x;
      end;
  end;
  Distance:=aTarget-aSource;
  Distance:=Round(Distance*Factor);
  Result:=aSource+Distance;
end;

function TAnimationItem.CalculateLinearPosition(const aSource, aTarget: Single;
  const aElapsedMilliseconds, aDuration: int64): Single;
var
  Factor: Single;
  Distance: Single;
begin
  if aDuration=0 then begin
    Result:=aSource;
    exit;
  end;
  Factor:=aElapsedMilliseconds / aDuration;
  if Factor>1.0 then Factor:=1.0;
  if Factor<0.0 then Factor:=0.0;
  case FTransitionMode of
    eAnimationTransitionLinear:
      begin
      end;
    eAnimationTransitionBallisticIn:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(0.9,1),SplinePoint(0.9,1),Factor).x;
      end;
    eAnimationTransitionBallisticOut:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(0.1,1),SplinePoint(0.1,1),Factor).x;
      end;
    eAnimationTransitionBallisticBoth:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(0,1),SplinePoint(1,1),Factor).x;
      end;
    eAnimationTransitionBallisticEdge:
      begin
        Factor:=CalculateSpline(SplinePoint(0,0),SplinePoint(1,0),SplinePoint(1,0),SplinePoint(0,1),Factor).x;
      end;
  end;
  Distance:=aTarget-aSource;
  Distance:=Distance*Factor;
  Result:=aSource+Distance;
end;

function TAnimationItem.GetElapsedMilliseconds: int64;
var
  LocalReversed: Boolean;
begin
  if FFinalizeAnimation and ((FRepeats-FRepeated)>=0) then begin
    LocalReversed:=false;
    if FAutoReverse then begin
      if (FRepeats-FRepeated) mod 2 = 1 then begin
        LocalReversed:=false;
      end else begin
        LocalReversed:=true;
      end;
    end;
    FRepeated:=FRepeats;
    if LocalReversed then begin
      Result:=0;
    end else begin
      Result:=Duration;
    end;
  end else begin
    if FReverse then begin
      Result:=Duration - (FQueue.GetAbsoluteAnimationClock - FStartTick);
    end else begin
      Result:=FQueue.GetAbsoluteAnimationClock - FStartTick;
    end;
  end;
end;

procedure TAnimationItem.FinalizeASAP(const aReverseIfNeeded: Boolean);
begin
  if aReverseIfNeeded then begin
    FFinalizeASAPMode:=TFINALIZEASAPMODE_REVERSED;
  end else begin
    FFinalizeASAPMode:=TFINALIZEASAPMODE_NEXT;
  end;
end;

procedure TAnimationItem.ProcessAnimationEnd;
var
  lShouldRepeat: Boolean=false;
  procedure Restart;
  var
    j: integer;
  begin
    Start;
    if Assigned(FStartStopSynchAnimationList) then begin
      for j := 0 to FStartStopSynchAnimationList.Count-1 do begin
        TAnimationItem(FStartStopSynchAnimationList[j]).Start;
      end;
    end;
  end;
  procedure ReverseIt;
  var
    j: integer;
  begin
    Reversed:=not Reversed;
    if Assigned(FStartStopSynchAnimationList) then begin
      for j := 0 to FStartStopSynchAnimationList.Count-1 do begin
        TAnimationItem(FStartStopSynchAnimationList[j]).ProcessAnimationEnd;
      end;
    end;
  end;
  procedure DoFinalize;
  begin
    case FAnimationOnEndAction of
      eAnimationOnEndStop: FState:=eAnimationStateStopped;
      eAnimationOnEndFree:  FState:=eAnimationStateToBeFreed;
    end;
    if Assigned(FOnAnimationEnd) then begin
      FOnAnimationEnd(Self);
    end;
  end;

begin
  inc(FRepeated);
  case FFinalizeASAPMode of
    TFINALIZEASAPMODE_NOW,
    TFINALIZEASAPMODE_NEXT:
      begin
        DoFinalize;
      end;
    TFINALIZEASAPMODE_REVERSED:
      begin
        if not Reversed then begin
          ReverseIt;
        end else begin
          DoFinalize;
        end;
      end;
    TFINALIZEASAPMODE_NONE:
      begin
        if FRepeats=0 then begin
          lShouldRepeat:=true;
        end else begin
          if FRepeated<FRepeats then begin
            lShouldRepeat:=true;
          end;
        end;
        if lShouldRepeat and FAutoReverse then begin
          ReverseIt;
        end else if lShouldRepeat then begin
          Restart;
        end else begin
          DoFinalize;
        end;
      end;
  end;
end;

constructor TAnimationItem.Create;
begin
  FFreeWithQueue:=true;
  FRepeats:=1;
  FFinalizeBehavior:=eAnimationFinalizeBehaviorFinal;
end;

destructor TAnimationItem.Destroy;
begin
  if Assigned(FStartStopSynchAnimationList) then FreeAndNil(FStartStopSynchAnimationList);
  inherited Destroy;
end;

function TAnimationItem.Reverse(const aReverseFinalBehavior: Boolean): Boolean;
begin
  if FState=eAnimationStateStarted then begin
    Reversed:=not FReverse;
    if aReverseFinalBehavior then begin
      case FFinalizeBehavior of
        eAnimationFinalizeBehaviorCurrent: ;
        eAnimationFinalizeBehaviorInitial: FFinalizeBehavior:=eAnimationFinalizeBehaviorFinal;
        eAnimationFinalizeBehaviorFinal: FFinalizeBehavior:=eAnimationFinalizeBehaviorInitial;
      end;
    end;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TAnimationItem.Perform;
begin
  if Assigned(FQueue) then begin
    FQueue.Paint(Self);
  end else begin
    if Assigned(FOnPerform) then begin
      FOnPerform(GetElapsedMilliseconds);
    end;
  end;
end;

procedure TAnimationItem.DoPerform;
begin
  // Nothing, nada.
end;

procedure TAnimationItem.Animate;
var
  lElapsedMilliseconds: int64;
begin
  if FState=eAnimationStateStarted then begin
    lElapsedMilliseconds:=GetElapsedMilliseconds;
    if Assigned(FOnPerform) then begin
      FOnPerform(lElapsedMilliseconds);
    end else begin
      DoPerform;
    end;
    if FFinalizeASAPMode=TFINALIZEASAPMODE_NOW then begin
      ProcessAnimationEnd;
    end else begin
      if FReverse then begin
        if lElapsedMilliseconds<=0 then begin
          ProcessAnimationEnd;
        end;
      end else begin
        if lElapsedMilliseconds>=FDuration then begin
          ProcessAnimationEnd;
        end;
      end;
    end;
  end;
end;

procedure TAnimationItem.DoFinalizeAnimation;
begin
  FFinalizeAnimation:=true;
end;

procedure TAnimationItem.DoInitialize;
begin
  //Do nothing
end;

procedure TAnimationItem.Start;
begin
  FStartTick:=FQueue.GetAbsoluteAnimationClock;
  FState:=eAnimationStateStarted;
  FRepeated:=0;
  FReverse:=false;
  FFinalizeAnimation:=false;
end;

procedure TAnimationItem.Pause;
begin
  if FState=eAnimationStateStarted then begin
    FState:=eAnimationStatePaused;
    if Assigned(FQueue) then begin
      FPauseTick:=GetElapsedMilliseconds;
    end;
  end else if FState=eAnimationStatePaused then begin
    if Assigned(FQueue) then begin
      FStartTick:=FQueue.GetAbsoluteAnimationClock-FPauseTick;
    end;
    FState:=eAnimationStateStarted;
  end;
end;

procedure TAnimationItem.Stop;
begin
  if (FState=eAnimationStateStarted) or (FState=eAnimationStatePaused) or (FState=eAnimationStateStopped) then begin
    FState:=eAnimationStateStopped;
    DoFinalizeAnimation;
  end;
end;

procedure TAnimationItem.AddSyncStartStopAnimation(
  const aAnimationItem: TAnimationItem);
begin
  if not Assigned(FStartStopSynchAnimationList) then begin
    FStartStopSynchAnimationList:=TFPList.Create;
  end;
  FStartStopSynchAnimationList.Add(aAnimationItem);
end;

{ TAnimationQueue }

function TAnimationQueue.GetAbsoluteAnimationClock: int64;
begin
  Result:=GetAnimationTick - FStartTick;
end;

function TAnimationQueue.RemoveGroupID(const aID: pointer; const aClass: TClass
  ): integer;
var
  j: Integer;
  A: TAnimationItem;
begin
  Result:=0;
  for j := Pred(FAnimationItems.Count) downto 0 do begin
    A:=TAnimationItem(FAnimationItems[j]);
    if A.GroupID=aID then begin
      if (Assigned(aClass) and (A is aClass)) or not Assigned(aClass) then begin
        Remove(A);
        inc(Result);
      end;
    end;
  end;
end;

function TAnimationQueue.FindGroupID(const aGroupID: pointer): TFPList;
var
  j: Integer;
  A: TAnimationItem;
begin
  Result:=TFPList.Create;
  for j := Pred(FAnimationItems.Count) downto 0 do begin
    A:=TAnimationItem(FAnimationItems[j]);
    if A.GroupID=aGroupID then begin
      Result.Add(A);
    end;
  end;
end;

procedure TAnimationQueue.SetReverse(aValue: Boolean);
var
  j: integer;
begin
  if FReverse=aValue then Exit;
  for j := 0 to FAnimationItems.Count-1 do begin
    TAnimationItem(FAnimationItems[j]).Reversed:=not TAnimationItem(FAnimationItems[j]).Reversed;
  end;
  FReverse:=aValue;
end;

function TAnimationQueue.GetAverageFPS: single;
begin
  if FState=eAnimationQueueStarted then begin
    if GetAbsoluteAnimationClock>0 then begin
      Result:=(FFramesCounter * 1000) / GetAbsoluteAnimationClock;
    end else begin
      Result:=0;
    end;
  end else begin
    Result:=0;
  end;
end;

function TAnimationQueue.GetPaused: Boolean;
begin
  if FState=eAnimationQueuePaused then begin
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

constructor TAnimationQueue.Create;
begin
  FAnimationItems:=TFPList.Create;
  FState:=eAnimationQueueStopped;
end;

destructor TAnimationQueue.Destroy;
var
  j: integer;
begin
  for j := Pred(FAnimationItems.Count) downto 0 do begin
    if TAnimationItem(FAnimationItems[j]).FFreeWithQueue then begin
      TAnimationItem(FAnimationItems[j]).Free;
      FAnimationItems.Delete(j);
    end;
  end;
  FreeAndNil(FAnimationItems);
  inherited Destroy;
end;

procedure TAnimationQueue.SetPaused(AValue: Boolean);
begin
  If AValue then begin
    if (FState=eAnimationQueueStarted) then begin
      FPauseElapsed:=GetAbsoluteAnimationClock;
      FState:=eAnimationQueuePaused;
    end;
  end else begin
    if FState=eAnimationQueuePaused then begin
      FStartTick:=GetTickCount64-FPauseElapsed;
      FState:=eAnimationQueueStarted;
    end;
  end;
end;

procedure TAnimationQueue.Start(const aStartQueuedAnimations: Boolean);
var
  j: Integer;
begin
  FTick:=GetTickCount64;
  if FState=eAnimationQueuePaused then begin
    SetPaused(false);
  end else begin
    FStartTick:=FTick;
    if aStartQueuedAnimations then begin
      for j := 0 to FAnimationItems.Count-1 do begin
        TAnimationItem(FAnimationItems[j]).Start;
      end;
    end;
    FState:=eAnimationQueueStarted;
    FFramesCounter:=0;
  end;
end;

procedure TAnimationQueue.Pause;
begin
  FPauseElapsed:=GetAbsoluteAnimationClock;
  FState:=eAnimationQueuePaused;
end;

procedure TAnimationQueue.Reverse;
begin
  Reversed:=not Reversed;
end;

procedure TAnimationQueue.Animate;
var
  j: integer;
  A: TAnimationItem;
begin
  if FState<>eAnimationQueueStarted then exit;
  FTick:=GetTickCount64;
  for j := FAnimationItems.Count-1 downto 0 do begin
    A:=TAnimationItem(FAnimationItems[j]);
    if A.FState=eAnimationStateToBeFreed then begin
      Remove(A);
    end;
  end;
  for j := 0 to FAnimationItems.Count-1 do begin
    A:=TAnimationItem(FAnimationItems[j]);
    if A.FState=eAnimationStateStarted then begin
      A.Animate;
    end;
  end;
  inc(FFramesCounter);
end;

procedure TAnimationQueue.Paint(const aPerformItem: TAnimationItem);
begin
  aPerformItem.DoPerform;
end;

procedure TAnimationQueue.Add(const aAnimationItem: TAnimationItem;
  const aRemoveSameGroupID: Boolean);
begin
  if Assigned(aAnimationItem.FQueue) then begin
    Raise TAnimationException.Create(rs_AnimationInOtherQueue);
  end;
  if aRemoveSameGroupID and (aAnimationItem.GroupID<>nil) then begin
     RemoveGroupID(aAnimationItem.GroupID);
  end;
  FAnimationItems.Add(aAnimationItem);
  aAnimationItem.FQueue:=Self;
  aAnimationItem.DoInitialize;
end;

procedure TAnimationQueue.Remove(const aAnimationItem: TAnimationItem);
var
  Index: integer;
  A: TAnimationItem;
  j: integer;
begin
  Index:=FAnimationItems.IndexOf(aAnimationItem);
  if Index<0 then begin
    Raise TAnimationException.Create('Trying to remove non exist animation');
  end;
  FAnimationItems.Delete(Index);
  aAnimationItem.DoFinalizeAnimation;
  //Check if object is in the synchronization list of other object
  for j := 0 to Pred(FAnimationItems.Count) do begin
    A:=TAnimationItem(FAnimationItems[j]);
    if Assigned(A.FStartStopSynchAnimationList) then begin
      Index:=A.FStartStopSynchAnimationList.IndexOf(aAnimationItem);
      if Index>-1 then begin
        //Delete it to avoid crash when synch takes place
        A.FStartStopSynchAnimationList.Delete(Index);
      end;
    end;
  end;
  aAnimationItem.Free;
end;

function TAnimationQueue.GetAnimationTick: int64;
begin
  Result:=FTick;
end;

function TAnimationQueue.intfGetTickCount64: int64;
{$IFDEF WINDOWS}
const
  lUseHighPrecision: Boolean=true;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    if lUseHighPrecision then begin
      Result:=0;
      if not QueryPerformanceCounter(Result) then begin
        Result:=GetTickCount64;
        lUseHighPrecision:=false;
      end;
    end else begin
      Result:=GetTickCount64;
    end;
  {$ELSE}
    Result:=GetTickCount64;
  {$ENDIF}
end;

end.

