unit mbBasicPicker;

{$mode objfpc}{$H+}

interface

uses
 {$IFDEF FPC}
  LMessages,
 {$ELSE}
  Messages,
 {$ENDIF}
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms;

type
  THintState = (hsOff, hsWaitingToShow, hsWaitingToHide);

  TGetHintStrEvent = procedure (Sender: TObject; X, Y: Integer; var AText: String) of object;

  { TmbBasicPicker }

  TmbBasicPicker = class(TCustomControl)
  private
    FOnGetHintStr: TGetHintStrEvent;
    {
    FHintWindow: THintWindow;
    FHintTimer: TTimer;
    FHintState: THintState;
    procedure HintTimer(Sender: TObject);
    }
  protected
    FBufferBmp: TBitmap;
    FGradientWidth: Integer;
    FGradientHeight: Integer;
    FHintShown: Boolean;
    procedure CreateGradient; virtual;
    function GetColorUnderCursor: TColor; virtual;
    function GetGradientColor(AValue: Integer): TColor; virtual;
    function GetGradientColor2D(X, Y: Integer): TColor; virtual;
    function GetHintPos(X, Y: Integer): TPoint; virtual;
    function GetHintStr(X, Y: Integer): String; virtual;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function MouseOnPicker(X, Y: Integer): Boolean; virtual;
    procedure PaintParentBack; virtual; overload;
    procedure PaintParentBack(ACanvas: TCanvas); overload;
    procedure PaintParentBack(ACanvas: TCanvas; ARect: TRect); overload;
    procedure PaintParentBack(ABitmap: TBitmap); overload;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    {$IFDEF DELPHI}
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ELSE}
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
//    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    {$ENDIF}
    property ColorUnderCursor: TColor read GetColorUnderCursor;
    property OnGetHintStr: TGetHintStrEvent read FOnGetHintStr write FOnGetHintStr;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColorAtPoint(X, Y: Integer): TColor; virtual;
    function GetHexColorAtPoint(X, Y: integer): string;
    function GetHexColorUnderCursor: string; virtual;
//    function GetDefaultColor(const DefaultColorType: TDefaultColorType): TColor; override;
  published
    property ParentColor default true;
  end;

implementation

uses
  LCLIntf,
  HTMLColors, mbUtils;

const
  HINT_SHOW_DELAY = 50;
  HINT_HIDE_DELAY = 3000;

constructor TmbBasicPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := ControlStyle - [csOpaque];
  ParentColor := true;
  {
  FHintTimer := TTimer.Create(self);
  FHintTimer.Interval := HINT_SHOW_DELAY;
  FHintTimer.Enabled := false;
  FHintTimer.OnTimer := @HintTimer;
  FHintState := hsOff;
  }
end;

destructor TmbBasicPicker.Destroy;
begin
  FBufferBmp.Free;
  inherited;
end;

procedure TmbBasicPicker.CMHintShow(var Message: TCMHintShow);
var
  cp: TPoint;
  hp: TPoint;
begin
  if GetColorUnderCursor <> clNone then
    with TCMHintShow(Message) do
      if not ShowHint then
        Message.Result := 1
      else
      if Hint <> '' then
        Message.Result := 0
      else
      begin
        cp := HintInfo^.CursorPos;
        hp := GetHintPos(cp.X, cp.Y);
        HintInfo^.ReshowTimeout := 0;  // must be zero!
        HintInfo^.HideTimeout := Application.HintHidePause;
        HintInfo^.HintStr := GetHintStr(cp.X, cp.Y);
        HintInfo^.HintPos := ClientToScreen(Point(hp.X + 16, hp.Y));
        HintInfo^.CursorRect := Rect(cp.X, cp.Y, cp.X+1, cp.Y+1);
        Result := 0;    // 0 means: show hint
      end;

  inherited;
end;

procedure TmbBasicPicker.CMParentColorChanged(var Message: TLMessage);
begin
  {
  if ParentColor then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
    }
  inherited;
end;

procedure TmbBasicPicker.CreateGradient;
begin
  // to be implemented by descendants
end;

function TmbBasicPicker.GetColorAtPoint(x, y: integer): TColor;
begin
  Result := Canvas.Pixels[x, y];  // valid for most descendents
end;

function TmbBasicPicker.GetColorUnderCursor: TColor;
var
  P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  Result := GetColorAtPoint(P.X, P.Y);
end;

function TmbBasicPicker.GetHexColorAtPoint(X, Y: integer): string;
begin
  Result := ColorToHex(GetColorAtPoint(x, y));
end;

function TmbBasicPicker.GetHexColorUnderCursor: string;
begin
  Result := ColorToHex(GetColorUnderCursor);
end;

                                     {
function TmbBasicPicker.GetDefaultColor(const DefaultColorType: TDefaultColorType): TColor;
begin
  result := inherited GetDefaultColor(DefaultColorType);
end;                                  }

function TmbBasicPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := clNone;
end;

function TmbBasicPicker.GetGradientColor2D(X, Y: Integer): TColor;
begin
  Result := clNone;
end;

function TmbBasicPicker.GetHintPos(X, Y: Integer): TPoint;
begin
  Result := Point(X, Y);
end;

function TmbBasicPicker.GetHintStr(X, Y: Integer): String;
begin
  Result := '';
  if Assigned(FOnGetHintStr) then
    FOnGetHintStr(Self, X, Y, Result);
end;

(*
function TmbBasicPicker.GetHintText: String;
begin
  Result := Hint;
end;

procedure TmbBasicPicker.HideHintWindow;
begin
  FHintTimer.Enabled := false;
  FHintState := hsOff;
  FreeAndNil(FHintWindow);
end;

procedure TmbBasicPicker.HintTimer(Sender: TObject);
begin
  case FHintState of
    hsWaitingToShow:
      ShowHintWindow(Mouse.CursorPos, GetHintText);
    hsWaitingToHide:
      HideHintWindow;
  end;
end;
   *)
procedure TmbBasicPicker.MouseLeave;
begin
  inherited;
    {
  HideHintWindow;
  FHintTimer.Enabled := false;
  FHintState := hsOff;
  }
end;

procedure TmbBasicPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  {
  if ShowHint and not FHintShown then
  begin
    if MouseOnPicker(X, Y) then
    begin
      FHintTimer.Enabled := false;
      FHintState := hsWaitingToShow;
      FHintTimer.Interval := HINT_SHOW_DELAY;
      FHintTimer.Enabled := true;
    end
    else
      HideHintWindow;
  end;
  }
end;

function TmbBasicPicker.MouseOnPicker(X, Y: Integer): Boolean;
begin
  Result := true;
end;

procedure TmbBasicPicker.PaintParentBack;
begin
  PaintParentBack(Canvas);
end;

procedure TmbBasicPicker.PaintParentBack(ABitmap: TBitmap);
begin
  ABitmap.Width := Width;
  ABitmap.Height := Height;
  {$IFNDEF DELPHI}
  if Color = clDefault then begin
    ABitmap.Transparent := true;
    ABitmap.TransparentColor := clForm;
    ABitmap.Canvas.Brush.Color := clForm; //GetDefaultColor(dctBrush)
  end else
  {$ENDIF}
    ABitmap.Canvas.Brush.Color := Color;
  ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);

  {$IFDEF DELPHI_7_UP}{$IFDEF DELPHI}
  if ParentBackground then
   with ThemeServices do
    if ThemesEnabled then
     begin
      MemDC := CreateCompatibleDC(0);
      OldBMP := SelectObject(MemDC, ABitmap.Handle);
      DrawParentBackground(Handle, MemDC, nil, False);
      if OldBMP <> 0 then SelectObject(MemDC, OldBMP);
      if MemDC <> 0 then DeleteDC(MemDC);
     end;
  {$ENDIF}{$ENDIF}
end;

procedure TmbBasicPicker.PaintParentBack(ACanvas: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  PaintParentBack(ACanvas, R);
  (*
var
  OffScreen: TBitmap;
begin
  Offscreen := TBitmap.Create;
  try
  //  Offscreen.PixelFormat := pf32bit;
    if Color = clDefault then begin
      Offscreen.Transparent := true;
      Offscreen.TransparentColor := clForm; //GetDefaultColor(dctBrush);
    end;
    Offscreen.Width := Width;
    Offscreen.Height := Height;
    PaintParentBack(Offscreen);
    ACanvas.Draw(0, 0, Offscreen);
  finally
    Offscreen.Free;
  end;
  *)
end;

procedure TmbBasicPicker.PaintParentBack(ACanvas: TCanvas; ARect: TRect);
var
  OffScreen: TBitmap;
begin
  Offscreen := TBitmap.Create;
  try
  //  Offscreen.PixelFormat := pf32bit;
    if Color = clDefault then begin
      Offscreen.Transparent := true;
      Offscreen.TransparentColor := clForm; //GetDefaultColor(dctBrush);
    end;
    Offscreen.Width := WidthOfRect(ARect);
    Offscreen.Height := HeightOfRect(ARect);
    PaintParentBack(Offscreen);
    ACanvas.Draw(ARect.Left, ARect.Top, Offscreen);
  finally
    Offscreen.Free;
  end;
end;
   (*
// Build and show the hint window
function TmbBasicPicker.ShowHintWindow(APoint: TPoint; AText: String): Boolean;
const
  MAXWIDTH = 400;
var
  RScr, RHint, R: TRect;
begin
  FHintTimer.Enabled := false;

  if AText = '' then
  begin
    HideHintWindow;
    exit(false);
  end;

  if FHintWindow = nil then
    FHintWindow := THintWindow.Create(nil);
  RScr := Screen.WorkAreaRect;
  RHint := FHintWindow.CalcHintRect(MAXWIDTH, AText, nil);
  OffsetRect(RHint, APoint.X, APoint.Y);
  OffsetRect(RHint, 0, -(RHint.Bottom - RHint.Top));
  R := RHint;
  if R.Left < RScr.Left then
    R := RHint;
  RHint := R;
  if (R.Bottom > RScr.Bottom) then begin
    R := RHint;
    OffsetRect(R, 0, R.Bottom - RScr.Bottom);
  end;
  FHintWindow.ActivateHint(R, AText);

  FHintState := hsWaitingToHide;
  FHintTimer.Interval := HINT_HIDE_DELAY;
  FHintTimer.Enabled := true;

  Result := true;
end;
*)
                                 (*  !!!!!!!!!!!!!!!!!
procedure TmbBasicPicker.WMEraseBkgnd(
  var Message: {$IFDEF DELPHI}TWMEraseBkgnd{$ELSE}TLMEraseBkgnd{$ENDIF} );
begin
  inherited;
//  Message.Result := 1;
end;                               *)

end.

