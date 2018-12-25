{
 /***************************************************************************
                                 switches.pp

  License: Modified LGPL (with linking exception)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Autho: Werner Pamler
 *****************************************************************************
}

unit switches;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, Types, Controls, ExtCtrls;

type
  TSwitchBorderStyle = (bsNone, bsThin, bsThick, bsThin3D, bsThick3D);
  TSwitchOrientation = (soHorizontal, soVertical);

  TCustomOnOffSwitch = class(TCustomControl)
  private
    FBorderStyle: TSwitchBorderStyle;
    FButtonSize: Integer;
    FCaptions: array[0..1] of string;
    FChecked: Boolean;
    FColors: array [0..2] of TColor;
    FInverse: Boolean;
    FDragging: Boolean;
    FDraggedDistance: Integer;
    FTogglePending: Boolean;
    FMousePt: TPoint;
    FButtonRect: TRect;
    FReadOnly: Boolean;
    FShowButtonBorder: Boolean;
    FShowCaption: Boolean;
    FOnChange: TNotifyEvent;
    FDblClickTimer: TTimer;
    function GetBorderWidth: Integer;
    function GetCaptions(AIndex: Integer): String;
    function GetColors(AIndex: Integer): TColor;
    function GetOrientation: TSwitchOrientation;
    function IsButtonSizeStored: Boolean;
    procedure SetBorderStyle(AValue: TSwitchBorderStyle); reintroduce;
    procedure SetButtonSize(AValue: Integer);
    procedure SetCaptions(AIndex: Integer; AValue: string);
    procedure SetChecked(AValue: Boolean);
    procedure SetColors(AIndex: Integer; AValue: TColor);
    procedure SetInverse(AValue: Boolean);
    procedure SetShowButtonBorder(AValue: Boolean);
    procedure SetShowCaption(AValue: Boolean);
  protected
    function CalcButtonRect(ADelta: Integer): TRect;
    function CalcMargin: Integer;
    function CanChange: Boolean; virtual;
    procedure DblClick; override;
    procedure DblClickTimerHandler(Sender: TObject);
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure DoChange; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    function DraggingToValue(ADistance: Integer): Boolean;
    procedure DrawButton(ARect: TRect); virtual;
    procedure DrawCaption(ARect: TRect; AChecked: Boolean); virtual;
    procedure DrawFocusRect(ARect: TRect);
    class function GetControlClassDefaultSize: TSize; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    function MouseOnButton(X, Y: Integer): Boolean; virtual;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure Paint; override;
    property BorderColor: TColor index 2 read GetColors write SetColors default clGray;
    property BorderStyle: TSwitchBorderStyle read FBorderStyle write SetBorderStyle default bsThin;
    property ButtonSize: Integer read FButtonSize write SetButtonSize stored IsButtonSizeStored;
    property CaptionOFF: String index 0 read GetCaptions write SetCaptions;
    property CaptionON: String index 1 read GetCaptions write SetCaptions;
    property Checked: Boolean read FChecked write SetChecked default false;
    property Color default clWindow;
    property ColorOFF: TColor index 0 read GetColors write SetColors default clMaroon;
    property ColorON: TColor index 1 read GetColors write SetColors default clGreen;
    property Inverse: Boolean read FInverse write SetInverse default false;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property ShowButtonBorder: Boolean read FShowButtonBorder write SetShowButtonBorder default true;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    property Orientation: TSwitchOrientation read GetOrientation;
  end;

  TOnOffSwitch = class(TCustomOnOffSwitch)
  published
    property BorderColor;
    property BorderStyle;
    property ButtonSize;
    property CaptionOFF;
    property CaptionON;
    property Checked;
    property Color;
    property ColorOFF;
    property ColorON;
    property Enabled;
    property Inverse;
    property ReadOnly;
    property ShowButtonBorder;
    property ShowCaption;
    property OnChange;

    // inherited
    property Action;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DoubleBuffered;
    property Font;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseUp;
    property OnResize;
    property OnShowHint;
  end;

implementation

uses
  LCLIntf, LCLType, Math;

const
  DEFAULT_BUTTON_SIZE = 24;

function TintedColor(AColor: TColor; ADelta: Integer): TColor;
var
  r, g, b: Byte;
begin
  AColor := ColorToRGB(AColor);
  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  if r + g + b < 3*128 then
    // Dark color --> make it brigher
    ADelta := abs(ADelta)
  else
    // Bright color --> make it darker
    ADelta := -abs(ADelta);
  r := EnsureRange(r + ADelta, 0, 255);
  g := EnsureRange(g + ADelta, 0, 255);
  b := EnsureRange(b + ADelta, 0, 255);
  Result := RGBToColor(r, g, b);
end;


{ TOnOffSwitch }

constructor TCustomOnOffSwitch.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := true;
  Color := clWindow;
  FBorderStyle := bsThin;
  FButtonSize := Scale96ToFont(DEFAULT_BUTTON_SIZE);
  FColors[0] := clMaroon; // unchecked color
  FColors[1] := clGreen;  // checked color
  FColors[2] := clGray;   // Border color
  FCaptions[0] := 'OFF';
  FCaptions[1] := 'ON';
  FShowCaption := true;
  FShowButtonBorder := true;
  FDblClickTimer := TTimer.Create(self);
  FDblClickTimer.Interval := 500;
  FDblClickTimer.Enabled := false;
  FDblClickTimer.OnTimer := @DblClickTimerHandler;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

function TCustomOnOffSwitch.CalcButtonRect(ADelta: Integer): TRect;

  function GetOffset(AMaxSize, ABtnSize: Integer): Integer;
  var
    pStart, pEnd, margin: Integer;
  begin
    margin := CalcMargin;
    if (FInverse xor FChecked) then begin
      // Button at right (or bottom), ADelta is negative
      pStart := AMaxSize - ABtnSize - margin;
      pEnd := margin;
      if ADelta < pEnd - pStart then
        result := pEnd
      else if ADelta > 0 then
        result := pStart
      else
        Result := pStart + ADelta;
    end else begin
      // Button at left (or top), ADelta is positive
      pStart := margin;
      pEnd := AMaxSize - ABtnSize - margin;
      if ADelta < 0 then
        Result := pStart
      else if ADelta > pEnd - pStart then
        Result := pEnd
      else
        Result := pStart + ADelta;
    end;
  end;

begin
  Result := FButtonRect;
  case Orientation of
    soHorizontal : OffsetRect(Result, GetOffset(Width, FButtonSize), 0);
    soVertical   : OffsetRect(Result, 0, GetOffset(Height, FButtonSize));
  end;
end;

function TCustomOnOffSwitch.CalcMargin: Integer;
begin
  Result := 3 + GetBorderWidth;
end;

function TCustomOnOffSwitch.CanChange: Boolean;
begin
  Result := Enabled and (not FReadOnly);
end;

procedure TCustomOnOffSwitch.DblClick;
begin
  inherited;
  if CanChange and FTogglePending then begin
    Checked := not Checked;
    FTogglePending := false;
  end;
  FDblClickTimer.Enabled := false;
end;

procedure TCustomOnOffSwitch.DblClickTimerHandler(Sender: TObject);
begin
  FTogglePending := true;
end;

procedure TCustomOnOffSwitch.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    DisableAutosizing;
    try
      if IsButtonSizeStored then
        case Orientation of
          soHorizontal : FButtonSize := Round(FButtonSize * AXProportion);
          soVertical   : FButtonSize := Round(FButtonSize * AYProportion);
        end;
    finally
      EnableAutoSizing;
    end;
  end;
end;

procedure TCustomOnOffSwitch.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TCustomOnOffSwitch.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TCustomOnOffSwitch.DoExit;
begin
  inherited;
  Invalidate;
end;

{ Determines whether the dragged distance lands in the part of the ON or OFF state }
function TCustomOnOffSwitch.DraggingToValue(ADistance: Integer): Boolean;
var
  margin: Integer;
begin
  if not (FChecked xor FInverse) and (ADistance < 0) then
    Result := false
  else
  if (FChecked xor FInverse) and (ADistance > 0) then
    Result := true
  else begin
    margin := CalcMargin;
    case Orientation of
      soHorizontal : Result := abs(ADistance) > (Width - FButtonSize) div 2 - margin;
      soVertical   : Result := abs(ADistance) > (Height - FButtonSize) div 2 - margin;
    end;
    if FChecked {xor FInverse} then
      Result := not Result;
  end;
end;

procedure TCustomOnOffSwitch.DrawButton(ARect: TRect);
begin
  if not Enabled then begin
    Canvas.Brush.Color := clGrayText;
    Canvas.Pen.Color := clGrayText;
  end else begin
    if FChecked then
      Canvas.Brush.Color := ColorON
    else
      Canvas.Brush.Color := ColorOFF;
    Canvas.Pen.Color := clBlack;
  end;
  if not FShowButtonBorder then
    Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  Canvas.Rectangle(ARect);
end;

procedure TCustomOnOffSwitch.DrawCaption(ARect: TRect; AChecked: Boolean);
var
  ts: TTextStyle;
begin
  Canvas.Font.Assign(Font);
  if not Enabled then
    Canvas.Font.Color := clGrayText;
  ts := Canvas.TextStyle;
  ts.Alignment := taCenter;
  ts.Layout := tlCenter;
  Canvas.TextStyle := ts;
  if AChecked then
    Canvas.TextRect(ARect, ARect.Left, ARect.Top, CaptionON)
  else
    Canvas.TextRect(ARect, ARect.Left, ARect.Top, CaptionOFF);
end;

procedure TCustomOnOffSwitch.DrawFocusRect(ARect: TRect);
var
  m: TPenMode;
  c: Boolean;
begin
  m := Canvas.Pen.Mode;
  c := Canvas.Pen.Cosmetic;
  try
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Cosmetic := false;
    Canvas.Pen.Mode := pmXOR;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ARect);
  finally
    Canvas.Pen.Mode := m;
    Canvas.Pen.Cosmetic := c;
  end;
end;

function TCustomOnOffSwitch.GetBorderWidth: Integer;
begin
  case FBorderStyle of
    bsNone, bsThin, bsThin3D:
      Result := 1;
    bsThick, bsThick3D:
      Result := 2;
  end;
end;

function TCustomOnOffSwitch.GetCaptions(AIndex: Integer): string;
begin
  Result := FCaptions[AIndex];
end;

function TCustomOnOffSwitch.GetColors(AIndex: Integer): TColor;
begin
  Result := FColors[AIndex];
end;

class function TCustomOnOffSwitch.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 60;
  Result.CY := 30;
end;

function TCustomOnOffSwitch.GetOrientation: TSwitchOrientation;
begin
  if Width > Height then Result := soHorizontal else Result := soVertical;
end;

function TCustomOnOffSwitch.IsButtonSizeStored: Boolean;
begin
  Result := FButtonSize <> Scale96ToFont(DEFAULT_BUTTON_SIZE);
end;

procedure TCustomOnOffSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if CanChange and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
    Checked := not Checked;
end;

procedure TCustomOnOffSwitch.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited;
  SetFocus;
  if CanChange and (Button = mbLeft) and MouseOnButton(X, Y) then begin
    FDragging := true;
    FMousePt := Point(X, Y);
    FDraggedDistance := 0;
    FDblClickTimer.Enabled := true;
  end;
end;

procedure TCustomOnOffSwitch.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  if FDragging then begin
    case Orientation of
      soHorizontal : FDraggedDistance := X - FMousePt.X;
      soVertical   : FDraggedDistance := Y - FMousePt.Y;
    end;
    Invalidate;
  end;
end;

function TCustomOnOffSwitch.MouseOnButton(X, Y: Integer): Boolean;
var
  R: TRect;
begin
  R := CalcButtonRect(FDraggedDistance);
  Result := PtInRect(R, Point(X, Y));
end;

procedure TCustomOnOffSwitch.MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  oldChecked: Boolean;
  d: Integer;
begin
  inherited;
  if Button = mbLeft then begin
    oldChecked := FChecked;
    d := FDraggedDistance;
    FDraggedDistance := 0;
    if FDragging then begin
      FChecked := DraggingToValue(d);
    end;
    FDragging := false;
    if CanChange then begin
      if FChecked <> oldChecked then
        DoChange
      else
        FTogglePending := true;
    end;
    Invalidate;
  end;
end;

procedure TCustomOnOffSwitch.Paint;
var
  R: TRect;
  margin: Integer;
  newChecked: Boolean;
begin
  if Enabled then begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := BorderColor;
  end else begin
    Canvas.Brush.Color := clInactiveBorder;
    Canvas.Pen.Color := clGrayText;
  end;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := GetBorderWidth;
  R := Rect(0, 0, Width, Height);
  case FBorderStyle of
    bsNone:
      begin
        Canvas.Pen.Style := psClear;
        Canvas.Rectangle(R);
      end;
    bsThin:
      Canvas.Rectangle(R);
    bsThick:
      Canvas.Rectangle(1, 1, Width, Height);
    bsThin3D, bsThick3D:
      begin
        Canvas.Pen.Color := clBtnShadow;
        Canvas.Line(R.Right, R.Top, R.Left, R.Top);
        Canvas.Line(R.Left, R.Top, R.Left, R.Bottom);
        if FBorderStyle = bsThick3D then begin
          InflateRect(R, -1, -1);
          Canvas.Line(R.Right, R.Top, R.Left, R.Top);
          Canvas.Line(R.Left, R.Top, R.Left, R.Bottom);
          InflateRect(R, +1, +1);
        end;
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.Line(R.Left, R.Bottom, R.Right, R.Bottom);
        Canvas.Line(R.Right, R.Bottom, R.Right, R.Top);
        InflateRect(R, -1, -1);
        if FBorderStyle = bsThin then
          Canvas.FillRect(R)
        else begin
          Canvas.Line(R.Left, R.Bottom, R.Right, R.Bottom);
          Canvas.Line(R.Right, R.Bottom, R.Right, R.Top);
          InflateRect(R, -1, -1);
          Canvas.FillRect(R);
        end;
      end;
  end;
  margin := CalcMargin;

  case Orientation of
    soHorizontal:
      FButtonRect := Rect(0, margin, FButtonSize, Height - margin);
    soVertical:
      FButtonRect := Rect(margin, 0, Width - margin, FButtonSize);
  end;

  if FShowCaption then begin
    newChecked := DraggingToValue(FDraggedDistance);
    case Orientation of
      soHorizontal:
        if FChecked xor FInverse then begin
          // Drag begins from button at right
          if FDragging and not (FInverse xor newChecked) then
            DrawCaption(Rect(margin + FButtonSize, margin, Width, Height - margin), FInverse)
          else
            DrawCaption(Rect(0, margin, Width - margin - FButtonSize, Height - margin), not FInverse);
        end else begin
          // Drag begins from button at left
          if FDragging and (FInverse xor newChecked) then
            DrawCaption(Rect(0, margin, Width - margin - FButtonSize, Height - margin), not FInverse)
          else
            DrawCaption(Rect(margin + FButtonSize, margin, Width, Height - margin), FInverse);
        end;
      soVertical:
        if FChecked xor FInverse then begin
          // Drag begins from button at bottom
          if FDragging and not (FInverse xor newChecked) then
            DrawCaption(Rect(margin, margin + FButtonSize, Width-margin, Height), FInverse)
          else
            DrawCaption(Rect(margin, 0, Width - margin, Height - margin - FButtonSize), not FInverse);
        end else begin
          // Drag begins from button at top
          if FDragging and (FInverse xor newChecked) then
            DrawCaption(Rect(margin, 0, Width - margin, Height - margin - FButtonSize), not FInverse)
          else
            DrawCaption(Rect(margin, margin + FButtonsize, Width - margin, Height), FInverse);
        end;
    end;
  end;

  R := CalcButtonRect(FDraggedDistance);
  DrawButton(R);
  if Focused then begin
    InflateRect(R, 2, 2);
    DrawFocusRect(R);
  end;
end;

procedure TCustomOnOffSwitch.SetBorderStyle(AValue: TSwitchBorderStyle);
begin
  if AValue = FBorderStyle then exit;
  FBorderStyle := AValue;
  Invalidate;
end;

procedure TCustomOnOffSwitch.SetButtonSize(AValue: Integer);
begin
  if (AValue = FButtonSize) and (AValue > 0) then
    exit;
  FButtonSize := AValue;
  Invalidate;
end;

procedure TCustomOnOffSwitch.SetCaptions(AIndex: Integer; AValue: String);
begin
  if AValue = FCaptions[AIndex] then exit;
  FCaptions[AIndex] := AValue;
  Invalidate;
end;

procedure TCustomOnOffSwitch.SetChecked(AValue: Boolean);
begin
  if AValue = FChecked then exit;
  FChecked := AValue;
  DoChange;
  Invalidate;
end;

procedure TCustomOnOffSwitch.SetColors(AIndex: Integer; AValue: TColor);
begin
  if AValue = FColors[AIndex] then exit;
  FColors[AIndex] := AValue;
  Invalidate;
end;

procedure TCustomOnOffSwitch.SetInverse(AValue: boolean);
begin
  if AValue = FInverse then exit;
  FInverse := AValue;
  Invalidate;
end;

procedure TCustomOnOffSwitch.SetShowButtonBorder(AValue: Boolean);
begin
  if AValue = FShowButtonBorder then exit;
  FShowButtonBorder := AValue;
  DoChange;
  Invalidate;
end;

procedure TCustomOnOffSwitch.SetShowCaption(AValue: Boolean);
begin
  if AValue = FShowCaption then exit;
  FShowCaption := AValue;
  DoChange;
  Invalidate;
end;

end.

