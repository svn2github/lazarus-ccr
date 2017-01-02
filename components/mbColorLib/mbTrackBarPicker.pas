unit mbTrackBarPicker;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, Forms,
  Themes, ExtCtrls,
  PalUtils, mbBasicPicker;

const
  TBA_Resize = 0;
  TBA_Paint = 1;
  TBA_MouseMove = 2;
  TBA_MouseDown = 3;
  TBA_MouseUp = 4;
  TBA_WheelUp = 5;
  TBA_WheelDown = 6;
  TBA_VKUp = 7;
  TBA_VKCtrlUp = 8;
  TBA_VKDown = 9;
  TBA_VKCtrlDown = 10;
  TBA_VKLeft = 11;
  TBA_VKCtrlLeft = 12;
  TBA_VKRight = 13;
  TBA_VKCtrlRight = 14;
  TBA_RedoBMP = 15;

type
  TTrackBarLayout = (lyHorizontal, lyVertical);
  TSliderPlacement = (spBefore, spAfter, spBoth);
  TSelIndicator = (siArrows, siRect);

  { TmbTrackBarPicker }

  TmbTrackBarPicker = class(TmbBasicPicker)
  private
    Aw, Ah: integer;
    mx, my: integer;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelWidth: TBevelWidth;
    FBorderStyle: TBorderStyle;
    FHintFormat: string;
    FIncrement: integer;
    FNewArrowStyle: boolean;
    FPlacement: TSliderPlacement;
    FSelIndicator: TSelIndicator;
    FWebSafe: boolean;
    procedure CalcPickRect;
    procedure DrawMarker(p: integer);
    procedure SetBevelInner(Value: TBevelCut);
    procedure SetBevelOuter(Value: TBevelCut);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetLayout(Value: TTrackBarLayout);
    procedure SetNewArrowStyle(s: boolean);
    procedure SetPlacement(Value: TSliderPlacement);
    procedure SetSelIndicator(Value: TSelIndicator);
    procedure SetWebSafe(s: boolean);
    function XToArrowPos(p: integer): integer;
    function YToArrowPos(p: integer): integer;
  protected
    FArrowPos: integer;
//    FBack: TBitmap;
    FLayout: TTrackBarLayout;
    FLimit: integer;
    FPickRect: TRect;
    procedure CreateGradient; override;
    procedure CreateWnd; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DrawFrames; dynamic;
    procedure Execute(tbaAction: integer); dynamic;
    function GetArrowPos: integer; dynamic;
    function GetHintPos(X, Y: Integer): TPoint; override;
    function GetHintStr(X, Y: Integer): String; override;
    function GetSelectedValue: integer; virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetBorderStyle(Value: TBorderStyle); override;
    procedure CMGotFocus(var Message: TLMessage); message CM_ENTER;
    procedure CMLostFocus(var Message: TLMessage); message CM_EXIT;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property HintFormat: string read FHintFormat write FHintFormat;
    property Increment: integer read FIncrement write FIncrement default 1;
    property Layout: TTrackBarLayout read FLayout write SetLayout default lyHorizontal;
    property ArrowPlacement: TSliderPlacement read FPlacement write SetPlacement default spAfter;
    property NewArrowStyle: boolean read FNewArrowStyle write SetNewArrowStyle default false;
    property SelectionIndicator: TSelIndicator read FSelIndicator write SetSelIndicator default siArrows;
    property WebSafe: boolean read FWebSafe write SetWebSafe default false;
    property TabStop default true;
    property ShowHint;
    property Color;
    property ParentColor;
    property ParentShowHint default true;
    property Anchors;
    property Align;
    property BorderSpacing;
    property Visible;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnChange;
    property OnContextPopup;
    property OnGetHintStr;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  IntfGraphics, fpimage, Math,
  ScanLines, HTMLColors;

const
  { 3D border styles }
  BDR_RAISEDOUTER = 1;
  BDR_SUNKENOUTER = 2;
  BDR_RAISEDINNER = 4;
  BDR_SUNKENINNER = 8;

  BDR_OUTER = 3;
  BDR_INNER = 12;

  { Border flags }
  BF_LEFT = 1;
  BF_TOP = 2;
  BF_RIGHT = 4;
  BF_BOTTOM = 8;
  BF_RECT = (BF_LEFT or BF_TOP or BF_RIGHT or BF_BOTTOM);


{TmbTrackBarPicker}

constructor TmbTrackBarPicker.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle - [csAcceptsControls]; // + [csOpaque];  // !!!!!!!!
  //DoubleBuffered := true;

  Width := 267;
  Height := 22;
  TabStop := true;
  ParentShowHint := true;

  FGradientWidth := 256;
  FGradientHeight := 1;

//  FBack := TBitmap.Create;

  FBufferBmp := TBitmap.Create;
  //FBufferBmp.PixelFormat := pf32bit;

  mx := 0;
  my := 0;
  FIncrement := 1;
  FArrowPos := GetArrowPos;
  FHintFormat := '';
  FLayout := lyHorizontal;
  FNewArrowStyle := false;
  Aw := 6;
  Ah := 10;
  FPlacement := spAfter;
  FPickRect := Rect(Aw, 0, Width - Aw, Height - Ah);
//  FDoChange := false;
  FSelIndicator := siArrows;
  FLimit := 7;
  FWebSafe := false;
  FBevelInner := bvNone;
  FBevelOuter := bvNone;
  FBevelWidth := 1;
  FBorderStyle := bsNone;
end;

destructor TmbTrackbarPicker.Destroy;
begin
//  FBack.Free;
  inherited;
end;

procedure TmbTrackBarPicker.CalcPickRect;
var
  f: integer;
begin
  case FSelIndicator of
    siArrows:
      if not FNewArrowStyle then
      begin
        f := 0;
        Aw := 6;
        Ah := 10;
        FLimit := 7;
      end
      else
      begin
        Aw := 8;
        Ah := 9;
        f := 2;
        FLimit := 7;
      end;

    siRect:
      begin
        f := 0;
        Aw := 4;
        Ah := 5;
        FLimit := 3;
      end

    else
      f := 0;
  end;

  case FLayout of
    lyHorizontal:
      case FSelIndicator of
        siArrows:
          case FPlacement of
            spAfter  : FPickRect := Rect(Aw, 0, Width - Aw, Height - Ah - f);
            spBefore : FPickRect := Rect(Aw, Ah + f, Width - Aw, Height);
            spBoth   : FPickRect := Rect(Aw, Ah + f, Width - Aw, Height - Ah - f);
          end;
        siRect:
         FPickRect := Rect(Aw, Ah, width - 2*Aw + 1, height - Ah);
      end;
    lyVertical:
      case FSelIndicator of
        siArrows:
          case FPlacement of
            spAfter  : FPickRect := Rect(0, Aw, Width - Ah - f, Height - Aw);
            spBefore : FPickRect := Rect(Ah + f, Aw, Width, Height - Aw);
            spBoth   : FPickRect := Rect(Ah + f, Aw, Width - Ah - f, Height - Aw);
          end;
        siRect:
         FPickRect := Rect(Ah, Aw, width - 5, height - 2*Aw + 1);
      end;
  end;
end;

procedure TmbTrackBarPicker.CMGotFocus(
  var Message: {$IFDEF FPC}TLMessage{$ELSE}TCMGotFocus{$ENDIF});
begin
  inherited;
  Invalidate;
end;

procedure TmbTrackBarPicker.CMLostFocus(
  var Message: {$IFDEF FPC}TLMessage{$ELSE}TCMLostFocus{$ENDIF});
begin
  inherited;
  Invalidate;
end;

{ AWidth and AHeight are seen for horizontal arrangement of the bar }
procedure TmbTrackbarPicker.CreateGradient;
var
  i,j: integer;
  col: TColor;
  fpcol: TFPColor;
  intfimg: TLazIntfImage;
  imgHandle, imgMaskHandle: HBitmap;
begin
  if FBufferBmp = nil then
    exit;

  intfimg := TLazIntfImage.Create(0, 0);
  try
    if Layout = lyHorizontal then
    begin
      FBufferBmp.Width := FGradientWidth;
      FBufferBmp.Height := FGradientHeight;
      intfImg.LoadFromBitmap(FBufferBmp.Handle, FBufferBmp.MaskHandle);

      for i := 0 to FBufferBmp.Width-1 do
      begin
        col := GetGradientColor(i);
        if WebSafe then col := GetWebSafe(col);
        fpcol := TColorToFPColor(col);
        for j := 0 to FBufferBmp.Height-1 do
          intfImg.Colors[i, j] := fpcol;
      end;
    end
    else
    begin
      FBufferBmp.Width := FGradientHeight;
      FBufferBmp.Height := FGradientWidth;
      intfImg.LoadFromBitmap(FBufferBmp.Handle, FBufferBmp.MaskHandle);
      for j := 0 to FBufferBmp.Height-1 do
      begin
        col := GetGradientColor(FBufferBmp.Height - 1 - j);
        if WebSafe then col := GetWebSafe(col);
        fpcol := TColorToFPColor(col);
        for i := 0 to FBufferBmp.Width-1 do
          intfImg.Colors[i, j] := fpcol;
      end;
    end;

    intfimg.CreateBitmaps(imgHandle, imgMaskHandle, false);
    FBufferBmp.Handle := imgHandle;
    FBufferBmp.MaskHandle := imgMaskHandle;
  finally
    intfImg.Free;
  end;
end;

procedure TmbTrackbarPicker.CreateWnd;
begin
  inherited;
  CreateGradient;
end;

function TmbTrackbarPicker.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Result := True;
    if WheelDelta > 0 then
      Execute(TBA_WheelUp)
    else
      Execute(TBA_WheelDown);
    DoChange;
  end;
end;

procedure TmbTrackBarPicker.DrawFrames;
var
  flags: cardinal;
  R: TRect;
  i: integer;
begin
  flags := 0;
  if (FBorderStyle = bsNone) or (FBevelWidth = 0) then Exit;
  case FBevelInner of
    bvNone: flags := 0;
    bvRaised: flags := BDR_RAISEDINNER;
    bvLowered: flags := BDR_SUNKENINNER;
    bvSpace: flags := BDR_INNER;
  end;
  case FBevelOuter of
    bvRaised: flags := flags or BDR_RAISEDOUTER;
    bvLowered: flags := flags or BDR_SUNKENOUTER;
    bvSpace: flags := flags or BDR_OUTER;
  end;
  R := FPickRect;
  InflateRect(R, -FBevelWidth + 1, -FBevelWidth + 1);
  for i := 0 to FBevelWidth do
  begin
    DrawEdge(Canvas.Handle, R, flags, BF_RECT);
    InflateRect(R, 1, 1);
  end;
end;

procedure TmbTrackBarPicker.DrawMarker(p: integer);
var
  x, y: integer;
  R: TRect;
begin
  case FSelIndicator of
    siRect:
      begin
        case FLayout of
          lyHorizontal:
            begin
              p := p + Aw;
              R := Rect(p - 2, 2, p + 3, Height - 2);
            end;
          lyVertical:
            begin
              p := p + Aw;
              R := Rect(2, p - 2, Width - 2, p + 3);
            end;
        end;
        Canvas.Pen.Mode := pmNot;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(R);
        Canvas.Brush.Style := bsSolid;
        Canvas.Pen.Mode := pmCopy;
      end;

    siArrows:
      begin
        if not FNewArrowStyle then
        begin
          if Focused or (csDesigning in ComponentState) then
          begin
            Canvas.Brush.Color := clBlack;
            Canvas.Pen.Color := clBlack;
          end
          else
          begin
            Canvas.Brush.Color := clGray;
            Canvas.Pen.Color := clGray;
          end;
        end
        else
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.Pen.Color := clBtnShadow;
        end;

        if FLayout = lyHorizontal then
        begin
          x := p + Aw;
          if x < Aw then x := Aw;
          if x > Width - Aw then x := Width - Aw;
          case FPlacement of
            spAfter:
              begin
                y := Height - Aw - 1;
                if not FNewArrowStyle then
                  Canvas.Polygon([Point(x, y), Point(x - 4, y + 6), Point(x + 4, y + 6)])
                else
                  Canvas.Polygon([Point(x, y), Point(x - 4, y + 4), Point(x - 4, y + 6),
                    Point(x - 3, y + 7), Point(x + 3, y + 7), Point(x + 4, y + 6),
                    Point(x + 4, y + 4)]);
              end;
            spBefore:
              begin
                y := Aw;
                if not FNewArrowStyle then
                  Canvas.Polygon([Point(x, y), Point(x - 4, y - 6), Point(x +4, y - 6)
                  ])
                else
                  Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 4, y - 6),
                    Point(x + 3, y - 7), Point(x - 3, y - 7), Point(x - 4, y - 6),
                    Point(x - 4, y - 4) ]);
              end;
            spBoth:
              begin
                y := Height - Aw - 1;
                if not FNewArrowStyle then
                  Canvas.Polygon([Point(x, y), Point(x - 4, y + 6), Point(x + 4, y + 6) ])
                else
                  Canvas.Polygon([Point(x, y), Point(x - 4, y + 4), Point(x - 4, y + 6),
                    Point(x - 3, y + 7), Point(x + 3, y + 7), Point(x + 4, y + 6),
                    Point(x + 4, y + 4) ]);
                 y := Aw;
                if not FNewArrowStyle then
                  Canvas.Polygon([Point(x, y), Point(x - 4, y - 6), Point(x +4, y - 6) ])
                else
                  Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 4, y - 6),
                    Point(x + 3, y - 7), Point(x - 3, y - 7), Point(x - 4, y - 6),
                    Point(x - 4, y - 4) ]);
              end;
          end;  // case FPlacement
        end  // if FLayout
        else
        begin
          if not FNewArrowStyle then
            y := p + Aw
          else
           y := p + Aw - 1;
          if y < Aw then y := Aw;
          if y > Height - Aw - 1 then y := Height - Aw - 1;
          case FPlacement of
            spAfter:
              begin
                x := width - Aw - 1;
                if not FNewArrowStyle then
                  Canvas.Polygon([Point(x, y), Point(x + 6, y - 4), Point(x + 6, y + 4)])
                else
                  Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 6, y - 4),
                    Point(x + 7, y - 3), Point(x + 7, y + 3), Point(x + 6, y + 4),
                    Point(x + 4, y + 4)]);
              end;
            spBefore:
              begin
                x := Aw;
                if not FNewArrowStyle then
                  Canvas.Polygon([Point(x, y), Point(x - 6, y - 4), Point(x - 6, y + 4)])
                else
                  Canvas.Polygon([Point(x, y), Point(x - 4, y - 4), Point(x - 6, y - 4),
                    Point(x - 7, y + 1 - 4), Point(x - 7, y + 3), Point(x - 6, y + 4),
                    Point(x - 4, y + 4)]);
              end;
            spBoth:
              begin
                x := width - Aw - 1;
                if not FNewArrowStyle then
                  Canvas.Polygon([Point(x, y), Point(x + 6, y - 4), Point(x + 6, y + 4)])
               else
                 Canvas.Polygon([Point(x, y), Point(x + 4, y - 4), Point(x + 6, y - 4),
                   Point(x + 7, y - 3), Point(x + 7, y + 3), Point(x + 6, y + 4),
                   Point(x + 4, y + 4)]);
               x := Aw;
               if not FNewArrowStyle then
                 Canvas.Polygon([Point(x, y), Point(x - 6, y - 4), Point(x - 6, y + 4)])
               else
                 Canvas.Polygon([Point(x, y), Point(x - 4, y - 4), Point(x - 6, y - 4),
                   Point(x - 7, y + 1 - 4), Point(x - 7, y + 3), Point(x - 6, y + 4),
                   Point(x - 4, y + 4)]);
              end;
          end;  // case FPlacement
        end;  // else (if FLayout)
      end;  // siArrow
  end;  // case FSelIndicator
end;

procedure TmbTrackBarPicker.Execute(tbaAction: integer);
begin
 case tbaAction of
   TBA_Paint   : Canvas.StretchDraw(FPickRect, FBufferBmp);
   TBA_RedoBMP : CreateGradient;
   // Rest handled in descendants
 end;
end;

function TmbTrackBarPicker.GetArrowPos: integer;
begin
  Result := 0;
  //handled in descendants
end;

function TmbTrackBarPicker.GetHintPos(X, Y: Integer): TPoint;
begin
  case FLayout of
    lyHorizontal:
      Result := Point(X - 8, Height + 2);
    lyVertical:
      Result := Point(Width + 2, Y - 8);
  end;
end;

function TmbTrackBarPicker.GetHintStr(X, Y: Integer): string;
begin
  Result := inherited GetHintStr(X, Y);
  if Result = '' then
    Result := ReplaceFlags(FHintFormat, ['%value', '%h', '%s', '%l', '%v', '%c',
      '%m', '%y', '%k', '%r', '%g', '%b'], GetSelectedValue);
end;

procedure TmbTrackBarPicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  eraseKey: Boolean;
begin
  eraseKey := true;
  case Key of
    VK_UP:
      if FLayout = lyHorizontal then
        eraseKey := false
      else
      begin
        if not (ssCtrl in Shift) then
          Execute(TBA_VKUp)
        else
          Execute(TBA_VKCtrlUp);
        DoChange;
      end;
    VK_LEFT:
      if FLayout = lyVertical then
        eraseKey := false
      else
      begin
        if not (ssCtrl in Shift) then
          Execute(TBA_VKLeft)
        else
          Execute(TBA_VKCtrlLeft);
        DoChange;
      end;
    VK_RIGHT:
      if FLayout = lyVertical then
        eraseKey := false
      else
      begin
        if not (ssCtrl in Shift) then
          Execute(TBA_VKRight)
        else
          Execute(TBA_VKCtrlRight);
        DoChange;
      end;
    VK_DOWN:
      if FLayout = lyHorizontal then
        eraseKey := false
      else
      begin
        if not (ssCtrl in Shift) then
          Execute(TBA_VKDown)
        else
          Execute(TBA_VKCtrlDown);
        DoChange;
      end
    else
      eraseKey := false;
  end;  // case

  if eraseKey then
    Key := 0;

  inherited;
end;

procedure TmbTrackBarPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocus;
    mx := X;
    my := Y;
    FArrowPos := IfThen(FLayout = lyHorizontal, XToArrowPos(X), YToArrowPos(Y));
    Execute(TBA_MouseDown);
    //Invalidate;
  end;
  inherited;
end;

procedure TmbTrackBarPicker.MouseLeave;
begin
  inherited;
  FHintShown := false;
end;

procedure TmbTrackBarPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in shift then
  begin
    mx := X;
    my := Y;
    FArrowPos := IfThen(FLayout = lyHorizontal, XToArrowPos(X), YToArrowPos(Y));
    Execute(TBA_MouseMove);
//    Invalidate;
  end;
  inherited;
end;

procedure TmbTrackBarPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    mx := X;
    my := Y;
    FArrowPos := IfThen(FLayout = lyHorizontal, XToArrowPos(X), YToArrowPos(Y));
    Execute(TBA_MouseUp);
//    Invalidate;
  end;

  inherited;
end;

procedure TmbTrackBarPicker.Paint;
begin
  CalcPickRect;
  PaintParentBack(Canvas);
  FArrowPos := GetArrowPos;
  Execute(TBA_Paint);
  if FBorderStyle <> bsNone then
    DrawFrames;
  DrawMarker(FArrowPos);
  {
  if FDoChange then
  begin
    if Assigned(OnChange) then OnChange(Self);
    FDoChange := false;
  end;
  }
end;

procedure TmbTrackBarPicker.Resize;
begin
  inherited;
  Execute(TBA_Resize);
end;

procedure TmbTrackBarPicker.SetBevelInner(Value: TBevelCut);
begin
  if FBevelInner <> Value then
  begin
    FBevelInner := Value;
    Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetBevelOuter(Value: TBevelCut);
begin
  if FBevelOuter <> Value then
  begin
    FBevelOuter := Value;
    Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetBevelWidth(Value: TBevelWidth);
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    Invalidate;
  end;
end;

{ IMPORTANT: If pickers are created at designtime the layout must be set before
  defining the picker width and height because changing the layout will flip the
  bounding rectangle !!! }
procedure TmbTrackBarPicker.SetLayout(Value: TTrackBarLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width);  // flip rectangle
    Execute(TBA_RedoBMP);
    Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetNewArrowStyle(s: boolean);
begin
  if FNewArrowStyle <> s then
  begin
    FNewArrowStyle := s;
    Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetPlacement(Value: TSliderPlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetSelIndicator(Value: TSelIndicator);
begin
  if FSelIndicator <> Value then
  begin
    FSelIndicator := Value;
    Invalidate;
  end;
end;

procedure TmbTrackBarPicker.SetWebSafe(s: boolean);
begin
  if FWebSafe <> s then
  begin
    FWebSafe := s;
    Execute(TBA_RedoBMP);
    Invalidate;
  end;
end;

function TmbTrackBarPicker.XToArrowPos(p: integer): integer;
var
  pos: integer;
begin
  pos := p - Aw;
  if pos < 0 then pos := 0;
  if pos > Width - Aw - 1 then pos := Width - Aw - 1;
  Result := pos;
end;

function TmbTrackBarPicker.YToArrowPos(p: integer): integer;
var
  pos: integer;
begin
  pos := p - Aw;
  if pos < 0 then pos := 0;
  if pos > Height - Aw - 1 then pos := Height - Aw - 1;
  Result := pos;
end;

end.
