unit mbColorPickerControl;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, Forms, Themes,
  RGBHSLUtils, RGBHSVUtils, RGBCMYKUtils, RGBCIEUtils, HTMLColors, mbBasicPicker;

type
  TMarkerStyle = (msCircle, msSquare, msCross, msCrossCirc);

  { TmbCustomPicker }

  TmbCustomPicker = class(TmbBasicPicker)
  private
    FHintFormat: string;
    FMarkerStyle: TMarkerStyle;
    FWebSafe: boolean;
    procedure SetMarkerStyle(s: TMarkerStyle);
    procedure SetWebSafe(s: boolean);
  protected
    FManual: Boolean;
    FSelected: TColor;
    mx, my, mdx, mdy: integer;
    procedure CreateGradient; override;
    function GetHintStr(X, Y: Integer): String; override;
    function GetSelectedColor: TColor; virtual;
    procedure InternalDrawMarker(X, Y: Integer; C: TColor);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetSelectedColor(C: TColor); virtual;
    procedure WebSafeChanged; dynamic;
    {$IFDEF DELPHI}
    procedure CMGotFocus(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMLostFocus(var Message: TCMLostFocus); message CM_EXIT;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    {$ELSE}
    procedure CMGotFocus(var Message: TLMessage); message CM_ENTER;
    procedure CMLostFocus(var Message: TLMessage); message CM_EXIT;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    {$ENDIF}
    property MarkerStyle: TMarkerStyle read FMarkerStyle write SetMarkerStyle;
  public
    constructor Create(AOwner: TComponent); override;
    property ColorUnderCursor;
    property Manual: boolean read FManual;
  published
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property HintFormat: string read FHintFormat write FHintFormat;
    property WebSafe: boolean read FWebSafe write SetWebSafe default false;
  end;

  TmbColorPickerControl = class(TmbCustomPicker)
  published
    property Anchors;
    property Align;
    property BorderSpacing;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property TabStop default true;
    property Color;
    property ParentColor;
   {$IFDEF DELPHI_7_UP}{$IFDEF DELPHI}
    property ParentBackground default true;
   {$ENDIF}{$ENDIF}
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnContextPopup;
    property OnGetHintStr;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
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
  {$IFDEF FPC}
  IntfGraphics, fpimage,
  {$ENDIF}
  ScanLines, PalUtils, SelPropUtils;

constructor TmbCustomPicker.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle + [csOpaque] - [csAcceptsControls];

  TabStop := true;
  mx := 0;
  my := 0;
  mdx := 0;
  mdy := 0;
  FHintFormat := 'Hex #%hex'#10#13'RGB[%r, %g, %b]'#10#13'HSL[%hslH, %hslS, %hslL]'#10#13'HSV[%hsvH, %hsvS, %hsvV]'#10#13'CMYK[%c, %m, %y, %k]'#10#13'L*a*b*[%cieL, %cieA, %cieB]'#10#13'XYZ[%cieX, %cieY, %cieZ]';
  FWebSafe := false;
end;

procedure TmbCustomPicker.CMGotFocus(
  var Message: {$IFDEF FPC}TLMessage{$ELSE}TCMGotFocus{$ENDIF} );
begin
  inherited;
  Invalidate;
end;

procedure TmbCustomPicker.CMLostFocus(
  var Message: {$IFDEF FPC}TLMessage{$ELSE}TCMLostFocus{$ENDIF} );
begin
  inherited;
  Invalidate;
end;

procedure TmbCustomPicker.CMMouseLeave(
  var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  mx := 0;
  my := 0;
  inherited;
end;

procedure TmbCustomPicker.CreateGradient;
var
  x, y: Integer;
  col: TColor;
  fpcol: TFPColor;
  intfimg: TLazIntfImage;
  imgHandle, imgMaskHandle: HBitmap;
begin
  if FBufferBmp = nil then
  begin
    FBufferBmp := TBitmap.Create;
//    FBufferBmp.PixelFormat := pf32bit;
  end;
  FBufferBmp.Width := FGradientWidth;
  FBufferBmp.Height := FGradientHeight;

  intfimg := TLazIntfImage.Create(FBufferBmp.Width, FBufferBmp.Height);
  try
    intfImg.LoadFromBitmap(FBufferBmp.Handle, FBufferBmp.MaskHandle);

    for y := 0 to FBufferBmp.Height - 1 do
    begin
      for x := 0 to FBufferBmp.Width - 1 do
      begin
        col := GetGradientColor2D(x, y);
        if WebSafe then
          col := GetWebSafe(col);
        fpcol := TColorToFPColor(col);
        intfImg.Colors[x, y] := fpcol;
      end;
    end;

    intfimg.CreateBitmaps(imgHandle, imgMaskHandle, false);
    FBufferBmp.Handle := imgHandle;
    FBufferBmp.MaskHandle := imgMaskHandle;
  finally
    intfimg.Free;
  end;
end;

function TmbCustomPicker.GetHintStr(X, Y: Integer): String;
begin
  Result := FormatHint(FHintFormat, GetColorUnderCursor);
end;

function TmbCustomPicker.GetSelectedColor: TColor;
begin
  Result := FSelected;  // valid for most descendents
end;

procedure TmbCustomPicker.InternalDrawMarker(X, Y: Integer; C: TColor);
begin
  case MarkerStyle of
    msCircle    : DrawSelCirc(x, y, Canvas);
    msSquare    : DrawSelSquare(x, y, Canvas);
    msCross     : DrawSelCross(x, y, Canvas, c);
    msCrossCirc : DrawSelCrossCirc(x, y, Canvas, c);
  end;
end;

procedure TmbCustomPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  mx := x;
  my := y;
end;

procedure TmbCustomPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  mx := x;
  my := y;
end;

procedure TmbCustomPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  mx := x;
  my := y;
end;

procedure TmbCustomPicker.SetMarkerStyle(s: TMarkerStyle);
begin
  if FMarkerStyle <> s then
  begin
    FMarkerStyle := s;
    Invalidate;
  end;
end;

procedure TmbCustomPicker.SetSelectedColor(C: TColor);
begin
  FSelected := C;
  //handled in descendents
end;

procedure TmbCustomPicker.SetWebSafe(s: boolean);
begin
  if FWebSafe <> s then
  begin
    FWebSafe := s;
    WebSafeChanged;
  end;
end;

procedure TmbCustomPicker.WebSafeChanged;
begin
  CreateGradient;
  Invalidate;
end;

end.
