unit mbColorPickerControl;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$I mxs.inc}

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Forms,
  {$IFDEF DELPHI_7_UP} Themes,{$ENDIF}
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
    FOnChange: TNotifyEvent;
    procedure CreateGradient; override;
    function GetHintText: String; override;
    function GetSelectedColor: TColor; virtual;
    procedure SetSelectedColor(C: TColor); virtual;
    procedure InternalDrawMarker(X, Y: Integer; C: TColor);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure WebSafeChanged; dynamic;
//    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColorAtPoint(x, y: integer): TColor; dynamic;
    function GetHexColorAtPoint(X, Y: integer): string;
    function GetColorUnderCursor: TColor;
    function GetHexColorUnderCursor: string;
    property ColorUnderCursor: TColor read GetColorUnderCursor;
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
  ControlStyle := ControlStyle + [csOpaque] - [csAcceptsControls];
  DoubleBuffered := true;
  TabStop := true;
 {$IFDEF DELPHI_7_UP}{$IFDEF DELPHI}
  ParentBackground := true;
 {$ENDIF}{$ENDIF}
  mx := 0;
  my := 0;
  mdx := 0;
  mdy := 0;
  FHintFormat := 'Hex #%hex'#10#13'RGB[%r, %g, %b]'#10#13'HSL[%hslH, %hslS, %hslL]'#10#13'HSV[%hsvH, %hsvS, %hsvV]'#10#13'CMYK[%c, %m, %y, %k]'#10#13'L*a*b*[%cieL, %cieA, %cieB]'#10#13'XYZ[%cieX, %cieY, %cieZ]';
  FWebSafe := false;
end;

procedure TmbCustomPicker.CreateWnd;
begin
  inherited;
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
//  x, y, skip: integer;
  x, y: Integer;
  row: pRGBQuadArray;
  c: TColor;
  {$IFDEF FPC}
  intfimg: TLazIntfImage;
  imgHandle, imgMaskHandle: HBitmap;
  {$ENDIF}
begin
  if FGradientBmp = nil then
  begin
    FGradientBmp := TBitmap.Create;
    FGradientBmp.PixelFormat := pf32bit;
  end;
  FGradientBmp.Width := FGradientWidth;
  FGradientBmp.Height := FGradientHeight;

  {$IFDEF FPC}
  intfimg := TLazIntfImage.Create(FGradientBmp.Width, FGradientBmp.Height);
  try
    intfImg.LoadFromBitmap(FGradientBmp.Handle, FGradientBmp.MaskHandle);
  {$ENDIF}

    for y := 0 to FGradientBmp.Height - 1 do
    begin
      {$IFDEF FPC}
      row := intfImg.GetDataLineStart(y); //FGradientBmp.Height - 1 - y);
      {$ELSE}
      row := FHSVBmp.Scanline(y); //FGradientBmp.Height - 1 - y);
      {$ENDIF}

      for x := 0 to FGradientBmp.Width - 1 do
      begin
        c := GetGradientColor2D(x, y);
        if WebSafe then
          c := GetWebSafe(c);
        row[x] := RGBToRGBQuad(GetRValue(c), GetGValue(c), GetBValue(c));
      end;
    end;

{$IFDEF FPC}
    intfimg.CreateBitmaps(imgHandle, imgMaskHandle, false);
    FGradientBmp.Handle := imgHandle;
    FGradientBmp.MaskHandle := imgMaskHandle;
  finally
   intfimg.Free;
  end;
{$ENDIF}
end;

function TmbCustomPicker.GetHintText: String;
begin
  Result := FormatHint(FHintFormat, GetColorUnderCursor)
end;

function TmbCustomPicker.GetSelectedColor: TColor;
begin
  Result := FSelected;  // valid for most descendents
end;

procedure TmbCustomPicker.SetSelectedColor(C: TColor);
begin
  FSelected := C;
  //handled in descendents
end;

function TmbCustomPicker.GetColorAtPoint(x, y: integer): TColor;
begin
  Result := Canvas.Pixels[x, y];  // valid for most descendents
end;

function TmbCustomPicker.GetHexColorAtPoint(X, Y: integer): string;
begin
  Result := ColorToHex(GetColorAtPoint(x, y));
end;

function TmbCustomPicker.GetColorUnderCursor: TColor;
begin
  Result := GetColorAtPoint(mx, my);
end;

function TmbCustomPicker.GetHexColorUnderCursor: string;
begin
  Result := ColorToHex(GetColorAtPoint(mx, my));
end;

procedure TmbCustomPicker.InternalDrawMarker(X, Y: Integer; C: TColor);
begin
  case MarkerStyle of
    msCircle: DrawSelCirc(x, y, Canvas);
    msSquare: DrawSelSquare(x, y, Canvas);
    msCross: DrawSelCross(x, y, Canvas, c);
    msCrossCirc: DrawSelCrossCirc(x, y, Canvas, c);
  end;
end;
             (*
procedure TmbCustomPicker.CMHintShow(var Message: TCMHintShow);
begin
  if GetColorUnderCursor <> clNone then
    with TCMHintShow(Message) do
      if not ShowHint then
        Message.Result := 1
      else
        with HintInfo^ do
        begin
          Result := 0;
          ReshowTimeout := 1;
          HideTimeout := 5000;
          HintStr := FormatHint(FHintFormat, GetColorUnderCursor);;
        end;
  inherited;
end;       *)

procedure TmbCustomPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  mx := x;
  my := y;
end;

procedure TmbCustomPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
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
