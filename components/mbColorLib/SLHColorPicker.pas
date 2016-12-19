unit SLHColorPicker;

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
  RGBHSLUtils, mbTrackBarPicker, SLColorPicker, HColorPicker, Menus,
  {$IFDEF DELPHI_7_UP} Themes, {$ENDIF} HTMLColors, mbBasicPicker;

type
  TSLHColorPicker = class(TmbBasicPicker)
  private
    FOnChange: TNotifyEvent;
    FSLPicker: TSLColorPicker;
    FHPicker: THColorPicker;
    FSelectedColor: TColor;
    FHValue, FSValue, FLValue: Double;
    FMaxH, FMaxS, FMaxL: Integer;
    FRValue, FGValue, FBValue: integer;
    FSLHint, FHHint: string;
    FSLMenu, FHMenu: TPopupMenu;
    FSLCursor, FHCursor: TCursor;
    PBack: TBitmap;
    function GetManual: boolean;
    procedure SelectColor(c: TColor);
    function GetH: Integer;
    function GetS: Integer;
    function GetL: Integer;
    procedure SetH(H: integer);
    procedure SetS(S: integer);
    procedure SetL(L: integer);
    procedure SetR(R: integer);
    procedure SetG(G: integer);
    procedure SetB(B: integer);
    procedure SetMaxH(H: Integer);
    procedure SetMaxS(S: Integer);
    procedure SetMaxL(L: Integer);
    procedure SetHHint(h: string);
    procedure SetSLHint(h: string);
    procedure SetSLMenu(m: TPopupMenu);
    procedure SetHMenu(m: TPopupMenu);
    procedure SetHCursor(c: TCursor);
    procedure SetSLCursor(c: TCursor);
    procedure HPickerChange(Sender: TObject);
    procedure SLPickerChange(Sender: TObject);
  protected
//    procedure CreateWnd; override;
    procedure DoChange;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    procedure Paint; override;
//    procedure PaintParentBack; override;
    procedure Resize; override;
    procedure WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF});
      message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    property ColorUnderCursor;
    property Hue: integer read GetH write SetH;
    property Saturation: integer read GetS write SetS;
    property Luminance: integer read GetL write SetL;
    property Red: integer read FRValue write SetR default 255;
    property Green: integer read FGValue write SetG default 0;
    property Blue: integer read FBValue write SetB default 0;
    property Manual: boolean read GetManual;
  published
    property SelectedColor: TColor read FSelectedColor write SelectColor default clRed;
    property HPickerPopupMenu: TPopupMenu read FHMenu write SetHMenu;
    property SLPickerPopupMenu: TPopupMenu read FSLMenu write SetSLMenu;
    property HPickerHintFormat: string read FHHint write SetHHint;
    property SLPickerHintFormat: string read FSLHint write SetSLHint;
    property HPickerCursor: TCursor read FHCursor write SetHCursor default crDefault;
    property SLPickerCursor: TCursor read FSLCursor write SetSLCursor default crDefault;
    property TabStop default true;
    property ShowHint;
    property ParentShowHint;
    property Anchors;
    property Align;
    property Visible;
    property Enabled;
    property TabOrder;
    property Color;
    property ParentColor default true;
   {$IFDEF DELPHI_7_UP}{$IFDEF DELPHI}
    property ParentBackground default true;
   {$ENDIF}{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseMove;
  end;

implementation

const
  WSL = 255;
  HSL = 255;
  WH = 40;
  DIST = 2;
  VDELTA = 8;

{TSLHColorPicker}

constructor TSLHColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
  DoubleBuffered := true;
  FMaxH := 359;
  FMaxS := 240;
  FMaxL := 100;
  PBack := TBitmap.Create;
  PBack.PixelFormat := pf32bit;
  ParentColor := true;
 {$IFDEF DELPHI_7_UP}{$IFDEF DELPHI}
  ParentBackground := true;
 {$ENDIF}{$ENDIF}
 {$IFDEF DELPHI}
  Width := 297;
  Height := 271;
 {$ELSE}
  SetInitialBounds(0, 0, WSL + DIST + WH, HSL + 2*VDELTA);
 {$ENDIF}
  TabStop := true;
  FSelectedColor := clRed;
  FHPicker := THColorPicker.Create(Self);
  InsertControl(FHPicker);
  FHCursor := crDefault;
  FSLCursor := crDefault;

  // Hue picker
  with FHPicker do
  begin
    Layout := lyVertical;  // put before setting width and height
    {$IFDEF DELPHI}
    Left := 257;
    Top := 0;
    Width := 40;
    Height := 271;
    {$ELSE}
    SetInitialBounds(WSL + DIST, 0, WH, HSL + 2*VDELTA);
    {$ENDIF}
    MaxHue := self.FMaxH;
    MaxSaturation := 255;
    MaxValue := 255;
    Saturation := MaxSaturation;
    Value := MaxValue;
  //  Anchors := [akTop, akRight, akBottom];
    Visible := true;
//    Layout := lyVertical;
    ArrowPlacement := spBoth;
    NewArrowStyle := true;
    OnChange := HPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  // Saturation-Lightness picker
  FSLPicker := TSLColorPicker.Create(Self);
  InsertControl(FSLPicker);
  with FSLPicker do
  begin
    {$IFDEF DELPHI}
    Left := 0;
    Top := DELTA;
    Width := 255;
    Height := self.Height - 2 * VDELTA;
    {$ELSE}
    SetInitialBounds(0, VDELTA, WSL, HSL);
    {$ENDIF}
    //Anchors := [akLeft, akRight, akTop, akBottom];
    Visible := true;
    SelectedColor := clRed;
    MaxHue := FMaxH;
    MaxSaturation := FMaxS;
    MaxLuminance := FMaxL;
    Saturation := FMaxS;
    Luminance := FMaxL;
    OnChange := SLPickerChange;
    OnMouseMove := DoMouseMove;
  end;
  FHValue := 0;
  FSValue := 1.0;
  FLValue := 1.0;
  FRValue := 255;
  FGValue := 0;
  FBValue := 0;
  FHHint := 'Hue: %h';
  FSLHint := 'S: %hslS L: %l'#13'Hex: %hex';
end;

destructor TSLHColorPicker.Destroy;
begin
  PBack.Free;
  FHPicker.Free;
  FSLPicker.Free;
  inherited Destroy;
end;

procedure TSLHColorPicker.HPickerChange(Sender: TObject);
begin
  FSLPicker.Hue := FHPicker.Hue;
  DoChange;
end;

procedure TSLHColorPicker.SLPickerChange(Sender: TObject);
begin
  FSelectedColor := FSLPicker.SelectedColor;
  DoChange;
end;

procedure TSLHColorPicker.DoChange;
begin
  FHValue := FHPicker.Hue / FHPicker.MaxHue;
  FSValue := FSLPicker.Saturation / FSLPicker.MaxSaturation;
  FLValue := FSLPicker.Luminance / FSLPicker.MaxLuminance;
  FRValue := GetRValue(FSLPicker.SelectedColor);
  FGValue := GetGValue(FSLPicker.SelectedColor);
  FBValue := GetBValue(FSLPicker.SelectedColor);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSLHColorPicker.SelectColor(c: TColor);
begin
  FSelectedColor := c;
  FHPicker.Hue := GetHValue(c);
  FSLPicker.SelectedColor := c;
end;

function TSLHColorPicker.GetH: Integer;
begin
  Result := Round(FHValue * FMaxH);
end;

function TSLHColorPicker.GetS: Integer;
begin
  Result := Round(FSValue * FMaxS);
end;

function TSLHColorPicker.GetL: Integer;
begin
  Result := ROund(FLValue * FMaxL);
end;

procedure TSLHColorPicker.SetH(H: integer);
begin
  FHValue := H / FMaxH;
  FSLPicker.Hue := H;
  FHPicker.Hue := H;
end;

procedure TSLHColorPicker.SetS(S: integer);
begin
  FSValue := S / FMaxS;
  FSLPicker.Saturation := S;
end;

procedure TSLHColorPicker.SetL(L: integer);
begin
  FLValue := L / FMaxL;
  FSLPicker.Luminance := L;
end;

procedure TSLHColorPicker.SetR(R: integer);
begin
  FRValue := R;
  SelectColor(RGB(FRValue, FGValue, FBValue));
end;

procedure TSLHColorPicker.SetG(G: integer);
begin
  FGValue := G;
  SelectColor(RGB(FRValue, FGValue, FBValue));
end;

procedure TSLHColorPicker.SetB(B: integer);
begin
  FBValue := B;
  SelectColor(RGB(FRValue, FGValue, FBValue));
end;

procedure TSLHColorPicker.SetMaxH(H: Integer);
begin
  FMaxH := H;
  FSLPicker.MaxHue := H;
  FHPicker.MaxHue := H;
end;

procedure TSLHColorPicker.SetMaxS(S: Integer);
begin
  FMaxS := S;
  FSLPicker.MaxSaturation := S;
end;

procedure TSLHColorPicker.SetMaxL(L: Integer);
begin
  FMaxL := L;
  FSLPicker.MaxLuminance := L;
end;

function TSLHColorPicker.GetSelectedHexColor: string;
begin
  Result := ColorToHex(FSelectedColor);
end;

procedure TSLHColorPicker.SetHHint(h: string);
begin
  FHHint := h;
  FHPicker.HintFormat := h;
end;

procedure TSLHColorPicker.SetSLHint(h: string);
begin
  FSLHint := h;
  FSLPicker.HintFormat := h;
end;

procedure TSLHColorPicker.SetSLMenu(m: TPopupMenu);
begin
  FSLMenu := m;
  FSLPicker.PopupMenu := m;
end;

procedure TSLHColorPicker.SetHMenu(m: TPopupMenu);
begin
  FHMenu := m;
  FHPicker.PopupMenu := m;
end;

procedure TSLHColorPicker.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, x, y);
 inherited;
end;

function TSLHColorPicker.GetColorUnderCursor: TColor;
begin
  Result := FSLPicker.ColorUnderCursor;
end;

function TSLHColorPicker.GetHexColorUnderCursor: string;
begin
  Result := FSLPicker.GetHexColorUnderCursor;
end;

procedure TSLHColorPicker.SetHCursor(c: TCursor);
begin
  FHCursor := c;
  FHPicker.Cursor := c;
end;

procedure TSLHColorPicker.SetSLCursor(c: TCursor);
begin
  FSLCursor := c;
  FSLPicker.Cursor := c;
end;

procedure TSLHColorPicker.WMSetFocus(
  var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF} );
begin
  FHPicker.SetFocus;
  Message.Result := 1;
end;

function TSLHColorPicker.GetManual:boolean;
begin
  Result := FHPicker.Manual or FSLPicker.Manual;
end;

procedure TSLHColorPicker.Resize;
begin
  inherited;
//  PaintParentBack;

  if (FSLPicker = nil) or (FHPicker = nil) then
    exit;

  FSLPicker.Width := Width - FHPicker.Width - DIST;
  FSLPicker.Height := Height - 2*VDELTA;

  FHPicker.Left := Width - FHPicker.Width;
  FHPicker.Height := Height;
end;
                          {
procedure TSLHColorPicker.PaintParentBack;
begin
  if PBack = nil then
  begin
    PBack := TBitmap.Create;
    PBack.PixelFormat := pf32bit;
  end;
  PBack.Width := Width;
  PBack.Height := Height;
  PaintParentBack(PBack);
end;                       }

procedure TSLHColorPicker.Paint;
begin
  PaintParentBack(Canvas);
//  Canvas.Draw(0, 0, PBack);
end;
    (*
procedure TSLHColorPicker.CreateWnd;
begin
  inherited;
  PaintParentBack;
end;
  *)
end.
