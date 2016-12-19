unit HSLColorPicker;

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
  SysUtils, Classes, Controls, Graphics, Forms, Menus,
  {$IFDEF DELPHI_7_UP} Themes, {$ENDIF}
  RGBHSLUtils, HSColorPicker, LColorPicker, HTMLColors, mbBasicPicker;

type
  THSLColorPicker = class(TmbBasicPicker)
  private
    FOnChange: TNotifyEvent;
    FHSPicker: THSColorPicker;
    FLPicker: TLColorPicker;
    FSelectedColor: TColor;
    FRValue, FGValue, FBValue: integer;
    FHSHint, FLHint: string;
    FLMenu, FHSMenu: TPopupMenu;
    FLumIncrement: integer;
    FHSCursor, FLCursor: TCursor;
    PBack: TBitmap;
    function GetManual: boolean;
    function GetH: Integer;
    function GetS: Integer;
    function GetL: Integer;
    function GetMaxH: Integer;
    function GetMaxS: Integer;
    function GetMaxL: Integer;
    procedure SetLumIncrement(i: integer);
    procedure SelectColor(c: TColor);
    procedure SetH(H: integer);
    procedure SetS(S: integer);
    procedure SetL(L: integer);
    procedure SetMaxH(H: Integer);
    procedure SetMaxS(S: Integer);
    procedure SetMaxL(L: Integer);
    procedure SetR(R: integer);
    procedure SetG(G: integer);
    procedure SetB(B: integer);
    procedure SetHSHint(h: string);
    procedure SetLHint(h: string);
    procedure SetLMenu(m: TPopupMenu);
    procedure SetHSMenu(m: TPopupMenu);
    procedure SetHSCursor(c: TCursor);
    procedure SetLCursor(c: TCursor);
    procedure SetSelectedColor(Value: TColor);
  protected
    procedure CreateWnd; override;
    procedure DoChange;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    procedure Resize; override;
    procedure Paint; override;
//    procedure PaintParentBack; override;
    procedure WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF});
      message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure HSPickerChange(Sender: TObject);
    procedure LPickerChange(Sender: TObject);
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
    property LuminanceIncrement: integer read FLumIncrement write SetLumIncrement default 1;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clRed;
    property HSPickerPopupMenu: TPopupMenu read FHSMenu write SetHSMenu;
    property LPickerPopupMenu: TPopupMenu read FLMenu write SetLMenu;
    property HSPickerHintFormat: string read FHSHint write SetHSHint;
    property LPickerHintFormat: string read FLHint write SetLHint;
    property HSPickerCursor: TCursor read FHSCursor write SetHSCursor default crDefault;
    property LPickerCursor: TCursor read FLCursor write SetLCursor default crDefault;
    property MaxHue: Integer read GetMaxH write SetMaxH default 359;
    property MaxSaturation: Integer read GetMaxS write SetMaxS default 240;
    property MaxLuminance: Integer read GetMaxL write SetMaxL default 240;
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

{THSLColorPicker}

uses
  mbTrackbarPicker;

constructor THSLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
//  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
  //DoubleBuffered := true;
  PBack := TBitmap.Create;
  PBack.PixelFormat := pf32bit;
  {$IFDEF DELPHI_7_UP} {$IFDEF DELPHI}
  ParentBackground := true;
  {$ENDIF} {$ENDIF}
  SetInitialBounds(0, 0, 206, 146);
  //Width := 206;
  //Height := 146;
  TabStop := true;
  FSelectedColor := clRed;
  FHSPicker := THSColorPicker.Create(Self);
  InsertControl(FHSPicker);
  FLumIncrement := 1;
  FHSCursor := crDefault;
  FLCursor := crDefault;
  with FHSPicker do
  begin
    {$IFDEF DELPHI}
    Left := 0;
    Top := 6;
    Width := 174;
    Height := 134;
    {$ELSE}
    SetInitialBounds(0, 6, 174, 134);
    {$ENDIF}
    Anchors := [akLeft, akTop, akRight, akBottom];
    Visible := true;
    MaxHue := 359;
    MaxSaturation := 240;
    MaxLuminance := 240;
    OnChange := HSPickerChange;
    OnMouseMove := DoMouseMove;
  end;
  FLPicker := TLColorPicker.Create(Self);
  InsertControl(FLPicker);
  with FLPicker do
  begin
    Layout := lyVertical;
    {$IFDEF DELPHI}
    Left := 184;
    Top := 0;
    Width := 25;
    Height := 146;
    {$ELSE}
    SetInitialBounds(184, 0, 25, 146);
    {$ENDIF}
    Anchors := [akRight, akTop, akBottom];
    Visible := true;
    MaxHue := 359;
    MaxSaturation := 240;
    MaxLuminance := 240;
    Luminance := 120;
    OnChange := LPickerChange;
    OnMouseMove := DoMouseMove;
  end;
  Hue := 0;
  Saturation := 240;
  Luminance := 120;
  FRValue := 255;
  FGValue := 0;
  FBValue := 0;
  FHSHint := 'H: %h S: %hslS'#13'Hex: %hex';
  FLHint := 'Luminance: %l';
end;

destructor THSLColorPicker.Destroy;
begin
  PBack.Free;
  FHSPicker.Free;
  FLPicker.Free;
  inherited Destroy;
end;

procedure THSLColorPicker.HSPickerChange(Sender: TObject);
begin
  FLPicker.Hue := FHSPicker.Hue;
  FLPicker.Saturation := FHSPicker.Saturation;
  FLPicker.Invalidate;
  DoChange;
end;

procedure THSLColorPicker.LPickerChange(Sender: TObject);
begin
//  FHSPicker.Lum := FLPicker.Luminance;
  FSelectedColor := FLPicker.SelectedColor;
  DoChange;
end;

procedure THSLColorPicker.DoChange;
begin
  FRValue := GetRValue(FLPicker.SelectedColor);
  FGValue := GetGValue(FLPicker.SelectedColor);
  FBValue := GetBValue(FLPicker.SelectedColor);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function THSLColorPicker.GetH: Integer;
begin
  Result := FHSPicker.Hue;
end;

function THSLColorPicker.GetS: Integer;
begin
  Result := FHSPicker.Saturation;
end;

function THSLColorPicker.GetL: integer;
begin
  Result := FLPicker.Luminance;
end;

function THSLColorPicker.GetMaxH: Integer;
begin
  Result := FHSPicker.MaxHue;
end;

function THSLColorPicker.GetMaxS: Integer;
begin
  Result := FHSPicker.MaxSaturation;
end;

function THSLColorPicker.GetMaxL: Integer;
begin
  Result := FLPicker.MaxLuminance;
end;

procedure THSLColorPicker.SelectColor(c: TColor);
begin
  FSelectedColor := c;
  FHSPicker.SelectedColor := c;
  FLPicker.SelectedColor := c;
end;

procedure THSLColorPicker.SetH(H: integer);
begin
  FHSPicker.Hue := H;
  FLPicker.Hue := H;
end;

procedure THSLColorPicker.SetS(S: integer);
begin
  FHSPicker.Saturation := S;
  FLPicker.Saturation := S;
end;

procedure THSLColorPicker.SetL(L: integer);
begin
  FLPicker.Luminance := L;
end;

procedure THSLColorPicker.SetMaxH(H: Integer);
begin
  FHSPicker.MaxHue := H;
  FLPicker.MaxHue := H;
end;

procedure THSLColorPicker.SetMaxS(S: Integer);
begin
  FHSPicker.MaxSaturation := S;
  FLPicker.MaxSaturation := S;
end;

procedure THSLColorPicker.SetMaxL(L: Integer);
begin
  FHSPicker.MaxLuminance := L;
  FLPicker.MaxLuminance := L;
end;

procedure THSLColorPicker.SetR(R: integer);
begin
  FRValue := R;
  SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLColorPicker.SetG(G: integer);
begin
  FGValue := G;
  SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLColorPicker.SetB(B: integer);
begin
  FBValue := B;
  SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

function THSLColorPicker.GetSelectedHexColor: string;
begin
  Result := ColorToHex(FSelectedColor);
end;

procedure THSLColorPicker.SetHSHint(h: string);
begin
  FHSHint := h;
  FHSPicker.HintFormat := h;
end;

procedure THSLColorPicker.SetLHint(h: string);
begin
  FLHint := h;
  FLPicker.HintFormat := h;
end;

procedure THSLColorPicker.SetLMenu(m: TPopupMenu);
begin
  FLMenu := m;
  FLPicker.PopupMenu := m;
end;

procedure THSLColorPicker.SetHSMenu(m: TPopupMenu);
begin
  FHSMenu := m;
  FHSPicker.PopupMenu := m;
end;

procedure THSLColorPicker.SetLumIncrement(i: integer);
begin
  FLumIncrement := i;
  FLPicker.Increment := i;
end;

procedure THSLColorPicker.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, x, y);
  inherited;
end;

function THSLColorPicker.GetColorUnderCursor: TColor;
begin
  Result := FHSPicker.ColorUnderCursor;
end;

function THSLColorPicker.GetHexColorUnderCursor: string;
begin
  Result := FHSPicker.GetHexColorUnderCursor;
end;

procedure THSLColorPicker.SetHSCursor(c: TCursor);
begin
  FHSCursor := c;
  FHSPicker.Cursor := c;
end;

procedure THSLColorPicker.SetLCursor(c: TCursor);
begin
  FLCursor := c;
  FLPicker.Cursor := c;
end;

procedure THSLColorPicker.WMSetFocus(
  var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF} );
begin
  FHSPicker.SetFocus;
  Message.Result := 1;
end;

function THSLColorPicker.GetManual:boolean;
begin
  Result := FHSPicker.Manual or FLPicker.Manual;
end;

                                         (*
procedure THSLColorPicker.PaintParentBack;
begin
  if PBack = nil then
  begin
    PBack := TBitmap.Create;
    PBack.PixelFormat := pf32bit;
  end;
  PBack.Width := Width;
  PBack.Height := Height;
  if Color = clDefault then begin
    PBack.Transparent := true;
    PBack.TransparentColor := clForm;
    PBack.Canvas.Brush.Color := clForm;
  end else
    PBack.Canvas.Brush.Color := Color;
  PBack.Canvas.FillRect(0, 0, Width, Height);
//  PaintParentBack(PBack);
end;
                                           *)
procedure THSLColorPicker.Resize;
begin
  inherited;

  if (FHSPicker = nil) or (FLPicker = nil) then
    exit;

  FHSPicker.Width := Width - FLPicker.Width - 15;
  FHSPicker.Height := Height - 12;

  FLPicker.Left := Width - FLPicker.Width - 2;
  FLPicker.Height := Height; // - 12;
end;

procedure THSLColorPicker.CreateWnd;
begin
  inherited;
 // PaintParentBack;
end;

procedure THSLColorPicker.Paint;
begin
  PaintParentBack(Canvas);
  Canvas.Draw(0, 0, PBack);
end;

procedure THSLColorPicker.SetSelectedColor(Value: TColor);
begin
  if FSelectedColor <> Value then
  begin
    SelectColor(Value);
    //FLPicker.Hue := FHSPicker.HueValue;
    //FLPicker.Saturation := FHSPicker.SaturationValue;
  end;
end;

end.
