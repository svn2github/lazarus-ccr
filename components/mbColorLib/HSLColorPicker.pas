unit HSLColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Controls, Graphics, Forms, Menus, Themes,
  HTMLColors, RGBHSLUtils, HSColorPicker, LColorPicker, mbBasicPicker;

type
  THSLColorPicker = class(TmbBasicPicker)
  private
    FHSPicker: THSColorPicker;
    FLPicker: TLColorPicker;
    FSelectedColor: TColor;
    FRValue, FGValue, FBValue: integer;
    FHSHint, FLHint: string;
    FLMenu, FHSMenu: TPopupMenu;
    FLumIncrement: integer;
    FHSCursor, FLCursor: TCursor;
    PBack: TBitmap;
    function GetH: Integer;
    function GetS: Integer;
    function GetL: Integer;
    function GetMaxH: Integer;
    function GetMaxS: Integer;
    function GetMaxL: Integer;
    procedure SetH(H: integer);
    procedure SetS(S: integer);
    procedure SetL(L: integer);
    procedure SetLumIncrement(i: integer);
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
    procedure DoChange; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    procedure HSPickerChange(Sender: TObject);
    procedure LPickerChange(Sender: TObject);
    procedure Paint; override;
    procedure Resize; override;
    procedure SelectColor(c: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    procedure SetFocus; override;
    property ColorUnderCursor;
    property Red: integer read FRValue write SetR;
    property Green: integer read FGValue write SetG;
    property Blue: integer read FBValue write SetB;
  published
    property Hue: integer read GetH write SetH default 0;
    property Saturation: integer read GetS write SetS default 240;
    property Luminance: integer read GetL write SetL default 120;
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
    property BorderSpacing;
    property Visible;
    property Enabled;
    property TabOrder;
    property Color;
    property ParentColor default true;
    property OnChange;
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

  PBack := TBitmap.Create;
//  PBack.PixelFormat := pf32bit;
  SetInitialBounds(0, 0, 206, 146);
  TabStop := true;
  FLumIncrement := 1;
  FHSCursor := crDefault;
  FLCursor := crDefault;

  FHSPicker := THSColorPicker.Create(Self);
  InsertControl(FHSPicker);
  with FHSPicker do
  begin
    SetInitialBounds(0, 6, 174, 134);
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
    SetInitialBounds(184, 0, 25, 146);
    Anchors := [akRight, akTop, akBottom];
    Visible := true;
    MaxHue := FHSPicker.MaxHue;
    MaxSaturation := FHSPicker.MaxSaturation;
    MaxLuminance := FHSPicker.MaxLuminance;
    Luminance := MaxLuminance div 2;
    OnChange := LPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  Hue := 0;
  Saturation := FHSPicker.MaxLuminance;
  Luminance := FHSPicker.MaxLuminance div 2;

  FHSHint := 'H: %h S: %hslS'#13'Hex: %hex';
  FLHint := 'Luminance: %l';
end;

destructor THSLColorPicker.Destroy;
begin
  PBack.Free;
  inherited Destroy;
end;

procedure THSLColorPicker.DoChange;
begin
  FSelectedColor := FLPicker.SelectedColor;
  FRValue := GetRValue(FSelectedColor);
  FGValue := GetGValue(FSelectedColor);
  FBValue := GetBValue(FSelectedColor);
  inherited;
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

function THSLColorPicker.GetH: Integer;
begin
  Result := FHSPicker.Hue;
end;

function THSLColorPicker.GetHexColorUnderCursor: string;
begin
  Result := FHSPicker.GetHexColorUnderCursor;
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

function THSLColorPicker.GetSelectedHexColor: string;
begin
  Result := ColorToHex(FSelectedColor);
end;

procedure THSLColorPicker.HSPickerChange(Sender: TObject);
begin
  if FHSPicker.Hue <> FLPicker.Hue then
    FLPicker.Hue := FHSPicker.Hue;
  if FHSPicker.Saturation <> FLPicker.Saturation then
    FLPicker.Saturation := FHSPicker.Saturation;
  DoChange;
end;

procedure THSLColorPicker.LPickerChange(Sender: TObject);
begin
  DoChange;
end;

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

procedure THSLColorPicker.Paint;
begin
  PaintParentBack(Canvas);
  Canvas.Draw(0, 0, PBack);
end;

procedure THSLColorPicker.SelectColor(c: TColor);
begin
  FSelectedColor := c;
  FHSPicker.SelectedColor := c;
  FLPicker.SelectedColor := c;
end;

procedure THSLColorPicker.SetB(B: integer);
begin
  FBValue := B;
  SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLColorPicker.SetFocus;
begin
  inherited;
  FHSPicker.SetFocus;
end;

procedure THSLColorPicker.SetG(G: integer);
begin
  FGValue := G;
  SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLColorPicker.SetH(H: integer);
begin
  FHSPicker.Hue := H;
  FLPicker.Hue := H;
end;

procedure THSLColorPicker.SetHSCursor(c: TCursor);
begin
  FHSCursor := c;
  FHSPicker.Cursor := c;
end;

procedure THSLColorPicker.SetHSHint(h: string);
begin
  FHSHint := h;
  FHSPicker.HintFormat := h;
end;

procedure THSLColorPicker.SetHSMenu(m: TPopupMenu);
begin
  FHSMenu := m;
  FHSPicker.PopupMenu := m;
end;

procedure THSLColorPicker.SetL(L: integer);
begin
  FLPicker.Luminance := L;
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

procedure THSLColorPicker.SetLumIncrement(i: integer);
begin
  FLumIncrement := i;
  FLPicker.Increment := i;
end;

procedure THSLColorPicker.SetLCursor(c: TCursor);
begin
  FLCursor := c;
  FLPicker.Cursor := c;
end;

procedure THSLColorPicker.SetMaxH(H: Integer);
begin
  FHSPicker.MaxHue := H;
  FLPicker.MaxHue := H;
end;

procedure THSLColorPicker.SetMaxL(L: Integer);
begin
  FHSPicker.MaxLuminance := L;
  FLPicker.MaxLuminance := L;
end;

procedure THSLColorPicker.SetMaxS(S: Integer);
begin
  FHSPicker.MaxSaturation := S;
  FLPicker.MaxSaturation := S;
end;

procedure THSLColorPicker.SetR(R: integer);
begin
  FRValue := R;
  SetSelectedColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLColorPicker.SetS(S: integer);
begin
  if S <> FHSPicker.Saturation then
    FHSPicker.Saturation := S;
  if S <> FLPicker.Saturation then
    FLPicker.Saturation := S;
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

(*
procedure THSLColorPicker.WMSetFocus(
  var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF} );
begin
  FHSPicker.SetFocus;
  Message.Result := 1;
end;
  *)

end.
