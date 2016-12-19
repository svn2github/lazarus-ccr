unit HSLRingPicker;

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
  SysUtils, Classes, Controls, Graphics, Forms, Menus, Math,
  {$IFDEF DELPHI_7_UP}Themes,{$ENDIF}
  RGBHSLUtils, HRingPicker, SLColorPicker, HTMLColors, mbBasicPicker;

type
  THSLRingPicker = class(TmbBasicPicker)
  private
    FOnChange: TNotifyEvent;
    FRingPicker: THRingPicker;
    FSLPicker: TSLColorPicker;
    FSelectedColor: TColor;
    FRValue, FGValue, FBValue: integer;
    FRingHint, FSLHint: string;
    FSLMenu, FRingMenu: TPopupMenu;
    FSLCursor, FRingCursor: TCursor;
    PBack: TBitmap;
    function GetManual: boolean;
    function GetHue: Integer;
    function GetLum: Integer;
    function GetSat: Integer;
    function GetMaxHue: Integer;
    function GetMaxLum: Integer;
    function GetMaxSat: Integer;
    procedure SelectColor(c: TColor);
    procedure SetHue(H: integer);
    procedure SetSat(S: integer);
    procedure SetLum(L: integer);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetR(v: integer);
    procedure SetG(v: integer);
    procedure SetB(v: integer);
    procedure SetRingHint(h: string);
    procedure SetSLHint(h: string);
    procedure SetSLMenu(m: TPopupMenu);
    procedure SetRingMenu(m: TPopupMenu);
    procedure SetRingCursor(c: TCursor);
    procedure SetSLCursor(c: TCursor);
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetColorUnderCursor: TColor; override;
    procedure RingPickerChange(Sender: TObject);
    procedure SLPickerChange(Sender: TObject);
    procedure DoChange;
    procedure Resize; override;
    procedure SetFocus; override;
    (*
    {$IFDEF DELPHI}
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    {$ELSE}
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    {$ENDIF}
    *)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHexColorUnderCursor: string; override;
    function GetSelectedHexColor: string;
    property ColorUnderCursor;
    property Hue: integer read GetHue write SetHue;
    property Saturation: integer read GetSat write SetSat;
    property Luminance: integer read GetLum write SetLum;
    property RValue: integer read FRValue write SetR default 255;
    property GValue: integer read FGValue write SetG default 0;
    property BValue: integer read FBValue write SetB default 0;
    property Manual: boolean read GetManual;
  published
    property SelectedColor: TColor read FSelectedColor write SelectColor default clRed;
    property RingPickerPopupMenu: TPopupMenu read FRingMenu write SetRingMenu;
    property SLPickerPopupMenu: TPopupMenu read FSLMenu write SetSLMenu;
    property RingPickerHintFormat: string read FRingHint write SetRingHint;
    property SLPickerHintFormat: string read FSLHint write SetSLHint;
    property RingPickerCursor: TCursor read FRingCursor write SetRingCursor default crDefault;
    property SLPickerCursor: TCursor read FSLCursor write SetSLCursor default crDefault;
    property MaxHue: Integer read GetMaxHue write SetMaxHue default 359;
    property MaxLuminance: Integer read GetMaxLum write SetMaxLum default 240;
    property MaxSaturation: Integer read GetMaxSat write SetMaxSat default 240;
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
    {$IFDEF DELPHI_7_UP} {$IFDEF DELPHI}
    property ParentBackground default true;
    {$ENDIF} {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseMove;
  end;

implementation

{THSLRingPicker}

constructor THSLRingPicker.Create(AOwner: TComponent);
begin
  inherited;
//  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque{$IFDEF DELPHI_7_UP}, csParentBackground{$ENDIF}];
  //DoubleBuffered := true;
  FRValue := 255;
  FGValue := 0;
  FBValue := 0;
  PBack := TBitmap.Create;
  PBack.PixelFormat := pf32bit;
  {$IFDEF DELPHI_7_UP} {$IFDEF DELPHI}
  ParentBackground := true;
  {$ENDIF} {$ENDIF}
  {$IFDEF DELPHI}
  Width := 245;
  Height := 245;
  {$ELSE}
  SetInitialBounds(0, 0, 245, 245);
  {$ENDIF}

  TabStop := true;
  FSelectedColor := clRed;
  FRingCursor := crDefault;
  FSLCursor := crDefault;
  FRingHint := 'Hue: %h';
  FSLHint := 'S: %hslS L: %l'#13'Hex: %hex';

  FRingPicker := THRingPicker.Create(Self);
  InsertControl(FRingPicker);
  with FRingPicker do
  begin
    {$IFDEF DELPHI}
    Left := 0;
    Top := 0;
    Width := 246;
    Height := 246;
    {$ELSE}
    SetInitialBounds(0, 0, 246, 246);
    {$ENDIF}
    //Radius := 40;
    Align := alClient;
    Visible := true;
    Saturation := FRingPicker.MaxSaturation;
    Value := FRingPicker.MaxValue;
    Hue := 0;
    OnChange := RingPickerChange;
    OnMouseMove := DoMouseMove;
  end;

  FSLPicker := TSLColorPicker.Create(Self);
  InsertControl(FSLPicker);
  with FSLPicker do
  begin
    {$IFDEF DELPHI}
    Left := 63;
    Top := 63;
    Width := 120;
    Height := 120;
    {$ELSE}
    SetInitialBounds(63, 63, 120, 120);
    {$ENDIF}
    MaxSaturation := 240;
    MaxLuminance := 240;
    Saturation := 240;
    Luminance := 240;
    Visible := true;
    OnChange := SLPickerChange;
    OnMouseMove := DoMouseMove;
  end;
end;

destructor THSLRingPicker.Destroy;
begin
  PBack.Free;
  //FRingPicker.Free;
  //FSLPicker.Free;
  inherited Destroy;
end;

procedure THSLRingPicker.Resize;
var
  circ: TPoint;
  ctr: double;
begin
  inherited;
  if (FRingPicker = nil) or (FSLPicker = nil) then
    exit;

  ctr := Min(Width, Height) / 100;
  circ.x := Min(Width, Height) div 2;
  circ.y := circ.x;

  FRingPicker.Radius := circ.x - round(12*ctr);

  FSLPicker.Left := circ.x - FSLPicker.Width  div 2;
  FSLPicker.Top  := circ.y - FSLPicker.Height div 2;
  FSLPicker.Width := round(50 * ctr);
  FSLPicker.Height := FSLPicker.Width;

  PaintParentBack(PBack);
end;

procedure THSLRingPicker.RingPickerChange(Sender: TObject);
begin
  if (FRingPicker = nil) or (FSLPicker = nil) then
    exit;
  FSLPicker.Hue := FRingPicker.Hue;
  DoChange;
end;

procedure THSLRingPicker.SLPickerChange(Sender: TObject);
begin
  if FSLPicker = nil then
    exit;
  FSelectedColor := FSLPicker.SelectedColor;
  DoChange;
end;

procedure THSLRingPicker.DoChange;
begin
  if (FRingPicker = nil) or (FSLPicker = nil) then
    exit;

  FRValue := GetRValue(FSLPicker.SelectedColor);
  FGValue := GetGValue(FSLPicker.SelectedColor);
  FBValue := GetBValue(FSLPicker.SelectedColor);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THSLRingPicker.SelectColor(c: TColor);
begin
  if (FRingPicker = nil) or (FSLPicker = nil) then
    exit;

  FRingPicker.Hue := GetHValue(c);
  //FRingPicker.Saturation := FRingPicker.MaxSaturation;
  //FRingPicker.Value := FRingPicker.MaxValue;

  FSLPicker.SelectedColor := c;
  FSelectedColor := c;
end;

procedure THSLRingPicker.SetHue(H: integer);
begin
  if (FRingPicker = nil) or (FSLPicker = nil) then
    exit;

  FRingPicker.Hue := H;
  FSLPicker.Hue := H;
end;

procedure THSLRingPicker.SetSat(S: integer);
begin
  if (FSLPicker = nil) then
    exit;
  FSLPicker.Saturation := S;
end;

procedure THSLRingPicker.SetLum(L: integer);
begin
  if (FSLPicker = nil) then
    exit;
  FSLPicker.Luminance := L;
end;

procedure THSLRingPicker.SetR(v: integer);
begin
  FRValue := v;
  SelectColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLRingPicker.SetG(v: integer);
begin
  FGValue := v;
  SelectColor(RGB(FRValue, FGValue, FBValue));
end;

procedure THSLRingPicker.SetB(v: integer);
begin
  FBValue := v;
  SelectColor(RGB(FRValue, FGValue, FBValue));
end;

function THSLRingPicker.GetSelectedHexColor: string;
begin
  Result := ColorToHex(FSelectedColor);
end;

procedure THSLRingPicker.SetRingHint(h: string);
begin
  FRingHint := h;
  FRingPicker.HintFormat := h;
end;

procedure THSLRingPicker.SetSLHint(h: string);
begin
  FSLHint := h;
  FSLPicker.HintFormat := h;
end;

procedure THSLRingPicker.SetRingMenu(m: TPopupMenu);
begin
  FRingMenu := m;
  FRingPicker.PopupMenu := m;
end;

procedure THSLRingPicker.SetSLMenu(m: TPopupMenu);
begin
  FSLMenu := m;
  FSLPicker.PopupMenu := m;
end;

procedure THSLRingPicker.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, x, y);
  inherited;
end;

function THSLRingPicker.GetColorUnderCursor: TColor;
begin
  Result := FSLPicker.ColorUnderCursor;
end;

function THSLRingPicker.GetHexColorUnderCursor: string;
begin
  Result := FSLPicker.GetHexColorUnderCursor;
end;

procedure THSLRingPicker.SetRingCursor(c: TCursor);
begin
  FRingCursor := c;
  FRingPicker.Cursor := c;
end;

procedure THSLRingPicker.SetSLCursor(c: TCursor);
begin
  FSLCursor := c;
  FSLPicker.Cursor := c;
end;
  (*
procedure THSLRingPicker.WMSetFocus(
  var Message: {$IFDEF DELPHI}TWMSetFocus{$ELSE}TLMSetFocus{$ENDIF} );
begin
  FRingPicker.SetFocus;
  Message.Result := 1;
end;
*)

procedure THSLRingPicker.SetFocus;
begin
  inherited;
  FRingPicker.SetFocus;
end;

function THSLRingPicker.GetManual:boolean;
begin
  Result := FRingPicker.Manual or FSLPicker.Manual;
end;

procedure THSLRingPicker.Paint;
begin
  PaintParentBack(PBack);
  Canvas.Draw(0, 0, PBack);
end;

procedure THSLRingPicker.CreateWnd;
begin
  inherited;
  PaintParentBack(PBack);
end;

function THSLRingPicker.GetHue: Integer;
begin
  Result := FRingPicker.Hue;
end;

function THSLRingPicker.GetSat: Integer;
begin
  Result := FSLPicker.Saturation;
end;

function THSLRingPicker.GetLum: Integer;
begin
  Result := FSLPicker.Luminance;
end;

function THSLRingPicker.GetMaxHue: Integer;
begin
  Result := FRingPicker.MaxHue;
end;

function THSLRingPicker.GetMaxSat: Integer;
begin
  Result := FSLPicker.MaxSaturation;
end;

function THSLRingPicker.GetMaxLum: Integer;
begin
  Result := FSLPicker.MaxLuminance;
end;

procedure THSLRingPicker.SetMaxHue(H: Integer);
begin
  FRingPicker.MaxHue := H;
end;

procedure THSLRingPicker.SetMaxLum(L: Integer);
begin
  FSLPicker.MaxLuminance := L;
end;

procedure THSLRingPicker.SetMaxSat(S: Integer);
begin
  FSLPicker.MaxSaturation := S;
end;

end.
