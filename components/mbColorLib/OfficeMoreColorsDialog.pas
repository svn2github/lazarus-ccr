unit OfficeMoreColorsDialog;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls,
  HexaColorPicker, HSLColorPicker, RGBHSLUtils, mbColorPreview,
  {$IFDEF mbXP_Lib}mbXPSpinEdit, mbXPSizeGrip,{$ELSE} Spin,{$ENDIF}
  HTMLColors, SLHColorPicker, HSLRingPicker, RColorPicker, GColorPicker,
  BColorPicker;

type

  { TOfficeMoreColorsWin }

  TOfficeMoreColorsWin = class(TForm)
    BTrackbar: TBColorPicker;
    Bevel1: TBevel;
    GTrackbar: TGColorPicker;
    HSLRing: THSLRingPicker;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LLum: TLabel;
    LSat: TLabel;
    LHue: TLabel;
    nbRGB: TPage;
    PickerNotebook: TNotebook;
    nbHSL: TPage;
    nbHSLRing: TPage;
    nbSLH: TPage;
    Pages: TPageControl;
    RTrackbar: TRColorPicker;
    SLH: TSLHColorPicker;
    Standard: TTabSheet;
    Custom: TTabSheet;
    Hexa: THexaColorPicker;
    HSL: THSLColorPicker;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbColorDisplay: TComboBox;
    LRed: TLabel;
    LGreen: TLabel;
    LBlue: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OKbtn: TButton;
    Cancelbtn: TButton;
    NewSwatch: TmbColorPreview;
    OldSwatch: TmbColorPreview;
    procedure cbColorDisplayChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HSLChange(Sender: TObject);
    procedure ERedChange(Sender: TObject);
    procedure EGreenChange(Sender: TObject);
    procedure EBlueChange(Sender: TObject);
    procedure EHueChange(Sender: TObject);
    procedure ESatChange(Sender: TObject);
    procedure ELumChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    function GetHint(c: TColor): string;
    procedure HexaChange(Sender: TObject);
    procedure HSLRingChange(Sender: TObject);
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure ColorPickerChange(Sender: TObject);
    procedure SLHChange(Sender: TObject);
  private
    {$IFDEF mbXP_Lib}
    ERed, EGreen, EBlue: TmbXPSpinEdit;
    EHue, ESat, ELum: TmbXPSpinEdit;
    grip: TmbXPSizeGrip;
    {$ELSE}
    ERed, EGreen, EBlue: TSpinEdit;
    EHue, ESat, ELum: TSpinEdit;
    {$ENDIF}
    FMaxHue: Integer;
    FMaxSat: Integer;
    FMaxLum: Integer;
    FLockChange: Integer;
    function GetShowHint: Boolean;
    procedure SetAllCustom(c: TColor);
    procedure SetAllToSel(c: TColor);
    procedure SetShowHint(AValue: boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
//    procedure CreateWnd; override;
  public
    property MaxHue: Integer read FMaxHue write FMaxHue;
    property MaxSaturation: Integer read FMaxSat write FMaxSat;
    property MaxLuminance: Integer read FMaxLum write FMaxLum;
  published
    property ShowHint: Boolean read GetShowHint write SetShowHint;
  end;

var
  OfficeMoreColorsWin: TOfficeMoreColorsWin;

implementation

{$R *.lfm}

procedure TOfficeMoreColorsWin.ColorPickerChange(Sender: TObject);
begin
  if Sender = HSL then
    SetAllCustom(HSL.SelectedColor);
  if Sender = HSLRing then
    SetAllCustom(HSLRing.SelectedColor);
  if Sender = SLH then
    SetAllCustom(SLH.SelectedColor);
  if Sender = RTrackbar then
    SetAllCustom(RTrackbar.SelectedColor);
  if Sender = GTrackbar then
    SetAllCustom(GTrackbar.SelectedColor);
  if Sender = BTrackbar then
    SetAllCustom(BTrackbar.SelectedColor);
end;

procedure TOfficeMoreColorsWin.CreateParams(var Params: TCreateParams);
begin 
  inherited CreateParams(Params); 
  Params.Style := WS_CAPTION or WS_SIZEBOX or WS_SYSMENU; 
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE; 
end; 

procedure TOfficeMoreColorsWin.cbColorDisplayChange(Sender: TObject);
begin
  PickerNotebook.PageIndex := cbColorDisplay.ItemIndex;
  SetAllCustom(NewSwatch.Color);
  {
  HSL.Visible := cbColorDisplay.ItemIndex = 0;
  HSLRing.Visible := cbColorDisplay.ItemIndex = 1;
  SLH.Visible := cbColorDisplay.ItemIndex = 2;
  }
  if HSL.Visible then
    HSL.SelectedColor := NewSwatch.Color;
  if HSLRing.Visible then
    HSLRing.SelectedColor := NewSwatch.Color;
  if SLH.Visible then
    SLH.SelectedColor := NewSwatch.Color;
end;

procedure TOfficeMoreColorsWin.EBlueChange(Sender: TObject);
begin
  if (EBlue.Text <> '') and EBlue.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Blue := EBlue.Value;
      SLH.Blue := EBlue.Value;
      NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EGreenChange(Sender: TObject);
begin
  if (EGreen.Text <> '') and EGreen.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Green := EGreen.Value;
      SLH.Green := EGreen.Value;
      NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EHueChange(Sender: TObject);
begin
  if (EHue.Text <> '') and EHue.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Hue := EHue.Value;
      SLH.Hue := EHue.Value;
      NewSwatch.Color := HSLToRGB(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELum.Value/FMaxLum);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.ELumChange(Sender: TObject);
begin
  if (ELum.Text <> '') and ELum.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Luminance := ELum.Value;
      NewSwatch.Color := HSLToRGB(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELum.Value/FMaxLum);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.ERedChange(Sender: TObject);
begin
  if (ERed.Text <> '') and ERed.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Red := ERed.Value;
      SLH.Red := ERed.Value;
      NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.ESatChange(Sender: TObject);
begin
  if (ESat.Text <> '') and ESat.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Saturation := ESat.Value;
      SLH.Saturation := ESat.Value;
      NewSwatch.Color := HSLToRGB(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELum.Value/FMaxLum);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.FormCreate(Sender: TObject);
begin
  FMaxHue := 359;
  FMaxSat := 240;
  FMaxLum := 240;

  HSL.MaxHue := FMaxHue;
  HSL.MaxSaturation := FMaxSat;
  HSL.MaxLuminance := FMaxLum;

  HSLRing.MaxHue := FMaxHue;
  HSLRing.MaxSaturation := FMaxSat;
  HSLRing.MaxLuminance := FMaxLum;

  SLH.MaxHue := FMaxHue;
  SLH.MaxSaturation := FMaxSat;
  SLH.MaxLuminance := FMaxLum;

 {$IFDEF mbXP_Lib}
  ERed := TmbXPSpinEdit.CreateParented(Custom.Handle);
  EGreen := TmbXPSpinEdit.CreateParented(Custom.Handle);
  EBlue := TmbXPSpinEdit.CreateParented(Custom.Handle);
  grip := TmbXPSizeGrip.CreateParented(Self.Handle);
 {$ELSE}
  ERed := TSpinEdit.CreateParented(Custom.Handle);
  EGreen := TSpinEdit.CreateParented(Custom.Handle);
  EBlue := TSpinEdit.CreateParented(Custom.Handle);
  EHue := TSpinEdit.CreateParented(Custom.Handle);
  ESat := TSpinEdit.CreateParented(Custom.Handle);
  ELum := TSpinEdit.CreateParented(Custom.Handle);
 {$ENDIF}
  with ERed do
  begin
    Name := 'ERed';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left;
    Top := LRed.Top - 4;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := 255;
    MinValue := 0;
    Value := 0;
    OnChange := @ERedChange;
//   TabOrder := cbColorDisplay.TabOrder + 1;
  end;
  with EGreen do
  begin
    Name := 'EGreen';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left;
    Top := LGreen.Top - 3;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := 255;
    MinValue := 0;
    Value := 0;
    OnChange := @EGreenChange;
//   TabOrder := ERed.TabOrder + 1;
  end;
  with EBlue do
  begin
    Name := 'EBlue';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left;
    Top := LBlue.Top - 4;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := 255;
    MinValue := 0;
    Value := 0;
    OnChange := @EBlueChange;
//   TabOrder := EGreen.TabOrder + 1;
  end;
  with EHue do
  begin
    Name := 'EHue';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left + cbColorDisplay.Width - Width;
    Top := ERed.Top;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := FMaxHue;
    MinValue := 0;
    Value := 0;
    OnChange := @EHueChange;
//   TabOrder := EBlue.TabOrder + 1;
  end;
  with ESat do
  begin
    Name := 'ESat';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left + cbColorDisplay.Width - Width;
    Top := EGreen.Top;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := FMaxSat;
    MinValue := 0;
    Value := 0;
    OnChange := @ESatChange;
//   TabOrder := EHue.TabOrder + 1;
  end;
  with ELum do
  begin
    Name := 'ELum';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left + cbColorDisplay.Width - Width;
    Top := EBlue.Top;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := FMaxLum;
    MinValue := 0;
    Value := 0;
    OnChange := @ELumChange;
//   TabOrder := ESat.TabOrder + 1;
  end;
  Custom.InsertControl(ERed);
  Custom.InsertControl(EGreen);
  Custom.InsertControl(EBlue);
  Custom.InsertControl(EHue);
  Custom.InsertControl(ESat);
  Custom.InsertControl(ELum);

 {$IFDEF mbXP_Lib}
  with grip do
  begin
    Name := 'grip';
    Width := 15;
    Height := 15;
    Left := 308;
    Top := 314;
    Anchors := [akRight, akBottom];
  end;
  InsertControl(grip);
 {$ENDIF}

  OKBtn.TabOrder := ELum.TabOrder + 1;
  CancelBtn.TabOrder := OKBtn.TabOrder + 1;
end;

procedure TOfficeMoreColorsWin.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: ModalResult := mrOK;
    VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

procedure TOfficeMoreColorsWin.FormResize(Sender: TObject);
begin
{$IFDEF mbXP_Lib}
  grip.Left := ClientWidth - 15;
  grip.Top := ClientHeight - 15;
{$ENDIF}
end;

procedure TOfficeMoreColorsWin.FormShow(Sender: TObject);
var
  h: Integer;
begin
  OKbtn.AutoSize := true;
  h := OKbtn.Height;
  OKbtn.AutoSize := false;
  OKbtn.Height := h;
  OKbtn.Width := Cancelbtn.Width;
  CancelBtn.Height := h;
end;

function TOfficeMoreColorsWin.GetHint(c: TColor): string;
begin
  Result := Format('RGB(%u, %u, %u)'#13'Hex: %s', [
    GetRValue(c), GetGValue(c), GetBValue(c), ColorToHex(c)
  ]);
end;

function TOfficeMoreColorsWin.GetShowHint: Boolean;
begin
  Result := inherited ShowHint;
end;

procedure TOfficeMoreColorsWin.HexaChange(Sender: TObject);
begin
  NewSwatch.Color := Hexa.SelectedColor;
end;

procedure TOfficeMoreColorsWin.HSLChange(Sender: TObject);
begin
  SetAllCustom(HSL.SelectedColor);
end;

procedure TOfficeMoreColorsWin.HSLRingChange(Sender: TObject);
begin
  SetAllCustom(HSLRing.SelectedColor);
end;

procedure TOfficeMoreColorsWin.NewSwatchColorChange(Sender: TObject);
var
  r,g,b: Integer;
  h,s,l: Integer;
begin
  NewSwatch.Hint := GetHint(NewSwatch.Color);
  if (ERed = nil) or (EBlue = nil) or (EGreen = nil) or
     (EHue = nil) or (ESat = nil) or (ELum = nil)
  then
    exit;

  SetAllCustom(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.OldSwatchColorChange(Sender: TObject);
begin
  OldSwatch.Hint := GetHint(OldSwatch.Color);
  SetAllToSel(OldSwatch.Color);
end;

procedure TOfficeMoreColorsWin.PagesChange(Sender: TObject);
begin
  SetAllToSel(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.SetAllCustom(c: TColor);
var
  r, g, b: Integer;
  H, S, L: Double;
//  h, s, l: Integer;
begin
  if (ERed = nil) or (EGreen = nil) or (EBlue = nil) or
     (EHue = nil) or (ESat = nil) or (ELum = nil) or
     (PickerNotebook = nil) or (HSL = nil) or (HSLRing = nil) or (SLH = nil)
  then
    exit;

  NewSwatch.Color := c;

  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  RGBToHSL(c, H, S, L);
//  RGBtoHSLRange(c, h, s, l);

  if PickerNotebook.ActivePage = nbHSL.Name then
    HSL.SelectedColor := c
  else
  if PickerNotebook.ActivePage = nbHSLRing.Name then
    HSLRing.SelectedColor := c
  else
  if PickerNotebook.ActivePage = nbSLH.Name then
    SLH.SelectedColor := c
  else
  if PickerNotebook.ActivePage = nbRGB.Name then
  begin
    RTrackbar.SelectedColor := c;
    GTrackbar.SelectedColor := c;
    BTrackbar.SelectedColor := c;
  end
  else
    exit; //raise Exception.Create('Notbook page not prepared for color pickers');

  ERed.Value := r;
  EGreen.Value := g;
  EBlue.Value := b;
  EHue.Value := H * HSL.MaxHue;
  ESat.Value := S * HSL.MaxSaturation;
  ELum.Value := L * HSL.MaxLuminance;
end;

procedure TOfficeMoreColorsWin.SetAllToSel(c: TColor);
var
  h, s, l: Integer;
begin
  case Pages.ActivePageIndex of
    // Standard Page
    0: Hexa.SelectedColor := c;
    // Custom Page
    1: SetAllCustom(c);
  end;
  NewSwatch.Color := c;
end;

procedure TOfficeMoreColorsWin.SetShowHint(AValue: Boolean);
begin
  inherited ShowHint := AValue;
  // Unfortunately Notebook does not have a Hint and ParentHint...
  HSL.ShowHint := AValue;
  HSLRing.ShowHint := AValue;
  SLH.ShowHint := AValue;
end;

procedure TOfficeMoreColorsWin.SLHChange(Sender: TObject);
begin
  SetAllCustom(SLH.SelectedColor);
end;

end.
