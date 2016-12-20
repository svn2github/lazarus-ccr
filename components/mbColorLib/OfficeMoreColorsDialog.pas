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
    FLockChange: Integer;
    function GetShowHint: Boolean;
    procedure SetAllCustom(c: TColor);
    procedure SetAllToSel(c: TColor);
    procedure SetShowHint(AValue: boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
//    procedure CreateWnd; override;
  published
    property ShowHint: Boolean read GetShowHint write SetShowHint;
  end;

var
  OfficeMoreColorsWin: TOfficeMoreColorsWin;

implementation

{$R *.lfm}

procedure TOfficeMoreColorsWin.ColorPickerChange(Sender: TObject);
begin
  if FLockChange <> 0 then
    exit;
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


  exit;

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
  if (EBlue.Text <> '') and EBlue.Focused then
  begin
    inc(FLockChange);
    HSL.Blue := EBlue.Value;
    SLH.Blue := EBlue.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.EGreenChange(Sender: TObject);
begin
  if (EGreen.Text <> '') and EGreen.Focused then
  begin
    inc(FLockChange);
    HSL.Green := EGreen.Value;
    SLH.Green := EGreen.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.EHueChange(Sender: TObject);
begin
  if (EHue.Text <> '') and EHue.Focused  then
  begin
    inc(FLockChange);
    HSL.Hue := EHue.Value;
    SLH.Hue := EHue.Value;
    NewSwatch.Color := HSLRangeToRGB(EHue.Value, ESat.Value, ELum.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.ELumChange(Sender: TObject);
begin
  if (ELum.Text <> '') and ELum.Focused then
  begin
    inc(FLockChange);
    HSL.Luminance := ELum.Value;
    NewSwatch.Color := HSLRangeToRGB(EHue.Value, ESat.Value, ELum.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.ERedChange(Sender: TObject);
begin
  if (ERed.Text <> '') and ERed.Focused then
  begin
    inc(FLockChange);
    HSL.Red := ERed.Value;
    SLH.Red := ERed.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.ESatChange(Sender: TObject);
begin
  if (ESat.Text <> '') and ESat.Focused then
  begin
    inc(FLockChange);
    HSL.Saturation := ESat.Value;
    SLH.Saturation := ESat.Value;
    NewSwatch.Color := HSLRangeToRGB(EHue.Value, ESat.Value, ELum.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.FormCreate(Sender: TObject);
begin
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
    MaxValue := MaxHue;
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
    MaxValue := MaxSat;
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
    MaxValue := MaxLum;
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
  if FLockChange <> 0 then
    exit;
  SetAllCustom(HSL.SelectedColor);
end;

procedure TOfficeMoreColorsWin.HSLRingChange(Sender: TObject);
begin
  if FLockChange <> 0 then
    exit;
  SetAllCustom(HSLRing.SelectedColor);
end;

procedure TOfficeMoreColorsWin.NewSwatchColorChange(Sender: TObject);
var
  r,g,b: Integer;
  h,s,l: Integer;
begin
  NewSwatch.Hint := GetHint(NewSwatch.Color);
  if (ERed = nil) or (EBlue = nil) or (EGreen = nil) or
     (EHue = nil) or (ESat = nil) or (ELum = nil) or (FLockChange <> 0)
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
  h, s, l: Integer;
begin
  if (ERed = nil) or (EGreen = nil) or (EBlue = nil) or
     (EHue = nil) or (ESat = nil) or (ELum = nil) or
     (PickerNotebook = nil) or (HSL = nil) or (HSLRing = nil) or (SLH = nil)
  then
    exit;

  inc(FLockChange);
  try
    NewSwatch.Color := c;
    r := GetRValue(c);
    g := GetGValue(c);
    b := GetBValue(c);
    RGBtoHSLRange(c, h, s, l);

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
    EHue.Value := h;
    ESat.Value := s;
    ELum.Value := l;
  finally
    dec(FLockChange);
  end;
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
  if FLockChange <> 0 then
    exit;
  SetAllCustom(SLH.SelectedColor);
end;

end.
