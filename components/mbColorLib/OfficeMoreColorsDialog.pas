unit OfficeMoreColorsDialog;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls,
  HexaColorPicker, HSLColorPicker, mbColorConv, mbColorPreview,
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
    LLumVal: TLabel;
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
    procedure ColorPickerChange(Sender: TObject);
    procedure EBlueChange(Sender: TObject);
    procedure EGreenChange(Sender: TObject);
    procedure EHueChange(Sender: TObject);
    procedure ELumValChange(Sender: TObject);
    procedure ERedChange(Sender: TObject);
    procedure ESatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetHint(c: TColor): string;
    procedure HexaChange(Sender: TObject);
    procedure HSLChange(Sender: TObject);
    procedure HSLRingChange(Sender: TObject);
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure PagesChanging(Sender: TObject; var {%H-}AllowChange: Boolean);
    procedure SLHChange(Sender: TObject);
  private
    {$IFDEF mbXP_Lib}
    ERed, EGreen, EBlue: TmbXPSpinEdit;
    EHue, ESat, ELumVal: TmbXPSpinEdit;
    grip: TmbXPSizeGrip;
    {$ELSE}
    ERed, EGreen, EBlue: TSpinEdit;
    EHue, ESat, ELumVal: TSpinEdit;
    {$ENDIF}
    FMaxHue: Integer;
    FMaxSat: Integer;
    FMaxLum: Integer;
    FMaxVal: Integer;
    FSelectedColor: TColor;
    FBrightnessMode: TBrightnessMode;
    FLockChange: Integer;
    function GetPickerIndex: Integer;
    function GetSelectedColor: TColor;
    function GetShowHint: Boolean;
    procedure SetAllCustom(c: TColor);
    procedure SetAllToSel(c: TColor);
    procedure SetBrightnessMode(AMode: TBrightnessMode);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetMaxVal(V: Integer);
    procedure SetPickerIndex(AValue: Integer);
    procedure SetSelectedColor(c: TColor);
    procedure SetShowHint(AValue: boolean);
  protected
    procedure BeginUpdate;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure EndUpdate;
  public
    property PickerIndex: Integer read GetPickerIndex write SetPickerIndex;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property MaxHue: Integer read FMaxHue write SetMaxHue;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum;
    property MaxValue: Integer read FMaxVal write SetMaxVal;
  published
    property ShowHint: Boolean read GetShowHint write SetShowHint;
  end;

var
  OfficeMoreColorsWin: TOfficeMoreColorsWin;

implementation

{$R *.lfm}

procedure TOfficeMoreColorsWin.BeginUpdate;
begin
  inc(FLockChange);
end;

procedure TOfficeMoreColorsWin.ColorPickerChange(Sender: TObject);
begin
  if FLockChange > 0 then
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
      case FBrightnessMode of
        bmLuminance:
          NewSwatch.Color := HSLToColor(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELumVal.Value/FMaxLum);
        bmValue:
          NewSwatch.Color := HSVtoColor(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELumVal.Value/FMaxVal);
      end;
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.ELumValChange(Sender: TObject);
begin
  if (ELumVal.Text <> '') and ELumVal.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Luminance := ELumVal.Value;
      case FBrightnessMode of
        bmLuminance:
          NewSwatch.Color := HSLToColor(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELumVal.Value/FMaxLum);
        bmValue:
          NewSwatch.Color := HSVtoColor(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELumVal.Value/FMaxVal);
      end;
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EndUpdate;
begin
  dec(FLockChange);
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
      case FBrightnessMode of
        bmLuminance:
          NewSwatch.Color := HSLToColor(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELumval.Value/FMaxLum);
        bmValue:
          NewSwatch.Color := HSVtoColor(EHue.Value/FMaxHue, ESat.Value/FMaxSat, ELumval.Value/FMaxVal);
      end;
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.FormCreate(Sender: TObject);
begin
  FBrightnessMode := bmLuminance;

  FMaxHue := 360;
  FMaxSat := 255;
  FMaxLum := 255;
  FMaxVal := 255;

  HSL.MaxHue := FMaxHue;
  HSL.MaxSaturation := FMaxSat;
  HSL.MaxLuminance := FMaxLum;
  HSL.BrightnessMode := FBrightnessMode;

  HSLRing.MaxHue := FMaxHue;
  HSLRing.MaxSaturation := FMaxSat;
  HSLRing.MaxLuminance := FMaxLum;
  HSLRing.BrightnessMode := FBrightnessMode;

  SLH.MaxHue := FMaxHue;
  SLH.MaxSaturation := FMaxSat;
  SLH.MaxLuminance := FMaxLum;
  SLH.BrightnessMode := FBrightnessMode;

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
  ELumVal := TSpinEdit.CreateParented(Custom.Handle);
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
  with ELumVal do
  begin
    Name := 'ELumVal';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left + cbColorDisplay.Width - Width;
    Top := EBlue.Top;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := FMaxLum;
    MinValue := 0;
    Value := 0;
    OnChange := @ELumValChange;
//   TabOrder := ESat.TabOrder + 1;
  end;
  Custom.InsertControl(ERed);
  Custom.InsertControl(EGreen);
  Custom.InsertControl(EBlue);
  Custom.InsertControl(EHue);
  Custom.InsertControl(ESat);
  Custom.InsertControl(ELumVal);

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

  OKBtn.TabOrder := ELumVal.TabOrder + 1;
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

function TOfficeMoreColorsWin.GetPickerIndex: Integer;
begin
  Result := PickerNotebook.PageIndex + 1;
  if Pages.PageIndex = 0 then
    Result := -Result;
end;

procedure TOfficeMoreColorsWin.SetPickerIndex(AValue: Integer);
begin
  if AValue = 0 then begin
    Pages.PageIndex := 0;
    PickerNotebook.PageIndex := 0;
  end else
  begin
    PickerNotebook.PageIndex := abs(AValue) - 1;
    if AValue > 0 then
      Pages.PageIndex := 1 else
      Pages.PageIndex := 0;
  end;
end;

function TOfficeMoreColorsWin.GetSelectedColor: TColor;
begin
  Result := NewSwatch.Color;
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
begin
  NewSwatch.Hint := GetHint(NewSwatch.Color);

  exit;


  if (ERed = nil) or (EBlue = nil) or (EGreen = nil) or
     (EHue = nil) or (ESat = nil) or (ELumVal = nil)
  then
    exit;

  SetAllCustom(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.OldSwatchColorChange(Sender: TObject);
begin
  OldSwatch.Hint := GetHint(OldSwatch.Color);



  //SetAllToSel(OldSwatch.Color);
end;

procedure TOfficeMoreColorsWin.PagesChange(Sender: TObject);
begin
  SetAllToSel(FSelectedColor); //NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.PagesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  FSelectedColor := NewSwatch.Color;
  {
  case Pages.PageIndex of
    0: FSelectedColor := Hexa.SelectedColor;
    1: case PickerNotebook.PageIndex of
         0: FSelectedColor := HSL.SelectedColor;
         1: FSelectedColor := HSLRing.SelectedColor;
         2: FSelectedColor := SLH.SelectedColor;
         3: FSelectedColor := RgbToColor(RTrackbar.Red, GTrackbar.Green, BTrackbar.Blue);
       end;
  end;
  }
end;

procedure TOfficeMoreColorsWin.SetAllCustom(c: TColor);
var
  r, g, b: Integer;
  H, S, L, V: Double;
begin
  if (ERed = nil) or (EGreen = nil) or (EBlue = nil) or
     (EHue = nil) or (ESat = nil) or (ELumVal = nil) or
     (PickerNotebook = nil) or (HSL = nil) or (HSLRing = nil) or (SLH = nil)
     or (FLockChange > 0)
  then
    exit;

  BeginUpdate;

  NewSwatch.Color := c;

  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  case FBrightnessMode of
    bmLuminance : ColorToHSL(c, H, S, L);
    bmValue     : ColortoHSV(c, H, S, V);
  end;

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
  case FBrightnessMode of
    bmLuminance: ELumVal.Value := L * HSL.MaxLuminance;
    bmValue    : ELumVal.Value := V * HSL.MaxValue;
  end;

  EndUpdate;
end;

procedure TOfficeMoreColorsWin.SetAllToSel(c: TColor);
begin
  //inc(FLockChange);
  case Pages.ActivePageIndex of
    // Standard Page
    0: Hexa.SelectedColor := c;
    // Custom Page
    1: SetAllCustom(c);
  end;
  NewSwatch.Color := c;
  //dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetBrightnessMode(AMode: TBrightnessMode);
begin
  FBrightnessMode := AMode;
  case AMode of
    bmLuminance: LLumVal.Caption := 'Lum:';
    bmValue    : LLumval.Caption := 'Val:';
  end;
end;

procedure TOfficeMoreColorsWin.SetMaxHue(H: Integer);
var
  hh: Double;
begin
  inc(FLockChange);
  hh := EHue.Value / FMaxHue;
  FMaxHue := H;
  EHue.MaxValue := H;
  EHue.Value := round(hh * FMaxHue);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetMaxLum(L: Integer);
var
  ll: Double;
begin
  inc(FLockChange);
  ll := ELumVal.Value / FMaxLum;
  FMaxLum := L;
  ELumVal.MaxValue := L;
  ELumVal.Value := round(ll * FMaxLum);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetMaxSat(S: Integer);
var
  ss: Double;
begin
  inc(FLockChange);
  ss := ESat.Value / FMaxSat;
  FMaxSat := S;
  ESat.MaxValue := S;
  ESat.Value := round(ss * FMaxSat);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetMaxVal(V: Integer);
var
  vv: Double;
begin
  inc(FLockChange);
  vv := ELumVal.Value / FMaxVal;
  FMaxVal := V;
  ELumVal.MaxValue := V;
  ELumVal.Value := round(vv * FMaxVal);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetSelectedColor(c: TColor);
begin
  FSelectedColor := c;
  OldSwatch.Color := c;
  SetAllToSel(FSelectedColor);
end;

procedure TOfficeMoreColorsWin.SetShowHint(AValue: Boolean);
begin
  inherited ShowHint := AValue;
  // Unfortunately Notebook does not have a Hint and ParentHint...
  HSL.ShowHint := AValue;
  HSLRing.ShowHint := AValue;
  SLH.ShowHint := AValue;
  RTrackbar.ShowHint := AValue;
  GTrackbar.ShowHint := AValue;
  BTrackbar.ShowHint := AValue;
end;

procedure TOfficeMoreColorsWin.SLHChange(Sender: TObject);
begin
  SetAllCustom(SLH.SelectedColor);
end;

end.
