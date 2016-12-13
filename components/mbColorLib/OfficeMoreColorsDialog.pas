unit OfficeMoreColorsDialog;

interface

{$I mxs.inc}

uses
 {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, {$IFDEF DELPHI_6_UP}Variants,{$ENDIF} Classes, Graphics, Controls,
  Forms, StdCtrls, ExtCtrls, ComCtrls,
  HexaColorPicker, HSLColorPicker, RGBHSLUtils,
  mbColorPreview, {$IFDEF mbXP_Lib}mbXPSpinEdit, mbXPSizeGrip,{$ELSE} Spin,{$ENDIF}
  HTMLColors, SLHColorPicker;

type

  { TOfficeMoreColorsWin }

  TOfficeMoreColorsWin = class(TForm)
    LLum: TLabel;
    LSat: TLabel;
    LHue: TLabel;
    Pages: TPageControl;
    SLH: TSLHColorPicker;
    Standard: TTabSheet;
    Custom: TTabSheet;
    Hexa: THexaColorPicker;
    HSL: THSLColorPicker;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ColorModel: TComboBox;
    LRed: TLabel;
    LGreen: TLabel;
    LBlue: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OKbtn: TButton;
    Cancelbtn: TButton;
    NewSwatch: TmbColorPreview;
    OldSwatch: TmbColorPreview;
    procedure ColorModelChange(Sender: TObject);
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
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure SetAllToSel(c: TColor);
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
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  end;

var
  OfficeMoreColorsWin: TOfficeMoreColorsWin;

implementation

{$IFDEF DELPHI}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TOfficeMoreColorsWin.CreateParams(var Params: TCreateParams);
begin 
  inherited CreateParams(Params); 
  Params.Style := WS_CAPTION or WS_SIZEBOX or WS_SYSMENU; 
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE; 
end; 

procedure TOfficeMoreColorsWin.CreateWnd;
begin 
  inherited CreateWnd;
  { wp : LM_SETICON not used in LCL }
  // SendMessage(Self.Handle, {$IFDEF FPC}LM_SETICON{$ELSE}WM_SETICON{$ENDIF}, 1, 0);
end; 

procedure TOfficeMoreColorsWin.ColorModelChange(Sender: TObject);
begin
  HSL.Visible := ColorModel.ItemIndex = 0;
  SLH.Visible := ColorModel.ItemIndex = 1;
  HSL.SelectedColor := NewSwatch.Color;
  SLH.SelectedColor := NewSwatch.Color;
end;

procedure TOfficeMoreColorsWin.HSLChange(Sender: TObject);
begin
  NewSwatch.Color := HSL.SelectedColor;
end;

procedure TOfficeMoreColorsWin.ERedChange(Sender: TObject);
begin
  if (ERed.Text <> '') and
     (ERed.Focused {$IFDEF DELPHI} or ERed.Button.Focused{$ENDIF}) then
  begin
    inc(FLockChange);
    HSL.RValue := ERed.Value;
    SLH.RValue := ERed.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.EGreenChange(Sender: TObject);
begin
  if (EGreen.Text <> '') and
     (EGreen.Focused {$IFDEF DELPHI}or EGreen.Button.Focused{$ENDIF}) then
  begin
    inc(FLockChange);
    HSL.GValue := EGreen.Value;
    SLH.GValue := EGreen.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.EBlueChange(Sender: TObject);
begin
  if (EBlue.Text <> '') and
     (EBlue.Focused {$IFDEF DELPHI} or EBlue.Button.Focused{$ENDIF}) then
  begin
    inc(FLockChange);
    HSL.BValue := EBlue.Value;
    SLH.BValue := EBlue.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.EHueChange(Sender: TObject);
begin
  if (EHue.Text <> '') and
     (EHue.Focused {$IFDEF DELPHI} or EHue.Button.Focused{$ENDIF}) then
  begin
    inc(FLockChange);
    HSL.HValue := EHue.Value;
    SLH.HValue := EHue.Value;
    NewSwatch.Color := HSLRangeToRGB(EHue.Value, ESat.Value, ELum.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.ESatChange(Sender: TObject);
begin
  if (ESat.Text <> '') and
     (ESat.Focused {$IFDEF DELPHI}or ESat.Button.Focused{$ENDIF}) then
  begin
    inc(FLockChange);
    HSL.SValue := ESat.Value;
    SLH.SValue := ESat.Value;
    NewSwatch.Color := HSLRangeToRGB(EHue.Value, ESat.Value, ELum.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.ELumChange(Sender: TObject);
begin
  if (ELum.Text <> '') and
     (ELum.Focused {$IFDEF DELPHI} or ELum.Button.Focused{$ENDIF}) then
  begin
    inc(FLockChange);
    HSL.LValue := ELum.Value;
    NewSwatch.Color := HSLRangeToRGB(EHue.Value, ESat.Value, ELum.Value);
    dec(FLockChange);
  end;
end;

procedure TOfficeMoreColorsWin.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
   VK_RETURN: ModalResult := mrOK;
   VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

procedure TOfficeMoreColorsWin.HexaChange(Sender: TObject);
begin
  NewSwatch.Color := Hexa.SelectedColor;
end;

function TOfficeMoreColorsWin.GetHint(c: TColor): string;
begin
  Result := Format('RGB(%u, %u, %u)'#13'Hex: %s', [
    GetRValue(c), GetGValue(c), GetBValue(c), ColorToHex(c)
  ]);
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

  ERed.Value := GetRValue(NewSwatch.Color);
  EGreen.Value := GetGValue(NewSwatch.Color);
  EBlue.Value := GetBValue(NewSwatch.Color);
  EHue.Value := GetHValue(NewSwatch.Color);
  ESat.Value := GetSValue(NewSwatch.Color);
  ELum.Value := GetLValue(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.OldSwatchColorChange(Sender: TObject);
begin
  OldSwatch.Hint := GetHint(OldSwatch.Color);
  SetAllToSel(OldSwatch.Color);
end;

procedure TOfficeMoreColorsWin.SetAllToSel(c: TColor);
var
  h, s, l: Integer;
begin
  case Pages.ActivePageIndex of
    // Standard Page
    0: Hexa.SelectedColor := c;
    // Custom Page
    1: begin
         HSL.SelectedColor := c;
         SLH.SelectedColor := c;
         ERed.Value := GetRValue(c);
         EGreen.Value := GetGValue(c);
         EBlue.Value := GetBValue(c);
         RGBtoHSLRange(c, h, s, l);
         EHue.Value := h;
         ESat.Value := s;
         ELum.Value := l;
       end;
  end;
  NewSwatch.Color := c;
end;

procedure TOfficeMoreColorsWin.SLHChange(Sender: TObject);
begin
  NewSwatch.Color := SLH.SelectedColor;
end;

procedure TOfficeMoreColorsWin.PagesChange(Sender: TObject);
begin
  SetAllToSel(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.FormResize(Sender: TObject);
begin
  SLH.Width := SLH.Parent.ClientWidth - SLH.Left;
  SLH.Height := ColorModel.Top - SLH.Top;
{$IFDEF mbXP_Lib}
  grip.Left := ClientWidth - 15;
  grip.Top := ClientHeight - 15;
{$ENDIF}
end;

procedure TOfficeMoreColorsWin.FormCreate(Sender: TObject);
begin
  SLH.Width := HSL.Width;
  SLH.Height := HSL.Height;
  SLH.Top := HSL.Top;
  SLH.Left := HSL.Left;
  SLH.Hide;
//  SLH.Anchors := [akLeft, akTop, akRight, akBottom];

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
  ESat := TSpinEdit.createParented(Custom.Handle);
  ELum := TSpinEdit.CreateParented(Custom.Handle);
 {$ENDIF}
  with ERed do
  begin
   Name := 'ERed';
   Width := 47;
   Height := 22;
   Left := ColorModel.Left;
   Top := LRed.Top - 4; //198;
   Alignment := taRightJustify;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := @ERedChange;
  end;
  with EGreen do
  begin
   Name := 'EGreen';
   Width := 47;
   Height := 22;
   Left := ColorModel.Left;
   Top := LGreen.Top - 3; //224;
   Alignment := taRightJustify;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := @EGreenChange;
  end;
  with EBlue do
  begin
   Name := 'EBlue';
   Width := 47;
   Height := 22;
   Left := ColorModel.Left;
   Top := LBlue.Top - 4; //251;
   Alignment := taRightJustify;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := @EBlueChange;
  end;
  with EHue do
  begin
   Name := 'EHue';
   Width := 47;
   Height := 22;
   Left := ColorModel.Left + ColorModel.Width - Width;
   Top := ERed.Top;
   Alignment := taRightJustify;
   Anchors := [akLeft, akBottom];
   MaxValue := MaxHue;
   MinValue := 0;
   Value := 0;
   OnChange := @EHueChange;
  end;
  with ESat do
  begin
   Name := 'ESat';
   Width := 47;
   Height := 22;
   Left := ColorModel.Left + ColorModel.Width - Width;
   Top := EGreen.Top;
   Alignment := taRightJustify;
   Anchors := [akLeft, akBottom];
   MaxValue := MaxSat;
   MinValue := 0;
   Value := 0;
   OnChange := @ESatChange;
  end;
  with ELum do
  begin
   Name := 'ELum';
   Width := 47;
   Height := 22;
   Left := ColorModel.Left + ColorModel.Width - Width;
   Top := EBlue.Top;
   Alignment := taRightJustify;
   Anchors := [akLeft, akBottom];
   MaxValue := MaxLum;
   MinValue := 0;
   Value := 0;
   OnChange := @ELumChange;
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
end;

end.
