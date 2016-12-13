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
  HTMLColors;

type
  TOfficeMoreColorsWin = class(TForm)
    Pages: TPageControl;
    Standard: TTabSheet;
    Custom: TTabSheet;
    Hexa: THexaColorPicker;
    HSL: THSLColorPicker;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ColorModel: TComboBox;
    LRedOrHue: TLabel;
    LGreenOrSat: TLabel;
    LBlueOrLum: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OKbtn: TButton;
    Cancelbtn: TButton;
    NewSwatch: TmbColorPreview;
    OldSwatch: TmbColorPreview;
    procedure ColorModelChange(Sender: TObject);
    procedure HSLChange(Sender: TObject);
    procedure ERedOrHueChange(Sender: TObject);
    procedure EGreenOrSatChange(Sender: TObject);
    procedure EBlueOrLumChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    function GetHint(c: TColor): string;
    procedure HexaChange(Sender: TObject);
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure SetAllToSel(c: TColor);
  private
    {$IFDEF mbXP_Lib}
    ERedOrHue, EGreenOrSat, EBlueOrLum: TmbXPSpinEdit;
    grip: TmbXPSizeGrip;
    {$ELSE}
    ERedOrHue, EGreenOrSat, EBlueOrLum: TSpinEdit;
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
var
  h, s, l: Integer;
begin
 case ColorModel.ItemIndex of
  0:
   begin
    LRedOrHue.Caption := '&Red:';
    LGreenOrSat.Caption := '&Green:';
    LBlueOrLum.Caption := '&Blue:';
    ERedOrHue.MaxValue := 255;
    EGreenOrSat.MaxValue := 255;
    EBlueOrLum.MaxValue := 255;
    ERedOrHue.Value := GetRValue(NewSwatch.Color);
    EGreenOrSat.Value := GetGValue(NewSwatch.Color);
    EBlueOrLum.Value := GetBValue(NewSwatch.Color);
   end;
  1:
   begin
    LRedOrHue.Caption := 'H&ue:';
    LGreenOrSat.Caption := '&Sat:';
    LBlueOrLum.Caption := '&Lum:';
    ERedOrHue.MaxValue := MaxHue; //238;
    EGreenOrSat.MaxValue := MaxSat; //240;
    EBlueOrLum.MaxValue := MaxLum; //240;
    RGBtoHSLRange(NewSwatch.Color, h, s, l);
    ERedOrHue.Value := h;
    EGreenOrSat.Value := s;
    EBlueOrLum.Value := l;
   end;
 end;
end;

procedure TOfficeMoreColorsWin.HSLChange(Sender: TObject);
begin
 if HSL.Manual then
  case ColorModel.ItemIndex of
   0:
    begin
      ERedOrHue.Value := HSL.RValue;
      EGreenOrSat.Value := HSL.GValue;
      EBlueOrLum.Value := HSL.BValue;
      NewSwatch.Color := HSL.SelectedColor;
    end;
   1:
    begin
      ERedOrHue.Value := HSL.HValue;
      EGreenOrSat.Value := HSL.SValue;
      EBlueOrLum.Value := HSL.LValue;
      NewSwatch.Color := HSL.SelectedColor;
    end;
  end;
end;

procedure TOfficeMoreColorsWin.ERedOrHueChange(Sender: TObject);
begin
  inc(FLockChange);
  if (ERedOrHue.Text <> '') and
     (ERedOrHue.Focused {$IFDEF DELPHI} or ERedOrHue.Button.Focused{$ENDIF})
  then
    case ColorModel.ItemIndex of
      0: begin
           HSL.RValue := ERedOrHue.Value;
           NewSwatch.Color := RGB(ERedOrHue.Value, EGreenOrSat.Value, EBlueOrLum.Value);
         end;
      1: begin
           HSL.HValue := ERedOrHue.Value;
           NewSwatch.Color := HSLRangeToRGB(ERedOrHue.Value, EGreenOrSat.Value, EBlueOrLum.Value);
         end;
    end;
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.EGreenOrSatChange(Sender: TObject);
begin
  inc(FLockChange);
  NewSwatch.OnColorChange := nil;
  if (EGreenOrSat.Text <> '') and
     (EGreenOrSat.Focused {$IFDEF DELPHI}or EGreen.ButtonOrSat.Focused{$ENDIF})
  then
    case ColorModel.ItemIndex of
      0: begin
           HSL.GValue := EGreenOrSat.Value;
           NewSwatch.Color := RGB(ERedOrHue.Value, EGreenOrSat.Value, EBlueOrLum.Value);
         end;
      1: begin
           HSL.SValue := EGreenOrSat.Value;
           NewSwatch.Color := HSLRangeToRGB(ERedOrHue.Value, EGreenOrSat.Value, EBlueOrLum.Value);
         end;
    end;
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.EBlueOrLumChange(Sender: TObject);
begin
  inc(FLockChange);
  if (EBlueOrLum.Text <> '') and
     (EBlueOrLum.Focused {$IFDEF DELPHI} or EBlueOrLum.Button.Focused{$ENDIF})
  then
    case ColorModel.ItemIndex of
      0: begin
           HSL.BValue := EBlueOrLum.Value;
           NewSwatch.Color := RGB(ERedOrHue.Value, EGreenOrSat.Value, EBlueOrLum.Value);
         end;
      1: begin
           HSL.LValue := EBlueOrLum.Value;
           NewSwatch.Color := HSLRangeToRGB(ERedOrHue.Value, EGreenOrSat.Value, EBlueOrLum.Value);
         end;
    end;
  dec(FLockChange);
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
  if (ERedOrHue = nil) or (EBlueOrLum = nil) or (EGreenOrSat = nil) or
     (FLockChange <> 0)
  then
    exit;

  if ColorModel.ItemIndex = 0 then  // RGB
  begin
    ERedOrHue.Value := GetRValue(NewSwatch.Color);
    EGreenOrSat.Value := GetGValue(NewSwatch.Color);
    EBlueOrLum.Value := GetBValue(NewSwatch.Color);
  end else
  begin
    ERedOrHue.Value := GetHValue(NewSwatch.Color);
    EGreenOrSat.Value := GetSValue(NewSwatch.Color);
    EBlueOrLum.Value := GetLValue(NewSwatch.Color);
  end;
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
  1:
   begin
    HSL.SelectedColor := c;
    case ColorModel.ItemIndex of
     0:
      begin
       ERedOrHue.Value := GetRValue(c);
       EGreenOrSat.Value := GetGValue(c);
       EBlueOrLum.Value := GetBValue(c);
      end;
     1:
      begin
       RGBtoHSLRange(c, h, s, l);
       ERedOrHue.Value := h;
       EGreenOrSat.Value := s;
       EBlueOrLum.Value := l;
      end;
    end;
   end;
 end;
 NewSwatch.Color := c;
end;

procedure TOfficeMoreColorsWin.PagesChange(Sender: TObject);
begin
  SetAllToSel(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.FormResize(Sender: TObject);
begin
{$IFDEF mbXP_Lib}
  grip.Left := ClientWidth - 15;
  grip.Top := ClientHeight - 15;
{$ENDIF}
end;

procedure TOfficeMoreColorsWin.FormCreate(Sender: TObject);
begin
 {$IFDEF mbXP_Lib}
  ERedOrHue := TmbXPSpinEdit.CreateParented(Custom.Handle);
  EGreenOrSat := TmbXPSpinEdit.CreateParented(Custom.Handle);
  EBlueOrLum := TmbXPSpinEdit.CreateParented(Custom.Handle);
  grip := TmbXPSizeGrip.CreateParented(Self.Handle);
 {$ELSE}
  ERedOrHue := TSpinEdit.CreateParented(Custom.Handle);
  EGreenOrSat := TSpinEdit.CreateParented(Custom.Handle);
  EBlueOrLum := TSpinEdit.CreateParented(Custom.Handle);
 {$ENDIF}
  with ERedOrHue do
  begin
   Name := 'ERed';
   Width := 47;
   Height := 22;
   Left := 74;
   Top := LRedOrHue.Top - 4; //198;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := @ERedOrHueChange;
  end;
  with EGreenOrSat do
  begin
   Name := 'EGreen';
   Width := 47;
   Height := 22;
   Left := 74;
   Top := LGreenOrSat.Top - 3; //224;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := @EGreenOrSatChange;
  end;
  with EBlueOrLum do
  begin
   Name := 'EBlue';
   Width := 47;
   Height := 22;
   Left := 74;
   Top := LBlueOrLum.Top - 4; //251;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := @EBlueOrLumChange;
  end;
  Custom.InsertControl(ERedOrHue);
  Custom.InsertControl(EGreenOrSat);
  Custom.InsertControl(EBlueOrLum);
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
