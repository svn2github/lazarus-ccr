unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, BColorPicker, GColorPicker, RColorPicker, CColorPicker,
  YColorPicker, MColorPicker, KColorPicker, HColorPicker, SColorPicker,
  LColorPicker, VColorPicker, mbColorPreview;

type

  { TForm1 }

  TForm1 = class(TForm)
    BColorPickerV: TBColorPicker;
    BColorPickerH: TBColorPicker;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    CColorPickerH: TCColorPicker;
    CColorPickerV: TCColorPicker;
    GColorPickerV: TGColorPicker;
    GColorPickerH: TGColorPicker;
    HColorPickerH: THColorPicker;
    HColorPickerV: THColorPicker;
    KColorPickerH: TKColorPicker;
    KColorPickerV: TKColorPicker;
    LblR: TLabel;
    lblLVv: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    lblLVh: TLabel;
    Label3: TLabel;
    LblC: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LblH: TLabel;
    Label9: TLabel;
    LColorPickerH: TLColorPicker;
    LColorPickerV: TLColorPicker;
    CMYKh: TmbColorPreview;
    HSLVh: TmbColorPreview;
    Panel1: TPanel;
    Panel2: TPanel;
    RGBv: TmbColorPreview;
    CMYKv: TmbColorPreview;
    HSLVv: TmbColorPreview;
    rbHSLv: TRadioButton;
    rbHSVv: TRadioButton;
    rbHSVh: TRadioButton;
    rbHSLh: TRadioButton;
    RGBh: TmbColorPreview;
    MColorPickerH: TMColorPicker;
    MColorPickerV: TMColorPicker;
    PageControl1: TPageControl;
    RColorPickerV: TRColorPicker;
    RColorPickerH: TRColorPicker;
    SColorPickerH: TSColorPicker;
    SColorPickerV: TSColorPicker;
    tabVertical: TTabSheet;
    tabHorizontal: TTabSheet;
    VColorPickerH: TVColorPicker;
    VColorPickerV: TVColorPicker;
    YColorPickerH: TYColorPicker;
    YColorPickerV: TYColorPicker;
    procedure CMYKPickerV_Change(Sender: TObject);
    procedure CMYKPickerH_Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HPickerH_Change(Sender: TObject);
    procedure HPickerV_Change(Sender: TObject);
    procedure rbHSLv_Change(Sender: TObject);
    procedure rbHSLh_Change(Sender: TObject);
    procedure RGBPickerH_Change(Sender: TObject);
    procedure RGBPickerV_Change(Sender: TObject);
    procedure SLVPickerH_Change(Sender: TObject);
    procedure SLVPickerV_Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, ScanLines, RGBCMYKUtils, RGBHSLUtils, RGBHSVUtils;

{ TForm1 }

procedure TForm1.CMYKPickerH_Change(Sender: TObject);
var
  c: TColor;
begin
  if (CColorPickerH = nil) or (YColorPickerH = nil) or (MColorPickerH = nil) or
     (KColorPickerH = nil) or (CMYKh = nil) then
    exit;
  CMYKh.Color := CMYKToColor(
    CColorPickerH.Cyan,
    MColorPickerH.Magenta,
    YColorPickerH.Yellow,
    KColorPickerH.Black
  );

  c := CMYKh.Color;
  CMYKh.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    GetHValue(c), GetSValue(c), GetLValue(c), GetVValue(c)
  ]);
end;

procedure TForm1.CMYKPickerV_Change(Sender: TObject);
var
  c: TColor;
begin
  if (CColorPickerV = nil) or (YColorPickerV = nil) or (MColorPickerV = nil) or
     (KColorPickerV = nil) or (CMYKv = nil) then
    exit;
  CMYKv.Color := CMYKToColor(
    CColorPickerV.Cyan,
    MColorPickerV.Magenta,
    YColorPickerV.Yellow,
    KColorPickerV.Black
  );

  c := CMYKv.Color;
  CMYKv.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    GetHValue(c), GetSValue(c), GetLValue(c), GetVValue(c)
  ]);
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  MaxHue := 359;
  MaxSat := 240;
  MaxLum := 240;

  VColorPickerH.Left := LColorPickerH.Left;
  VColorPickerH.Top := LColorPickerH.Top;
  VColorPickerH.Width := LColorPickerH.Width;
  VColorPickerH.Anchors := [akLeft, akTop, akRight];

  VColorPickerV.Left := LColorPickerV.Left;
  VColorPickerV.Top := LColorPickerV.Top;
  VColorPickerV.Height := LColorPickerV.Height;
  VColorPickerV.Anchors := [akLeft, akTop, akBottom];

  RGBPickerH_Change(nil);
  CMYKPickerH_Change(nil);
  SLVPickerH_Change(nil);

  RGBPickerV_Change(nil);
  CMYKPickerV_Change(nil);
  SLVPickerV_Change(nil);
end;

procedure TForm1.HPickerH_Change(Sender: TObject);
begin
  SLVPickerH_Change(nil);
  SColorPickerH.Hue := HColorPickerH.Hue;
  LColorPickerH.Hue := HColorPickerH.Hue;
  VColorPickerH.Hue := HColorPickerH.Hue;
end;

procedure TForm1.HPickerV_Change(Sender: TObject);
begin
  SLVPickerV_Change(nil);
  SColorPickerV.Hue := HColorPickerV.Hue;
  LColorPickerV.Hue := HColorPickerV.Hue;
  VColorPickerV.Hue := HColorPickerV.Hue;
end;

procedure TForm1.rbHSLv_Change(Sender: TObject);
begin
  if rbHSLv.Checked then
  begin
    lblLVv.Caption := 'L';
    VColorPickerV.Visible := false;
    LColorPickerV.Visible := true;
  end;
  if rbHSVv.Checked then
  begin
    lblLVv.Caption := 'V';
    LColorPickerV.Visible := false;
    VColorPickerV.Visible := true;
  end;
  HPickerV_Change(nil);
end;

procedure TForm1.rbHSLh_Change(Sender: TObject);
begin
  if rbHSLh.Checked then
  begin
    lblLVh.Caption := 'L';
    VColorPickerH.Visible := false;
    LColorPickerH.Visible := true;
  end;
  if rbHSVh.Checked then
  begin
    lblLVh.Caption := 'V';
    lColorPickerH.Visible := false;
    VColorPickerH.Visible := true;
  end;
  HPickerH_Change(nil);
end;

procedure TForm1.RGBPickerH_Change(Sender: TObject);
var
  c: TColor;
begin
  if (RColorPickerH = nil) or (GColorPickerH = nil) or (BColorPickerH = nil) or (RGBh = nil) then
    exit;
  RGBh.Color := RGBToColor(
    RColorPickerH.Red,
    GColorPickerH.Green,
    BColorPickerH.Blue
  );

  c := RGBh.Color;
  RGBh.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    GetHValue(c), GetSValue(c), GetLValue(c), GetVValue(c)
  ]);
end;

procedure TForm1.RGBPickerV_Change(Sender: TObject);
var
  c: TColor;
begin
  if (RColorPickerV = nil) or (GColorPickerV = nil) or (BColorPickerV = nil) or (RGBv = nil) then
    exit;
  RGBv.Color := RGBToColor(
    RColorPickerV.Red,
    GColorPickerV.Green,
    BColorPickerV.Blue
  );

  c := RGBv.Color;
  RGBv.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    GetHValue(c), GetSValue(c), GetLValue(c), GetVValue(c)
  ]);
end;

procedure TForm1.SLVPickerH_Change(Sender: TObject);
var
  triple: TRGBTriple;
  c: TColor;
begin
  if (HSLVh = nil) or (HColorPickerH = nil) or (SColorPickerH = nil) then
    exit;
  if rbHSLh.Checked then begin
    if (LColorPickerH = nil) then
      exit;
    HSLVh.Color := HSLRangeToRGB(HColorPickerH.Hue, SColorPickerH.Saturation, LColorPickerH.Luminance);
  end;
  if rbHSVh.Checked then begin
    if (VColorPickerH = nil) then
      exit;
    HSLVh.Color := HSVRangetoColor(HColorPickerH.Hue, SColorPickerH.Saturation, VColorPickerH.Value);
  end;

  c := HSLVh.Color;
  HSLVh.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    GetHValue(c), GetSValue(c), GetLValue(c), GetVValue(c)
  ]);
end;

procedure TForm1.SLVPickerV_Change(Sender: TObject);
var
  triple: TRGBTriple;
  c: TColor;
begin
  if (HSLVv = nil) or (HColorPickerV = nil) or (SColorPickerV = nil) then
    exit;
  if rbHSLv.Checked then begin
    if (LColorPickerV = nil) then
      exit;
    triple := HSLToRGBTriple(HColorPickerV.Hue, SColorPickerV.Saturation, LColorPickerV.Luminance);
    HSLVv.Color := RGBTripleToColor(triple);
  end;
  if rbHSVv.Checked then begin
    if (VColorPickerV = nil) then
      exit;
    HSLVv.Color := HSVtoColor(
      HColorPickerV.Hue/HColorPickerV.MaxHue,
      SColorPickerV.Saturation/SColorPickerV.MaxSaturation,
      VColorPickerV.Value/VColorPickerV.MaxValue
    );
  end;

  c := HSLVv.Color;
  HSLVv.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    GetHValue(c), GetSValue(c), GetLValue(c), GetVValue(c)
  ]);
end;

end.

