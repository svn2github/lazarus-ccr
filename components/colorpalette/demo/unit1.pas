unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Menus, ColorPalette;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    BtnDeleteColor: TButton;
    BtnLoadDefaultPal1: TButton;
    BtnLoadRndPalette: TButton;
    BtnCreateRndPalette: TButton;
    BtnAddColor: TButton;
    BtnLoadDefaultPal: TButton;
    BtnEditColor: TButton;
    CbShowSelection: TCheckBox;
    ColorDialog: TColorDialog;
    ColorPalette: TColorPalette;
    CbPickMode: TComboBox;
    LblPickMode: TLabel;
    EdColCount: TSpinEdit;
    Label2: TLabel;
    LblColorInfo: TLabel;
    MnuEditPickedColor: TMenuItem;
    MnuDeletePickedColor: TMenuItem;
    PalettePopupMenu: TPopupMenu;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    ColorSample: TShape;
    procedure BtnAddColorClick(Sender: TObject);
    procedure BtnCreateRndPaletteClick(Sender: TObject);
    procedure BtnDeleteColorClick(Sender: TObject);
    procedure BtnEditColorClick(Sender: TObject);
    procedure BtnLoadDefaultPalClick(Sender: TObject);
    procedure BtnLoadRndPaletteClick(Sender: TObject);
    procedure CbPickModeSelect(Sender: TObject);
    procedure CbShowSelectionChange(Sender: TObject);
    procedure ColorPaletteDblClick(Sender: TObject);
    procedure ColorPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColorPaletteSelectColor(Sender: TObject; AColor: TColor);
    procedure EdColCountChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MnuDeletePickedColorClick(Sender: TObject);
    procedure MnuEditPickedColorClick(Sender: TObject);
  private
    { private declarations }
    procedure EditCurColor;
    procedure SetColorInfo(ATitle: string; AColor: TColor);
    procedure UpdateCaption;
    procedure UpdatePalette;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.BtnAddColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    ColorPalette.AddColor(ColorDialog.Color);
  UpdateCaption;
end;

procedure TMainForm.BtnCreateRndPaletteClick(Sender: TObject);
const
  N = 64;
var
  i: Integer;
  R,G,B: Byte;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Add('$COLS 16');
    for i:=1 to N do begin
      R := Random(256);
      G := Random(256);
      B := Random(256);
      L.Add(Format('%d, %d, %d', [R, G, B]));
    end;
    L.SaveToFile('random_palette.pal');
  finally
    L.Free;
  end;
  BtnLoadRndPalette.Enabled := true;
end;

procedure TMainForm.BtnDeleteColorClick(Sender: TObject);
begin
  with ColorPalette do
  begin
    DeleteColor(SelectedIndex);
    if SelectedIndex = ColorCount then SelectedIndex := ColorCount-1;
    ColorSample.Brush.Color := Colors[SelectedColor];
    if Colors[SelectedColor] = clNone then
      ColorSample.Brush.Style := bsClear else
      ColorSample.Brush.Style := bsSolid;
    UpdateCaption;
    SetColorInfo('Current', Colors[SelectedIndex]);
  end;
end;

procedure TMainForm.BtnLoadDefaultPalClick(Sender: TObject);
begin
  if not FileExists('..\default.pal') then
  begin
    ShowMessage('File "default.pal" not found. Copy it from the TColorPalette folder to the current exe folder.');
    exit;
  end;
  ColorPalette.LoadPalette('..\default.pal');
  UpdateCaption;
  EdColCount.Value := ColorPalette.ColumnCount;
end;

procedure TMainForm.BtnLoadRndPaletteClick(Sender: TObject);
begin
  ColorPalette.LoadPalette('random_palette.pal');
  UpdateCaption;
  EdColCount.Value := ColorPalette.ColumnCount;
end;

procedure TMainForm.BtnEditColorClick(Sender: TObject);
begin
  if BtnEditColor.caption = 'Edit' then
    EditCurColor
  else
    UpdatePalette;
end;

procedure TMainForm.CbPickModeSelect(Sender: TObject);
begin
  ColorPalette.PickMode := TPickMode(CbPickMode.ItemIndex);
end;

procedure TMainForm.CbShowSelectionChange(Sender: TObject);
begin
  ColorPalette.ShowSelection := CbShowSelection.Checked;
end;

procedure TMainForm.ColorPaletteDblClick(Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := ColorPalette.Colors[ColorPalette.SelectedIndex];
    if Execute then
    begin
      ColorPalette.Colors[ColorPalette.SelectedIndex] := Color;
      ColorSample.Brush.Color := Color;
      ColorSample.Brush.Style := bsSolid;
      SetColorInfo('Current', Color);
      with  BtnEditColor do
      begin
        Caption := 'Edit';
        Hint := 'Edit current color';
      end;
    end;
  end;
end;

procedure TMainForm.ColorPaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
exit;



  BtnDeleteColor.caption := 'Delete color #' + IntToStr(ColorPalette.SelectedIndex);
  UpdateCaption;
end;

procedure TMainForm.ColorPaletteSelectColor(Sender: TObject; AColor: TColor);
begin
  ColorSample.Brush.Color := AColor;
  if AColor = clNone then
    ColorSample.Brush.Style := bsClear else
    ColorSample.Brush.Style := bsSolid;
  SetColorInfo('SelectedColor', AColor);
  BtnDeleteColor.Caption := 'Delete color #' + IntToStr(ColorPalette.SelectedIndex);
  UpdateCaption;
end;

procedure TMainForm.EdColCountChange(Sender: TObject);
begin
  ColorPalette.ColumnCount := EdColCount.Value;
end;

procedure TMainForm.EditCurColor;
begin
  with ColorDialog do
  begin
    Color := ColorSample.Brush.color;
    if Execute then begin
      ColorSample.Brush.Color := Color;
      ColorSample.Brush.Style := bsSolid;
    end;
  end;
  if ColorSample.Brush.Color <> ColorPalette.SelectedColor then
  begin
    BtnEditColor.caption := 'Update >';
    BtnEditColor.hint := 'Update palette';
    SetColorInfo('New color', ColorSample.Brush.Color);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ColorSample.Brush.Color := ColorPalette.SelectedColor;
  SetColorInfo('Current', ColorPalette.SelectedColor);
  UpdateCaption;

  { ColorPalette.PickShift must contain ssRight in order to be able to select
    colors for the context menu. Use object inspector, or use this code:  }
  ColorPalette.PickShift := [ssLeft, ssRight];
end;

procedure TMainForm.MnuDeletePickedColorClick(Sender: TObject);
begin
  BtnDeleteColorClick(self);
end;

procedure TMainForm.MnuEditPickedColorClick(Sender: TObject);
begin
  BtnEditColorClick(self);
end;

procedure TMainForm.SetColorInfo(ATitle: string; AColor: TColor);
begin
  if AColor = clNone then
    LblColorInfo.Caption := Format(
      '%s: %s', [ATitle, ColorToString(AColor)]
    )
  else
    LblColorInfo.caption := Format(
      '%s: %s'#13+
      ' red = %d'#13+
      ' green = %d'#13+
      ' blue = %d',
      [ATitle, ColorToString(AColor), Red(AColor), Green(AColor), Blue(AColor)]
    );
end;

procedure TMainForm.UpdateCaption;
begin
  Caption := Format('ColorPalette demo - CurIndex: %d (%d colors available)',
    [ColorPalette.SelectedIndex, ColorPalette.ColorCount]
  );
end;

procedure TMainForm.UpdatePalette;
begin
  ColorPalette.Colors[ColorPalette.SelectedIndex] := ColorSample.Brush.Color;
  SetColorInfo('Current', ColorSample.Brush.Color);
  with  BtnEditColor do
  begin
    Caption := 'Edit';
    Hint := 'Edit current color';
  end;
end;

end.

