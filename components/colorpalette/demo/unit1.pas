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
    BtnDeleteCurrent: TButton;
    BtnLoadDefaultPal1: TButton;
    BtnLoadRndPalette: TButton;
    BtnCreateRndPalette: TButton;
    BtnAddColor: TButton;
    BtnLoadDefaultPal: TButton;
    BtnEditColor: TButton;
    ColorDialog: TColorDialog;
    ColorPalette: TColorPalette;
    LblPaletteSize: TLabel;
    EdColCount: TSpinEdit;
    Label2: TLabel;
    LblInfo: TLabel;
    MnuEditPickedColor: TMenuItem;
    MnuDeletePickedColor: TMenuItem;
    PalettePopupMenu: TPopupMenu;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    curColor: TShape;
    procedure BtnDeleteCurrentClick(Sender: TObject);
    procedure BtnLoadDefaultPal1Click(Sender: TObject);
    procedure BtnLoadRndPaletteClick(Sender: TObject);
    procedure BtnCreateRndPaletteClick(Sender: TObject);
    procedure BtnAddColorClick(Sender: TObject);
    procedure BtnLoadDefaultPalClick(Sender: TObject);
    procedure ColorPaletteColorPick(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure ColorPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EdColCountChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MnuDeletePickedColorClick(Sender: TObject);
    procedure MnuEditPickedClick(Sender: TObject);
    procedure BtnEditColorClick(Sender: TObject);
  private
    { private declarations }
    curIndex: integer;
    procedure EditCurColor;
    procedure SetLabel(ATitle: string; AColor: TColor);
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
  LblPaletteSize.caption := IntToStr(ColorPalette.ColorCount) + ' colors available';
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

procedure TMainForm.BtnDeleteCurrentClick(Sender: TObject);
begin
  with ColorPalette do
  begin
    if (curIndex < ColorCount) and (ColorCount > 0) then
    begin
      DeleteColor(curIndex);
      if curIndex = ColorCount then dec(curIndex);
      curColor.Brush.Color := Colors[curIndex] ;
      LblPaletteSize.Caption := IntToStr(ColorCount) + ' colors available';
      SetLabel('Current', ColorPalette.Colors[curIndex]);
    end;
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
  LblPaletteSize.caption := IntToStr(ColorPalette.ColorCount) + ' colors available';
  EdColCount.Value := ColorPalette.ColumnCount;
end;

procedure TMainForm.BtnLoadDefaultPal1Click(Sender: TObject);
begin
  Showmessage('???');
  SaveDialog.FileName := 'random_palette.pal';
  SaveDialog.InitialDir := ExtractFileDir(ParamStr(0));
  if SaveDialog.Execute then
    ColorPalette.SavePalette(SaveDialog.FileName);
end;

procedure TMainForm.BtnLoadRndPaletteClick(Sender: TObject);
begin
  ColorPalette.LoadPalette('random_palette.pal');
  LblPaletteSize.Caption := IntToStr(ColorPalette.ColorCount) + ' colors available';
  EdColCount.Value := ColorPalette.ColumnCount;
end;

procedure TMainForm.BtnEditColorClick(Sender: TObject);
begin
  if BtnEditColor.caption = 'Edit' then
    EditCurColor
  else
    UpdatePalette;
end;

procedure TMainForm.ColorPaletteColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  curColor.Brush.Color := ColorPalette.PickedColor;
  SetLabel('PickedColor', ColorPalette.PickedColor);
end;

procedure TMainForm.ColorPaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with ColorPalette do
  begin
    X := X div ButtonWidth;
    Y := Y div ButtonHeight;
    curIndex := X + Y * ColumnCount;
  end;
  BtnDeleteCurrent.caption := 'Delete color #' + IntToStr(curIndex);
  Caption := 'CurIndex: ' + IntToStr(curIndex);
end;

procedure TMainForm.EdColCountChange(Sender: TObject);
begin
  ColorPalette.ColumnCount := EdColCount.Value;
end;

procedure TMainForm.EditCurColor;
begin
  with ColorDialog do
  begin
    Color := curColor.Brush.color;
    if Execute then
      curColor.Brush.Color := Color;
  end;
  if curColor.Brush.Color <> ColorPalette.PickedColor then
  begin
    BtnEditColor.caption := 'Update';
    BtnEditColor.hint := 'Update palette';
    SetLabel('New color', curColor.Brush.Color);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'TColorPalette Demo';
  curIndex := 0;
  curColor.brush.color := ColorPalette.Colors[0];
  SetLabel('Current', ColorPalette.Colors[curIndex]);
  LblPaletteSize.Caption := IntToStr(ColorPalette.ColorCount) + ' colors available';
end;

procedure TMainForm.MnuDeletePickedColorClick(Sender: TObject);
begin
  BtnDeleteCurrentClick(self);
end;

procedure TMainForm.MnuEditPickedClick(Sender: TObject);
begin
  BtnEditColorClick(self);
end;

procedure TMainForm.SetLabel(ATitle: string; AColor: TColor);
begin
  LblInfo.caption := Format(
    '%s: %s'#13+
    ' red = %d'#13+
    ' green = %d'#13+
    ' blue = %d', [ATitle, ColorToString(AColor), Red(AColor), Green(AColor), Blue(AColor)]
  );
end;

procedure TMainForm.UpdatePalette;
begin
  ColorPalette.Colors[curIndex] := curColor.Brush.Color;
  ColorPalette.Refresh;
  SetLabel('Current', ColorPalette.Colors[curIndex]);
  with  BtnEditColor do
  begin
    Caption := 'Edit';
    Hint := 'Edit current color';
  end;
end;

end.

