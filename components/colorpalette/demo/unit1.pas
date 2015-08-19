unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorPalette;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnDeleteColor0: TButton;
    BtnLoadRndPalette: TButton;
    BtnCreateRndPalette: TButton;
    BtnAddColor: TButton;
    BtnLoadDefaultPal: TButton;
    ColorDialog1: TColorDialog;
    ColorPalette1: TColorPalette;
    Label1: TLabel;
    procedure BtnDeleteColor0Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnLoadRndPaletteClick(Sender: TObject);
    procedure BtnCreateRndPaletteClick(Sender: TObject);
    procedure BtnAddColorClick(Sender: TObject);
    procedure BtnLoadDefaultPalClick(Sender: TObject);
    procedure ColorPalette1ColorPick(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ColorPalette1ColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  ShowMessage(Format(
    'Color %s picked.'+#13+
    '  red = %d'#13+
    '  green = %d'#13+
    '  blue = %d', [ColorToString(AColor), Red(AColor), Green(AColor), Blue(AColor)]));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ColorPalette1.LoadPalette('palette1.txt');
  Label1.caption := IntToStr(ColorPalette1.ColorCount) + ' colors available';
end;

procedure TForm1.BtnDeleteColor0Click(Sender: TObject);
begin
  if ColorPalette1.ColorCount > 0 then
  begin
    ColorPalette1.DeleteColor(0);
    Label1.Caption := IntToStr(ColorPalette1.ColorCount) + ' colors available';
  end;
end;

procedure TForm1.BtnLoadRndPaletteClick(Sender: TObject);
begin
  ColorPalette1.LoadPalette('random_palette.pal');
  Label1.Caption := IntToStr(ColorPalette1.ColorCount) + ' colors available';
end;

procedure TForm1.BtnCreateRndPaletteClick(Sender: TObject);
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

procedure TForm1.BtnAddColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
    ColorPalette1.AddColor(ColorDialog1.Color);
  Label1.caption := IntToStr(ColorPalette1.ColorCount) + ' colors available';
end;

procedure TForm1.BtnLoadDefaultPalClick(Sender: TObject);
begin
  if not FileExists('..\default.pal') then
  begin
    ShowMessage('File "default.pal" not found. Copy it from the TColorPalette folder to the current exe folder.');
    exit;
  end;
  ColorPalette1.LoadPalette('..\default.pal');
  Label1.caption := IntToStr(ColorPalette1.ColorCount) + ' colors available';
end;

end.

