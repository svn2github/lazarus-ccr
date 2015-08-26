unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ColorPalette;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorPalette: TColorPalette;
    CoolBar: TCoolBar;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    ToolBar: TToolBar;
    TbChangeOrientation: TToolButton;
    TbSpacer: TToolButton;
    procedure ColorPaletteColorPick(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Paint(Sender: TObject);
    procedure TbChangeOrientationClick(Sender: TObject);
  private
    { private declarations }
    FStartColor: TColor;
    FEndColor: TColor;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
                  (*
procedure TForm1.ColorPalettePickColorColor(Sender: TObject; AColor: TColor;
  AShift: TShiftState);
begin
  if ColorPalette.SelectedColor = clNone then
    Shape1.Brush.Style := bsClear
  else
  begin
    Shape1.Brush.Style := bsSolid;
    Shape1.Brush.Color := ColorPalette.SelectedColor;
  end;
  Label1.Caption := Format('Selected color:'#13'%s', [
    ColorPalette.ColorNames[ColorPalette.SelectedIndex]
  ]);

  inc(counter);
  if odd(counter) then
    FStartColor := Colorpalette.SelectedColor else
    FEndColor := ColorPalette.SelectedColor;

  Panel1.Invalidate;
end;
            *)
procedure TForm1.FormCreate(Sender: TObject);
begin
//  ColorPalette.InsertColor(0, clNone);
  //ColorPalette.ColumnCount := 3; //ColorPalette.ColorCount div 3;
 // ColorPalette.Vertical := true;
  //ColorPalette.ColumnCount := 3;
  ColorPalette.SelectedIndex := -1;
  colorPalette.SelectedIndex := 0;
  Toolbar.BorderSpacing.Left := 0;
  Toolbar.AutoSize := true;
  Coolbar.AutoSize := true;

  ColorPaletteColorPick(self, ColorPalette.SelectedColor, [Classes.ssLeft]);
  ColorPaletteColorPick(self, ColorPalette.Colors[ColorPalette.ColorCount-1], [Classes.ssRight]);

  Panel1.OnPaint := @Panel1Paint;
end;

procedure TForm1.Panel1Paint(Sender: TObject);
begin
//  Panel1.Canvas.GradientFill(Panel1.ClientRect, clSkyBlue, clNavy, gdVertical);
  Panel1.Canvas.GradientFill(Panel1.ClientRect,
    FStartColor,
    FEndColor,
    gdVertical
  );
end;

procedure TForm1.TbChangeOrientationClick(Sender: TObject);
var
  i: Integer;
begin
  // Vertical orientation
  CoolBar.AutoSize := false;
  if TbChangeOrientation.Down then
  begin
    CoolBar.Vertical := true;
    CoolBar.Align := alLeft;
    ToolBar.Align := alLeft;
    ColorPalette.Flipped := not ColorPalette.Flipped;
    ColorPalette.Top := 9999;
  end else
  // Horizontal orientation
  begin
    CoolBar.Vertical := false;
    CoolBar.Align := alTop;
    ToolBar.Align := alTop;
    ColorPalette.Flipped := not ColorPalette.Flipped;
    ColorPalette.Left := 9999;
  end;
  CoolBar.AutoSize := true;
end;

procedure TForm1.ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Shape2.Brush.Color := ColorPalette.MouseColor;
  Label2.Caption := Format('Mouse color:'#13'%s', [
    ColorPalette.ColorNames[ColorPalette.MouseIndex]
  ]);
end;

procedure TForm1.ColorPaletteColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  if (Shift * [Classes.ssLeft] <> []) then
  begin
    FStartColor := AColor;
    if FStartColor = clNone then
      Shape1.Brush.Style := bsClear
    else begin
      Shape1.Brush.Style := bsSolid;
      Shape1.Brush.Color := FStartColor;
    end;
    Label1.Caption := 'Gradient start color:'#13 +
      ColorPalette.ColorNames[ColorPalette.MouseIndex] +
      #13'(Left click)';
  end;

  if (Shift * [Classes.ssRight] <> []) then
  begin
    FEndColor := AColor;
    if FEndColor = clNone then
      Shape3.Brush.Style := bsClear
    else begin
      Shape3.Brush.Style := bsSolid;
      Shape3.Brush.Color := FEndColor;
    end;
    Label3.Caption := 'Gradient end color:'#13 +
      ColorPalette.ColorNames[ColorPalette.MouseIndex] +
      #13'(Right click)';
  end;

  Panel1.Invalidate;
end;

end.

