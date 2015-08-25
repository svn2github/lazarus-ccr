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
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    ToolBar: TToolBar;
    TbChangeOrientation: TToolButton;
    TbSpacer: TToolButton;
    procedure ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ColorPaletteSelectColor(Sender: TObject; AColor: TColor);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Paint(Sender: TObject);
    procedure TbChangeOrientationClick(Sender: TObject);
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

procedure TForm1.ColorPaletteSelectColor(Sender: TObject; AColor: TColor);
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
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ColorPalette.InsertColor(0, clNone);
  ColorPalette.ColumnCount := ColorPalette.ColorCount;
  ColorPalette.SelectedIndex := -1;
  Toolbar.BorderSpacing.Left := 0;
  Toolbar.AutoSize := true;
  Coolbar.AutoSize := true;
end;

procedure TForm1.Panel1Paint(Sender: TObject);
begin
  Panel1.Canvas.GradientFill(Panel1.ClientRect, clSkyBlue, clNavy, gdVertical);
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
    ColorPalette.Vertical := true;
    ColorPalette.Top := 9999;
  end else
  // Horizontal orientation
  begin
    CoolBar.Vertical := false;
    CoolBar.Align := alTop;
    ToolBar.Align := alTop;
    ColorPalette.Vertical := false;
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

end.

