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

procedure TForm1.FormCreate(Sender: TObject);
begin
  Toolbar.BorderSpacing.Left := 0;
  Toolbar.AutoSize := true;
  Coolbar.AutoSize := true;

  ColorPaletteColorPick(self, ColorPalette.Colors[0], [ssLeft]);
  ColorPaletteColorPick(self, ColorPalette.Colors[ColorPalette.ColorCount-1], [ssRight]);

  // For Laz 1.4.2 where TPanel.OnPaint is not published:
  Panel1.OnPaint := @Panel1Paint;
end;

procedure TForm1.Panel1Paint(Sender: TObject);
begin
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
  Toolbar.AutoSize := false;
  if TbChangeOrientation.Down then
  begin
    CoolBar.Vertical := true;
    CoolBar.Align := alLeft;
    ToolBar.Align := alLeft;
    ColorPalette.Flipped := not ColorPalette.Flipped;
    ColorPalette.Top := 9999;
  end
  else
  // Horizontal orientation
  begin
    CoolBar.Vertical := false;
    CoolBar.Align := alTop;
    ToolBar.Align := alTop;
    ColorPalette.Flipped := not ColorPalette.Flipped;
    ColorPalette.Left := 9999;
  end;
  Toolbar.AutoSize := true;
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
  // Select gradient start color with left mouse button
  if (Shift * [ssLeft] <> []) then
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

  // Select gradient end color with right mouse button
  if (Shift * [ssRight] <> []) then
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

