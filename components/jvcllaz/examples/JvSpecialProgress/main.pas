unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ColorBox,
  ExtCtrls, JvSpecialProgress;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    CbBorderColor: TColorBox;
    CbTextColor: TColorBox;
    CbTextCentered: TCheckBox;
    CbStartColor: TColorBox;
    CbEndColor: TColorBox;
    CbGradientBlocks: TCheckBox;
    CbSolid: TCheckBox;
    CbBorder: TCheckBox;
    CbFlat: TCheckBox;
    CbTextOption: TComboBox;
    EdFormat: TEdit;
    JvSpecialProgress1: TJvSpecialProgress;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LblFormat: TLabel;
    ScrollBar1: TScrollBar;
    procedure CbBorderChange(Sender: TObject);
    procedure CbBorderColorChange(Sender: TObject);
    procedure CbTextColorChange(Sender: TObject);
    procedure CbEndColorChange(Sender: TObject);
    procedure CbFlatChange(Sender: TObject);
    procedure CbGradientBlocksChange(Sender: TObject);
    procedure CbSolidChange(Sender: TObject);
    procedure CbStartColorChange(Sender: TObject);
    procedure CbTextCenteredChange(Sender: TObject);
    procedure CbTextOptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    FSavedCaption: String;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ScrollBar1Change(Sender: TObject);
begin
  JvSpecialProgress1.Position := Scrollbar1.Position;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSavedCaption := JvSpecialProgress1.Caption;
  Scrollbar1.Position := JvSpecialProgress1.Position;
  CbStartColor.Selected := JvSpecialProgress1.StartColor;
  CbEndcolor.Selected := JvSpecialProgress1.EndColor;
  CbGradientBlocks.Checked := JvSpecialProgress1.GradientBlocks;
  CbSolid.Checked := JvSpecialProgress1.Solid;
  CbBorder.Checked := JvSpecialProgress1.BorderStyle <> bsNone;
  CbBorderColor.Selected := JvSpecialProgress1.BorderColor;
  CbFlat.Checked := JvSpecialProgress1.Flat;
  CbFlat.Enabled := CbBorder.Checked;
  CbBorderColor.Visible := CbBorder.Checked and CbFlat.Checked;
  CbTextOption.ItemIndex := Ord(JvSpecialProgress1.TextOption);
  CbTextCentered.Checked := JvSpecialProgress1.TextCentered;
  if JvSpecialProgress1.Font.Color = clDefault then
    CbTextColor.Selected := GetDefaultColor(dctFont)
  else
    CbTextColor.Selected := JvSpecialProgress1.Font.Color;
end;

procedure TMainForm.CbStartColorChange(Sender: TObject);
begin
  JvSpecialProgress1.StartColor := CbStartColor.Selected;
end;

procedure TMainForm.CbTextCenteredChange(Sender: TObject);
begin
  JvSpecialProgress1.TextCentered := CbTextCentered.Checked;
end;

procedure TMainForm.CbTextOptionChange(Sender: TObject);
begin
  JvSpecialProgress1.TextOption := TJvTextOption(CbTextOption.ItemIndex);
  if JvSpecialProgress1.TextOption = toFormat then
    JvSpecialProgress1.Caption := EdFormat.Text
  else
    JvSpecialProgress1.Caption := FSavedCaption;
end;

procedure TMainForm.CbEndColorChange(Sender: TObject);
begin
  JvSpecialProgress1.EndColor := CbEndColor.Selected;
end;

procedure TMainForm.CbFlatChange(Sender: TObject);
begin
  JvSpecialProgress1.Flat := CbFlat.Checked;
  CbBorderColor.Visible := CbBorder.Checked and CbFlat.Checked;
end;

procedure TMainForm.CbBorderChange(Sender: TObject);
begin
  if CbBorder.Checked then
    JvSpecialProgress1.BorderStyle := bsSingle
  else
    JvSpecialProgress1.Borderstyle := bsNone;
  CbFlat.Enabled := CbBorder.Checked;
  CbBorderColor.Visible := CbBorder.Checked and CbFlat.Checked;
end;

procedure TMainForm.CbBorderColorChange(Sender: TObject);
begin
  JvSpecialProgress1.BorderColor := CbBorderColor.Selected;
end;

procedure TMainForm.CbTextColorChange(Sender: TObject);
begin
  JvSpecialProgress1.Font.Color := CbTextColor.Selected;
end;

procedure TMainForm.CbGradientBlocksChange(Sender: TObject);
begin
  JvSpecialProgress1.GradientBlocks := CbGradientBlocks.Checked;
end;

procedure TMainForm.CbSolidChange(Sender: TObject);
begin
  JvSpecialProgress1.Solid := CbSolid.Checked;
end;

end.

