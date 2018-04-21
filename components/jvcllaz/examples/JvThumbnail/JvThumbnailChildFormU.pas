{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{$mode objfpc}{$H+}

unit JvThumbnailChildFormU;

interface

uses
  Classes, SysUtils, Controls, Forms,
  StdCtrls, ExtCtrls, FileCtrl, ComCtrls, ShellCtrls, Spin,
  JvThumbImage, JvThumbNails, JvBaseThumbnail, JvExExtCtrls;

type

  { TJvThumbnailChildForm }

  TJvThumbnailChildForm = class(TForm)
    CenterBevel: TBevel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit1: TSpinEdit;
    Splitter2: TSplitter;
    Panel6: TPanel;
    Splitter4: TSplitter;
    ShellTreeView: TShellTreeView;
    ShellListView: TShellListView;
    Panel8: TPanel;
    LblRed: TLabel;
    LblGreen: TLabel;
    LblBlue: TLabel;
    LblContrast: TLabel;
    REDBar: TTrackBar;
    GreenBar: TTrackBar;
    BlueBar: TTrackBar;
    ContrastBar: TTrackBar;
    BtnApply: TButton;
    Panel10: TPanel;
    FilterComboBox: TFilterComboBox;
    Panel7: TPanel;
    Panel5: TPanel;
    LblThumbTitle: TLabel;
    CbAsButton: TCheckBox;
    CbAutoLoad: TCheckBox;
    CbMinimizeMem: TCheckBox;
    GbTitlePlacement: TRadioGroup;
    EdThumbTitle: TEdit;
    GbThumbImage: TGroupBox;
    BtnInvert: TButton;
    BtnGrayScale: TButton;
    LblLightness: TLabel;
    LightnessBar: TTrackBar;
    BtnExit: TButton;
    GbAngle: TRadioGroup;
    ThumbNail: TJVThumbNail;
    ThumbImage: TJvThumbImage;
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnInvertClick(Sender: TObject);
    procedure BtnGrayScaleClick(Sender: TObject);
    procedure CbAsButtonClick(Sender: TObject);
    procedure CbAutoLoadClick(Sender: TObject);
    procedure CbMinimizeMemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GbAngleClick(Sender: TObject);
    procedure GbTitlePlacementClick(Sender: TObject);
    procedure Panel10Resize(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure Panel8Resize(Sender: TObject);
    procedure ShellListViewChange(Sender: TObject);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure SpinEdit1Change(Sender: TObject);
    procedure ThumbNailClick(Sender: TObject);
    procedure ThumbImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ThumbImageMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure ThumbImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FMousePt: TPoint;
    procedure LoadFile(const AFileName: String);
  public
    function GetFileName: String;
    procedure SetFileName(AFileName: String);
  end;

var
  JvThumbnailChildForm: TJvThumbnailChildForm;

implementation

{$R *.lfm}

uses
  JvThumbnailDatamodule;

procedure TJvThumbnailChildForm.BtnApplyClick(Sender: TObject);
begin
  ThumbImage.ChangeRGB(RedBar.Position, GreenBar.Position, BlueBar.Position);
  ThumbImage.Contrast(ContrastBar.Position);
  ThumbImage.Lightness(LightnessBar.Position);
  RedBar.Position := 0;
  GreenBar.Position :=0;
  BlueBar.Position := 0;
  ContrastBar.Position := 0;
  LightnessBar.Position := 0;
end;

procedure TJvThumbnailChildForm.LoadFile(const AFileName: String);
var
  crs: TCursor;
begin
  crs := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Application.ProcessMessages;
  try
    ThumbNail.FileName := AFileName;
    ThumbImage.LoadFromFile(AFileName);
    ThumbImage.Width := ThumbImage.Picture.Width;
    ThumbImage.Height := ThumbImage.Picture.Height;
    ThumbImage.Left := 0;
    ThumbImage.Top := 0;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TJvThumbnailChildForm.ShellListViewChange(Sender: TObject);
var
  fn: String;
begin
  if ShellListView.Selected <> nil then begin
    fn := ShellListView.GetPathFromItem(ShellListView.Selected);
    Loadfile(fn);
  end;
end;

procedure TJvThumbnailChildForm.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.ImageIndex := 0
  else
    Node.ImageIndex := 1;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TJvThumbnailChildForm.SpinEdit1Change(Sender: TObject);
begin
  Thumbnail.Margin := SpinEdit1.Value;
end;

procedure TJvThumbnailChildForm.ThumbImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  W, H: Integer;
begin
  FMousePt := Point(X, Y);
  W := ThumbImage.Parent.Width;
  H := ThumbImage.Parent.Height;
  if (ThumbImage.Width > W) or (ThumbImage.Height > H) then
    ThumbImage.Cursor := crDrag;
end;

procedure TJvThumbnailChildForm.ThumbImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  L, T: Integer;
  W, H: Integer;
begin
  if ssLeft in Shift then begin
    W := ThumbImage.Parent.Width;
    H := ThumbImage.Parent.Height;
    if (ThumbImage.Width <= W) and (ThumbImage.Height <= H) then
      exit;
    L := ThumbImage.Left + (X - FMousePt.X);
    T := ThumbImage.Top + (Y - FMousePt.Y);
    {
    if L < 0 then L := 0;
    if T > 0 then T := 0;
    if L + ThumbImage.Width > W then L := W - ThumbImage.Width;
    if T + ThumbImage.Height > H then T := H - ThumbImage.Height;
    }
    ThumbImage.SetBounds(L, T, ThumbImage.Width, ThumbImage.Height);
  end;
end;

procedure TJvThumbnailChildForm.ThumbImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ThumbImage.Cursor := crDefault;
end;

procedure TJvThumbnailChildForm.CbAsButtonClick(Sender: TObject);
begin
  ThumbNail.Asbutton := CbAsButton.Checked;
end;

procedure TJvThumbnailChildForm.CbAutoLoadClick(Sender: TObject);
begin
  ThumbNail.AutoLoad := CbAutoLoad.Checked;
end;

procedure TJvThumbnailChildForm.CbMinimizeMemClick(Sender: TObject);
begin
  ThumbNail.MinimizeMemory := CbMinimizeMem.Checked;
end;

procedure TJvThumbnailChildForm.FormCreate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  ThumbImage.Parent.DoubleBuffered := true;
  ShellListView.DoubleBuffered := true;
  {$ENDIF}
  FilterCombobox.ItemIndex := 0;
  ShellListView.Mask := FilterCombobox.Mask;
end;

procedure TJvThumbnailChildForm.GbTitlePlacementClick(Sender: TObject);
begin
  ThumbNail.TitlePlacement := TTitlePos(GbTitlePlacement.ItemIndex);
end;

procedure TJvThumbnailChildForm.Panel8Resize(Sender: TObject);
begin
  RedBar.Width := panel8.ClientWidth;
end;

procedure TJvThumbnailChildForm.BtnInvertClick(Sender: TObject);
begin
  ThumbImage.Invert;
end;

procedure TJvThumbnailChildForm.BtnGrayScaleClick(Sender: TObject);
begin
  ThumbImage.GrayScale;
end;

procedure TJvThumbnailChildForm.ThumbNailClick(Sender: TObject);
begin
  if ThumbNail.FileName<>'' then
    ThumbImage.Loadfromfile(ThumbNail.FileName);
end;

procedure TJvThumbnailChildForm.Panel10Resize(Sender: TObject);
begin
  FilterComboBox.Width := panel10.ClientWidth;
  FilterComboBox.Height:= panel10.ClientHeight;
end;

procedure TJvThumbnailChildForm.Panel2Resize(Sender: TObject);
var
  L, T: Integer;
begin
  L := ThumbImage.Left;
  T := ThumbImage.Top;
  if (L < 0) and (L + ThumbImage.Width < Width) then
    L := Width - ThumbImage.Width;
  if (T < 0) and (T + ThumbImage.Height < Height) then
    T := Height - ThumbImage.Height;
  if (L <> ThumbImage.Left) or (T <> ThumbImage.Top) then
    ThumbImage.SetBounds(L, T, ThumbImage.Width, ThumbImage.Height);
end;

procedure TJvThumbnailChildForm.FormShow(Sender: TObject);
begin
  //ThumbImage.Picture.Free;
  GbTitlePlacement.ItemIndex := integer(ThumbNail.titlePlacement);
  GbAngle.ItemIndex := integer(ThumbImage.angle);
  SpinEdit1.Value := Thumbnail.Margin;
end;

procedure TJvThumbnailChildForm.GbAngleClick(Sender: TObject);
var
  w, h: Integer;
begin
  w := ThumbImage.Picture.Width;
  h := ThumbImage.Picture.Height;
  ThumbImage.Angle := TAngle(GbAngle.ItemIndex);
  if (w <> ThumbImage.Picture.Width) or (h <> ThumbImage.Picture.Height) then
    with ThumbImage do SetBounds(0, 0, Picture.Width, Picture.Height);
end;

function TJvThumbnailChildForm.GetfileName: String;
begin
  Result := ShellListView.GetPathFromItem(ShellListView.Selected);
end;

procedure TJvThumbnailChildForm.SetFileName(AFileName: String);
var
  dir, fn: String;
  item: TListItem;
begin
  dir := ExtractFilePath(AFileName);
  fn := ExtractFileName(AFileName);
  if dir <> ShellListView.Root then
    ShellTreeView.Path := dir;
  item := ShellListView.Items.FindCaption(-0, fn, false, true, false);
  ShellListView.Selected := item;
end;


end.
