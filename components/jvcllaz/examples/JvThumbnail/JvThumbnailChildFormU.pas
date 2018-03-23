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
    Bevel2: TBevel;
    Panel1: TPanel;
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
    FilterComboBox1: TFilterComboBox;
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
    Button5: TButton;
    LblLightness: TLabel;
    LightnessBar: TTrackBar;
    BtnExit: TButton;
    GbAngle: TRadioGroup;
    ThumbNail: TJVThumbNail;
    ThumbImage: TJvThumbImage;
    procedure BtnApplyClick(Sender: TObject);
    procedure ShellListViewChange(Sender: TObject);
    procedure CbAsButtonClick(Sender: TObject);
    procedure CbAutoLoadClick(Sender: TObject);
    procedure CbMinimizeMemClick(Sender: TObject);
    procedure GbTitlePlacementClick(Sender: TObject);
    procedure Panel8Resize(Sender: TObject);
    procedure BtnInvertClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure SpinEdit1Change(Sender: TObject);
    procedure ThumbNailClick(Sender: TObject);
    procedure Panel10Resize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GbAngleClick(Sender: TObject);
  public
    procedure SetFileName(AFileName: String);
    function GetFileName: String;
  end;

var
  JvThumbnailChildForm: TJvThumbnailChildForm;

implementation

{$R *.lfm}

uses
  JvThumbnailDatamodule;

procedure TJvThumbnailChildForm.BtnApplyClick(Sender: TObject);
begin
  ThumbImage.ChangeRGB(redbar.Position,greenbar.Position,bluebar.Position);
  ThumbImage.Contrast(ContrastBar.Position);
  ThumbImage.Lightness(LightnessBar.Position);
  RedBar.Position := 0;
  GreenBar.Position :=0;
  BlueBar.Position := 0;
  ContrastBar.Position := 0;
  LightnessBar.Position := 0;
end;

procedure TJvThumbnailChildForm.ShellListViewChange(Sender: TObject);
var
  fn: String;
begin
  if ShellListView.Selected <> nil then begin
    fn := ShellListView.GetPathFromItem(ShellListView.Selected);
    ThumbNail.FileName := fn;
    ThumbImage.Loadfromfile(fn);
  end;
end;

procedure TJvThumbnailChildForm.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.ImageIndex := 0
  else
    Node.ImageIndex := 1;
end;

procedure TJvThumbnailChildForm.SpinEdit1Change(Sender: TObject);
begin
  Thumbnail.Margin := SpinEdit1.Value;
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

procedure TJvThumbnailChildForm.Button5Click(Sender: TObject);
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
  filtercombobox1.Width := panel10.ClientWidth;
  filtercombobox1.Height:= panel10.ClientHeight;
end;

procedure TJvThumbnailChildForm.FormShow(Sender: TObject);
begin
  //ThumbImage.Picture.Free;
  GbTitlePlacement.ItemIndex := integer(ThumbNail.titlePlacement);
  GbAngle.ItemIndex := integer(ThumbImage.angle);
  SpinEdit1.Value := Thumbnail.Margin;
end;

procedure TJvThumbnailChildForm.GbAngleClick(Sender: TObject);
begin
  ThumbImage.angle := TAngle(GbAngle.ItemIndex)
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
