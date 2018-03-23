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
  StdCtrls, ExtCtrls, FileCtrl, ComCtrls, ShellCtrls,
  JvThumbImage, JvThumbNails, JvBaseThumbnail, JvExExtCtrls;

type
  TJvThumbnailChildForm = class(TForm)
    Splitter2: TSplitter;
    Panel6: TPanel;
    Splitter4: TSplitter;
    ShellTreeView: TShellTreeView;
    ShellListView: TShellListView;
    Panel8: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    REDBar: TTrackBar;
    GreenBar: TTrackBar;
    BlueBar: TTrackBar;
    contrastBar: TTrackBar;
    Button2: TButton;
    Panel10: TPanel;
    FilterComboBox1: TFilterComboBox;
    Panel7: TPanel;
    Panel5: TPanel;
    Label5: TLabel;
    Bevel1: TBevel;
    CbAsButton: TCheckBox;
    CbAutoLoad: TCheckBox;
    CbMinimizeMem: TCheckBox;
    GbTitlePlacement: TRadioGroup;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    BtnInvert: TButton;
    Button5: TButton;
    Label1: TLabel;
    LightnessBar: TTrackBar;
    BtnExit: TButton;
    GbAngle: TRadioGroup;
    ThumbNail1: TJVThumbNail;
    ThumbImage1: TJvThumbImage;
    procedure Button2Click(Sender: TObject);
    procedure ShellListViewChange(Sender: TObject);
    procedure CbAsButtonClick(Sender: TObject);
    procedure CbAutoLoadClick(Sender: TObject);
    procedure CbMinimizeMemClick(Sender: TObject);
    procedure GbTitlePlacementClick(Sender: TObject);
    procedure Panel8Resize(Sender: TObject);
    procedure BtnInvertClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure thumbnail1Click(Sender: TObject);
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

procedure TJvThumbnailChildForm.Button2Click(Sender: TObject);
begin
  ThumbImage1.ChangeRGB(redbar.Position,greenbar.Position,bluebar.Position);
  ThumbImage1.Contrast(contrastbar.Position);
  ThumbImage1.Lightness(LightnessBar.Position);
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
    Thumbnail1.FileName := fn;
    ThumbImage1.Loadfromfile(fn);
  end;
end;

procedure TJvThumbnailChildForm.CbAsButtonClick(Sender: TObject);
begin
  THumbnail1.Asbutton := CbAsButton.Checked;
end;

procedure TJvThumbnailChildForm.CbAutoLoadClick(Sender: TObject);
begin
  thumbnail1.autoload := CbAutoLoad.Checked;
end;

procedure TJvThumbnailChildForm.CbMinimizeMemClick(Sender: TObject);
begin
  thumbnail1.minimizememory:=CbMinimizeMem.Checked;
end;

procedure TJvThumbnailChildForm.GbTitlePlacementClick(Sender: TObject);
begin
  thumbnail1.TitlePlacement := ttitlepos(GbTitlePlacement.ItemIndex);
end;

procedure TJvThumbnailChildForm.Panel8Resize(Sender: TObject);
begin
  RedBar.Width := panel8.ClientWidth;
end;

procedure TJvThumbnailChildForm.BtnInvertClick(Sender: TObject);
begin
  ThumbImage1.Invert;
end;

procedure TJvThumbnailChildForm.Button5Click(Sender: TObject);
begin
  ThumbImage1.GrayScale;
end;

procedure TJvThumbnailChildForm.thumbnail1Click(Sender: TObject);
begin
  if thumbnail1.FileName<>'' then
    thumbimage1.Loadfromfile(thumbnail1.FileName);
end;

procedure TJvThumbnailChildForm.Panel10Resize(Sender: TObject);
begin
  filtercombobox1.Width := panel10.ClientWidth;
  filtercombobox1.Height:= panel10.ClientHeight;
end;

procedure TJvThumbnailChildForm.FormShow(Sender: TObject);
begin
  //thumbimage1.Picture.Free;
  GbTitlePlacement.ItemIndex := integer(thumbnail1.titlePlacement);
  GbAngle.ItemIndex := integer(thumbimage1.angle);
end;

procedure TJvThumbnailChildForm.GbAngleClick(Sender: TObject);
begin
  thumbimage1.angle := TAngle(GbAngle.ItemIndex)
end;

function TJvThumbnailChildForm.GetfileName: String;
begin
  Result := ShellListView.GetPathFromItem(ShellListView.Selected);
end;

procedure TJvThumbnailChildForm.SetFileName(AFileName: String);
var
  dir, fn: String;
begin
  dir := ExtractFilePath(AFileName);
  fn := ExtractFileName(AFileName);
  if dir <> ShellListView.Root then
    ShellTreeView.Path := dir;
  ShellListView.Selected := ShellListView.Items.FindCaption(0, fn, false, false, false);
end;


end.
