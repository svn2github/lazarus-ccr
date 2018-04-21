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

unit JvThumbnailMainFormU;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Graphics, Forms, Spin,
  StdCtrls, ExtCtrls, ComCtrls, Dialogs, ShellCtrls,
  JvThumbnails, JvThumbViews, {%H-}JvThumbnailDatamodule;

  {JvSpecialProgress,
  JvListBox, JvDriveCtrls, JvCombobox, JvExControls, JvComponent,
  JvExStdCtrls, JvExForms; }

type

  { TJvThumbnailMainForm }

  TJvThumbnailMainForm = class(TForm)
    CbThumbTitleBevelInner: TComboBox;
    CbThumbTitleBevelOuter: TComboBox;
    CbThumbTitleBorderStyle: TComboBox;
    CbThumbColor: TColorButton;
    CbTitleColor: TColorButton;
    CbThumbBevelInner: TComboBox;
    CbThumbBevelOuter: TComboBox;
    CbThumbBorderStyle: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    InfoDateAccessed: TLabel;
    InfoFileName: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    LblDateAccessed: TLabel;
    LblFileSize: TLabel;
    InfoFileSize: TLabel;
    LblDateCreated: TLabel;
    InfoDateCreated: TLabel;
    LblDateModified: TLabel;
    InfoDateModified: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LblFilename: TLabel;
    PageControl1: TPageControl;
    Panel4: TPanel;
    Splitter2: TSplitter;
    TabSheet1: TTabSheet;
    Splitter1: TSplitter;
    Panel1: TPanel;
    LblThumbSize: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TbThumbSize: TTrackBar;
    CbAutoScrolling: TCheckBox;
    CbAutoHandleKeyboard: TCheckBox;
    EdGap: TSpinEdit;
    EdSelected: TSpinEdit;
    CbSorted: TCheckBox;
    CbMinMemory: TCheckBox;
    Panel2: TPanel;
    ShellTreeView: TShellTreeView;
    RgAlignView: TRadioGroup;
    RgScrollMode: TRadioGroup;
    Panel3: TPanel;
    BtnStopLoading: TButton;
    BtnEditSelThumb: TButton;
    ThumbView: TJVThumbview;
    Panel5: TPanel;
    ProgressBar: TProgressBar;
    Bevel1: TBevel;

    procedure BtnEditSelThumbClick(Sender: TObject);
    procedure BtnStopLoadingClick(Sender: TObject);

    procedure CbAutoHandleKeyboardClick(Sender: TObject);
    procedure CbAutoScrollingClick(Sender: TObject);
    procedure CbMinMemoryClick(Sender: TObject);
    procedure CbThumbBevelInnerChange(Sender: TObject);
    procedure CbThumbBevelOuterChange(Sender: TObject);
    procedure CbThumbBorderStyleChange(Sender: TObject);
    procedure CbThumbColorColorChanged(Sender: TObject);
    procedure CbThumbTitleBevelInnerChange(Sender: TObject);
    procedure CbThumbTitleBevelOuterChange(Sender: TObject);
    procedure CbThumbTitleBorderStyleChange(Sender: TObject);
    procedure CbTitleColorColorChanged(Sender: TObject);
    procedure EdGapChange(Sender: TObject);
    procedure EdSelectedChange(Sender: TObject);

    procedure FormShow(Sender: TObject);

    procedure RgAlignViewClick(Sender: TObject);
    procedure RgScrollModeClick(Sender: TObject);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TbThumbSizeChange(Sender: TObject);
    procedure ThumbViewChange(Sender: TObject);
    procedure ThumbViewDblClick(Sender: TObject);
    procedure ThumbViewScanProgress(Sender: TObject; APosition: Integer;
      var Break: Boolean);
    procedure ThumbViewStartScanning(Sender: TObject; AMax: Integer);
    procedure ThumbViewStopScanning(Sender: TObject);

  public
    NewDir: Boolean;
    Scanning: Boolean;
  end;

var
  JvThumbnailMainForm: TJvThumbnailMainForm;

implementation

uses
  StrUtils,
  JvThumbnailChildFormU;

{$R *.lfm}

procedure TJvThumbnailMainForm.ThumbViewScanProgress(Sender: TObject;
  APosition: Integer;
  var Break: Boolean);
begin
  ProgressBar.Position := APosition;
  break := Newdir;
end;

procedure TJvThumbnailMainForm.ThumbViewStartScanning(Sender: TObject; AMax: Integer);
begin
  Scanning := True;
//  ShellTreeView.Enabled := False;
  BtnStopLoading.Enabled := True;
  ProgressBar.Max := AMax;
  ProgressBar.Visible := true;
end;

procedure TJvThumbnailMainForm.ThumbViewStopScanning(Sender: TObject);
begin
  Scanning := False;
//  ShellTreeView.Enabled := True;
  EdSelected.MaxValue := ThumbView.Count - 1;
  newdir := False;
  BtnStopLoading.Enabled := False;
  ProgressBar.Position := 0;
  ProgressBar.Visible := false;
  ThumbViewChange(nil);
end;

procedure TJvThumbnailMainForm.BtnStopLoadingClick(Sender: TObject);
begin
  NewDir := True;
end;

procedure TJvThumbnailMainForm.CbAutoScrollingClick(Sender: TObject);
begin
  ThumbView.AutoScrolling := CbAutoScrolling.Checked;
end;

procedure TJvThumbnailMainForm.CbAutoHandleKeyboardClick(Sender: TObject);
begin
  ThumbView.AutoHandleKeyb := CbAutoHandleKeyboard.Checked;
end;

procedure TJvThumbnailMainForm.CbMinMemoryClick(Sender: TObject);
begin
  ThumbView.MinMemory := CbMinMemory.Checked;
end;

procedure TJvThumbnailMainForm.CbThumbBevelInnerChange(Sender: TObject);
begin
  ThumbView.ThumbBevelInner := TPanelBevel(CbThumbBevelInner.ItemIndex);
end;

procedure TJvThumbnailMainForm.CbThumbBevelOuterChange(Sender: TObject);
begin
  ThumbView.ThumbBevelOuter := TPanelBevel(CbThumbBevelOuter.ItemIndex);
end;

procedure TJvThumbnailMainForm.CbThumbBorderStyleChange(Sender: TObject);
begin
  ThumbView.ThumbBorderStyle := TBorderStyle(CbThumbBorderStyle.ItemIndex);
end;

procedure TJvThumbnailMainForm.CbThumbColorColorChanged(Sender: TObject);
begin
  ThumbView.ThumbColor := CbThumbColor.ButtonColor;
end;

procedure TJvThumbnailMainForm.CbThumbTitleBevelInnerChange(Sender: TObject);
begin
  ThumbView.ThumbTitleBevelInner := TPanelBevel(CbThumbTitleBevelInner.ItemIndex);
end;

procedure TJvThumbnailMainForm.CbThumbTitleBevelOuterChange(Sender: TObject);
begin
  ThumbView.ThumbTitleBevelOuter := TPanelBevel(CbThumbTitleBevelOuter.ItemIndex);
end;

procedure TJvThumbnailMainForm.CbThumbTitleBorderStyleChange(Sender: TObject);
begin
  ThumbView.ThumbTitleBorderStyle := TBorderStyle(CbThumbTitleBorderStyle.ItemIndex);
end;

procedure TJvThumbnailMainForm.CbTitleColorColorChanged(Sender: TObject);
begin
  ThumbView.ThumbTitleColor := CbTitleColor.ButtonColor;
end;

procedure TJvThumbnailMainForm.EdGapChange(Sender: TObject);
begin
  if EdGap.Text <> '' then ThumbView.ThumbGap := EdGap.Value;
end;

procedure TJvThumbnailMainForm.EdSelectedChange(Sender: TObject);
begin
  ThumbView.Selected := EdSelected.Value;
end;

procedure TJvThumbnailMainForm.RgAlignViewClick(Sender: TObject);
begin
  ThumbView.AlignView := TViewType(RgAlignView.ItemIndex);
end;

procedure TJvThumbnailMainForm.RgScrollModeClick(Sender: TObject);
begin
  ThumbView.ScrollMode := TscrollMode(RgScrollMode.ItemIndex);
end;

procedure TJvThumbnailMainForm.ShellTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  if not scanning then
    repeat
      ThumbView.Directory := ShellTreeView.Path;
    until ThumbView.Directory = ShellTreeView.Path
  else
    NewDir := True;
end;

procedure TJvThumbnailMainForm.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.ImageIndex := 0
  else
    Node.ImageIndex := 1;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TJvThumbnailMainForm.FormShow(Sender: TObject);
begin
  CbThumbColor.ButtonColor := ThumbView.ThumbColor; //ColorToRGB(ThumbVIew.ThumbColor);
  CbTitleColor.ButtonColor := ThumbView.ThumbTitleColor; //ColorToRGB(ThumbVIew.ThumbColor);
  CbAutoScrolling.Checked := ThumbView.AutoScrolling;
  CbAutoHandleKeyboard.Checked := ThumbView.AutoHandleKeyb;
  CbSorted.Checked := ThumbView.Sorted;
  CbSorted.Checked := ThumbView.MinMemory;
  EdGap.Value := ThumbView.ThumbGap;
  EdSelected.MaxValue := 0;
  EdGap.MinValue := 0;
  RgAlignView.ItemIndex := integer(ThumbView.alignview);
  RgScrollMode.ItemIndex := integer(ThumbView.scrollMode);
  Newdir := False;
  Scanning := False;
end;

procedure TJvThumbnailMainForm.TbThumbSizeChange(Sender: TObject);
begin
  ThumbView.Size := TbThumbSize.Position;
end;

procedure TJvThumbnailMainForm.ThumbViewDblClick(Sender: TObject);
var
  F: TJvThumbnailChildForm;
begin
  F := TJvThumbnailChildForm.Create(Self);
  try
    F.ShellListView.Mask := GraphicFileMask(TCustomBitmap);
    {$IFNDEF Windows}
    F.ShellListView.Mask := Uppercase(F.ShellListView.Mask)+';'+Lowercase(F.ShellListView.Mask);
    // ShellListView, by default, has an uppercase Mask which is usually not good for Linux.
    {$ENDIF}
    F.ShelLTreeView.Path := ShellTreeView.Path;
    if Sender is TJvThumbView then
      F.SetFileName(TJvThumbView(Sender).SelectedFile);
    if Sender is TJvThumbnail then
      F.SetFileName(TJvThumbnail(Sender).FileName);
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TJvThumbnailMainForm.BtnEditSelThumbClick(Sender: TObject);
begin
  ThumbViewDblClick(ThumbView);
end;

procedure TJvThumbnailMainForm.ThumbViewChange(Sender: TObject);
var
  thumbnail: TJvThumbnail;
begin
  Caption := 'JvThumbView Demo - ' + ThumbView.SelectedFile;
  if ThumbView.Selected > -1 then begin
    thumbnail := ThumbView.ThumbList[ThumbView.Selected];
    InfoFilename.Caption := ExtractfileName(thumbnail.FileName);
    InfoFileSize.Caption := Format('%.1n kB', [thumbnail.FileSize/1024]);
    InfoDateCreated.Caption := IfThen(thumbnail.FileCreated = 0, '-',
      DateTimeToStr(thumbnail.FileCreated));
    InfoDateAccessed.Caption := IfThen(thumbnail.FileAccessed = 0, '-',
      DateTimeToStr(thumbnail.FileAccessed));
    InfoDateModified.Caption := IfThen(thumbnail.FileChanged = 0, '-',
      DateTimeToStr(thumbnail.FileChanged));
  end else begin
    InfoFileSize.Caption := '-';
    InfoDateCreated.Caption := '-';
    InfoDateAccessed.Caption := '-';
    InfoDateModified.Caption := '-';
  end;
  EdSelected.Value := ThumbView.Selected;
end;

end.
