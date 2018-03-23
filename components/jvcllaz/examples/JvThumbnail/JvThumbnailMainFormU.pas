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
  Classes, Controls, Forms, StdCtrls, ExtCtrls, FileCtrl, ComCtrls, Spin, ShellCtrls,
  JvThumbNails, {JvSpecialProgress, }JvThumbViews, JvBaseThumbnail;
  {
  JvListBox, JvDriveCtrls, JvCombobox, JvExControls, JvComponent,
  JvExStdCtrls, JvExForms; }

type

  { TJvThumbnailMainForm }

  TJvThumbnailMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Splitter1: TSplitter;
    Panel1: TPanel;
    LblThumbSize: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TbThumbSize: TTrackBar;
    CbAutoScrolling: TCheckBox;
    CbAutoHandleKeyboard: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    CbSorted: TCheckBox;
    CbMinMemory: TCheckBox;
    Panel2: TPanel;
    ShellTreeView: TShellTreeView;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Panel3: TPanel;
    DirInfoPanel: TPanel;
    BtnStopLoading: TButton;
    BtnEditSelThumb: TButton;
    ThumbView: TJVTHumbview;
    Panel5: TPanel;
    ProgressBar: TProgressBar;
    Bevel1: TBevel;
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ThumbViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ThumbViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ThumbViewScanProgress(Sender: TObject; APosition: Integer;
      var Break: Boolean);
    procedure ThumbViewStartScanning(Sender: TObject; AMax: Integer);
    procedure ThumbViewStopScanning(Sender: TObject);
    procedure BtnStopLoadingClick(Sender: TObject);
    procedure CbAutoScrollingClick(Sender: TObject);
    procedure CbAutoHandleKeyboardClick(Sender: TObject);
    procedure CbMinMemoryClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TbThumbSizeChange(Sender: TObject);
    procedure ThumbViewDblClick(Sender: TObject);
    procedure BtnEditSelThumbClick(Sender: TObject);
    procedure ThumbViewChange(Sender: TObject);
  public
    NewDir: Boolean;
    Scanning: Boolean;
  end;

var
  JvThumbnailMainForm: TJvThumbnailMainForm;

implementation

uses JvThumbnailChildFormU;

{$R *.lfm}

procedure TJvThumbnailMainForm.ThumbViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DirInfoPanel.Caption := ThumbView.SelectedFile;
end;

procedure TJvThumbnailMainForm.ThumbViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DirInfoPanel.Caption := ThumbView.SelectedFile;
end;

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
  Spinedit2.MaxValue := ThumbView.Count - 1;
  newdir := False;
  BtnStopLoading.Enabled := False;
  ProgressBar.Position := 0;
  ProgressBar.Visible := false;
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

procedure TJvThumbnailMainForm.SpinEdit1Change(Sender: TObject);
begin
  if spinedit1.Text <> '' then ThumbView.ThumbGap := spinedit1.Value;
end;

procedure TJvThumbnailMainForm.SpinEdit2Change(Sender: TObject);
begin
  ThumbView.Selected := spinedit2.Value;
end;

procedure TJvThumbnailMainForm.RadioGroup1Click(Sender: TObject);
begin
  ThumbView.AlignView := TViewType(radiogroup1.ItemIndex);
end;

procedure TJvThumbnailMainForm.RadioGroup2Click(Sender: TObject);
begin
  ThumbView.ScrollMode := TscrollMode(radiogroup2.ItemIndex);
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

procedure TJvThumbnailMainForm.FormShow(Sender: TObject);
begin
  CbAutoScrolling.Checked := ThumbView.AutoScrolling;
  CbAutoHandleKeyboard.Checked := ThumbView.AutoHandleKeyb;
  CbSorted.Checked := ThumbView.Sorted;
  CbSorted.Checked := ThumbView.MinMemory;
  spinedit1.Value := ThumbView.ThumbGap;
  spinedit2.MaxValue := 0;
  spinedit1.MinValue := 0;
  radiogroup1.ItemIndex := integer(ThumbView.alignview);
  radiogroup2.ItemIndex := integer(ThumbView.scrollMode);
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
    F.ShelLTreeView.Path := ShellTreeView.Path;
    if Sender is TJvThumbView then
    begin
      F.SetFileName(TJvThumbView(Sender).SelectedFile);
//      F.FileListBox1.FileName := tjvThumbView(Sender).SelectedFile;
    end;
    if Sender is TJvThumbnail then
    begin
      F.SetFileName(TJvThumbnail(Sender).FileName);
  //    F.FileListBox1.FileName := tjvthumbnail(Sender).FileName;
    end;
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
begin
  DirInfoPanel.Caption := ThumbView.SelectedFile;
end;

end.
