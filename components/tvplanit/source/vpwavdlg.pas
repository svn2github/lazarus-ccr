{*********************************************************}
{*                   VPWAVDLG.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit VpWavDlg;

{$I vp.inc}

interface

{$WARNINGS OFF} {Some of this stuff in here isn't platform friendly}

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf, LResources,
  {$ELSE}
  Windows,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, StdCtrls, ExtCtrls, Buttons, VpBase, ComCtrls, ShellCtrls;

type

  { TFrmSoundDialog }

  TFrmSoundDialog = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RightPanel: TPanel;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    PlayButton: TSpeedButton;
//    DriveComboBox1: TDriveComboBox;
//    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    CBDefault: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure FileListBox1Change(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure CBDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FOnPlaySound: TVpPlaySoundEvent;
  public
    DingPath: string;
    ReturnCode : TVpEditorReturnCode;
    procedure Populate;
    property OnPlaySound: TVpPlaySoundEvent read FOnPlaySound write FOnPlaySound;
  end;


implementation

uses
  VpSR;

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

procedure TFrmSoundDialog.FileListBox1Change(Sender: TObject);
begin
  if FileListBox1.Items.Count > 0 then begin
    PlayButton.Enabled := true;
    DingPath := FileListBox1.FileName;
  end else begin
   PlayButton.Enabled := false;
   DingPath := '';
  end;
end;
{=====}

procedure TFrmSoundDialog.PlayButtonClick(Sender: TObject);
begin
  if Assigned(FOnPlaySound) then begin
    PlayButton.Enabled := false;
    FOnPlaySound(self, FileListbox1.FileName, psmSync);
    PlayButton.Enabled := true;
  end;
end;
{=====}

procedure TFrmSoundDialog.Populate;
var
  Drive: char;
begin
  TabSheet1.Caption := RSSelectASound;
  Self.Caption := RSSoundFinder;
  CBDefault.Caption := RSDefaultSound;
  OkBtn.Caption := RSOkBtn;
  CancelBtn.Caption := RSCancelBtn;
  if DingPath = '' then begin
    CBDefault.Checked := true;
    ShellTreeView.Path := ExtractFileDir(ParamStr(0));
//    DirectoryListBox1.Directory := ExtractFileDir(ParamStr(0));
  end else begin
    Drive := UpCase(ExtractFileDrive(DingPath)[1]);
    if FileExists(DingPath) and (Drive in ['A'..'Z']) then begin
      ShellTreeview.Path := ExtractFileDir(DingPath);
      FileListBox1.FileName := DingPath;
    end else begin
      ShellTreeView.Path := ExtractFileDir(ParamStr(0));
    end;
  end;
end;

procedure TFrmSoundDialog.ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  FileListbox1.Directory := ShellTreeView.Path;
end;

{=====}

procedure TFrmSoundDialog.CBDefaultClick(Sender: TObject);
begin
//  DriveComboBox1.Enabled := not CBDefault.Checked;
//  DirectoryListBox1.Enabled := not CBDefault.Checked;
  ShellTreeview.Enabled := not CBDefault.Checked;
  FileListBox1.Enabled := not CBDefault.Checked;
  PlayButton.Enabled := not CBDefault.Checked;
end;
{=====}

procedure TFrmSoundDialog.FormCreate(Sender: TObject);
begin
  ReturnCode := rtAbandon;
end;
{=====}

procedure TFrmSoundDialog.OkBtnClick(Sender: TObject);
begin
  ReturnCode := rtCommit;
  Close;
end;
{=====}

procedure TFrmSoundDialog.CancelBtnClick(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TFrmSoundDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
  
