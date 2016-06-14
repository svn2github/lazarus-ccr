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
    Panel3: TPanel;
    Panel4: TPanel;
    RightPanel: TPanel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    PlayButton: TSpeedButton;
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
  private
    FMediaFolder: String;
    FOnPlaySound: TVpPlaySoundEvent;
    function FindFileItem(AFilename: String): TListItem;
  public
    DingPath: string;
    ReturnCode : TVpEditorReturnCode;
    function GetSelectedFileName: String;
    procedure Populate;
    property MediaFolder: String read FMediaFolder write FMediaFolder;
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
  if ShellListview.Items.Count > 0 then begin
    PlayButton.Enabled := true;
    DingPath := GetSelectedFileName;
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
    FOnPlaySound(self, GetSelectedFileName, psmSync);
    PlayButton.Enabled := true;
  end;
end;
{=====}

function TFrmSoundDialog.FindFileItem(AFileName: String): TListItem;
var
  i: Integer;
begin
  AFileName := ExtractFileName(AFileName);
  for i:=0 to ShellListview.Items.Count-1 do
    if ShellListview.Items[i].Caption = AFilename then begin
      Result := ShellListview.Items[i];
      exit;
    end;
  Result := nil;
end;

procedure TFrmSoundDialog.Populate;
begin
  TabSheet1.Caption := RSSelectASound;
  Self.Caption := RSSoundFinder;
  CBDefault.Caption := RSDefaultSound;
  OkBtn.Caption := RSOkBtn;
  CancelBtn.Caption := RSCancelBtn;
  Panel3.Caption := RSNothingToSelectFrom;
  Panel4.Caption := RSNothingToSelectFrom;
  if DingPath = '' then begin
    CBDefault.Checked := true;
    ShellTreeView.Path := FMediaFolder; //ExtractFileDir(ParamStr(0));
  end else
  if FileExists(DingPath) then begin
    ShellTreeview.Path := ExtractFileDir(DingPath);
    ShellListview.Selected := FindFileItem(DingPath);
  end else begin
    ShellTreeView.Path := FMediaFolder; //ExtractFileDir(ParamStr(0));
  end;
  CBDefaultClick(nil);
end;

{=====}

procedure TFrmSoundDialog.CBDefaultClick(Sender: TObject);
begin
  ShellTreeview.Visible := not CBDefault.Checked;
  ShellListview.Visible := not CBDefault.Checked;
  Panel3.Visible := CBDefault.Checked;
  Panel4.Visible := CBDefault.Checked;
  PlayButton.Visible := not CBDefault.Checked;
end;
{=====}

procedure TFrmSoundDialog.FormCreate(Sender: TObject);
begin
  Panel3.Align := alClient;
  Panel4.Align := alClient;
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

function TFrmSoundDialog.GetSelectedFileName: String;
begin
  if ShellListview.ItemFocused <> nil then
    Result := IncludeTrailingPathDelimiter(ShellTreeView.Path) + ShellListview.ItemFocused.Caption
  else
    Result := '';
end;

end.
  
