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

unit JvId3v2MainFormU;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FileUtil, Classes, Controls, Forms, //JvSearchFiles,
  ComCtrls,
  //JvDriveCtrls,
  ExtCtrls, JvId3v2Base, JvId3v2, JvId3v2Types, //JvComponent,
  StdCtrls, ShellCtrls; //, JvListBox, JvCombobox, JvExStdCtrls, JvComponentBase;

type

  { TJvID3v2MainForm }

  TJvID3v2MainForm = class(TForm)
    ImageList1: TImageList;
    ListView1: TListView;
    Splitter1: TSplitter;
    JvID3v21: TJvID3v2;
    Panel1: TPanel;
    ShellTreeView: TShellTreeView;
    procedure ListView1DblClick(Sender: TObject);
    procedure ShellTreeViewChange(Sender: TObject);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewSelectionChanged(Sender: TObject);
  private
    FDir: string;
    procedure FileFoundHandler(AIterator: TFileIterator);
  public
    procedure UpdateItem(Item: TListItem; const AFileName: string);
    procedure ChangeToDir(const ANewDir: string);
  end;

var
  JvID3v2MainForm: TJvID3v2MainForm;

implementation

uses
  LazFileUtils,
  JvId3v2EditFormU;

{$R *.lfm}

procedure TJvID3v2MainForm.ChangeToDir(const ANewDir: string);
var
  lCursor: TCursor;
  searcher: TFileSearcher;
begin
  if ANewDir = FDir then
    Exit;

  FDir := ANewDir;

  lCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  searcher := TFileSearcher.Create;
  try
    searcher.OnFileFound := @FileFoundHandler;
    ListView1.Items.BeginUpdate;
    try
      ListView1.Items.Clear;
      searcher.Search(ANewDir, '*.mp3', false);
    finally
      ListView1.Items.EndUpdate;
    end;
  finally
    searcher.Free;
    Screen.Cursor := lCursor;
  end;
end;

procedure TJvID3v2MainForm.ShellTreeViewChange(Sender: TObject);
begin
  ChangeToDir(ShellTreeView.Path);
end;

procedure TJvID3v2MainForm.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.ImageIndex := 0
  else
    Node.ImageIndex := 1;
end;

procedure TJvID3v2MainForm.ShellTreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.SelectedIndex := 0
  else
    Node.SelectedIndex := 2;
end;

procedure TJvID3v2MainForm.ShellTreeViewSelectionChanged(Sender: TObject);
begin
  ChangeToDir(ShellTreeView.Path);
end;

procedure TJvID3v2MainForm.FileFoundHandler(AIterator: TFileIterator);
var
  Item: TListItem;
  HasTag: Boolean;
  Version: TJvID3Version;
begin
  Item := ListView1.Items.Add;
  GetID3v2Version(AIterator.FileName, HasTag, Version);
  if HasTag then
    case Version of
      iveLowerThan2_2: Item.Caption := '<2.2';
      ive2_2: Item.Caption := '2.2';
      ive2_3: Item.Caption := '2.3';
      ive2_4: Item.Caption := '2.4';
      iveHigherThan2_4: Item.Caption := '>2.4'
    else
      Item.Caption := '?';
    end
  else
    Item.Caption := '-';
  Item.SubItems.Add(ExtractFileName(AIterator.Filename));
end;

procedure TJvID3v2MainForm.ListView1DblClick(Sender: TObject);
var
  lFileName: string;
begin
  if Assigned(ListView1.Selected) then
  begin
    //LFileName := IncludeTrailingPathDelimiter(ShellTreeView.Directory) +
    lFileName := AppendPathDelim(ShellTreeView.Path) + ListView1.Selected.SubItems[0];
    if TJvID3v2EditForm.Execute(lFileName) then
      UpdateItem(ListView1.Selected, lFileName);
  end;
end;

procedure TJvID3v2MainForm.UpdateItem(Item: TListItem; const AFileName: string);
var
  HasTag: Boolean;
  Version: TJvID3Version;
begin
  GetID3v2Version(AFileName, HasTag, Version);
  if HasTag then
    case Version of
      iveLowerThan2_2: Item.Caption := '<2.2';
      ive2_2: Item.Caption := '2.2';
      ive2_3: Item.Caption := '2.3';
      ive2_4: Item.Caption := '2.4';
      iveHigherThan2_4: Item.Caption := '>2.4'
    else
      Item.Caption := '?';
    end
  else
    Item.Caption := '-';

  Item.SubItems[0] := ExtractFileName(AFileName);
end;

end.
