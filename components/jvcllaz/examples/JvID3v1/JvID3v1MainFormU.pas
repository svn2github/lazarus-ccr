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

unit JvID3v1MainFormU;

{$mode objfpc}{$H+}

interface

uses
  //Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, //JvComponent,
  StdCtrls, //Mask, //JvToolEdit,
  JvId3v1, ComCtrls, //ToolWin,
  ActnList, //ImgList,
  EditBtn, Spin;
{
  JvBaseDlg, JvTipOfDay, JvBalloonHint, JvMaskEdit, JvSpin, JvJVCLAboutForm,
  JvExMask;
}
type

  { TJvID3v1MainForm }

  TJvID3v1MainForm = class(TForm)
    JvFilenameEdit1: TFilenameEdit;
    edtTitle: TEdit;
    JvId3v11: TJvId3v1;
    edtAlbum: TEdit;
    edtArtist: TEdit;
    edtYear: TEdit;
    edtComment: TEdit;
    cmbGenre: TComboBox;
    lblArtist: TLabel;
    lblAlbum: TLabel;
    lblYear: TLabel;
    lblComment: TLabel;
    lblGenre: TLabel;
    ActionList1: TActionList;
    actSave: TAction;
    actRefresh: TAction;
    actErase: TAction;
    actExit: TAction;
    actOnTop: TAction;
    actAbout: TAction;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    lblHasTag: TLabel;
//    JvTipOfDay1: TJvTipOfDay;
//    JvJVCLAboutComponent1: TJvJVCLAboutComponent;
//    JvBalloonHint1: TJvBalloonHint;
    sedTrack: TSpinEdit;
    lblTitle: TLabel;
    lblTrack: TLabel;
    procedure actAboutExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actEraseExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actOnTopExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure JvFilenameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure JvFilenameEdit1KeyPress(Sender: TObject; var Key: Char);
  public
    procedure ChangeFileNameTo(S: string);
    procedure FillGenres(Strings: TStrings);
    procedure UpdateCtrls;
    procedure UpdateCaption;
  end;

var
  JvID3v1MainForm: TJvID3v1MainForm;

implementation

uses
  JvId3v2Types;

{$R *.lfm}

procedure TJvID3v1MainForm.ChangeFileNameTo(S: string);
begin
  JvFilenameEdit1.Text := S;
  JvFilenameEdit1.Hint := S;
  JvId3v11.FileName := S;
  JvId3v11.Open;
  UpdateCtrls;
  UpdateCaption;
  FocusControl(edtTitle);
end;

procedure TJvID3v1MainForm.FillGenres(Strings: TStrings);
begin
  ID3_Genres(Strings,true);
end;

procedure TJvID3v1MainForm.actSaveExecute(Sender: TObject);
begin
  if JvId3v11.FileName = '' then
//    JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'First select a mp3 file', ikError, 'Error', 5000)
  else
  begin
    JvId3v11.SongName := edtTitle.Text;
    JvId3v11.Artist := edtArtist.Text;
    JvId3v11.Album := edtAlbum.Text;
    JvId3v11.Year := edtYear.Text;
    JvId3v11.GenreAsString := cmbGenre.Text;
    JvId3v11.Comment := edtComment.Text;
    JvId3v11.AlbumTrack := sedTrack.Value; //AsInteger;

    if JvId3v11.Commit then
      UpdateCaption
    else
    {
      JvBalloonHint1.ActivateHint(ToolButton2, 'Could not save changes.'#13+
        'The file is probably opened by another application.', ikError, 'Error')}
    ;
  end;
end;

procedure TJvID3v1MainForm.actEraseExecute(Sender: TObject);
begin
  if JvId3v11.FileName = '' then
    //JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'First select a mp3 file', ikError, 'Error', 5000)
  else
  begin
    JvId3v11.Erase;
    UpdateCtrls;
    UpdateCaption;
  end;
end;

procedure TJvID3v1MainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TJvID3v1MainForm.actRefreshExecute(Sender: TObject);
begin
  if JvId3v11.FileName = '' then
    //JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'First select a mp3 file', ikError, 'Error', 5000)
  else
    ChangeFileNameTo(JvId3v11.FileName);
end;

procedure TJvID3v1MainForm.actOnTopExecute(Sender: TObject);
const
  CStyle: array[Boolean] of TFormStyle = (fsNormal, fsStayOnTop);
begin
  //JvDragDrop1.AcceptDrag := False;
  actOnTop.Checked := not actOnTop.Checked;
  FormStyle := CStyle[actOnTop.Checked];
  //JvDragDrop1.AcceptDrag := True;
end;

procedure TJvID3v1MainForm.FormCreate(Sender: TObject);
begin
  { This is put in the OnCreate and not in the OnShow event, because we change
    Form1.FormStyle at run-time that will trigger the OnShow event }
  FillGenres(cmbGenre.Items);
  UpdateCaption;
end;

procedure TJvID3v1MainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if Length(FileNames) > 0 then
    ChangeFileNameTo(FileNames[0]);
end;

procedure TJvID3v1MainForm.JvFilenameEdit1AcceptFileName(Sender: TObject;
  var Value: String);
begin
  ChangeFileNameTo(Value);
end;

procedure TJvID3v1MainForm.JvFilenameEdit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if JvFilenameEdit1.Text = '' then
      //JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'Empty strings are no file names', ikError, 'Error', 5000)
    else
      ChangeFileNameTo(JvFilenameEdit1.FileName);
  end;
end;

procedure TJvID3v1MainForm.UpdateCaption;
const
  CHasTagStr: array[Boolean] of string = ('No tag', 'Has Tag');
  CHasTagColor: array[Boolean] of TColor = (clRed, clBlack);
var
  HasTag: Boolean;
begin
  if JvId3v11.FileName > '' then
  begin
    { Store TagPresent in variabele to prevent double checks whether the file
      has a tag }
    HasTag := JvId3v11.HasTag;
    lblHasTag.Font.Color := CHasTagColor[HasTag];
    lblHasTag.Caption := CHasTagStr[HasTag];
  end
  else
    lblHasTag.Caption := '';
end;

procedure TJvID3v1MainForm.UpdateCtrls;
begin
  edtTitle.Text := JvId3v11.SongName;
  edtAlbum.Text := JvId3v11.Album;
  edtArtist.Text := JvId3v11.Artist;
  edtYear.Text := JvId3v11.Year;
  edtComment.Text := JvId3v11.Comment;
  sedTrack.Value := JvId3v11.AlbumTrack;
  cmbGenre.ItemIndex := cmbGenre.Items.IndexOfObject(TObject(PtrInt(JvId3v11.Genre)));
end;

procedure TJvID3v1MainForm.actAboutExecute(Sender: TObject);
begin
  //JvJVCLAboutComponent1.Execute;
end;

end.
