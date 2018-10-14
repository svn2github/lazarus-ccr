unit JvId3v2EditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  PropEdits, ComponentEditors,
  JvId3v2Types, JvId3v2Base;

type

  TFSDesigner = class;

  { TJvId3FramesEditor }

  TJvId3FramesEditor = class(TForm)
    FrameListBox: TListBox;
    LocalMenu: TPopupMenu;
    NewItem: TMenuItem;
    Separator: TMenuItem;
    DeleteItem: TMenuItem;
    SelectAllItem: TMenuItem;
    procedure DeleteClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewFrameClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
  private
    FFSDesigner: TFSDesigner;
    FController: TJvID3Controller;
    FMinWidth: Integer;
    FMinHeight: Integer;
    procedure RemoveFrames;
    procedure SelectAll;
    procedure SetController(Value: TJvID3Controller);
    procedure UpdateDisplay;
    procedure UpdateCaption;
    procedure UpdateFrameList;
    procedure UpdateSelection;

  protected
//    procedure Activated; override;

  public
    function DoNewFrame: TJvID3Frame;

    property Controller: TJvID3Controller read FController write SetController;

  end;

  TJvID3ControllerEditor = class(TDefaultComponentEditor)
  private
    FDesigner: TComponentEditorDesigner;
  protected
    procedure Commit;
    function CreateFramesEditor(ADesigner: TComponentEditorDesigner;
      AController: TJvID3Controller; out Shared: Boolean): TJvID3FramesEditor;
    procedure RemoveTag;
    procedure ShowFramesEditor(ADesigner: TComponentEditorDesigner;
      AController: TJvID3Controller);
  public
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TFSDesigner = class(TJvID3ControllerDesigner)
  private
    FFramesEditor: TJvID3FramesEditor;
    function GetFrameDescription(const FrameID: TJvID3FrameID): string;
  public
    destructor Destroy; override;
    procedure ID3Event(Event: TJvID3Event; {%H-}Info: Longint); override;
    property FramesEditor: TJvID3FramesEditor read FFramesEditor;
    property FrameDescription[const FrameID: TJvID3FrameID]: string read GetFrameDescription;
  end;

  TJvID3FileInfoEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

var
  JvId3FramesEditor: TJvId3FramesEditor;

implementation

{$R *.lfm}

uses
  Math, IDEWindowIntf,
  JvDsgnConsts, JvId3v2DefineForm;

type
  TJvID3ControllerAccess = class(TJvID3Controller);

const
  cFrameDescriptions: array [TJvID3FrameID] of string = (
    RsfiErrorFrame,
    RsfiPaddingFrame,
    RsfiNoFrame,
    RsfiAudioCrypto,
    RsfiPicture,
    RsfiAudioSeekPoint,
    RsfiComment,
    RsfiCommercial,
    RsfiCryptoReg,
    RsfiEqualization2,
    RsfiEqualization,
    RsfiEventTiming,
    RsfiGeneralObject,
    RsfiGroupingReg,
    RsfiInvolvedPeople,
    RsfiLinkedInfo,
    RsfiCDID,
    RsfiMPEGLookup,
    RsfiOwnership,
    RsfiPrivate,
    RsfiPlayCounter,
    RsfiPopularimeter,
    RsfiPositionsync,
    RsfiBufferSize,
    RsfiVolumeAdj2,
    RsfiVolumeAdj,
    RsfiReverb,
    RsfiSeekFrame,
    RsfiSignature,
    RsfiSyncedLyrics,
    RsfiSyncedTempo,
    RsfiAlbum,
    RsfiBPM,
    RsfiComposer,
    RsfiContentType,
    RsfiCopyright,
    RsfiDate,
    RsfiEncodingTime,
    RsfiPlaylistDelay,
    RsfiOrigReleaseTime,
    RsfiRecordingTime,
    RsfiReleaseTime,
    RsfiTaggingTime,
    RsfiInvolvedPeople2,
    RsfiEncodedBy,
    RsfiLyricist,
    RsfiFileType,
    RsfiTime,
    RsfiContentGroup,
    RsfiTitle,
    RsfiSubTitle,
    RsfiInitialKey,
    RsfiLanguage,
    RsfiSongLen,
    RsfiMusicianCreditList,
    RsfiMediaType,
    RsfiMood,
    RsfiOrigAlbum,
    RsfiOrigFileName,
    RsfiOrigLyricist,
    RsfiOrigArtist,
    RsfiOrigYear,
    RsfiFileOwner,
    RsfiLeadArtist,
    RsfiBand,
    RsfiConductor,
    RsfiMixArtist,
    RsfiPartInSet,
    RsfiProducedNotice,
    RsfiPublisher,
    RsfiTrackNum,
    RsfiRecordingDates,
    RsfiNetRadioStation,
    RsfiNetRadioOwner,
    RsfiSize,
    RsfiAlbumSortOrder,
    RsfiPerformerSortOrder,
    RsfiTitleSortOrder,
    RsfiISRC,
    RsfiEncoderSettings,
    RsfiSetSubTitle,
    RsfiUserText,
    RsfiYear,
    RsfiUniqueFileID,
    RsfiTermsOfUse,
    RsfiUnsyncedLyrics,
    RsfiWWWCommercialInfo,
    RsfiWWWCopyright,
    RsfiWWWAudioFile,
    RsfiWWWArtist,
    RsfiWWWAudioSource,
    RsfiWWWRadioPage,
    RsfiWWWPayment,
    RsfiWWWPublisher,
    RsfiWWWUser,
    RsfiMetaCrypto,
    RsfiMetaCompression
  );
         (*
//function CreateFramesEditor(Designer: IJvFormDesigner;
//  AController: TJvID3Controller; var Shared: Boolean): TJvID3FramesEditor;
function CreateFramesEditor(Designer: TComponentEditorDesigner;
  AController: TJvID3Controller; var Shared: Boolean): TJvID3FramesEditor;
begin
  Shared := True;
  if AController.Designer <> nil then
    Result := (AController.Designer as TFSDesigner).FFramesEditor
  else
  begin
    Result := TJvID3FramesEditor.Create(Application);
    Result.Designer := Designer;
    Result.Controller := AController;
    Shared := False;
  end;
end;
           *)
  (*
//procedure ShowFramesEditor(Designer: IJvFormDesigner; AController: TJvID3Controller);
procedure ShowFramesEditor(Designer: TComponentEditorDesigner;
  AController: TJvID3Controller);
var
  FramesEditor: TJvID3FramesEditor;
  VShared: Boolean;
  Hook: TPropertyEditorHook;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;
  FramesEditor := CreateFramesEditor(Designer, AController, VShared);
  if FramesEditor <> nil then
    FramesEditor.Show;
end;
*)
procedure ShowFileInfo(AController: TJvID3Controller);
const
  cVersion: array [TJvMPEGVersion] of string =
    (RsMPEG25, RsMPEGUnknown, RsMPEG20, RsMPEG10);
  cLayer: array [TJvMPEGLayer] of string =
    (RsLayerUnknown, RsLayer3, RsLayer2, RsLayer1);
  cChannelMode: array [TJvMPEGChannelMode] of string =
    (RsChannelModeStereo, RsChannelModeJointStereo,
     RsChannelModeDualChannel, RsChannelModeMono);
  cEmphasis: array [TJvMPEGEmphasis] of string =
    (RsEmphasisNone, RsEmphasisMicrosec, RsEmphasisUnknown, RsEmphasisCCITTJ17);
  cBool: array [Boolean] of string =
    (RsBoolNo, RsBoolYes);
  cVbr: array [Boolean] of string =
    (RsVbrNo, RsVbrVbr);
var
  Msg: string;
  SavedActive: Boolean;
begin
  SavedActive := AController.Active;
  try
    with TJvID3ControllerAccess(AController) do
    begin
      if FileName = '' then
      begin
        MessageDlg(RsID3Err_NoFileSpecified, mtError, [mbOK], 0);
        Exit;
      end;

      if not FileExists(FileName) then
      begin
        MessageDlg(Format(RSID3Err_FileDoesNotExists, [FileName]),
          mtError, [mbOK], 0);
        Exit;
      end;

      Active := True;

      with FileInfo do
      begin
        if not IsValid then
        begin
          MessageDlg(RSID3Err_NoValidMPEGTag, mtError, [mbOK], 0);
          Exit;
        end;

        Msg := Format(RsIDV2FileInfoFmt, [FileSize, HeaderFoundAt, LengthInSec,
          cVersion[Version], cLayer[Layer], Bitrate, cVbr[IsVbr], FrameCount,
          SamplingRateFrequency, cChannelMode[ChannelMode],
          cBool[mbProtection in Bits], cBool[mbCopyrighted in Bits],
          cBool[mbOriginal in Bits], cEmphasis[Emphasis]]);
      end;
    end;

    { We don't use MessageDlg now, because we want a custom caption }
    with CreateMessageDialog(Msg, mtCustom, [mbOK]) do
    try
      Position := poScreenCenter;
      Caption := RsIDV2FileInfoCaption;
      ShowModal;
    finally
      Free;
    end;
  finally
    AController.Active := SavedActive;
  end;
end;


// ===  TJvId3FramesEditor =====================================================
      (*
procedure TJvID3FramesEditor.Activated;
begin
  Designer.Activate;
  try
    UpdateSelection;
  except
    FrameListBox.Items.Clear;
  end;
end;    *)

procedure TJvID3FramesEditor.DeleteClick(Sender: TObject);
begin
  RemoveFrames;
end;

function TJvID3FramesEditor.DoNewFrame: TJvID3Frame;
var
  DefineFrame: TJvID3DefineDlg;
begin
  Result := nil;
  DefineFrame := TJvID3DefineDlg.Create(Application);
  try
    DefineFrame.FSDesigner := FFSDesigner;
    DefineFrame.Designer := Designer;
    DefineFrame.Controller := Controller;
    if DefineFrame.ShowModal = mrOk then
    begin
      Result := DefineFrame.Frame;
      if Visible then
        UpdateDisplay;
      Designer.Modified;
    end;
  finally
    DefineFrame.Release;
  end;
end;

procedure TJvId3FramesEditor.FormActivate(Sender: TObject);
begin
  (*
  Designer.Activate;
  try
    UpdateSelection;
  except
    FrameListBox.Items.Clear;
  end;
  *)
end;

procedure TJvId3FramesEditor.FormCreate(Sender: TObject);
begin
  FMinWidth := Width;
  FMinHeight := Height;
end;

procedure TJvId3FramesEditor.FormDestroy(Sender: TObject);
begin
  if FFSDesigner <> nil then
  begin
    { Destroy the designer if the editor is destroyed }
    FFSDesigner.FFramesEditor := nil;
    FFSDesigner.Free;
    FFSDesigner := nil;
  end;
end;

procedure TJvID3FramesEditor.NewFrameClick(Sender: TObject);
var
  Selection: TStringList;
  Frame: TJvID3Frame;
begin
  Frame := DoNewFrame;
  if Frame <> nil then
  begin
    (*
    Selection := TStringList.Create;
    try
      Selection.Add(Frame.Name);
    finally
      RestoreSelection(Selection, -1, -1, False);
    end;
    *)
  end;
  FrameListBox.SetFocus;
end;

procedure TJvID3FramesEditor.RemoveFrames;
var
  I, lFocused: Integer;
begin
  try
    FFSDesigner.BeginDesign;
    try
      lFocused := FrameListBox.ItemIndex;
      with FrameListBox do
        for I := Items.Count - 1 downto 0 do
          if Selected[I] then
            TJvID3Frame(Items.Objects[I]).Free;
    finally
      FFSDesigner.EndDesign;
      Designer.Modified;
    end;
  finally
    UpdateDisplay;
  end;
  if lFocused <> -1 then
  begin
    lFocused := Min(lFocused, FrameListBox.Items.Count - 1);
    FrameListBox.ItemIndex := lFocused;
    FrameListBox.Selected[lFocused] := True;
    UpdateSelection;
  end;
  FrameListBox.SetFocus;
end;

procedure TJvID3FramesEditor.SelectAll;
var
  I: Integer;
begin
  with FrameListBox do
    for I := 0 to Items.Count - 1 do
      Selected[I] := True;
end;

procedure TJvID3FramesEditor.SelectAllClick(Sender: TObject);
begin
  SelectAll;
  UpdateSelection;
end;

procedure TJvID3FramesEditor.SetController(Value: TJvID3Controller);
begin
  if FController <> Value then
  begin
    if FController <> nil then
      FreeAndNil(FFSDesigner);
    FController := Value;
    if FController <> nil then
    begin
      FFSDesigner := TFSDesigner.Create(Value);
      FFSDesigner.FFramesEditor := Self;
      UpdateDisplay;
    end
    else
    begin
      if not (csDestroying in ComponentState) then
        Release;
    end;
  end;
end;

procedure TJvID3FramesEditor.UpdateCaption;
const
  cFrameEditor = '%s%s%s';
var
  NewCaption: string;
begin
  if (Controller <> nil) and (Controller.Owner <> nil) then
    NewCaption := Format(cFrameEditor,
      [Controller.Owner.Name, '.', Controller.Name]);
  if Caption <> NewCaption then
    Caption := NewCaption;
end;

procedure TJvID3FramesEditor.UpdateDisplay;
begin
  UpdateFrameList;
  UpdateCaption;
  UpdateSelection;
end;

procedure TJvID3FramesEditor.UpdateFrameList;
var
  ItemIndex, TopIndex: Integer;
  Selection: TStringList;
  EnableList: Boolean;
  I: Integer;
  Frame: TJvID3Frame;
  FrameName: string;
begin
//  SaveSelection(Selection, ItemIndex, TopIndex, True);
  try
    FrameListBox.Clear;
    EnableList := False;
    try
      if Controller = nil then
        Exit;
      for I := 0 to Controller.Frames.Count - 1 do
      begin
        Frame := Controller.Frames[I];
        if not (csDestroying in Frame.ComponentState) then
        begin
          FrameName := string(Frame.FrameName);
          if FrameName = '' then
            FrameName := Format('<%s>', [Controller.Frames[I].Name]);
          FrameName := FrameName + ' - ' + cFrameDescriptions[Frame.FrameID];
          FrameListBox.Items.AddObject(FrameName, Frame);
        end;
      end;

      EnableList := True;
    finally
      FrameListBox.Enabled := EnableList;
    end;
  finally
  //  RestoreSelection(Selection, ItemIndex, TopIndex, True)
  end;
end;

procedure TJvID3FramesEditor.UpdateSelection;
{
var
  I: Integer;
  Frame: TJvID3Frame;
  ComponentList: IDesignerSelections;
  }
begin
  {
  if Active then
  begin
    ComponentList := TDesignerSelections.Create;
    with FrameListBox do
      for I := 0 to Items.Count - 1 do
        if Selected[I] then
        begin
          Frame := TJvID3Frame(Items.Objects[I]);
          if Frame <> nil then
            ComponentList.Add(Frame);
        end;
    if ComponentList.Count = 0 then
      ComponentList.Add(Controller);
    Designer.SetSelections(ComponentList);
  end;
  }
end;


                    (*
function FindFramesEditor(AController: TJvID3Controller): TJvID3FramesEditor;
var
  i : Integer;
begin
  if AController <> nil then
    for i:=0 to EditorForms.Count-1 do begin
      if TActionListEditor(EditorForms[i]).FActionList=AList then
        Exit(TActionListEditor(EditorForms[i]));
    end;
  Result:=nil
end;
                      *)

// === TJvID3ControllerEditor ==================================================

constructor TJvID3ControllerEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner := ADesigner;
end;

procedure TJvID3ControllerEditor.Commit;
begin
  if MessageDlg(RsCommit, mtConfirmation, mbOKCancel, 0) = mrOk then
    TJvID3Controller(Component).Commit;
end;

function TJvID3ControllerEditor.CreateFramesEditor(ADesigner: TComponentEditorDesigner;
  AController: TJvID3Controller; out Shared: Boolean): TJvID3FramesEditor;
begin
  Shared := True;
  if AController.Designer <> nil then
    Result := (AController.Designer as TFSDesigner).FFramesEditor
  else
  begin
    Result := TJvID3FramesEditor.Create(Application);
    Result.Designer := ADesigner;
    Result.Controller := AController;
    Shared := False;
  end;
end;

procedure TJvID3ControllerEditor.Edit;
var
  lController: TJvID3Controller;
  lEditor: TJvId3FramesEditor;
begin
  lController := GetComponent as TJvID3Controller;
  if lController = nil then
    raise Exception.Create('TJvID3ControllerEditor.Edit lController=nil');

  lEditor := TJvId3FramesEditor.Create(Application);
  with lEditor do begin
    Designer := Self.FDesigner;
    Controller := lController;
  end;
  SetPopupModeParentForPropertyEditor(lEditor);
  lEditor.ShowOnTop;
  (*
  lController :AEditor:=FindActionEditor(AActionList);
  if not Assigned(AEditor) then begin
    AEditor:=TActionListEditor.Create(Application);
    with AEditor do begin
      lstActionName.ItemIndex := -1;
      Designer := Self.FDesigner;
      SetActionList(AActionList);
    end;
  end;
  SetPopupModeParentForPropertyEditor(AEditor);
  AEditor.ShowOnTop;
  *)
end;

procedure TJvID3ControllerEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowFramesEditor(Designer, TJvID3Controller(Component));
    1: RemoveTag;
    2: ShowFileInfo(TJvID3Controller(Component));
    3: Commit;
  end;
end;

function TJvID3ControllerEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := RSID3FrameEditorTag;
    1: Result := RSID3RemoveTag;
    2: Result := RSID3FileInfoTag;
    3: Result := RSID3CommitTag;
  end;
end;

function TJvID3ControllerEditor.GetVerbCount: Integer;
begin
  Result := 3;
  with TJvID3Controller(Component) do
    if Active and Modified then
      Inc(Result);
end;

procedure TJvID3ControllerEditor.RemoveTag;
begin
  with TJvID3Controller(Component) do
  begin
    if FileName = '' then
    begin
      MessageDlg(RsID3Err_NoFileSpecified, mtError, [mbOK], 0);
      Exit;
    end;

    if not FileExists(FileName) then
    begin
      MessageDlg(Format(RSID3Err_FileDoesNotExists, [FileName]), mtError, [mbOK], 0);
      Exit;
    end;

    if MessageDlg(RSID3RemoveTagConfirmation, mtConfirmation, mbOKCancel, 0) = mrOk then
      Erase;
  end;
end;

procedure TJvID3ControllerEditor.ShowFramesEditor(ADesigner: TComponentEditorDesigner;
  AController: TJvID3Controller);
var
  FramesEditor: TJvID3FramesEditor;
  VShared: Boolean;
  Hook: TPropertyEditorHook;
begin
  Hook := nil;
  if not GetHook(Hook) then exit;
  FramesEditor := CreateFramesEditor(ADesigner, AController, VShared);
  if FramesEditor <> nil then
    FramesEditor.Show;
end;


//=== TFSDesigner  =============================================================

destructor TFSDesigner.Destroy;
var
  F: TJvID3FramesEditor;
begin
  if FFramesEditor <> nil then
  begin
    F := FFramesEditor;
    FFramesEditor := nil;
    F.FFSDesigner := nil;
    { (rb) DSDesign.pas uses Release, but that gave problems, with recompiling }
    F.Free;
  end;
  inherited Destroy;
end;

function TFSDesigner.GetFrameDescription(const FrameID: TJvID3FrameID): string;
begin
  Result := cFrameDescriptions[FrameID];
end;

procedure TFSDesigner.ID3Event(Event: TJvID3Event; Info: Longint);
begin
  if Event in [ideFrameListChange, ideID3Change] then
    FFramesEditor.UpdateFrameList;
end;


// === TJvId3FileInfoEditor ===================================================

procedure TJvID3FileInfoEditor.Edit;
var
  P: TPersistent;
begin
  P := TPersistent(GetComponent(0));
  if P is TJvID3Controller then
    ShowFileInfo(TJvID3Controller(P));
end;

function TJvID3FileInfoEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.

