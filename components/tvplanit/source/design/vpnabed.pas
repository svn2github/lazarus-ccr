{*********************************************************}
{*                  VPNABED.PAS 1.03                     *}
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

{$I vp.inc}

unit VpNabEd;
  {-property editor for the NavBar}

interface

uses
  lazlogger,

  LCLProc, LCLType, LCLIntf,
  PropEdits, LazarusPackageIntf, FieldsEditor, ComponentEditors,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons,
  VpBase, VpNavBar, Types;

type

  { TVpNavBarEditor }

  TVpNavBarEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

  { TfrmNavBarEd }

  TfrmNavBarEd = class(TForm)
    Bevel1: TBevel;
    Label4: TLabel;
    lbImages: TListBox;
    pnlFoldersAndItems: TPanel;
    pnlItems: TPanel;
    pnlFolders: TPanel;
    lbItems: TListBox;
    lbFolders: TListBox;
    Panel1: TPanel;
    btnItemAdd: TSpeedButton;
    btnItemDelete: TSpeedButton;
    btnItemUp: TSpeedButton;
    btnItemDown: TSpeedButton;
    Panel4: TPanel;
    Label2: TLabel;
    Panel5: TPanel;
    btnFolderAdd: TSpeedButton;
    btnFolderDelete: TSpeedButton;
    btnFolderUp: TSpeedButton;
    btnFolderDown: TSpeedButton;
    Panel6: TPanel;
    Label1: TLabel;

    procedure btnFolderAddClick(Sender: TObject);
    procedure btnFolderDeleteClick(Sender: TObject);
    procedure btnFolderDownClick(Sender: TObject);
    procedure btnFolderUpClick(Sender: TObject);

    procedure btnItemAddClick(Sender: TObject);
    procedure btnItemDeleteClick(Sender: TObject);
    procedure btnItemDownClick(Sender: TObject);
    procedure btnItemUpClick(Sender: TObject);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
//    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure lbFoldersClick(Sender: TObject);
    procedure lbImagesClick(Sender: TObject);
    procedure lbImagesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbItemsMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);

  private
    FBar: TVpNavBar;
    function FindBtnIndex(APersistent: TPersistent): Integer;
    function FindFolderIndex(APersistent: TPersistent): Integer;
    function GetFolderDisplayName(AFolder: TVpNavFolder): String;
    function GetItemDisplayName(AItem: TVpNavBtnItem): String;
    procedure SelectionChanged(AOrderChanged: Boolean = false);
    procedure SelectList(SelList: TPersistentSelectionList);
    procedure UpdateBtnStates;

  private
    FDesigner: TComponentEditorDesigner;
    procedure AddDesignHookHandlers;
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnRefreshPropertyValues;
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent; ADesigner: TComponentEditorDesigner;
      ABar: TVpNavBar); reintroduce;
    destructor Destroy; override;
    procedure PopulateFolderList;
    procedure PopulateImagesList;
    procedure PopulateItemList;
    procedure SetData(ADesigner: TComponentEditorDesigner; ABar: TVpNavBar);
    property Bar: TVpNavBar read FBar;
    property Designer: TComponentEditorDesigner read FDesigner;
  end;


implementation

{$R *.lfm}

uses
  PropEditUtils, IDEWindowIntf, StrUtils, ImgList,
  VpMisc;

const
  ITEMS_MARGIN = 2;
  IMG_MARGIN_HOR = 8;
  IMG_MARGIN_VERT = 4;

var
  EditorForms : TList = nil;

procedure InitFormsList;
begin
  EditorForms := TList.Create;
end;

procedure ReleaseFormsList;
begin
  FreeAndNil(EditorForms);
end;

procedure AddNavBarEditor(Editor: TfrmNavBarEd);
begin
  if Assigned(EditorForms) and (EditorForms.IndexOf(Editor) < 0) then
    EditorForms.Add(Editor);
end;

function FindNavBarEditor(ABar: TVpNavBar): TfrmNavBarEd;
var
  i : Integer;
begin
  if ABar <> nil then
    for i:=0 to EditorForms.Count-1 do begin
      if TfrmNavBarEd(EditorForms[i]).Bar = ABar then
        Exit(TfrmNavBarEd(EditorForms[i]));
    end;
  Result := nil
end;

procedure ReleaseNavBarEditor(Editor: TfrmNavBarEd);
var
  i : Integer;
begin
  if not Assigned(EditorForms) then Exit;
  i := EditorForms.IndexOf(Editor);
  if i >= 0 then EditorForms.Delete(i);
end;


{-------------------------------------------------------------------------------
                            TVpNavBarEditor
-------------------------------------------------------------------------------}

procedure TVpNavBarEditor.ExecuteVerb(Index : Integer);
var
  bar: TVpNavBar;
  editor: TObject;
begin
  if Index = 0 then begin
    bar := Component as TVpNavBar;
    editor := FindNavBarEditor(bar);
    if editor = nil then begin
      editor := TfrmNavBarEd.Create(Application, Designer, bar);
    end else
      TfrmNavBarEd(editor).SetData(Designer, bar);
    if editor <> nil then
      TfrmNavBarEd(editor).ShowOnTop;
  end;
end;

function TVpNavBarEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then
    Result := 'Layout Tool...';
end;

function TVpNavBarEditor.GetVerbCount : Integer;
begin
  Result := 1;
end;


{-------------------------------------------------------------------------------
                               TfrmNavBarEd
-------------------------------------------------------------------------------}

constructor TfrmNavBarEd.Create(AOwner: TComponent;
  ADesigner: TComponentEditorDesigner; ABar: TVpNavBar);
begin
  inherited Create(AOwner);
  Position := poScreenCenter;

  FBar := ABar;
  FDesigner := ADesigner;

  PopulateFolderList;
  AddDesignHookHandlers;
  SelectionChanged;
  UpdateBtnStates;

  AddNavBarEditor(self);
end;

destructor TfrmNavBarEd.Destroy;
begin
  ReleaseNavBarEditor(Self);
  inherited Destroy;
end;

procedure TfrmNavBarEd.AddDesignHookHandlers;
begin
  if GlobalDesignHook <> nil then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
    if FBar <> nil then
    begin
      GlobalDesignHook.AddHandlerGetSelection(OnGetSelection);
      GlobalDesignHook.AddHandlerPersistentAdded(OnPersistentAdded);
      GlobalDesignHook.AddHandlerPersistentDeleting(OnPersistentDeleting);
      GlobalDesignHook.AddHandlerRefreshPropertyValues(OnRefreshPropertyValues);
      GlobalDesignHook.AddHandlerSetSelection(OnSetSelection);
    end;
  end;
end;

procedure TfrmNavBarEd.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  Action := caFree;
end;

procedure TfrmNavBarEd.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TfrmNavBarEd.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook = nil then
    exit;
  if Assigned(FBar) and ((lbFolders.SelCount > 0) or (lbItems.SelCount > 0)) then
    GlobalDesignHook.SelectOnlyThis(FBar);
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TfrmNavBarEd.FormShow(Sender: TObject);
begin
  PopulateImagesList;
  lbFolders.SetFocus;
end;

function TfrmNavBarEd.FindFolderIndex(APersistent: TPersistent): Integer;
begin
  for Result := 0 to lbFolders.Items.Count-1 do
    if TPersistent(lbFolders.Items.Objects[Result]) = APersistent then
      exit;
  Result := -1;
end;

function TfrmNavBarEd.FindBtnIndex(APersistent: TPersistent): Integer;
begin
  for Result := 0 to lbItems.Items.Count-1 do
    if TPersistent(lbItems.Items.Objects[Result]) = APersistent then
      exit;
  Result := -1;
end;

function TfrmNavBarEd.GetFolderDisplayName(AFolder: TVpNavFolder): String;
begin
  Result := IfThen(AFolder.Caption <> '', AFolder.Caption, AFolder.Name);
end;

function TfrmNavBarEd.GetItemDisplayName(AItem: TVpNavBtnItem): String;
begin
  Result := IfThen(AItem.Caption <> '', AItem.Caption, AItem.Name);
end;

procedure TfrmNavBarEd.OnGetSelection(const ASelection: TPersistentSelectionList);
var
  i: Integer;
begin
  if not Assigned(ASelection) then
    exit;
  if ASelection.Count > 0 then
    ASelection.Clear;

  if lbFolders.Focused then begin
    for i:=0 to lbFolders.Items.Count-1 do
      if lbFolders.Selected[i] then
        ASelection.Add(TPersistent(lbFolders.Items.Objects[i]));
  end else
  if lbItems.Focused then begin
    for i:=0 to lbItems.Items.Count-1 do
      if lbItems.Selected[i] then
        ASelection.Add(TPersistent(lbItems.Items.Objects[i]));
  end;
end;

procedure TfrmNavBarEd.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
var
  i: Integer;
begin
  if APersistent = nil then
    DebugLn('OnPersistentAdded: Persistent = nil')
  else
    DebugLn('OnPersistentAdded: Persistent = ' + APersistent.ClassName);

  if not Assigned(APersistent) then
    exit;

  if (APersistent is TVpNavFolder) then
  begin
    PopulateFolderList;
    if Select then begin
      i := FindFolderIndex(APersistent);
      lbFolders.ItemIndex := i;
    end;
  end else
  if (APersistent is TVpNavBtnItem) then
  begin
    PopulateItemList;
    if Select then begin
      i := FindBtnIndex(APersistent);
      lbItems.ItemIndex := i;
    end;
  end;
  UpdateBtnStates;
end;

procedure TfrmNavBarEd.OnPersistentDeleting(APersistent: TPersistent);
var
  i: Integer;
begin
  if APersistent is TVpNavFolder then
  begin
    i := FindFolderIndex(APersistent);
    if i <> -1 then lbFolders.Items.Delete(i);
  end else
  if APersistent is TVpNavBtnItem then
  begin
    i := FindBtnIndex(APersistent);
    if i <> -1 then lbItems.Items.Delete(i);
  end;
  UpdateBtnStates;
end;

procedure TfrmNavBarEd.OnRefreshPropertyValues;
var
  selections: TPersistentSelectionList;
  i: Integer;
begin
  if FBar = nil then
    exit;

  selections := TPersistentSelectionList.Create;
  try
    Assert(Assigned(GlobalDesignHook));
    GlobalDesignHook.GetSelection(selections);
    for i:=0 to selections.Count-1 do begin
      if selections[i] is TVpNavFolder then
        lbFolders.Items[i] := GetFolderDisplayName(TVpNavFolder(selections[i]))
      else if selections[i] is TVpNavBtnItem then
        lbItems.Items[i] := GetItemDisplayName(TVpNavBtnItem(selections[i]))
      else if (selections[i] is TCustomImageList) and (TCustomImageList(selections[i]) = FBar.Images) then
        PopulateImagesList;
    end;
  finally
    selections.Free;
  end;
end;

procedure TfrmNavBarEd.OnSetSelection(const ASelection: TPersistentSelectionList);
var
  i, j: Integer;
begin
  if Assigned(ASelection) and (ASelection.Count > 0) then
  begin
    if ASelection[0] is TVpNavFolder then begin
      //Unselect all
      lbFolders.ClearSelection;
      //select from list
      for i := 0 to ASelection.Count - 1 do begin
        j := FindFolderIndex(ASelection[i]);
        if j <> -1 then lbFolders.Selected[j] := true;
      end;
    end else
    if ASelection[0] is TVpNavBtnItem then
    begin
      // Unselect all
      lbItems.ClearSelection;
      // Select from list
      for i := 0 to ASelection.Count - 1 do begin
        j := FindBtnIndex(ASelection[i]);
        if j <> -1 then lbItems.Selected[j] := true;
      end;
    end;
  end;
end;

procedure TfrmNavBarEd.PopulateFolderList;
var
  I : Integer;
  S : string;
begin
  lbFolders.Clear;
  if FBar = nil then
    exit;

  for I := 0 to FBar.FolderCount-1 do begin
    S := GetFolderDisplayName(FBar.Folders[I]);
    lbFolders.Items.AddObject(S, FBar.Folders[I]);
  end;
end;

procedure TfrmNavbarEd.PopulateImagesList;
var
  i: Integer;
begin
  lbImages.Clear;
  if (FBar = nil) or (FBar.Images = nil) then
    exit;

  for i:=0 to FBar.Images.Count-1 do
    lbImages.Items.Add('');

  lbImages.ItemHeight := FBar.Images.Width + 2*IMG_MARGIN_HOR;
  lbImages.ClientWidth := FBar.Images.Width + 2*IMG_MARGIN_VERT + GetScrollbarWidth;
end;

procedure TfrmNavBarEd.PopulateItemList;
var
  I : Integer;
  S : string;
begin
  lbItems.Clear;
  if (lbFolders.ItemIndex = -1) or (FBar = nil) then
    exit;

  with FBar.Folders[lbFolders.ItemIndex] do
    for I := 0 to ItemCount-1 do begin
      S := GetItemDisplayName(Items[I]);
      lbItems.Items.AddObject(S, Items[i]);
    end;
end;

procedure TfrmNavBarEd.SetData(ADesigner: TComponentEditorDesigner; ABar: TVpNavBar);
var
  i: Integer;
begin
  if FBar <> nil then
    FBar.RemoveFreeNotification(self);

  FBar := ABar;
  FDesigner := ADesigner;

  if FBar <> nil then
    FBar.FreeNotification(self);

  PopulateFolderList;
  PopulateImagesList;

  AddDesignHookHandlers;
  UpdateBtnStates;
end;

procedure TfrmNavBarEd.lbFoldersClick(Sender: TObject);
var
  SelList: TPersistentSelectionList;
  i: Integer;
begin
  if FBar = nil then
    exit;

  PopulateItemList;
  FBar.ActiveFolder := lbFolders.ItemIndex;
  lbImages.ItemIndex := -1;

  SelList := TPersistentSelectionList.Create;
  SelList.ForceUpdate := true;

  for i := 0 to pred(lbFolders.Items.Count) do
    if lbFolders.Selected[i] then begin
      SelList.Add(TPersistent(lbFolders.Items.Objects[i]));
      FBar.FolderCollection.DoOnItemSelected(i);
    end;

  if SelList.Count > 0 then
    SelectList(SelList);

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.lbImagesClick(Sender: TObject);
var
  btn: TVpNavBtnItem;
  res: Integer;
begin
  if (lbImages.ItemIndex <> -1) and (lbItems.ItemIndex <> -1) then begin
    btn := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);
    if btn.IconIndex <> -1 then begin
      res := MessageDlg('Do you want to replace the button icon by this one?',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if res <> mrYes then
        exit;
    end;
    btn.IconIndex := lbImages.ItemIndex;
    lbItems.Invalidate;
    if Assigned(Designer) then
      Designer.Modified;
  end;
end;

procedure TfrmNavBarEd.lbImagesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  x, y: Integer;
begin
  if [odSelected, odFocused] * State <> [] then
    lbImages.Canvas.Brush.Color := clHighlight
  else
    lbImages.Canvas.Brush.Color := clWindow;
  lbImages.Canvas.FillRect(ARect);

  if (FBar = nil) or (FBar.Images = nil) then
    exit;

  x := (ARect.Left + ARect.Right - FBar.Images.Width) div 2;
  y := (ARect.Top + ARect.Bottom - FBar.Images.Height) div 2;
  FBar.Images.Draw(lbImages.Canvas, x, y, Index);
end;

procedure TfrmNavBarEd.lbItemsMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Unused(Control, Index);
  if (FBar <> nil) and (Bar.Images <> nil) then
    Height := Bar.Images.Height + 2 * ITEMS_MARGIN;
end;

procedure TfrmNavBarEd.lbItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  btn: TVpNavBtnItem;
  lb: TListbox;
  ts: TTextStyle;
  x, y: Integer;
  delta: Integer;
begin
  Unused(State);
  lb := TListBox(Control);
  lb.Canvas.FillRect(Rect);

  if (FBar = nil) or (Index = -1) then
    exit;

  // Draw button image
  delta := ITEMS_MARGIN;
  btn := TVpNavBtnItem(lbItems.Items.Objects[Index]);
  if (FBar.Images <> nil) and (btn <> nil) and
     (btn.IconIndex > -1) and (btn.IconIndex < Bar.Images.Count) then
  begin
    FBar.Images.Draw(
      lb.Canvas,
      Rect.Right - Bar.Images.Width - delta,
      (Rect.Top + Rect.Bottom - Bar.Images.Height) div 2,
      btn.IconIndex
    );
  end;

  // Draw text
  ts := lb.Canvas.TextStyle;
  ts.Alignment := taLeftJustify;
  ts.Layout := tlCenter;
  ts.EndEllipsis := true;
  ts.Singleline := true;
  ts.Wordbreak := false;
  x := Rect.Left + 2;
  y := (Rect.Top + Rect.Bottom - lb.Canvas.TextHeight('Tg')) div 2;
  lb.Canvas.TextRect(Rect, x, y, lb.Items[Index]);
end;

procedure TfrmNavBarEd.lbItemsClick(Sender: TObject);
var
  SelList: TPersistentSelectionList;
  i: Integer;
  btn: TVpNavBtnItem;
begin
  if  (FBar <> nil) and (FBar.ActiveFolder <> -1) and (lbItems.ItemIndex <> -1) then
  begin
    btn := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);
    lbImages.ItemIndex := btn.IconIndex;

    SelList := TPersistentSelectionList.Create;
    SelList.ForceUpdate := true;
    for i:=0 to lbItems.Items.Count-1 do
      if lbItems.Selected[i] then
      begin
        SelList.Add(TPersistent(lbItems.Items.Objects[i]));
        FBar.Folders[FBar.ActiveFolder].ItemCollection.DoOnItemSelected(I);
      end;
    if SelList.Count > 0 then
      SelectList(SelList);
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnItemUpClick(Sender: TObject);
var
  SaveItemIndex : Integer;
  Item: TVpNavBtnItem;
begin
  if (lbItems.ItemIndex > 0) then begin
    SaveItemIndex := lbItems.ItemIndex;
    Item := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);
    if Item.Index > 0 then
      Item.Index := Item.Index - 1;

    PopulateItemList;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(Item);
      Designer.Modified;
    end;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnItemDownClick(Sender: TObject);
var
  Item: TVpNavBtnItem;
begin
  if (lbItems.ItemIndex > -1) then begin
    Item := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);

    if Item.Index < Pred(lbItems.Items.Count) then
      Item.Index := Item.Index + 1;

    PopulateItemList;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(Item);
      Designer.Modified;
    end;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnFolderUpClick(Sender: TObject);
var
  SaveItemIndex : Integer;
  Folder: TVpNavFolder;
begin
  if (lbFolders.ItemIndex > 0) then begin
    SaveItemIndex := lbFolders.ItemIndex;
    Folder := TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]);

    if Folder.Index > 0 then
      Folder.Index := Folder.Index - 1;

    PopulateFolderList;
    lbFolders.ItemIndex := SaveItemIndex - 1;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(folder);
      Designer.Modified;
    end;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnFolderDownClick(Sender: TObject);
var
  Folder: TVpNavFolder;
begin
  if (lbFolders.ItemIndex > -1) then begin
    Folder := TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]);

    if Folder.Index < pred(lbFolders.Items.Count) then
      Folder.Index := Folder.Index + 1;

    PopulateFolderList;
    lbFolders.ItemIndex := Folder.Index;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(folder);
      Designer.Modified;
    end;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnItemDeleteClick(Sender: TObject);
begin
  if (lbItems.ItemIndex <> -1) then begin
    TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]).Free;
    lbItems.ItemIndex := -1;
    lbImages.ItemIndex := -1;
    PopulateItemList;
    if Assigned(Designer) then
      Designer.Modified;
    UpdateBtnStates;
  end;
end;

procedure TfrmNavBarEd.btnFolderDeleteClick(Sender: TObject);
begin
  if (lbFolders.ItemIndex <> -1) and (FBar <> nil) then begin
    TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]).Free;
    lbFolders.ItemIndex := -1;
    FBar.Activefolder := -1;
    lbImages.ItemIndex := -1;
    PopulateFolderList;
    PopulateItemList;
    if Assigned(Designer) then
      Designer.Modified;
    UpdateBtnStates;
  end;
end;

procedure TfrmNavBarEd.btnFolderAddClick(Sender: TObject);
var
  folder: TVpNavFolder;
begin
  if FBar = nil then
    exit;
  folder := TVpNavFolder(FBar.FolderCollection.Add);
  GlobalDesignHook.PersistentAdded(folder, true);
  lbFoldersClick(self);
end;

procedure TfrmNavBarEd.btnItemAddClick(Sender: TObject);
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
begin
  if (lbFolders.ItemIndex <> -1) then begin
    folder := TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]);
    item := TVpNavBtnItem(folder.ItemCollection.Add);
    GlobalDesignHook.PersistentAdded(item, true);
  end;
  UpdateBtnStates;
end;

procedure TfrmNavBarEd.SelectionChanged(AOrderChanged: Boolean = false);
var
  SelList: TPersistentSelectionList;
begin
  {
  if (FUpdateSelectionCount>0) or (GlobalDesignHook=nil) then
    exit;
  }
  GlobalDesignHook.RemoveHandlerSetSelection(OnSetSelection);
  try
    SelList := TPersistentSelectionList.Create;
    SelList.ForceUpdate := AOrderChanged;
    try
      OnGetSelection(SelList);
      FDesigner.PropertyEditorHook.SetSelection(SelList) ;
    finally
      SelList.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(OnSetSelection);
  end;
end;

procedure TfrmNavBarEd.Selectlist(SelList: TPersistentSelectionList);
begin
  if (GlobalDesignHook <> nil) and (FBar <> nil) then
  begin
    GlobalDesignHook.SetSelection(SelList);
    GlobalDesignHook.LookupRoot := GetLookupRootForComponent(FBar);
  end;
  SelList.Free;
end;

procedure TfrmNavBarEd.UpdateBtnStates;
var
  canChangeFolders: Boolean;
  canChangeItems: Boolean;
begin
  canChangeFolders := (FBar <> nil);
  canChangeItems := (FBar <> nil) and (FBar.ActiveFolder <> -1) and
    not FBar.Folders[FBar.ActiveFolder].ItemCollection.ReadOnly;

  btnFolderAdd.Enabled := canChangeFolders;
  btnFolderDelete.Enabled := canChangeFolders and (lbFolders.ItemIndex > -1);
  btnFolderUp.Enabled := canChangeFolders and (lbFolders.ItemIndex > 0);
  btnFolderDown.Enabled := canChangeFolders and (lbFolders.ItemIndex < lbFolders.Items.Count-1);

  btnItemAdd.Enabled := canChangeItems;
  btnItemDelete.Enabled := canChangeItems and (lbItems.ItemIndex > -1);
  btnItemUp.Enabled := canChangeItems and (lbItems.ItemIndex > 0);
  btnItemDown.Enabled := canChangeItems and (lbItems.ItemIndex < lbItems.Items.Count-1);
end;

procedure TfrmNavBarEd.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FBar then FBar := nil;
  end;
end;

initialization
  InitFormsList;

finalization
  ReleaseFormsList;

end.
  
