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
  LCLProc, LCLType, LCLIntf, LCLVersion,
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
    lblImages: TLabel;
    lbImages: TListBox;
    pnlFoldersAndItems: TPanel;
    pnlItems: TPanel;
    pnlFolders: TPanel;
    lbItems: TListBox;
    lbFolders: TListBox;
    pnlItemBtns: TPanel;
    btnItemAdd: TSpeedButton;
    btnItemDelete: TSpeedButton;
    btnItemUp: TSpeedButton;
    btnItemDown: TSpeedButton;
    lblItems: TLabel;
    pnlFolderBtns: TPanel;
    btnFolderAdd: TSpeedButton;
    btnFolderDelete: TSpeedButton;
    btnFolderUp: TSpeedButton;
    btnFolderDown: TSpeedButton;
    lblFolders: TLabel;
    btnUseImage: TSpeedButton;
    Splitter1: TSplitter;

    procedure btnFolderAddClick(Sender: TObject);
    procedure btnFolderDeleteClick(Sender: TObject);
    procedure btnFolderDownClick(Sender: TObject);
    procedure btnFolderUpClick(Sender: TObject);
    procedure btnItemAddClick(Sender: TObject);
    procedure btnItemDeleteClick(Sender: TObject);
    procedure btnItemDownClick(Sender: TObject);
    procedure btnItemUpClick(Sender: TObject);
    procedure btnUseImageClick(Sender: TObject);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure lbFoldersClick(Sender: TObject);
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
    procedure UpdateBtnStates;

  private
    FDesigner: TComponentEditorDesigner;
    procedure AddDesignHookHandlers;
    procedure Modified;
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnRefreshPropertyValues;
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);

  protected
    {$IF LCL_FullVersion >= 1080000}
     procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
       const AXProportion, AYProportion: Double); override;
    {$ENDIF}
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
  VpConst, VpMisc;

const
  ITEMS_MARGIN = 2;
  IMG_MARGIN_HOR = 8;
  IMG_MARGIN_VERT = 4;

var
  vITEMS_MARGIN: Integer = ITEMS_MARGIN;
  vIMG_MARGIN_HOR: Integer = IMG_MARGIN_HOR;
  vIMG_MARGIN_VERT: Integer = IMG_MARGIN_VERT;

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

{$IF LCL_FullVersion >= 1080000}
 procedure TFrmNavBarEd.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
   const AXProportion, AYProportion: Double);
 begin
   inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
   if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
   begin
     DisableAutoSizing;
     try
       vITEMS_MARGIN := round(ITEMS_MARGIN * AXProportion);
       vIMG_MARGIN_HOR := round(IMG_MARGIN_HOR * AXProportion);
       vIMG_MARGIN_VERT := round(IMG_MARGIN_VERT * AYProportion);
     finally
       EnableAutoSizing;
     end;
   end;
 end;

{$ENDIF}

procedure TfrmNavBarEd.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  Action := caFree;
end;

procedure TfrmNavBarEd.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self);

 {$IFDEF NEW_ICONS}
  LoadGlyphFromRCDATA(btnUseImage.Glyph, 'VpLArrow', 16, 24, 32);
 {$ELSE}
  vtnUseImage.Glyph.LoadFromResourceName(HINSTANCE, 'VPLEFTARROW');
 {$ENDIF}

 {$IF VP_LCL_SCALING = 0}
  btnUseImage.Width := ScaleX(btnUseImage.Width, DesignTimeDPI);
  btnUseImage.Height := ScaleY(btnUseImage.Width, DesignTimeDPI);
 {$ENDIF}
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
var
  delta: Integer;
  h: Integer = 0;
begin
  PopulateImagesList;
  delta := lbFolders.Left;
  pnlFolders.Constraints.MinWidth := btnFolderDown.Left + btnFolderDown.Width + delta;
  pnlItems.Constraints.MinWidth := btnItemDown.Left + btnItemDown.Width + lbImages.Width + btnUseImage.Width + delta;
  Constraints.MinWidth := pnlFolders.Constraints.MinWidth + pnlItems.Constraints.MinWidth;
  lbItemsMeasureItem(nil, 0, h);
  Constraints.MinHeight := lbItems.Top + h + pnlItemBtns.Height + 3*delta;
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

{ Is called when the selection has been  changed within the Component Editor
  in order pass the new selection to the OI designer }
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
  selList: TPersistentSelectionList;
begin
  if not Assigned(APersistent) or (FBar = nil) or (GlobalDesignHook = nil) then
    exit;

  if (APersistent is TVpNavFolder) then
  begin
    PopulateFolderList;
    if Select then begin
      i := FindFolderIndex(APersistent);
      lbFolders.ItemIndex := i;

      selList := TPersistentSelectionList.Create;
      try
        selList.ForceUpdate := true;
        for i := 0 to pred(lbFolders.Items.Count) do
          if lbFolders.Selected[i] then begin
            SelList.Add(TPersistent(lbFolders.Items.Objects[i]));
            FBar.FolderCollection.DoOnItemSelected(i);
          end;

        if (SelList.Count > 0) then begin
          GlobalDesignHook.SetSelection(SelList);
          GlobalDesignHook.LookupRoot := GetLookupRootForComponent(FBar);
        end;
      finally
        selList.Free;
      end;
    end;
  end else
  if (APersistent is TVpNavBtnItem) then
  begin
    PopulateItemList;
    if Select then begin
      i := FindBtnIndex(APersistent);
      lbItems.ItemIndex := i;

      selList := TPersistentSelectionList.Create;
      try
        selList.ForceUpdate := true;
        for i:=0 to lbItems.Items.Count-1 do
          if lbItems.Selected[i] then
          begin
            selList.Add(TPersistent(lbItems.Items.Objects[i]));
            FBar.Folders[FBar.ActiveFolder].ItemCollection.DoOnItemSelected(I);
          end;
        if selList.Count > 0 then begin
          GlobalDesignHook.SetSelection(selList);
          GlobalDesignHook.LookupRoot := GetLookupRootForComponent(FBar);
        end;
      finally
        selList.Free;
      end;
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
    if i <> -1 then begin
      lbFolders.Items.Delete(i);
      lbItems.Clear;
    end;
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
  i, idx: Integer;
begin
  if (FBar = nil) or (GlobalDesignHook = nil) then
    exit;

  selections := TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(selections);
    for i:=0 to selections.Count-1 do begin
      if selections[i] is TVpNavFolder then begin
        idx := FindFolderIndex(TVpNavFolder(selections[i]));
        lbFolders.Items[idx] := GetFolderDisplayName(TVpNavFolder(selections[i]))
      end else
      if selections[i] is TVpNavBtnItem then begin
        idx := FindBtnIndex(TVpNavBtnItem(selections[i]));
        lbItems.Items[idx] := GetItemDisplayName(TVpNavBtnItem(selections[i]))
      end else
      if (selections[i] is TCustomImageList) and (TCustomImageList(selections[i]) = FBar.Images) then
        PopulateImagesList;
    end;
  finally
    selections.Free;
  end;
end;

{ Is called when a new selection has been made in the object tree of the OI.
  The controls in the component editor must be updated to the new selection. }
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
        if j <> -1 then begin
          lbFolders.Selected[j] := true;
          PopulateItemList;
          FBar.ActiveFolder := lbFolders.ItemIndex;
          lbImages.ItemIndex := -1;
        end;
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

  lbImages.ItemHeight := FBar.Images.Width + 2*vIMG_MARGIN_HOR;
  lbImages.ClientWidth := FBar.Images.Width + 2*vIMG_MARGIN_VERT + GetScrollbarWidth;
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
begin
  if FBar <> nil then
    FBar.RemoveFreeNotification(self);

  FBar := ABar;
  FDesigner := ADesigner;

  if FBar <> nil then begin
    FBar.FreeNotification(self);
    if FBar.Images = nil then
      lbItems.Style := lbStandard
    else begin
      lbItems.ItemHeight := FBar.Images.Height + 2 * vITEMS_MARGIN;
      lbItems.Style := lbOwnerDrawFixed;
    end;
  end;

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
  if (FBar = nil) or (GlobalDesignHook = nil) then
    exit;

  PopulateItemList;
  FBar.ActiveFolder := lbFolders.ItemIndex;
  lbImages.ItemIndex := -1;

  SelList := TPersistentSelectionList.Create;
  try
    SelList.ForceUpdate := true;

    for i := 0 to pred(lbFolders.Items.Count) do
      if lbFolders.Selected[i] then begin
        SelList.Add(TPersistent(lbFolders.Items.Objects[i]));
        FBar.FolderCollection.DoOnItemSelected(i);
      end;

    if SelList.Count > 0 then begin
      GlobalDesignHook.SetSelection(SelList);
      GlobalDesignHook.LookupRoot := GetLookupRootForComponent(FBar);
    end;
  finally
    SelList.Free;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.lbImagesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  x, y: Integer;
begin
  Unused(Control);

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
    Height := Bar.Images.Height + 2 * vITEMS_MARGIN
  else
    Height := lbItems.ItemHeight;
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
  delta := vITEMS_MARGIN;
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
    dec(Rect.Right, Bar.Images.Width + 2*delta);
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
  lb.Canvas.TextRect(Rect, x, y, lb.Items[Index], ts);
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
    try
      SelList.ForceUpdate := true;
      for i:=0 to lbItems.Items.Count-1 do
        if lbItems.Selected[i] then
        begin
          SelList.Add(TPersistent(lbItems.Items.Objects[i]));
          FBar.Folders[FBar.ActiveFolder].ItemCollection.DoOnItemSelected(I);
        end;
      if SelList.Count > 0 then begin
        GlobalDesignHook.SetSelection(SelList);
        GlobalDesignHook.LookupRoot := GetLookupRootForComponent(FBar);
      end;
    finally
      SelList.Free;
    end;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnItemUpClick(Sender: TObject);
var
  //SaveItemIndex : Integer;
  Item: TVpNavBtnItem;
begin
  if (lbItems.ItemIndex > 0) then begin
    //SaveItemIndex := lbItems.ItemIndex;
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
    Modified;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnFolderDownClick(Sender: TObject);
var
  folder: TVpNavFolder;
begin
  if (lbFolders.ItemIndex > -1) then begin
    folder := TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]);

    if folder.Index < pred(lbFolders.Items.Count) then
      folder.Index := folder.Index + 1;

    PopulateFolderList;
    lbFolders.ItemIndex := folder.Index;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(folder);
      Designer.Modified;
    end;
    Modified;
  end;

  UpdateBtnStates;
end;

procedure TfrmNavBarEd.btnFolderDeleteClick(Sender: TObject);
var
  folder: TVpNavFolder;
  s: String;
begin
  if (lbFolders.ItemIndex <> -1) and (FBar <> nil) then begin

    folder := TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]);
    s := folder.Caption;
    if MessageDlg(Format('Do you really want to delete folder "%s"?', [s]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes
    then
      exit;

    FDesigner.DeleteSelection;
    lbFolders.ItemIndex := -1;
    FBar.ActiveFolder := -1;
    lbImages.ItemIndex := -1;
    PopulateItemList;
  end;
end;

procedure TfrmNavBarEd.btnFolderAddClick(Sender: TObject);
var
  folder: TVpNavFolder;
begin
  if (FBar = nil) or (GlobalDesignHook = nil) then
    exit;
  folder := TVpNavFolder(FBar.FolderCollection.Add);
  GlobalDesignHook.PersistentAdded(folder, true);
end;

procedure TfrmNavBarEd.btnItemAddClick(Sender: TObject);
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
begin
  if (lbFolders.ItemIndex = -1) or (GlobalDesignHook = nil) then
    exit;
  folder := TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]);
  item := TVpNavBtnItem(folder.ItemCollection.Add);
  GlobalDesignHook.PersistentAdded(item, true);
end;

procedure TfrmNavBarEd.btnItemDeleteClick(Sender: TObject);
var
  btn: TVpNavBtnItem;
  s: String;
begin
  if (lbItems.ItemIndex <> -1) then begin
    btn := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);
    s := btn.Caption;
    if MessageDlg(Format('Do you really want to delete item "%s"?', [s]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes
    then
      exit;

    FDesigner.DeleteSelection;
  end;
end;

procedure TfrmNavBarEd.btnUseImageClick(Sender: TObject);
var
  btn: TVpNavBtnItem;
  res: Integer;
begin
  if (lbImages.ItemIndex <> -1) and (lbItems.ItemIndex <> -1) then begin
    btn := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);
    if btn.IconIndex = lbImages.itemIndex then
      exit;
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

procedure TfrmNavBarEd.Modified;
begin
  FDesigner.PropertyEditorHook.RefreshPropertyValues;
  FDesigner.Modified;

  if GlobalDesignHook <> nil then
    GlobalDesignHook.Modified(self);

end;

procedure TfrmNavBarEd.SelectionChanged(AOrderChanged: Boolean = false);
var
  SelList: TPersistentSelectionList;
begin
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
  
