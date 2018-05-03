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
  VpBase, VpNavBar;

type

  { TVpNavBarEditor }

  TVpNavBarEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

  { TfrmNavBarEd }

  TfrmNavBarEd = class(TForm)
    pnlImageView: TPanel;
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
    pnlImages: TPanel;
    Panel8: TPanel;
    Label3: TLabel;
    sbImages: TScrollBox;

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
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbItemsMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);

    procedure pnlImageViewClick(Sender: TObject);
    procedure pnlImageViewPaint(Sender: TObject);

    procedure sbImagesResize(Sender: TObject);

  private
    FBar: TVpNavBar;
    RefreshTimer: TTimer;
    FSelImgIndex: Integer;
    function FindBtnIndex(APersistent: TPersistent): Integer;
    function FindFolderIndex(APersistent: TPersistent): Integer;
    function GetFolderDisplayName(AFolder: TVpNavFolder): String;
    function GetItemDisplayName(AItem: TVpNavBtnItem): String;
    procedure OnTimer(Sender: TObject);
    procedure SelectionChanged(AOrderChanged: Boolean = false);
    procedure SelectList(SelList: TPersistentSelectionList);
    procedure UpdateBtnState;

  private
    FDesigner: TComponentEditorDesigner;
    procedure AddDesignHookHandlers;
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnObjectPropertyChanged(Sender: TObject; ANewObject: TPersistent);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent; ABar: TVpNavBar;
      ADesigner: TComponentEditorDesigner); reintroduce;
    destructor Destroy; override;
    procedure PopulateFolderList;
    procedure PopulateItemList;
    procedure SetData(ADesigner: TComponentEditorDesigner; ABar: TVpNavBar);
    property Bar: TVpNavBar read FBar;
    property Designer: TComponentEditorDesigner read FDesigner;
  end;

var
  frmNavEd: TfrmNavBarEd;

implementation

{$R *.lfm}

uses
  PropEditUtils, StrUtils,
  VpMisc;

const
  ITEMS_MARGIN = 2;
  IMG_MARGIN = 4;
  (*
procedure EditNavBar(ADesigner: TComponentEditorDesigner; ABar: TVpNavBar);
var
  editor: TObject;
begin
  editor := FindEditorForm(ABar);
  if editor = nil then begin
    DebugLn('EditorForm not found');
    editor := TfrmNavBarEd.Create(Application, ABar, ADesigner);
    RegisterEditorForm(editor, ABar);
  end;
  if editor <> nil then
    with TfrmNavBarEd(editor) do begin
      ShowOnTop;
    end;
end;
*)

{*** TVpNavBarEditor ***}

procedure TVpNavBarEditor.ExecuteVerb(Index : Integer);
var
  bar: TVpNavBar;
  editor: TObject;
begin
  if Index = 0 then begin
    bar := Component as TVpNavBar;
    editor := FindEditorForm(bar);
    if editor = nil then begin
      DebugLn('EditorForm not found.');
      editor := TfrmNavBarEd.Create(Application, bar, Designer);
      RegisterEditorForm(editor, bar);
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


{*** TfrmNavBarEd ***}

constructor TfrmNavBarEd.Create(AOwner: TComponent; ABar: TVpNavBar;
  ADesigner: TComponentEditorDesigner);
var
  w: Integer;
begin
  inherited Create(AOwner);

  FBar := ABar;
  FDesigner := ADesigner;

  PopulateFolderList;

  if FBar.Images <> nil then begin
    w := (FBar.Images.Width + 2*IMG_MARGIN) * FBar.Images.Count;
    pnlImageView.ClientWidth := w;
    pnlImageView.Constraints.MinHeight := FBar.Images.Height + 2 * IMG_MARGIN + GetScrollbarHeight;
    if w > sbImages.ClientWidth then begin
      sbImages.HorzScrollbar.Range := w - sbImages.ClientWidth;
      sbImages.HorzScrollbar.Visible := true;
    end else
      sbImages.HorzScrollbar.Visible := false;
  end;
  FSelImgIndex := -1;

  AddDesignHookHandlers;
  SelectionChanged;
end;

destructor TfrmNavBarEd.Destroy;
begin
  UnregisterEditorForm(Self);
  inherited Destroy;
end;

procedure TfrmNavBarEd.AddDesignHookHandlers;
begin
  if GlobalDesignHook <> nil then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
    if FBar <> nil then
    begin
      GlobalDesignHook.AddHandlerObjectPropertyChanged(OnObjectPropertyChanged);
      GlobalDesignHook.AddHandlerPersistentAdded(OnPersistentAdded);
      GlobalDesignHook.AddHandlerPersistentDeleting(OnPersistentDeleting);
      GlobalDesignHook.AddHandlerGetSelection(OnGetSelection);
      GlobalDesignHook.AddHandlerSetSelection(OnSetSelection);
    end;
  end;
end;

procedure TfrmNavBarEd.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  UnregisterEditorForm(Self);
  Action := caFree;
end;

procedure TfrmNavBarEd.FormCreate(Sender: TObject);
begin
  Top := (Screen.Height - Height) div 3;
  Left := (Screen.Width - Width) div 2;
  RefreshTimer := TTimer.Create(Self);
  RefreshTimer.Interval := 1000;
  RefreshTimer.OnTimer := OnTimer;
  RefreshTimer.Enabled := true;
end;

procedure TfrmNavBarEd.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook = nil then
    exit;
  if Assigned(FBar) and ((lbFolders.SelCount > 0) or (lbItems.SelCount > 0)) then
    GlobalDesignHook.SelectOnlyThis(FBar);
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

{ Changed}
{ Could not find a way to get notification from the IDE that a change had }
{ been made to the component outside of the component editor, so I used a }
{ timer }
procedure TfrmNavBarEd.OnTimer(Sender: TObject);
var
  S : string;
begin
  if (Bar = nil) or (Bar.ActiveFolder = -1) then
    exit;

  { update folder }
  S := Bar.Folders[Bar.ActiveFolder].Caption;
  if S = '' then
    S := Bar.Folders[Bar.ActiveFolder].Name;
  lbFolders.Items[Bar.ActiveFolder] := S;

  if (lbItems.ItemIndex > -1) then begin                                 
    S := lbItems.Items.Strings[lbItems.ItemIndex];
    PopulateItemList;
    if S <> '' then
      lbItems.ItemIndex := lbItems.Items.IndexOf(S);                     
  end;                                                                   
end;

procedure TfrmNavBarEd.FormShow(Sender: TObject);
begin
  if (Bar <> nil) and (Bar.Images <> nil) then begin
    pnlImages.Height := Bar.Images.Height + GetScrollbarHeight + 2*IMG_MARGIN +
      Panel8.Height + pnlImages.BorderSpacing.Top + pnlImages.BorderSpacing.Bottom;
  end;
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

procedure TfrmNavBarEd.OnObjectPropertyChanged(Sender: TObject; ANewObject: TPersistent);
var
  i: integer;
  item: TVpNavBtnItem;
  folder: TVpNavFolder;
begin
  if ANewObject is TVpNavBtnItem then begin
    item := TVpNavBtnItem(ANewObject);
    i := FindBtnIndex(item);
    if i > -1 then
      lbItems.Items[i] := GetItemDisplayName(item);
  end else
  if ANewObject is TVpNavFolder then begin
    folder := TVpNavFolder(ANewObject);
    i := FindFolderIndex(folder);
    if i > -1 then
      lbFolders.Items[i] := GetFolderDisplayName(folder);
  end;
end;

procedure TfrmNavBarEd.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
var
  i: Integer;
begin
  if APersistent = nil then
    DebugLn('OnPersistentAdded: Persistent = nil');

  if not Assigned(APersistent) then
    exit;

  if (APersistent is TVpNavFolder) then
  begin
    DebugLn('OnPersistentAdded: Persistent is folder');
    PopulateFolderList;
    if Select then begin
      i := FindFolderIndex(APersistent);
      lbFolders.ItemIndex := i;
    end;
  end else
  if (APersistent is TVpNavBtnItem) then
  begin
    DebugLn('OnPersistentAdded: Persistent is item');
    PopulateItemList;
    if Select then begin
      i := FindBtnIndex(APersistent);
      lbItems.ItemIndex := i;
    end;
  end;
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
end;

procedure TfrmNavBarEd.OnSetSelection(const ASelection: TPersistentSelectionList);
var
  i, j: Integer;
begin
  if Assigned(ASelection) and (ASelection.Count > 0) then
  begin
    if TPersistent(ASelection[0]) is TVpNavFolder then begin
      //Unselect all
      for i := 0 to lbFolders.Items.Count-1 do
        lbFolders.Selected[i] := false;
      //select from list
      for i := 0 to ASelection.Count - 1 do begin
        j := FindFolderIndex(ASelection[i]);
        if j <> -1 then lbFolders.Selected[j] := true;
      end;
    end else
    if TPersistent(ASelection[0]) is TVpNavBtnItem then
    begin
      // Unselect all
      for i := 0 to lbItems.Items.Count - 1 do
        lbItems.Selected[i] := false;
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
  for I := 0 to Pred(Bar.FolderCount) do begin
    S := GetFolderDisplayName(Bar.Folders[I]);
    lbFolders.Items.AddObject(S, Bar.Folders[I]);
  end;
end;

procedure TfrmNavBarEd.PopulateItemList;
var
  I : Integer;
  S : string;
begin
  lbItems.Clear;
  if lbFolders.ItemIndex = -1 then exit;
  with Bar.Folders[lbFolders.ItemIndex] do
    for I := 0 to pred(ItemCount) do begin
      S := GetItemDisplayName(Items[I]);
      lbItems.Items.AddObject(S,Items[i]);
    end;
end;

procedure TfrmNavBarEd.SetData(ADesigner: TComponentEditorDesigner; ABar: TVpNavBar);
var
  i: Integer;
  w: Integer;
begin
  if FBar <> nil then
    FBar.RemoveFreeNotification(self);

  FBar := ABar;
  FDesigner := ADesigner;

  if FBar <> nil then
    FBar.FreeNotification(self);

  PopulateFolderList;

  if FBar.Images <> nil then begin
    w := (Bar.Images.Width + 2*IMG_MARGIN) * Bar.Images.Count;
    pnlImageView.ClientWidth := w;
    pnlImageView.Constraints.MinHeight := Bar.Images.Height + 2 * IMG_MARGIN + GetScrollbarHeight;
    if w > sbImages.ClientWidth then begin
      sbImages.HorzScrollbar.Range := w - sbImages.ClientWidth;
      sbImages.HorzScrollbar.Visible := true;
    end else
      sbImages.HorzScrollbar.Visible := false;
  end;
  FSelImgIndex := -1;

  AddDesignHookHandlers;
end;

procedure TfrmNavBarEd.lbFoldersClick(Sender: TObject);
var
  SelList: TPersistentSelectionList;
  i: Integer;
begin
  PopulateItemList;
  Bar.ActiveFolder := lbFolders.ItemIndex;
  FSelImgIndex := -1;
  pnlImageView.Invalidate;

  SelList := TPersistentSelectionList.Create;
  SelList.ForceUpdate := true;

  for i := 0 to pred(lbFolders.Items.Count) do
    if lbFolders.Selected[i] then begin
      SelList.Add(TPersistent(lbFolders.Items.Objects[i]));
      Bar.FolderCollection.DoOnItemSelected(i);
    end;

  if SelList.Count > 0 then
    SelectList(SelList);

  UpdateBtnState;
end;

procedure TfrmNavBarEd.lbItemsMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Unused(Control, Index);
  if (Bar.Images <> nil) then
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

  if Index = -1 then
    exit;

  // Draw button image
  delta := ITEMS_MARGIN;
  btn := TVpNavBtnItem(lbItems.Items.Objects[Index]);
  if (Bar.Images <> nil) and (btn <> nil) and
    (btn.IconIndex > -1) and (btn.IconIndex < Bar.Images.Count) then
  begin
    Bar.Images.Draw(
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
    FSelImgIndex := btn.IconIndex;
    pnlImageView.Invalidate;
    SelList := TPersistentSelectionList.Create;
    SelList.ForceUpdate := true;
    for i:=0 to lbItems.Items.Count-1 do
      if lbItems.Selected[i] then
      begin
        SelList.Add(TPersistent(lbItems.Items.Objects[i]));
        Bar.Folders[Bar.ActiveFolder].ItemCollection.DoOnItemSelected(I);
      end;
    if SelList.Count > 0 then
      SelectList(SelList);
  end;

  UpdateBtnState;
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
    UpdateBtnState;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(Item);
      Designer.Modified;
    end;
  end;
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
    UpdateBtnState;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(Item);
      Designer.Modified;
    end;
  end;
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
    UpdateBtnState;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(folder);
      Designer.Modified;
    end;
  end;
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
    UpdateBtnState;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(folder);
      Designer.Modified;
    end;
  end;
end;

procedure TfrmNavBarEd.btnItemDeleteClick(Sender: TObject);
begin
  if (lbItems.ItemIndex <> -1) then begin
    TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]).Free;
    lbItems.ItemIndex := -1;
    FSelImgIndex := -1;
    PopulateItemList;
    if Assigned(Designer) then
      Designer.Modified;
    UpdateBtnState;
  end;
end;

procedure TfrmNavBarEd.btnFolderDeleteClick(Sender: TObject);
begin
  if (lbFolders.ItemIndex <> -1) then begin
    TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]).Free;
    lbFolders.ItemIndex := -1;
    PopulateFolderList;
    PopulateItemList;
    if Assigned(Designer) then
      Designer.Modified;
    UpdateBtnState;
  end;
end;

procedure TfrmNavBarEd.btnFolderAddClick(Sender: TObject);
begin
  Bar.FolderCollection.Add;
  PopulateFolderList;
  lbFolders.ItemIndex := lbFolders.Items.Count - 1;
  SelectionChanged(true);
  if Assigned(Designer) then
    Designer.Modified;
  lbFoldersClick(Self);
  UpdateBtnState;
end;

procedure TfrmNavBarEd.btnItemAddClick(Sender: TObject);
begin
  if (lbFolders.ItemIndex <> -1) then begin
    TVpNavFolder(
      lbFolders.Items.Objects[lbFolders.ItemIndex]).ItemCollection.Add;
    lbItems.ItemIndex := -1;
    PopulateItemList;
    SelectionChanged(true);
    if assigned(Designer) then
      Designer.Modified;
  end;
  UpdateBtnState;
end;

procedure TfrmNavBarEd.pnlImageViewPaint(Sender: TObject);
var
  R: TRect;
  Rimg: TRect;
  i: Integer;
  x, y: Integer;
  wimg, himg: Integer;
begin
  R := Rect(0, 0, sbImages.Width, sbImages.Height);
  pnlImageView.Canvas.Brush.Color := clWindow;
  pnlImageView.Canvas.FillRect(R);

  if (Bar.Images = nil) or (Bar.Images.Count = 0) then
    exit;

  wimg := Bar.Images.Width;
  himg := Bar.Images.Height;

  x := 0;
  y := R.Top + IMG_MARGIN;
  if pnlImageView.Width <= sbImages.Width then // no scrollbar
    inc(y, GetScrollbarHeight div 2);

  i := 0;
  while i < Bar.Images.Count do begin
    if i = FSelImgIndex then begin
      R := Rect(x, R.Top, x+wimg+2*IMG_MARGIN, R.Bottom);
      pnlImageView.Canvas.Brush.Color := clHighlight;
      pnlImageView.Canvas.FillRect(R);
    end;
    FBar.Images.Draw(pnlImageView.Canvas, x + IMG_MARGIN, y, i, true);
    inc(i);
    inc(x, wimg + 2*IMG_MARGIN);
  end;
end;

procedure TfrmNavBarEd.pnlImageViewClick(Sender: TObject);
var
  P: TPoint;
  btn: TVpNavBtnItem;
  res: Integer;
begin
  if (Bar.Images = nil) or (Bar.Images.Count = 0) then
    exit;

  P := pnlImageView.ScreenToClient(Mouse.CursorPos);
  FSelImgIndex := P.X div (Bar.Images.Width + 2*IMG_MARGIN);
  if FSelImgIndex >= Bar.Images.Count then FSelImgIndex := Bar.Images.Count - 1;
  pnlImageView.Invalidate;

  if (FSelImgIndex <> -1) and (lbItems.ItemIndex <> -1) then begin
    btn := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);
    if btn.IconIndex <> -1 then begin
      res := MessageDlg('Do you want to replace the button icon by this one?',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if res <> mrYes then
        exit;
    end;
    btn.IconIndex := FSelImgIndex;
    lbItems.Invalidate;
    if Assigned(Designer) then
      Designer.Modified;
  end;
end;

procedure TfrmNavBarEd.sbImagesResize(Sender: TObject);
begin
  sbImages.HorzScrollbar.Visible := sbImages.ClientWidth < pnlImageView.ClientWidth;
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
  if GlobalDesignHook <> nil then
  begin
    GlobalDesignHook.SetSelection(SelList);
    GlobalDesignHook.LookupRoot := GetLookupRootForComponent(Bar);
  end;
  SelList.Free;
end;

procedure TfrmNavBarEd.UpdateBtnState;
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


end.
  
