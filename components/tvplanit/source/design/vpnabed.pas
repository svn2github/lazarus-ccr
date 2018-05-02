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
 {$IFDEF LCL}

 lazlogger,

  LCLProc, LCLType, LCLIntf,
  PropEdits, LazarusPackageIntf, FieldsEditor, ComponentEditors,
 {$ELSE}
  Windows, Messages,
  {$IFDEF VERSION6}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons,
  VpBase, VpNavBar;

type
{$IFNDEF LCL}
 {$IFDEF VERSION6}
  TProtectedSelList = class(TDesignerSelections);
 {$ENDIF}
{$ENDIF}

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
//    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure lbFoldersClick(Sender: TObject);
    procedure lbItemsMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

    procedure pnlImageViewClick(Sender: TObject);
    procedure pnlImageViewPaint(Sender: TObject);

    procedure sbImagesResize(Sender: TObject);

  private
    FBar: TVpNavBar;
    FDesigner: TIDesigner;
    RefreshTimer: TTimer;
    FSelImgIndex: Integer;
  {$IFDEF LCL}
    function FindBtnIndex(APersistent: TPersistent): Integer;
    function FindFolderIndex(APersistent: TPersistent): Integer;
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure SelectList(SelList: TPersistentSelectionList);
  {$ELSE}
   {$IFDEF VERSION5}
    {$IFDEF VERSION6}
    procedure SelectList(SelList: TDesignerSelections);
    {$ELSE}
    procedure SelectList(SelList: TDesignerSelectionList);
    {$ENDIF}
   {$ELSE}
    procedure SelectList(SelList: TComponentList);
   {$ENDIF}
  {$ENDIF}
    procedure OnTimer(Sender: TObject);

  public
    { Public declarations }
    procedure PopulateFolderList;
    procedure PopulateItemList;
    procedure SetData(ADesigner: TIDesigner; ABar: TVpNavBar);
    property Bar: TVpNavBar read FBar;
    property Designer: TIDesigner read FDesigner;
  end;

var
  frmNavEd: TfrmNavBarEd;

implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.DFM}
{$ENDIF}

uses
  PropEditUtils,
  VpMisc;

const
  ITEMS_MARGIN = 2;
  IMG_MARGIN = 4;

{$IFDEF LCL}
procedure EditNavBar(Designer: TIDesigner; Bar: TVpNavBar);
{$ELSE}
 {$IFDEF VERSION6}
procedure EditNavBar(Designer: TIDesigner; Bar: TVpNavBar);
 {$ELSE}
procedure EditNavBar(Designer: TIFormDesigner; Bar: TVpNavBar);
 {$ENDIF}
{$ENDIF}
begin
  if frmNavEd = nil then
    frmNavEd := TfrmNavBarEd.Create(Application);
  frmNavEd.SetData(Designer, Bar);
  frmNavEd.Show;
end;

{*** TVpNavBarEditor ***}

procedure TVpNavBarEditor.ExecuteVerb(Index : Integer);
begin
  if Index = 0 then
    EditNavBar(Designer, (Component as TVpNavBar));
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

procedure TfrmNavBarEd.FormCreate(Sender: TObject);
begin
  Top := (Screen.Height - Height) div 3;
  Left := (Screen.Width - Width) div 2;
  RefreshTimer := TTimer.Create(Self);
  RefreshTimer.Interval := 1000;
  RefreshTimer.OnTimer := OnTimer;
  RefreshTimer.Enabled := true;
end;

{=====}

procedure TfrmNavBarEd.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  (*
  Unused(Action);
  RefreshTimer.Free;
  Release;
  *)
end;
{=====}

{ Changed}
{ Could not find a way to get notification from the IDE that a change had }
{ been made to the component outside of the component editor, so I used a }
{ timer }
procedure TfrmNavBarEd.OnTimer(Sender: TObject);
var
  S : string;
begin
  if Bar.ActiveFolder < 0 then
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
{=====}
        (*
procedure TfrmNavBarEd.FormResize(Sender: TObject);
begin
  pnlFolders.Width := (pnlItems.Width + pnlFolders.Width) div 2;
  if Bar.Images <> nil then begin
    pnlImages.Height := 25 + (5 * (Bar.Images.Height div 3));
    lbImages.Columns := lbImages.ClientWidth div Bar.Images.Width;
    {Allow for scrollbar if excessive number of images}
    if (lbImages.Width >= Bar.Images.Width) then
      pnlImages.Height := pnlImages.Height + 20;
  end;
end;  *)

procedure TfrmNavBarEd.FormShow(Sender: TObject);
begin
  if Bar.Images <> nil then begin
//    sbImages.Constraints.MinHeight := Bar.Images.Height + GetScrollbarHeight + 2*IMG_MARGIN;
    pnlImages.Height := Bar.Images.Height + GetScrollbarHeight + 2*IMG_MARGIN +
      Panel8.Height + pnlImages.BorderSpacing.Top + pnlImages.BorderSpacing.Bottom;
    //lbImages.Columns := lbImages.ClientWidth div Bar.Images.Width;
  end;
end;

{=====}

{$IFDEF LCL}
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
{$ENDIF}

{$IFDEF LCL}
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
{$ENDIF}

{$IFDEF LCL}
procedure TfrmNavBarEd.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
var
  i: Integer;
begin
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
end;
{$ENDIF}

{$IFDEF LCL}
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
{$ENDIF}

{$IFDEF LCL}
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
{$ENDIF}

procedure TfrmNavBarEd.PopulateFolderList;
var
  I : Integer;
  S : string;
begin
  lbFolders.Clear;
  for I := 0 to Pred(Bar.FolderCount) do begin
    S := Bar.Folders[I].Caption;
    if S = '' then
      S := Bar.Folders[I].Name;
    lbFolders.Items.AddObject(S, Bar.Folders[I]);
  end;
end;
{=====}

procedure TfrmNavBarEd.PopulateItemList;
var
  I : Integer;
  S : string;
begin
  lbItems.Clear;
  if lbFolders.ItemIndex = -1 then exit;
  with Bar.Folders[lbFolders.ItemIndex] do
    for I := 0 to pred(ItemCount) do begin
      S := Items[I].Caption;
      if S = '' then
        S := Items[I].Name;
      lbItems.Items.AddObject(S,Items[i]);
    end;
end;
{=====}

procedure TfrmNavBarEd.SetData(ADesigner: TIDesigner; ABar: TVpNavBar);
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
    (*
    lbImages.ItemHeight := Bar.Images.Height + 4;
    for i := 0 to pred(FBar.Images.Count) do
      lbImages.Items.Add(IntToStr(i));
      *)
  end;
  FSelImgIndex := -1;

  if GlobalDesignHook <> nil then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
    if FBar <> nil then
    begin
      GlobalDesignHook.AddHandlerPersistentAdded(OnPersistentAdded);
      GlobalDesignHook.AddHandlerPersistentDeleting(OnPersistentDeleting);
      GlobalDesignHook.AddHandlerGetSelection(OnGetSelection);
      GlobalDesignHook.AddHandlerSetSelection(OnSetSelection);
    end;
  end;
end;

procedure TfrmNavBarEd.lbFoldersClick(Sender: TObject);
{$IFDEF LCL}
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
  if not Bar.FolderCollection.ReadOnly then
  begin
    btnFolderUp.Enabled := SelList.Count = 1;
    btnFolderDown.Enabled := btnFolderUp.Enabled;
    btnFolderDelete.Enabled := btnFolderUp.Enabled;
  end;
  if SelList.Count > 0 then
    SelectList(SelList);
end;
{$ELSE}
var
{$IFDEF VERSION5}
 {$IFDEF VERSION6}
  SelList : TDesignerSelections;
 {$ELSE}
  SelList : TDesignerSelectionList;
 {$ENDIF}
{$ELSE}
  SelList : TComponentList;
{$ENDIF}
  {%H-}i : Integer;
begin
  PopulateItemList;
  Bar.ActiveFolder := lbFolders.ItemIndex;

{$IFDEF VERSION5}
  {$IFDEF VERSION6}
    SelList := TDesignerSelections.Create;
  {$ELSE}
    SelList := TDesignerSelectionList.Create;
  {$ENDIF}
{$ELSE}
  SelList := TComponentList.Create;
{$ENDIF}
  for i := 0 to pred(lbFolders.Items.Count) do
    if lbFolders.Selected[i] then begin
      {$IFDEF VERSION6}
        TProtectedSelList(SelList).Add(TComponent(lbFolders.Items.Objects[i]));
      {$ELSE}
        SelList.Add(TComponent(lbFolders.Items.Objects[i]));
      {$ENDIF}
      Bar.FolderCollection.DoOnItemSelected(I);
    end;
  if not Bar.FolderCollection.ReadOnly
  then begin
    {$IFDEF VERSION6}
      btnFolderUp.Enabled := TProtectedSelList(SelList).Count = 1;
    {$ELSE}
      btnFolderUp.Enabled := SelList.Count = 1;
    {$ENDIF}
    btnFolderDown.Enabled := btnFolderUp.Enabled;
    btnFolderDelete.Enabled := btnFolderUp.Enabled;
  end;
  {$IFDEF VERSION6}
  if TProtectedSelList(SelList).Count > 0 then
  {$ELSE}
  if SelList.Count > 0 then
  {$ENDIF}
    SelectList(SelList);
end;
{$ENDIF}
{=====}

procedure TfrmNavBarEd.lbItemsMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Unused(Control, Index);
  if (Bar.Images <> nil) then
    Height := Bar.Images.Height + 2 * ITEMS_MARGIN;
end;
{=====}

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
{=====}

procedure TfrmNavBarEd.lbItemsClick(Sender: TObject);
{$IFDEF LCL}
var
  SelList: TPersistentSelectionList;
  i: Integer;
  btn: TVpNavBtnItem;
begin
  if (lbItems.ItemIndex <> -1) and (Bar.ActiveFolder <> 1) then
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
    if not Bar.Folders[Bar.ActiveFolder].ItemCollection.ReadOnly
    then begin
      btnItemUp.Enabled := SelList.Count = 1;
      btnItemDown.Enabled := btnItemUp.Enabled;
      btnItemDelete.Enabled := btnItemUp.Enabled;
    end;
    if SelList.Count > 0 then
      SelectList(SelList);
  end;
end;
{$ELSE}
var
{$IFDEF VERSION5}
 {$IFDEF VERSION6}
  SelList : TDesignerSelections;
 {$ELSE}
  SelList : TDesignerSelectionList;
 {$ENDIF}
{$ELSE}
  SelList : TComponentList;
{$ENDIF}
  i : Integer;
begin
  if (lbItems.ItemIndex <> -1) then begin
    lbImages.ItemIndex :=
      TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]).IconIndex;

   {$IFDEF VERSION5}
    {$IFDEF VERSION6}
    SelList := TDesignerSelections.Create;
    {$ELSE}
    SelList := TDesignerSelectionList.Create;
    {$ENDIF}
   {$ELSE}
    SelList := TComponentList.Create;
   {$ENDIF}

    for i := 0 to pred(lbItems.Items.Count) do
      if lbItems.Selected[i] then begin
       {$IFDEF VERSION6}
        TProtectedSelList(SelList).Add(TComponent(lbItems.Items.Objects[i]));
       {$ELSE}
        SelList.Add(TComponent(lbItems.Items.Objects[i]));
       {$ENDIF}
        Bar.Folders[Bar.ActiveFolder].ItemCollection.DoOnItemSelected(I);
      end;
    if not Bar.Folders[Bar.ActiveFolder].ItemCollection.ReadOnly
    then begin
     {$IFDEF VERSION6}
      btnItemUp.Enabled := TProtectedSelList(SelList).Count = 1;
     {$ELSE}
      btnItemUp.Enabled := SelList.Count = 1;
     {$ENDIF}
      btnItemDown.Enabled := btnItemUp.Enabled;
      btnItemDelete.Enabled := btnItemUp.Enabled;
    end;
   {$IFDEF VERSION6}
    if TProtectedSelList(SelList).Count > 0 then
      SelectList(SelList);
   {$ELSE}
    if SelList.Count > 0 then
      SelectList(SelList);
   {$ENDIF}
  end;
end;
{$ENDIF}
{=====}
                                   (*
procedure TfrmNavBarEd.lbImagesClick(Sender: TObject);
begin
  if (lbImages.ItemIndex <> -1) and (lbItems.ItemIndex <> -1) then begin
    TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]).IconIndex :=
      lbImages.ItemIndex;
    lbItems.Invalidate;
    if assigned(Designer) then
      Designer.Modified;
  end;
end;                                 *)

{=====}

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

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(Item);
      Designer.Modified;
    end;

    PopulateItemList;

    lbItems.ItemIndex := SaveItemIndex - 1;
  end;
end;
{=====}

procedure TfrmNavBarEd.btnItemDownClick(Sender: TObject);
var
  Item: TVpNavBtnItem;
begin
  if (lbItems.ItemIndex > -1) then begin
    Item := TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]);

    if Item.Index < Pred(lbItems.Items.Count) then
      Item.Index := Item.Index + 1;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(Item);
      Designer.Modified;
    end;

    PopulateItemList;

    lbItems.ItemIndex := Item.Index;
  end;
end;
{=====}

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

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(folder);
      Designer.Modified;
    end;

    PopulateFolderList;
    
    lbFolders.ItemIndex := SaveItemIndex - 1;
  end;
end;
{=====}

procedure TfrmNavBarEd.btnFolderDownClick(Sender: TObject);
var
  Folder: TVpNavFolder;
begin
  if (lbFolders.ItemIndex > -1) then begin
    Folder := TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]);

    if Folder.Index < pred(lbFolders.Items.Count) then
      Folder.Index := Folder.Index + 1;

    if Assigned(Designer) then begin
      GlobalDesignHook.SelectOnlyThis(nil);
      GlobalDesignHook.SelectOnlyThis(folder);
      Designer.Modified;
    end;

    PopulateFolderList;

    lbFolders.ItemIndex := Folder.Index;
  end;
end;
{=====}

procedure TfrmNavBarEd.btnItemDeleteClick(Sender: TObject);
begin
  if (lbItems.ItemIndex <> -1) then begin
    TVpNavBtnItem(lbItems.Items.Objects[lbItems.ItemIndex]).Free;
    lbItems.ItemIndex := -1;
    FSelImgIndex := -1;
    PopulateItemList;
    if assigned(Designer) then
      Designer.Modified;
  end;
end;
{=====}

procedure TfrmNavBarEd.btnFolderDeleteClick(Sender: TObject);
begin
  if (lbFolders.ItemIndex <> -1) then begin
    TVpNavFolder(lbFolders.Items.Objects[lbFolders.ItemIndex]).Free;
    lbFolders.ItemIndex := -1;
    PopulateFolderList;
    PopulateItemList;
    if assigned(Designer) then
      Designer.Modified;
  end;
end;
{=====}

procedure TfrmNavBarEd.btnFolderAddClick(Sender: TObject);
begin
  Bar.FolderCollection.Add;
  PopulateFolderList;
  lbFolders.ItemIndex := lbFolders.Items.Count - 1;
  if assigned(Designer) then
    Designer.Modified;
  lbFoldersClick(Self);
end;
{=====}

procedure TfrmNavBarEd.btnItemAddClick(Sender: TObject);
begin
  if (lbFolders.ItemIndex <> -1) then begin
    TVpNavFolder(
      lbFolders.Items.Objects[lbFolders.ItemIndex]).ItemCollection.Add;
    lbItems.ItemIndex := -1;
    PopulateItemList;
    if assigned(Designer) then
      Designer.Modified;
  end;
end;
{=====}

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
   Bar.Images.Draw(pnlImageView.Canvas, x + IMG_MARGIN, y, i, true);
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

{$IFDEF LCL}
procedure TfrmNavBarEd.Selectlist(SelList: TPersistentSelectionList);
begin
  if GlobalDesignHook <> nil then
  begin
    GlobalDesignHook.SetSelection(SelList);
    GlobalDesignHook.LookupRoot := GetLookupRootForComponent(Bar);
  end;
  SelList.Free;
end;
{$ELSE}
{$IFDEF VERSION5}
 {$IFDEF VERSION6}
  procedure TfrmNavBarEd.SelectList(SelList : TDesignerSelections);
 {$ELSE}
  procedure TfrmNavBarEd.SelectList(SelList : TDesignerSelectionList);
 {$ENDIF}
{$ELSE}
procedure TfrmNavBarEd.SelectList(SelList : TComponentList);
{$ENDIF}
begin
 {$IFNDEF Ver80}
  {$IFDEF VERSION4}
  if Designer <> nil then
    {$IFDEF VERSION6}
    (Designer as IDesigner).SetSelections(SelList);
    {$ELSE}
    (Designer as IFormDesigner).SetSelections(SelList);
    {$ENDIF}
  {$ELSE}
  if Designer <> nil then
    (Designer as TFormDesigner).SetSelections(SelList);
  {$ENDIF}
  SelList.Free;
 {$ELSE}
  CompLib.SetSelection(Designer, Designer.Form, SelList);
 {$ENDIF}
end;
{$ENDIF}
{=====}

end.
  
