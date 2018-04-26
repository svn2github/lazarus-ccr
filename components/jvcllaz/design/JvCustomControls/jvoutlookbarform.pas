unit JvOutlookBarForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList, Menus,
  PropEdits, ComponentEditors,
  JvOutlookBar;

type

  { TFrmOLBEditor }

  TFrmOLBEditor = class(TForm)
    AcNewPage: TAction;
    AcNewButton: TAction;
    AcDelete: TAction;
    AcMoveUp: TAction;
    AcMoveDown: TAction;
    AcToolbar: TAction;
    AcUpdate: TAction;
    AcShowToolbarCaptions: TAction;
    AlActions: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MnuNewPage: TMenuItem;
    MnuNewButton: TMenuItem;
    popNew: TPopupMenu;
    popToolbar: TPopupMenu;
    popForm: TPopupMenu;
    StatusBar1: TStatusBar;
    TbTop: TToolBar;
    BtnNew: TToolButton;
    BtnDel: TToolButton;
    ToolButton1: TToolButton;
    BtnUp: TToolButton;
    BtnDown: TToolButton;
    TvItems: TTreeView;
    procedure AcDeleteExecute(Sender: TObject);
    procedure AcMoveDownExecute(Sender: TObject);
    procedure AcMoveUpExecute(Sender: TObject);
    procedure AcNewButtonExecute(Sender: TObject);
    procedure AcNewPageExecute(Sender: TObject);
    procedure AcShowToolbarCaptionsExecute(Sender: TObject);
    procedure AcToolbarExecute(Sender: TObject);
    procedure AcUpdateExecute(Sender: TObject);
    procedure AlActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure TvItemsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TvItemsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TvItemsKeyPress(Sender: TObject; var Key: char);
  private
    FUpdateSelectionCount: Integer;
    FLargeToolBtnSize: Integer;
    FSmallToolBtnSize: Integer;
    procedure BeginUpdateSelection;
    procedure EndUpdateSelection;
    procedure BuildTreeData;
    procedure DeleteItem(Item: TPersistent);
    procedure ExchangeItems(Node1, Node2: TTreeNode);
//    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure SelectButtonInObjectInspector(AButton: TJvOutlookBarButton; ForceUpdate: Boolean);
    procedure SelectPageInObjectInspector(APage: TJvOutlookBarPage; ForceUpdate: Boolean);
    function ValidateTreeData: boolean;

  protected
    FOutlookBar: TJvCustomOutlookBar;
    FDesigner: TComponentEditorDesigner;
    function CheckValidButtonNode(Node: TTreeNode): boolean;
    function CheckValidPageNode(Node: TTreeNode): boolean;
    function FindNode(ACandidate: TPersistent; out ANode: TTreeNode): Boolean;
    procedure Modified;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SelectionChanged(AOrderChanged: Boolean = false);

  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshNames;
    procedure SetData(AOutlookBar: TJvCustomOutlookBar; ADesigner: TComponentEditorDesigner);
    property Outlookbar: TJvCustomOutlookBar read FOutlookBar;

  end;

var
  FrmOLBEditor: TFrmOLBEditor;

implementation

{$R *.lfm}

uses
  PropEditUtils, IDEWindowIntf, IDEImagesIntf, ObjInspStrConsts,
  JvConsts;

type
  THackOutlookBar = class(TJvCustomOutlookBar);

const
  SDamagedTreeStructure = 'Dameged tree structure.';


{ TFrmOLBEditor }

constructor TFrmOLBEditor.Create(AOwner: TComponent);
begin
  inherited;
  FLargeToolBtnSize := 34;
  FSmallToolBtnSize := 22;
  TbTop.ButtonHeight := FLargeToolBtnSize;
  TbTop.ButtonWidth := TbTop.ButtonHeight;

  AlActions.Images := IDEImages.Images_16;
  AcNewPage.ImageIndex := IDEImages.LoadImage('laz_add');
  AcNewButton.ImageIndex := IDEImages.LoadImage('laz_add');
  AcDelete.ImageIndex := IDEImages.LoadImage('laz_delete');
  AcMoveDown.ImageIndex := IDEImages.LoadImage('arrow_down');
  AcMoveUp.ImageIndex := IDEImages.LoadImage('arrow_up');

  TbTop.Images := AlActions.Images;
  popNew.Images := AlActions.Images;
  popForm.Images := AlActions.Images;
  popToolbar.Images := AlActions.Images;
  (*
  if Assigned(GlobalDesignHook) then
  begin
//    GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
    GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
    GlobalDesignHook.AddHandlerGetSelection(@OnGetSelection);
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
    GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
  end;
  *)
end;

procedure TFrmOLBEditor.AcDeleteExecute(Sender: TObject);
var
  s: String;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) or (TvItems.Selected = nil) then
    exit;

  s := TCollectionItem(TvItems.Selected.Data).DisplayName;
  if MessageDlg(oisConfirmDelete, Format(oisDeleteItem, [s]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes
  then
    exit;

  BeginUpdateSelection;
  try
    FDesigner.DeleteSelection;
  finally
    EndUpdateSelection;
  end;
end;

procedure TFrmOLBEditor.AcMoveDownExecute(Sender: TObject);
var
  node: TTreeNode;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) or (TvItems.Selected = nil) then
    exit;

  ExchangeItems(TvItems.Selected, TvItems.Selected.GetNextSibling);
  node := TvItems.Selected.GetNextSibling;
  node.MoveTo(TvItems.Selected, naInsert);
  node.Expand(True);
end;

procedure TFrmOLBEditor.AcMoveUpExecute(Sender: TObject);
var
  node: TTreeNode;
begin
  node := TvItems.Selected;
  if (FOutlookBar = nil) or (FDesigner = nil) or (node = nil) then
    exit;

  ExchangeItems(TvItems.Selected, node.getPrevSibling);
  node.MoveTo(node.GetPrevSibling, naInsert);
  node.Expand(True);
  TvItems.Selected := node;
end;

procedure TFrmOLBEditor.AcNewButtonExecute(Sender: TObject);
var
  btn: TJvOutlookBarButton;
  page: TJvOutlookBarPage;
  N: TTreeNode;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  if FDesigner.PropertyEditorHook = nil then
    exit;

  N := TvItems.Selected;
  if N.Parent <> nil then
    N := N.Parent;
  page := TJvOutlookBarPage(N.Data);
  btn := page.Buttons.Add;
  FDesigner.PropertyEditorHook.PersistentAdded(btn, True);
  Modified;

  if FindNode(btn, N) then TvItems.Selected := N;
  //TvItems.Selected := TvItems.Items.AddChildObject(N, btn.Caption, btn);
  //SelectionChanged;
end;

procedure TFrmOLBEditor.AcNewPageExecute(Sender: TObject);
var
  page: TJvOutlookBarPage;
  node: TTreeNode;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  if FDesigner.PropertyEditorHook = nil then
    exit;

  page := THackOutlookBar(FOutlookBar).Pages.Add;
  FDesigner.PropertyEditorHook.PersistentAdded(page,True);
  Modified;
  if FindNode(page, node) then TvItems.Selected := node;
end;

procedure TFrmOLBEditor.AcShowToolbarCaptionsExecute(Sender: TObject);
begin
  AcShowToolbarCaptions.Checked := not AcShowToolbarCaptions.Checked;
  tbTop.ShowCaptions := AcShowToolbarCaptions.Checked;
  if AcShowToolbarCaptions.Checked then begin
    TbTop.ButtonHeight := FLargeToolBtnSize;
    TbTop.ButtonWidth := FLargeToolBtnSize;
  end else
  begin
    TbTop.ButtonHeight := FSmallToolBtnSize;
    TbTop.ButtonWidth := FSmallToolBtnSize;
  end;
end;

procedure TFrmOLBEditor.AcToolbarExecute(Sender: TObject);
begin
  AcToolBar.Checked := not acToolBar.Checked;
  TbTop.Visible := AcToolBar.Checked;
end;

procedure TFrmOLBEditor.AcUpdateExecute(Sender: TObject);
begin
  BuildTreeData;
end;

procedure TFrmOLBEditor.AlActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
var
 Sel: Boolean;
begin
  Sel := TvItems.Selected <> nil;
  AcNewButton.Enabled := Sel;
  AcDelete.Enabled := Sel;
  AcMoveUp.Enabled := Sel and (TvItems.Selected.getPrevSibling <> nil);
  AcMoveDown.Enabled := Sel and (TvItems.Selected.getNextSibling <> nil);
  AcUpdate.Enabled := Screen.ActiveForm = Self;
end;

procedure TFrmOLBEditor.BeginUpdateSelection;
begin
  Inc(FUpdateSelectionCount);
end;

procedure TFrmOLBEditor.BuildTreeData;
var
  i, j: Integer;
  page: TJvOutlookBarPage;
  pageNode: TTreeNode;
  button: TJvOutlookBarButton;
  olb: THackOutlookBar;
  s: String;
begin
  TvItems.OnDeletion := nil;
  TvItems.Items.Clear;
//  TvItems.OnDeletion := TvItemsDeletion;

  if FOutlookbar = nil then
    exit;
  {if FDesigner = nil) then
    exit; }

  TvItems.BeginUpdate;
  try
    olb := THackOutlookbar(FOutlookbar);
    for i := 0 to olb.Pages.Count-1 do begin
      page := olb.Pages[i];
      s := page.Caption;
      if s = '' then s := page.DisplayName;
      pageNode := TvItems.Items.AddObject(nil, s, page);
      for j := 0 to page.Buttons.Count-1 do begin
        button := page.Buttons[j];
        s := button.Caption;
        if s = '' then s := button.DisplayName;
        TvItems.Items.AddChildObject(pageNode, s, button);
      end;
      pageNode.Expand(false);
    end;
  finally
    TvItems.EndUpdate;
  end;
end;

function TFrmOLBEditor.CheckValidButtonNode(Node: TTreeNode): boolean;
begin
  Result := false;
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  {$B-}
  Result := (Node <> nil) and
            (Node.Data <> nil) and
            (TObject(Node.Data) is TJvOutlookBarButton);
end;

function TFrmOLBEditor.CheckValidPageNode(Node: TTreeNode): boolean;
begin
  Result := false;
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  {$B-}
  Result := (Node <> nil) and
            (Node.Data <> nil) and
            (TObject(Node.Data) is TJvOutlookBarPage);
end;

procedure TFrmOLBEditor.DeleteItem(Item: TPersistent);
var
  N, N2: TTreeNode;

  function FindNextNode(const Node: TTreeNode): TTreeNode;
  begin
    if Node = nil then
    begin
      Result := nil;
      Exit;
    end;
    Result := Node.GetNextSibling;
    if Result = nil then
      Result := Node.GetPrevSibling;
    if Result = nil then
      Result := Node.Parent;
    if Result = nil then
      Result := TvItems.Items.GetFirstNode;
    if Result = Node then
      Result := nil;
  end;

begin
  N2 := TvItems.Selected;
  N := TvItems.Items.GetFirstNode;
  try
    while Assigned(N) do
    begin
      if N = Item then
      begin
        N2 := FindNextNode(N);
        N.Data := nil;
        N.Delete;
        Exit;
      end;
      N := N.GetNext;
      N2 := N;
    end;
  finally
    TvItems.Selected := N2;
  end;
end;

procedure TFrmOLBEditor.EndUpdateSelection;
begin
  dec(FUpdateSelectionCount);
  if FUpdateSelectionCount=0 then
    SelectionChanged;
end;

procedure TFrmOLBEditor.ExchangeItems(Node1, Node2: TTreeNode);
var
  I: Integer;
  page1, page2: TJvOutlookBarPage;
  btn1, btn2: TJvOutlookBarButton;
begin
  (*
  if TObject(Node1.Data) is TJvOutlookBarButton then
  begin
    btn1 := TJvOutlookBarButton(Node1.Data);
    btn2 := TJvOutlookBarButton(Node2.Data);
    btn1.Collection.Exchange(btn1.Index, btn2.Index);
  end else
  if TObject(Node1.Data) is TJvOutlookBarPage then
  begin
    page1 := TJvOutlookBarPage(Node1.Data);
    page2 := TJvOutlookBarPage(Node2.Data);
    page1.Collection.Exchange(page1.Index, page2.Index);
  end;
//  FDesigner.Modified;
  FDesigner.PropertyEditorHook.RefreshPropertyValues;
  *)

  if TObject(Node1.Data) is TJvOutlookBarButton then
  begin
    I := TJvOutlookBarButton(Node1.Data).Index;
    TJvOutlookBarButton(Node1.Data).Index := TJvOutlookBarButton(Node2.Data).Index;
    TJvOutlookBarButton(Node2.Data).Index := I;
  end
  else
  if TObject(Node1.Data) is TJvOutlookBarPage then
  begin
    I := TJvOutlookBarPage(Node1.Data).Index;
    TJvOutlookBarPage(Node1.Data).Index := TJvOutlookBarPage(Node2.Data).Index;
    TJvOutlookBarPage(Node2.Data).Index := I;
  end;
  Modified;
end;

function TFrmOLBEditor.FindNode(ACandidate: TPersistent;
  out ANode: TTreeNode): Boolean;
var
  pageNode, btnNode: TTreeNode;
begin
  pageNode := TvItems.Items.GetFirstNode;
  while pageNode <> nil do begin
    if (ACandidate is TJvOutlookBarPage) then begin
      if TJvOutlookBarPage(pageNode.Data) = ACandidate then begin
        ANode := pageNode;
        Result := true;
        exit;
      end;
    end else
    if (ACandidate is TJvOutlookBarButton) then begin
      btnNode := pageNode.GetFirstChild;
      while btnnode <> nil do begin
        if TJvOutlookBarButton(btnNode.Data) = ACandidate then begin
          ANode := btnNode;
          Result := true;
          exit;
        end;
        btnNode := btnNode.GetNextSibling;
      end;
    end;
    pageNode := pageNode.GetNextSibling;
  end;
  Result := false;
end;

procedure TFrmOLBEditor.FormActivate(Sender: TObject);
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  if not ValidateTreeData then
    BuildTreeData;
end;

procedure TFrmOLBEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TFrmOLBEditor.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TFrmOLBEditor.FormDestroy(Sender: TObject);
begin
  if FOutlookBar <> nil then
    FOutlookBar.RemoveFreeNotification(self);

  if GlobalDesignHook = Nil then
    Exit;
  (*
  if Assigned(FComponentEditor) and Assigned(LinkDataset)
  and not (csDestroying in LinkDataset.ComponentState)
  and (FieldsListBox.SelCount > 0) then
    GlobalDesignHook.SelectOnlyThis(LinkDataset);
    *)
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TFrmOLBEditor.FormShow(Sender: TObject);
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  BuildTreeData;
end;

procedure TFrmOLBEditor.Modified;
begin
  FDesigner.PropertyEditorHook.RefreshPropertyValues;
  FDesigner.Modified;
  if GlobalDesignHook <> nil then
    GlobalDesignHook.Modified(self);
end;

procedure TFrmOLBEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FOutlookBar) and (Operation = opRemove) then
  begin
    // The toolbar is currently released, whose content is displayed in the
    // editor window. Need to clean up the content - otherwise the window will
    // have references to the already removed toolbars, which will end in AVs ...
    SetData(nil, nil);
  end;
end;
                                          (*
procedure TFrmOLBEditor.OnComponentRenamed(AComponent: TComponent);
var
  page: TJvOutlookBarPage;
  button: TJvOutlookBarButton;
  i: integer;
begin
  if AComponent is TJvOutlookBarPage then begin
    page := TJvOutlookBarPage(AComponent);
  if AComponent is TField then begin
    Field := TField(AComponent);
    if not Assigned( Field ) then Exit;
    i := FieldsListBox.Items.IndexOfObject(Field);
    if i >= 0 then
      FieldsListBox.Items[i] := Field.FieldName;
  end else
  if AComponent is TDataset And (AComponent = LinkDataset) then
    Caption := fesFeTitle + ' - ' + LinkDataset.Name;
end;                                        *)

procedure TFrmOLBEditor.OnGetSelection(
  const ASelection: TPersistentSelectionList);
var
  pagenode, btnnode: TTreeNode;
begin
  if not Assigned(ASelection) then
    exit;
  if ASelection.Count > 0 then
    ASelection.Clear;
  pageNode := TvItems.Items.GetFirstNode;
  while pageNode <> nil do begin
    if pageNode.Selected then
      ASelection.Add(TPersistent(pageNode.Data));
    btnNode := pageNode.GetFirstChild;
    while btnNode <> nil do begin
      if btnNode.selected then
        ASelection.Add(TPersistent(btnNode.Data));
      btnNode := btnNode.GetNextSibling;
    end;
    pageNode := pageNode.GetNextSibling;
  end;
end;

procedure TFrmOLBEditor.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
var
  olb: TJvCustomOutlookBar;
  page: TJvOutlookBarPage;
  button: TJvOutlookBarButton;
  node: TTreeNode;
begin
  if not Assigned(APersistent) then
    exit;

  if (APersistent is TJvOutlookBarPage) then begin
    page := TJvOutlookBarPage(APersistent);
    olb := page.GetOutlookBar;
    if olb = FOutlookBar then begin
      BuildTreeData;
      if FindNode(page, node) then
        TvItems.Selected := node;
    end;
  end else
  if (APersistent is TJvOutlookBarButton) then begin
    button := TJvOutlookBarButton(APersistent);
    olb := button.GetOutlookBar;
    if olb = FOutlookBar then begin
      BuildTreeData;
      if FindNode(button, node) then
        TvItems.Selected := node;
    end;
  end;

{
  if (APersistent is TJvOutlookBarPage) then begin
    page := TJvOutlookBarPage(APersistent);
    olb := THackOutlookbar((page.Collection).Owner);
    if (olb = FOutlookBar) then begin
      if not FindNode(page, node) then
        node := TvItems.Items.AddObject(nil, page.Caption, page);
      TvItems.Selected := node;
    end;
  end else
  if (APersistent is TJvOutlookBarButton) then begin
    button := TJvOutlookBarButton(APersistent);
    page := TJvOutlookBarPage(button.Collection.Owner);
    olb := THackOutlookBar(page.Collection.Owner);
    if (olb = FOutlookBar) and FindNode(page, node) then begin
      if not FindNode(button, node) then
        node := TvItems.Items.AddChildObject(node, button.Caption, button);
      TvItems.Selected := node;
    end;
  end;
  }
end;

procedure TFrmOLBEditor.OnPersistentDeleting(APersistent: TPersistent);
var
  node: TTreeNode;
begin
  if FindNode(APersistent, node) then
    TvItems.Items.Delete(node);
end;

procedure TFrmOLBEditor.OnSetSelection(
  const ASelection: TPersistentSelectionList);
var
  i: Integer;
  node: TTreeNode;
begin
  if Assigned(ASelection) then begin
    //Unselect all
    for i := 0 to TvItems.SelectionCount-1 do
      TvItems.Items.GetSelections(I).Selected := false;
    //select from list
    for i := 0 to ASelection.Count - 1 do
      if FindNode(ASelection.Items[i], node) then
        node.Selected := true;
  end;
end;

procedure TFrmOLBEditor.RefreshNames;
var
  pageNode, buttonNode: TTreeNode;
  obj: TObject;
  s: string;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;

  pagenode := TvItems.Items.GetFirstNode;
  while pagenode <> nil do
  begin
    if not CheckValidPageNode(pagenode) then
      raise Exception.Create('TFrmOLBEditor.RefreshNames: '+ SDamagedTreeStructure);
    pageNode.Text := TJvOutlookBarPage(pageNode.Data).Caption;
    buttonNode := pageNode.GetFirstChild;
    while buttonNode <> nil do
    begin
      if not CheckValidButtonNode(buttonNode) then
        raise Exception.Create('TFrmOLBEditor.RefreshNames: '+ SDamagedTreeStructure);
      buttonNode.Text := TJvOutlookBarButton(buttonNode.Data).Caption;
      buttonNode := buttonNode.GetNextSibling;
    end;
    pageNode := pageNode.GetNextSibling;
  end;
end;

procedure TFrmOLBEditor.SelectionChanged(AOrderChanged: Boolean = false);
var
  SelList: TPersistentSelectionList;
begin
  if (FUpdateSelectionCount>0) or (GlobalDesignHook=nil) then
    exit;

  GlobalDesignHook.RemoveHandlerSetSelection(@OnSetSelection);
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
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
  AlActions.UpdateAction(nil);
end;

procedure TFrmOLBEditor.SelectPageInObjectInspector(APage: TJvOutlookbarPage;
  ForceUpdate: Boolean);
var
  I: Integer;
  NewSelection: TPersistentSelectionList;
begin
  if (FOutlookBar = nil) or (GlobalDesignHook = nil) then
    Exit;

  // select in OI
  NewSelection := TPersistentSelectionList.Create;
  NewSelection.ForceUpdate := ForceUpdate;
  try
    NewSelection.Add(APage);
    GlobalDesignHook.SetSelection(NewSelection);
    GlobalDesignHook.LookupRoot := GetLookupRootForComponent(FOutlookBar);
  finally
    NewSelection.Free;
  end;
end;

procedure TFrmOLBEditor.SelectButtonInObjectInspector(AButton: TJvOutlookBarButton;
  ForceUpdate: Boolean);
var
  I: Integer;
  NewSelection: TPersistentSelectionList;
begin
  if (FOutlookBar = nil) or (GlobalDesignHook = nil) then
    Exit;

  // select in OI
  NewSelection := TPersistentSelectionList.Create;
  NewSelection.ForceUpdate := ForceUpdate;
  try
    NewSelection.Add(AButton);
    GlobalDesignHook.SetSelection(NewSelection);
    GlobalDesignHook.LookupRoot := GetLookupRootForComponent(FOutlookBar);
  finally
    NewSelection.Free;
  end;
end;

procedure TFrmOLBEditor.SetData(AOutlookBar: TJvCustomOutlookBar;
  ADesigner: TComponentEditorDesigner);
begin
  if FOutlookBar <> nil then
    FOutlookBar.RemoveFreeNotification(self);

  FOutlookBar := AOutlookBar;
  FDesigner := ADesigner;

  if FOutlookBar <> nil then
    FOutlookBar.FreeNotification(self);

  if GlobalDesignHook <> nil then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
    if FOutlookbar <> nil then
    begin
      GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
      GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
      GlobalDesignHook.AddHandlerGetSelection(@OnGetSelection);
      GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
    end;
  end;

  BuildTreeData;
end;

procedure TFrmOLBEditor.TvItemsChange(Sender: TObject; Node: TTreeNode);
var
  Obj: TObject;
  olb: THackOutlookBar;
  page: TJvOutlookBarPage;
  btn: TJvOutlookBarButton;
  index: integer;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;

  olb := THackOutlookBar(FOutlookBar);
  if Assigned(Node) then
  begin
    Obj := TObject(Node.Data);
    if Obj = nil then
      raise Exception.Create('TFrmOLBEditor.TvItemsChange: Incorrect data in the field.');
    if Obj is TJvOutlookBarPage then
    begin
      if not(CheckValidPageNode(Node)) then
        raise Exception.Create('TFrmOLBEditor.TvItemsChange: ' + SDamagedTreeStructure);
      page := TJvOutlookBarPage(Obj);
      SelectPageInObjectInspector(page, true);
      index := olb.Pages.IndexOf(page);
      if index = -1 then
        raise Exception.Create('TFrmOLBEditor.TvItemsChange: ' + SDamagedTreeStructure);
      olb.ActivePageIndex := index;
    end else
    if Obj is TJvOutlookBarButton then
    begin
      btn := TJvOutlookBarButton(Obj);
      SelectButtonInObjectInspector(btn, true);
      if not(CheckValidButtonNode(Node)) then
        raise Exception.Create('TFrmOLBEditor.TvItemsChange: ' + SDamagedTreeStructure);
      page := TJvOutlookBarPage(Node.Parent.Data);
      index := olb.Pages.IndexOf(page);
      if index = -1 then
        raise Exception.Create('TFrmOLBEditor.TvItemsChange: ' + SDamagedTreeStructure);
      olb.ActivePageIndex := index;
    end;
  end else
     FDesigner.SelectOnlyThisComponent(FOutlookBar);

  Modified;
end;

procedure TFrmOLBEditor.TvItemsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  //
end;

procedure TFrmOLBEditor.TvItemsEdited(Sender: TObject; Node: TTreeNode; var S: string);
var
  page: TJvOutlookBarPage;
  btn: TJvOutlookBarButton;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;

  if Node.Data = nil then
    raise Exception.Create('TfrmOLBEditor.TvItemsEdited: ' + SDamagedTreeStructure);

  if TObject(Node.Data) is TJvOutlookBarPage then
  begin
    page := TJvOutlookBarPage(Node.Data);
    page.Caption := S;
    Modified;
  end else
  if TObject(Node.Data) is TJvOutlookBarButton then
  begin
    btn := TJvOutlookBarButton(Node.Data);
    btn.Caption := S;
    Modified;
  end else
    raise Exception.Create('TFrmOLBEditor.TvItemsEdited: ' + SDamagedTreeStructure);
end;

procedure TFrmOLBEditor.TvItemsKeyPress(Sender: TObject; var Key: char);
begin
  //
end;

function TFrmOLBEditor.ValidateTreeData: boolean;
var
  i, j: Integer;
  pagesValid: Boolean;
  btnsValid: Boolean;
  pageNode: TTreeNode;
  btnNode: TTreeNode;
  olb: THackOutlookBar;
begin
  Result := false;
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;

  olb := THackOutlookBar(FOutlookBar);
  i := 0;
  pagesValid := true;
  pageNode := TvItems.Items.GetFirstNode;

  while (i < olb.Pages.Count) and pagesValid do
  begin
    pagesValid := pagesValid and (pageNode <> nil);
    if pagesValid then
      pagesValid := pagesValid and (TObject(pageNode.Data) = olb.Pages[i]);
    if pagesValid then
    begin
      j := 0;
      btnsValid := true;
      btnNode := pageNode.GetFirstChild;
      while (j < olb.Pages[i].Buttons.Count) and btnsValid do
      begin
        btnsValid := btnsValid and (btnNode <> nil);
        if btnsValid then
          btnsValid := btnsValid and (TObject(btnNode.Data) = olb.Pages[i].Buttons[j]);
        if btnsValid then
        begin
          inc(j);
          btnNode := btnNode.GetNextSibling;
        end;
      end;

      // Important! You need to make sure that there are no extra items in the tree!
      btnsValid := btnsValid and (btnNode = nil);
    end;

    if pagesValid then
    begin
      inc(i);
      pageNode := pageNode.GetNextSibling;
    end;
  end;

  // Important! You need to make sure that there are no extra items in the tree!
  pagesValid := pagesValid and (pageNode = nil);
  Result := pagesValid;
end;

end.

