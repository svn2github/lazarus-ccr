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
    AcShowTextLabels: TAction;
    AlActions: TActionList;
    ImgList16: TImageList;
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
    procedure AcShowTextLabelsExecute(Sender: TObject);
    procedure AcToolbarExecute(Sender: TObject);
    procedure AcUpdateExecute(Sender: TObject);
    procedure AlActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure TvItemsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TvItemsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TvItemsKeyPress(Sender: TObject; var Key: char);
  private
    procedure BuildTreeData;
    procedure DeleteItem(Item: TPersistent);
    procedure ExchangeItems(Node1, Node2: TTreeNode);
    class function GetButtonName(OLBar: TJvCustomOutlookBar): string;
    class function GetPageName(OLBar: TJvCustomOutlookBar): string;
    procedure SelectButtonInObjectInspector(AButton: TJvOutlookBarButton; ForceUpdate: Boolean);
    procedure SelectPageInObjectInspector(APage: TJvOutlookBarPage; ForceUpdate: Boolean);
    function ValidateTreeData: boolean;

  protected
    FOutlookBar: TJvCustomOutlookBar;
    FDesigner: TComponentEditorDesigner;
    function CheckValidButtonNode(Node: TTreeNode): boolean;
    function CheckValidPageNode(Node: TTreeNode): boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    procedure RefreshNames;
    procedure SetData(AOutlookBar: TJvCustomOutlookBar; ADesigner: TComponentEditorDesigner);
    property Outlookbar: TJvCustomOutlookBar read FOutlookBar;

  end;

var
  FrmOLBEditor: TFrmOLBEditor;

implementation

{$R *.lfm}

uses
  PropEditUtils,
  JvConsts;

type
  THackOutlookBar = class(TJvCustomOutlookBar);

const
  SDamagedTreeStructure = 'Dameged tree structure.';


{ TFrmOLBEditor }

procedure TFrmOLBEditor.AcDeleteExecute(Sender: TObject);
var
  P: TPersistent;
  page: TJvOutlookBarPage;
  node: TTreeNode;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) or (TvItems.Selected = nil) then
    exit;

  TvItems.Items.BeginUpdate;
  try
    node := TvItems.Selected;
    P := TPersistent(node.Data);
    if P is TJvOutlookBarPage then
      THackOutlookBar(FOutlookBar).Pages.Delete(TJvOutlookBarPage(P).Index)
    else
    if P is TJvOutlookBarButton then
    begin
      page := TJvOutlookBarPage(node.Parent.Data);
      page.Buttons.Delete(TJvOutlookBarButton(P).Index);
    end;
    DeleteItem(node);
  finally
    TvItems.Items.EndUpdate;
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
//  btn.Caption := FDesigner.CreateUniqueComponentName(btn.ClassName);
  btn.Caption := GetButtonName(OutlookBar);
  TvItems.Selected := TvItems.Items.AddChildObject(N, btn.Caption, btn);

  FDesigner.PropertyEditorHook.PersistentAdded(btn, True);
  FDesigner.Modified;
end;

procedure TFrmOLBEditor.AcNewPageExecute(Sender: TObject);
var
  page: TJvOutlookBarPage;
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  if FDesigner.PropertyEditorHook = nil then
    exit;

  page := THackOutlookBar(FOutlookBar).Pages.Add;
//  page.Caption := FDesigner.CreateUniqueComponentName(page.ClassName);
  page.Caption := GetPageName(FOutlookBar);
  TvItems.Selected := TvItems.Items.AddObject(nil, page.Caption, page);

  FDesigner.PropertyEditorHook.PersistentAdded(page,True);
  FDesigner.Modified;
end;

procedure TFrmOLBEditor.AcShowTextLabelsExecute(Sender: TObject);
begin
  AcShowTextLabels.Checked := not AcShowTextLabels.Checked;
  tbTop.ShowCaptions := AcShowTextLabels.Checked;
  if AcShowTextLabels.Checked then begin
    tbTop.ButtonHeight := 32;
    tbTop.ButtonWidth := 32;
  end else
  begin
    tbTop.ButtonHeight := 22;
    tbTop.ButtonWidth := 22;
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

procedure TFrmOLBEditor.BuildTreeData;
var
  i, j: Integer;
  page: TJvOutlookBarPage;
  pageNode: TTreeNode;
  button: TJvOutlookBarButton;
  olb: THackOutlookBar;
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
      pageNode := TvItems.Items.AddObject(nil, page.Caption, page);
      for j := 0 to page.Buttons.Count-1 do begin
        button := page.Buttons[j];
        TvItems.Items.AddChildObject(pageNode, button.Caption, button);
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

procedure TFrmOLBEditor.ExchangeItems(Node1, Node2: TTreeNode);
var
  I: Integer;
begin
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
end;

procedure TFrmOLBEditor.FormActivate(Sender: TObject);
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  if not ValidateTreeData then
    BuildTreeData;
end;

procedure TFrmOLBEditor.FormDestroy(Sender: TObject);
begin
  if FOutlookBar <> nil then
    FOutlookBar.RemoveFreeNotification(self);
end;

procedure TFrmOLBEditor.FormShow(Sender: TObject);
begin
  if (FOutlookBar = nil) or (FDesigner = nil) then
    exit;
  BuildTreeData;
end;

class function TFrmOLBEditor.GetButtonName(OLBar: TJvCustomOutlookBar): string;
const
  cPrefix = 'JvOutlookBarButton';
  cTemplate = cPrefix + '%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;
    for I := 0 to THackOutlookBar(OLBar).Pages.Count - 1 do
      for J := 0 to THackOutlookBar(OLBar).Pages[I].Buttons.Count - 1 do
        if AnsiSameText(THackOutlookBar(OLBar).Pages[I].Buttons[J].Caption, S) then
          Exit;
    Result := True;
  end;

begin
  Result := cPrefix;
  if OLBar <> nil then
    for K := 1 to MaxInt - 1 do
    begin
      Tmp := Format(cTemplate, [K]);
      if IsUnique(Tmp) then
      begin
        Result := Tmp;
        Exit;
      end;
    end;
end;

class function TFrmOLBEditor.GetPageName(OLBar: TJvCustomOutlookBar): string;
const
  cPrefix = 'JvOutlookBarPage';
  cTemplate = cPrefix + '%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to THackOutlookBar(OLBar).Pages.Count - 1 do
      if AnsiSameText(THackOutlookBar(OLBar).Pages[I].Caption, S) then
        Exit;
    Result := True;
  end;

begin
  Result := cPrefix;
  if OLBar <> nil then
    for K := 1 to MaxInt - 1 do
    begin
      Tmp := Format(cTemplate, [K]);
      if IsUnique(Tmp) then
      begin
        Result := Tmp;
        Exit;
      end;
    end;
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
      (****************** FIX ME
      FDesigner.SelectOnlyThisComponent(page);  *)
      index := olb.Pages.IndexOf(page);
      if index = -1 then
        raise Exception.Create('TFrmOLBEditor.TvItemsChange: ' + SDamagedTreeStructure);
      olb.ActivePageIndex := index;
    end else
    if Obj is TJvOutlookBarButton then
    begin
      btn := TJvOutlookBarButton(Obj);
      SelectButtonInObjectInspector(btn, true);
      (******************* FIX ME
      FDesigner.SelectOnlyThisComponent(btn);
      **********)
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

//    CheckActionsAvailability;
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
    FDesigner.Modified;
  end else
  if TObject(Node.Data) is TJvOutlookBarButton then
  begin
    btn := TJvOutlookBarButton(Node.Data);
    btn.Caption := S;
    FDesigner.Modified;
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

