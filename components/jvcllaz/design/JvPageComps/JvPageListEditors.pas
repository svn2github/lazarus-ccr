{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListEditors.PAS, released on 2004-03-31.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPageListEditors;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ImgList, Menus,
  PropEdits, GraphPropEdits, ComponentEditors,
  JvPageList; //, JvDsgnEditors;

type
  (*
  { a property editor for the ActivePage property of TJvPageList }
  TJvActivePageProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
    *)

  TJvShowDesignCaptionProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  { a component editor for the TJvPageList }

  (********************************** NOT WORKING ***************
  // adapted from TUntabbedNotebookComponentEditor)
  TJvCustomPageListEditor = class(TDefaultComponentEditor)
  protected
    procedure AddNewPageToDesigner(Index: integer); virtual;
    procedure DoAddPage; virtual;
    procedure DoDeletePage; virtual;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function PageList: TJvCustomPageList; virtual;
  end;

  TJvCustomPageEditor = class(TJvCustomPageListEditor)
  public
    function PageList: TJvCustomPageList; override;
    function Page: TJvCustomPage; virtual;
  end;

     {
  TJvCustomPageEditor = class(TComponentEditor)
  private
    procedure InsertPage;
    procedure PrevPage;
    procedure NextPage;
    procedure RemovePage;
    function GetPageControl: TJvCustomPageList;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;    }
  ***********************************************)

  TJvSettingsTreeImagesProperty = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

implementation

uses
  TypInfo,
  JvDsgnConsts, JvPageListTreeView; //, JvPageListEditorForm;

type
  THackPageList = class(TJvPageList);
  THackTreeView = class(TJvCustomPageListTreeView);

const
  cShowEditor = 0;
  cDash = 1;
  cNewPage = 2;
  cNextPage = 3;
  cPrevPage = 4;
  cDelPage = 5;

  cElementCount = 6;

const
  unbvAddPage       = 0;
  unbvDeletePage    = 1;
  unbvShowPage      = 2;


{ TJvCustomPageListEditor }
(****************************************** NOT WORKING
{ adapted from TUntabbedNotebookComponentEditor for LCL's Notebook
 originally named "TJvCustomPageEditor" }

function TJvCustomPageListEditor.PageList: TJvCustomPageList;
begin
  Result := TJvCustomPageList(GetComponent);
end;

procedure TJvCustomPageListEditor.AddNewPageToDesigner(Index: integer);
var
  Hook: TPropertyEditorHook;
  newPage: TJvCustomPage;
  newName: string;
begin
  Hook := nil;
  if not GetHook(Hook) then exit;
  DebugLn('GetHook ' + Hook.ClassName);
  newPage := PageList.Pages[Index];
  newName := GetDesigner.CreateUniqueComponentName(newPage.ClassName);
  newPage.Caption := newName;
  newPage.Name := newName;
  PageList.ActivePageIndex := Index;
  Hook.PersistentAdded(newPage, true);
  Modified;
end;

procedure TJvCustomPageListEditor.DoAddPage;
begin
  DebugLn('DoAddPage: ENTER');
  if not HasHook then exit;

  DebugLn('DoAddPage');

  THackPageList(PageList).AddPage('');
  DebugLn('DoAddPage: PageCount=' + IntToStr(PageList.pageCount));
  AddNewPageToDesigner(PageList.PageCount-1);
  DebugLn('DoAddPage: EXIT');
end;

procedure TJvCustomPageListEditor.DoDeletePage;
var
  Hook: TPropertyEditorHook;
  OldIndex: integer;
  PageComponent: TPersistent;
begin
  OldIndex := PageList.ActivePageIndex;
  if (OldIndex >= 0) and (OldIndex < PageList.PageCount) then
  begin
    if not GetHook(Hook) then exit;
    PageComponent := TPersistent(PageList.Pages[OldIndex]);
    Hook.DeletePersistent(PageComponent);
  end;
end;

procedure TJvCustomPageListEditor.AddMenuItemsForPages(ParentMenuItem: TMenuItem);
var
  n: Integer;
  i: integer;
  NewMenuItem: TMenuItem;
begin
  n := PageList.PageCount;
  ParentMenuItem.Enabled := n > 0;
  for i := 0 to n - 1 do
  begin
    NewMenuItem := TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name := 'ShowPage' + IntToStr(i);
    NewMenuItem.Caption := PageList.Pages[i].Name + ' "' + PageList.Pages[i].Caption + '"';
    NewMenuItem.OnClick := @ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

procedure TJvCustomPageListEditor.ShowPageMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewPageIndex: integer;
begin
  AMenuItem := TMenuItem(Sender);
  if (AMenuItem = nil) or (not (AMenuItem is TMenuItem)) then exit;
  NewPageIndex := AMenuItem.MenuIndex;
  if (NewPageIndex < 0) or (NewPageIndex >= PageList.PageCount) then exit;
  PageList.ActivePageIndex := NewPageIndex;
  GetDesigner.SelectOnlyThisComponent(PageList.Pages[NewPageIndex]);
end;

procedure TJvCustomPageListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    unbvAddPage    : DoAddPage;
    unbvDeletePage : DoDeletePage; // beware: this can free the editor itself
  end;
end;

function TJvCustomPageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    unbvAddPage    : Result := RsNewPage;
    unbvDeletePage : Result := RsDelPage;
    unbvShowPage   : Result := RsShowPage;
  else
    Result := '';
  end;
end;

function TJvCustomPageListEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

procedure TJvCustomPageListEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    unbvAddPage    : ;
    unbvDeletePage : AnItem.Enabled := PageList.ActivePageIndex >= 0;
    unbvShowPage   : AddMenuItemsForPages(AnItem);
  end;
end;


{ TJvCustomPageEditor }
{ adapted from TUNBPageComponentEditor for LCL's Notebook pages }

function TJvCustomPageEditor.PageList: TJvCustomPageList;
var
  lPage: TJvCustomPage;
begin
  lPage := Page;
  if (lPage.Parent <> nil) and (lPage.Parent is TJvCustomPageList) then
    Result := TJvCustomPageList(lPage.Parent)
  else
    Result := nil;
end;

function TJvCustomPageEditor.Page: TJvCustomPage;
begin
  Result := TJvCustomPage(GetComponent);
end;


{

procedure TJvCustomPageEditor.Edit;
begin
  ExecuteVerb(cShowEditor);
end;

procedure TJvCustomPageEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    cShowEditor:
      ShowMessage('NOT IMPLEMENTED');
      // ShowPageListEditor(Designer, GetPageControl);
    cNextPage:
      NextPage;
    cPrevPage:
      PrevPage;
    cNewPage:
      InsertPage;
    cDelPage:
      RemovePage;
  end;
end;

function TJvCustomPageEditor.GetPageControl: TJvCustomPageList;
begin
  if Component is TJvCustomPageList then
    Result := TJvCustomPageList(Component)
  else
    Result := TJvCustomPageList(TJvCustomPage(Component).PageList);
end;

function TJvCustomPageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    cShowEditor:
      Result := RsPageListEditorEllipsis;
    cDash:
      Result := '-';
    cNewPage:
      Result := RsNewPage;
    cNextPage:
      Result := RsNextPageAmp;
    cPrevPage:
      Result := RsPrevPage;
    cDelPage:
      Result := RsDelPage;
  end;
end;

function TJvCustomPageEditor.GetVerbCount: Integer;
begin
  Result := cElementCount; // list, div, new, next, previous, delete
end;

procedure TJvCustomPageEditor.InsertPage;
var
  P: TJvCustomPage;
  C: TJvCustomPageList;
begin
  C := GetPageControl;
  P := C.GetPageClass.Create(Designer.LookupRoot);
  try
    P.Parent := C;
    P.Name := Designer.UniqueName(C.GetPageClass.ClassName);
    P.PageList := C;
    C.ActivePage := P;
  except
    P.Free;
    raise;
  end;
end;

procedure TJvCustomPageEditor.NextPage;
begin
  GetPageControl.NextPage;
end;

procedure TJvCustomPageEditor.PrevPage;
begin
  GetPageControl.PrevPage;
end;

procedure TJvCustomPageEditor.RemovePage;
var
  AList: TJvCustomPageList;
  APage: TJvCustomPage;
begin
  AList := GetPageControl;
  if (AList <> nil) and (AList.ActivePage <> nil) then
  begin
    APage := AList.ActivePage;
//    Designer.SelectComponent(APage);
    Designer.SelectOnlyThisComponent(APage);
    APage.PageList := nil;
    APage.Free;
    Designer.Modified;
  end;
end;
    }
*********************************************************)


//=== { TJvActivePageProperty } ==============================================
                 (*
function TJvActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.GetRoot.ComponentCount - 1 do
  begin
    Component := Designer.GetRoot.Components[I];
    if (Component.Name <> '') and (Component is TJvCustomPage) and
      (TJvCustomPage(Component).PageList = GetComponent(0)) then
      Proc(Component.Name);
  end;
end;
                   *)


//=== { TJvSettingsTreeImagesProperty } ======================================

function TJvSettingsTreeImagesProperty.GetImageList: TCustomImageList;
var
  T: TJvCustomPageListTreeView;
begin
  if (GetComponent(0) is TJvSettingsTreeImages) and
    (TJvSettingsTreeImages(GetComponent(0)).TreeView <> nil) then
  begin
    T := TJvSettingsTreeImages(GetComponent(0)).TreeView;
    Result := THackTreeView(T).Images;
  end
  else
    Result := nil;
end;


//=== { TJvShowDesignCaptionProperty } =======================================

function TJvShowDesignCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  // we don't want sorting for this property
  Result := [paMultiSelect, paValueList, paRevertable];
end;

end.
