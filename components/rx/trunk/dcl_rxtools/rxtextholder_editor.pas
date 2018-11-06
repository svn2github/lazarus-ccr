{ RxTextHolder_Editor

  Copyright (C) 2005-2018 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit RxTextHolder_Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, Menus, Buttons, ActnList, ListFilterEdit, RxTextHolder;

type

  { TRxTextHolder_EditorForm }

  TRxTextHolder_EditorForm = class(TForm)
    Edit1: TEdit;
    itemRemove: TAction;
    itemAdd: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CLabel1: TLabel;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    procedure Edit1Exit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure itemAddExecute(Sender: TObject);
    procedure itemRemoveExecute(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure Memo1Exit(Sender: TObject);
  private
    FTextHolder:TRxTextHolder;
    FCurrentItem: TRxTextHolderItem;
    procedure UpdateTextNames;
    procedure Localize;
    procedure UpdateCtrlState;
    function MakeItemName:string;
  public

  end;


function ShowRxTextHolderEditorForm(ATextHolder:TRxTextHolder):boolean;
implementation
uses rxconst;

function ShowRxTextHolderEditorForm(ATextHolder:TRxTextHolder):boolean;
var
  RxTextHolder_EditorForm: TRxTextHolder_EditorForm;
begin
  RxTextHolder_EditorForm:=TRxTextHolder_EditorForm.Create(Application);
  RxTextHolder_EditorForm.FTextHolder.Assign(ATextHolder);
  RxTextHolder_EditorForm.UpdateTextNames;
  Result:=RxTextHolder_EditorForm.ShowModal = mrOk;
  if Result then
  begin
    ATextHolder.Assign(RxTextHolder_EditorForm.FTextHolder);
  end;
  RxTextHolder_EditorForm.Free;
end;

{$R *.lfm}

{ TRxTextHolder_EditorForm }

procedure TRxTextHolder_EditorForm.FormCreate(Sender: TObject);
begin
  FTextHolder:=TRxTextHolder.Create(Self);
  Localize;
end;

procedure TRxTextHolder_EditorForm.itemAddExecute(Sender: TObject);
begin
  FCurrentItem:=FTextHolder.Items.Add(MakeItemName);
  ListBox1.Items.Add(FCurrentItem.Caption);
  ListBox1.ItemIndex:=ListBox1.Items.Count-1;
  ListBox1Click(nil);
end;

procedure TRxTextHolder_EditorForm.itemRemoveExecute(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(FCurrentItem) then
  begin
    I:=ListBox1.ItemIndex;
    ListBox1.Items.Delete(I);
    FTextHolder.Items.Delete(I);
    if ListBox1.Items.Count > 0 then
      ListBox1.ItemIndex:=0;
    ListBox1Click(nil);
  end;
  UpdateCtrlState;
end;

procedure TRxTextHolder_EditorForm.Edit1Exit(Sender: TObject);
begin
  if Assigned(FCurrentItem) then
  begin
    FCurrentItem.Caption:=Edit1.Text;
    ListBox1.Items[ListBox1.ItemIndex]:=Edit1.Text;
  end;
end;

procedure TRxTextHolder_EditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    Edit1Exit(nil);
    Memo1Exit(nil);
  end;
end;

procedure TRxTextHolder_EditorForm.ListBox1Click(Sender: TObject);
begin
  if (ListBox1.ItemIndex>=0) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
  begin
    FCurrentItem:=FTextHolder.Items[ListBox1.ItemIndex];
    Edit1.Text:=FCurrentItem.Caption;
    Memo1.Text:=FCurrentItem.Lines.Text;
    if Visible then
    begin
      Memo1.Enabled:=true;
      Memo1.SetFocus
    end
    else
      ActiveControl:=Memo1;
  end
  else
  begin
    FCurrentItem:=Nil;
    Memo1.Lines.Clear;
  end;
  UpdateCtrlState;
end;

procedure TRxTextHolder_EditorForm.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  //
end;

procedure TRxTextHolder_EditorForm.Memo1Exit(Sender: TObject);
begin
  if Assigned(FCurrentItem) then
    FCurrentItem.Lines.Assign(Memo1.Lines);
end;

procedure TRxTextHolder_EditorForm.UpdateTextNames;
var
  i: Integer;
begin
  ListBox1.Items.BeginUpdate;
  ListBox1.Items.Clear;
  for i:=0 to FTextHolder.Items.Count-1 do
    ListBox1.Items.Add(FTextHolder.Items[i].Caption);
  ListBox1.Items.EndUpdate;
  if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex:=0;
  ListBox1Click(nil);
end;

procedure TRxTextHolder_EditorForm.Localize;
begin
  Caption:=sRxTextHolderEditor;
  Label1.Caption:=sRxTextHolderTextCaption;
  itemAdd.Caption:=sRxTextHolderAdd;
  itemRemove.Caption:=sRxTextHolderRemove;
end;

procedure TRxTextHolder_EditorForm.UpdateCtrlState;
begin
  itemRemove.Enabled:=(FTextHolder.Items.Count>0) and Assigned(FCurrentItem);
  Edit1.Enabled:=Assigned(FCurrentItem);
  Memo1.Enabled:=Assigned(FCurrentItem);
end;

function TRxTextHolder_EditorForm.MakeItemName: string;
var
  i: Integer;
begin
  i:=0;
  repeat
    Inc(i);
    Result:='Item '+IntToStr(i);
  until (FTextHolder.IndexByName(Result) = -1);
end;

end.

