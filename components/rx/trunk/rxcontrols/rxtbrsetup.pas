{ rxtbrsetup unit

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

unit rxtbrsetup;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  rxtoolbar, StdCtrls, ComCtrls, ExtCtrls, ButtonPanel;

type

  { TToolPanelSetupForm }

  TToolPanelSetupForm = class(TForm)
    btnLeft2: TBitBtn;
    btnLeft: TBitBtn;
    btnRight: TBitBtn;
    btnRight2: TBitBtn;
    btnUp: TBitBtn;
    btnDown: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbShowHint: TCheckBox;
    cbTransp: TCheckBox;
    cbFlatBtn: TCheckBox;
    cbShowCaption: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBtnAvaliable: TListBox;
    ListBtnVisible: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnLeft2Click(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnRight2Click(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBtnAvaliableClick(Sender: TObject);
    procedure cbShowCaptionChange(Sender: TObject);
    procedure ListBtnVisibleDblClick(Sender: TObject);
  private
    procedure FillItems(List:TStrings; AVisible:boolean);
    procedure UpdateStates;
    procedure Localize;
    procedure DoMoveItems(ASrc, ADst:TListBox);
  public
    FToolPanel:TToolPanel;
    constructor CreateSetupForm(AToolPanel:TToolPanel);
  end; 

var
  ToolPanelSetupForm: TToolPanelSetupForm;

implementation
uses rxlclutils, Math, ActnList, rxboxprocs, rxconst, LCLProc, rxShortCutUnit;

{$R *.lfm}

{ TToolPanelSetupForm }

procedure TToolPanelSetupForm.FormResize(Sender: TObject);
begin
  ListBtnVisible.Width:=btnRight2.Left - 4 - ListBtnVisible.Left;
  ListBtnAvaliable.Left:=btnRight2.Left + btnRight2.Width + 4;
  ListBtnAvaliable.Width:=Width - ListBtnAvaliable.Left - 4;
  Label1.Left:=ListBtnAvaliable.Left;
end;

procedure TToolPanelSetupForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Offset, TW:integer;
  P:TToolbarItem;
  BtnRect:TRect;
  Cnv:TCanvas;
  C: TColor;
  S, SText: String;
begin
  Cnv:=(Control as TListBox).Canvas;
  C:=Cnv.Brush.Color;
  TW:=Cnv.TextHeight('Wg');

  Cnv.FillRect(ARect);       { clear the rectangle }
  P:=TToolbarItem((Control as TListBox).Items.Objects[Index]);
  if Assigned(P) then
  begin
    Offset := 2;
    if Assigned(FToolPanel.ImageList) and Assigned(P.Action) then
    begin
      if (P.Action is TCustomAction) and
         (TCustomAction(P.Action).ImageIndex>-1) and
         (TCustomAction(P.Action).ImageIndex < FToolPanel.ImageList.Count) then
      begin
        BtnRect.Top:=ARect.Top + 2;
        BtnRect.Left:=ARect.Left + Offset;
        BtnRect.Right:=BtnRect.Left + FToolPanel.DefImgWidth * 2;
        BtnRect.Bottom:=BtnRect.Top + FToolPanel.DefImgHeight * 2;
        Cnv.Brush.Color := clBtnFace;
        Cnv.FillRect(BtnRect);
        DrawButtonFrame(Cnv, BtnRect, false, false);
        FToolPanel.ImageList.Draw(Cnv, BtnRect.Left + FToolPanel.DefImgWidth div 2,
                                       BtnRect.Top + ((BtnRect.Bottom - BtnRect.Top - FToolPanel.DefImgHeight) div 2),
                                       TCustomAction(P.Action).ImageIndex, True);
        Offset:=BtnRect.Right;
      end;
    end;

    if P.ButtonStyle in [tbrSeparator, tbrDivider] then
      SText:=sSeparator
    else
    if Assigned(P.Action) and (P.Action is TCustomAction) then
      SText:=TCustomAction(P.Action).Caption
    else
      SText:=(Control as TListBox).Items[Index];




    Offset := Offset + 6;
    Cnv.Brush.Color:=C;
    Cnv.TextOut(ARect.Left + Offset, (ARect.Top + ARect.Bottom  - TW) div 2, SText);  { display the text }

    if (P.Action is TAction) then
      if TAction(P.Action).ShortCut <> 0 then
      begin
        S:=ShortCutToText(TAction(P.Action).ShortCut);
        if S<> '' then
          Cnv.TextOut(ARect.Right - Cnv.TextWidth(S) - 2, (ARect.Top + ARect.Bottom  - TW) div 2, S);  { display the shortut caption }
      end;
  end;
end;

procedure TToolPanelSetupForm.ListBtnAvaliableClick(Sender: TObject);
begin
  with (Sender as TListBox) do
  begin
    if (ItemIndex>-1) and (ItemIndex<Items.Count) then
    begin
      if Assigned(TToolbarItem(Items.Objects[ItemIndex]).Action) then
        Panel1.Caption:=TCustomAction(TToolbarItem(Items.Objects[ItemIndex]).Action).Hint
      else
        Panel1.Caption:='';
      if Sender = ListBtnVisible then
        cbShowCaption.Checked:=TToolbarItem(Items.Objects[ItemIndex]).ShowCaption;
    end;
  end;
  UpdateStates;
end;

procedure TToolPanelSetupForm.cbShowCaptionChange(Sender: TObject);
begin
  if (ListBtnVisible.ItemIndex>-1) and (ListBtnVisible.ItemIndex<ListBtnVisible.Items.Count) then
    TToolbarItem(ListBtnVisible.Items.Objects[ListBtnVisible.ItemIndex]).ShowCaption:=cbShowCaption.Checked;
end;

procedure TToolPanelSetupForm.ListBtnVisibleDblClick(Sender: TObject);
var
  Act: TBasicAction;
  A: TShortCut;
begin
  if FToolPanel.CustomizeShortCut then
  if (TListBox(Sender).ItemIndex>-1) and (TListBox(Sender).ItemIndex<TListBox(Sender).Items.Count) then
  begin
    Act:=TToolbarItem(TListBox(Sender).Items.Objects[TListBox(Sender).ItemIndex]).Action;
    if Act is TCustomAction then
    begin
      A:=TCustomAction(Act).ShortCut;
      if RxSelectShortCut(A) then
      begin
        TCustomAction(Act).ShortCut:=A;
        TListBox(Sender).Invalidate;
      end;
    end;
  end;
end;

procedure TToolPanelSetupForm.FillItems(List: TStrings; AVisible: boolean);
var
  TI: TToolbarItem;
begin
  List.Clear;
  for TI in FToolPanel.Items do
    if (TI.Visible = AVisible) then
      if Assigned(TI.Action) then
        List.AddObject(TI.Action.Name, TI)
      else
        List.AddObject('Separator', TI);
end;

procedure TToolPanelSetupForm.UpdateStates;
var
  i:integer;
begin
  for I:=0 to ListBtnVisible.Items.Count - 1 do
    TToolbarItem(ListBtnVisible.Items.Objects[i]).Visible:=true;

  for I:=0 to ListBtnAvaliable.Items.Count - 1 do
    TToolbarItem(ListBtnAvaliable.Items.Objects[i]).Visible:=false;
    

  btnRight2.Enabled:=ListBtnVisible.Items.Count>0;
  btnRight.Enabled:=ListBtnVisible.Items.Count>0;

  btnLeft.Enabled:=ListBtnAvaliable.Items.Count>0;
  btnLeft2.Enabled:=ListBtnAvaliable.Items.Count>0;

  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;

  btnUp.Enabled:=(ListBtnVisible.Items.Count>0) and (ListBtnVisible.ItemIndex>0);
  btnDown.Enabled:=(ListBtnVisible.Items.Count>0) and (ListBtnVisible.ItemIndex < ListBtnVisible.Items.Count-1) and (ListBtnVisible.ItemIndex >-1);

  if (ListBtnVisible.ItemIndex>=0) and (ListBtnVisible.ItemIndex<ListBtnVisible.Items.Count) then
    cbShowCaption.Enabled:=not (TToolbarItem(ListBtnVisible.Items.Objects[ListBtnVisible.ItemIndex]).ButtonStyle in [tbrSeparator, tbrDivider])
  else
    cbShowCaption.Enabled:=false;
end;

procedure TToolPanelSetupForm.Localize;
begin
  Caption:=sToolPanelSetup;
  TabSheet1.Caption:=sVisibleButtons;
  TabSheet2.Caption:=sOptions;
  Label2.Caption:=sVisibleButtons;
  Label2.Caption:=sVisibleButtons;
  Label1.Caption:=sAvaliableButtons;
  cbShowCaption.Caption:=sShowCaption;
  RadioGroup2.Caption:=sToolBarStyle;
  RadioGroup2.Items.Clear;
  RadioGroup2.Items.Add(sToolBarStyle1);
  RadioGroup2.Items.Add(sToolBarStyle2);
  RadioGroup2.Items.Add(sToolBarStyle3);
  cbFlatBtn.Caption:=sFlatButtons;
  cbTransp.Caption:=sTransparent;
  cbShowHint.Caption:=sShowHint;
  RadioGroup1.Caption:=sButtonAlign;
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.Add(sButtonAlign1);
  RadioGroup1.Items.Add(sButtonAlign2);
  RadioGroup1.Items.Add(sButtonAlign3);
end;

procedure TToolPanelSetupForm.DoMoveItems(ASrc, ADst: TListBox);
var
  BtnIndex: Integer;
  S: String;
  P: TToolbarItem;
begin
  BtnIndex:=ASrc.ItemIndex;
  if (ASrc.Items.Count>0) and (BtnIndex>=0) and (BtnIndex < ASrc.Items.Count) then
  begin
    S:=ASrc.Items[BtnIndex];
    P:=TToolbarItem(ASrc.Items.Objects[BtnIndex]);

    ADst.Items.AddObject(S, P);

    ASrc.Items.Delete(BtnIndex);

    if ASrc = ListBtnAvaliable then
    begin
      FToolPanel.VisibleItems.Add(P);
      P.Visible:=true;
    end
    else
    begin
      FToolPanel.VisibleItems.Remove(P);
      P.Visible:=false;
    end;

    if ASrc.Items.Count > 0 then
      ASrc.ItemIndex:=Min(ASrc.Items.Count-1, BtnIndex);

    ADst.ItemIndex:=ADst.Items.Count-1;
  end;
end;

procedure TToolPanelSetupForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TToolPanelSetupForm.CheckBox1Change(Sender: TObject);
var
  tpo:TToolPanelOptions;
begin
  tpo:=FToolPanel.Options;
  if cbTransp.Checked then
    tpo:=tpo + [tpTransparentBtns]
  else
    tpo:=tpo - [tpTransparentBtns];

  FToolPanel.ToolBarStyle:=TToolBarStyle(RadioGroup2.ItemIndex);

  if cbFlatBtn.Checked then
    tpo:=tpo + [tpFlatBtns]
  else
    tpo:=tpo - [tpFlatBtns];

  FToolPanel.ShowHint:=cbShowHint.Checked;
  FToolPanel.Options:=tpo;
  
  FToolPanel.ButtonAllign:=TToolButtonAllign(RadioGroup1.ItemIndex);
  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;
end;

procedure TToolPanelSetupForm.btnLeftClick(Sender: TObject);
begin
  DoMoveItems(ListBtnAvaliable, ListBtnVisible);
  UpdateStates;
  FToolPanel.ReAlign;
end;

procedure TToolPanelSetupForm.btnLeft2Click(Sender: TObject);
begin
  if (ListBtnAvaliable.Items.Count>0) and (ListBtnAvaliable.ItemIndex<0) then
    ListBtnAvaliable.ItemIndex:=0;

  while ListBtnAvaliable.Items.Count>0 do
    DoMoveItems(ListBtnAvaliable, ListBtnVisible);
  UpdateStates;
  FToolPanel.ReAlign;
end;

procedure TToolPanelSetupForm.btnRightClick(Sender: TObject);
begin
  DoMoveItems(ListBtnVisible, ListBtnAvaliable);
  UpdateStates;
  FToolPanel.ReAlign;
end;

procedure TToolPanelSetupForm.btnRight2Click(Sender: TObject);
begin
  if (ListBtnVisible.Items.Count>0) and (ListBtnVisible.ItemIndex<0) then
    ListBtnVisible.ItemIndex:=0;

  while ListBtnVisible.Items.Count>0 do
    DoMoveItems(ListBtnVisible, ListBtnAvaliable);
  UpdateStates;
  FToolPanel.ReAlign;
end;

procedure TToolPanelSetupForm.btnUpClick(Sender: TObject);
var
  I, J: Integer;
  P: TToolbarItem;
begin
  I:=ListBtnVisible.ItemIndex;
  J:=I + TComponent(Sender).Tag;
  ListBtnVisible.ItemIndex:=J;

  UpdateStates;

  FToolPanel.VisibleItems.Exchange(I, J);
  FToolPanel.ReAlign;
end;

constructor TToolPanelSetupForm.CreateSetupForm(AToolPanel: TToolPanel);
begin
  inherited Create(AToolPanel);
  Localize;
  PageControl1.ActivePageIndex:=0;
  FormResize(nil);
  FToolPanel:=AToolPanel;

  RxAssignBitmap(btnUp.Glyph, 'rx_up');
  RxAssignBitmap(btnDown.Glyph, 'rx_down');
  RxAssignBitmap(btnRight.Glyph, 'rx_right');
  RxAssignBitmap(btnRight2.Glyph, 'rx_right2');
  RxAssignBitmap(btnLeft.Glyph, 'rx_left');
  RxAssignBitmap(btnLeft2.Glyph, 'rx_left2');

  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;
  cbTransp.Checked:=tpTransparentBtns in FToolPanel.Options;
  cbShowHint.Checked:=FToolPanel.ShowHint;

  ListBtnAvaliable.ItemHeight:=FToolPanel.DefImgHeight*2 + 4;
  ListBtnVisible.ItemHeight:=FToolPanel.DefImgHeight*2 + 4;

  FillItems(ListBtnVisible.Items, true);
  FillItems(ListBtnAvaliable.Items, false);

  RadioGroup1.ItemIndex:=Ord(FToolPanel.ButtonAllign);
  RadioGroup2.ItemIndex:=Ord(FToolPanel.ToolBarStyle);

  UpdateStates;

  cbFlatBtn.OnChange:=@CheckBox1Change;
  cbTransp.OnChange:=@CheckBox1Change;
  cbShowHint.OnChange:=@CheckBox1Change;
  RadioGroup1.OnClick:=@CheckBox1Change;
  RadioGroup2.OnClick:=@CheckBox1Change;

end;

end.

