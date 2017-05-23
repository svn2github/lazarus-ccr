{*********************************************************}
{*                VPTASKEDITDLG.PAS 1.03                 *}
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

unit VpTaskEditDlg;
  { default task editing dialog }

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources, EditBtn,
  {$ELSE}
  Windows, Messages, VpEdPop, VpDateEdit,
  {$ENDIF}
  SysUtils,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, VpData, StdCtrls, ExtCtrls, ComCtrls,
  VpBase, VpSR, VpDlg;

type
  { forward declarations }
  TVpTaskEditDialog = class;

  { TTaskEditForm }

  TTaskEditForm = class(TForm)
    ButtonPanel: TPanel;
    CbCategory: TComboBox;
    CbPriority: TComboBox;
    LblCategory: TLabel;
    LblPriority: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    PageControl1: TPageControl;
    tabTask: TTabSheet;
    DescriptionEdit: TEdit;
    DueDateLbl: TLabel;
    DueDateEdit: TDateEdit;
    CbComplete: TCheckBox;
    LblCreatedOn: TLabel;
    LblCompletedOn: TLabel;
    DetailsMemo: TMemo;
    ResourceNameLbl: TLabel;
    Bevel1: TBevel;
    imgCalendar: TImage;
    imgCompleted: TImage;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    FReturnCode: TVpEditorReturnCode;
    FTask: TVpTask;
    FResource: TVpResource;
//    FBtnHeight: Integer;
//    FBtnWidth: Integer;
//    FEditHeight: Integer;
    procedure PositionControls;
    procedure SetCaptions;
  public
    procedure PopulateSelf;
    procedure DePopulateSelf;
    property Task: TVpTask
      read FTask write FTask;
    property Resource: TVpResource
      read FResource write FResource;
    property ReturnCode: TVpEditorReturnCode
      read FReturnCode;
  end;

  TVpTaskEditDialog = class(TVpBaseDialog)
  protected {private}
    teEditDlg: TTaskEditForm;
    teTask: TVpTask;
  public
    constructor Create(AOwner: TComponent); override;
    function AddNewTask: Boolean;
    function Execute(Task: TVpTask): Boolean; reintroduce;
  published
    {properties}
    property DataStore;
    property Options;
    property Placement;
  end;

implementation

uses
  Math, VpMisc;

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

{ TTaskEditForm }

procedure TTaskEditForm.FormCreate(Sender: TObject);
begin
  FReturnCode := rtAbandon;
//  FBtnHeight := ScaleY(OKBtn.Height, DesignTimeDPI);
//  FEditHeight := ScaleY(DueDateEdit.Height, DesignTimeDPI);
end;

procedure TTaskEditForm.DePopulateSelf;
begin
  Task.Description := DescriptionEdit.Text;
  Task.DueDate := DueDateEdit.Date;
  Task.Details := DetailsMemo.Text;
  Task.Complete := CbComplete.Checked;
  Task.Priority := CbPriority.ItemIndex-1;
  Task.Category := CbCategory.ItemIndex;
//  DueDateLbl.Caption := RSDueDateLabel;
end;
{=====}

procedure TTaskEditForm.SetCaptions;
var
  ct: TVpCategoryType;
  tp: TVpTaskPriority;
begin
  ResourceNameLbl.Caption := Resource.Description;
  CbComplete.Caption := RSTaskComplete;
  DueDateLbl.Caption := RSDueDateLabel;
  OKBtn.Caption := RSOKBtn;
  CancelBtn.Caption := RSCancelBtn;
  TabTask.Caption := RSDlgTaskEdit;
  LblPriority.Caption := RSPriorityLabel;
  LblCategory.Caption := RSCategoryLabel;

  CbCategory.Items.Clear;
  for ct in TVpCategoryType do
    CbCategory.Items.Add(CategoryLabel(ct));

  CbPriority.Items.Clear;
  for tp in TVpTaskPriority do
    CbPriority.Items.Add(TaskPriorityToStr(tp));
end;

procedure TTaskEditForm.PopulateSelf;
begin
  SetCaptions;

  DescriptionEdit.Text := Task.Description;
  DueDateEdit.Date := Task.DueDate;
  DetailsMemo.Text := Task.Details;
  CbComplete.Checked := Task.Complete;
  if Task.CompletedOn <> 0 then
    LblCompletedOn.Caption := RSCompletedOn + ' ' + FormatDateTime('ddddd', Task.CompletedOn);
  LblCompletedOn.Visible := CbComplete.Checked;
  LblCreatedOn.Caption := RSCreatedOn + ' ' + FormatDateTime('ddddd', Task.CreatedOn);
  CbPriority.ItemIndex := Task.Priority + 1;
  CbCategory.ItemIndex := Task.Category;
end;

procedure TTaskEditForm.PositionControls;
var
  i, w: Integer;
  cnv: TControlCanvas;
begin
  AutoSize := false;
  DueDateEdit.ButtonWidth := DueDateEdit.Height;

  cnv := TControlCanvas.Create;
  try
    cnv.Control := DueDateEdit;
    cnv.Font.Assign(DueDateEdit.Font);
    w := cnv.TextWidth(' 99-99-9999 ') + DueDateEdit.ButtonWidth + 10;
    DueDateEdit.Width := w;
  finally
    cnv.Free;
  end;

  cnv := TControlCanvas.Create;
  try
    cnv.Control := CbCategory;
    cnv.Font.Assign(CbCategory.Font);
    w := 0;
    for i :=0 to CbCategory.Items.Count - 1 do
      w := max(w, cnv.TextWidth(CbCategory.Items[i]));
    inc(w, GetSystemMetrics(SM_CXVSCROLL)*2);
    w := Max(w, DueDateEdit.Width);
    CbCategory.Width := w;
  finally
    cnv.Free;
  end;

  cnv := TControlCanvas.Create;
  try
    cnv.Control := CbPriority;
    cnv.Font.Assign(CbPriority.Font);
    w := 0;
    for i :=0 to CbPriority.Items.Count - 1 do
      w := max(w, cnv.TextWidth(CbPriority.Items[i]));
    inc(w, GetSystemMetrics(SM_CXVSCROLL)*2);
    CbPriority.Width := w;
  finally
    cnv.Free;
  end;

  AlignOKCancel(OKBtn, CancelBtn, ButtonPanel);

  {$IFDEF NEW_ICONS}
  LoadImageFromRCDATA(imgCalendar, 'VpAlarmClock', 32, 48, 64);
  LoadImageFromRCDATA(imgCompleted, 'VpFinished', 32, 48, 64);
  LoadGlyphFromRCDATA(DueDateEdit.Button.Glyph, 'VpDateEdit', 16, 24, 32);
  {$ENDIF}

  AutoSize := true;
end;
{=====}

procedure TTaskEditForm.OnChange(Sender: TObject);
begin
  Task.Changed := true;
end;

{=====}

procedure TTaskEditForm.OKBtnClick(Sender: TObject);
begin
  FReturnCode := rtCommit;
  Close;
end;
{=====}

procedure TTaskEditForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TTaskEditForm.FormShow(Sender: TObject);
begin
  PositionControls;
  DescriptionEdit.SetFocus;
end;
{=====}

{ TVpTaskEditDialog }

constructor TVpTaskEditDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FPlacement.Height := 340;
  FPlacement.Width := 545;
end;

function TVpTaskEditDialog.Execute(Task: TVpTask): Boolean;
var
  TaskEditForm: TTaskEditForm;
begin
  Result := false;
  teTask := Task;
  if (teTask <> nil) and (DataStore <> nil) and (DataStore.Resource <> nil) then
  begin
    Application.CreateForm(TTaskEditForm, TaskEditForm);
    try
      DoFormPlacement(TaskEditForm);
      SetFormCaption(TaskEditForm, Task.Description, RSDlgTaskEdit);
      TaskEditForm.Task := Task;
      TaskEditForm.Resource := DataStore.Resource;
      TaskEditForm.PopulateSelf;
      TaskEditForm.ShowModal;
      Result := (TaskEditForm.ReturnCode = rtCommit);
      Task.Changed := Result;
      if Result then begin
        TaskEditForm.DePopulateSelf;
//        DataStore.PostTasks;
//        DataStore.NotifyDependents;
      end;
    finally
      TaskEditForm.Release;
    end;
  end;
end;
{=====}

function TVpTaskEditDialog.AddNewTask: Boolean;
begin
  result := false;
  if DataStore <> nil then begin
    teTask := DataStore.Resource.Tasks.AddTask(DataStore.GetNextID('Tasks'));
    if teTask <> nil then begin
      Result := Execute(teTask);
      if not Result then
        teTask.Free;
    end;
  end;
end;
{=====}

end.
  
