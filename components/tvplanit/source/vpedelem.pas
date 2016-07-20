{*********************************************************}
{*                VPEDELEM.PAS 1.03                      *}
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

unit VpEdElem;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  VpBase, VpSR, VpPrtFmt, ComCtrls;

type

  { TfrmEditElement }

  TfrmEditElement = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    btnShape: TButton;
    edName: TEdit;
    gbDayOffset: TGroupBox;
    lblName: TLabel;
    rgDayOffsetUnit: TRadioGroup;
    rgItemType: TRadioGroup;
    gbVisual: TGroupBox;
    LblTop: TLabel;
    LblLeft: TLabel;
    LblHeight: TLabel;
    LblWidth: TLabel;
    rgMeasurement: TRadioGroup;
    rgRotation: TRadioGroup;
    edTop: TEdit;
    edLeft: TEdit;
    edHeight: TEdit;
    edWidth: TEdit;
    chkVisible: TCheckBox;
    gbCaption: TGroupBox;
    btnCaptionFont: TButton;
    FontDialog1: TFontDialog;
    edCaptionText: TEdit;
    lblCaptionText: TLabel;
    edOffset: TEdit;
    udOffset: TUpDown;
    udTop: TUpDown;
    udLeft: TUpDown;
    udHeight: TUpDown;
    udWidth: TUpDown;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgItemTypeClick(Sender: TObject);
    procedure btnShapeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCaptionFontClick(Sender: TObject);
    procedure edCaptionTextChange(Sender: TObject);
    procedure rgMeasurementClick(Sender: TObject);
    procedure PosEditExit(Sender: TObject);
    procedure PosEditEnter(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
  private
    procedure PositionControls;
    procedure SetCaptions;
    procedure SetMaxSpin(Spin: Integer);
  protected
    TheShape: TVpPrintShape;
    TheCaption: TVpPrintCaption;
    CurEdit: TEdit;

    MaxSpin: Integer;
    procedure SaveData(AnElement: TVpPrintFormatElementItem);
    procedure SetData(AnElement: TVpPrintFormatElementItem);
    procedure SetItemType(Index: Integer);
    function Validate: Boolean;
    { Private declarations }
  public
    function Execute(AnElement : TVpPrintFormatElementItem) : Boolean;
    { Public declarations }
  end;


implementation

uses
  Math, VpMisc, VpEdShape;

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

function EvalFmt(Val : Extended) : string;
begin
  Result := FormatFloat('0.00', Val);
end;
{=====}
procedure TfrmEditElement.FormCreate(Sender: TObject);
begin
  btnShape.Enabled := False;

  gbCaption.Enabled := False;
  edCaptionText.Enabled := False;
  lblCaptionText.Enabled := False;
  btnCaptionFont.Enabled := False;

  SetCaptions;
end;
{=====}
procedure TfrmEditElement.FormShow(Sender: TObject);
begin
  PositionControls;
  edName.SetFocus;
end;
{=====}
procedure TfrmEditElement.btnCaptionFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    TheCaption.Font := FontDialog1.Font;
end;
{=====}
procedure TfrmEditElement.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{=====}
procedure TfrmEditElement.btnOkClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk
  else begin
    ShowMessage(RSNeedElementName);
    edName.SetFocus;
    Exit;
  end;
end;
{=====}
procedure TfrmEditElement.btnShapeClick(Sender: TObject);
var
  frmEditShape: TfrmEditShape;
begin
  Application.CreateForm(TfrmEditShape, frmEditShape);
  frmEditShape.Execute(TheShape);
  frmEditShape.Free;
end;
{=====}
procedure TfrmEditElement.edCaptionTextChange(Sender: TObject);
begin
  TheCaption.Caption := edCaptionText.Text;
end;
{=====}
function TfrmEditElement.Execute(AnElement : TVpPrintFormatElementItem) : Boolean;
begin
  SetData(AnElement);
  Result := ShowModal = mrOk;
  if Result then
    SaveData(AnElement);
end;
{=====}
procedure TfrmEditElement.PosEditEnter(Sender: TObject);
begin
  CurEdit := (Sender as TEdit);
end;
{=====}
procedure TfrmEditElement.PosEditExit(Sender: TObject);
var
  ed : TEdit;
  Val : Extended;
begin
  ed := (Sender as TEdit);
  try
    Val := StrToFloat(ed.Text);
    if Val > MaxSpin then begin
      ed.Text := EvalFmt(MaxSpin);
    end else
    if Val < 0.0 then begin
      ed.Text := EvalFmt(0);
    end;
  except
    on EConvertError do begin
      ShowMessage('Please Enter a Floating Point Value');
      ed.SetFocus;
    end;
  end;
end;
{=====}
procedure TfrmEditElement.rgItemTypeClick(Sender: TObject);
begin
  SetItemType(rgItemType.ItemIndex);
end;
{=====}
procedure TfrmEditElement.rgMeasurementClick(Sender: TObject);
begin
  SetMaxSpin(rgMeasurement.ItemIndex);
end;
{=====}
procedure TfrmEditElement.SaveData(AnElement : TVpPrintFormatElementItem);
begin
  AnElement.ElementName := edName.Text;

  AnElement.DayOffset := udOffset.Position;

  AnElement.Top   := StrToFloat(edTop.Text);
  AnElement.Left  := StrToFloat(edLeft.Text);
  AnElement.Height:= StrToFloat(edHeight.Text);
  AnElement.Width := StrToFloat(edWidth.Text);

  AnElement.ItemType       :=  TVpItemType(rgItemType.ItemIndex);

  AnElement.DayOffsetUnits :=  TVpDayUnits(rgDayOffsetUnit.ItemIndex);
  AnElement.Rotation       :=  TVpRotationAngle(rgRotation.ItemIndex);
  AnElement.Measurement    :=  TVpItemMeasurement(rgMeasurement.ItemIndex);

  AnElement.Visible := chkVisible.Checked;
end;

procedure TfrmEditElement.SetCaptions;
begin
  Caption := RSEditElementCaption;

  lblName.Caption := RSNameLbl;

  rgItemType.Caption := RSElementTypeLbl;
  rgItemType.Items[0] := RSDayViewElement;
  rgItemType.Items[1] := RSWeekViewElement;
  rgItemType.Items[2] := RSMonthViewElement;
  rgItemType.Items[3] := RSCalendarElement;
  rgItemType.Items[4] := RSShapeElement;
  rgItemType.Items[5] := RSCaptionElement;
  rgItemType.Items[6] := RSTasksElement;
  rgItemType.Items[7] := RSContactsElement;

  gbDayOffset.Caption := RSTimeIncLbl;
  rgDayOffsetUnit.Caption := RSTimeIncUnits;
  rgDayOffsetUnit.Items[0] := RSDays;
  rgDayOffsetUnit.Items[1] := RSWeeks;
  rgDayOffsetUnit.Items[2] := RSMonths;
  rgDayOffsetUnit.Items[3] := RSYears;

  gbVisual.Caption := RSVisualCaption;
  rgRotation.Caption := RSRotationCaption;
  rgMeasurement.Caption := RSMeasurementCaption;
  rgMeasurement.Items[0] := RSPixels;
  rgMeasurement.Items[1] := RSPercent;
  rgMeasurement.Items[2] := RSInches;
  lblLeft.Caption := RSLeft;
  lblTop.Caption := RSTop;
  lblWidth.Caption := RSWidth;
  lblHeight.Caption := RSHeight;
  chkVisible.Caption := RSVisible;

  gbCaption.Caption := RSCaption;
  lblCaptionText.Caption := RSTextCaption;
  btnCaptionFont.Caption := RSFontBtn;
  btnShape.Caption := RSShapeBtn;
  btnOK.Caption := RSOKBtn;
  btnCancel.Caption := RSCancelBtn;
end;

procedure TfrmEditElement.PositionControls;
const
  MARGIN = 16;
  DELTA = 8;
  RADIOITEM_CORRECTION = 24 + DELTA;
  BUTTON_CORRECTION = 16;
  GROUPBOX_CORRECTION = 16;
  GROUPBOX_DISTANCE = 16;
var
  i, w, h, hEd, hBtn: Integer;
  cnv: TControlCanvas;
  rb: TRadioButton;
begin
  // Fix edit heights at higher dpi
  with TEdit.Create(self) do
  try
    Parent := self;
    hEd := Height;
  finally
    free;
  end;
  edName.Height := hEd;
  edOffset.Height := hEd;
  udOffset.Height := hEd;
  edTop.Height := hEd;
  edLeft.Height := hEd;
  edHeight.Height := hEd;
  edWidth.Height := hEd;
  udTop.Height := hEd;
  udLeft.Height := hEd;
  udHeight.Height := hEd;
  udWidth.Height := hEd;
  edCaptionText.Height := hEd;

  // Fix button heights a higher dpi
  with TButton.Create(self) do
  try
    Parent := self;
    hBtn := Height;
  finally
    Free;
  end;
  btnOK.Height := hBtn;
  btnCancel.Height := hBtn;
  btnShape.Height := hBtn;
  btnCaptionFont.Height := hBtn;

  cnv := TControlCanvas.Create;
  try
    cnv.Control := rgItemType;

    // Calculate with of ItemType groupbbox
    cnv.Font.Assign(rgItemType.Font);
    w := 0;
    for i:=0 to rgItemType.Items.Count - 1 do
      w := Max(w, cnv.TextWidth(rgItemType.Items[i]));
    rgItemType.ClientWidth := rgItemType.Columns * (w + RADIOITEM_CORRECTION);

    // Calculate width of Visual groupbox
    cnv.Font.Assign(rgRotation.Font);
    rgRotation.ClientWidth := Max(
      cnv.TextWidth(RSRotationCaption) + GROUPBOX_CORRECTION,
      cnv.TextWidth('270') + RADIOITEM_CORRECTION
    );

    cnv.Font.Assign(rgMeasurement.Font);
    w := 0;
    for i:=0 to RgMeasurement.Items.Count-1 do
      w := Max(w, cnv.TextWidth(RgMeasurement.Items[i]));
    rgMeasurement.ClientWidth := Max(
      cnv.TextWidth(RSMeasurementCaption) + GROUPBOX_CORRECTION,
      w + RADIOITEM_CORRECTION
    );
    rgMeasurement.Left := rgRotation.Left + rgRotation.Width + GROUPBOX_DISTANCE;

    w := Max(GetLabelWidth(lblTop), GetLabelWidth(lblLeft)) + DELTA + EdTop.Width + udTop.Width + 24 +
         Max(GetLabelWidth(lblHeight), GetLabelWidth(lblWidth) + DELTA) + EdHeight.Width + udHeight.Width;

    gbVisual.ClientWidth := RightOf(rgMeasurement) + 24 + w + rgRotation.Left;

    // The longest box determines the width of the form
    if gbVisual.ClientWidth > rgItemType.ClientWidth then
      rgItemType.ClientWidth := gbVisual.ClientWidth
    else
      gbVisual.ClientWidth := rgItemType.ClientWidth;

    // Width of the form
    ClientWidth := gbVisual.ClientWidth + MARGIN * 2;

    // Position Left/Top etc controls
    edTop.Left := (gbVisual.ClientWidth + RightOf(rgMeasurement) - w) div 2 +
      Max(GetLabelWidth(lblTop), GetLabelWidth(lblLeft)) + DELTA;
    edLeft.Left := edTop.Left;
    udTop.Left := RightOf(edTop);
    udLeft.Left := udTop.Left;
    lblTop.Left := edTop.Left - GetLabelWidth(lblTop) - DELTA;
    lblLeft.Left := edTop.Left - GetLabelWidth(lblLeft) - DELTA;
    chkVisible.Left := edLeft.Left;

    edHeight.Left := (gbVisual.ClientWidth + RightOf(rgMeasurement) + w ) div 2 - udHeight.Width - edHeight.Width;
    edWidth.Left := edHeight.Left;
    lblHeight.Left := edHeight.Left - GetLabelWidth(lblHeight) - DELTA;
    lblWidth.Left := edHeight.Left - GetLabelWidth(lblWidth) - DELTA;
    udHeight.Left := RightOf(edHeight);
    udWidth.Left := RightOf(edWidth);

    // Name
    LblName.Left := MARGIN;
    EdName.Left := LblName.Left + GetLabelWidth(LblName) + DELTA;
    EdName.Width := RightOf(gbVisual) - EdName.Left;

    // DayOffset groupbox
    cnv.Font.Assign(gbDayOffset.Font);
    gbDayOffset.Width := Max(RightOf(udOffset) + DELTA, cnv.TextWidth(gbDayOffset.Caption) + GROUPBOX_CORRECTION);

    // Day Offset Unit groupbox
    rgDayOffsetUnit.Left := RightOf(gbDayOffset) + GROUPBOX_DISTANCE;
    rgDayOffsetUnit.Width := RightOf(gbVisual) - rgDayOffsetUnit.Left;

    // Caption groupbox
    gbCaption.Width := gbVisual.Width;
    lblCaptionText.Left := DELTA;
    edCaptionText.Left := lblCaptionText.Left + GetLabelWidth(lblCaptionTExt) + DELTA;
    cnv.Font.Assign(btnCaptionFont.Font);
    w := cnv.TextWidth(btnCaptionFont.Caption) + BUTTON_CORRECTION;
    btnCaptionFont.Width := w;
    btnCaptionFont.Left := gbCaption.ClientWidth - DELTA - w;
    edCaptionText.Width := btnCaptionFont.Left - DELTA - edCaptionText.Left;

    // Buttons at the bottom
    w := Max(GetButtonWidth(btnOK), GetButtonWidth(btnCancel));
    btnOK.Width := w;
    btnCancel.Width := w;
    {
    cnv.Font.Assign(btnOK.Font);
    w := Max(cnv.TextWidth(btnOK.Caption), cnv.TextWidth(btnCancel.Caption));
    btnOK.Width := w + BUTTON_CORRECTION;
    btnCancel.Width := btnOK.Width;
    }
    btnCancel.Left := RightOf(gbCaption) - btnCancel.Width;
    btnOK.Left := btnCancel.Left - DELTA - btnOK.Width;
    btnShape.Width := cnv.TextWidth(btnShape.Caption) + BUTTON_CORRECTION;

    // Height
    gbDayOffset.ClientHeight := edOffset.Height + DELTA*2;
    rgDayOffsetUnit.Height := gbDayOffset.Height;

    rb := TRadioButton.Create(self);
    try
      rb.Parent := self;
      h := rb.Height;
    finally
      rb.Free;
    end;
    rgRotation.Height := 4*h + 28;
    rgMeasurement.Height := rgRotation.Height;
    gbVisual.ClientHeight := BottomOf(rgMeasurement) + DELTA;

    gbCaption.Top := BottomOf(gbVisual) + DELTA;
    gbCaption.ClientHeight := BottomOf(edCaptionText) + DELTA;

    btnOK.Top := BottomOf(gbCaption) + DELTA;
    btnCancel.Top := btnOK.Top;
    btnShape.Top := btnOK.Top;

    ClientHeight := BottomOf(btnOK) + DELTA;
  finally
    cnv.Free;
  end;
end;

procedure TfrmEditElement.SetData(AnElement : TVpPrintFormatElementItem);
begin
  edName.Text := AnElement.ElementName;

  udOffset.Position := AnElement.DayOffset;

  rgItemType.ItemIndex := Ord(AnElement.ItemType);
  TheShape := AnElement.Shape;
  TheCaption := AnElement.Caption;

  rgDayOffsetUnit.ItemIndex := Ord(AnElement.DayOffsetUnits);
  rgRotation.ItemIndex := Ord(AnElement.Rotation);
  rgMeasurement.ItemIndex := Ord(AnElement.Measurement);
  SetMaxSpin(rgMeasurement.ItemIndex);

  edTop.Text := EvalFmt(AnElement.Top);
  udTop.Position := Trunc(AnElement.Top);
  edLeft.Text := EvalFmt(AnElement.Left);
  udLeft.Position := Trunc(AnElement.Left);
  edHeight.Text := EvalFmt(AnElement.Height);
  udHeight.Position := Trunc(AnElement.Height);
  edWidth.Text := EvalFmt(AnElement.Width);
  udWidth.Position := Trunc(AnElement.Width);

  edCaptionText.Text := AnElement.Caption.Caption;
  FontDialog1.Font := AnElement.Caption.Font;

  chkVisible.Checked := AnElement.Visible;
end;
{=====}
procedure TfrmEditElement.SetItemType(Index : Integer);
begin
  rgItemType.ItemIndex := Index;
  gbCaption.Enabled := False;
  edCaptionText.Enabled := False;
  lblCaptionText.Enabled := False;
  btnCaptionFont.Enabled := False;


  btnShape.Enabled := Index = 4;
  if Index = 5 then begin
    gbCaption.Enabled := True;
    edCaptionText.Enabled := True;
    lblCaptionText.Enabled := True;
    btnCaptionFont.Enabled := True;
  end;
end;
{=====}
procedure TfrmEditElement.SetMaxSpin(Spin : Integer);
begin
  case Spin of
    0: MaxSpin := 2000;
    1: MaxSpin := 100;
    2: MaxSpin := 50;
  end;

  udLeft.Max := MaxSpin;
  udTop.Max := MaxSpin;
  udHeight.Max := MaxSpin;
  udWidth.Max := MaxSpin;

end;
{=====}
procedure TfrmEditElement.UpDownClick(Sender: TObject; Button: TUDBtnType);
var
  Val, Inc : Extended;
begin
  if Sender = udLeft   then CurEdit := edLeft  ;
  if Sender = udTop    then CurEdit := edTop   ;
  if Sender = udHeight then CurEdit := edHeight;
  if Sender = udWidth  then CurEdit := edWidth ;

  Val := 0.0;
  try
    Val := StrToFloat(CurEdit.Text);
  except
    on EConvertError do begin
      ShowMessage('Please Enter a Floating Point Value');
      CurEdit.SetFocus;
    end;
  end;

  Inc := udLeft.Increment / 100;
  case Button of
    btNext: begin
      if Trunc(Val + Inc) > Trunc(Val) then
        (Sender as TUpDown).Position := (Sender as TUpDown).Position + 1;
      CurEdit.Text := FormatFloat('0.00 ', Val + Inc);
    end;
    btPrev: begin
      if Trunc(Val - Inc) < Trunc(Val) then
        (Sender as TUpDown).Position := (Sender as TUpDown).Position - 1;
      CurEdit.Text := FormatFloat('0.00 ', Val - Inc);
    end;
  end;
end;
{=====}
function TfrmEditElement.Validate : Boolean;
begin
  Result := edName.Text <> '';
end;
{=====}

end.
  
