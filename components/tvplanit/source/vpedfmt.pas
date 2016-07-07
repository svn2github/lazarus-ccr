{*********************************************************}
{*                VPEDFMT.PAS 1.03                       *}
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

unit VpEdFmt;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TypInfo, ComCtrls,
  VpPrtFmt;

type

  { TfrmEditFormat }

  TfrmEditFormat = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edDescription: TEdit;
    edName: TEdit;
    LblIncrement: TLabel;
    LblDescription: TLabel;
    LblName: TLabel;
    rgDayIncrement: TRadioGroup;
    udIncrement: TUpDown;
    edIncrement: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetCaptions;
  protected
    procedure SaveData(AFormat: TVpPrintFormatItem);
    procedure SetData(AFormat: TVpPrintFormatItem);
    function Validate: Boolean;
  public
    function Execute(AFormat: TVpPrintFormatItem) : Boolean;
  end;


implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  Math, VpMisc, VpSR;

{ TfrmEditLayout }

procedure TfrmEditFormat.FormShow(Sender: TObject);
begin
  edName.SetFocus;
end;
{=====}
procedure TfrmEditFormat.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{=====}
procedure TfrmEditFormat.btnOkClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk
  else begin
    ShowMessage('Please supply a Format Name');
    edName.SetFocus;
    Exit;
  end;
end;
{=====}
function TfrmEditFormat.Execute(AFormat: TVpPrintFormatItem) : Boolean;
begin
  SetData(AFormat);
  Result := ShowModal = mrOk;
  if Result then
    SaveData(AFormat);
end;

procedure TfrmEditFormat.FormCreate(Sender: TObject);
begin
  SetCaptions;
end;

{=====}
procedure TfrmEditFormat.SaveData(AFormat: TVpPrintFormatItem);
var
  EnumVal : Integer;
begin
  AFormat.FormatName := edName.Text;
  AFormat.Description := edDescription.Text;
  AFormat.DayInc := udIncrement.Position;

  EnumVal := GetEnumValue(TypeInfo(TVpDayUnits), 'du' + rgDayIncrement.Items[rgDayIncrement.ItemIndex]);
  if EnumVal > -1 then
    AFormat.DayIncUnits := TVpDayUnits(EnumVal)
  else
    AFormat.DayIncUnits := duDay;
end;

procedure TfrmEditFormat.SetCaptions;
const
  DELTA = 8;
  MARGIN = 16;
var
  cnv: TControlCanvas;
  w: Integer;
begin
  Caption := RSEditFormatCaption;
  LblName.Caption := RSNameLbl;
  LblDescription.Caption := RSDescriptionLbl;
  LblIncrement.Caption := RsTimeIncLbl;
  rgDayIncrement.Caption := RsTimeIncUnits;
  rgDayIncrement.Items[0] := RSDays;
  rgDayIncrement.Items[1] := RSWeeks;
  rgDayIncrement.Items[2] := RSMonths;
  rgDayIncrement.Items[3] := RSYears;
  btnOK.Caption := RSOKBtn;
  btnCancel.Caption := RSCancelBtn;

  w := MaxValue([GetLabelWidth(LblName), GetLabelWidth(LblDescription), GetLabelWidth(LblIncrement)]);
  edName.Left := MARGIN + w + DELTA;
  edDescription.Left := edName.Left;
  edDescription.Width := edName.Width;
  edIncrement.Left := edName.Left;
  udIncrement.Left := edIncrement.Left + edIncrement.Width;
  LblName.Left := edName.Left - GetLabelWidth(LblName) - DELTA;
  LblDescription.Left := edDescription.Left - GetLabelWidth(lblDescription) - DELTA;
  lblIncrement.Left := edIncrement.Left - GetLabelWidth(lblIncrement) - DELTA;

  ClientWidth := MARGIN + w + DELTA + edName.Width + MARGIN;
  rgDayIncrement.Width := ClientWidth - 2*MARGIN;

  cnv := TControlCanvas.Create;
  try
    cnv.Control := btnOK;
    cnv.Font.Assign(btnOK.Font);
    w := Max(cnv.TextWidth(RSOKBtn), cnv.TextWidth(RSCancelBtn));
  finally
    cnv.Free;
  end;
  btnOK.Width := w + 16;
  btnCancel.Width := w + 16;
  btnCancel.Left := ClientWidth - MARGIN - btnCancel.Width;
  btnOK.Left := btnCancel.Left - 8 - btnOK.Width;
end;

procedure TfrmEditFormat.SetData(AFormat: TVpPrintFormatItem);
var
  IncName : string;
begin
  edName.Text := AFormat.FormatName;
  edDescription.Text := AFormat.Description;
  udIncrement.Position := AFormat.DayInc;

  IncName := GetEnumName(TypeInfo(TVpDayUnits), Ord(AFormat.DayIncUnits));
  if IncName <> '' then begin
    rgDayIncrement.ItemIndex := rgDayIncrement.Items.IndexOf(Copy(IncName, 3, Length(IncName) - 2));
  end
  else
    rgDayIncrement.ItemIndex := 0;
end;
{=====}
function TfrmEditFormat.Validate : Boolean;
begin
  Result := edName.Text <> '';
end;
{=====}



end.
  
