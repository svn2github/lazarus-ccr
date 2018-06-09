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

unit ExVpRptSetup;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, EditBtn, ExtCtrls,

  VpBaseDS, VpPrtFmtCBox;

type
  TReportDataRec = record
    StartDate, EndDate : TDateTime;
    Format : string;
  end;

  { TfrmReportSetup }

  TfrmReportSetup = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblStartDate: TLabel;
    lblEndDate: TLabel;
    lblFormat: TLabel;
    edStartDate: TDateEdit;
    edEndDate: TDateEdit;
    Panel1: TPanel;
    ButtonPanel: TPanel;
    PrintFormatCombo: TVpPrintFormatComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure PositionControls;
    procedure SetCaptions;
    function GetControlLink: TVpControlLink;
    procedure SetControlLink(const Value: TVpControlLink);
    function GetDate(Index: Integer): TDateTime;
    procedure SetDate(Index: Integer; Value: TDateTime);
    procedure SaveData(out ReportData: TReportDataRec);
  public
    { Public declarations }
    function Execute(var ReportData: TReportDataRec) : Boolean;
    property ControlLink : TVpControlLink
      read GetControlLink write SetControlLink;
    property StartDate : TDateTime index 1
      read GetDate write SetDate;
    property EndDate : TDateTime index 2
      read GetDate write SetDate;
  end;

var
  frmReportSetup: TfrmReportSetup;
  ReportData: TReportDataRec = (StartDate: 0; EndDate: 0; Format: '');

implementation

{$R *.lfm}

uses
  Math, LCLIntf, LCLType,
  VpSR, VpMisc;

{ TfrmReportSetup }

function TfrmReportSetup.Execute(var ReportData: TReportDataRec) : Boolean;
begin
  StartDate := ReportData.StartDate;
  EndDate := ReportData.EndDate;
  PrintFormatCombo.ItemIndex := ControlLink.Printer.Find(ReportData.Format);

  Result := ShowModal = mrOk;

  if Result then
    SaveData(ReportData);
end;

procedure TfrmReportSetup.FormCreate(Sender: TObject);
begin
  SetCaptions;
end;

procedure TfrmReportSetup.FormShow(Sender: TObject);
begin
  PositionControls;
end;

function TfrmReportSetup.GetControlLink: TVpControlLink;
begin
  Result := PrintFormatCombo.ControlLink;
end;

function TfrmReportSetup.GetDate(Index: Integer) : TDateTime;
begin
  Result := 0.0;
  case Index of
    1: Result := edStartDate.Date;
    2: Result := edEndDate.Date;
  end;
end;

procedure TfrmReportSetup.PositionControls;
var
  i, w: Integer;
  cnv: TControlCanvas;
begin
  AutoSize := false;

  AlignOKCancel(btnOK, btnCancel, ButtonPanel);

  cnv := TControlCanvas.Create;
  try
    cnv.Control := PrintFormatCombo;
    w := 0;
    for i:=0 to PrintFormatCombo.Items.Count-1 do
      w := Max(w, cnv.TextWidth(PrintFormatCombo.Items[i]));
    inc(w, GetSystemMetrics(SM_CXVSCROLL));
    edStartDate.Width := w;
    // The width of the PrintFormatCombo is anchored to the edStartDate!
  finally
    cnv.Free;
  end;

  edStartDate.ButtonWidth := edStartDate.Height;
  edEndDate.ButtonWidth := edEndDate.Height;
  AutoSize := true;
end;

procedure TfrmReportSetup.SaveData(out ReportData: TReportDataRec);
begin
  if (edStartDate.Text = '') and (edEndDate.Text = '') then begin
    ReportData.StartDate := now;
    ReportData.EndDate := now;
  end else
  if (edStartDate.Text = '') then begin
    ReportData.EndDate := edEndDate.Date;
    ReportData.StartDate := edEndDate.Date;
  end else
  if (edEndDate.Text = '') then begin
    ReportData.StartDate := edStartDate.Date;
    ReportData.EndDate := edStartDate.date;
  end else
  begin
    ReportData.StartDate := edStartDate.Date;
    ReportData.EndDate := edEndDate.Date;
  end;
  ReportData.Format := PrintFormatCombo.Text;
end;

procedure TfrmReportSetup.SetCaptions;
begin
  Caption := RSReportSetup;
  lblStartDate.Caption := RSStartTimeLbl;
  lblEndDate.Caption := RSEndTimeLbl;
  lblFormat.Caption := RSFormatLbl;
  btnOK.Caption := RSOKBtn;
  btnCancel.Caption := RSCancelBtn;
end;

procedure TfrmReportSetup.SetControlLink(const Value: TVpControlLink);
begin
  PrintFormatCombo.ControlLink := Value;
end;

procedure TfrmReportSetup.SetDate(Index: Integer;
  Value: TDateTime);
begin
  case Index of
    1: edStartDate.Date := Value;
    2: edEndDate.Date := Value;
  end;
end;

end.
 
