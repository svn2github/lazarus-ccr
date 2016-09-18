{*********************************************************}
{*              VPCONTACTEDITDLG.PAS 1.03                *}
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

{$I Vp.INC}

unit VpContactEditDlg;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, VpData, ExtCtrls, StdCtrls,
  VpException, VpMisc, VpBase, VpSR, VpDlg, VpBaseDS, ComCtrls, EditBtn;

type
  { forward declarations }
  TVpContactEditDialog = class;

  { TContactEditForm }

  TContactEditForm = class(TForm)
    cbCountryH: TComboBox;
    cbStateW: TComboBox;
    cbStateH: TComboBox;
    cbWebsite2: TComboBox;
    cbEMail1: TComboBox;
    cbEMail2: TComboBox;
    cbEMail3: TComboBox;
    cbWebsite1: TComboBox;
    edAddressH: TEdit;
    edCityH: TEdit;
    edCompany: TEdit;
    edCountryH: TEdit;
    edDepartment: TEdit;
    edStateH: TEdit;
    edZipCodeH: TEdit;
    gbWorkAddress: TGroupBox;
    gbHomeAddress: TGroupBox;
    lblAddressH: TLabel;
    lblCityH: TLabel;
    lblCompany: TLabel;
    lblCountryComboH: TLabel;
    lblCountryW: TLabel;
    lblCountryH: TLabel;
    lblDepartment: TLabel;
    edBirthdate: TDateEdit;
    edEMail2: TEdit;
    edEMail3: TEdit;
    edWebsite1: TEdit;
    edWebsite2: TEdit;
    gbPhone: TGroupBox;
    gbEMail: TGroupBox;
    gbWebsites: TGroupBox;
    lblBirthdate: TLabel;
    lblCategory: TLabel;
    cbCategory: TComboBox;
    edFirstName: TEdit;
    lblFirstName: TLabel;
    edLastName: TEdit;
    lblLastName: TLabel;
    lblStateComboW: TLabel;
    lblStateComboH: TLabel;
    lblStateH: TLabel;
    lblZipCodeH: TLabel;
    tabBaseData: TTabSheet;
    edTitle: TEdit;
    lblTitle: TLabel;
    PageControl: TPageControl;
    tabAddresses: TTabSheet;
    lblAddressW: TLabel;
    lblCityW: TLabel;
    lblStateW: TLabel;
    lblZipCodeW: TLabel;
    lblCountryComboW: TLabel;
    lblPosition: TLabel;
    edAddressW: TEdit;
    edCityW: TEdit;
    edStateW: TEdit;
    edZipCodeW: TEdit;
    edPosition: TEdit;
    cbCountryW: TComboBox;
    edCountryW: TEdit;
    tabContact: TTabSheet;
    tabCustom: TTabSheet;
    pnlBottom: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    tabNotes: TTabSheet;
    memoNotes: TMemo;
    lblCustom1: TLabel;
    lblCustom2: TLabel;
    lblCustom3: TLabel;
    lblCustom4: TLabel;
    edCustom1: TEdit;
    edCustom2: TEdit;
    edCustom3: TEdit;
    edCustom4: TEdit;
    cbPhone1: TComboBox;
    cbPhone2: TComboBox;
    cbPhone3: TComboBox;
    cbPhone4: TComboBox;
    edPhone4: TEdit;
    edPhone3: TEdit;
    edPhone2: TEdit;
    edPhone1: TEdit;
    cbPhone5: TComboBox;
    edPhone5: TEdit;
    edEMail1: TEdit;
    procedure cbCountryChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ItemChanged(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FBtnHeight: Integer;
    FEditHeight: Integer;
    procedure DisplayCurrentCountry(AddressType: TVpAddressType);
    procedure ResizeControls;
    procedure SetCaptions;
  public
    Resource: TVpResource;
    Contact: TVpContact;
    ReturnCode: TVpEditorReturnCode;
    ControlLink: TVpControlLink;
    procedure ArrangeControls;
    procedure DePopulateSelf;
    procedure PopulateSelf;
  end;

  TVpContactEditDialog = class(TVpBaseDialog)
  protected {private}
    ceEditDlg: TContactEditForm;
    ceContact: TVpContact;
    ceResource: TVpResource;
  public
    function AddNewContact: Boolean;
    function Execute(Contact: TVpContact): Boolean; reintroduce;
  published
    {properties}
    property ControlLink;
    property DataStore;
    property Placement;                  
  end;


implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  VpConst;

{ Utility functions }

function Max(const a, b: Integer): Integer;
begin
  if a >= b then
    Result := a
  else
    Result := b;
end;


{ TContactEditForm }

procedure TContactEditForm.FormCreate(Sender: TObject);
begin
  ReturnCode := rtAbandon;
  SetCaptions;
  FBtnHeight := ScaleY(OKBtn.Height, DesignTimeDPI);
  FEditHeight := ScaleY(edCompany.Height, DesigntimeDPI);
end;

procedure TContactEditForm.SetCaptions;
begin
  tabBaseData.Caption := RSMasterData;
  tabAddresses.Caption := RSAddresses;
  tabContact.Caption := RSDlgContactEdit;
  tabCustom.Caption := RSCustom;
  tabNotes.Caption := RSNotes;

  gbWorkAddress.Caption := RSWorkAddress;
  gbHomeAddress.Caption := RSHomeAddress;
  gbPhone.Caption := RSPhoneFax;
  gbEMail.Caption := RSEmail;
  gbWebsites.Caption := RSWebsites;

  OkBtn.Caption := RSOKBtn;
  CancelBtn.Caption := RSCancelBtn;
  lblLastName.Caption := RSLastNameLbl;
  lblFirstName.Caption := RSFirstNameLbl;
  lblTitle.Caption := RSTitleLbl;
  lblCategory.Caption := RSCategoryLbl;
  lblBirthdate.Caption := RSBirthDateLbl;

  lblCompany.Caption := RSCompanyLbl;
  lblDepartment.Caption := RSDepartmentLbl;
  lblPosition.Caption := RSPositionLbl;

  lblAddressW.Caption := RSAddressLbl;
  lblAddressH.Caption := RSAddressLbl;
  lblCityW.Caption := RSCityLbl;
  lblCityH.Caption := RSCityLbl;
  lblStateW.Caption := RSStateLbl;
  lblStateH.Caption := RSStateLbl;
  lblStateComboW.Caption := RSStateLbl;
  lblStateComboH.Caption := RSStateLBL;
  lblZipCodeW.Caption := RSZipCodeLbl;
  lblZipCodeH.Caption := RSZipCodeLbl;
  lblCountryComboW.Caption := RSCountryLbl;
  lblCountryComboH.Caption := RSCountryLbl;
  lblCountryW.Caption := RSCountryLbl;
  lblCountryH.Caption := RSCountryLbl;

  lblCustom1.Caption := RSCustom1;
  lblCustom2.Caption := RSCustom2;
  lblCustom3.Caption := RSCustom3;
  lblCustom4.Caption := RSCustom4;
end;

procedure TContactEditForm.OKBtnClick(Sender: TObject);
begin
  if (edLastName.Text = '') and (edFirstName.Text = '') then begin
    raise EVpContactEditError.Create(RSNameIsRequired);
    exit;
  end;
  ReturnCode := rtCommit;
  Close;
end;

procedure TContactEditForm.CancelBtnClick(Sender: TObject);
begin
  ReturnCode := rtAbandon;
  Close;
end;

procedure TContactEditForm.DePopulateSelf;
begin
  Contact.LastName := edLastName.Text;
  Contact.FirstName := edFirstName.Text;
  Contact.Title := edTitle.Text;
  Contact.Category := cbCategory.ItemIndex;
  Contact.Birthdate := edBirthdate.Date;

  Contact.Company := edCompany.Text;
  Contact.Department := edDepartment.Text;
  Contact.Job_Position := edPosition.Text;

  Contact.AddressType1 := ord(atWork);
  Contact.Address1 := edAddressW.Text;
  Contact.City1 := edCityW.Text;
  if cbStateW.Visible then
    Contact.State1 := cbStateW.Text
  else
    Contact.State1 := edStateW.Text;
  Contact.Zip1 := edZipCodeW.Text;
  if cbCountryW.Visible then
    Contact.Country1 := cbCountryW.Text
  else
    Contact.Country1 := edCountryW.Text;

  Contact.AddressType2 := ord(atHome);
  Contact.Address2 := edAddressH.Text;
  Contact.City2 := edCityH.Text;
  if cbStateH.Visible then
    Contact.State2 := cbStateH.Text
  else
    Contact.State2 := edStateH.Text;
  Contact.Zip2 := edZipCodeH.Text;
  if cbCountryH.Visible then
    Contact.Country2 := cbCountryH.Text
  else
    Contact.Country2 := edCountryH.Text;

  Contact.Phone1 := edPhone1.Text;
  Contact.Phone2 := edPhone2.Text;
  Contact.Phone3 := edPhone3.Text;
  Contact.Phone4 := edPhone4.Text;
  Contact.Phone5 := edPhone5.Text;
  Contact.PhoneType1 := cbPhone1.ItemIndex;
  Contact.PhoneType2 := cbPhone2.ItemIndex;
  Contact.PhoneType3 := cbPhone3.ItemIndex;
  Contact.PhoneType4 := cbPhone4.ItemIndex;
  Contact.PhoneType5 := cbPhone5.ItemIndex;

  Contact.EMail1 := edEMail1.Text;
  Contact.EMail2 := edEMail2.Text;
  contact.EMail3 := edEMail3.Text;
  Contact.EMailType1 := cbEMail1.ItemIndex;
  Contact.EMailType2 := cbEMail2.ItemIndex;
  Contact.EMailType3 := cbEMail3.ItemIndex;

  Contact.Website1 := edWebsite1.Text;
  Contact.Website2 := edWebsite2.Text;
  Contact.WebsiteType1 := cbWebsite1.ItemIndex;
  Contact.WebsiteType2 := cbWebsite2.ItemIndex;

  Contact.Custom1 := edCustom1.Text;
  Contact.Custom2 := edCustom2.Text;
  Contact.Custom3 := edCustom3.Text;
  Contact.Custom4 := edCustom4.Text;

  Contact.Notes := memoNotes.Text;
end;

procedure TContactEditForm.PopulateSelf;
var
  CurCountry: Integer;
  pt: TVpPhoneType;
  mt: TVpEMailType;
  wt: TVpWebsiteType;
  ct: TVpCategoryType;
begin
  edLastName.Text := Contact.LastName;
  edFirstName.Text := Contact.FirstName;
  edTitle.Text := Contact.Title;
  if contact.Birthdate = 0.0 then
    edBirthdate.Clear else
    edBirthdate.Date := Contact.Birthdate;

  cbCategory.Items.Clear;
  for ct := Low (TVpCategoryType) to High (TVpCategoryType) do
    cbCategory.Items.Add(CategoryLabel(ct));
  cbCategory.ItemIndex := Contact.Category;

  if Contact.Birthdate = 0.0 then
    edBirthdate.Clear else
    edBirthdate.Date := Contact.Birthdate;

  edCompany.Text := Contact.Company;
  edDepartment.Text := Contact.Department;
  edPosition.Text := Contact.Job_Position;

  edAddressW.Text := Contact.Address1;
  edCityW.Text := Contact.City1;
  edZipCodeW.Text := Contact.Zip1;
  cbCountryW.Text := Contact.Country1;
  edCountryW.Text := Contact.Country1;
  if (Contact.Country1 = '') and Assigned(ControlLink) then begin
    if ControlLink.DefaultCountry <> '' then begin
      cbCountryW.Text := ControlLink.DefaultCountry;
      edCountryW.Text := ControlLink.DefaultCountry;
    end else begin
      CurCountry := ControlLink.Localization.GetCurrentCountry;
      if CurCountry >= 0 then begin
        cbCountryW.Text := ControlLink.Localization.Countries.Items[CurCountry].Name;
        edCountryW.Text := ControlLink.Localization.Countries.Items[CurCountry].Name;
      end;
    end;
  end;
  edStateW.Text := Contact.State1;
  cbStateW.Text := Contact.State1;

  edAddressH.Text := Contact.Address2;
  edCityH.Text := Contact.City2;
  edZipCodeH.Text := Contact.Zip2;
  cbCountryH.Text := Contact.Country2;
  edCountryH.Text := Contact.Country2;
  if (Contact.Country2 = '') and Assigned(ControlLink) then begin
    if ControlLink.DefaultCountry <> '' then begin
      cbCountryH.Text := ControlLink.DefaultCountry;
      edCountryH.Text := ControlLink.DefaultCountry;
    end else begin
      CurCountry := ControlLink.Localization.GetCurrentCountry;
      if CurCountry >= 0 then begin
        cbCountryH.Text := ControlLink.Localization.Countries.Items[CurCountry].Name;
        edCountryH.Text := ControlLink.Localization.Countries.Items[CurCountry].Name;
      end;
    end;
  end;
  edStateH.Text := Contact.State2;
  cbStateH.Text := Contact.State2;

  memoNotes.Text := Contact.Notes;

  edCustom1.Text := Contact.Custom1;
  edCustom2.Text := Contact.Custom2;
  edCustom3.Text := Contact.Custom3;
  edCustom4.Text := Contact.Custom4;

  edPhone1.Text := Contact.Phone1;
  edPhone2.Text := Contact.Phone2;
  edPhone3.Text := Contact.Phone3;
  edPhone4.Text := Contact.Phone4;
  edPhone5.Text := Contact.Phone5;
  cbPhone1.Items.Clear;
  cbPhone2.Items.Clear;
  cbPhone3.Items.Clear;
  cbPhone4.Items.Clear;
  cbPhone5.Items.Clear;
  for pt := Low (TVpPhoneType) to High (TVpPhoneType) do begin
    cbPhone1.Items.Add(PhoneLabel(pt));
    cbPhone2.Items.Add(PhoneLabel(pt));
    cbPhone3.Items.Add(PhoneLabel(pt));
    cbPhone4.Items.Add(PhoneLabel(pt));
    cbPhone5.Items.Add(PhoneLabel(pt));
  end;
  cbPhone1.ItemIndex := Contact.PhoneType1;
  cbPhone2.ItemIndex := Contact.PhoneType2;
  cbPhone3.ItemIndex := Contact.PhoneType3;
  cbPhone4.ItemIndex := Contact.PhoneType4;
  cbPhone5.ItemIndex := Contact.PhoneType5;

  edEMail1.Text := Contact.EMail1;
  edEMail2.Text := Contact.EMail2;
  edEMail3.Text := Contact.EMail3;
  cbEMail1.Items.Clear;
  cbEMail2.Items.Clear;
  cbEMail3.Items.Clear;
  for mt := Low(TVpEMailType) to High(TVpEMailType) do begin
    cbEMail1.Items.Add(EMailLabel(mt));
    cbEMail2.Items.Add(EMailLabel(mt));
    cbEMail3.Items.Add(EMailLabel(mt));
  end;
  cbEMail1.ItemIndex := Contact.EMailType1;
  cbEMail2.ItemIndex := Contact.EMailType2;
  cbEMail3.ItemIndex := Contact.EMailType3;

  edWebsite1.Text := Contact.Website1;
  edWebsite2.Text := Contact.Website2;
  cbWebsite1.Items.Clear;
  cbWebsite2.Items.Clear;
  for wt := Low(TVpWebsiteType) to High(TVpWebsiteType) do begin
    cbWebsite1.Items.Add(WebsiteLabel(wt));
    cbWebsite2.Items.Add(WebsiteLabel(wt));
  end;
  cbWebsite1.ItemIndex := Contact.WebsiteType1;
  cbWebsite2.ItemIndex := Contact.WebsiteType2;

  DisplayCurrentCountry(atWork);
  DisplayCurrentCountry(atHome);
  ResizeControls;
end;

procedure TContactEditForm.ItemChanged(Sender: TObject);
begin
  Contact.Changed := true;
end;

procedure TContactEditForm.ArrangeControls;
begin
  if (not Assigned (ControlLink)) or (ControlLink.Localization.Countries.Count = 0)
  then begin
    edCountryW.Show;
    cbCountryW.Hide;
    edCountryH.Show;
    cbCountryH.Hide;
  end
  else begin
    ControlLink.Localization.CountriesToTStrings(cbCountryW.Items);
    edCountryW.Hide;
    cbCountryW.Show;
    ControlLink.Localization.CountriesToTStrings(cbCountryH.Items);
    edCountryH.Hide;
    cbCountryH.Show;
  end;

  lblCountryComboW.Visible := cbCountryW.Visible;
  lblCountryW.Visible := edCountryW.Visible;
  lblCountryComboH.Visible := cbCountryH.Visible;
  lblCountryH.Visible := edCountryH.Visible;

  PageControl.ActivePage := tabBaseData;
end;

procedure TContactEditForm.ResizeControls;
const
  ComboArrowWidth  = 32;
//  FieldVertSep     = 25;
//  FormRightBorder  = 20;
//  MinFormWidth     = 265;
//  FormHeightOffset = 103;
//  MinFormHeight    = 250;
  TopField         = 8;

type
  TLabelArray = array of TLabel;
  TComboboxArray = array of TCombobox;
  TEditArray = array of TEdit;

var
  Labels: TLabelArray;
  Comboboxes: TComboboxArray;
  Edits: TEditArray;
  largestLabelWidth: Integer;
  i, j: Integer;
  OldFont: TFont;
  FieldTop: Integer;
  delta: Integer;
  hdist: Integer = 4;    // Horizontal distance between label and edit/combo
  vDist: Integer = 4;    // Vertical distance between edits
  hBorder: Integer = 8;  // Horizontal distance between container border and label
  vBorder: Integer = 8;  // Vertical distance between container border and 1st control
begin
  {----------------------------------------------------------------------------}
  { Preparations                                                               }
  {----------------------------------------------------------------------------}
  hdist := ScaleX(hdist, DesignTimeDPI);
  vdist := ScaleY(vdist, DesignTimeDPI);
  hBorder := ScaleX(hBorder, DesignTimeDPI);
  vBorder := ScaleY(vBorder, DesignTimeDPI);
  edBirthdate.ButtonWidth := FEditHeight;

  {----------------------------------------------------------------------------}
  { Page "Base data"                                                           }
  {----------------------------------------------------------------------------}
  SetLength(Labels, 5);
  Labels[0] := lblLastName;
  Labels[1] := lblFirstName;
  Labels[2] := lblTitle;
  Labels[3] := lblCategory;
  Labels[4] := lblBirthdate;

  largestLabelWidth := 0;
  for i := Low(Labels) to High(Labels) do
    largestLabelWidth := Max(largestLabelWidth, GetLabelWidth(Labels[i]));

  // Determine width of label based upon dpi of screen
  for i := Low(Labels) to High(Labels) do begin
    Labels[i].FocusControl.Left := HBorder + largestLabelWidth + hDist;
    Labels[i].Left := Labels[i].FocusControl.Left - hDist - GetLabelWidth(Labels[i]);
  end;

  // Set edit and combo widths
  edLastName.Width := edLastName.Parent.ClientWidth - hBorder - edLastName.Left;
  edFirstname.Width := edLastName.Width;
  edBirthdate.Width := edTitle.Width;
  cbCategory.Width := edTitle.Width;

  // Vertically arrange the fields. }
  delta := (Labels[0].FocusControl.Height - labels[0].Height) div 2;
  FieldTop := vBorder;
  for i := Low(Labels) to High(Labels) do
    if Labels[i].Visible then begin
      Labels[i].FocusControl.Top := FieldTop;
      Labels[i].Top := FieldTop + delta;
      inc(FieldTop, FEditHeight + VDist);
    end;

  {----------------------------------------------------------------------------}
  { Page "Contact"                                                             }
  {----------------------------------------------------------------------------}
  gbPhone.Left := hBorder;
  gbPhone.Width := gbPhone.Parent.ClientWidth - 2*hBorder;

  gbEMail.Left := hBorder;
  gbEMail.Width := gbPhone.Width;

  gbWebsites.Left := hBorder;
  gbWebsites.Width := gbPhone.Width;

  SetLength(Comboboxes, 10);
  Comboboxes[0] := cbPhone1;
  Comboboxes[1] := cbPhone2;
  Comboboxes[2] := cbPhone3;
  Comboboxes[3] := cbPhone4;
  Comboboxes[4] := cbPhone5;
  Comboboxes[5] := cbEMail1;
  Comboboxes[6] := cbEMail2;
  Comboboxes[7] := cbEMail3;
  Comboboxes[8] := cbWebsite1;
  Comboboxes[9] := cbWebsite1;

  SetLength(Edits, 10);
  Edits[0] := edPhone1;
  Edits[1] := edPhone2;
  Edits[2] := edPhone3;
  Edits[3] := edPhone4;
  Edits[4] := edPhone5;
  Edits[5] := edEMail1;
  Edits[6] := edEMail2;
  Edits[7] := edEMail3;
  Edits[8] := edWebsite1;
  Edits[9] := edWebsite2;

  largestLabelWidth := 0;
  OldFont := TFont.Create;
  try
    OldFont.Assign(Canvas.Font);
    Canvas.Font.Assign(cbPhone1.Font);
    for i:=0 to cbPhone1.Items.Count-1 do
      largestLabelWidth := Max(Canvas.TextWidth(cbPhone1.Items[i]) + ComboArrowWidth, largestlabelWidth);
    canvas.Font.Assign(cbEmail1.Font);
    for i:=0 to cbEMail1.Items.Count-1 do
      largestLabelWidth := Max(Canvas.TextWidth(cbEMail1.Items[i]) + ComboArrowWidth, largestLabelWidth);
    canvas.Font.Assign(cbWebsite1.Font);
    for i:=0 to cbWebsite1.Items.Count-1 do
      largestlabelWidth := Max(Canvas.TextWidth(cbWebsite1.Items[i]) + ComboArrowWidth, largestLabelWidth);
  finally
    Canvas.Font.Assign(OldFont);
    OldFont.Free;
  end;

  for i:=Low(Comboboxes) to High(Comboboxes) do begin
    Comboboxes[i].Left := HBorder;
    Comboboxes[i].Width := largestLabelWidth;
  end;

  for i:= Low(Edits) to High(Edits) do begin
    Edits[i].Left := cbPhone1.Left + cbPhone1.Width + hDist;
    Edits[i].Width := Edits[i].Parent.ClientWidth - Edits[i].Left - HBorder;
  end;

  gbPhone.AutoSize := true;
  gbEMail.AutoSize := true;
  gbWebsites.AutoSize := true;


  {----------------------------------------------------------------------------}
  { Page "Addresses"                                                             }
  {----------------------------------------------------------------------------}
  { Note: The resizing algorithm is dependent upon the labels having their
    FocusControl property set to the corresponding edit field or combobox. }

  SetLength(Labels, 17);
  Labels[0] := lblCompany;
  Labels[1] := lblDepartment;
  Labels[2] := lblPosition;
  Labels[3] := lblAddressW;
  Labels[4] := lblCityW;
  Labels[5] := lblStateW;
  Labels[6] := lblStateComboW;
  Labels[7] := lblZipCodeW;
  Labels[8] := lblCountryW;
  Labels[9] := lblCountryComboW;
  Labels[10] := lblAddressH;
  Labels[11] := lblCityH;
  Labels[12] := lblStateH;
  Labels[13] := lblStateComboH;
  Labels[14] := lblZipCodeH;
  Labels[15] := lblCountryH;
  Labels[16] := lblCountryComboH;

  largestLabelWidth := 0;
  for i := Low(Labels) to High(Labels) do begin
    largestLabelWidth := Max(largestLabelWidth, GetLabelWidth(Labels[i]));
    Labels[i].FocusControl.Height := FEditHeight;
  end;

  edCompany.Left := HBorder + largestLabelWidth + hDist;
  edCompany.Width := edCompany.Parent.ClientWidth - hBorder - edCompany.Left;

  edAddressH.Left := edCompany.Left;
  edAddressH.Width := edCompany.Width;

  {----------------------------------------------------------------------------}
  { Page "User-defined"                                                        }
  {----------------------------------------------------------------------------}
  SetLength(Labels, 4);
  Labels[0] := lblCustom1;
  Labels[1] := lblCustom2;
  Labels[2] := lblCustom3;
  Labels[3] := lblCustom4;

  largestLabelWidth := 0;
  for i := Low(Labels) to High(Labels) do
    largestLabelWidth := Max(largestLabelWidth, GetLabelWidth(Labels[i]));

  FieldTop := vBorder;
  edCustom1.Left := hBorder + largestLabelWidth + hDist;
  edCustom1.Width := edCustom1.Parent.ClientWidth - hBorder - edCustom1.Left;
  for i := Low(Labels) to High(Labels) do begin
    Labels[i].FocusControl.Top := FieldTop;
    Labels[i].FocusControl.Left := edCustom1.Left;
    Labels[i].FocusControl.Width := edCustom1.Width;
    Labels[i].FocusControl.Height := FEditHeight;
    labels[i].Top := FieldTop + delta;
    Labels[i].Width := LargestLabelWidth;
    Labels[i].Left := Labels[i].FocusControl.Left - GetLabelWidth(Labels[i]) - hDist;
    inc(FieldTop, FEditHeight + vDist);
  end;

  {----------------------------------------------------------------------------}
  { Buttons                                                                    }
  {----------------------------------------------------------------------------}
  OKBtn.Width := Max(GetButtonWidth(OKBtn), GetButtonWidth(CancelBtn));
  CancelBtn.Width := OKBtn.Width;
 {$IFDEF MSWINDOWS}   // button order: OK - Cancel
  CancelBtn.Left := pnlBottom.ClientWidth - HBorder - CancelBtn.Width;
  OKBtn.Left := CancelBtn.Left - hDist - OKBtn.Width;
 {$ELSE}              // button order: Cancel - OK
  OKBtn.Left := pnlBottom.ClientWidth - HBorder - OKBtn.Width;
  CancelBtn.Left := OKBtn.Left - hDist - CancelBtn.Width;
  OKBtn.TabOrder := 1;
 {$ENDIF}
  OKBtn.Height := FBtnHeight;
  CancelBtn.Height := FBtnHeight;
  OKBtn.Top := vDist;
  CancelBtn.Top := vDist;
  pnlBottom.ClientHeight := OKBtn.Height + vDist*2; // Centering is done by anchors

  {----------------------------------------------------------------------------}
  { Set form size                                                              }
  {----------------------------------------------------------------------------}
//  ClientHeight := h1 + h2 + pnlBottom.Height + vDist + 16 + PageControl.Height - tabAddresses.ClientHeight;
  AutoSize := true;
//  AdjustSize;
end;

procedure TContactEditForm.DisplayCurrentCountry(AddressType: TVpAddressType);
var
  idx : Integer;
  countryCombo: TCombobox;
  stateCombo: TCombobox;
  stateComboLabel: TLabel;
  stateLabel: TLabel;
  stateEdit: TEdit;
  addressLabel: TLabel;
  addressEdit: TEdit;
  cityLabel: TLabel;
  cityEdit: TEdit;
  zipcodeLabel: TLabel;
  zipcodeEdit: TEdit;
begin
 // ArrangeControls;

  if not Assigned(ControlLink) then
    Exit;

  case AddressType of
    atWork:
      begin
        countryCombo := cbCountryW;
        stateCombo := cbStateW;
        stateComboLabel := lblStateComboW;
        stateEdit := edStateW;
        stateLabel := lblStateW;
        addressEdit := edAddressW;
        addressLabel := lblAddressW;
        cityEdit := edCityW;
        cityLabel := lblCityW;
        zipcodeLabel := lblZipCodeW;
        zipcodeEdit := edZipCodeW;
      end;
    atHome:
      begin
        countryCombo := cbCountryH;
        stateCombo := cbStateH;
        stateComboLabel := lblStateComboH;
        stateEdit := edStateH;
        stateLabel := lblStateH;
        addressEdit := edAddressH;
        addressLabel := lblAddressH;
        cityEdit := edCityH;
        cityLabel := lblCityH;
        zipcodeLabel := lblZipCodeH;
        zipcodeEdit := edZipCodeH;
      end;
  end;

  idx := ControlLink.Localization.CountryNameToIndex(countryCombo.Text);
  if idx > -1 then begin
    ControlLink.Localization.StatesToTStrings(idx, stateCombo.Items);

    if ControlLink.Localization.Countries.Items[idx].Address1Visible then begin
      addressEdit.Show;
      addressLabel.Show;
      if ControlLink.Localization.Countries.Items[idx].Address1Caption <> '' then
        addressLabel.Caption := ControlLink.Localization.Countries.Items[idx].Address1Caption
      else
        addressLabel.Caption := RSAddressLbl;
    end else begin
      addressEdit.Hide;
      addressLabel.Hide;
    end;

    if ControlLink.Localization.Countries.Items[idx].CityVisible then begin
      cityEdit.Show;
      cityLabel.Show;
      if ControlLink.Localization.Countries.Items[idx].CityCaption <> '' then
        cityLabel.Caption := ControlLink.Localization.Countries.Items[idx].CityCaption
      else
        cityLabel.Caption := RSCityLbl;
    end else begin
      cityEdit.Hide;
      cityLabel.Show;
    end;

    if ControlLink.Localization.Countries.Items[idx].StatesVisible then begin
      stateLabel.Visible := True;
      if ControlLink.Localization.Countries.Items[Idx].States.Count > 0 then begin
        stateComboLabel.Show;
        stateCombo.Show;
        stateLabel.Hide;
        stateEdit.Hide;
      end else begin
        stateComboLabel.Hide;
        stateCombo.Hide;
        stateLabel.Show;
        stateEdit.Show;
      end;
      if ControlLink.Localization.Countries.Items[idx].StateCaption <> '' then begin
        stateComboLabel.Caption := ControlLink.Localization.Countries.Items[idx].StateCaption;
        stateLabel.Caption := StateComboLabel.Caption;
      end else begin
        stateLabel.Caption := RSStateLbl;
        stateComboLabel.Caption := stateLabel.Caption;
      end;
    end else begin
      stateComboLabel.Hide;
      stateCombo.Hide;
      stateLabel.Hide;
      stateEdit.Hide;
    end;

    if ControlLink.Localization.Countries.Items[idx].ZipVisible then begin
      zipcodeEdit.Show;
      zipcodeLabel.Show;
      if ControlLink.Localization.Countries.Items[idx].ZipCaption <> '' then
        zipcodeLabel.Caption := ControlLink.Localization.Countries.Items[idx].ZipCaption
      else
        zipcodeLabel.Caption := RSZipCodeLbl;
    end else begin
      zipcodeEdit.Hide;
      zipcodeLabel.Hide;
    end;

  end else begin
    stateEdit.Show;
    stateLabel.Show;
    stateCombo.Hide;
    stateComboLabel.Hide;
    stateCombo.Items.Clear;
  end;

//  ResizeControls;
end;

procedure TContactEditForm.cbCountryChange(Sender: TObject);
begin
  if Sender = cbCountryW then begin
    edStateW.Text := '';
    cbStateW.Text := '';
    DisplayCurrentCountry(atWork);
  end else begin
    edStateH.Text := '';
    cbStateH.Text := '';
    DisplayCurrentCountry(atHome);
  end;
  ResizeControls;
end;

procedure TContactEditForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Unused(Shift);
  if Key = VK_ESCAPE then begin
    ReturnCode := rtAbandon;
    Close;
  end;
end;

procedure TContactEditForm.PageControlChange(Sender: TObject);
begin
  if Visible then
    if PageControl.ActivePage = tabBaseData then
      edLastName.SetFocus
    else if PageControl.ActivePage = tabAddresses then
      edCompany.SetFocus
    else if PageControl.ActivePage = tabContact then
      edPhone1.SetFocus
    else if PageControl.ActivePage = tabCustom then
      edCustom1.SetFocus
    else if PageControl.ActivePage = tabNotes then
      memoNotes.SetFocus;
end;

procedure TContactEditForm.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tabBaseData;
  if PageControl.ActivePage = tabBaseData then
    edLastName.SetFocus;
end;


{ TVpContactEditDialog }

function TVpContactEditDialog.Execute(Contact: TVpContact): Boolean;
var
  EditForm: TContactEditForm;
begin
  ceContact := Contact;
  Result := false;
  Application.CreateForm(TContactEditForm, EditForm);
  try
    DoFormPlacement(EditForm);
    SetFormCaption(EditForm, Contact.FullName, RSDlgContactEdit);
    EditForm.Contact := ceContact;
    EditForm.Resource := DataStore.Resource;
    EditForm.ControlLink := ControlLink;
    EditForm.ArrangeControls;
    EditForm.PopulateSelf;
    EditForm.ShowModal;
    if EditForm.ReturnCode = rtCommit then begin
      EditForm.DePopulateSelf;
      Result := true;
    end;
  finally
    EditForm.Release;
  end;

  if Result then begin
    ceContact.Changed := true;
    DataStore.PostContacts;
    DataStore.NotifyDependents;
  end;
end;

function TVpContactEditDialog.AddNewContact: Boolean;
begin
  result := false;
  if DataStore <> nil then begin
    if DataStore.Resource = nil then
      Exit;
    ceContact := DataStore.Resource.Contacts.AddContact(
      DataStore.GetNextID(ContactsTableName));
    if ceContact <> nil then begin
      Result := Execute(ceContact);
      if not Result then
      (*
      if Result then
        DataStore.PostContacts
      else
        *)
        ceContact.Free;
    end;
  end;
end;

end.

  
