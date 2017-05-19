{*********************************************************}
{*                VPEVNTEDITDLG.PAS 1.03                 *}
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

unit VpEvntEditDlg;
  { The default event edit dialog }

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LResources, LCLVersion, EditBtn,
  {$ELSE}
  Windows, Messages, Mask,
  {$ENDIF}
  SysUtils, {$IFDEF VERSION6}Variants,{$ENDIF} Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  VpData, VpBase, VpBaseDS, VpDlg, VpConst;

const
 blabla = 1;  // to make the $IF work in Laz 1.4.4. Why?

{$UNDEF NEW_TIME_EDIT}

{$IFDEF LCL}
  {$DEFINE NEW_TIME_EDIT}
  {$IF (lcl_major=1) and (lcl_minor<6)}
    {$UNDEF NEW_TIME_EDIT}
  {$ENDIF}
{$ENDIF}

type
  { forward declarations }
  TVpEventEditDialog = class;

  TEventEditDlgRtnType = (rtCommit, rtAbandon);

  TVpRightAlignedEdit = class(TEdit)
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params : TCreateParams); override;
  end;

  { TDlgEventEdit }

  TDlgEventEdit = class(TForm)
    AlarmAdvance: TEdit;
    Bevel4: TBevel;
    StartTimePlaceholder: TEdit;
    EndTimePlaceholder: TEdit;
    LocationEdit: TEdit;
    LocationLbl: TLabel;
    NotesMemo: TMemo;
    Panel1: TPanel;
    StartDate: TDateEdit;
    EndDate: TDateEdit;
    RepeatUntil: TDateEdit;
    ButtonPanel: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    ResourceNameLbl: TLabel;
    FileDialog: TOpenDialog;
    pgEvent: TPageControl;
    tabEvent: TTabSheet;
    AppointmentGroupBox: TGroupBox;
    DescriptionLbl: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CategoryLbl: TLabel;
    StartTimeLbl: TLabel;
    EndTimeLbl: TLabel;
    ImgRecurring: TImage;
    RecurringLbl: TLabel;
    Bevel3: TBevel;
    IntervalLbl: TLabel;
    ImgAlarm: TImage;
    SoundFinderBtn: TSpeedButton;
    DescriptionEdit: TEdit;
    AlarmSet: TCheckBox;
    Category: TComboBox;
    RecurringType: TComboBox;
    IntervalUpDown: TUpDown;
    AlarmAdvanceType: TComboBox;
    AdvanceUpDown: TUpDown;
    CBAllDay: TCheckBox;
    CustomInterval: TEdit;
    imgClock: TImage;
    RecurrenceEndsLbl: TLabel;
    procedure AdvanceUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure AlarmAdvanceChange(Sender: TObject);
    procedure AlarmSetClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CategoryDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CBAllDayClick(Sender: TObject);
    procedure CustomIntervalChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IntervalUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure RecurringTypeChange(Sender: TObject);
    procedure SoundFinderBtnClick(Sender: TObject);

  private { Private declarations }
   {$IFDEF NEW_TIME_EDIT}
    StartTime: TTimeEdit;
    EndTime: TTimeEdit;
   {$ELSE}
    StartTime: TCombobox;
    EndTime: TCombobox;
   {$ENDIF}
    FDatastore: TVpCustomDatastore;
    AAVerifying: Boolean;
    CIVerifying: Boolean;
    procedure PopLists;
    procedure PositionControls;
    procedure LoadCaptions;
    procedure DoPlaySound(Sender: TObject; const AWavFile: String; AMode: TVpPlaySoundMode);

  protected
    property Datastore: TVpCustomDatastore read FDatastore write FDatastore;

  public { Public declarations }
    Event: TVpEvent;
    CatColorMap: TVpCategoryColorMap;
    Resource: TVpResource;
    ReturnCode: TEventEditDlgRtnType;
    Conflicts : Integer;
    TimeFormat: TVpTimeFormat;
    AlarmWavPath: string;
    FLastEndTime : TDateTime;

    procedure PopulateDialog;
    procedure DePopulateDialog;
  end;

  TVpEventEditDialog = class(TVpBaseDialog)
  protected {private}
    ceEditDlg: TDlgEventEdit;
    FTimeFormat: TVpTimeFormat;
    ceEvent: TVpEvent;
  public
    constructor Create(AOwner : TComponent); override;
    function Execute(Event: TVpEvent): Boolean; reintroduce;
    function AddNewEvent(PlaceholderStartTime, EndTimePlaceholder: TDateTime): Boolean;
  published
    {properties}
    property TimeFormat: TVpTimeFormat read FTimeFormat write FTimeFormat default tf12Hour;
    property DataStore;
    property Options;
    property Placement;
  end;

implementation

uses
  Math, DateUtils,
  VpSR, VpMisc, VpWavDlg;

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}


{ TVpRightAlignedEdit }

constructor TVpRightAlignedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
 {$IFDEF LCL}
  Alignment := taRightJustify;
 {$ENDIF}
end;

procedure TVpRightAlignedEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  {$IFNDEF LCL}
  Params.Style := Params.Style or ES_MULTILINE or ES_RIGHT;
  {$ENDIF}
end;

{=====}

{ TDlgEventEdit }

procedure TDlgEventEdit.FormCreate(Sender: TObject);
begin
 {$IFDEF NEW_TIME_EDIT}
  StartTime := TTimeEdit.Create(self);
 {$ELSE}
  StartTime := TCombobox.Create(self);
  StartTime.ItemIndex := -1;
 {$ENDIF}
  StartTime.Parent := AppointmentGroupbox;
  StartTime.Width := StartDate.Width;
  StartTime.Left := AlarmAdvanceType.Left;
  StartTime.Top := StartDate.Top;
  StartTime.AnchorSideLeft.Control := AlarmAdvanceType;
  StartTime.AnchorSideTop.Control := StartDate;
  StartTime.Anchors := [akLeft, akTop];
  StartTime.TabOrder := StartDate.TabOrder + 1;
  Bevel3.AnchorsideLeft.Control := Starttime;
  StartTimePlaceHolder.Free;

 {$IFDEF NEW_TIME_EDIT}
  EndTime := TTimeEdit.Create(self);
 {$ELSE}
  EndTime := TCombobox.Create(self);
  EndTime.ItemIndex := -1;
 {$ENDIF}
  EndTime.Parent := AppointmentGroupbox;
  EndTime.Width := EndDate.Width;
  EndTime.Left := AlarmAdvanceType.Left;
  EndTime.Top := EndDate.Top;
  EndTime.AnchorSideLeft.Control := AlarmAdvanceType;
  EndTime.AnchorSideTop.Control := EndDate;
  EndTime.Anchors := [akLeft, akTop];
  EndTime.TabOrder := EndDate.TabOrder + 1;
  EndTimePlaceHolder.Free;

  SoundFinderBtn.Height := AlarmAdvanceType.Height;
  SoundFinderBtn.Width := SoundFinderBtn.Height;

  ReturnCode := rtAbandon;
  PopLists;
  LoadCaptions;
  EndDate.Enabled := False;
  EndTime.Enabled := false;
end;
{=====}

procedure TDlgEventEdit.OKBtnClick(Sender: TObject);
var
  res: Integer;
  tStart, tEnd: TDateTime;
begin
 {$IFDEF NEW_TIME_EDIT}
  tStart := trunc(StartDate.Date) + frac(StartTime.Time);
  tEnd := trunc(EndDate.Date) + frac(EndTime.Time);
 {$ELSE}
  tStart := trunc(StartDate.Date) + StrToTime(StartTime.Text);
  tEnd := trunc(EndDate.Date) + StrToTime(EndTime.Text);
 {$ENDIF}

  if (tStart > tEnd) then begin
    res := MessageDlg(RSStartEndTimeError,
      mtConfirmation, [mbYes, mbNo], 0);
    if res = mrYes then begin
      StartDate.Date := trunc(tEnd);
      EndDate.Date := trunc(tStart);
     {$IFDEF NEW_TIME_EDIT}
      StartTime.Time := TimeOf(tEnd);
      EndTime.Time := TimeOf(tStart);
     {$ELSE}
      StartTime.Text := FormatDateTime('hh:nn', TimeOf(tEnd));
      EndTime.Text := FormatDateTime('hh:nn', TimeOf(tStart));
     {$ENDIF}
    end else
      exit;
  end;

  ReturnCode := rtCommit;
  Close;
end;

procedure TDlgEventEdit.CategoryDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  Color, SaveColor: TColor;
  Name: string;
  ColorRect: TRect;
begin
  Unused(Control, State);

  Category.Canvas.FillRect(ARect);

  Color := CatColorMap.GetCategory(Index).Color;
  Name := CatColorMap.GetCategory(Index).Description;

  SaveColor := Category.Canvas.Brush.Color;
  Category.Canvas.Brush.Color := Color;
  Category.Canvas.Pen.Color := clBlack;
  ColorRect.Left := ARect.Left + 3;
  ColorRect.Top := ARect.Top + 2;
  ColorRect.Bottom := ARect.Bottom - 2;
  ColorRect.Right := ColorRect.Left + 20;
  Category.Canvas.FillRect(ColorRect);
  {$IFDEF VERSION5}
  Category.Canvas.Rectangle(ColorRect);
  {$ELSE}
  Category.Canvas.Rectangle(ColorRect.Left, ColorRect.Top, ColorRect.Right,
    ColorRect.Bottom);
  {$ENDIF}
  ARect.Left := ColorRect.Right + 5;
  Category.Canvas.Brush.Color := SaveColor;
  Category.Canvas.TextOut(ARect.Left, ARect.Top, Name);
end;

procedure TDlgEventEdit.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TDlgEventEdit.PopulateDialog;
var
  I: Integer;
begin
  { Resource }
  ResourceNameLbl.Caption := Resource.Description;

  { Events }
  StartDate.Date := trunc(Event.StartTime);
  EndDate.Date := trunc(Event.EndTime);
  RepeatUntil.Date := trunc(Event.RepeatRangeEnd);
 {$IFDEF NEW_TIME_EDIT}
  StartTime.Time := frac(Event.StartTime);
  EndTime.Time := frac(Event.EndTime);
 {$ELSE}
  StartTime.Text := FormatDateTime('hh:nn', Event.StartTime);
  EndTime.Text := FormatDateTime('hh:nn', Event.EndTime);
 {$ENDIF}

  CBAllDay.Checked := Event.AllDayEvent;
  AlarmWavPath := Event.DingPath;

  StartDate.Enabled := not CBAllDay.Checked;
  EndDate.Enabled := not CBAllDay.Checked;
  StartTime.Enabled := not CBAllDay.Checked;
  EndTime.Enabled := not CBAllDay.Checked;

  DescriptionEdit.Text := Event.Description;
  LocationEdit.Text := Event.Location;
  NotesMemo.Text := Event.Notes;
  AlarmSet.Checked := Event.AlarmSet;
  AlarmSetClick(Self);
  if not Event.AlarmSet then
    AlarmAdvance.Text := '15'
  else
    AlarmAdvance.Text := IntToStr(Event.AlarmAdvance);
  AlarmAdvanceType.ItemIndex := Ord(Event.AlarmAdvanceType);
  RecurringType.ItemIndex := Ord(Event.RepeatCode);
  RecurringTypeChange(Self);
  CustomInterval.Text := IntToStr(Event.CustomInterval);

  Category.Items.Clear;

  for I := 0 to 9 do
    if (CatColorMap.GetName(I) <> '') then
      Category.Items.Add(CatColorMap.GetName(I));

  Category.ItemIndex := Event.Category;

  FLastEndTime := Event.EndTime;
end;

procedure TDlgEventEdit.DePopulateDialog;
begin
  { Events }
 {$IFDEF NEW_TIME_EDIT}
  Event.StartTime := StartDate.Date + StartTime.Time;
  Event.EndTime := EndDate.Date + EndTime.Time;
 {$ELSE}
  Event.StartTime := StartDate.Date + StrToTime(StartTime.Text);
  Event.EndTime := EndDate.Date + StrToTime(EndTime.Text);
 {$ENDIF}
  Event.RepeatRangeEnd := RepeatUntil.Date;
  Event.Description := DescriptionEdit.Text;
  Event.Location := LocationEdit.Text;
  Event.Notes := NotesMemo.Text;
  Event.Category := Category.ItemIndex;
  Event.AlarmSet := AlarmSet.Checked;
  Event.AlarmAdvance := StrToIntDef(AlarmAdvance.Text, 0);
  Event.AlarmAdvanceType := TVpAlarmAdvType(AlarmAdvanceType.ItemIndex);
  Event.RepeatCode := TVpRepeatType(RecurringType.ItemIndex);
  Event.CustomInterval := StrToIntDef(CustomInterval.Text, 0);
  Event.AllDayEvent := CBAllDay.Checked;
  Event.DingPath := AlarmWavPath;
end;

procedure TDlgEventEdit.PopLists;
{$IFNDEF NEW_TIME_EDIT}
var
  StringList: TStringList;
  I, Hour, Minute: Integer;
  MinStr, AMPMStr: string;
{$ENDIF}
begin
 {$IFNDEF NEW_TIME_EDIT}      // No longer needed for Lazarus using a TTimeEdit now.
 { Time Lists }
  StringList := TStringList.Create;
  try
    Minute := 0;
    AMPMStr := ' AM';
    for I := 0 to 96 do begin
      if I > 0 then Inc(Minute, 15);
      if Minute > 719 then
        AMPMStr := ' PM';
      if Minute = MinutesInDay then
        AMPMStr := ' AM';
      Hour := (Minute div 15) div 4;
      MinStr := IntToStr(Minute mod 60);
      if MinStr = '0' then MinStr := '00';
      if TimeFormat = tf24Hour then
        StringList.Add(IntToStr(Hour) + ':' + MinStr)
      else begin
        if Hour > 12 then Hour := Hour - 12;
        if Hour = 0 then Hour := 12;
        StringList.Add(IntToStr(Hour) + ':' + MinStr + AMPMStr);
      end;
    end;
    PlaceholderStartTime.Items.Assign(StringList);
    PlaceholderStartTime.ItemIndex := 0;

    EndTimePlaceholder.Items.Assign(StringList);
    EndTimePlaceholder.ItemIndex := 0;
  finally
    StringList.Free;
  end;
 {$ENDIF}

  { RecurringList }
  RecurringType.Items.Add(RSNone);
  RecurringType.Items.Add(RSDaily);
  RecurringType.Items.Add(RSWeekly);
  RecurringType.Items.Add(RSMonthlyByDay);
  RecurringType.Items.Add(RSMonthlyByDate);
  RecurringType.Items.Add(RSYearlyByDay);
  RecurringType.Items.Add(RSYearlyByDate);
  RecurringType.Items.Add(RSCustom);
  RecurringType.ItemIndex := 0;

  { Alarm Advance Type }
  AlarmAdvanceType.Items.Add(RSMinutes);
  AlarmAdvanceType.Items.Add(RSHours);
  AlarmAdvanceType.Items.Add(RSDays);
  AlarmAdvanceType.ItemIndex := 0;
end;
{=====}

procedure TDlgEventEdit.LoadCaptions;
begin
  OKBtn.Caption := RSOKBtn;
  CancelBtn.Caption := RSCancelBtn;
  AppointmentGroupBox.Caption := RSAppointmentGroupBox;
  DescriptionLbl.Caption := RSDescriptionLbl;
  LocationLbl.Caption := RSLocationLbl;
  CategoryLbl.Caption := RSCategoryLbl;
  StartTimeLbl.Caption := RSStartTimeLbl;
  EndTimeLbl.Caption := RSEndTimeLbl;
  AlarmSet.Caption := RSAlarmSet;
  RecurringLbl.Caption := RSRecurringLbl;
  IntervalLbl.Caption := RSIntervalLbl;
  RecurrenceEndsLbl.Caption := RSRecurrenceEndsLbl;
  CBAllDay.Caption := RSAllDayEvent;
end;
{=====}

procedure TDlgEventEdit.AlarmAdvanceChange(Sender: TObject);
var
  I: Integer;
  Str: string;
begin
  if AAVerifying then exit;

  AAVerifying := true;
  { Don't allow non numeric values. }
  Str := AlarmAdvance.Text;
  I := Length(Str);
  if (Str[I] > '9') or (Str[I] < '0') then
    Delete(Str, I, 1);
  AlarmAdvance.Text := Str;
  AAVerifying := false;

  if Str <> '' then
    AdvanceUpDown.Position := StrToInt(Str);
end;
{=====}

{ Inc or Dec AlarmAdvance according to which button was pressed }
procedure TDlgEventEdit.AdvanceUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext:
      AlarmAdvance.Text := IntToStr(StrToIntDef(AlarmAdvance.Text, 0) + 1);
    btPrev:
      AlarmAdvance.Text := IntToStr(StrToIntDef(AlarmAdvance.Text, 0) - 1);
  end;
end;
{=====}

procedure TDlgEventEdit.CustomIntervalChange(Sender: TObject);
var
  I: Integer;
  Str: string;
begin
  { Don't allow non numeric values. }
  if CIVerifying then Exit;
  CIVerifying := true;
  Str := CustomInterval.Text;
  for I := 1 to Length(Str) do
    if (Str[I] in ['0'..'9']) then
      Continue
    else
      Delete(Str, I, 1);
  CustomInterval.Text := Str;
  if Str <> '' then
    IntervalUpDown.Position := StrToInt(Str);
  CIVerifying := false;
end;
{=====}

procedure TDlgEventEdit.IntervalUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  Unused(Button);
  CustomInterval.Text := IntToStr(IntervalUpDown.Position);
end;
{=====}

procedure TDlgEventEdit.RecurringTypeChange(Sender: TObject);
begin
  if (RecurringType.ItemIndex > 0) and (RepeatUntil.Date <= StartDate.Date) then
    RepeatUntil.Date := StartDate.Date + 365;

  RecurrenceEndsLbl.Enabled := (RecurringType.ItemIndex > 0);
  RepeatUntil.Enabled := RecurrenceEndsLbl.Enabled;

  CustomInterval.Enabled := RecurringType.ItemIndex = 7;
  IntervalLbl.Enabled := CustomInterval.Enabled;
  IntervalUpDown.Enabled := CustomInterval.Enabled;
  if CustomInterval.Enabled then begin
    CustomInterval.Text := IntToStr(IntervalUpDown.Position);
    if Visible then
      CustomInterval.SetFocus;
  end;
end;
{=====}

procedure TDlgEventEdit.AlarmSetClick(Sender: TObject);
begin
  AlarmAdvance.Enabled  := AlarmSet.Checked;
  AlarmAdvanceType.Enabled  := AlarmSet.Checked;
  AdvanceUpDown.Enabled := AlarmSet.Checked;
  SoundFinderBtn.Enabled := AlarmSet.Checked;
  Event.SnoozeTime := 0.0;
end;
{=====}

procedure TDlgEventEdit.CBAllDayClick(Sender: TObject);
begin
  StartDate.Enabled := not CBAllDay.Checked;
  StartTime.Enabled := not CBAllDay.Checked;
  EndDate.Enabled := not CBAllDay.Checked;
  EndTime.Enabled := not CBAllDay.Checked;
end;
{=====}

procedure TDlgEventEdit.SoundFinderBtnClick(Sender: TObject);
var
  SoundFinder: TfrmSoundDialog;
begin
  SoundFinder := TFrmSoundDialog.Create(nil);
  try
    SoundFinder.DingPath := AlarmWavPath;
    SoundFinder.MediaFolder := Datastore.MediaFolder;
    SoundFinder.OnPlaySound := DoPlaySound;
    SoundFinder.Populate;
    if SoundFinder.ShowModal = mrOK then begin
      if SoundFinder.CBDefault.Checked then
        AlarmWavPath := ''
      else
        AlarmWavPath := SoundFinder.GetSelectedFilename;
    end;
  finally
    SoundFinder.Free;
  end;
end;
{=====}

procedure TDlgEventEdit.DoPlaySound(Sender: TObject; const AWavFile: String;
  AMode: TVpPlaySoundMode);
begin
  if DataStore <> nil then
    Datastore.PlaySound(AWavFile, AMode);
end;
{=====}

procedure TDlgEventEdit.FormShow(Sender: TObject);
begin
  PositionControls;
  DescriptionEdit.SetFocus;
 {$IFNDEF MSWINDOWS}
  if not Assigned(FDatastore.OnPlaySound) then
    SoundFinderBtn.Hide;
 {$ENDIF}
end;
{=====}

procedure TDlgEventEdit.PositionControls;
const
  DELTA = 8;
  VDELTA = 8;
  VDIST = 5;
var
  w, h: Integer;
  cnv: TControlCanvas;
  editHeight: Integer;
begin
  editHeight := startDate.Height;

  startDate.ButtonWidth := editHeight;
  endDate.ButtonWidth := editHeight;
 {$IFDEF NEW_TIME_EDIT}
  StartTime.ButtonWidth := editHeight;
  EndTime.ButtonWidth := editHeight;
 {$ENDIF}
  RepeatUntil.ButtonWidth := editHeight;

  cnv := TControlCanvas.Create;
  try
    cnv.Control := StartDate;
    w := cnv.TextWidth(FormatDateTime(' dd. mm. yyyy ', EncodeDate(2000,12,30)));
    Startdate.Width := w + StartDate.ButtonWidth;
    EndDate.Width := StartDate.Width;
    StartTime.Width := StartDate.Width;
    EndTime.Width := StartDate.Width;
  finally
    cnv.Free;
  end;
  RepeatUntil.Width := StartDate.Width;
//  CustomInterval.Left := RepeatUntil.Left;
  AlarmAdvance.Width := AdvanceUpDown.Left - 2 - AlarmAdvance.Left;
  AlarmAdvanceType.Width := StartTime.Width;

  AlignOKCancel(OKBtn, CancelBtn, ButtonPanel);
end;


{ TVpEventEditDialog }

constructor TVpEventEditDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlacement.Position := mpCenterTop;
  FPlacement.Height := 415;
  FPlacement.Width := 710;
end;
{=====}

function TVpEventEditDialog.Execute(Event: TVpEvent): Boolean;
var
  DlgEventEdit: TDlgEventEdit;
begin
  ceEvent := Event;
  DlgEventEdit := TDlgEventEdit.Create(Self);
  try
    DoFormPlacement(DlgEventEdit);
    SetFormCaption(DlgEventEdit, Event.Description, RSDlgEventEdit);
    DlgEventEdit.Datastore := Datastore;
    DlgEventEdit.Event := Event;
    DlgEventEdit.TimeFormat := FTimeFormat;
    DlgEventEdit.Resource := DataStore.Resource;
    DlgEventEdit.CatColorMap := DataStore.CategoryColorMap;
    DlgEventEdit.PopulateDialog;
    DlgEventEdit.ShowModal;
    result := (DlgEventEdit.ReturnCode = rtCommit);
    if Result then begin
      DlgEventEdit.DePopulateDialog;
//      DataStore.PostEvents;
    end;
  finally
    DlgEventEdit.Release;
  end;
end;
{=====}

function TVpEventEditDialog.AddNewEvent(PlaceholderStartTime, EndTimePlaceholder: TDateTime): Boolean;
begin
  Result := false;
  if DataStore <> nil then begin
    ceEvent := DataStore.Resource.Schedule.AddEvent(
      DataStore.GetNextID(EventsTableName),
      PlaceholderStartTime, EndTimePlaceholder
    );
    if ceEvent <> nil then begin
      Result := Execute(ceEvent);
      if (not Result) then
        ceEvent.Free;
    end;
  end;
end;

end.
 
