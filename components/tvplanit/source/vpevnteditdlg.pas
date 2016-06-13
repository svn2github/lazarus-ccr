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

{$I Vp.INC}

unit VpEvntEditDlg;
  { The default event edit dialog }

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf, LResources, EditBtn,
  {$ELSE}
  Windows, Messages, Mask,
  {$ENDIF}
  SysUtils, {$IFDEF VERSION6}Variants,{$ENDIF} Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  VpData, VpBase, VpBaseDS, VpDlg, VpConst; //VpEdPop,

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
    LocationEdit: TEdit;
    LocationLbl: TLabel;
    NotesMemo: TMemo;
    StartDate: TDateEdit;
    EndDate: TDateEdit;
    RepeatUntil: TDateEdit;
    Panel1: TPanel;
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
    Image2: TImage;
    RecurringLbl: TLabel;
    Bevel3: TBevel;
    IntervalLbl: TLabel;
    Image1: TImage;
    SoundFinderBtn: TSpeedButton;
    DescriptionEdit: TEdit;
    AlarmSet: TCheckBox;
    Category: TComboBox;
    RecurringType: TComboBox;
    IntervalUpDown: TUpDown;
    AlarmAdvanceType: TComboBox;
    AdvanceUpDown: TUpDown;
    CBAllDay: TCheckBox;
    edtUnusedPlaceholder: TEdit;
    imgClock: TImage;
    RecurrenceEndsLbl: TLabel;
    procedure CategoryDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartDateChange(Sender: TObject);
    procedure StartTimeChange(Sender: TObject);
    procedure EndTimeChange(Sender: TObject);
    procedure AlarmAdvanceChange(Sender: TObject);
    procedure AdvanceUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure CustomIntervalChange(Sender: TObject);
    procedure IntervalUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure RecurringTypeChange(Sender: TObject);
    procedure AlarmSetClick(Sender: TObject);
    procedure EndDateChange(Sender: TObject);
    procedure CBAllDayClick(Sender: TObject);
    procedure SoundFinderBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StartTimeExit(Sender: TObject);
    procedure EndTimeExit(Sender: TObject);
  private { Private declarations }
   {$IFDEF LCL}
    StartTime: TTimeEdit;
    EndTime: TTimeEdit;
   {$ENDIF}
   {$IFDEF DELPHI}
    StartTime: TComboBox;
    EndTime: TComboBox;
   {$ENDIF}
    FDatastore: TVpCustomDatastore;
    AAVerifying: Boolean;
    CIVerifying: Boolean;
    FCustomInterval : TVpRightAlignedEdit;
    procedure PopLists;
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
    function Execute(Event: TVpEvent; TimeFormat: TVpTimeFormat = tf12Hour): Boolean; reintroduce;
    function AddNewEvent(StartTime, EndTime: TDateTime): Boolean;
  published
    {properties}
    property TimeFormat: TVpTimeFormat read FTimeFormat write FTimeFormat default tf12Hour;
    property DataStore;
    property Options;
    property Placement;
  end;

implementation

uses
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
const
  DELTA = 8;
begin
 {$IFDEF LCL}
  StartTime := TTimeEdit.Create(self);
 {$ELSE}
  StartTime := TCombobox.Create(self);
  StartTime.Width := 93;
  StartTime.ItemIndex := -1;
 {$ENDIF}
  StartTime.Parent := AppointmentGroupbox;
  StartTime.Left := AlarmAdvanceType.Left;
  StartTime.Top := StartDate.Top;

 {$IFDEF LCL}
  EndTime := TTimeEdit.Create(self);
 {$ELSE}
  EndTime := TCombobox.Create(self);
  EndTime.Width := 93;
  EndTime.ItemIndex := -1;
 {$ENDIF}
  EndTime.Parent := AppointmentGroupbox;
  EndTime.Left := AlarmAdvanceType.Left;
  EndTime.Top := EndDate.Top;

  ReturnCode := rtAbandon;
  PopLists;
  LoadCaptions;
  EndDate.Enabled := False;

  FCustomInterval := TVpRightAlignedEdit.Create(Self);
  with FCustomInterval do begin
    Parent := AppointmentGroupbox;
    Top := IntervalUpDown.Top + 1;
    Left := IntervalUpDown.Left - 65;
    Height := IntervalUpDown.Height - 1;
    Width := 65;
    MaxLength := 5;
    OnChange := CustomIntervalChange;
    TabOrder := edtUnusedPlaceholder.TabOrder;
  end;
  IntervalUpDown.Associate := FCustomInterval;

  DescriptionLbl.Left := DescriptionEdit.Left - GetLabelWidth(DescriptionLbl) - DELTA;
  LocationLbl.Left := LocationEdit.Left - GetLabelWidth(LocationLbl) - DELTA;
  CategoryLbl.Left := Category.Left - GetLabelWidth(CategoryLbl) - DELTA;
  StartTimeLbl.Left := StartDate.Left - GetLabelWidth(StartTimeLbl) - DELTA;
  EndTimeLbl.Left := EndDate.Left - GetLabelWidth(EndTimeLbl) - DELTA;
  RecurrenceEndsLbl.Left := RepeatUntil.Left - GetLabelWidth(RecurrenceEndsLbl) - DELTA;
end;
{=====}

procedure TDlgEventEdit.OKBtnClick(Sender: TObject);
begin
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
  Category.Canvas.FillRect(ARect);
  Color := clBlack;
  case Index of
    0: begin
         Color := CatColorMap.Category0.Color;
         Name := CatColorMap.Category0.Description;
       end;
    1: begin
         Color := CatColorMap.Category1.Color;
         Name := CatColorMap.Category1.Description;
       end;
    2: begin
         Color := CatColorMap.Category2.Color;
         Name := CatColorMap.Category2.Description;
       end;
    3: begin
         Color := CatColorMap.Category3.Color;
         Name := CatColorMap.Category3.Description;
       end;
    4: begin
         Color := CatColorMap.Category4.Color;
         Name := CatColorMap.Category4.Description;
       end;
    5: begin
         Color := CatColorMap.Category5.Color;
         Name := CatColorMap.Category5.Description;
       end;
    6: begin
         Color := CatColorMap.Category6.Color;
         Name := CatColorMap.Category6.Description;
       end;
    7: begin
         Color := CatColorMap.Category7.Color;
         Name := CatColorMap.Category7.Description;
       end;
    8: begin
         Color := CatColorMap.Category8.Color;
         Name := CatColorMap.Category8.Description;
       end;
    9: begin
         Color := CatColorMap.Category9.Color;
         Name := CatColorMap.Category9.Description;
       end;
  end; {Case}

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

{=====}

procedure TDlgEventEdit.CancelBtnClick(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TDlgEventEdit.PopulateDialog;
var
  I: Integer;
begin
  { Resource }
  ResourceNameLbl.Caption := Resource.Description;

  { Events }
  StartDate.Date := Event.StartTime;
  EndDate.Date := Event.EndTime;
  RepeatUntil.Date := Event.RepeatRangeEnd;
  StartTime.Text := FormatDateTime('hh:mm',Event.StartTime);
  EndTime.Text := FormatDateTime('hh:mm',Event.EndTime);

  StartTimeChange(Self);
  CBAllDay.Checked := Event.AllDayEvent;
  AlarmWavPath := Event.DingPath;

  StartDate.Enabled := not CBAllDay.Checked;
  EndDate.Enabled := not CBAllDay.Checked;
  EndTime.Enabled := not CBAllDay.Checked;
  StartTime.Enabled := not CBAllDay.Checked;

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
  FCustomInterval.Text := IntToStr(Event.CustomInterval);

  Category.Items.Clear;

  for I := 0 to 9 do
    if (CatColorMap.GetName(I) <> '') then
      Category.Items.Add(CatColorMap.GetName(I));

  Category.ItemIndex := Event.Category;

  FLastEndTime := Event.EndTime;                                         
end;
{=====}

procedure TDlgEventEdit.DePopulateDialog;
begin
  { Events }
  Event.StartTime := StartDate.Date + StrToTime(StartTime.Text);
  Event.EndTime := EndDate.Date + StrToTime(EndTime.Text);
  Event.RepeatRangeEnd := RepeatUntil.Date;
  Event.Description := DescriptionEdit.Text;
  Event.Location := LocationEdit.Text;
  Event.Notes := NotesMemo.Text;
  Event.Category := Category.ItemIndex;
  Event.AlarmSet := AlarmSet.Checked;
  Event.AlarmAdvance := StrToIntDef(AlarmAdvance.Text, 0);
  Event.AlarmAdvanceType := TVpAlarmAdvType(AlarmAdvanceType.ItemIndex);
  Event.RepeatCode := TVpRepeatType(RecurringType.ItemIndex);
  Event.CustomInterval := StrToIntDef(FCustomInterval.Text, 0);
  Event.AllDayEvent := CBAllDay.Checked;
  Event.DingPath := AlarmWavPath;
end;
{=====}

procedure TDlgEventEdit.StartDateChange(Sender: TObject);
begin
  if StartDate.Date > EndDate.Date then
    EndDate.Date := StartDate.Date;
end;
{=====}

procedure TDlgEventEdit.EndDateChange(Sender: TObject);
begin
  if StartDate.Date > EndDate.Date then
    StartDate.Date := EndDate.Date;
end;
{=====}

procedure TDlgEventEdit.StartTimeChange(Sender: TObject);
var
  ST: TDateTime;
begin
  { Verify the value is valid }
  try
    ST := StrToTime(StartTime.Text);
  except
    StartTime.Color := clRed;
    if Visible then
      StartTime.SetFocus;
    Exit;
  end;
  StartTime.Color := clWindow;

  { if the end time is less than the start time then change the end time to }
  { follow the start time by 30 minutes }
  if ST > StrToTime(EndTime.Text) then begin
    if TimeFormat = tf24Hour then
      EndTime.Text := FormatDateTime('h:mm', ST + 30 / MinutesInDay)
    else
      EndTime.Text := FormatDateTime('hh:mm AM/PM', ST + 30 / MinutesInDay);
  end;

end;
{=====}

procedure TDlgEventEdit.EndTimeChange(Sender: TObject);

  function IsMidnight(ATime: TDateTime) : Boolean;
  begin                                                                  
    Result := ATime = Trunc(ATime);
  end;                                                                   

var                                                                      
  ET: TDateTime;                                                         

begin
  { Verify the value is valid }
  try
    ET := StrToTime(EndTime.Text);
    if (IsMidnight(ET)) and (not IsMidnight(FLastEndTime)) then
      EndDate.Date := EndDate.Date + 1                                   
    else if (not IsMidnight(ET)) and (IsMidnight(FLastEndTime)) then
      EndDate.Date := EndDate.Date - 1;                                  
    FLastEndTime := ET;                                                  
  except
    EndTime.Color := clRed;
    EndTime.SetFocus;
    Exit;
  end;
  EndTime.Color := clWindow;

  { if the end time is less than the start time then change the start time to }
  { precede the end time by 30 minutes }
  if ET < StrToTime(StartTime.Text) then begin
    if TimeFormat = tf24Hour then
      StartTime.Text := FormatDateTime('h:mm', ET - 30 / MinutesInDay)
    else
      StartTime.Text := FormatDateTime('h:mm AM/PM', ET - 30 / MinutesInDay);
  end;
end;
{=====}

procedure TDlgEventEdit.PopLists;
{$IFDEF DELPHI}
var
  StringList: TStringList;
  I, Hour, Minute: Integer;
  MinStr, AMPMStr: string;
{$ENDIF}
begin
 {$IFDEF DELPHI}      // No longer needed for Lazarus using a TTimeEdit now.
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
    StartTime.Items.Assign(StringList);
    StartTime.ItemIndex := 0;

    EndTime.Items.Assign(StringList);
    EndTime.ItemIndex := 0;
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
  if (Str[I] > #57) or (Str[I] < #48) then
    Delete(Str, I, 1);
  AlarmAdvance.Text := Str;
  AAVerifying := false;

  if Str <> '' then
    AdvanceUpDown.Position := StrToInt(Str);
end;
{=====}

procedure TDlgEventEdit.AdvanceUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  { Inc or Dec AlarmAdvance according to which button was pressed }
{  case Button of
    btNext:
      AlarmAdvance.Text := IntToStr(StrToIntDef(AlarmAdvance.Text, 0) + 1);
    btPrev:
      AlarmAdvance.Text := IntToStr(StrToIntDef(AlarmAdvance.Text, 0) - 1);
  end;}
  AlarmAdvance.Text := IntToStr(AdvanceUpDown.Position);
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
  Str := FCustomInterval.Text;
  for I := 1 to Length(Str) do
    if (Ord(Str[I]) in [48..57]) then
      Continue
    else
      Delete(Str, I, 1);
  FCustomInterval.Text := Str;
  if Str <> '' then
    IntervalUpDown.Position := StrToInt(Str);
  CIVerifying := false;
end;
{=====}

procedure TDlgEventEdit.IntervalUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  FCustomInterval.Text := IntToStr(IntervalUpDown.Position);
end;
{=====}

procedure TDlgEventEdit.RecurringTypeChange(Sender: TObject);
begin
  if (RecurringType.ItemIndex > 0) and (RepeatUntil.Date <= StartDate.Date) then
    RepeatUntil.Date := StartDate.Date + 365;

  RecurrenceEndsLbl.Enabled := (RecurringType.ItemIndex > 0);
  RepeatUntil.Enabled := RecurrenceEndsLbl.Enabled;

  FCustomInterval.Enabled := RecurringType.ItemIndex = 7;
  IntervalLbl.Enabled := FCustomInterval.Enabled;
  IntervalUpDown.Enabled := FCustomInterval.Enabled;
  if FCustomInterval.Enabled then begin
    FCustomInterval.Text := IntToStr(IntervalUpDown.Position);
    if Visible then
      FCustomInterval.SetFocus;
  end;

end;
{=====}

procedure TDlgEventEdit.AlarmSetClick(Sender: TObject);
begin
  AlarmAdvance.Enabled  := AlarmSet.Checked;
  AlarmAdvanceType.Enabled  := AlarmSet.Checked;
  AdvanceUpDown.Enabled := AlarmSet.Checked;
  Event.SnoozeTime := 0.0;
end;
{=====}

procedure TDlgEventEdit.CBAllDayClick(Sender: TObject);
begin
  StartDate.Enabled := not CBAllDay.Checked;
  EndDate.Enabled := not CBAllDay.Checked;
  EndTime.Enabled := not CBAllDay.Checked;
  StartTime.Enabled := not CBAllDay.Checked;
end;
{=====}

procedure TDlgEventEdit.SoundFinderBtnClick(Sender: TObject);
var
  SoundFinder: TfrmSoundDialog;
begin
  Application.CreateForm(TfrmSoundDialog, SoundFinder);
  try
    SoundFinder.DingPath := AlarmWavPath;
    SoundFinder.Populate;
    SoundFinder.OnPlaySound := DoPlaySound;
    SoundFinder.ShowModal;
    if SoundFinder.ReturnCode = TvpEditorReturnCode(rtCommit) then begin
      if SoundFinder.CBDefault.Checked then
        AlarmWavPath := ''
      else
        AlarmWavPath := SoundFinder.FileListBox1.FileName;
    end;
  finally
    SoundFinder.Release;
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
  DescriptionEdit.SetFocus;
end;
{=====}

{ TVpEventEditDialog }

constructor TVpEventEditDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlacement.Position := mpCenterTop;
  FPlacement.Height := 415;
  FPlacement.Width  := 710;
end;
{=====}

function TVpEventEditDialog.Execute(Event: TVpEvent;
  TimeFormat: TVpTimeFormat = tf12Hour): Boolean;
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
      DataStore.PostEvents;
    end;
  finally
    DlgEventEdit.Release;
  end;
end;
{=====}

function TVpEventEditDialog.AddNewEvent(StartTime, EndTime: TDateTime): Boolean;
begin
  Result := false;
  if DataStore <> nil then begin
    ceEvent := DataStore.Resource.Schedule.AddEvent(
      DataStore.GetNextID(EventsTableName), StartTime, EndTime);
    if ceEvent <> nil then begin
      Result := Execute(ceEvent);
      if (not Result) or (ceEvent = nil) then
        ceEvent.Free;
    end;
  end;
end;
{=====}

procedure TDlgEventEdit.StartTimeExit(Sender: TObject);                  
var                                                                      
  ST : TDateTime;                                                        
begin                                                                    
  { Verify the value is valid }
  try                                                                    
    ST := StartDate.Date + StrToTime(StartTime.Text);
  except
    StartTime.Color := clRed;
    StartTime.SetFocus;
    Exit;
  end;                                                                   
  StartTime.Color := clWindow;

  { If the end time is less than the start time then change the end    }
  {  time to  follow the start time by 30 minutes                      } 

  if ST > EndDate.Date + StrToTime (EndTime.Text) then
    EndTime.Text := FormatDateTime('hh:mm', ST + 30 / MinutesInDay);
end;                                                                     

procedure TDlgEventEdit.EndTimeExit(Sender: TObject);                    
var                                                                      
  ET: TDateTime;
begin                                                                    
  { Verify the value is valid }
  try
    ET := EndDate.Date + StrToTime(EndTime.Text);
  except
    EndTime.Color := clRed;
    EndTime.SetFocus;
    Exit;
  end;
  EndTime.Color := clWindow;

  { If the end time is less than the start time then change the        }
  { start time to precede the end time by 30 minutes                   } 

  if ET < StartDate.Date + StrToTime (StartTime.Text) then
    StartTime.Text := FormatDateTime('hh:mm', ET - 30 / MinutesInDay);
end;                                                                     

end.
 
