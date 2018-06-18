{*********************************************************}
{*                 VPWEEKVIEW.PAS 1.03                   *}
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

{
  This unit handles the TVpWeekView component as well as it's inline editor
  and navigation.

  The rendering of Visual PlanIt components is a bit involved.  The component's
  Paint method calls RenderToCanvas.  The RenderToCanvas method of each of
  the visual VisualPlanIt controls is repsonsible both for drawing to the
  screen (both design and run time) as well as printing.  In the case of
  printing, the component needs to render itself to an arbitrary rectangle
  and possibly rotated (for the screen the rectangle is the ClientRect
  and the rotation angle is always zero).  To achieve that goal, the
  functions in VpCanvasUtils are used to go between the rendering of the
  control and the TCanvas that it needs to render to.  
}
{$I vp.inc}

unit VpWeekView;

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf, FileUtil,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Controls, ComCtrls, ExtCtrls, StdCtrls, Forms, Menus,
  VpConst, VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpCanvasUtils, VpDayView;

type
  TVpWeekdayRec = packed record
    Rec: TRect;
    Day: TDateTime;
  end;

  TVpWeekViewLayout = (wvlVertical, wvlHorizontal);

type
  TVpWeekdayArray = array of TVpWeekdayRec;

  { Forward Declarations }
  TVpWeekView = class;

  TVpWvInPlaceEdit = class(TCustomEdit)
  protected{private}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVpWvHeadAttributes = class(TPersistent)
  protected{ private }
    FOwner: TVpWeekView;
    FColor: TColor;
    FFont: TVpFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(Value: TVpFont);
  public
    constructor Create(AOwner: TVpWeekView);
    destructor Destroy; override;
    property Owner: TVpWeekView read FOwner;
  published
    property Font: TVpFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
  end;

  TVpDayHeadAttr = class(TPersistent)
  protected{private}
    FWeekView: TVpWeekView;
    FFont: TVpFont;
    FDateFormat: string;
    FColor: TColor;
    FBordered: Boolean;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TVpFont);
    procedure SetBordered(Value: Boolean);
    procedure SetDateFormat(Value: string);
  public
    constructor Create(AOwner: TVpWeekView);
    destructor Destroy; override;
    property WeekView: TVpWeekView read FWeekView;
  published
    property Color: TColor read FColor write SetColor;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property Font: TVpFont read FFont write SetFont;
    property Bordered: Boolean read FBordered write SetBordered;
  end;

  { TVpWeekView }

  TVpWeekView = class(TVpLinkableControl)
  private
    FComponentHint: TTranslateString;
    FHintMode: TVpHintMode;
    FMouseEvent: TVpEvent;
    FLayout: TVpWeekviewLayout;
    FOnHoliday: TVpHolidayEvent;
    procedure SetActiveEvent(AValue: TVpEvent);
    procedure SetLayout(AValue: TVpWeekviewLayout);
  protected{ private }
    FActiveDate: TDateTime;
    FColumnWidth: Integer;
    FColor: TColor;
    FDateLabelFormat: string;
    FDayHeadAttributes: TVpDayHeadAttr;
    FDrawingStyle: TVpDrawingStyle;
    FActiveEvent: TVpEvent;
    FHeadAttr: TVpWvHeadAttributes;
    FEventFont: TVpFont;  // was: TFont
    FLineColor: TColor;
    FLineCount: Integer;
    FTimeFormat: TVpTimeFormat;
    FShowEventTime: Boolean;
    FVisibleLines: Integer;
    FWeekStartsOn: TVpDayType;
    FDefaultPopup: TPopupMenu;
    FAllDayEventAttr: TVpAllDayEventAttributes;
    FAllowInplaceEdit: Boolean;
    FAllowDragAndDrop: Boolean;
    FDragDropTransparent: Boolean;
    { event variables }
    FBeforeEdit: TVpBeforeEditEvent;
    FAfterEdit: TVpAfterEditEvent;
    FOwnerEditEvent: TVpEditEvent;
    FOnAddEvent: TVpOnAddNewEvent;
    { internal variables }
    wvInLinkHandler: Boolean;
    wvClickTimer: TTimer;
    wvLoaded: Boolean;
    wvRowHeight: Integer;
//    wvDayHeadHeight: Integer;
    wvHeaderHeight: Integer;
    wvStartDate: TDateTime;
    wvSpinButtons: TUpDown;
    wvEventList: TList;
    wvEventArray: TVpEventArray;
    wvWeekdayArray: TVpWeekdayArray;
    wvActiveEventRec: TRect;
    wvInPlaceEditor: TVpWvInPlaceEdit;
    wvCreatingEditor: Boolean;
    wvPainting: Boolean;
    wvDragging: Boolean;
    wvMouseDown: Boolean;
    wvMouseDownPoint: TPoint;
    wvHotPoint: TPoint;

    { property methods }
    procedure SetDrawingStyle(Value: TVpDrawingStyle);
    procedure SetColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetDateLabelFormat(Value: string);
    procedure SetEventFont(Value: TVpFont);
    procedure SetShowEventTime(Value: Boolean);
    procedure SetTimeFormat(Value: TVpTimeFormat);
    procedure SetActiveDate(Value: TDateTime);
    procedure SetWeekStartsOn(Value: TVpDayType);

    { internal methods }
    procedure wvEditInPlace(Sender: TObject);
    procedure wvHookUp;
    procedure PopupAddEvent(Sender: TObject);
    procedure PopupAddFromICalFile(Sender: TObject);
    procedure PopupDeleteEvent(Sender: TObject);
    procedure PopupEditEvent(Sender: TObject);
    procedure PopupToday(Sender: TObject);
    procedure PopupNextWeek(Sender: TObject);
    procedure PopupPrevWeek(Sender: TObject);
    procedure PopupNextMonth(Sender: TObject);
    procedure PopupPrevMonth(Sender: TObject);
    procedure PopupNextYear(Sender: TObject);
    procedure PopupPrevYear(Sender: TObject);
    procedure PopupPickResourceGroupEvent(Sender: TObject);
    procedure PopupDropdownEvent(Sender: TObject);
    procedure InitializeDefaultPopup;
    procedure wvPopulate;
    procedure wvSpinButtonClick(Sender: TObject; Button: TUDBtnType);

    { event related methods }
    procedure EditEvent;
    procedure EndEdit(Sender: TObject);
    function EventAtCoord(Pt: TPoint): Boolean;
    function GetEventAtCoord(Pt: TPoint): TVpEvent;
    function GetEventRect(AEvent: TVpEvent): TRect;
    procedure wvSetDateByCoord(Point: TPoint);
    procedure wvSpawnEventEditDialog(IsNewEvent: Boolean);

    { inherited standard methods }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure Paint; override;

    { drag and drop }
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;

    { hints }
    procedure ShowHintWindow(APoint: TPoint; AEvent: TVpEvent);
    procedure HideHintWindow;
    procedure SetHint(const AValue: TTranslateString); override;

    { message handlers }
    {$IFNDEF LCL}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMRButtonDown(var Msg : TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
              message CM_WANTSPECIALKEY;
    {$ELSE}
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
    procedure WMLButtonDblClk(var Msg : TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    //TODO: Bug 0020755 braks this in GTK2...
    {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildEventString(AEvent: TVpEvent; AStartTime, AEndTime: TDateTime;
      UseAsHint: Boolean): String;
    procedure LoadLanguage;
    procedure DeleteActiveEvent(Verify: Boolean);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure Invalidate; override;
    function IsHoliday(ADate: TDate; out AHolidayName: String): Boolean;
    procedure LinkHandler(Sender: TComponent;
      NotificationType: TVpNotificationType; const Value: Variant); override;
    function GetControlType: TVpItemType; override;
    procedure EditSelectedEvent(IsNewEvent: Boolean = false);
    procedure PaintToCanvas(ACanvas: TCanvas;  ARect: TRect;
      Angle: TVpRotationAngle; ADate: TDateTime);
    procedure RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
      Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
      StartLine: Integer; StopLine: Integer; UseGran: TVpGranularity;
      DisplayOnly: Boolean); override;

    {$IF VP_LCL_SCALING = 2}
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
    {$ELSEIF VP_LCL_SCALING = 1}
    procedure ScaleFontsPPI(const AProportion: Double); override;
    {$ENDIF}

    property ActiveEvent: TVpEvent read FActiveEvent write SetActiveEvent;
    property Date: TDateTime read FActiveDate write SetActiveDate;
    property VisibleLines: Integer read FVisibleLines;

  published
    property AllDayEventAttributes: TVpAllDayEventAttributes read FAllDayEventAttr write FAllDayEventAttr;
    property AllowDragAndDrop: Boolean read FAllowDragAndDrop write FAllowDragAndDrop default false;
    property AllowInplaceEditing: Boolean read FAllowInplaceEdit write FAllowInplaceEdit default true;
    property Color: TColor read FColor write SetColor;
    property DateLabelFormat: string read FDateLabelFormat write SetDateLabelFormat;
    property DayHeadAttributes: TVpDayHeadAttr read FDayHeadAttributes write FDayHeadAttributes;
    property DragDropTransparent: Boolean read FDragDropTransparent write FDragDropTransparent default false;
    property DrawingStyle: TVpDrawingStyle read FDrawingStyle write SetDrawingStyle stored True;
    property EventFont: TVpFont read FEventFont write SetEventFont;
    property HeadAttributes: TVpWvHeadAttributes read FHeadAttr write FHeadAttr;
    property HintMode: TVpHintMode read FHintMode write FHintMode default hmPlannerHint;
    property LineColor: TColor read FLineColor write SetLineColor;
    property Layout: TVpWeekviewLayout read FLayout write SetLayout default wvlVertical;
    property TimeFormat: TVpTimeFormat read FTimeFormat write SetTimeFormat;
    property ShowEventTime: Boolean read FShowEventTime write SetShowEventTime;
    property WeekStartsOn: TVpDayType read FWeekStartsOn write SetWeekStartsOn;

    {inherited properties}
    property Align;
    property Anchors;
    {$IFDEF LCL}
    property BorderSpacing;
    {$ENDIF}
    property TabStop;
    property TabOrder;

    {events}
    property AfterEdit : TVpAfterEditEvent read FAfterEdit write FAfterEdit;
    property BeforeEdit: TVpBeforeEditEvent read FBeforeEdit write FBeforeEdit;
    property OnAddEvent: TVpOnAddNewEvent read FOnAddEvent write FOnAddEvent;
    property OnHoliday: TVpHolidayEvent read FOnHoliday write FOnHoliday;
    property OnOwnerEditEvent: TVpEditEvent read FOwnerEditEvent write FOwnerEditEvent;
  end;


implementation

uses
 {$IFDEF LCL}
  DateUtils,
 {$ENDIF}
  SysUtils, StrUtils, LazUTF8, Dialogs,
  VpEvntEditDlg, VpWeekViewPainter, VpICal;

(*****************************************************************************)
{ TVpTGInPlaceEdit }
(*****************************************************************************)

constructor TVpWvInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  BorderStyle := bsNone;
  {$IFDEF VERSION4}
//  DoubleBuffered := False;
  {$ENDIF}
end;
{=====}

procedure TVpWvInPlaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
//  Params.Style := Params.Style or ES_MULTILINE;
end;
{=====}

procedure TVpWvInPlaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Grid: TVpWeekView;
begin
  Grid := TVpWeekView(Owner);

  case Key of
  VK_RETURN:
    begin
      Key := 0;
      Grid.EndEdit(Self);
    end;

  VK_UP:
    begin
      Key := 0;
      Grid.EndEdit(Self);
    end;

  VK_DOWN:
    begin
      Key := 0;
      Grid.EndEdit(Self);
    end;

  VK_ESCAPE:
    begin
      Key := 0;
      Hide;
      Grid.SetFocus;
    end;

  else
    inherited;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpContactHeadAttr }
constructor TVpDayHeadAttr.Create(AOwner: TVpWeekView);
begin
  inherited Create;
  FWeekView := AOwner;
  FDateFormat := 'dddd mmmm, dd';
  FFont := TVpFont.Create(AOwner);
//  FFont.Assign(FWeekView.Font);
//  FFont.Size := 8;
  FColor := clSilver;
  FBordered := true;
end;
{=====}

destructor TVpDayHeadAttr.Destroy;
begin
  FFont.Free;
end;
{=====}

procedure TVpDayHeadAttr.SetBordered(Value: Boolean);
begin
  if Value <> FBordered then begin
    FBordered := Value;
    WeekView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetDateFormat(Value: string);
begin
  if Value <> FDateFormat then begin
    FDateFormat := Value;
    WeekView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    WeekView.Invalidate;
  end;
end;
{=====}

procedure TVpDayHeadAttr.SetFont(Value: TVpFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    WeekView.Invalidate;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpWeekView }

constructor TVpWeekView.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  HintWindowClass := TVpHintWindow;

  { Create internal classes and stuff }
  FDayHeadAttributes := TVpDayHeadAttr.Create(self);
  FHeadAttr := TVpWvHeadAttributes.Create(self);
  FAllDayEventAttr := TVpAllDayEventAttributes.Create(self);

  FEventFont := TVpFont.Create(self);
  FEventFont.Assign(Font);
  FShowEventTime := true;
  wvInLinkHandler := false;
  wvEventList := TList.Create;
  wvClickTimer := TTimer.Create(self);
  wvSpinButtons := TUpDown.Create(self);
  wvSpinButtons.OnClick := wvSpinButtonClick;
  wvSpinButtons.Orientation := udHorizontal;
  wvSpinButtons.Min := -32768;
  wvSpinButtons.Max := 32767;
  wvHotPoint := Point(0, 0);
  wvDragging := false;
  wvMouseDownPoint := Point(0, 0);
  wvMouseDown := false;
  DragMode := dmManual;

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
//  DoubleBuffered := true;
  {$ENDIF}

  FWeekStartsOn := dtMonday;
  wvClickTimer.Enabled := false;
  wvClickTimer.Interval := ClickDelay;
  wvClickTimer.OnTimer := wvEditInPlace;
  wvCreatingEditor := false;
  FDrawingStyle := ds3d;
  wvPainting := false;
  FColor := clWindow;
  FLineColor := clGray;
  wvStartDate := trunc(GetStartOfWeek(Now, FWeekStartsOn));
  FTimeFormat := tf12Hour;
  FDateLabelFormat := 'dddd, mmmm dd, yyyy';
  FColumnWidth := 200;
  FAllowInplaceEdit := true;

  { set up fonts and colors }
//  FDayHeadAttributes.Font.Name := 'Tahoma';
  FDayHeadAttributes.Font.Size := 10;
  FDayHeadAttributes.Font.Style := [];
  FDayHeadAttributes.Color := clBtnFace;
  FDayHeadAttributes.Bordered := true;

  SetLength(wvEventArray, MaxVisibleEvents);
  SetLength(wvWeekdayArray, 7);

  { size }
  Height := 225;
  Width := 300;

  FDefaultPopup := TPopupMenu.Create (Self);
  Self.PopupMenu := FDefaultPopup;
  FDefaultPopup.OnPopup := PopupDropDownEvent;
  LoadLanguage;

  FAllDayEventAttr.BackgroundColor := Color;
  FAllDayEventAttr.EventBackgroundColor := clBtnFace;
  FAllDayEventAttr.EventBorderColor := LineColor;
  FAllDayEventAttr.Font.Assign (Font);

  wvHookUp;
  SetActiveDate(Now);
end;

destructor TVpWeekView.Destroy;
begin
  FreeAndNil(wvInplaceEditor);
  FDayHeadAttributes.Free;
  FAllDayEventAttr.Free;
  FHeadAttr.Free;
  wvClickTimer.Free;
  FEventFont.Free;
  wvSpinButtons.Free;
  wvEventList.Free;
  FDefaultPopup.Free;
  inherited;
end;

function TVpWeekView.BuildEventString(AEvent: TVpEvent;
  AStartTime, AEndTime: TDateTime; UseAsHint: Boolean): String;
var
  timeFmt: String;
  timeStr: String;
  s: String;
  res: TVpResource;
  grp: TVpResourceGroup;
  isOverlayed: Boolean;
  showDetails: Boolean;
begin
  Result := '';

  if (AEvent = nil) or (Datastore = nil) or (Datastore.Resource = nil) then
    exit;

  grp := Datastore.Resource.Group;
  showDetails := (grp <> nil) and (odEventDescription in grp.ShowDetails);
  isOverlayed := AEvent.IsOverlayed;
  timefmt := GetTimeFormatStr(TimeFormat);

  if UseAsHint then begin
    { Usage as hint }
    if isOverlayed then begin
      grp := Datastore.Resource.Group;
      if (odResource in grp.ShowDetails) then begin
        res := Datastore.Resources.GetResource(AEvent.ResourceID);
        Result := RSOverlayed + ': ' + res.Description;
      end else
        Result := RSOverlayed;
    end else
      showDetails := true;

    timeStr := IfThen(AEvent.AllDayEvent,
      RSAllDay,
      FormatDateTime(timeFmt, AEvent.StartTime) + ' - ' + FormatDateTime(timeFmt, AEvent.EndTime)
    );

    Result := IfThen(Result = '',
      timeStr,
      Result + LineEnding + timeStr
    );

    if showDetails then begin
      // Event description
      Result := Result + LineEnding + LineEnding +
        RSEvent + ':' + LineEnding + AEvent.Description;

      // Event notes
      if (AEvent.Notes <> '') then begin
        s := WrapText(AEvent.Notes, MAX_HINT_WIDTH);
        s := StripLastLineEnding(s);
        Result := Result + LineEnding + LineEnding +
          RSNotes + ':' + LineEnding + s;
      end;

      // Event location
      if (AEvent.Location <> '') then
        Result := Result + LineEnding + LineEnding +
          RSLocation + ':' + LineEnding + AEvent.Location;
    end;
  end
  else
  begin
    { Usage as cell text }
    timeStr := IfThen(ShowEventTime, Format('%s - %s: ', [
      FormatDateTime(timeFmt, AStartTime),
      FormatDateTime(timeFmt, AEndTime)
    ]));
    Result := timeStr;

    if isOverlayed then
    begin
      if (grp <> nil) and (odResource in grp.ShowDetails) then
      begin
        res := Datastore.Resources.GetResource(AEvent.ResourceID);
        if res <> nil then
          Result := Result + '[' + res.Description + '] ';
      end else
        Result := Result + '[' + RSOverlayedEvent + '] ';
    end else
      showDetails := True;

    if showDetails then
      Result := Result + AEvent.Description;
  end;
end;


procedure TVpWeekView.LoadLanguage;
begin
  FDefaultPopup.Items.Clear;
  InitializeDefaultPopup;
end;

{=====}

procedure TVpWeekView.Invalidate;
begin
  inherited;
end;

function TVpWeekView.IsHoliday(ADate: TDate; out AHolidayName: String): Boolean;
begin
  AHolidayName := '';
  if Assigned(FOnHoliday) then
    FOnHoliday(Self, ADate, AHolidayName);
  Result := AHolidayName <> '';
end;

procedure TVpWeekView.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  wvInLinkHandler := true;
  try
    case NotificationType of
      neDateChange      : Date := Value;
      neDataStoreChange : Invalidate;
      neInvalidate      : Invalidate;
    end;
  finally
    wvInLinkHandler := false;
  end;
end;

procedure TVpWeekView.wvHookUp;
var
  I: Integer;
begin
  { If the component is being dropped on a form at designtime, then }
  { automatically hook up to the first datastore component found    }
  if csDesigning in ComponentState then
    for I := 0 to pred(Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpCustomDataStore) then begin
        DataStore := TVpCustomDataStore(Owner.Components[I]);
        Exit;
      end;
    end;
end;
{=====}

procedure TVpWeekView.Loaded;
begin
  inherited;
  wvLoaded := true;
  wvPopulate;
end;
{=====}

function TVpWeekView.GetControlType : TVpItemType;
begin
  Result := itWeekView;
end;

procedure TVpWeekView.Paint;
begin
  RenderToCanvas(
    Canvas,                      // Paint Canvas
    Rect (0, 0, Width, Height),  // Paint Rectangle
    ra0,
    1,                           // Scale
    wvStartDate,                 // Date
    -1,                          // Start At
    -1,                          // End At
    gr30Min,
    False                        // Display Only
  );
end;
{=====}

procedure TVpWeekView.PaintToCanvas(ACanvas: TCanvas; ARect: TRect;
  Angle: TVpRotationAngle; ADate: TDateTime);
begin
  RenderToCanvas(ACanvas, ARect, Angle, 1, ADate, -1, -1, gr30Min, True);
end;
{=====}

procedure TVpWeekView.RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
  Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
  StartLine: Integer; StopLine: Integer; UseGran: TVpGranularity;
  DisplayOnly: Boolean);
var
  painter: TVpWeekViewPainter;
begin
  wvPainting := true;
  painter := TVpWeekViewPainter.Create(self, RenderCanvas);
  try
    painter.RenderToCanvas(RenderIn, Angle, Scale, RenderDate, Startline, StopLine, UseGran, DisplayOnly);
  finally
    painter.Free;
    wvPainting := false;
  end;
end;
{=====}

procedure TVpWeekView.wvPopulate;
begin
  if DataStore <> nil then
    DataStore.Date := FActiveDate;
end;
{=====}

procedure TVpWeekView.DeleteActiveEvent(Verify: Boolean);
var
  DoIt: Boolean;
begin
  if ReadOnly then
    exit;
  if (FActiveEvent <> nil) and (not FActiveEvent.CanEdit) then
    exit;

  wvClickTimer.Enabled := false;
  EndEdit(nil);

  DoIt := not Verify;

  if ActiveEvent <> nil then begin
    if Verify then
      DoIt := (MessageDlg(RSConfirmDeleteEvent + #13#10#10 + RSPermanent,
        mtConfirmation, [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      ActiveEvent.Deleted := true;
      ActiveEvent := nil;
      DataStore.PostEvents;
      Invalidate;
    end;
  end;
end;
{=====}

procedure TVpWeekView.wvSpinButtonClick(Sender: TObject; Button: TUDBtnType);
begin
  if Button = btNext then
    Date := Date + 7
  else
    Date := Date - 7;
end;
{=====}

procedure TVpWeekView.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetActiveEvent(AValue: TVpEvent);
begin
  if FActiveEvent = AValue then Exit;
  FActiveEvent := AValue;
end;

procedure TVpWeekView.SetDrawingStyle(Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpWeekView.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then begin
    FLineColor := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpWeekView.SetDateLabelFormat(Value: string);
begin
  if Value <> FDateLabelFormat then begin
    FDateLabelFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetEventFont(Value: TVpFont);
begin
  FEventFont.Assign(Value);
  Invalidate;
end;
{=====}

procedure TVpWeekView.SetLayout(AValue: TVpWeekviewLayout);
begin
  if AValue <> FLayout then begin
    FLayout := AValue;
    Invalidate;
  end;
end;

procedure TVpWeekView.SetShowEventTime(Value: Boolean);
begin
  if Value <> FShowEventTIme then begin
    FShowEventTime := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetTimeFormat(Value: TVpTimeFormat);
begin
  if Value <> FTimeFormat then begin
    FTimeFormat := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpWeekView.SetActiveDate(Value: TDateTime);
begin
  if FActiveDate <> Value then begin
    FActiveDate := Value;

    if (Value < wvStartDate) or (Value >= wvStartDate + 7) then
      wvStartDate := Trunc(GetStartOfWeek(Value, FWeekStartsOn));

    if wvStartDate > Value then
      wvStartDate := wvStartDate - 7;

    if wvLoaded then
      wvPopulate;

    Invalidate;

    if (not wvInLinkHandler) and (ControlLink <> nil) then
      ControlLink.Notify(self, neDateChange, FActiveDate);
  end;
end;
{=====}

procedure TVpWeekView.SetWeekStartsOn(Value: TVpDayType);
begin
  if FWeekStartsOn <> Value then begin
    FWeekStartsOn := Value;
    Invalidate;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpWeekView.WMSize(var Msg: TWMSize);
{$ELSE}
procedure TVpWeekView.WMSize(var Msg: TLMSize);
{$ENDIF}
begin
  inherited;
  { force a repaint on resize }
  Invalidate;
end;
{=====}

procedure TVpWeekView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
{$IFDEF DELPHI}
    WindowClass.style := CS_DBLCLKS;
{$ENDIF}
  end;
end;
{=====}

procedure TVpWeekView.CreateWnd;
begin
  inherited;
  wvSpinButtons.Parent := self;
end;
{=====}

procedure TVpWeekView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  Unused(Target, X, Y);

  if ReadOnly or (not FAllowDragAndDrop) then
    Exit;
 {$IFNDEF LCL}
  TVpEventDragObject(Target).Free;
 {$ENDIF}
 // not needed for LCL: we use DragObjectEx !!
end;

procedure TVpWeekView.DoStartDrag(var DragObject: TDragObject);
{$IFDEF LCL}
var
  P, HotSpot: TPoint;
  EventName: string;
{$ENDIF}
begin
  if ReadOnly or not FAllowDragAndDrop then
    Exit;

  if FActiveEvent <> nil then begin
    {$IFDEF LCL}
    GetCursorPos(P{%H-});
    P := TVpWeekView(Self).ScreenToClient(P);
    EventName := FActiveEvent.Description;
    HotSpot := Point(P.X - Self.wvActiveEventRec.Left, P.Y - Self.wvActiveEventRec.Top);
    DragObject := TVpEventDragObject.CreateWithDragImages(Self as TControl,
      HotSpot, Self.wvActiveEventRec, EventName, FDragDropTransparent);
   {$ELSE}
    DragObject := DragObject := TVpEventDragObject.Create(Self);
   {$ENDIF}
    TVpEventDragObject(DragObject).Event := FActiveEvent;
  end
  else
   {$IFDEF LCL}
    CancelDrag;
   {$ELSE}
    DragObject.Free;//EndDrag(false);
   {$ENDIF}
end;

procedure TVpWeekView.DragDrop(Source: TObject; X, Y: Integer);
var
  Event: TVpEvent;
  i: Integer;
  P: TPoint;
  newDate, dateDiff: TDate;
begin
  if ReadOnly or (not FAllowDragAndDrop) then
    Exit;

  P := Point(X, Y);
  newDate := -1;
  for i := 0 to pred(Length(wvWeekdayArray)) do
    if PointInRect(P, wvWeekdayArray[i].Rec) then begin
      newDate := wvWeekdayArray[i].Day;
      break;
    end;
  if newDate = -1 then
    exit;

  Event := TVpEventDragObject(Source).Event;
  if Event <> nil then begin
    dateDiff := trunc(newDate) - trunc(Event.StartTime);
    Event.StartTime := newDate + frac(Event.StartTime);
    Event.EndTime := Event.EndTime + dateDiff;
    DataStore.PostEvents;
    Repaint;
  end;
end;

procedure TVpWeekView.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Unused(Source, X, State);

  Accept := false;
  if ReadOnly or (not FAllowDragAndDrop) then
    Exit;

  if (Y > wvHeaderHeight) then
    Accept := true;
end;

{$IFNDEF LCL}
procedure TVpWeekView.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
{$ELSE}
procedure TVpWeekView.WMLButtonDblClk(var Msg: TLMLButtonDblClk);
{$ENDIF}
var
  StartTime, EndTime: TDateTime;
begin
  inherited;
  wvClickTimer.Enabled := false;
  wvMouseDownPoint := Point(0, 0);
  wvMouseDown := false;
  wvDragging := false;

  if not CheckCreateResource then
    Exit;

  if DataStore = nil then
    Exit;

  wvSetDateByCoord(Point(Msg.XPos, Msg.YPos));
  EventAtCoord(Point (Msg.XPos, Msg.YPos));

  // if the mouse was pressed down in the client area, then select the cell.
  if not focused then
    SetFocus;

  if (Msg.YPos > wvHeaderHeight) then
  begin
    { The mouse click landed inside the client area }
    { If we have hit an active event then we must want to edit it }
    if ActiveEvent <> nil then begin
      { edit this event }
      wvSpawnEventEditDialog(False);
    end
    else
    if (DataStore.Resource <> nil) then begin
      { otherwise, we must want to create a new event }
      StartTime := trunc(Date) + 0.5; { default to 12:00 noon }
      EndTime := StartTime + 30 / MinutesInDay; { StartTime + 30 minutes }
      ActiveEvent := DataStore.Resource.Schedule.AddEvent(
        DataStore.GetNextID('Events'),
        StartTime,
        EndTime
      );
      { edit this new event }
      wvSpawnEventEditDialog(True);  // true = new event
    end;
  end;
end;

{ Hints }

procedure TVpWeekView.ShowHintWindow(APoint: TPoint; AEvent: TVpEvent);
var
  txt: String;
begin
  HideHintWindow;
  case FHintMode of
    hmPlannerHint:
      begin
        if (AEvent = nil) or (Datastore = nil) or (Datastore.Resource = nil) then
          exit;
        txt := BuildEventString(AEvent, AEvent.StartTime, AEvent.EndTime, true);
      end;
    hmComponentHint:
      txt := FComponentHint;
  end;
  if (txt <> '') and not ((wvInplaceEditor <> nil) and wvInplaceEditor.Visible)
     and not (csDesigning in ComponentState) then
  begin
    Hint := txt;
    Application.Hint := txt;
    Application.ActivateHint(ClientToScreen(APoint), true);
  end;
end;

procedure TVpWeekView.HideHintWindow;
begin
  Application.CancelHint;
end;

procedure TVpWeekView.SetHint(const AValue: TTranslateString);
begin
  inherited;
  if FHintMode = hmComponentHint then
    FComponentHint := AValue;
end;


{ Popup menu }

procedure TVpWeekView.InitializeDefaultPopup;
var
  NewItem: TMenuItem;
  NewSubItem: TMenuItem;
  canEdit: Boolean;
begin
  canEdit := (FActiveEvent <> nil) and FActiveEvent.CanEdit;
  FDefaultPopup.Items.Clear;

  if RSPopupAddEvent <> '' then begin               // Add
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSPopupAddEvent;
    NewItem.OnClick := PopupAddEvent;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSPopupAddEventFromICal <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSPopupAddEventFromICal;    // Import from iCal
    NewItem.OnClick := PopupAddFromICalFile;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSPopupEditEvent <> '' then begin            // Edit
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSPopupEditEvent;
    NewItem.Enabled := canEdit;
    NewItem.OnClick := PopupEditEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSPopupDeleteEvent <> '' then begin            // Delete
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSPopupDeleteEvent;
    NewItem.Enabled := canEdit;
    NewItem.OnClick := PopupDeleteEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;

  NewItem := TMenuItem.Create(Self);               // ---
  NewItem.Caption := '-';
  FDefaultPopup.Items.Add(NewItem);

  if RSPopupChangeDate <> '' then begin           // Change date
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSPopupChangeDate;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);

    if RSToday <> '' then begin                  // Today
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSToday;
      NewSubItem.OnClick := PopupToday;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    NewSubItem := TMenuItem.Create(Self);      // -------
    NewSubItem.Caption := '-';
    NewItem.Add(NewSubItem);

    if RSNextWeek <> '' then begin             // Next week
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSNextWeek;
      NewSubItem.OnClick := PopupNextWeek;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSPrevWeek <> '' then begin           // Previous week
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSPrevWeek;
      NewSubItem.OnClick := PopupPrevWeek;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    NewSubItem := TMenuItem.Create(Self);     // -----
    NewSubItem.Caption := '-';
    NewItem.Add(NewSubItem);

    if RSNextMonth <> '' then begin          // Next month
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSNextMonth;
      NewSubItem.OnClick := PopupNextMonth;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSPrevMonth <> '' then begin           // Previous month
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSPrevMonth;
      NewSubItem.OnClick := PopupPrevMonth;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    NewSubItem := TMenuItem.Create(Self);       // -------
    NewSubItem.Caption := '-';
    NewItem.Add(NewSubItem);

    if RSNextYear <> '' then begin             // Next year
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSNextYear;
      NewSubItem.OnClick := PopupNextYear;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;

    if RSPrevYear <> '' then begin             // previous year
      NewSubItem := TMenuItem.Create(Self);
      NewSubItem.Caption := RSPrevYear;
      NewSubItem.OnClick := PopupPrevYear;
      NewSubItem.Tag := 0;
      NewItem.Add(NewSubItem);
    end;
  end;

  if (Datastore <> nil) and (Datastore.Resource <> nil) then
    AddResourceGroupMenu(FDefaultPopup.Items, Datastore.Resource, PopupPickResourceGroupEvent);
end;
{=====}

procedure TVpWeekView.PopupAddEvent(Sender: TObject);
var
  StartTime: TDateTime;
  EndTime: TDateTime;
begin
  if ReadOnly or (not CheckCreateResource) or
    (not Assigned(DataStore) ) or (not Assigned(DataStore.Resource))
  then
    Exit;

  StartTime := trunc(Date) + 1 / 2; { default to 12:00 noon }
  EndTime := StartTime + (30 / MinutesInDay); { StartTime + 30 minutes }
  ActiveEvent := DataStore.Resource.Schedule.AddEvent(
    DataStore.GetNextID('Events'),
    StartTime,
    EndTime
  );

  { edit this new event }
  wvSpawnEventEditDialog(True);
end;
{=====}

procedure TVpWeekView.PopupAddFromICalFile(Sender: TObject);
var
  dlg: TOpenDialog;
  ical: TVpICalendar;
  fn: String;
  i: Integer;
  id: Integer;
  startTime, endTime: TDateTime;
begin
  if ReadOnly or (not CheckCreateResource) or
     (not Assigned(DataStore)) or (not Assigned(DataStore.Resource))
  then
    Exit;

  dlg := TOpenDialog.Create(nil);
  try
    dlg.Title := RSLoadICalTitle;
    dlg.Filter := RSICalFilter;
    dlg.FileName := '';
    dlg.Options := dlg.Options + [ofAllowMultiSelect, ofFileMustExist];
    if dlg.Execute then begin
      Screen.Cursor := crHourGlass;
      Application.ProcessMessages;
      ical := TVpICalendar.Create;
      try
        for fn in dlg.Files do begin
          ical.LoadFromFile(fn);
          for i := 0 to ical.Count-1 do begin
            if not (ical[i] is TVpICalEvent) then
              Continue;
            startTime := TVpICalEvent(ical[i]).StartTime[false];  // use local times
            endTime := TVpICalEvent(ical[i]).EndTime[false];
            if (startTime = 0) and (endTime = 0) then
              continue;
            id := DataStore.GetNextID(EventsTableName);
            FActiveEvent := Datastore.Resource.Schedule.AddEvent(id, starttime, endtime);
            FActiveEvent.Changed := true;
            FActiveEvent.LoadFromICalendar(TVpICalEvent(ical[i]));
            Datastore.PostEvents;
            Datastore.NotifyDependents;
          end;
        end;
        Invalidate;
      finally
        ical.Free;
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TVpWeekView.PopupDeleteEvent(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  if ActiveEvent <> nil then
    DeleteActiveEvent (True);
end;
{=====}

procedure TVpWeekView.PopupEditEvent(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  if ActiveEvent <> nil then
    { edit this Event }
    wvSpawnEventEditDialog(False);
end;
{=====}

procedure TVpWeekView.EditSelectedEvent(IsNewEvent: Boolean = false);
begin
  if ActiveEvent <> nil then
    wvSpawnEventEditDialog(IsNewEvent);
end;
{=====}

procedure TVpWeekView.PopupToday(Sender: TObject);
begin
  Date := Now;
end;
{=====}

procedure TVpWeekView.PopupNextWeek(Sender: TObject);
begin
  Date := Date + 7;
end;
{=====}

procedure TVpWeekView.PopupPrevWeek(Sender: TObject);
begin
  Date := Date - 7;
end;
{=====}

procedure TVpWeekView.PopupNextMonth(Sender: TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  if M = 12 then begin
    M := 1;
    Y := Y + 1;
  end else
    M := M + 1;
  if (D > DaysInAMonth(Y, M)) then
    D := DaysInAMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpWeekView.PopupPrevMonth(Sender : TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  if M = 1 then begin
    M := 12;
    Y := Y - 1;
  end else
    M := M - 1;
  if (D > DaysInAMonth(Y, M)) then
    D := DaysInAMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;
{=====}

procedure TVpWeekView.PopupNextYear(Sender: TObject);
var
  M, D, Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  Date := EncodeDate(Y + 1, M, 1);
end;
{=====}

procedure TVpWeekView.PopupPrevYear(Sender: TObject);
var
  M, D, Y : Word;
begin
  DecodeDate(Date, Y, M, D);
  Date := EncodeDate(Y - 1, M, 1);
end;

procedure TVpWeekView.PopupPickResourceGroupEvent(Sender: TObject);
begin
  Datastore.Resource.Group := TVpResourceGroup(TMenuItem(Sender).Tag);
  Datastore.UpdateGroupEvents;
end;

procedure TVpWeekView.PopupDropDownEvent(Sender: TObject);
begin
  InitializeDefaultPopup;
end;

procedure TVpWeekView.wvSpawnEventEditDialog(IsNewEvent: Boolean);
var
  AllowIt: Boolean;
  EventDlg : TVpEventEditDialog;
begin
  if DataStore = nil then Exit;

  if (not IsNewEvent) and (not ActiveEvent.CanEdit) then begin
    MessageDlg(RSCannotEditOverlayedEvent, mtInformation, [mbOk], 0);
    exit;
  end;

  AllowIt := false;
  if Assigned(FOwnerEditEvent) then
    FOwnerEditEvent(self, ActiveEvent, IsNewEvent, DataStore.Resource, AllowIt)
  else begin
    EventDlg := TVpEventEditDialog.Create(nil);
    try
      EventDlg.DataStore := DataStore;
      EventDlg.TimeFormat := FTimeFormat;
      AllowIt := EventDlg.Execute(ActiveEvent);
    finally
      EventDlg.Free;
    end;
  end;

  if AllowIt then begin
    ActiveEvent.Changed := true;
    DataStore.PostEvents;
    if IsNewEvent and Assigned(FOnAddEvent) then
      FOnAddEvent(self, ActiveEvent);
  end else begin
    if IsNewEvent then begin
      DataStore.Resource.Schedule.DeleteEvent(ActiveEvent);
      ActiveEvent := nil;
    end;
    DataStore.PostEvents;
  end;
  Invalidate;
end;
{=====}

{$IFNDEF LCL}
procedure TVpWeekView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;
{$ENDIF}
{=====}

procedure TVpWeekView.wvSetDateByCoord(Point: TPoint);
var
  I: Integer;
begin
  for I := 0 to pred(Length(wvWeekdayArray)) do
    if PointInRect(Point, wvWeekdayArray[I].Rec) then
    begin
      Date := wvWeekdayArray[I].Day;
      Invalidate;
      Exit;
    end;
end;
{=====}

function TVpWeekView.EventAtCoord(Pt: TPoint): Boolean;
var
  I: Integer;
begin
  result := false;
  for I := 0 to pred(Length(wvEventArray)) do begin
    // We've hit the end of visible events without finding a match
    if wvEventArray[I].Event = nil then
      Break;

    // Point falls inside this event's rectangle
    if PointInRect(Pt, wvEventArray[I].Rec) then
    begin
      wvHotPoint := Pt;
      ActiveEvent := TVpEvent(wvEventArray[I].Event);
      wvActiveEventRec := wvEventArray[I].Rec;
      result := true;
      Exit;
    end;
  end;

  // Not found
  ActiveEvent := nil;
  wvActiveEventRec.Top := 0;
  wvActiveEventRec.Bottom := 0;
  wvActiveEventRec.Right := 0;
  wvActiveEventRec.Left := 0;
end;

function TVpWeekView.GetEventAtCoord(Pt: TPoint): TVpEvent;
var
  i: Integer;
begin
  for i:=0 to High(wvEventArray) do begin
    // We've hit the end of visible events without finding a match
    if wvEventArray[i].Event = nil then
      Break;

    // Point falls inside this event's rectangle
    if PointInRect(Pt, wvEventArray[i].Rec) then
    begin
      Result := wvEventArray[i].Event;
      Exit;
    end;
  end;
  Result := nil;
end;

function TVpWeekView.GetEventRect(AEvent: TVpEvent): TRect;
var
  i: Integer;
begin
  for i:=0 to High(wvEventArray) do
    if wvEventArray[i].Event = AEvent then begin
      Result := wvEventArray[i].Rec;
      exit;
    end;
end;

{=====}

{ This is the timer event which spawns an in-place editor.
  If the event is double-clicked before this timer fires, then the event is
  edited in a dialog based editor. }
procedure TVpWeekView.wvEditInPlace(Sender: TObject);
begin
  wvClickTimer.Enabled := false;
  EditEvent;
end;
{=====}

procedure TVpWeekView.EditEvent;
var
  AllowIt: Boolean;
begin
  if ActiveEvent <> nil then begin
    if (not FAllowInplaceEdit) or (not ActiveEvent.CanEdit) then
      exit;

    AllowIt := true;
    { call the user defined BeforeEdit event }
    if Assigned(FBeforeEdit) then
      FBeforeEdit(Self, ActiveEvent, AllowIt);

    if AllowIt then begin
      { create and spawn the in-place editor }
      if wvInplaceEditor = nil then begin
        wvInPlaceEditor := TVpWvInPlaceEdit.Create(Self);
        wvInPlaceEditor.Parent := self;
        wvInPlaceEditor.OnExit := EndEdit;
      end;
      if ActiveEvent.AllDayEvent then
        wvInPlaceEditor.SetBounds(
          wvActiveEventRec.Left + TextMargin,
          wvActiveEventRec.Top,
          wvActiveEventRec.Right - TextMargin * 3,
          wvActiveEventRec.Bottom - TextMargin * 2
        )
      else
        wvInPlaceEditor.SetBounds(
          wvActiveEventRec.Left + TextMargin,
          wvActiveEventRec.Top,
          wvActiveEventRec.Right - TextMargin * 2,
          wvActiveEventRec.Bottom - TextMargin * 2
        );
      wvInplaceEditor.Show;
      wvInPlaceEditor.Text := ActiveEvent.Description;
      Invalidate;
      wvInPlaceEditor.SetFocus;
    end;
  end;
end;
{=====}

procedure TVpWeekView.KeyDown(var Key: Word; Shift: TShiftState);
var
  PopupPoint : TPoint;
begin
  case Key of
    VK_DELETE : DeleteActiveEvent(true);
    VK_RIGHT  : if Shift = [ssShift] then
                  PopupNextWeek (Self)
                else if (Shift = [ssCtrl]) then
                  PopupNextMonth (Self)
                else if (Shift = [ssShift, ssCtrl]) then
                  PopupNextYear (Self)
                else if Shift = [] then begin
                  case DayOfWeek (FActiveDate) of
                    1 : FActiveDate := FActiveDate - 4;
                    2 : FActiveDate := FActiveDate + 3;
                    3 : FActiveDate := FActiveDate + 3;
                    4 : FActiveDate := FActiveDate + 3;
                    5 : FActiveDate := FActiveDate - 3;
                    6 : FActiveDate := FActiveDate - 3;
                    7 : FActiveDate := FActiveDate - 3;
                  end;
                  Invalidate;
                end;
    VK_LEFT   : if Shift = [ssShift] then
                  PopupPrevWeek (Self)
                else if (Shift = [ssCtrl]) then
                  PopupPrevMonth (Self)
                else if (Shift = [ssShift, ssCtrl]) then
                  PopupPrevYear (Self)
                else if Shift = [] then begin
                  case DayOfWeek (FActiveDate) of
                    1 : FActiveDate := FActiveDate - 4;
                    2 : FActiveDate := FActiveDate + 3;
                    3 : FActiveDate := FActiveDate + 3;
                    4 : FActiveDate := FActiveDate + 3;
                    5 : FActiveDate := FActiveDate - 3;
                    6 : FActiveDate := FActiveDate - 3;
                    7 : FActiveDate := FActiveDate - 3;
                  end;
                  Invalidate;
                end;
    VK_UP     : begin
                  if Shift = [] then
                    case DayOfWeek (FActiveDate) of
                      1 : FActiveDate := FActiveDate - 1;
                      2 : FActiveDate := FActiveDate + 2;
                      3 : FActiveDate := FActiveDate - 1;
                      4 : FActiveDate := FActiveDate - 1;
                      5 : FActiveDate := FActiveDate + 3;
                      6 : FActiveDate := FActiveDate - 1;
                      7 : FActiveDate := FActiveDate - 1;
                    end;
                  Invalidate;
                end;
    VK_DOWN   : begin
                  if Shift = [] then
                    case DayOfWeek (FActiveDate) of
                      1 : FActiveDate := FActiveDate - 3;
                      2 : FActiveDate := FActiveDate + 1;
                      3 : FActiveDate := FActiveDate + 1;
                      4 : FActiveDate := FActiveDate - 2;
                      5 : FActiveDate := FActiveDate + 1;
                      6 : FActiveDate := FActiveDate + 1;
                      7 : FActiveDate := FActiveDate + 1;
                    end;
                  Invalidate;
                end;
    VK_INSERT : PopupAddEvent(Self);
{$IFNDEF LCL}
    VK_TAB    :
      if ssShift in Shift then
        Windows.SetFocus(GetNextDlgTabItem(GetParent(Handle), Handle, False))
      else
        Windows.SetFocus(GetNextDlgTabItem(GetParent(Handle), Handle, True));
{$ENDIF}
    VK_F10:
      if (ssShift in Shift) and not Assigned(PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
    VK_APPS:
      if not Assigned (PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
  end;
end;
{=====}

procedure TVpWeekView.EndEdit(Sender: TObject);
begin
  if (wvInPlaceEditor <> nil) and wvInplaceEditor.Visible and (ActiveEvent <> nil)
  then begin
    if wvInPlaceEditor.Text <> ActiveEvent.Description then begin
      ActiveEvent.Description := wvInPlaceEditor.Text;
      ActiveEvent.Changed := true;
      if Assigned(FAfterEdit) then
        FAfterEdit(self, ActiveEvent);
      DataStore.PostEvents;
    end;
    wvInplaceEditor.Hide;
    Invalidate;
//    SetFocus;
  end;
end;

procedure TVpWeekView.MouseEnter;
begin
  FMouseEvent := nil;
end;

procedure TVpWeekView.MouseLeave;
begin
  HideHintWindow;
end;

procedure TVpWeekView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
var
  oldDate: TDate;
  i: Integer;
begin
  inherited;

  if not Focused then SetFocus;

  { Left button }
  if Button = mbLeft then
  begin
    if (wvInPlaceEditor <> nil) and wvInPlaceEditor.Visible then
      EndEdit(Self);

    wvMouseDown := true;
    wvMouseDownPoint := Point(X, Y);

    if (Y > wvHeaderHeight) then
    begin
      { The mouse click landed inside the client area }
      oldDate := FActiveDate;
      wvSetDateByCoord(wvMouseDownPoint);

      { We must repaint the control here, before evaluation of the click on the
        events, because if the day has changed by wvSetDateByCoord then events
        will have different indexes in the event array; and index positions are
        evaluated during painting. }
      if oldDate <> FActiveDate then
        Paint;

      { If an active event was clicked, then enable the click timer.  If the
        item is double clicked before the click timer fires, then the edit
        dialog will appear, otherwise the in-place editor will appear. }
      if EventAtCoord(wvMouseDownPoint) then
        wvClickTimer.Enabled := true;
    end;
  end;

  { Right button }
  if Button = mbRight then
  begin
    if not Assigned(PopupMenu) then
      exit;

    { The mouse click landed inside the client area }
    wvSetDateByCoord(Point(X, Y));
    EventAtCoord(Point(X, Y));
    wvClickTimer.Enabled := false;

    if not Assigned(ActiveEvent) then begin
      for i := 0 to FDefaultPopup.Items.Count - 1 do
        if (FDefaultPopup.Items[i].Tag = 1) or (ReadOnly) then
          FDefaultPopup.Items[i].Enabled := False;
    end else begin
      for i := 0 to FDefaultPopup.Items.Count - 1 do
        FDefaultPopup.Items[i].Enabled := True;
    end;
  end;
end;

procedure TVpWeekView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  event: TVpEvent;
begin
  inherited MouseMove(Shift, X, Y);
  if (FActiveEvent <> nil) and (not ReadOnly) then begin
    if (not wvDragging) and wvMouseDown and
       ((wvMouseDownPoint.x <> x) or (wvMouseDownPoint.y <> y)) and
       FActiveEvent.CanEdit
    then begin
      wvDragging := true;
      wvClickTimer.Enabled := false;
      BeginDrag(true);
    end;
  end;

  if ShowHint then
  begin
    event := GetEventAtCoord(Point(X, Y));
    if event = nil then
      HideHintWindow
    else if FMouseEvent <> event then begin
      HideHintWindow;
      ShowHintWindow(Point(X, Y), event);
      FMouseEvent := event;
    end;
  end;
end;

procedure TVpWeekView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then begin
    wvMouseDownPoint := Point(0, 0);
    wvMouseDown := false;
    wvDragging := false;
  end;
end;

{$IF VP_LCL_SCALING = 2}
procedure TVpWeekView.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
  inherited;
  DoScaleFontPPI(AllDayEventAttributes.Font, AToPPI, AProportion);
  DoScaleFontPPI(DayHeadAttributes.Font, AToPPI, AProportion);
  DoScaleFontPPI(EventFont, AToPPI, AProportion);
  DoScaleFontPPI(HeadAttributes.Font, AToPPI, AProportion);
end;
{$ELSEIF VP_LCL_SCALING = 1}
procedure TVpWeekView.ScaleFontsPPI(const AProportion: Double);
begin
  inherited;
  DoScaleFontPPI(AllDayEventAttributes.Font, AProportion);
  DoScaleFontPPI(DayHeadAttributes.Font, AProportion);
  DoScaleFontPPI(EventFont, AProportion);
  DoScaleFontPPI(HeadAttributes.Font, AProportion);
end;
{$ENDIF}


{ TVpWvHeadAttributes }

constructor TVpWvHeadAttributes.Create(AOwner: TVpWeekView);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clBtnFace;
  FFont := TVpFont.Create(AOwner);
end;
{=====}

destructor TVpWvHeadAttributes.Destroy;
begin
  FFont.Free;
  inherited;
end;
{=====}

procedure TVpWvHeadAttributes.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate;
  end;
end;
{=====}

procedure TVpWvHeadAttributes.SetFont(Value: TVpFont);
begin
  FFont.Assign(Value);
end;
{=====}

end.
