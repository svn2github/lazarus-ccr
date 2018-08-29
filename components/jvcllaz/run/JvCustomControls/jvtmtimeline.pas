﻿{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTMTimeLine.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A component that mimicks the time line in MS Team Manager

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTMTimeLine;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, LMessages, LCLVersion, Types,
  SysUtils, Classes, Controls, Buttons, Graphics, ExtCtrls, Forms, ImgList,
  JvExControls;

const
  cTMTimeLineDayWidth = 19;
  cTMTimeLineButtonWidth = 16;
  cTMTimeLineIconDayDist = 4;

type
  TJvTLSelFrame = class(TPersistent)
  private
    FVisible: Boolean;
    FPen: TPen;
    FOnChange: TNotifyEvent;
    procedure SetPen(const Value: TPen);
    procedure SetVisible(const Value: Boolean);
    procedure PenChange(Sender: TObject);
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen: TPen read FPen write SetPen;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvBtnDown = (bdNone, bdLeft, bdRight);
  TJvObjectReadEvent = procedure(Sender: TObject; Stream: TStream; var AObject: TObject) of object;
  TJvObjectWriteEvent = procedure(Sender: TObject; Stream: TStream; const AObject: TObject) of object;

  TJvCustomTMTimeline = class(TCustomPanel) //(TJvCustomPanel)
  private
    FTimer: TTimer;
    FImages: TImageList;
    FChangeLink: TChangeLink;
    FDateImages: TStringlist;
    FObjects: TStringlist;
    FLeftBtn: TSpeedButton;
    FRightBtn: TSpeedButton;
    FMonthFont: TFont;
    FBtnDown: TJvBtnDown;
    FReadOnly: Boolean;
    FRightClickSelect: Boolean;
    FDayWidth: Integer;
    FButtonWidth: Integer;
    FIconDayDist: Integer;
    FDate: TDate;
    FSelDate: TDate;
    FMinDate: TDate;
    FMaxDate: TDate;
    FImageCursor: TCursor;
    FRealCursor: TCursor;
    FTodayColor: TColor;
    FSelection: TJvTLSelFrame;
    FOnChange: TNotifyEvent;
    FLargeChange: Word;
    FSmallChange: Word;
    FOnWriteObject: TJvObjectWriteEvent;
    FOnReadObject: TJvObjectReadEvent;
    FObjectsFontStyle: TFontStyles;
    FShowWeeks: Boolean;
    FShowMonths: Boolean;
    FShowToday: Boolean;
    FLineColor: TColor;
    FShift: TShiftState;
    FShowTodayIcon: Boolean;
    function ButtonWidthStored: Boolean;
    function DayWidthStored: Boolean;
    function IconDayDistStored: Boolean;
    function GetButtonWidth: Integer;
    function GetDayWidth: Integer;
    function GetIconDayDist: Integer;
    function GetRectForDate(ADate: TDate): TRect;
    function DateFromPos(APos: Integer): TDate;
    procedure DoTimer(Sender: TObject);
    procedure SetFirstDate(const Value: TDate);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetMonthFont(const Value: TFont);
    procedure SetSelDate(const Value: TDate);
    procedure SetDayWidth(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    procedure DoChange(Sender: TObject);
    function GetImageIndex(ADate: TDate): Integer;
    procedure SetImageIndex(ADate: TDate; const Value: Integer);
    procedure DrawDates(ACanvas: TCanvas);
    procedure DrawSelectionFrame(ACanvas: TCanvas; ARect: TRect);
    procedure DrawImage(ACanvas: TCanvas; ADate: TDate; const ARect: TRect);
    procedure DrawToday(ACanvas: TCanvas; const ARect: TRect);
    procedure SetImageCursor(const Value: TCursor);
    procedure SetSelection(const Value: TJvTLSelFrame);
    procedure DoLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure DoMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure DoRMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    // this is needed so we receive the arrow keys

    procedure DrawFrame(ACanvas: TCanvas; AColor: TColor;
      ALineWidth: Integer; ARect: TRect);
    procedure SetTodayColor(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetRightClickSelect(const Value: Boolean);
    procedure SetMaxDate(const Value: TDate);
    procedure SetMinDate(const Value: TDate);
    procedure SetLargeChange(const Value: Word);
    procedure SetSmallChange(const Value: Word);
    function GetObjects(ADate: TDate): TObject;
    procedure SetObjects(ADate: TDate; const Value: TObject);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetIconDayDist(const Value: Integer);
    procedure SetObjectsFontStyle(const Value: TFontStyles);
    procedure SetShowMonths(const Value: Boolean);
    procedure SetShowToday(const Value: Boolean);
    procedure SetShowWeeks(const Value: Boolean);
    function ReadMagic(Stream: TStream): Boolean;
    procedure StartTimer;
    procedure StopTimer;
    function DateHasImage(ADate: TDateTime): Boolean;
    procedure SetShowTodayIcon(const Value: Boolean);
  protected
//    procedure GetDlgCode(var Code: TDlgCodes); override;   <--- wp
//    procedure CursorChanged; override;   <--- wo
    procedure Change; virtual;
    {$IFDEF LCL_FullVersion >= 1080000}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    {$IFEND}
    function DoMouseWheelDown(Shift: TShiftState;  MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState;  MousePos: TPoint): Boolean; override;
    procedure EnabledChanged; override;
    function GetBorderStyle: TBorderStyle;
    function GetLastVisibleDate: TDate;
    function GetVisibleDays: Integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LoadObject(Stream: TStream; var AObject: TObject); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SaveObject(Stream: TStream; const AObject: TObject); virtual;
    procedure SetBorderStyle(Value: TBorderStyle); override;

    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth stored ButtonWidthStored;
    property Cursor;
    property DayWidth: Integer read GetDayWidth write SetDayWidth stored DayWidthStored;
    property ObjectsFontStyle: TFontStyles read FObjectsFontStyle write SetObjectsFontStyle default [fsUnderline];
    property IconDayDistance: Integer read GetIconDayDist write SetIconDayDist stored IconDayDistStored;
    property ImageCursor: TCursor read FImageCursor write SetImageCursor default crHandPoint;
    property Images: TImageList read FImages write SetImages;
    property LargeChange: Word read FLargeChange write SetLargeChange default 30;
    property Date: TDate read FDate write SetFirstDate;
    property SelDate: TDate read FSelDate write SetSelDate;
    property MaxDate: TDate read FMaxDate write SetMaxDate;
    property MinDate: TDate read FMinDate write SetMinDate;
    property MonthFont: TFont read FMonthFont write SetMonthFont;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property RightClickSelect: Boolean read FRightClickSelect write SetRightClickSelect;
    property SmallChange: Word read FSmallChange write SetSmallChange default 7;
    property Selection: TJvTLSelFrame read FSelection write SetSelection;
    property TodayColor: TColor read FTodayColor write SetTodayColor default clAqua;
    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    property ShowToday: Boolean read FShowToday write SetShowToday default True;
    property ShowTodayIcon: Boolean read FShowTodayIcon write SetShowTodayIcon default True;
    property ShowWeeks: Boolean read FShowWeeks write SetShowWeeks default True;
    property ShowMonths: Boolean read FShowMonths write SetShowMonths default True;
    property LastVisibleDate: TDate read GetLastVisibleDate;
    property VisibleDays: Integer read GetVisibleDays;
    property Height default 56;
    property Color default clWindow;
    property RightButton: TSpeedButton read FRightBtn;
    property LeftButton: TSpeedButton read FLeftBtn;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnReadObject: TJvObjectReadEvent read FOnReadObject write FOnReadObject;
    property OnWriteObject: TJvObjectWriteEvent read FOnWriteObject write FOnWriteObject;
    property Align default alTop;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // this procedure resets all imageindexes to -1
    procedure ClearImages;
    // this procedure frees all the objects in the Objects array
    procedure ClearObjects;
    // scrools the display Delta number of days. Delta can be either negative or positive
    procedure ScrollDate(Sender: TObject; Delta: Integer);
    // this procedure loads data from a stream and calls the OnReadObject event
    procedure LoadFromStream(Stream: TStream);
    // this procedure saves data to a stream and calls the OnWriteObject event
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    // gets / sets the imageindex for a specific date
    property ImageIndex[ADate: TDate]: Integer read GetImageIndex write SetImageIndex;
    // gets / sets the TObject for a specific date
    property Objects[ADate: TDate]: TObject read GetObjects write SetObjects;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvTMTimeline = class(TJvCustomTMTimeline)
  public
    property RightButton;
    property LeftButton;
  published
    // gets / sets the borderstyle of the control and the scroll-buttons
    property BorderStyle;
    // gets / sets the width of the buttons
    property ButtonWidth;
    // gets / sets the selected date
    property SelDate;
    // gets / sets the width of each day
    property DayWidth;
    // gets / sets the distance between days and associated icons
    property IconDayDistance;
    // gets / sets the cursor to use when a date has an image associated
    property ImageCursor;
    // sets / gets the imagelist associated with the control
    property Images;
    // sets the interval for large changes (ctrl+click or ctrl+arrows)
    property LargeChange;
    // gets / sets the first visible date on the left edge
    // OnChange is called when this date changes
    property Date;
    // sets the maximum date that users can scroll to
    property MaxDate;
    // sets the minimum date that users can scroll to
    property MinDate;
    // gets / sets the font used for the month display
    property MonthFont;
    // gets / sets the fontstyle for Objects that are non-nil
    property ObjectsFontStyle;
    property ReadOnly;
    // gets / sets whether a right-click changes the Date property and moves the selection frame
    property RightClickSelect;
    // sets the interval for small changes (left-click or arrows)
    property SmallChange;
    // gets / sets the properties for the selection frame
    property Selection;
    // displays todays date in a different color
    property ShowToday;
    // displays todays date with a double-diamond icon
    property ShowTodayIcon;
    // shows / hides the dotted week separator
    property ShowWeeks;
    // shows the month separator line
    property ShowMonths;
    // gets / sets the background color for the today item
    property TodayColor;
    // gets / sets color of lines (dotted and solid)
    property LineColor;
    // this returns the date of the last fully visible date in the control
    property LastVisibleDate;
    // this returns the number of fully visible days in the control
    property VisibleDays;
    property Action;
    property Align default alTop;
    property Anchors;
    property Constraints;
    property Cursor;
    property Enabled;
    property Font;
    property Height;
    property Hint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Color;
    // triggered when the display is scrolled or when the left-most date changes
    property OnChange;
    // triggered when the control is clicked
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // triggered for each object when reading from a file
    property OnReadObject;
    // triggered for each object when writing to a file
    property OnWriteObject;
    property OnStartDrag;
    property OnStartDock;
    property OnEndDock;
  end;


implementation

uses
  LCLStrConsts, Themes,
  JvConsts, JvJCLUtils, JvJVCLUtils, JvResources;

{$R ..\..\resource\jvtmtimeline.res}

const
  cMagic = 'Jv.TMTIMELINE1';

function HighDpi_Suffix: String;
begin
  Result := '';
  if Screen.SystemFont.PixelsPerInch >= 168 then
    Result := Result + '_200'
  else
  if Screen.SystemFont.PixelsPerInch >= 120 then
    Result := Result + '_150';
end;

//=== { TJvTLSelFrame } ======================================================

constructor TJvTLSelFrame.Create;
begin
  inherited Create;
  FPen := TPen.Create;
  FPen.OnChange := @PenChange;
  FVisible := True;
end;

destructor TJvTLSelFrame.Destroy;
begin
  FPen.Free;
  inherited Destroy;
end;

procedure TJvTLSelFrame.Assign(Source: TPersistent);
begin
  if Source is TJvTLSelFrame then
  begin
    Pen := TJvTLSelFrame(Source).Pen;
    Visible := TJvTLSelFrame(Source).Visible;
    PenChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TJvTLSelFrame.PenChange(Sender: TObject);
begin
  DoChange;
end;

procedure TJvTLSelFrame.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvTLSelFrame.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
  DoChange;
end;

procedure TJvTLSelFrame.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

//=== { TJvCustomTMTimeline } ================================================

constructor TJvCustomTMTimeline.Create(AOwner: TComponent);
var
  png: TPortableNetworkGraphic;
  resname: String;
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  // IncludeThemeStyle(Self, [csNeedsBorderPaint]);   <--- wp

  FSelection := TJvTLSelFrame.Create;
  FSelection.Pen.Width := 2;
  FSelection.OnChange := @DoChange;

  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := @DoChange;

  FDateImages := TStringlist.Create;
  FDateImages.Sorted := True;
  FObjects := TStringlist.Create;
  FObjects.Sorted := True;

  FMonthFont := TFont.Create;
  FMonthFont.Style := [fsItalic, fsBold];
  FMonthFont.Name := 'Times New Roman';
  FMonthFont.Size := 18;

  FObjectsFontStyle := [fsUnderline];
  FButtonWidth := -1;
  FDayWidth := -1;
  FIconDayDist := -1;
  FDate := SysUtils.Date - 7;
  FSelDate := FDate - 1;
  FImageCursor := crHandPoint;
  FSmallChange := 7;
  FLargeChange := 30;
  FTodayColor := clAqua;
  FLineColor := clBlack;
  FShowToday := True;
  FShowTodayIcon := True;
  FShowWeeks := True;
  FShowMonths := True;
  Font.Size := 0;
  Font.Name := 'default';

  FLeftBtn := TSpeedButton.Create(Self);
  with FLeftBtn do
  begin
    Align := alLeft;
    Width := ButtonWidth;
    Parent := Self;
    Transparent := False;
    Layout := blGlyphTop;
    png := TPortableNetworkGraphic.Create;
    try
      resName := 'jvcustomtmtimelinescrollleft' + HighDPI_Suffix;
      png.LoadFromResourceName(HInstance, resname);
      Glyph.Assign(png);
    finally
      png.Free;
    end;

    OnMouseDown := @DoLMouseDown;
    OnMouseUp := @DoMouseUp;
    //    OnClick := LeftClick;
  end;

  FRightBtn := TSpeedButton.Create(Self);
  with FRightBtn do
  begin
    Align := alRight;
    Width := ButtonWidth;
    Parent := Self;
    Transparent := False;
    Layout := blGlyphTop;
    png := TPortableNetworkGraphic.Create;
    try
      resname := 'jvcustomtmtimelinescrollright' + HighDPI_Suffix;
      png.LoadFromResourceName(HInstance, resname);
      Glyph.Assign(png);
    finally
      png.Free;
    end;

    OnMouseDown := @DoRMouseDown;
    OnMouseUp := @DoMouseUp;
  end;
  FLeftBtn.SetSubComponent(True);
  FRightBtn.SetSubComponent(True);
  Height := 64;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Color := clWindow;
  Align := alTop;
  BorderStyle := bsSingle;
end;

destructor TJvCustomTMTimeline.Destroy;
begin
  FChangeLink.Free;
  FMonthFont.Free;
  FSelection.Free;
  FDateImages.Free;
  FObjects.Free;
  inherited Destroy;
end;

procedure TJvCustomTMTimeline.StartTimer;
begin
  if not Assigned(FTimer) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.OnTimer := @DoTimer;
    FTimer.Interval := 400;
  end;
  FTimer.Enabled := True;
end;

procedure TJvCustomTMTimeline.StopTimer;
begin
  FTimer.Free;
  FTimer := nil;
end;

procedure TJvCustomTMTimeline.DoLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    Exit;
  if ssCtrl in Shift then
    ScrollDate(Sender, -LargeChange)
  else
    ScrollDate(Sender, -SmallChange);
  FBtnDown := bdLeft;
  FShift := Shift;
  StartTimer;
end;

procedure TJvCustomTMTimeline.DoRMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    Exit;
  if ssCtrl in Shift then
    ScrollDate(Sender, LargeChange)
  else
    ScrollDate(Sender, SmallChange);
  FShift := Shift;
  FBtnDown := bdRight;
  StartTimer;
end;

procedure TJvCustomTMTimeline.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FBtnDown := bdNone;
  StopTimer;
end;

procedure TJvCustomTMTimeline.DoTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  case FBtnDown of
    bdLeft:
      if ssCtrl in FShift then
        ScrollDate(Sender, -LargeChange)
      else
        ScrollDate(Sender, -SmallChange);
    bdRight:
      if ssCtrl in FShift then
        ScrollDate(Sender, LargeChange)
      else
        ScrollDate(Sender, SmallChange);
    bdNone:
      begin
        FTimer.Interval := 400;
        Exit;
      end;
  end;
  FTimer.Interval := 70;
  FTimer.Enabled := True;
end;

procedure TJvCustomTMTimeline.DoChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomTMTimeline.ScrollDate(Sender: TObject; Delta: Integer);
begin
  Delta := Trunc(Self.Date + Delta);
  if ((MinDate = 0) or (Delta > MinDate)) and
    ((MaxDate = 0) or (Delta < MaxDate)) then
    Self.Date := Delta;
end;

function TJvCustomTMTimeline.GetRectForDate(ADate: TDate): TRect;
begin
  // all rects are the same size...
  Result := Classes.Rect(0, 0, DayWidth + 1, ClientHeight + 1);
  // ...but we must move the entire rect to the correct date
  OffsetRect(Result, Trunc(ADate - Self.Date) * DayWidth, 0);
  // ...and finally compensate for the inital offset
  if ReadOnly then
    OffsetRect(Result, 1, 0) // no buttons showing
  else
    OffsetRect(Result, ButtonWidth, 0);
end;

function TJvCustomTMTimeline.DateFromPos(APos: Integer): TDate;
var
  Tmp: Integer;
begin
  if not ReadOnly then
    Tmp := APos - ButtonWidth
  else
    Tmp := APos - 1;
  Result := Self.Date + (Tmp div DayWidth);
end;

procedure TJvCustomTMTimeline.DrawToday(ACanvas: TCanvas; const ARect: TRect);
var
  Tmp: TColor;
  png: TPortableNetworkGraphic;
  R: TRect;
  x, y: Integer;
  resname: String;
begin
  Tmp := ACanvas.Brush.Color;
  try
    if ShowToday then begin
      ACanvas.Brush.Color := FTodayColor;
      ACanvas.FillRect(ARect);
    end;
    if ShowTodayIcon then begin
      png := TPortableNetworkGraphic.Create;
      try
        resname := 'jvcustomtmtimelinemilestonelarge' + HighDpi_Suffix;
        png.LoadFromResourceName(HInstance, resname);
        x := (ARect.Left + ARect.Right - png.Width) div 2;
        y := ARect.Top + CanvasMaxTextHeight(ACanvas) + IconDayDistance;
        R := Classes.Rect(x, y, x + png.Width, y + png.Height);
        ACanvas.Draw(R.Left, R.Top, png);
      finally
        png.Free;
      end;
    end;
  finally
    ACanvas.Brush.Color := Tmp;
  end;
end;

procedure TJvCustomTMTimeline.DrawDates(ACanvas: TCanvas);
const
  TOP_MARGIN = 2;
var
  I, FirstOffset: Integer;
  Y, M, D: Word;
  R: TRect;
  Size: TSize;
  S: string;
  FTmpStyle: TFontStyles;
  AContinue: Boolean;
  h: Integer;
begin
  AContinue := True;
  // DoBeforeDraw(ACanvas);
  if not AContinue then
    Exit;
  if not ReadOnly then
    FirstOffset := ButtonWidth
  else
    FirstOffset := 1;
  // first loop: draw dates, today and images
  FTmpStyle := Font.Style;
  h := Canvas.TextHeight('Tg');
  for I := 0 to Width div DayWidth do
  begin
    R := GetRectForDate(Self.Date + I);
    if Self.Date + I = SysUtils.Date then
      DrawToday(ACanvas, R);
    DecodeDate(Self.Date + I, Y, M, D);
    R := Classes.Rect(I * DayWidth, 0, I * DayWidth + DayWidth, h);
    OffsetRect(R, FirstOffset, TOP_MARGIN);
    S := Format('%.2d', [D]);
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    if Objects[Self.Date + I] <> nil then
      ACanvas.Font.Style := FObjectsFontStyle
    else
      ACanvas.Font.Style := FTmpStyle;

    DrawText(ACanvas.Handle, PChar(S), Length(S), R,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);
    DrawImage(ACanvas, Self.Date + I, GetRectForDate(Self.Date + I));
    // frame should be drawn on top of text and image
    if (Trunc(SelDate) = Trunc(Self.Date + I)) and not ReadOnly then
      DrawSelectionFrame(ACanvas, GetRectForDate(SelDate));

    ACanvas.Font := Font;
    if not Enabled then
      ACanvas.Font.Color := clGrayText;
  end;

  // second loop: draw months and years and separators
  if ShowWeeks or ShowMonths then
    for I := 0 to (Width div DayWidth) do
    begin
      R := GetRectForDate(Self.Date + I);
      DecodeDate(FDate + I, Y, M, D);
      if ShowWeeks and (DayOfWeek(Self.Date + I) = 1) then
        with ACanvas do
        begin
          // draw the dotted week separator between sunday and monday
          Brush.Color := Color;
          Pen.Width := 1;
          Pen.Style := psDot;
          Pen.Color := FLineColor;
          MoveTo(I * DayWidth + DayWidth + FirstOffset, 0);
          LineTo(I * DayWidth + DayWidth + FirstOffset, Height);
        end;

      ACanvas.Font := MonthFont;
      if not Enabled then
        ACanvas.Font.Color := clGrayText;
      if ShowMonths then
      begin
        if MonthDays[IsLeapYear(Y), M] = D then
        begin
          // draw text for end of this month:
          S := FormatSettings.ShortMonthNames[M];
          Size := ACanvas.TextExtent(S);
          R := Classes.Rect(I * DayWidth + DayWidth - Size.cx - 8,
            Height - Size.cy - 4, I * DayWidth + DayWidth, Height - 4);
          OffsetRect(R, FirstOffset, 0);
          SetBkMode(ACanvas.Handle, TRANSPARENT);
          DrawText(ACanvas.Handle, PChar(S), Length(S), R,
            DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);
        end
        else
        if D = 1 then
        begin
          // draw text for start of this month and the year:
          S := Format('%s %d', [FormatSettings.ShortMonthNames[M], Y]);
          Size := ACanvas.TextExtent(S);
          R := Classes.Rect(I * DayWidth + 4, Height - Size.cy - 4, I * DayWidth + Size.cx + 4, Height - 4);
          OffsetRect(R, FirstOffset, 0);
          SetBkMode(ACanvas.Handle, TRANSPARENT);
          DrawText(ACanvas.Handle, PChar(S), Length(S), R,
            DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);

          // Draw the separator
          with ACanvas do
          begin
            Pen.Width := 1;
            Pen.Style := psSolid;
            Pen.Color := FLineColor;
            MoveTo(I * DayWidth + FirstOffset, 0);
            LineTo(I * DayWidth + FirstOffset, Height);
          end;
        end;
      end;
    end;

  // finally, clean up the display
  if (ButtonWidth > 0) and not ReadOnly then
    with ACanvas do
    begin
      // draw a vertical line just to the right of the left scroll button and
      // just to the left of the right scroll button to
      // make them stand out a little bit more when buttons are flat:
      Pen.Width := 1;
      Pen.Color := clBlack;
      Pen.Style := psSolid;
      if BorderStyle = bsNone then
      begin
        MoveTo(FLeftBtn.Width, 0);
        LineTo(FLeftBtn.Width, Height);
      end;
      MoveTo(FRightBtn.Left - 1, 0);
      LineTo(FRightBtn.Left - 1, Height);
    end;
  // DoAfterDraw(ACanvas);
end;

procedure TJvCustomTMTimeline.DrawSelectionFrame(ACanvas: TCanvas; ARect: TRect);
begin
  if not FSelection.Visible then
    Exit;
  if (ARect.Right > 0) and (ARect.Left <= Width) then
  begin
    ARect.Bottom := ARect.Bottom - ACanvas.Pen.Width;
    with FSelection do
      DrawFrame(ACanvas, Pen.Color, Pen.Width, ARect);
  end;
end;

procedure TJvCustomTMTimeline.DrawImage(ACanvas: TCanvas; ADate: TDate; const ARect: TRect);
var
  I, X, Y: Integer;
begin
  if DateHasImage(ADate) then
  begin
    I := ImageIndex[ADate];
    X := ARect.Left + (DayWidth - Images.Width) div 2;
    Y := CanvasMaxTextHeight(ACanvas) + IconDayDistance;
    Images.Draw(ACanvas, X, Y, I);
  end;
end;

procedure TJvCustomTMTimeline.Paint;
begin
  if not Showing or (csLoading in ComponentState) then
    Exit;
  inherited Canvas.Font := Font;
  DrawDates(inherited Canvas);
end;

procedure TJvCustomTMTimeline.DrawFrame(ACanvas: TCanvas; AColor: TColor;
  ALineWidth: Integer; ARect: TRect);
var
  Tmp: TColor;
begin
  if ALineWidth = 0 then
    Exit;
  Tmp := ACanvas.Brush.Color;
  try
    ACanvas.Brush.Color := AColor;
    ACanvas.FrameRect(ARect);
    InflateRect(ARect, -Abs(ALineWidth) + 1, -Abs(ALineWidth) + 1);
    ACanvas.FrameRect(ARect);
    ACanvas.FloodFill(ARect.Left - 1, ARect.Top - 1, AColor, fsBorder);
  finally
    ACanvas.Brush.Color := Tmp;
  end;
end;

procedure TJvCustomTMTimeline.SetFirstDate(const Value: TDate);
begin
  if Trunc(FDate) <> Trunc(Value) then
  begin
    if (FMinDate > 0) and (Trunc(FMinDate) > Trunc(FDate)) then
      FDate := FMinDate
    else
    if (FMaxDate > 0) and (Trunc(FMaxDate) < Trunc(FDate)) then
      FDate := Trunc(FMaxDate)
    else
      FDate := Trunc(Value);
    Change;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    FLeftBtn.Visible := not FReadOnly;
    FRightBtn.Visible := not FReadOnly;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.SetMonthFont(const Value: TFont);
begin
  FMonthFont.Assign(Value);
  Invalidate;
end;

procedure TJvCustomTMTimeline.SetSelDate(const Value: TDate);
var
  R: TRect;
begin
  if FSelDate <> Value then
  begin
    // erase old selection
    R := GetRectForDate(FSelDate);
    InflateRect(R, Selection.Pen.Width + 1, Selection.Pen.Width + 1);
    LCLIntf.InvalidateRect(Handle, @R, True);
    FSelDate := Value;
    if Enabled then
    begin
      // draw new selection
      R := GetRectForDate(FSelDate);
      InflateRect(R, Selection.Pen.Width + 1, Selection.Pen.Width + 1);
      LCLIntf.InvalidateRect(Handle, @R, True);
    end;
  end;
end;

function TJvCustomTMTimeLine.ButtonWidthStored: Boolean;
begin
  Result := FButtonWidth >= 0;
end;

function TJvCustomTMTimeLine.DayWidthStored: Boolean;
begin
  Result := FDayWidth >= 0;
end;

function TJvCustomTMTimeLine.GetButtonWidth: Integer;
begin
  if ButtonWidthStored then
    Result := FButtonWidth
  else
    Result := Scale96ToFont(cTMTimeLineButtonWidth);
end;

function TJvCustomTMTimeLine.GetDayWidth: Integer;
begin
  if DayWidthStored then
    Result := FDayWidth
  else
    Result := Scale96ToFont(cTMTimeLineDayWidth);
end;

function TJvCustomTMTimeLine.GetIconDayDist: Integer;
begin
  if IconDayDistStored then
    Result := FIconDayDist
  else
    Result := Scale96ToFont(cTMTimelineIconDayDist);
end;

function TJvCustomTMTimeLine.IconDayDistStored: Boolean;
begin
  Result := FIconDayDist >= 0;
end;

procedure TJvCustomTMTimeline.SetDayWidth(const Value: Integer);
begin
  if (FDayWidth <> Value) and (Value <> 0) and (Value >= -1) then
  begin
    FDayWidth := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) or ((Button = mbRight) and RightClickSelect) then
    SelDate := DateFromPos(X);
  if CanFocus and not Focused then
    SetFocus;
end;

procedure TJvCustomTMTimeline.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

function TJvCustomTMTimeline.GetBorderStyle: TBorderStyle;
begin
  Result := inherited BorderStyle;
end;

procedure TJvCustomTMTimeline.SetBorderStyle(Value: TBorderStyle);
begin
  inherited;
  if BorderStyle <> Value then
  begin
    FLeftBtn.Flat := (BorderStyle = bsNone);
    FRightBtn.Flat := (BorderStyle = bsNone);
  end;
end;

procedure TJvCustomTMTimeline.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
      FImages.UnRegisterChanges(FChangeLink);
    ReplaceComponentReference(Self, Value, TComponent(FImages));
    if Assigned(FImages) then
      FImages.RegisterChanges(FChangeLink);
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

function TJvCustomTMTimeline.GetImageIndex(ADate: TDate): Integer;
begin
  Result := FDateImages.IndexOf(IntToStr(Trunc(ADate)));
  if Result > -1 then
    Result := PtrUInt(FDateImages.Objects[Result]);
end;

procedure TJvCustomTMTimeline.SetImageIndex(ADate: TDate;
  const Value: Integer);
var
  I: Integer;
begin
  I := FDateImages.IndexOf(IntToStr(Trunc(ADate)));
  if I < 0 then
    I := FDateImages.Add(IntToStr(Trunc(ADate)));
  FDateImages.Objects[I] := TObject(PtrUInt(Value));
  Invalidate;
end;

function TJvCustomTMTimeline.GetObjects(ADate: TDate): TObject;
var
  I: Integer;
begin
  Result := nil;
  I := FObjects.IndexOf(IntToStr(Trunc(ADate)));
  if I > -1 then
    Result := FObjects.Objects[I];
end;

procedure TJvCustomTMTimeline.SetObjects(ADate: TDate; const Value: TObject);
var
  I: Integer;
begin
  I := FObjects.IndexOf(IntToStr(Trunc(ADate)));
  if I < 0 then
    I := FObjects.Add(IntToStr(Trunc(ADate)));
  if Value = nil then
    FObjects.Delete(I)
  else
    FObjects.Objects[I] := Value;
  Invalidate;
end;

procedure TJvCustomTMTimeline.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{$IFDEF LCL_FullVersion >= 1080000}
procedure TCustomTMTimeLine.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);

  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    if FDayWidthStored then
      FDayWidth := Round(FDayWidth * AXProportion);
    if FButtonWidthStored then
      FButtonWidth := Round(FButtonWidth * aXProportion);
    Invalidate;
  end;
end;
{$IFEND}

procedure TJvCustomTMTimeline.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ADate: TDate;
begin
  inherited MouseMove(Shift, X, Y);
  ADate := DateFromPos(X);
  if DateHasImage(ADate) then
    inherited Cursor := FImageCursor
  else
    Cursor := FRealCursor;
end;

procedure TJvCustomTMTimeline.SetImageCursor(const Value: TCursor);
begin
  if FImageCursor <> Value then
  begin
    FImageCursor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.SetSelection(const Value: TJvTLSelFrame);
begin
  FSelection.Assign(Value);
end;

{
procedure TJvCustomTMTimeline.GetDlgCode(var Code: TDlgCodes);
begin
  Include(Code, dcWantArrows);
  Exclude(Code, dcNative);
end;
}
procedure TJvCustomTMTimeline.EnabledChanged;
begin
  inherited EnabledChanged;
  // asn: VisualCLX inherited Create emits EnableChanged event
  if Assigned(FRightBtn) then
  begin
    FLeftBtn.Enabled := Enabled;
    FRightBtn.Enabled := Enabled;
  end;
  Invalidate;
end;

procedure TJvCustomTMTimeline.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not Enabled or ReadOnly then
    Exit;
  // handling keys in KeyDown gives automatic
  // scrolling when holding the key down
  case Key of
    VK_LEFT:
      if ssCtrl in Shift then
        ScrollDate(nil, -LargeChange)
      else
      if ssShift in Shift then
      begin
        SelDate := SelDate - 1;
        // make sure the selection is visible:
        if SelDate > GetLastVisibleDate then
          Self.Date := SelDate - GetVisibleDays + 1;
        if SelDate < Self.Date then
          Self.Date := SelDate;
        Click;
      end
      else
        ScrollDate(nil, -SmallChange);
    VK_RIGHT:
      if ssCtrl in Shift then
        ScrollDate(nil, LargeChange)
      else
      if ssShift in Shift then
      begin
        SelDate := SelDate + 1;
        // make sure the selection is visible:
        if SelDate > GetLastVisibleDate then
          Self.Date := SelDate - GetVisibleDays + 1;
        if SelDate < Self.Date then
          Self.Date := SelDate;
        Click;
      end
      else
        ScrollDate(nil, SmallChange);
  end;
end;

procedure TJvCustomTMTimeline.SetTodayColor(const Value: TColor);
begin
  if FTodayColor <> Value then
  begin
    FTodayColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.SetRightClickSelect(const Value: Boolean);
begin
  if FRightClickSelect <> Value then
    FRightClickSelect := Value;
end;

procedure TJvCustomTMTimeline.SetMaxDate(const Value: TDate);
begin
  if Trunc(FMaxDate) <> Trunc(Value) then
  begin
    FMaxDate := Trunc(Value);
    if FMaxDate <= 0 then
      Exit;
    if FMaxDate < Trunc(Self.Date) then
      Self.Date := FMaxDate;
    if FMaxDate < Trunc(FSelDate) then
      SelDate := FMaxDate;
  end;
end;

procedure TJvCustomTMTimeline.SetMinDate(const Value: TDate);
begin
  if Trunc(FMinDate) <> Trunc(Value) then
  begin
    FMinDate := Trunc(Value);
    if FMinDate <= 0 then
      Exit;
    if FMinDate > Trunc(Self.Date) then
      Self.Date := FMinDate;
    if FMinDate > Trunc(FSelDate) then
      SelDate := FMinDate;
  end;
end;

procedure TJvCustomTMTimeline.SetLargeChange(const Value: Word);
begin
  FLargeChange := Value;
end;

procedure TJvCustomTMTimeline.SetSmallChange(const Value: Word);
begin
  FSmallChange := Value;
end;

procedure TJvCustomTMTimeline.ClearObjects;
begin
  while FObjects.Count > 0 do
  begin
    FObjects.Objects[0].Free;
    FObjects.Delete(0);
  end;
  Invalidate;
end;

procedure TJvCustomTMTimeline.ClearImages;
begin
  FDateImages.Clear;
end;

function TJvCustomTMTimeline.GetLastVisibleDate: TDate;
var
  Tmp: Integer;
begin
  if not ReadOnly then
    Tmp := ButtonWidth * 2
  else
    Tmp := 1;
  Result := FDate + ((Width - Tmp) div DayWidth) - 1;
end;

procedure TJvCustomTMTimeline.SetButtonWidth(const Value: Integer);
begin
  if (FButtonWidth <> Value) and (Value <> 0) and (Value >= -1) then
  begin
    FButtonWidth := Value;
    FLeftBtn.Width := ButtonWidth;
    FRightBtn.Width := ButtonWidth;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeLine.SetIconDayDist(const Value: Integer);
begin
  if (FIconDayDist <> Value) and (Value >= -1) then begin
    FIconDayDist := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.LoadFromFile(const Filename: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure WriteInt(Stream: TStream; Value: Integer);
begin
  Stream.Write(Value, SizeOf(Value));
end;

procedure WriteStr(Stream: TStream; const Value: string);
var
  I: Integer;
  UTF8Value: UTF8String;
begin
  UTF8Value := UTF8Encode(Value);
  I := Length(UTF8Value);
  WriteInt(Stream, I);
  if I > 0 then
    Stream.Write(UTF8Value[1], I);
end;

function ReadInt(Stream: TStream): Integer;
var
  n: Integer = 0;
begin
  Stream.Read(n, SizeOf(n));
  Result := n;
end;

function ReadStr(Stream: TStream): string;
var
  I: Integer;
  UTF8Value: UTF8String = '';
begin
  I := ReadInt(Stream);
  SetLength(Result, I);
  if I > 0 then
  begin
    Stream.Read(UTF8Value[1], I);
//    Result := UTF8ToString(UTF8Value);
    Result := UTF8Value;
  end;
end;

function TJvCustomTMTimeline.ReadMagic(Stream: TStream): Boolean;
begin
  Result := AnsiSameStr(ReadStr(Stream), cMagic);
end;

procedure TJvCustomTMTimeline.LoadFromStream(Stream: TStream);
var
  O: TObject;
  I: Integer;
begin
  ClearImages;
  ClearObjects;
  if not ReadMagic(Stream) then
    raise EStreamError.CreateRes(@RsInvalidImage);
  FDateImages.Text := ReadStr(Stream);
  for I := 0 to FDateImages.Count - 1 do
    FDateImages.Objects[I] := TObject(PtrUInt(ReadInt(Stream)));
  FObjects.Text := ReadStr(Stream);
  for I := 0 to FObjects.Count - 1 do
  begin
    O := nil;
    LoadObject(Stream, O);
    FObjects.Objects[I] := O;
  end;
end;

procedure TJvCustomTMTimeline.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  WriteStr(Stream, cMagic);
  WriteStr(Stream, FDateImages.Text);
  for I := 0 to FDateImages.Count - 1 do
    WriteInt(Stream, PtrUInt(FDateImages.Objects[I]));
  WriteStr(Stream, FObjects.Text);
  for I := 0 to FObjects.Count - 1 do
    SaveObject(Stream, FObjects.Objects[I]);
end;

procedure TJvCustomTMTimeline.SaveToFile(const Filename: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TJvCustomTMTimeline.LoadObject(Stream: TStream; var AObject: TObject);
begin
  if Assigned(FOnReadObject) then
    FOnReadObject(Self, Stream, AObject);
end;

procedure TJvCustomTMTimeline.SaveObject(Stream: TStream; const AObject: TObject);
begin
  if Assigned(FOnWriteObject) then
    FOnWriteObject(Self, Stream, AObject);
end;

procedure TJvCustomTMTimeline.SetObjectsFontStyle(const Value: TFontStyles);
begin
  if FObjectsFontStyle <> Value then
  begin
    FObjectsFontStyle := Value;
    Invalidate;
  end;
end;

function TJvCustomTMTimeline.DoMouseWheelDown(Shift: TShiftState;  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    ScrollDate(Self, -1);
end;

function TJvCustomTMTimeline.DoMouseWheelUp(Shift: TShiftState;  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    ScrollDate(Self, 1);
end;

function TJvCustomTMTimeline.GetVisibleDays: Integer;
begin
  Result := Trunc(GetLastVisibleDate - Self.Date) + 1;
end;

procedure TJvCustomTMTimeline.SetShowMonths(const Value: Boolean);
begin
  if FShowMonths <> Value then
  begin
    FShowMonths := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.SetShowToday(const Value: Boolean);
begin
  if FShowToday <> Value then
  begin
    FShowToday := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.SetShowWeeks(const Value: Boolean);
begin
  if FShowWeeks <> Value then
  begin
    FShowWeeks := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTMTimeline.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;
  {
procedure TJvCustomTMTimeline.CursorChanged;
begin
  inherited CursorChanged;
  FRealCursor := Cursor;
end;
   }
function TJvCustomTMTimeline.DateHasImage(ADate: TDateTime): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Assigned(Images) then
  begin
    I := ImageIndex[ADate];
    Result := (I >= 0) and (I < Images.Count);
  end;
end;

procedure TJvCustomTMTimeline.SetShowTodayIcon(const Value: Boolean);
begin
  if FShowTodayIcon <> Value then
  begin
    FShowTodayIcon := Value;
    Invalidate;
  end;
end;


end.
