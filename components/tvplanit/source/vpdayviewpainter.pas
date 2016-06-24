unit VpDayViewPainter;

interface
{$I vp.inc}


uses
  SysUtils, LCLType, LCLIntf, Types,
  Classes, Graphics, VpConst, VPBase, VpData, VpBasePainter, VpDayView;

type
  { Defines matrix of event records for managing how events overlap with each other. }
  TVpDvEventRec = packed record
    Event: Pointer;
    Level: Integer;
    OLLevels: Integer; { Number of levels which overlap with the event represented by this record. }
    WidthDivisor: Integer; { Maximum OLEvents of all of this event's overlapping neighbors. }
    RealStartTime: TDateTime;
    RealEndTime: TDateTime;
  end;

  TVpDvEventArray = array of TVpDvEventRec;

  TVpDayViewPainter = class(TVpBasePainter)
  private
    FDayView: TVpDayView;
    // local parameters of the old render procedure
    TextWidth: Integer;
    ColHeadRect: TRect;
    CellsRect: TRect;
    RowHeadRect: TRect;
    ADEventsRect: TRect;
    SaveBrushColor: TColor;
    SavePenStyle: TPenStyle;
    SavePenColor: TColor;
    Drawn: Boolean;
    ScrollBarOffset: Integer;
    EventCount: Integer;
    DayWidth: Integer;
    RealNumDays: Integer;
    RealRowHeight: Integer;
    RealColHeadHeight: Integer;
    RealRowHeadWidth: Integer;
    RealVisibleLines: Integer;
    BevelShadow: TColor;
    BevelHighlight: TColor;
    BevelDarkShadow: TColor;
    WindowColor: TColor;
    HighlightText: TColor;
    RealHeadAttrColor: TColor;
    RealRowHeadAttrColor: TColor;
    RealLineColor: TColor;
    RealColor: TColor;
    BevelFace: TColor;
    HighlightBkg: TColor;
    RealADEventBkgColor: TColor;
    ADEventAttrBkgColor: TColor;
    ADEventBorderColor: TColor;
    // variables from local procedures for better access
    dvBmpRecurring: TBitmap;
    dvBmpCategory: TBitmap;
    dvBmpAlarm: TBitmap;
    dvBmpCustom: TBitmap;
    RecurringW: Integer;
    RecurringH: Integer;
    CategoryW: Integer;
    CategoryH: Integer;
    AlarmW: Integer;
    AlarmH: Integer;
    CustomW: Integer;
    CustomH: Integer;

  protected
    function CountOverlappingEvents(Event: TVpEvent; const EArray: TVpDvEventArray): Integer;
    procedure CreateBitmaps;
    function DetermineIconRect(AEventRect: TRect; AEvent: TVpEvent): TRect;
    function GetMaxOLEvents(Event: TVpEvent; const EArray: TVpDvEventArray): Integer;
    procedure DrawAllDayEvents;
    procedure DrawAllDays;
    procedure DrawCells(R: TRect; ColDate: TDateTime; Col: Integer);
    procedure DrawColHeader(R: TRect; ARenderDate: TDateTime; Col: Integer);
    procedure DrawEditFrame(R: TRect; AGutter: Integer; AColor: TColor);
    procedure DrawEvents(ARenderDate: TDateTime; Col: Integer);
    procedure DrawIcons(AIconRect: TRect);
    procedure DrawRowHeader(R: TRect);
    procedure FreeBitmaps;
    procedure GetIcons(Event: TVpEvent);
    procedure InitColors;
    procedure InitializeEventRectangles;
    procedure ScaleIcons(EventRect: TRect);

    procedure SetMeasurements; override;

  public
    constructor Create(ADayView: TVpDayview; ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AUseGran: TVpGranularity; ADisplayOnly: Boolean); override;

  end;

implementation

uses
  VpCanvasUtils, VpMisc;

type
  TVpDayViewOpener = class(TVpDayView);

constructor TVpDayViewPainter.Create(ADayView: TVpDayView; ARenderCanvas: TCanvas);
begin
  inherited Create(ARenderCanvas);
  FDayView := ADayView;
end;

function TVpDayViewPainter.CountOverlappingEvents(Event: TVpEvent;
  const EArray: TVpDvEventArray): Integer;
var
  K, SelfLevel: Integer;
  Tmp: TVpEvent;
  Levels: array of Integer;
begin
  { initialize the levels array }
  SetLength(Levels, MaxEventDepth);
  for K := 0 to pred(MaxEventDepth) do
    Levels[K] := 0;
  result := 0;
  { First, simply count the number of overlapping events. }
  K := 0;
  SelfLevel := -1;
  Tmp := TVpEvent(EArray[K].Event);
  while Tmp <> nil do begin
    if Tmp = Event then begin
      SelfLevel := K;
      Inc(K);
      Tmp := TVpEvent(EArray[K].Event);
      Continue;
    end;

    { if the Tmp event's StartTime or EndTime falls within the range of }
    { Event... }
    if TimeInRange(frac(Tmp.StartTime), frac(Event.StartTime), frac(Event.EndTime), false) or
       TimeInRange(frac(Tmp.EndTime), frac(Event.StartTime), frac(Event.EndTime), false) or
      { or the Tmp event's StartTime is before or equal to the Event's  }
      { start time AND its end time is after or equal to the Event's    }
      { end time, then the events overlap and we will need to increment }
      { the value of K.          }
       ((frac(Tmp.StartTime) <= frac(Event.StartTime)) and (frac(Tmp.EndTime) >= frac(Event.EndTime)))
    then begin
      { Count this event at this level }
      Inc(Levels[EArray[K].Level]);
      Inc(result);
    end;

    Inc(K);
    Tmp := TVpEvent(EArray[K].Event);
  end;
  { Then adjust count for overlapping events which share a level. }
  for K := 0 to pred(MaxEventDepth) do begin
    if K = SelfLevel then Continue;
    if Levels[K] = 0 then Continue;
    result := result - (Levels[K] - 1);
  end;
end;

procedure TVpDayViewPainter.CreateBitmaps;
begin
  dvBmpRecurring := TBitmap.Create;
  dvBmpCategory := TBitmap.Create;
  dvBmpAlarm := TBitmap.Create;
  dvBmpCustom := TBitmap.Create;
end;

function TVpDayViewPainter.DetermineIconRect(AEventRect: TRect;
  AEvent: TVpEvent): TRect;
var
  MaxHeight: Integer;
begin
  Result.Left := AEventRect.Left;
  Result.Top := AEventRect.Top;
  Result.Bottom := AEventRect.Bottom;
  Result.Right := AEventRect.Left + AlarmW + RecurringW + CategoryW + CustomW + 2;

  MaxHeight := AlarmH;
  if RecurringH > MaxHeight then
    MaxHeight := dvBmpRecurring.Height;
  if CategoryH > MaxHeight then
    MaxHeight := dvBmpCategory.Height;
  if CustomH > MaxHeight then
    MaxHeight := dvBmpCustom.Height;
  if MaxHeight > AEventRect.Bottom - AEventRect.Top then
    MaxHeight := AEventRect.Bottom - AEventRect.Top;

  Result.Bottom := AEventRect.Top + MaxHeight;
  if Result.Right > AEventRect.Right then
    Result.Right := AEventRect.Right;
end;

{ returns the maximum OLEvents value from all overlapping neighbors }
function TVpDayViewPainter.GetMaxOLEvents(Event: TVpEvent; const EArray: TVpDvEventArray): Integer;
var
  K: Integer;
  Tmp: TVpEvent;
begin
  result := 1;
  K := 0;
  Tmp := TVpEvent(EArray[K].Event);
  while Tmp <> nil do begin
    { if the Tmp event's StartTime or EndTime falls within the range of Event. }
    if TimeInRange(frac(Tmp.StartTime), frac(Event.StartTime), frac(Event.EndTime), false) or
       TimeInRange(frac(Tmp.EndTime), frac(Event.StartTime), frac(Event.EndTime), false) or
      { or the Tmp event's StartTime is before or equal to the Event's  }
      { start time AND its end time is after or equal to the Event's    }
      { end time, then the events overlap and we will need to check the }
      { value of OLLevels. If it is bigger than result, then modify     }
      { Result accordingly. }
      ((frac(Tmp.StartTime) <= frac(Event.StartTime)) and (frac(Tmp.EndTime) >= frac(Event.EndTime)))
    then begin
      if EArray[K].OLLevels > result then
        Result := EArray[K].OLLevels;
    end;

    Inc(K);
    Tmp := TVpEvent(EArray[K].Event);
  end;
end;


{ Draws the all-day events at the top of the DayView in a special manner }
procedure TVpDayViewPainter.DrawAllDayEvents;
var
  ADEventsList: TList;
  TempList: TList;
  I, J, K: Integer;
  Event: TVpEvent;
  ADEventRect: TRect;
  StartsBeforeRange : Boolean;
  MaxADEvents: Integer;
  Skip: Boolean;
  ADTextHeight: Integer;
  EventStr: string;
  I2: Integer;
  DI: Integer;
  AllDayWidth: Integer;
  OldTop: LongInt;
begin
  if (FDayView.DataStore = nil) or (FDayView.DataStore.Resource = nil) then
    Exit;

  { Collect all of the events for this range and determine the maximum     }
  { number of all day events for the range of days covered by the control. }
  MaxADEvents := 0;

  AllDayWidth := RealWidth - RealRowHeadWidth - 1 - ScrollBarOffset;
  DayWidth := AllDayWidth div FDayView.NumDays;

  ADEventsList := TList.Create;
  try
    TempList := TList.Create;
    try
      for I := 0 to pred(RealNumDays) do begin
        { skip weekends }
        if ((DayOfWeek (RenderDate + i) = 1) or (DayOfWeek (RenderDate + i) = 7)) and
            (not FDayView.IncludeWeekends)
        then
          Continue;

        { get the all day events for the day specified by RenderDate + I }
        FDayView.DataStore.Resource.Schedule.AllDayEventsByDate(RenderDate + I, TempList);

        { Iterate through these events and place them in ADEventsList    }
        Skip := false;
        for J := 0 to pred(TempList.Count) do begin
          if AdEventsList.Count > 0 then begin
            for K := 0 to pred(AdEventsList.Count) do begin
              if TVpEvent(AdEventsList[K]) = TVpEvent(TempList[J]) then begin
                Skip := true;
                Break;
              end;
            end;
            if not Skip then
              AdEventsList.Add(TempList[J]);
          end else
            AdEventsList.Add(TempList[J]);
        end;

        if TempList.Count > MaxADEvents then
          MaxADEvents := TempList.Count;
      end;
    finally
      TempList.Free;
    end;

    if MaxADEvents > 0 then begin
      RenderCanvas.Brush.Color := RealADEventBkgColor;
      RenderCanvas.Font.Assign(FDayView.AllDayEventAttributes.Font);

      { Measure the AllDayEvent TextHeight }
      ADTextHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin;

      { set the top of the event's rect }
      OldTop := ADEventsRect.Top;
      AdEventRect.Top := OldTop + TextMargin + I * ADTextHeight;

      { Build the AllDayEvent rect based on the value of MaxADEvents }
      ADEventsRect.Bottom := AdEventsRect.Top + MaxADEvents * ADTextHeight + TextMargin * 2;

      { Clear the AllDayEvents area }
      TpsFillRect(RenderCanvas, Angle, RenderIn, ADEventsRect);

      for I := 0 to pred(RealNumDays) do begin
        { Set attributes }
        StartsBeforeRange  := false;
        DI := 0;
        { Cycle through the all day events and draw them appropriately }
        for I2 := 0 to pred(ADEventsList.Count) do begin
          Event := ADEventsList[I2];
          if (trunc(Event.StartTime)<=(trunc(RenderDate)+I)) and
             (trunc(Event.EndTime)>=(trunc(RenderDate)+I))
          then begin
            { set the top of the event's rect }
            AdEventRect.Top := OldTop + TextMargin + DI * ADTextHeight;
            inc(DI);

            { see if the event began before the start of the range }
            if (Event.StartTime < trunc(RenderDate)) then
              StartsBeforeRange := true;

            AdEventRect.Bottom := ADEventRect.Top + ADTextHeight;
            AdEventRect.Left := AdEventsRect.Left + DayWidth*I + TextMargin div 2;
            AdEventRect.Right := AdEventRect.Left+DayWidth;

            if StartsBeforeRange then
              EventStr := '>> '
            else
              EventStr := '';

            EventStr := EventStr + Event.Description;

            RenderCanvas.Brush.Color := ADEventAttrBkgColor;
            RenderCanvas.Pen.Color := ADEventBorderColor;
            TPSRectangle(RenderCanvas, Angle, RenderIn,
              ADEventRect.Left + TextMargin,
              ADEventRect.Top + TextMargin div 2,
              ADEventRect.Right - TextMargin,
              ADEventRect.Top + ADTextHeight + TextMargin div 2
            );
            TPSTextOut(RenderCanvas,Angle, RenderIn,
              AdEventRect.Left + TextMargin * 2 + TextMargin div 2,
              AdEventRect.Top + TextMargin div 2,
              EventStr
            );

            TVpDayViewOpener(FDayView).dvEventArray[EventCount].Rec := Rect(
              ADEventRect.Left,
              ADEventRect.Top - 2,
              ADEventRect.Right - TextMargin,
              ADEventRect.Bottom
            );
            TVpDayViewOpener(FDayView).dvEventArray[EventCount].Event := Event;

            Inc(EventCount);
          end;
        end; { for I2 := 0 to pred(ADEventsList.Count) do ... }
      end;
    end;   { if MaxADEvents > 0 }

  finally
    ADEventsList.Free;
  end;
end;

procedure TVpDayViewPainter.DrawAllDays;
var
  i: Integer;
  RPos: Integer;
  AllDayWidth: Integer;
  ExtraSpace: Integer;
  DrawMe: Boolean;
  RealDay: Integer;
begin
  if RealNumDays = 0 then begin
    while (DayOfWeek(RenderDate) = 1) or (DayOfWeek(RenderDate) = 7) do
      RenderDate := RenderDate + 1;
    RealNumDays := FDayView.NumDays;
  end;
  AllDayWidth := RealWidth - RealRowHeadWidth - 1 - ScrollBarOffset;

  DayWidth := AllDayWidth div FDayView.NumDays;
  ExtraSpace := AllDayWidth mod FDayView.NumDays;

  RPos := RowHeadRect.Right;

  RealDay := 0;
  for i := 0 to RealNumDays - 1 do begin
    DrawMe := True;
    if not FDayView.IncludeWeekends then begin
      if (DayOfWeek(RenderDate + i) = 1) or (DayOfWeek(RenderDate + i) = 7) then
        DrawMe := False
    end;
    if DrawMe then begin
      { Draw Column Header }
      ColHeadRect := Rect(RPos, RealTop + 2, RPos + DayWidth - 1, RealTop + RealColHeadHeight);
      if (i = RealNumDays - 1) and (ExtraSpace > 0) then
        ColHeadRect.Right := ColHeadRect.Right + ExtraSpace;

      if Assigned(FDayView.OwnerDrawColHeader) then begin
        Drawn := false;
        FDayView.OwnerDrawColHeader(self, RenderCanvas, ColHeadRect, Drawn);
        if not Drawn then
          DrawColHeader(ColHeadRect, RenderDate + i, RealDay);
      end else
        DrawColHeader(ColHeadRect, RenderDate + i, RealDay);

      { Calculate the column rect for this day }
      RenderCanvas.Font.Assign(FDayView.Font);
      CellsRect := Rect(RPos, ADEventsRect.Bottom + 1, RPos + DayWidth, RealBottom - 2);
      if (i = RealNumDays - 1) and (ExtraSpace > 0) then
        CellsRect.Right := CellsRect.Right + ExtraSpace;

      { set the ColRectArray }
      TVpDayViewOpener(FDayView).dvColRectArray[RealDay].Rec := CellsRect;
      TVpDayViewOpener(FDayView).dvColRectArray[RealDay].Date := RenderDate + i;

      { Draw the cells }
      if Assigned(FDayView.OwnerDrawCells) then begin
        FDayView.OwnerDrawCells(self, RenderCanvas, CellsRect, RealRowHeight, Drawn);
        if not Drawn then
          DrawCells(CellsRect, RenderDate + i, RealDay);
      end else
        DrawCells(CellsRect, RenderDate + i, RealDay);

      { Draw the regular events }
      DrawEvents(RenderDate + i, RealDay);

      Inc(RPos, DayWidth);
      Inc(RealDay);
    end;
  end;
end;

procedure TVpDayViewPainter.DrawCells(R: TRect; ColDate: TDateTime; Col: Integer);
var
  I: Integer;
  LineRect: TRect;
  SavedFont: TFont;
  GutterRect: TRect;
  LineStartTime: Double;
begin
  if StartLine < 0 then
    StartLine := FDayView.TopLine;

  { Set GutterRect size }
  GutterRect.Left := R.Left;
  GutterRect.Top := R.Top;
  GutterRect.Bottom := R.Bottom;
  GutterRect.Right := GutterRect.Left + Round(FDayView.GutterWidth * Scale);
  R.Left := R.Left + Round(FDayView.GutterWidth * Scale) + 1;

  { paint gutter area }
  RenderCanvas.Brush.Color := RealColor;
  TPSFillRect(RenderCanvas, Angle, RenderIn, GutterRect);
  { draw the line down the right side of the gutter }
  RenderCanvas.Pen.Color := BevelShadow;
  RenderCanvas.Pen.Style := psSolid;
  TPSMoveTo(RenderCanvas, Angle, RenderIn, GutterRect.Right, GutterRect.Top);
  TPSLineTo(RenderCanvas, Angle, RenderIn, GutterRect.Right, GutterRect.Bottom);

  for I := 0 to FDayView.LineCount - 1 do begin  // wp: was withoug -1
    with TVpDayViewOpener(FDayView) do begin
      dvLineMatrix[Col, I].Rec.Left := -1;
      dvLineMatrix[Col, I].Rec.Top := -1;
      dvLineMatrix[Col, I].Rec.Right := -1;
      dvLineMatrix[Col, I].Rec.Bottom := -1;
    end;
  end;

  SavedFont := TFont.Create;
  SavedFont.Assign(RenderCanvas.Font);
  try
    RenderCanvas.Font.Assign(FDayView.Font);
    RenderCanvas.Brush.Color := RealColor;
    TPSFillRect(RenderCanvas, Angle, RenderIn, R);

    LineRect := Rect(R.left, R.top, R.Right, R.Top + RealRowHeight);
    RenderCanvas.Pen.Style := psSolid;
    RenderCanvas.Pen.Color := FDayView.LineColor;

    { Paint the client area }
    for I := 0 to RealVisibleLines do begin

      if (I > pred(FDayView.LineCount)) then
        Break;

      if FDayView.TopLine + i >= FDayView.LineCount then
        Break;

      RenderCanvas.Brush.Color := RealColor;
      RenderCanvas.Font.Assign(SavedFont);
      LineRect.Top := Round (R.Top + (i * RealRowHeight));
      LineRect.Bottom := Round (LineRect.Top + (RealRowHeight));
      if I + StartLine < FDayView.LineCount then
        TVpDayViewOpener(FDayView).dvLineMatrix[Col, I + StartLine].Rec := LineRect;

      { color-code cells }

      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      // !!!! This causes problems at design time - implement a better   !!!!
      // !!!! Fix - check the value after the component is streamed in   !!!!
      // !!!! May be a good use for ... loaded or in my message          !!!!
      // !!!! Handler (the message handler would be better               !!!!
      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//        if ActiveRow = -1 then
//         ActiveRow := TopLine;

      if not DisplayOnly then begin
        if FDayView.Focused and (FDayView.ActiveCol = col) and
           (FDayView.ActiveRow = StartLine + I)
        then begin
          { Paint background hilight color }
          RenderCanvas.Brush.Color := HighlightBkg;
          RenderCanvas.Font.Color := HighlightText;
          TPSFillRect(RenderCanvas, Angle, RenderIn, LineRect);
        end else begin
          { paint the active, inactive, weekend, and holiday colors }

          { HOLIDAY COLORS ARE NOT IMPLEMENTED YET }

          { if ColDate is a weekend, then paint all rows the weekend }
          { color. }
          if (DayOfWeek(ColDate) = 1) or (DayOfWeek(ColDate) = 7) then begin
            { this is a weekend }
            RenderCanvas.Brush.Color := FDayView.TimeSlotColors.Weekend;
            TPSFillRect(RenderCanvas, Angle, RenderIn, LineRect);
          end

          else begin
            { ColDate is a weekday, so check to see if the active     }
            { range is set. If it isn't then paint all rows the color }
            { corresponding to Weekday. If it is, then paint inactive }
            { rows the color corresponding to inactive and the active }
            { rows the color corresponding to Active Rows.            }
            if FDayView.TimeSlotColors.ActiveRange.RangeBegin = FDayView.TimeSlotColors.ActiveRange.RangeEnd then
            begin
              { there is no active range, so all time slots are to be }
              { painted the color of Weekday }
              RenderCanvas.Brush.Color := FDayView.TimeSlotColors.Weekday;
              TPSFillRect(RenderCanvas, Angle, RenderIn, LineRect);
            end
            else begin
              { there is an active range defined, so we need to see if }
              { the current line falls in the active range or not, and }
              { paint it accordingly }
              LineStartTime := TVpDayViewOpener(FDayView).dvLineMatrix[Col, StartLine + I].Time;
              if TimeInRange(LineStartTime,
                FDayView.TimeSlotColors.ActiveRange.StartTime,
                FDayView.TimeSlotColors.ActiveRange.EndTime - (1/MinutesInDay), true)
              then begin
                RenderCanvas.Brush.Color := FDayView.TimeSlotColors.Active;
                TPSFillRect (RenderCanvas, Angle, RenderIn, LineRect);
              end else begin
                RenderCanvas.Brush.Color := FDayView.TimeSlotColors.Inactive;
                TPSFillRect (RenderCanvas, Angle, RenderIn, LineRect);
              end;
            end;
          end;
        end;
      end;

      { Draw the lines }
      if I + StartLine <= FDayView.LineCount then begin
        RenderCanvas.Pen.Color := FDayView.LineColor;
        TPSMoveTo(RenderCanvas, Angle, RenderIn, LineRect.Left, LineRect.Top);
        TPSLineTo(RenderCanvas, Angle, RenderIn, LineRect.Right - 1, LineRect.Top);
        TPSMoveTo(RenderCanvas, Angle, RenderIn, LineRect.Left, LineRect.Bottom);
        TPSLineTo(RenderCanvas, Angle, RenderIn, LineRect.Right - 1, LineRect.Bottom);
      end;
    end;

    { Draw a line down the right side of the column to close the }
    { cells right sides }
    RenderCanvas.Pen.Color := BevelShadow;
    RenderCanvas.Pen.Style := psSolid;
    TPSMoveTo(RenderCanvas, Angle, RenderIn, R.Right - 1, R.Bottom);
    TPSLineTo(RenderCanvas, Angle, RenderIn, R.Right - 1, R.Top - 1);

    RenderCanvas.Font.Assign(SavedFont);
  finally
    SavedFont.Free;
  end;
end;

procedure TVpDayViewPainter.DrawColHeader(R: TRect; ARenderDate: TDateTime;
  Col: Integer);
var
  SaveFont: TFont;
  DateStr, ResStr: string;
  DateStrLen, ResStrLen: integer;
  StrHt: Integer;
  TextRect: TRect;
  X, Y: Integer;
begin
  SaveFont := TFont.Create;
  try
    SaveFont.Assign(RenderCanvas.Font);
    { Draw Column Header }
    RenderCanvas.Font.Assign(FDayView.HeadAttributes.Font);
    RenderCanvas.Brush.Color := RealHeadAttrColor;
    RenderCanvas.Pen.Style := psClear;
    TPSRectangle(RenderCanvas, Angle, RenderIn, R);
    RenderCanvas.Pen.Style := psSolid;

    { Size text rect }
    TextRect.TopLeft := R.TopLeft;
    TextRect.BottomRight := R.BottomRight;
    TextRect.Right := TextRect.Right - 3;
    TextRect.Left := TextRect.Left + 2;

    { Fix Date String }
    DateStr := FormatDateTime(FDayView.DateLabelFormat, ARenderDate);
    DateStrLen := RenderCanvas.TextWidth(DateStr);
    StrHt := RenderCanvas.TextHeight(DateStr);
    if DateStrLen > TextRect.Right - TextRect.Left then begin
      DateStr := GetDisplayString(RenderCanvas, DateStr, 0, TextRect.Right - TextRect.Left);
      DateStrLen := RenderCanvas.TextWidth(DateStr);
    end;

    if (FDayView.DataStore <> nil) and (FDayView.DataStore.Resource <> nil)
       and FDayView.ShowResourceName
    then begin
      { fix Res String }
      ResStr := FDayView.DataStore.Resource.Description;
      ResStrLen := RenderCanvas.TextWidth(ResStr);
      if ResStrLen > TextRect.Right - TextRect.Left then begin
        ResStr := GetDisplayString(RenderCanvas, ResStr, 0, TextRect.Right - TextRect.Left);
        ResStrLen := RenderCanvas.TextWidth(ResStr);
      end;
      { center and write the resource name in the first column }
      if (Col = 0) then begin
        X := TextRect.Left + (TextRect.Right - TextRect.Left) div 2 - ResStrLen div 2;
        Y := TextRect.Top + TextMargin;
        TPSTextOut(RenderCanvas, Angle, RenderIn, X, Y, FDayView.DataStore.Resource.Description);
      end;
      { center and write the date string }
      X := TextRect.Left + (TextRect.Right - TextRect.Left) div 2 - DateStrLen div 2;
      Y := TextRect.Top + (TextMargin * 2) + StrHt;
      TPSTextOut(RenderCanvas, Angle, RenderIn, X, Y, DateStr);
    end else begin
      { center and write the date string }
      Y := TextRect.Top + TextMargin;
      X := TextRect.Left + (TextRect.Right - TextRect.Left) div 2 - DateStrLen div 2;
      TPSTextOut(RenderCanvas, Angle, RenderIn, X, Y, DateStr);
    end;

    {Draw Column Head Borders }
    if FDayView.DrawingStyle = dsFlat then begin
      RenderCanvas.Pen.Color := BevelShadow;
      {bottom}
      TPSMoveTo(RenderCanvas, Angle, RenderIn, R.Right, R.Bottom);
      TPSLineTo(RenderCanvas, Angle, RenderIn, R.Left - 1, R.Bottom);
      {right side}
      TPSMoveTo(RenderCanvas, Angle, RenderIn, R.Right, R.Bottom - 4);
      TPSLineTo(RenderCanvas, Angle, RenderIn, R.Right, R.Top + 3);
      RenderCanvas.Pen.Color := BevelHighlight;
      {left side}
      TPSMoveTo(RenderCanvas, Angle, RenderIn, R.Left, R.Bottom - 4);
      TPSLineTo(RenderCanvas, Angle, RenderIn, R.Left, R.Top + 3);
    end
    else
    if FDayView.DrawingStyle = ds3d then begin
      DrawBevelRect(
        RenderCanvas,
        TPSRotateRectangle(Angle, RenderIn, Rect (R.Left, R.Top, R.Right, R.Bottom)),
        BevelHighlight,
        BevelDarkShadow
      );
    end;
    RenderCanvas.Font.Assign(SaveFont);
  finally
    SaveFont.Free;
  end;
end;

{ paint extra borders around the editor }
procedure TVpDayViewPainter.DrawEditFrame(R: TRect; AGutter: Integer;
  AColor: TColor);
begin
  RenderCanvas.Pen.Color := clWindowFrame;
  RenderCanvas.Brush.Color := AColor;
  with R do begin
    TPSFillRect(RenderCanvas, Angle, RenderIn, Rect(Left, Top-AGutter, Right, Top));
    TPSFillRect(RenderCanvas, Angle, RenderIn, Rect(Left-AGutter, Top, Left, Bottom));
    TPSFillRect(RenderCanvas, Angle, RenderIn, Rect(Left, Bottom, Right, Bottom + AGutter));
    TPSFillRect(RenderCanvas, Angle, RenderIn, Rect(Right, Top, Right+AGutter, Bottom));
  end;
end;

procedure TVpDayViewPainter.DrawEvents(ARenderDate: TDateTime; Col: Integer);
var
  I,J, StartPixelOffset, EndPixelOffset: Integer;
  Level, EventWidth, EventSLine, EventELine: Integer;
  EventLineCount: Integer;
  EventSTime, EventETime, ThisTime: Double;
  EventDuration, LineDuration, PixelDuration: Double;
  StartOffset, EndOffset, STime, ETime: Double;
  EventRect, VisibleRect, GutterRect: TRect;
  EventString, Format: string;
  Event: TVpEvent;
  SaveFont: TFont;
  SaveColor: TColor;
  EventArray: TVpDvEventArray;
  EventList: TList;
  IconRect: TRect;
  {$IFDEF DEBUGDV}
  SL : TStringList;
  {$ENDIF}

  procedure VerifyMaxWidthDivisors;
  var
    I, K: Integer;
    Event1, Event2: TVpEvent;
  begin
    for I := 0 to pred(MaxVisibleEvents) do begin
      { if we hit a null event, then we're through }
      if EventArray[I].Event = nil then
        Break;

      { otherwise keep going }
      Event1 := EventArray[I].Event;

      { initialize the WidthDivisor for this record }
      EventArray[I].WidthDivisor := 1;

      {now iterate through all events and get the maximum OLEvents value of }
      { all the overlapping events }
      for K := 0 to pred(MaxVisibleEvents) do begin
        { if we hit a null event, then we're through }
        if EventArray[K].Event = nil then
          Break;

        Event2 := EventArray[K].Event;

        { if the Tmp event overlaps with Event, then check it's Width divisor }
        (* -- original
        if (TimeInRange(Event2.StartTime, Event1.StartTime, Event1.EndTime, false)
          or TimeInRange(Event2.EndTime, Event1.StartTime, Event1.EndTime, false))
        or ((Event2.StartTime <= Event1.StartTime)
          and (Event2.EndTime >= Event1.EndTime))
          *)
        if TimeInRange(frac(Event2.StartTime), frac(Event1.StartTime), frac(Event1.EndTime), false) or
           TimeInRange(frac(Event2.EndTime), frac(Event1.StartTime), frac(Event1.EndTime), false) or
          ((frac(Event2.StartTime) <= frac(Event1.StartTime)) and (frac(Event2.EndTime) >= frac(Event1.EndTime)))
        then begin
          if EventArray[I].WidthDivisor < EventArray[K].WidthDivisor then
            EventArray[I].WidthDivisor := EventArray[K].WidthDivisor;
        end;
      end;
    end;
  end;

var
  OKToDrawEditFrame: Boolean;
  TextRegion : HRGN;
  WorkRegion1: HRGN;
  WorkRegion2: HRGN;
  CW: Integer;
  EventIsEditing: Boolean;
  EventCategory: TVpCategoryInfo;
  OldPen: TPen;
  OldBrush: TBrush;
  OldFont: TFont;
begin
  if (FDayView.DataStore = nil) or (FDayView.DataStore.Resource = nil) or
     (not FDayView.DataStore.Connected) then
    Exit;

  { Save the canvas color and font }
  SaveColor := RenderCanvas.Brush.Color;
  SaveFont := TFont.Create;
  SaveFont.Assign(RenderCanvas.Font);

  { Initialize some stuff }
  if FDayView.TimeFormat = tf24Hour then
    Format := 'h:nn'
  else
    Format := 'h:nnam/pm';

  { set the event array's size }
  SetLength(EventArray, MaxVisibleEvents);

  { Initialize the new matrix }
  for I := 0 to pred(MaxVisibleEvents) do begin
    EventArray[I].Event := nil;
    EventArray[I].Level := 0;
    EventArray[I].OLLevels := 0;
    EventArray[I].WidthDivisor := 0;
  end;

  EventList := TList.Create;
  try
    {Get all of the events for this day}
    FDayView.DataStore.Resource.Schedule.EventsByDate(ARenderDate, EventList);

    { Discard AllDayEvents, because they are drawn above. }
    for I := pred(EventList.Count) downto 0 do begin
      Event := EventList[I];
      if Event.AllDayEvent then
        EventList.Delete(I);
    end;

    { Arrange this day's events in the event matrix }
    Level := 0;
    I := 0;
    while EventList.Count > 0 do begin
      { Iterate through the events, and place them all in the proper     }
      { place in the EventMatrix, according to their start and end times }
      J := 0;
      ThisTime := 0.0;
      while (J < EventList.Count) and (J < MaxVisibleEvents) do begin
        Event := EventList[J];
        if frac(Event.StartTime) >= ThisTime then begin
          ThisTime := frac(Event.EndTime);
          { Handle end times of midnight }
          if ThisTime = 0 then
            ThisTime := EncodeTime(23, 59, 59, 0);
          EventList.Delete(J);
          EventArray[I].Event := Event;
          EventArray[I].Level := Level;
          Inc(I);
          Continue;
        end
        else
          Inc(J);
      end;
      Inc(Level);
    end;

  finally
    EventList.Free;
  end;

  { Count the number of events which all share some of the same time }
  for I := 0 to pred(MaxVisibleEvents) do begin
    if EventArray[I].Event = nil then
      Break;
    EventArray[I].OLLevels := 1 + { it is necessary to count this event too }
      CountOverlappingEvents(TVpEvent(EventArray[I].Event), EventArray);
  end;

  { Calculate the largest width divisor of all overlapping events, }
  { for each event. }
  for I := 0 to pred(MaxVisibleEvents) do begin
    if EventArray[I].Event = nil then
      Break;
    EventArray[I].WidthDivisor := GetMaxOLEvents(TVpEvent(EventArray[I].Event), EventArray);
  end;

  {Make one last pass, to make sure that we have set up the width divisors properly }
  VerifyMaxWidthDivisors;

/////// Debug Code /////////
  { Dump a debug report to drive C }
  {$IFDEF DEBUGDV}
  SL := TStringList.Create;
  try
    I := 0;
    while EventArray[I].Event <> nil do begin
      SL.Add('Description: ' + TVpEvent(EventArray[I].Event).Description
        + #13#10 + '   Level: ' + IntToStr(EventArray[I].Level)
        + #13#10 + '   OLLevels: ' + IntToStr(EventArray[I].OLLevels)
        + #13#10 + '   WidthDivisor: ' + IntToStr(EventArray[I].WidthDivisor));
      Inc(I);
    end;
    SL.SaveToFile('C:\EventList' + IntToStr(Col) + '.txt');
  finally
    Sl.Free;
  end;
  {$ENDIF}
/////// Debug Code /////////

  { Time to paint 'em. Let's see if we calculated their placements correctly   }
  IconRect := Rect(0, 0, 0, 0);
  CreateBitmaps;
  OldFont := TFont.Create;
  OldPen := TPen.Create;
  OldBrush := TBrush.Create;
  try
    { get a rectangle of the visible area }
    VisibleRect := TVpDayViewOpener(FDayView).dvLineMatrix[Col, StartLine].Rec;
    VisibleRect.Bottom := FDayView.ClientRect.Bottom;

    STime := TVpDayViewOpener(FDayView).dvLineMatrix[0, StartLine].Time;
    ETime := TVpDayViewOpener(FDayView).dvLineMatrix[0, StartLine + RealVisibleLines].Time;

    LineDuration := GetLineDuration(FDayView.Granularity);
    { Determine how much time is represented by one pixel. It is the   }
    { amount of time represented by one line, divided by the height of }
    { a line in pixels. }
    with TVpDayViewOpener(FDayView) do
      if HeightOf(dvLineMatrix[Col, StartLine].Rec) > 0 then
        PixelDuration := LineDuration / HeightOf(dvLineMatrix[Col, StartLine].Rec)
      else
        PixelDuration := 0;

    { Iterate through events and paint them }
    for I := 0 to pred(MaxVisibleEvents) do begin
      { get the next event }
      Event := TVpEvent(EventArray[I].Event);

      { if we have hit the end of the events, then bail out }
      if Event = nil then
        Break;

      { Collect useful information needed later }
      EventCategory := FDayView.Datastore.CategoryColorMap.GetCategory(Event.Category);

      EventIsEditing := false;
      if (TVpDayViewOpener(FDayView).dvInPlaceEditor <> nil) and
          TVpDayViewOpener(FDayView).dvInplaceEditor.Visible and
          (FDayView.ActiveEvent = Event)
      then
        EventIsEditing := true;

 (*  -- original
      { remove the date portion from the start and end times }
      EventSTime := Event.StartTime;
      EventETime := Event.EndTime;
      if trunc(EventSTime) < trunc(ARenderDate) then //First Event
        EventSTime := 0+trunc(ARenderDate);
      if trunc(EventETime) > trunc(ARenderDate) then //First Event
        EventETime := 0.999+trunc(ARenderDate);
      EventSTime := EventSTime - ARenderDate;
      EventETime := EventETime - ARenderDate;
      { Find the line on which this event starts }
      EventSLine := GetStartLine(EventSTime, Granularity);
      { Handle End Times of Midnight }
      if EventETime = 0 then
        EventETime := EncodeTime (23, 59, 59, 0);
  *)

      { remove the date portion from the start and end times }
      EventSTime := Event.StartTime;
      EventETime := Event.EndTime;
      if (EventSTime < trunc(ARenderDate)) and (Event.RepeatCode = rtNone) then  // First Event
        EventSTime := trunc(ARenderDate)
      else if (Event.RepeatCode <> rtNone) then
        EventSTime := frac(EventSTime) + trunc(ARenderDate);
      if (trunc(EventETime) > trunc(ARenderDate)) and (Event.RepeatCode = rtNone) then  // First Event
        EventETime := 0.999 + trunc(ARenderDate)
      else if (Event.RepeatCode <> rtNone) then
        EventETime := frac(EventETime) + trunc(ARenderDate);
      EventSTime := EventSTime - trunc(ARenderDate);
      EventETime := EventETime - trunc(ARenderDate);
      { Find the line on which this event starts }
      EventSLine := GetStartLine(EventSTime, FDayView.Granularity);
      { Handle End Times of Midnight }
      if EventETime = 0 then
        EventETime := EncodeTime(23, 59, 59, 0);

      { calculate the number of lines this event will cover }
      EventELine := GetEndLine(EventETime {Event.EndTime}, FDayView.Granularity);
      EventLineCount := EventELine - EventSLine + 1;
      EventDuration := EventETime - EventSTime;

      { if the event doesn't occupy area that is currently visible, then skip it. }
      if (EventELine < StartLine) or (EventSLine > StartLine + RealVisibleLines) then
        Continue;

      { Build the rectangle in which the event will be painted. }
      EventRect := TVpDayViewOpener(FDayView).dvLineMatrix[Col, EventSLine].Rec;
      if EventRect.Left < VisibleRect.Left then
        EventRect.Left := VisibleRect.Left;
      if EventRect.Top < VisibleRect.Top then
        EventRect.Top := VisibleRect.Top;
      EventRect.Bottom := TVpDayViewOpener(FDayView).dvLineMatrix[Col, EventELine].Rec.Bottom;
      if EventRect.Bottom < VisibleRect.Top then
        EventRect.Bottom := VisibleRect.Bottom;
      EventWidth := WidthOf(VisibleRect) div EventArray[I].WidthDivisor;

      { Slide the rect over to correspond with the level }
      if EventArray[I].Level > 0 then
        EventRect.Left := EventRect.Left + EventWidth * EventArray[I].Level
      { added because level 0 events were one pixel too far to the right }
      else
        EventRect.Left := EventRect.Left - 1;
      EventRect.Right := EventRect.Left + EventWidth - FDayView.GutterWidth;

      { Draw the event rectangle }
      { paint Event text area clWindow }
      if Assigned(FDayView.DataStore) then begin
        if EventIsEditing then
          RenderCanvas.Brush.Color := clWindow
        else
          RenderCanvas.Brush.Color := EventCategory.BackgroundColor
      end else
        RenderCanvas.Brush.Color := WindowColor;
      TPSFillRect(RenderCanvas, Angle, RenderIn, EventRect);

      { paint the little area to the left of the text the color }
      { corresponding to the event's category                   }
      { These colors are used even when printing }
      if Assigned(FDayView.DataStore) then
        RenderCanvas.Brush.Color := EventCategory.Color; //FDayView.DataStore.CategoryColorMap.GetColor(Event.Category);

      { find the pixel offset to use for determining where to start and }
      { stop drawing colored area according to the start time and end time of the event. }
      StartPixelOffset := 0;
      EndPixelOffset := 0;

      if (PixelDuration > 0) and (EventDuration < GetLineDuration(FDayView.Granularity) * EventLineCount)
      then begin
        if (EventSLine >= StartLine) and (EventSTime > TVpDayViewOpener(FDayView).dvLineMatrix[0, EventSLine].Time)
        then begin
          { Get the start offset in TDateTime format }
          StartOffset := EventSTime - TVpDayViewOpener(FDayView).dvLineMatrix[0, EventSLine].Time;

          { determine how many pixels to scooch down before painting the event's color code. }
          StartPixelOffset := trunc(StartOffset / PixelDuration);
        end;

        if (EventELine <= StartLine + RealVisibleLines) and
           (EventETime < TVpDayViewOpener(FDayView).dvLineMatrix[0, EventELine + 1].Time)
        then begin
          { Get the end offset in TDateTime format }
          EndOffset := TVpDayViewOpener(FDayView).dvLineMatrix[0, EventELine + 1].Time  - EventETime;

          { determine how many pixels to scooch down before painting the   }
          { event's color code. }
          EndPixelOffset := trunc(EndOffset / PixelDuration);
        end;
      end;

      { Paint the gutter inside the EventRect all events }
      if (EventArray[I].Level = 0) then
        GutterRect.Left := EventRect.Left - Trunc(FDayView.GutterWidth * Scale)
      else
        GutterRect.Left := EventRect.Left;
      GutterRect.Right := GutterRect.Left + Round(FDayView.GutterWidth * Scale);
      GutterRect.Top := EventRect.Top + StartPixelOffset;
      GutterRect.Bottom := EventRect.Bottom - EndPixelOffset;

      TPSFillRect(RenderCanvas, Angle, RenderIn, GutterRect);

      RenderCanvas.Brush.Color := WindowColor;

{      if (TVpDayViewOpener(FDayView).dvInPlaceEditor <> nil) and
          TVpDayViewOpener(FDayView).dvInplaceEditor.Visible then
      begin
        if FDayView.ActiveEvent = Event then
          EventIsEditing := True
        else
          EventIsEditing := False;
      end else
        EventIsEditing := False;
}
      { build the event string }
      IconRect.Left := EventRect.Left;
      IconRect.Top := EventRect.Top;
      IconRect.Right := EventRect.Left;
      IconRect.Bottom := EventRect.Top;
      if not DisplayOnly then begin
        GetIcons(Event);
        if EventArray[I].Level = 0 then begin
          ScaleIcons(EventRect);
          IconRect := DetermineIconRect(EventRect, Event);
        end else begin
          ScaleIcons(Rect(
            EventRect.Left + FDayView.GutterWidth,
            EventRect.Top,
            EventRect.Right,
            EventRect.Bottom
          ));
          IconRect := DetermineIconRect(Rect(
            EventRect.Left + FDayView.GutterWidth,
            EventRect.Top,
            EventRect.Right,
            EventRect.Bottom
            ),
            Event
          );
        end;
      end;

      { wp: Using Canvas here does not look correct...
      OldPen.Assign(Canvas.Pen);
      OldBrush.Assign(Canvas.Brush);
      OldFont.Assign(Canvas.Font);
      }
      OldPen.Assign(RenderCanvas.Pen);
      OldBrush.Assign(RenderCanvas.Brush);
      OldFont.Assign(RenderCanvas.Font);
      if Assigned(FDayView.OnBeforeDrawEvent) and (EventArray[I].Level = 0) then
        FDayView.OnBeforeDrawEvent(Self, Event, FDayView.ActiveEvent = Event, RenderCanvas, EventRect, IconRect)
      else if Assigned(FDayView.OnBeforeDrawEvent) then
        FDayView.OnBeforeDrawEvent(Self, Event, FDayView.ActiveEvent = Event, RenderCanvas,
          Rect(EventRect.Left + FDayView.GutterWidth, EventRect.Top, EventRect.Right, EventRect.Bottom),
          IconRect
        );

      if not DisplayOnly then
        DrawIcons(IconRect);

      if FDayView.ShowEventTimes then
        EventString := FormatDateTime(Format, Event.StartTime) + '-' +
          FormatDateTime(Format, Event.EndTime) + ' ' + Event.Description
      else
        EventString := Event.Description;

      if FDayView.WrapStyle = wsNone then begin
        { if the string is longer than the availble space then chop    }
        { off the and and place those little '...'s at the end         }
        if RenderCanvas.TextWidth(EventString) > EventRect.Right - IconRect.Right - Round(FDayView.GutterWidth * Scale) - TextMargin
        then
          EventString := GetDisplayString(
            RenderCanvas,
            EventString,
            0,
            EventRect.Right - IconRect.Right - Round(FDayView.GutterWidth * Scale) - TextMargin
          );
        end;

        if (FDayView.WrapStyle <> wsNone) and (not EventIsEditing) then begin
          if (EventRect.Bottom <> IconRect.Bottom) and (EventRect.Left <> IconRect.Right)
        then begin
          if FDayView.WrapStyle = wsIconFlow then
          begin
            WorkRegion1 := CreateRectRgn(IconRect.Right, EventRect.Top, EventRect.Right, IconRect.Bottom);
            WorkRegion2 := CreateRectRgn(EventRect.Left + FDayView.GutterWidth, IconRect.Bottom, EventRect.Right, EventRect.Bottom);
            TextRegion := CreateRectRgn(IconRect.Right, EventRect.Top, EventRect.Right, IconRect.Bottom);
            CombineRgn(TextRegion, WorkRegion1, WorkRegion2, RGN_OR);
          end else
            TextRegion := CreateRectRgn(IconRect.Right, EventRect.Top, EventRect.Right, EventRect.Bottom);
        end else
          TextRegion := CreateRectRgn(IconRect.Right + FDayView.GutterWidth, EventRect.Top, EventRect.Right, EventRect.Bottom);
        try
          CW := RenderTextToRegion(RenderCanvas, Angle, RenderIn, TextRegion, EventString);
          { write the event string to the proper spot in the EventRect }
          if CW < Length (EventString) then begin
            RenderCanvas.Brush.Color := FDayView.DotDotDotColor;
            { draw dot dot dot }
            TPSFillRect(RenderCanvas, Angle, RenderIn,
              Rect(EventRect.Right - 20, EventRect.Bottom - 7, EventRect.Right - 17, EventRect.Bottom - 4)
            );
            TPSFillRect(RenderCanvas, Angle, RenderIn,
              Rect(EventRect.Right - 13, EventRect.Bottom - 7, EventRect.Right - 10, EventRect.Bottom - 4));
            TPSFillRect(RenderCanvas, Angle, RenderIn,
              Rect(EventRect.Right - 6, EventRect.Bottom - 7, EventRect.Right - 3, EventRect.Bottom - 4));
          end;

        finally
          if ((EventRect.Bottom > IconRect.Bottom) and (EventRect.Left > IconRect.Right)) or
             (FDayView.WrapStyle = wsIconFlow)
          then begin
            DeleteObject(WorkRegion1);
            DeleteObject(WorkRegion2);
            DeleteObject(TextRegion);
          end else begin
            DeleteObject(TextRegion);
          end;
        end;
      end
      else
      if (not EventIsEditing) then begin
        if EventArray[I].Level = 0 then
          { don't draw the gutter in the EventRest for level 0 events. }
          TPSTextOut(RenderCanvas,
            Angle,
            RenderIn,
            IconRect.Right + FDayView.GutterWidth + TextMargin,
            EventRect.Top + TextMargin,
            EventString
          )
        else
          TPSTextOut(RenderCanvas,
            Angle,
            RenderIn,
            IconRect.Right + FDayView.GutterWidth + TextMargin,
            EventRect.Top + TextMargin,
            EventString
          );
      end;

      { paint the borders around the event text area }
      TPSPolyline(RenderCanvas, Angle, RenderIn, [
        Point(EventRect.Left, EventRect.Top),
        Point(EventRect.Right, EventRect.Top),
        Point(EventRect.Right, EventRect.Bottom),
        Point(EventRect.Left, EventRect.Bottom),
        Point(EventRect.Left, EventRect.Top)
      ]);
      { don't paint gutter area on level 0 items }
      if EventArray[I].Level > 0 then begin
        TPSMoveTo(RenderCanvas, Angle, RenderIn, EventRect.Left + Round(FDayView.GutterWidth * Scale), EventRect.Top);
        TPSLineTo(RenderCanvas, Angle, RenderIn, EventRect.Left + Round(FDayView.GutterWidth * Scale), EventRect.Bottom);
      end;

      if Assigned(FDayView.OnAfterDrawEvent) and (EventArray[I].Level = 0) then
        FDayView.OnAfterDrawEvent(Self, Event, FDayView.ActiveEvent = Event, RenderCanvas, EventRect, IconRect)
      else
      if Assigned(FDayView.OnAfterDrawEvent) then
        FDayView.OnAfterDrawEvent(Self, Event, FDayView.ActiveEvent = Event, RenderCanvas,
          Rect(EventRect.Left + FDayView.GutterWidth, EventRect.Top, EventRect.Right, EventRect.Bottom),
          IconRect
        );

      { Canvas does not look correct here...
      Canvas.Brush.Assign(OldBrush);
      Canvas.Pen.Assign(OldPen);
      Canvas.Font.Assign(OldFont);           }

      RenderCanvas.Brush.Assign(OldBrush);
      RenderCanvas.Pen.Assign(OldPen);
      RenderCanvas.Font.Assign(OldFont);

      TVpDayViewOpener(FDayView).dvEventArray[EventCount].Rec := Rect(
        EventRect.Left, EventRect.Top, EventRect.Right, EventRect.Bottom + 1
      );
      TVpDayViewOpener(FDayView).dvEventArray[EventCount].IconRect := IconRect;
      TVpDayViewOpener(FDayView).dvEventArray[EventCount].Event := Event;

      Inc(EventCount);
    end;

    { paint extra borders around the editor }
    OKToDrawEditFrame := True;
    if Assigned(FDayView.ActiveEvent) then
      OKToDrawEditFrame := not (FDayView.ActiveEvent.AllDayEvent);

    if (TVpDayViewOpener(FDayView).dvInPlaceEditor <> nil) and
        TVpDayViewOpener(FDayView).dvInplaceEditor.Visible and
        OKToDrawEditFrame
    then
      with TVpDayViewOpener(FDayView) do
        DrawEditFrame(dvActiveEventRec, GutterWidth, Datastore.CategoryColorMap.GetColor(ActiveEvent.Category));

  finally
    { Clean Up }
    try
      SetLength(EventArray, 0);
      FreeBitmaps;
    finally
      { restore canvas color and font }
      RenderCanvas.Brush.Color := SaveColor;
      RenderCanvas.Font.Assign(SaveFont);
      SaveFont.Free;
      OldFont.Free;
      OldPen.Free;
      OldBrush.Free;
    end;
  end;
end;

procedure TVpDayViewPainter.DrawIcons(AIconRect: TRect);
var
  DrawPos: Integer;

  procedure DrawIcon(bmp: TBitmap; w, h: Integer; IncDrawPos: Boolean = false);
  var
    R: TRect;
  begin
    if (bmp.Width <> 0) and (bmp.Height <> 0) then
    begin
      bmp.Transparent := True;
      R := Rect(0, 0, w, h);
      OffsetRect(R, AIconRect.Left + 1, AIconRect.Top + 1);
      RenderCanvas.StretchDraw(R, bmp);
      {
      RenderCanvas.CopyRect(  // wp: was FDayview.Canvas -- does not look correct...
        Rect(AIconRect.Left + 1, AIconRect.Top + 1, AIconRect.Left + w + 1, AIconRect.Top + h + 1),
        bmp.Canvas,
        Rect(0, 0, bmp.Width, bmp.Height)
      );
      }
      if IncDrawPos then
        inc(DrawPos, w);
    end;
  end;

begin
  DrawPos := 1;
  DrawIcon(dvBmpCustom, CustomW, CustomH);
  DrawIcon(dvBmpCategory, CategoryW, CategoryH);
  DrawIcon(dvBmpAlarm, AlarmW, AlarmH);
  DrawIcon(dvBmpRecurring, RecurringW, RecurringH, false);
end;

procedure TVpDayViewPainter.DrawRowHeader(R: TRect);
var
  Temp , I: Integer;
  LineRect: TRect;
  LastHour, Hour: Integer;
  MinuteStr, HourStr: string;
  SaveFont: TFont;
begin
  if StartLine < 0 then
    StartLine := FDayView.TopLine;

  SaveFont := TFont.Create;
  try
    RenderCanvas.Pen.Style := psClear;
    RenderCanvas.Brush.Color := RealRowHeadAttrColor;
    TPSFillRect(RenderCanvas, Angle, RenderIn, R);
    RenderCanvas.Pen.Style := psSolid;

    RenderCanvas.Font.Assign(FDayView.RowHeadAttributes.MinuteFont);
    RealVisibleLines := TVpDayViewOpener(FDayView).dvCalcVisibleLines(
      R.Bottom - R.Top,
      RealColHeadHeight,
      RealRowHeight,
      Scale,
      StartLine,
      StopLine
    );
    Temp := RenderCanvas.TextWidth('33');
    Temp := Temp + 10;
    RenderCanvas.Pen.Style := psSolid;
    RenderCanvas.Pen.Color := RealLineColor;
    LineRect := Rect(R.Left, R.Top, R.Right, R.Top + RealRowHeight);
    Hour := Ord(TVpDayViewOpener(FDayView).dvLineMatrix[0, StartLine].Hour);

    for I := 0 to RealVisibleLines do begin
      { prevent any extranneous drawing below the last hour }
      if (I + FDayView.TopLine >= FDayView.LineCount) or (Hour > 23) then
        Break;

      if I = 0 then begin
        if Hour < 12 then
          MinuteStr := 'am'
        else
          MinuteStr := 'pm';
      end
      else if Ord(Hour) = 12 then
        MinuteStr := 'pm'
      else
        MinuteStr := '00';

      if FDayView.TimeFormat = tf24Hour then
        MinuteStr := '00';

      { Position the rect }
      LineRect.Top := R.Top + i * RealRowHeight;
      LineRect.Bottom := LineRect.Top + RealRowHeight;

      if (Hour > 12) and (FDayView.TimeFormat = tf12Hour) then
        HourStr := IntToStr(Hour - 12)
      else begin
        HourStr := IntToStr(Hour);
        if (FDayView.TimeFormat = tf12Hour) and (HourStr = '0') then
          HourStr := '12';
      end;

      if UseGran = gr60Min then begin
        { Paint time }
        RenderCanvas.Font.Assign(FDayView.RowHeadAttributes.MinuteFont);
        TPSTextOut(RenderCanvas, Angle, RenderIn,
          LineRect.Right - RenderCanvas.TextWidth(HourStr + ':' + MinuteStr) - 7,
          LineRect.Top + TextMargin,
          HourStr + ':' + MinuteStr
        );
        LastHour := Hour;
        Inc(Hour);
      end else begin
        { Paint Minute Text}
        if TVpDayViewOpener(FDayView).dvLineMatrix[0, StartLine + i].Minute = 0 then begin
          RenderCanvas.Font.Assign(FDayView.RowHeadAttributes.MinuteFont);
          TPSTextOut(RenderCanvas, Angle, RenderIn,
            LineRect.Right - RenderCanvas.TextWidth(MinuteStr) - 7,
            LineRect.Top + TextMargin,
            MinuteStr
          );
          { Paint Hour Text }
          RenderCanvas.Font.Assign(FDayView.RowHeadAttributes.HourFont);
          TPSTextOut(RenderCanvas, Angle, RenderIn,
            LineRect.Right - RenderCanvas.TextWidth(HourStr) - 2 - Temp,
            LineRect.Top + TextMargin - 2,
            HourStr
          );
        end;
        LastHour := Hour;
        Hour := Ord(TVpDayViewOpener(FDayView).dvLineMatrix[0, StartLine + i + 1].Hour);
      end;

      TPSMoveTo(RenderCanvas, Angle, RenderIn, LineRect.Right-6, LineRect.Bottom);
      if LastHour <> Hour then
        TPSLineTo(RenderCanvas, Angle, RenderIn, LineRect.Left + 6, LineRect.Bottom)
      else
        TPSLineTo(RenderCanvas, Angle, RenderIn, LineRect.Right-Temp, LineRect.Bottom);
    end; {for}

    { Draw Row Header Borders }
    if FDayView.DrawingStyle = dsFlat then begin
      DrawBevelRect(RenderCanvas, TPSRotateRectangle (Angle, RenderIn,
        Rect(R.Left - 1, R.Top, R.Right - 1, R.Bottom - 2)),
        BevelHighlight,
        BevelShadow
      );
    end
    else if FDayView.DrawingStyle = ds3d then begin
      DrawBevelRect(RenderCanvas,
        TPSRotateRectangle (Angle, RenderIn, Rect(R.Left + 1, R.Top, R.Right - 1, R.Bottom - 3)),
        BevelHighlight,
        BevelDarkShadow
      );
    end;

    RenderCanvas.Font.Assign(SaveFont);

  finally
    SaveFont.Free;
  end;
end;

procedure TVpDayViewPainter.FreeBitmaps;
begin
  dvBmpRecurring.Free;
  dvBmpCategory.Free;
  dvBmpAlarm.Free;
  dvBmpCustom.Free;
end;

procedure TVpDayViewPainter.GetIcons(Event: TVpEvent);
var
  ShowAlarm: Boolean;
  ShowRecurring: Boolean;
  ShowCategory: Boolean;
  ShowCustom: Boolean;
  Icons: TVpDVIcons;
  cat: TVpCategoryInfo;
  w, h: Integer;
begin
  ShowAlarm := False;
  ShowRecurring := False;
  ShowCategory := False;
  ShowCustom := False;

 // FDayView.IconAttributes.AlarmBitmap.SaveToFile('d:\test.bmp');


  if Event.AlarmSet then begin
    dvBmpAlarm.Assign(FDayView.IconAttributes.AlarmBitmap);
    ShowAlarm := (dvBmpAlarm.Width <> 0) and (dvBmpAlarm.Height <> 0);
  end;

  if Event.RepeatCode <> rtNone then begin
    dvBmpRecurring.Assign(FDayView.IconAttributes.RecurringBitmap);
    ShowRecurring := (dvBmpRecurring.Width <> 0) and (dvBmpRecurring.Height <> 0);
  end;

  if Assigned(FDayView.DataStore) then begin
    if Event.Category < 10 then begin
      cat := FDayView.Datastore.CategoryColorMap.GetCategory(Event.Category);
      w := cat.Bitmap.Width;
      h := cat.Bitmap.Height;
      dvBmpCategory.Width := w;
      dvBmpCategory.Height := h;
      dvBmpCategory.Canvas.CopyRect(
        Rect(0, 0, w, h),
        cat.Bitmap.Canvas,
        Rect(0, 0, w, h)
      );
    end else
    begin
      dvBmpCategory.Width  := 0;
      dvBmpCategory.Height := 0;
    end;
    ShowCategory := (dvBmpCategory.Width <> 0) and (dvBmpCategory.Height <> 0);
  end;

  dvBmpCustom.Width  := 0;
  dvBmpCustom.Height := 0;

  if not FDayView.IconAttributes.ShowAlarmBitmap then
    ShowAlarm := False;
  if not FDayView.IconAttributes.ShowCategoryBitmap then
    ShowCategory := False;
  if not FDayView.IconAttributes.ShowRecurringBitmap then
    ShowRecurring := False;

  if Assigned(FDayView.OnDrawIcons) then begin
    Icons[itAlarm].Show := ShowAlarm;
    Icons[itAlarm].Bitmap := dvBmpAlarm;
    Icons[itRecurring].Show := ShowRecurring;
    Icons[itRecurring].Bitmap := dvBmpRecurring;
    Icons[itCategory].Show := ShowCategory;
    Icons[itCategory].Bitmap := dvBmpCategory;
    Icons[itCustom].Show := ShowCustom;
    Icons[itCustom].Bitmap := dvBmpCustom;

    FDayView.OnDrawIcons (Self, Event, Icons);

    ShowAlarm := Icons[itAlarm].Show;
    ShowRecurring := Icons[itRecurring].Show;
    ShowCategory := Icons[itCategory].Show;
    ShowCustom := Icons[itCustom].Show;
  end;

  if not ShowAlarm then begin
    dvBmpAlarm.Width := 0;
    dvBmpAlarm.Height := 0;
  end;

  if not ShowRecurring then begin
    dvBmpRecurring.Width := 0;
    dvBmpRecurring.Height := 0;
  end;

  if not ShowCategory then begin
    dvBmpCategory.Width := 0;
    dvBmpCategory.Height := 0;
  end;

  if not ShowCustom then begin
    dvBmpCustom.Width := 0;
    dvBmpCustom.Height := 0;
  end;

  AlarmW := dvBmpAlarm.Width;
  RecurringW := dvBmpRecurring.Width;
  CategoryW := dvBmpCategory.Width;
  CustomW := dvBmpCustom.Width;
  AlarmH := dvBmpAlarm.Height;
  RecurringH := dvBmpRecurring.Height;
  CategoryH := dvBmpCategory.Height;
  CustomH := dvBmpCustom.Height;
end;

procedure TVpDayViewPainter.InitColors;
begin
  if DisplayOnly then begin
    BevelShadow := clBlack;
    BevelHighlight := clBlack;
    BevelDarkShadow := clBlack;
    BevelFace := clBlack;
    WindowColor := clWhite;
    HighlightText := clBlack;
    RealHeadAttrColor := clSilver;
    RealRowHeadAttrColor := clSilver;
    RealLineColor := clBlack;
    RealColor := clWhite;
    HighlightBkg := clWhite;
    RealADEventBkgColor := clWhite;
    ADEventAttrBkgColor := clWhite;
    ADEventBorderColor := clBlack;
  end else begin
    BevelShadow := clBtnShadow;
    BevelHighlight := clBtnHighlight;
    BevelDarkShadow := cl3DDkShadow;
    BevelFace := clBtnFace;
    WindowColor := clWindow;
    HighlightText := clHighlightText;
    HighlightBkg := clHighlight;
    RealHeadAttrColor := FDayView.HeadAttributes.Color;
    RealRowHeadAttrColor := FDayView.RowHeadAttributes.Color;
    RealLineColor := FDayView.LineColor;
    RealColor := FDayView.Color;
    RealADEventBkgColor := FDayView.AllDayEventAttributes.BackgroundColor;
    ADEventAttrBkgColor := FDayView.AllDayEventAttributes.EventBackgroundColor;
    ADEventBorderColor := FDayView.AllDayEventAttributes.EventBorderColor;
  end;
end;

procedure TVpDayViewPainter.InitializeEventRectangles;
var
  I : Integer;
begin
  EventCount := 0;
  with TVpDayViewOpener(FDayView) do
    for I := 0 to pred(Length(dvEventArray)) do begin
      dvEventArray[I].Rec.Left := -1;
      dvEventArray[I].Rec.Top := -1;
      dvEventArray[I].Rec.Right := -1;
      dvEventArray[I].Rec.Bottom := -1;
      dvEventArray[I].Event := nil;
    end;
end;

procedure TVpDayViewPainter.RenderToCanvas(ARenderIn: TRect;
  AAngle: TVpRotationAngle; AScale: Extended; ARenderDate: TDateTime;
  AStartLine, AStopLine: Integer; AUseGran: TVpGranularity; ADisplayOnly: Boolean);
begin
  inherited;

  // Here begins the original routine...
  InitColors;
  SavePenBrush;
  InitPenBrush;

  SetMeasurements;

  if StartLine < 0 then
    StartLine := FDayView.TopLine;

  if DisplayOnly then
    ScrollBarOffset := 2
  else
    ScrollBarOffset := 14;

  Rgn := CreateRectRgn(RenderIn.Left, RenderIn.Top, RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn(RenderCanvas.Handle, Rgn);

    { Calculate Row Header }
    RealRowHeight := TVpDayViewOpener(FDayView).dvCalcRowHeight(Scale, UseGran);
    RealColHeadHeight := TVpDayViewOpener(FDayView).dvCalcColHeadHeight(Scale);

    RenderCanvas.Font.Assign(FDayView.RowHeadAttributes.HourFont);
    TextWidth := RenderCanvas.TextWidth('33');
    RealRowHeadWidth := TextWidth * 2 + 10;

    { initialize the All Day Events area... }
    ADEventsRect.Left := RealLeft + 3 + RealRowHeadWidth;
    ADEventsRect.Top := RealTop + RealColHeadHeight;
    ADEventsRect.Right := FDayView.ClientRect.Right;
    ADEventsRect.Bottom := AdEventsRect.Top;

    { Calculate the RealNumDays (The number of days the control covers) }
    RealNumDays := TVpDayViewOpener(FDayView).GetRealNumDays(RenderDate);

    InitializeEventRectangles;

    { Draw the All Day Events }
    DrawAllDayEvents;

    { draw the area in the top left corner, where the nav buttons go. }
    RowHeadRect := Rect(
      RealLeft + 1,
      RealTop,
      RealLeft + 3 + RealRowHeadWidth,
      RealTop + RealColHeadHeight + 2
    );

    RenderCanvas.Brush.Color := RealHeadAttrColor;
    TPSFillRect(RenderCanvas, Angle, RenderIn, RowHeadRect);

    if FDayView.DrawingStyle = ds3d then
      DrawBevelRect(
        RenderCanvas,
        TPSRotateRectangle(Angle, RenderIn, Rect(
          RowHeadRect.Left + 1,
          RowHeadRect.Top + 2,
          RowHeadRect.Right - 2,
          RowHeadRect.Bottom - 2
        )),
        BevelHighlight,
        BevelShadow
      )
    else begin
      RenderCanvas.Pen.Color := BevelShadow;
      TPSMoveTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2, RowHeadRect.Bottom - 2);
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Left, RowHeadRect.Bottom - 2);
      RenderCanvas.Pen.Color := BevelHighlight;
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Left, RowHeadRect.Top);
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2, RowHeadRect.Top);
      RenderCanvas.Pen.Color := BevelShadow;
      TPSMoveTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2, RowHeadRect.Top + 6);
      TPSLineTo(RenderCanvas, Angle, RenderIn, RowHeadRect.Right - 2, RowHeadRect.Bottom - 5);
    end;

    RenderCanvas.Font.Assign(FDayView.RowHeadAttributes.HourFont);
    if FDayView.DrawingStyle = dsFlat then
      RowHeadRect := Rect(RealLeft + 2, ADEventsRect.Bottom + 1, RealLeft + 2 + RealRowHeadWidth, RealBottom)
    else
      RowHeadRect := Rect(RealLeft + 1, ADEventsRect.Bottom + 1, RealLeft + 2 + RealRowHeadWidth, RealBottom);

    if Assigned(FDayView.OwnerDrawRowHeader) then begin
      Drawn := false;
      FDayView.OwnerDrawRowHeader(self, RenderCanvas, RowHeadRect, RealRowHeight, Drawn);
      if not Drawn then
        DrawRowHeader(RowHeadRect);
    end else
      DrawRowHeader(RowHeadRect);

    { Draw the regular events }
    DrawAllDays;

    { Draw Borders }
    if FDayView.DrawingStyle = dsFlat then begin
      { Draw an outer and inner bevel }
      DrawBevelRect(
        RenderCanvas,
        TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft, RealTop, RealRight - 1, RealBottom - 1)),
        BevelShadow,
        BevelHighlight
      );
      DrawBevelRect(
        RenderCanvas,
        TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft + 1, RealTop + 1, RealRight - 2, RealBottom - 2)),
        BevelHighlight,
        BevelShadow
      );
    end else
    if FDayView.DrawingStyle = ds3d then begin
      { Draw a 3d bevel }
      DrawBevelRect(
        RenderCanvas,
        TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft, RealTop, RealRight - 1, RealBottom - 1)),
        BevelShadow,
        BevelHighlight
      );
      DrawBevelRect(
        RenderCanvas,
        TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft + 1, RealTop + 1, RealRight - 2, RealBottom - 2)),
        BevelDarkShadow,
        BevelFace
      );
    end;

    { Place navigation buttons               }
    { size and place the Today button first. }
    with TVpDayViewOpener(FDayView) do begin
      dvTodayBtn.Height := trunc(RealColHeadHeight div 2);
      if DrawingStyle = dsFlat then begin
        dvTodayBtn.Left := 1;
        dvTodayBtn.Top := 1;
        dvTodayBtn.Width := RealRowHeadWidth + 1;
      end else begin
        dvTodayBtn.Left := 2;
        dvTodayBtn.Top := 2;
        dvTodayBtn.Width := RealRowHeadWidth;
      end;
      { size and place the WeekDown button }
      dvWeekDownBtn.Height := dvTodayBtn.Height;
      dvWeekDownBtn.Width := trunc(RealRowHeadWidth * 0.25) + 2;
      dvWeekDownBtn.Left := dvTodayBtn.Left;
      dvWeekDownBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;
      { size and place the DayDown button }
      dvDayDownBtn.Height := dvTodayBtn.Height;
      dvDayDownBtn.Width := dvWeekDownBtn.Width - 4;
      dvDayDownBtn.Left := dvWeekDownBtn.Left + dvWeekDownBtn.Width;
      dvDayDownBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;
      { size and place the DayUp button }
      dvDayUpBtn.Height := dvTodayBtn.Height;
      dvDayUpBtn.Width := dvWeekDownBtn.Width - 4;
      dvDayUpBtn.Left := dvDayDownBtn.Left + dvDayDownBtn.Width;
      dvDayUpBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;
      { size and place the WeekUp button }
      dvWeekUpBtn.Height := dvTodayBtn.Height;
      dvWeekUpBtn.Width := dvTodayBtn.Width - dvWeekDownBtn.Width - dvDayDownBtn.Width - dvDayUpBtn.Width;
      dvWeekUpBtn.Left := dvDayUpBtn.Left + dvDayUpBtn.Width;
      dvWeekUpBtn.Top := dvTodayBtn.Top + dvTodayBtn.Height;
    end;

    { Reinstate RenderCanvas settings }
    RestorePenBrush;

  finally
    SelectClipRgn(RenderCanvas.Handle, 0);
    DeleteObject(Rgn);
  end;
end;

procedure TVpDayViewPainter.ScaleIcons(EventRect: TRect);
var
  h: Integer;
begin
  h := EventRect.Bottom - EventRect.Top - 2;

  if (dvBmpAlarm.Height > h) and (dvBmpAlarm.Height * dvBmpAlarm.Width <> 0)
  then begin
    AlarmW := Trunc((h / dvBmpAlarm.Height) * dvBmpAlarm.Width);
    AlarmH := h;
  end;

  if (dvBmpRecurring.Height > h) and (dvBmpRecurring.Height * dvBmpRecurring.Width <> 0)
  then begin
    RecurringW := Trunc((h / dvBmpRecurring.Height) * dvBmpRecurring.Width);
    RecurringH := h;
  end;

  if (dvBmpCategory.Height > h) and (dvBmpCategory.Height * dvBmpCategory.Width <> 0)
  then begin
    CategoryW := Trunc((h / dvBmpCategory.Height) *  dvBmpCategory.Width);
    CategoryH := h;
  end;

  if (dvBmpCustom.Height > h) and (dvBmpCustom.Height * dvBmpCustom.Width <> 0)
  then begin
    CustomW := Trunc((h / dvBmpCustom.Height) * dvBmpCustom.Width);
    CustomH := h;
  end;
end;

procedure TVpDayViewPainter.SetMeasurements;
begin
  inherited;
  TVpDayViewOpener(FDayView).dvCalcColHeadHeight(Scale);
end;


end.
