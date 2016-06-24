{$I vp.inc}

unit VpWeekViewPainter;

interface

uses
  SysUtils, LCLType, LCLIntf, Types,
  Classes, Graphics, VpConst, VPBase, VpData, VpBasePainter, VpWeekView;

type
  TVpWeekViewPainter = class(TVpBasePainter)
  private
    FWeekView: TVpWeekView;
    // local parameters of the old TVpWeekView method
    HeadRect: TRect;
    SaveBrushColor: TColor;
    SavePenStyle: TPenStyle;
    SavePenColor: TColor;
    DayRectHeight: Integer;
    StrLn: Integer;
    StartDate: TDateTime;
    ADEventsRect: TRect;
    DotDotDotColor: TColor;
    BevelHighlightColor: TColor;
    BevelShadowColor: TColor;
    BevelDarkShadow: TColor;
    BevelButtonFace: TColor;
    RealLineColor: TColor;
    RealDayHeadAttrColor: TColor;
    RealColor: TColor;
    RealHeadAttrColor: TColor;
    ADBackgroundColor: TColor;
    ADEventBackgroundColor: TColor;
    ADEventBorderColor: TColor;

  protected
    procedure Clear;
    function DrawAllDayEvents(ADate: TDateTime; DayRect: TRect; var EAIndex: Integer): Boolean;
    procedure DrawBorders;
    procedure DrawDay(AIndex: Integer; var DayRect: TRect; var EAIndex: Integer);
    procedure DrawDays;
    procedure DrawHeader;
    procedure InitColors;
    procedure SetMeasurements; override;

  public
    constructor Create(AWeekView: TVpWeekView; ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AUseGran: TVpGranularity; ADisplayOnly: Boolean); override;
  end;


implementation

uses
  VpCanvasUtils, VpMisc, VpSR;

type
  TVpWeekViewOpener = class(TVpWeekView);

constructor TVpWeekViewPainter.Create(AWeekView: TVpWeekView;
  ARenderCanvas: TCanvas);
begin
  inherited Create(ARenderCanvas);
  FWeekView := AWeekView;
end;

procedure TVpWeekViewPainter.Clear;
begin
  RenderCanvas.Brush.Color := RealColor;
  RenderCanvas.FillRect(RenderIn);
end;

function TVpWeekViewPainter.DrawAllDayEvents(ADate: TDateTime; DayRect: TRect;
  var EAIndex: Integer): Boolean;
var
  ADEventsList: TList;
  TempList: TList;
  I, J, K: Integer;
  Event: TVpEvent;
  ADEventRect: TRect;
  StartsBeforeRange: Boolean;
  MaxADEvents: Integer;
  Skip: Boolean;
  ADTextHeight: Integer;
  EventStr: string;
begin
  Result := False;
  { initialize the All Day Events area... }
  ADEventsRect := DayRect;

  if (FWeekView.DataStore = nil) or (FWeekView.DataStore.Resource = nil) then
    Exit;

  { Collect all of the events for this range and determine the maximum     }
  { number of all day events for the range of days covered by the control. }
  MaxADEvents := 0;

  ADEventsList := TList.Create;
  try
    TempList := TList.Create;
    try
      { get the all day events for the day specified by ADate + I }
      FWeekView.DataStore.Resource.Schedule.AllDayEventsByDate(ADate, TempList);

      { Iterate through these events and place them in ADEventsList }
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
    finally
      TempList.Free;
    end;

    if MaxADEvents > 0 then begin
      { Set attributes }
      RenderCanvas.Brush.Color := ADBackgroundColor;
      RenderCanvas.Font.Assign(FWeekView.AllDayEventAttributes.Font);

      { Measure the AllDayEvent TextHeight }
      ADTextHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin + TextMargin div 2;

      { Build the AllDayEvent rect based on the value of MaxADEvents }
      if AdEventsRect.Top + (MaxADEvents * ADTextHeight) + TextMargin * 2 > DayRect.Bottom
      then
        ADeventsrect.Bottom := DayRect.Bottom
      else
        ADEventsRect.Bottom := AdEventsRect.Top + (MaxADEvents * ADTextHeight) + TextMargin * 2;

      { Clear the AllDayEvents area }
      TpsFillRect(RenderCanvas, Angle, RenderIn, ADEventsRect);

      StartsBeforeRange  := false;
      { Cycle through the all day events and draw them appropriately }
      for I := 0 to pred(ADEventsList.Count) do begin
        Event := ADEventsList[I];

        { set the top of the event's rect }
        AdEventRect.Top := ADEventsRect.Top + TextMargin + I * ADTextHeight;

        if ADEventsRect.Top + TextMargin + ((I + 1)  * ADTextHeight) - TextMargin > DayRect.Bottom
        then begin
          RenderCanvas.Brush.Color := DotDotDotColor;
          { draw dot dot dot }
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 20,  DayRect.Bottom - 7, DayRect.Right - 17,  DayRect.Bottom - 4));
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 13,  DayRect.Bottom - 7, DayRect.Right - 10,  DayRect.Bottom - 4));
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 6,  DayRect.Bottom - 7, DayRect.Right -  3,  DayRect.Bottom - 4));
          break;
        end;

        { see if the event began before the start of the range }
        if (Event.StartTime < trunc(RenderDate)) then
          StartsBeforeRange := true;

        AdEventRect.Bottom := ADEventRect.Top + ADTextHeight;
        AdEventRect.Left := AdEventsRect.Left + (TextMargin div 2);
        AdEventRect.Right := DayRect.Right;

        if (StartsBeforeRange) then
          EventStr := '>> '
        else
          EventStr := '';

        EventStr := EventStr + Event.Description;

        RenderCanvas.Brush.Color := ADEventBackgroundColor;
        RenderCanvas.Pen.Color := ADEventBorderColor;
        TPSRectangle(RenderCanvas, Angle, RenderIn,
          ADEventRect.Left + TextMargin,
          ADEventRect.Top + TextMargin div 2,
          ADEventRect.Right - TextMargin,
          ADEventRect.Top + ADTextHeight + TextMargin div 2
        );
        TPSTextOut(RenderCanvas,Angle, RenderIn,
          AdEventRect.Left + TextMargin * 2 + TextMargin div 2,
          AdEventRect.Top + TextMargin,
          EventStr
        );
        Result := True;
        TVpWeekViewOpener(FWeekView).wvEventArray[EAIndex].Rec := Rect(
          ADEventRect.Left + TextMargin,
          ADEventRect.Top + TextMargin,
          ADEventRect.Right - TextMargin,
          ADEventRect.Bottom
        );
        TVpWeekViewOpener(FWeekView).wvEventArray[EAIndex].Event := Event;
        Inc(EAIndex);
      end; { for I := 0 to pred(ADEventsList.Count) do ... }

    end;   { if MaxADEvents > 0 }

  finally
    ADEventsList.Free;
  end;
end;

procedure TVpWeekViewPainter.DrawBorders;
var
  shadow, bright: TColor;
begin
  if FWeekView.DrawingStyle = dsFlat then begin
    {
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft, RealTop, RealRight - 1, RealBottom - 1)),
      BevelShadowColor,
      BevelShadowColor
    );
    }
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft + 1, RealTop + 1, RealRight - 2, RealBottom - 2)),
      BevelShadowColor,
      BevelShadowColor    // use the same color --> no bevel in flat mode!
    );
  end else
  if FWeekView.DrawingStyle = ds3d then begin
    { draw a 3d bevel }
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft, RealTop, RealRight - 1, RealBottom - 1)),
      BevelShadowColor,
      BevelShadowColor
    );
    DrawBevelRect(RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, Rect(RealLeft + 1, RealTop + 1, RealRight - 2, RealBottom - 2)),
      BevelDarkShadow,
      BevelButtonFace
    );
  end;
end;

procedure TVpWeekViewPainter.DrawDay(AIndex: Integer; var DayRect: TRect;
  var EAIndex: Integer);
var
  TextRect: TRect;
  DayStr: String;
  SL: Integer;
  tmpRect: TRect;
  J: Integer;
  EventList: TList;
  TodayStartTime: Double;
  TodayEndTime: Double;
begin
  { draw day head}
  RenderCanvas.Font.Assign(FWeekView.DayHeadAttributes.Font);
  RenderCanvas.Brush.Color := RealDayHeadAttrColor;
  TextRect := DayRect;
  TextRect.Bottom := DayRect.Top + TVpWeekViewOpener(FWeekView).wvDayHeadHeight;
  TPSFillRect(RenderCanvas, Angle, RenderIn, TextRect);
  if FWeekView.DayHeadAttributes.Bordered then
    TPSRectangle(RenderCanvas, Angle, RenderIn, TextRect);

  { Fix Header String }
  DayStr := FormatDateTime(FWeekView.DayHeadAttributes.DateFormat, StartDate + AIndex);
  {$IFDEF LCL}
  {$IF FPC_FULLVERSION < 30000}DayStr := SysToUTF8(DayStr); {$ENDIF}
  {$ENDIF}
  SL := RenderCanvas.TextWidth(DayStr);
  if SL > WidthOf(TextRect) then
    DayStr := GetDisplayString(RenderCanvas, DayStr, 0, WidthOf(TextRect) - TextMargin);
  SL := RenderCanvas.TextWidth(DayStr);
  TextRect.Left := TextRect.Right - SL - TextMargin;
  TPSTextOut(
    RenderCanvas,
    Angle,
    RenderIn,
    TextRect.Left,
    TextRect.Top + TextMargin - 1,
    DayStr
  );

  if (FWeekView.DataStore <> nil) and (FWeekView.DataStore.Resource <> nil) and
     (FWeekView.DataStore.Resource.Schedule.EventCountByDay(StartDate + AIndex) > 0) and
     (HeightOf(DayRect) >= TextMargin * 2 + TVpWeekViewOpener(FWeekView).wvDayHeadHeight)
  then begin
    { events exist for this day }
    EventList := TList.Create;
    try
      { populate the eventlist with events for this day }
      FWeekView.DataStore.Resource.Schedule.EventsByDate(StartDate + AIndex, EventList);
      { initialize TextRect for this day }
      TextRect := DayRect;
      TextRect.Top := DayRect.Top + TVpWeekViewOpener(FWeekView).wvDayHeadHeight;
      TextRect.Bottom := TextRect.Top + TVpWeekViewOpener(FWeekView).wvRowHeight;

      { Handle All Day Events }
      tmpRect := TextRect;
      tmpRect.Bottom := DayRect.Bottom;
      if DrawAllDayEvents(StartDate + AIndex, tmpRect, EAIndex) then
      begin
        TextRect.Bottom := TextRect.Bottom + ADEventsRect.Bottom - TextRect.Top;
        TextRect.Top := ADEventsRect.Bottom;
      end;

      { Discard AllDayEvents, because they are drawn above. }
      for J := pred(EventList.Count) downto 0 do
        if TVpEvent (EventList[J]).AllDayEvent then
          EventList.Delete(J);

      { iterate the events, painting them one by one }
      for J := 0 to pred(EventList.Count) do begin
        { if the TextRect extends below the available space then draw a   }
        { dot dot dot to indicate there are more events than can be drawn }
        { in the available space }
        if TextRect.Bottom - TextMargin > DayRect.Bottom then begin
          RenderCanvas.Brush.Color := DotDotDotColor;
          { draw dot dot dot }
          tmpRect := Rect(DayRect.Right, DayRect.Bottom, DayRect.Right + 3, DayRect.Bottom + 3);
          OffsetRect(tmpRect, -20, -7);
          TPSFillRect(RenderCanvas, Angle, RenderIn, tmpRect);
          OffsetRect(tmpRect, 7, 0);
          TPSFillRect(RenderCanvas, Angle, RenderIn, tmpRect);
          OffsetRect(tmpRect, 7, 0);
          TPSFillRect(RenderCanvas, Angle, RenderIn, tmpRect);
          {
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 20,  DayRect.Bottom - 7, DayRect.Right - 17,  DayRect.Bottom - 4)
          );
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 13,  DayRect.Bottom - 7, DayRect.Right - 10,  DayRect.Bottom - 4)
          );
          TPSFillRect(RenderCanvas, Angle, RenderIn,
            Rect(DayRect.Right - 6,  DayRect.Bottom - 7, DayRect.Right -  3,  DayRect.Bottom - 4)
          );}
          break;
        end;

        { format the display text }
        DayStr := '';
        TodayStartTime := TVpEvent(EventList.List^[j]).StartTime;
        TodayEndTime := TVpEvent(EventList.List^[j]).EndTime;
        if trunc(TodayStartTime) < trunc(StartDate + AIndex) then //First Event
          TodayStartTime := 0;
        if trunc(TodayEndTime) > trunc(StartDate + AIndex) then //Last Event
          TodayEndTime := 0.9999;
        if FWeekView.ShowEventTime then
        begin
          if FWeekView.TimeFormat = tf24Hour then                             // wp: why different times for 24 and 12 hours ?
            DayStr := FormatDateTime('hh:nn', TodayStartTime) + ' - ' +
                      FormatDateTime('hh:nn', TodayEndTime) + ': '
          else
            DayStr := FormatDateTime('hh:nn AM/PM', TVpEvent(EventList.List^[J]).StartTime) + ' - ' +
                      FormatDateTime('hh:nn AM/PM', TVpEvent(EventList.List^[J]).EndTime) + ': ';
        end;
        if DayStr = '' then
          DayStr := TVpEvent(EventList.List^[J]).Description
        else
          DayStr := DayStr + ' ' + TVpEvent(EventList.List^[J]).Description;

        { set the event font }
        RenderCanvas.Font.Assign(FWeekView.EventFont);
        RenderCanvas.Brush.Color := RealColor;

        StrLn := RenderCanvas.TextWidth(DayStr);
        if (StrLn > WidthOf(TextRect) - TextMargin) then
          DayStr := GetDisplayString(RenderCanvas, DayStr, 0, WidthOf(TextRect) - TextMargin * 2);

        { write the event text }
        TPSTextOut(RenderCanvas, Angle, RenderIn,
          TextRect.Left + TextMargin, TextRect.Top + TextMargin div 2,
          DayStr
        );

        { update the EventArray }
        TVpWeekViewOpener(FWeekView).wvEventArray[EAIndex].Rec := TextRect;
        TVpWeekViewOpener(FWeekView).wvEventArray[EAIndex].Event := TVpEvent(EventList.List^[J]);
        Inc(EAIndex);

        TextRect.Top := TextRect.Bottom;
        TextRect.Bottom := TextRect.Top + TVpWeekViewOpener(FWeekView).wvRowHeight;
      end; { for loop }
    finally
      EventList.Free;
    end;
  end;

  { Draw focus rect if this is the current day }

  if (not DisplayOnly) and (StartDate + AIndex = Trunc(FWeekView.Date)) and FWeekView.Focused
  then begin
    tmpRect := DayRect;
    InflateRect(tmpRect, -2, -2);
    tmpRect.Top := tmpRect.Top + TVpWeekViewOpener(FWeekView).wvDayHeadHeight;
    TPSDrawFocusRect(RenderCanvas, Angle, RenderIn, tmpRect);
    {
    TPSDrawFocusRect(RenderCanvas, Angle, RenderIn, Rect(
      DayRect.Left + 2,
      DayRect.Top + TVpWeekViewOpener(FWeekView).wvDayHeadHeight + 2,
      DayRect.Right - 2,
      DayRect.Bottom - 2
    ));
    }
  end;

  { update WeekdayArray }
  with TVpWeekViewOpener(FWeekView).wvWeekdayArray[AIndex] do begin
    Rec := DayRect;
    Day := StartDate + AIndex;
  end;

  { adjust the DayRect for the next day }
  if (AIndex = 2) then begin
    { move the dayrect to the top of the next column }
    if FWeekView.DrawingStyle = ds3D then begin
      DayRect.TopLeft := Point(
        RealLeft + (RealRight - RealLeft) div 2,
        RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 3
      );
      DayRect.BottomRight := Point(
        RealRight - 2,
        RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + DayRectHeight
      );
    end
    else begin
      DayRect.TopLeft := Point(
        RealLeft + (RealRight - RealLeft) div 2,
        RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 2
      );
      DayRect.BottomRight := Point(
        RealRight - 1,
        RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + DayRectHeight
      );
    end;
  end
  else
  if (AIndex = 4 {Friday}) then begin
    { shrink DayRect for weekend days }
    DayRectHeight := DayRectHeight div 2;
    DayRect.Top := DayRect.Bottom;
    DayRect.Bottom := DayRect.Top + DayRectHeight;
  end
  else begin
    DayRect.Top := DayRect.Bottom;
    DayRect.Bottom := DayRect.Top + DayRectHeight;
  end;
end;

procedure TVpWeekViewPainter.DrawDays;
var
  DayRect: TRect;
  EAIndex: Integer;
  I: Integer;
  {
  TextRect: TRect;
  I, J, SL: Integer;
  DayStr: string;
  EventList: TList;
  TodayStartTime: Double;
  TodayEndTime: Double;
  }
begin
  { initialize WeekdayArray }
  with TVpWeekViewOpener(FWeekView) do
    for I := 0 to pred(Length(wvWeekdayArray)) do begin
      wvWeekdayArray[I].Rec.TopLeft := Point(-1, -1);
      wvWeekdayArray[I].Rec.BottomRight := Point(-1, -1);
      wvWeekdayArray[I].Day := 0;
    end;

  { initialize Event Array }
  EAIndex := 0;
  with TVpWeekViewOpener(FWeekView) do
    for I := 0 to pred(Length(wvEventArray)) do begin
      wvEventArray[I].Rec.TopLeft := Point(-1, -1);
      wvEventArray[I].Rec.BottomRight := Point(-1, -1);
      wvEventArray[I].Event := nil;
    end;

  RenderCanvas.Pen.Color := RealLineColor;
  RenderCanvas.Pen.Style := psSolid;

  { build the first dayrect }
  DayRectHeight := (RealBottom - RealTop - TVpWeekViewOpener(FWeekView).wvHeaderHeight) div 3;
  if FWeekView.DrawingStyle = ds3D then
    DayRect.TopLeft := Point(RealLeft + 1, RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 3)
  else
    DayRect.TopLeft := Point(RealLeft + 1, RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 2);
  DayRect.BottomRight := Point(
    RealLeft + (RealRight - RealLeft) div 2 + 1,
    RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + DayRectHeight
  );

  { draw the day frames }
  for I := 0 to 6 do
    DrawDay(I, DayRect, EAIndex);

  { Draw the center vertical line }
  RenderCanvas.Pen.Color := RealLineColor;
  TPSMoveTo(RenderCanvas, Angle, RenderIn,
    RealLeft + (RealRight - RealLeft) div 2, RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 2
  );
  TPSLineTo(RenderCanvas, Angle, RenderIn,
    RealLeft + (RealRight - RealLeft) div 2, RealBottom - 1
  );
end;

procedure TVpWeekViewPainter.DrawHeader;
var
  HeadTextRect: TRect;
  HeadStr: string;
  HeadStrLen : Integer;
begin
  RenderCanvas.Brush.Color := RealHeadAttrColor;
  RenderCanvas.Font.Assign(TFont(FWeekView.HeadAttributes.Font));
  { draw the header cell and borders }
  if FWeekView.DrawingStyle = dsFlat then begin
    { draw an outer and inner bevel }
    HeadRect := Rect(RealLeft, RealTop, RealRight, RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight + 2);
    TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);
    { wp: above lines replace the next ones - no bevel in flat style!
    HeadRect.Left := RealLeft + 1;
    HeadRect.Top := RealTop + 1;
    HeadRect.Right := RealRight - 1;
    HeadRect.Bottom := HeadRect.Top + wvHeaderHeight;
    TPSFillRect (RenderCanvas, Angle, RenderIn, HeadRect);
    DrawBevelRect (RenderCanvas,
                   TPSRotateRectangle (Angle, RenderIn, HeadRect),
                   BevelHighlightColor, BevelShadowColor);
    }
  end else
  if FWeekView.DrawingStyle = ds3d then begin
    { draw a 3d bevel }
    HeadRect.Left := RealLeft + 2;
    HeadRect.Top := RealTop + 2;
    HeadRect.Right := RealRight - 3;
    HeadRect.Bottom := RealTop + TVpWeekViewOpener(FWeekView).wvHeaderHeight;
    TPSFillRect(RenderCanvas, Angle, RenderIn, HeadRect);
    DrawBevelRect(
      RenderCanvas,
      TPSRotateRectangle(Angle, RenderIn, HeadRect),
      BevelHighlightColor,
      BevelDarkShadow
    );
  end else begin
    HeadRect.Left := RealLeft + 1;
    HeadRect.Top := RealTop + 1;
    HeadRect.Right := RealRight - 1;
    HeadRect.Bottom := HeadRect.Top + TVpWeekViewOpener(FWeekView).wvHeaderHeight;
  end;

  { build header caption }
  HeadStr := HeadStr + Format('%s %s (%s %d)', [
    RSWeekOf, FormatDateTime(FWeekView.DateLabelFormat, StartDate), RSCalendarWeekAbbr, GetWeekOfYear(StartDate)
  ]);
//    HeadStr := HeadStr + RSWeekof + ' ' + FormatDateTime(DateLabelFormat, StartDate)+' (KW'+IntToStr(GetWeekOfYear(StartDate))+')';
  { draw the text }
  if DisplayOnly and (RenderCanvas.TextWidth(HeadStr) >= RenderIn.Right - RenderIn.Left)
  then
    HeadTextRect.TopLeft:= Point(RealLeft + TextMargin * 2, HeadRect.Top)
  else
  if DisplayOnly then
    HeadTextRect.TopLeft := Point(
      RealLeft + (RealRight - RealLeft - RenderCanvas.TextWidth(HeadStr)) div 2,
      HeadRect.Top
    )
  else
    HeadTextRect.TopLeft := Point(
      RealLeft + Trunc(TVpWeekViewOpener(FWeekView).wvHeaderHeight * 0.8) * 2 + TextMargin * 2,
      HeadRect.Top
    );
  HeadTextRect.BottomRight := HeadRect.BottomRight;
  { Fix Header String }
  HeadStrLen := RenderCanvas.TextWidth(HeadStr);
  if HeadStrLen > HeadTextRect.Right - HeadTextRect.Left - TextMargin then
  begin
    HeadStr := GetDisplayString(RenderCanvas, HeadStr, 0,
      HeadTextRect.Right - HeadTextRect.Left - TextMargin );
  end;
  { position the spinner }
  with TVpWeekViewOpener(FWeekView) do begin
    wvSpinButtons.Height := Trunc(wvHeaderHeight * 0.8);
    wvSpinButtons.Width := wvSpinButtons.Height * 2;
    wvSpinButtons.Left := TextMargin;
    wvSpinButtons.Top := (wvHeaderHeight - wvSpinButtons.Height) div 2 + 2;
  end;
  TPSTextOut(RenderCanvas, Angle, RenderIn,
    HeadTextRect.Left + TextMargin,
    HeadTextRect.Top + TextMargin,
    HeadStr
  );
end;

procedure TVpWeekViewPainter.InitColors;
begin
  if DisplayOnly then begin
    BevelHighlightColor := clBlack;
    BevelShadowColor := clBlack;
    BevelDarkShadow := clBlack;
    BevelButtonFace := clBlack;
    RealLineColor := clBlack;
    RealColor := clWhite;
    RealDayHeadAttrColor := clSilver;
    RealHeadAttrColor := clSilver;
    ADBackgroundColor := clWhite;
    ADEventBackgroundColor := clWhite;
    ADEventBorderColor := clSilver;
  end else begin
    BevelHighlightColor := clBtnHighlight;
    BevelShadowColor := clBtnShadow;
    BevelDarkShadow := cl3DDkShadow;
    BevelButtonFace := clBtnFace;
    RealLineColor := FWeekView.LineColor;
    RealColor := FWeekView.Color;
    RealDayHeadAttrColor := FWeekView.DayHeadAttributes.Color;
    RealHeadAttrColor := FWeekView.HeadAttributes.Color;
    ADBackgroundColor := FWeekView.AllDayEventAttributes.BackgroundColor;
    ADEventBackgroundColor := FWeekView.AllDayEventAttributes.EventBackgroundColor;
    ADEventBorderColor := FWeekView.AllDayEventAttributes.EventBorderColor;
  end;
  DotDotDotColor := clBlack;
end;

procedure TVpWeekViewPainter.RenderToCanvas(ARenderIn: TRect;
  AAngle: TVpRotationAngle; AScale: Extended; ARenderDate: TDateTime;
  AStartLine, AStopLine: Integer; AUseGran: TVpGranularity; ADisplayOnly: Boolean);
begin
  inherited;

  InitColors;
  SavePenBrush;
  InitPenBrush;

  Rgn := CreateRectRgn(RenderIn.Left, RenderIn.Top, RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn(RenderCanvas.Handle, Rgn);

    { clear client area }
    Clear;

    { measure the row heights }
    SetMeasurements;

    { draw header }
    DrawHeader;

    { draw days }
    DrawDays;

    { draw the borders }
    DrawBorders;

  finally
    { reinstate canvas settings}
    SelectClipRgn(RenderCanvas.Handle, 0);
    DeleteObject(Rgn);
  end;

  RestorePenBrush;
end;

procedure TVpWeekViewPainter.SetMeasurements;
begin
  inherited;

  with TVpWeekViewOpener(FWeekView) do
    if RenderDate = 0 then
      StartDate := GetStartOfWeek(wvStartDate, WeekStartsOn)
    else
      StartDate := GetStartOfWeek(RenderDate, WeekStartsOn);

  RenderCanvas.Font.Assign(FWeekView.DayHeadAttributes.Font);
  with TVpWeekViewOpener(FWeekView) do
    wvDayHeadHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin + 2 ;
  RenderCanvas.Font.Assign(FWeekView.EventFont);
  with TVpWeekViewOpener(FWeekView) do
    wvRowHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin div 2;
  RenderCanvas.Font.Assign(TFont(FWeekView.HeadAttributes.Font));
  with TVpWeekViewOpener(FWeekView) do
    wvHeaderHeight := RenderCanvas.TextHeight(VpProductName) + TextMargin * 2;
end;

end.
