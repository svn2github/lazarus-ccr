{ TCalendarLite is a lightweight calendar component, a TGraphiccontrol
  descendant, which is consequently not dependent on any widgetset.
  It is not a fixed-size component, as are most calendars, but will align
  and resize as needed

  Originator    : H Page-Clark, 2013/2016
  Contributions : Ariel Rodriguez, 2013
                  Werner Pamler, 2013/2016
                  John Greetham, 2016

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit CalendarLite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  ExtCtrls, Menus;

const
  LastCol = 7;

type
  TCalendarLite = class;

  TColArray = array[1..LastCol] of word;
  TRowArray = array of word;

  TArrowDirection = (adLeft, adRight);
  TArrowhead = (ahSingle, ahDouble);
  TArrowPoints = array[1..3] of TPoint;

  TDayOfWeek = (dowSunday=1, dowMonday=2, dowTuesday=3, dowWednesday=4,
    dowThursday=5, dowFriday=6, dowSaturday=7);
  TDaysOfWeek = set of TDayOfWeek;

  TDisplayText = (dtToday=0, dtTodayFormat=1, dtHolidaysDuring=2,
    dtNoHolidaysDuring=3, dtTodayFormatLong=4, dtCaptionFormat=5);

  THolidays = DWord;
  TGetHolidaysEvent = procedure (Sender: TObject; AMonth, AYear: Integer;
    var Holidays: THolidays) of object;

  TCalCellState = (csSelectedDay, csToday, csOtherMonth);
  TCalCellStates = set of TCalCellState;

  TCalPrepareCanvasEvent = procedure (Sender: TObject; ACanvas: TCanvas;
    AYear, AMonth, ADay: Word; AState: TCalCellStates) of object;

  TCalDrawCellEvent = procedure (Sender: TObject; ACanvas: TCanvas;
    AYear, AMonth, ADay: Word; AState: TCalCellStates; var ARect: TRect;
    var AContinueDrawing: Boolean) of object;

  TCalGetDayTextEvent = procedure (Sender: TObject; AYear, AMonth, ADay: Word;
    var AText: String) of object;

  TCalOption = (coBoldDayNames, coBoldHolidays, coBoldToday, coBoldTopRow,
                coBoldWeekend, coDayLine, coShowBorder, coShowHolidays,
                coShowTodayFrame, coShowTodayName, coShowTodayRow,
                coShowWeekend, coUseTopRowColors);
  TCalOptions = set of TCalOption;

  TCalDateArray = array of TDate;

  TCalSelMode = (smFirstSingle, smNextSingle, smFirstRange, smNextRange,
    smFirstWeek, smNextWeek, smNextWeekRange);

  TLanguage = (lgEnglish, lgFrench, lgGerman, lgHebrew, lgSpanish, lgItalian,
               lgPolish, lgFinnish, lgGreek, lgCustom);


  { TCalDateList }

  TCalDateList = class
  private
    FList: TFPList;
    function GetCount: Integer;
    function GetDate(AIndex: Integer): TDate;
    procedure SetDate(AIndex: Integer; AValue: TDate);
  protected
    procedure Sort;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDate(ADate: TDate);
    function AsArray: TCalDateArray;
    procedure Clear;
    procedure DeleteDate(ADate: TDate);
    function IndexOfDate(ADate: TDate): Integer;
    procedure Insert(AIndex: Integer; ADate: TDate);
    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: TDate read GetDate write SetDate; default;
  end;


  { TCalDrawer }

  TCalDrawer = class
  private
    FBoundsRect: TRect;
    FCanvas: TCanvas;
    FCellSize: TSize;
    FColPositions: TColArray;
    FOwner: TCalendarLite;
    FRowPositions: TRowArray;
    FStartDate: TDateTime;
    FThisDay: word;
    FThisMonth: word;
    FThisYear: word;
    FTextStyle: TTextStyle;
    procedure CalcSettings;
    procedure DrawArrow(ARect: TRect; AHead: TArrowhead; ADirec: TArrowDirection);
    procedure DrawDayCells;
    procedure DrawDayLabels;
    procedure DrawTodayRow;
    procedure DrawTopRow;
    function  GetCellAt(aPoint: TPoint): TSize;
    function  GetCellAtColRow(aCol, aRow: integer): TRect;
    function  GetColRowPosition(aCol, aRow: integer): TSize;
    function  GetDateOfCell(ACell: TSize): TDate;
    function  GetLeftColIndex: Integer;
    procedure GetMonthYearRects(var AMonthRect, AYearRect: TRect);
    function  GetRightColIndex: Integer;
    procedure GotoDay(ADate: word);
    procedure GotoMonth(AMonth: word);
    procedure GotoToday;
    procedure GotoYear(AYear: word);
    procedure LeftClick(APoint: TPoint; Shift: TShiftState);
    procedure RightClick;
  public
    constructor Create(ACanvas: TCanvas);
    procedure Draw;
  end;


  { TCalColors }

  TCalColors = class(TPersistent)
  private
    FOwner: TCalendarLite;
    FColors: Array[0..12] of TColor;
    function GetColor(AIndex: Integer): TColor;
    procedure SetColor(AIndex: Integer; AValue: TColor);
  public
    constructor Create(AOwner: TCalendarLite);
  published
    property ArrowBorderColor: TColor index 0 read GetColor write SetColor default clSilver;
    property ArrowColor: TColor index 1 read GetColor write SetColor default clSilver;
    property BackgroundColor: TColor index 2 read GetColor write SetColor default clWhite;
    property BorderColor: TColor index 3 read GetColor write SetColor default clSilver;
    property DaylineColor: TColor index 4 read GetColor write SetColor default clSilver;
    property HolidayColor: TColor index 5 read GetColor write SetColor default clRed;
    property PastMonthColor: TColor index 6 read GetColor write SetColor default clSilver;
    property SelectedDateColor: TColor index 7 read GetColor write SetColor default clMoneyGreen;
    property TextColor: TColor index 8 read GetColor write SetColor default clBlack;
    property TodayFrameColor: TColor index 9 read GetColor write SetColor default clLime;
    property TopRowColor: TColor index 10 read GetColor write SetColor default clHighlight;
    property TopRowTextColor: TColor index 11 read GetColor write SetColor default clHighlightText;
    property WeekendColor: TColor index 12 read GetColor write SetColor default clRed;
  end;


  { TCalendarLite }

  TCalendarLite = class(TCustomControl)
  private
    FCalDrawer: TCalDrawer;
    FColors: TCalColors;
    FDate: TDateTime;
    FCustomDayNames: string;
    FCustomDisplayTexts: String;
    FCustomMonthNames: string;
    FDisplayTexts: array[TDisplayText] of string;
    FOnDateChange: TNotifyEvent;
    FOnMonthChange: TNotifyEvent;
    FOnGetDayText: TCalGetDayTextEvent;
    FOnDrawCell: TCalDrawCellEvent;
    FOnGetHolidays: TGetHolidaysEvent;
    FOnHint: TCalGetDayTextEvent;
    FOnPrepareCanvas: TCalPrepareCanvasEvent;
    FOptions: TCalOptions;
    FPopupMenu: TPopupMenu;
    FStartingDayOfWeek: TDayOfWeek;
    FWeekendDays: TDaysOfWeek;
    FPrevMouseDate: TDate;
    FPrevDate: TDate;
    FSavedHint: String;
    FMultiSelect: Boolean;
    FSelDates: TCalDateList;
    FClickShift: TShiftState;
    FClickPoint: TPoint;
    FClickButton: TMouseButton;
    FLanguage: TLanguage;
    FDblClickTimer: TTimer;
    FFormatSettings: TFormatSettings;
    function GetDayNames: String;
    function GetDisplayText(aTextIndex: TDisplayText): String;
    function GetDisplayTexts: String;
    function GetMonthNames: String;
    procedure HolidayMenuItemClicked(Sender: TObject);
    procedure MonthMenuItemClicked(Sender: TObject);
    procedure PopulateHolidayPopupMenu;
    procedure PopulateMonthPopupMenu;
    procedure PopulateYearPopupMenu;
    procedure SetCustomDayNames(const AValue: String);
    procedure SetCustomDisplayTexts(const AValue: String);
    procedure SetCustomMonthNames(const AValue: String);
    procedure SetDate(AValue: TDateTime);
    procedure SetDefaultDayNames;
    procedure SetDefaultDisplayTexts;
    procedure SetDefaultMonthNames;
    procedure SetDisplayTexts(AValue: String);
    procedure SetLanguage(AValue: TLanguage);
    procedure SetMultiSelect(AValue: Boolean);
    procedure SetOptions(AValue: TCalOptions);
    procedure SetStartingDayOfWeek(AValue: TDayOfWeek);
    procedure SetWeekendDays(AValue: TDaysOfWeek);
    procedure TimerExpired(Sender: TObject);
    procedure YearMenuItemClicked(Sender: TObject);

  protected
    procedure ChangeDateTo(ADate: TDate; ASelMode: TCalSelMode);
    procedure DateChange; virtual;
    procedure DblClick; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure InternalClick;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MonthChange; virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    function SelMode(Shift: TShiftState): TCalSelMode;

    procedure Paint; override;
    procedure UseDayName(ADayOfWeek: TDayOfWeek; const AValue: String);
    procedure UseDayNames(const AValue: String);
    procedure UseDisplayTexts(const AValue: String);
    procedure UseMonthName(AMonth: Integer; const AValue: String);
    procedure UseMonthNames(const AValue: String);

    { Hints }
    procedure ShowHintWindow(APoint: TPoint; ADate: TDate);
    procedure HideHintWindow;

  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;

    function GetDayName(ADayOfWeek: TDayOfWeek): String;
    function GetMonthName(AMonth: Integer): String;

    procedure AddSelectedDate(ADate: TDate);
    procedure ClearSelectedDates;
    function IsSelected(ADate: TDate): Boolean;
    function SelectedDates: TCalDateArray;

  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Cursor;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property Left;
    property Name;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    // new properties
    property Colors: TCalColors read FColors write FColors;
    property Date: TDateTime read FDate write SetDate;
    property DayNames: String read FCustomDayNames write SetCustomDayNames;
    property DisplayTexts: String read GetDisplayTexts write SetCustomDisplayTexts;
    property MonthNames: String read FCustomMonthNames write SetCustomMonthNames;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect
      default false;
    property Options: TCalOptions read FOptions write SetOptions
      default [coShowTodayFrame, coBoldHolidays, coShowWeekend, coShowHolidays, coShowTodayRow];
    property StartingDayOfWeek: TDayOfWeek read FStartingDayOfWeek
      write SetStartingDayOfWeek default dowSunday;
    property WeekendDays: TDaysOfWeek read FWeekendDays
      write SetWeekendDays default [dowSunday];
    property Languages: TLanguage read FLanguage
      write SetLanguage default lgEnglish;

    // new event properties
    property OnDateChange: TNotifyEvent read FOnDateChange write FOnDateChange;
    property OnDrawCell: TCalDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnGetDayText: TCalGetDayTextEvent read FOnGetDayText write FOnGetDayText;
    property OnGetHolidays: TGetHolidaysEvent read FOnGetHolidays write FOnGetHolidays;
    property OnHint: TCalGetDayTextEvent read FOnHint write FOnHint;
    property OnMonthChange: TNotifyEvent read FOnMonthChange write FOnMonthChange;
    property OnPrepareCanvas: TCalPrepareCanvasEvent read FOnPrepareCanvas write FOnPrepareCanvas;
  end;

procedure ClearHolidays(var AHolidays: THolidays);
procedure AddHoliday(ADay: Integer; var AHolidays: THolidays);
function  IsHoliday(ADay: Integer; AHolidays: THolidays): Boolean;

procedure Register;


implementation

{$R calendarlite_icon.res}

uses
  LCLType, LazUTF8, dateutils, math;

resourcestring
  rsCalTodayIs = 'Today is %s';
  rsCalTodayFormat = 'mmm dd, yyyy';
  rsCalTodayFormatLong = 'dddd mmm dd, yyyy';
  rsCalCaptionFormat = 'mmmm yyyy';
  rsCalHolidaysIn = 'Holidays in %d';
  rsCalNoHolidaysIn = 'There are no holidays set for %d';

  rsCalJanuary = 'January|Jan';
  rsCalFebruary = 'February|Feb';
  rsCalMarch = 'March|Mar';
  rsCalApril = 'April|Apr';
  rsCalMay = 'May|May';
  rsCalJune = 'June|Jun';
  rsCalJuly = 'July|Jul';
  rsCalAugust = 'August|Aug';
  rsCalSeptember = 'September|Sp';
  rsCalOctober = 'October|Oct';
  rsCalNovember = 'November|Nov';
  rsCalDecember = 'December|Dec';

  rsCalSunday = 'Sunday|Sun';
  rsCalMonday = 'Monday|Mon';
  rsCalTuesday = 'Tuesday|Tue';
  rsCalWednesday = 'Wesnesday|Wed';
  rsCalThursday = 'Thursday|Thu';
  rsCalFriday = 'Friday|Fri';
  rsCalSaturday = 'Saturday|Sat';


const
  TopRow = 0;
  DayRow = 1;
  FirstDateRow = 2;
  LastDateRow = 7;
  TodayRow = 8;
  LastRow: word = 0;
  DefCalHeight = 160;
  DefCalWidth = 210;
  DefMinHeight = 120;
  DefMinWidth = 120;
  DefTextStyle: TTextStyle = (
    Alignment  : taCenter; Layout     : tlCenter;
    SingleLine : False;    Clipping   : True;
    ExpandTabs : False;    ShowPrefix : False;
    Wordbreak  : True;     Opaque     : False;
    SystemFont : False;    RightToLeft: False;
    EndEllipsis: False
  );

  // IMPORTANT NOTE: NO SPACES IN FRONT OF QUOTES !!!

  EnglishDays = 'Sunday|Sun,Monday|Mon,Tuesday|Tue,Wednesday|Wed,Thursday|Thu,Friday|Fri,Saturday|Sat';
  EnglishMonths = 'January|Jan,February|Feb,March|Mar,April|Apr,May|May,June|Jun,'+
    'July|Jul,August|Aug,September|Sep,October|Oct,November|Nov,December|Dec';
  EnglishTexts = 'Today is %s,"mmm dd"", ""yyyy",Holidays in %d,'+
    'There are no holidays set for %d,"dddd"", "" mmm dd"", ""yyyy",mmmm yyyy';

  HebrewDays = 'א,ב,ג,ד,ה,ו,ש';
  HebrewMonths = ('ינואר,פברואר,מרץ,אפריל,מאי,יוני,    יולי,אוגוסט,ספטמבר,אוקטובר,נובמבר,דצמבר');
  HebrewTexts = 'היום הוא,yyyy-mm-dd,במהלך החגים, אין חגים מוגדרים עבור';

  FrenchDays = 'dimanche|dim,lundi|lun,mardi|mar,mercredi|mer,jeudi|jeu,vendredi|ven,samedi|sam';
  FrenchMonths = 'janvier|janv.,février|févr.,mars|mars,avril|avr.,mai|mai,juin|juin,'+
    'juillet|juill.,août|août,septembre|sept.,octobre|oct.,novembre|nov.,décembre|déc.';
  FrenchTexts = 'Est aujourd''hui %s, dd/mm/yyyy, vacances pendant %d, '+
    'Il n''y a pas de jours fériés fixés pour %d, dddd dd/mm/yyyy, mmmm yyyy';

  GermanDays = 'Sonntag|So,Montag|Mo,Dienstag|Di,Mittwoch|Mi,Donnerstag|Do,Freitag|Fr,Samstag|Sa';
  GermanMonths = 'Januar|Jan.,Februar|Febr.,März|März,April|Apr.,Mai|Mai,Juni|Jun,'+
    'Juli|Jul,August|Aug.,September|Sept.,Oktober|Okt.,November|Nov.,Dezember|Dez.';
  GermamTexts = 'Heute ist %s, dd.mm.yyyy, Feiertage in %d, '+
    'Keine Feiertage vorbereitet für %d, dddd dd.mm.yyyy, mmmm yyyy';

  SpanishDays = 'dom,lun,mar,mié,jue,vie,sáb';
  SpanishMonths = 'enero|ene,febrero|feb,marzo|mar,abril|abr,mayo|may,junio|jun,'+
    'julio|jul,agosto|ago,septiembre|sep,octubre|oct,noviembre|nov,diciembre|dic';
  SpanishTexts = 'Hoy es %s, dd/mm/yyyy, Dias de fiestas %d, '+
    'No hay dias feriados establecidos para %d, dddd dd/mm/yyyy, mmmm yyyy';

  ItalianDays = 'domenica|dom,lunedi|lun,martedi|mar,mercoledì|mer,giovedì|gio,venerdì|ven,sabato|sab';
  ItalianMonths = 'gennaio|gen,febbraio|feb,marzo|mar,aprile|apr,maggio|mag,giugno|giu,'+
    'luglio|lug,agosto|ago,settembre|set,ottobre|ott,novembre|nov,dicembre|dic';
  ItalianTexts = 'Oggi è %s, dd/mmm/yyyy, Vacanze durante %d, '+
    'Non ci sono vacanze fissati per %d,"dddd, dd/mmm/yyyy",mmmm yyyy';

  PolishDays = 'nie,pon,wto,Śro,czw,pią,sob';
  PolishMonths = 'Styczeń,Luty,Marzec,Kwiecień,Maj,Czerwiec,Lipiec,Sierpień,Wrzesień,Październik,Listopad,Grudzień';
  PolishTexts = 'Dziś jest,dd/mmm/yyyy,urlop w czasie,Brak święta określone dla';

  FinnishDays = 'Su,Ma,Ti,ke,To,Pe,La';
  FinnishMonths = 'Tammikuu,Helmikuu,Maaliskuu,Huhtikuu,Toukokuu,Kesäkuu,Heinäkuu,Elokuu,Syyskuu,Lokakuu,Marraskuu,Joulukuu';
  FinnishTexts ='Tänään on %s, dd.mm.yyyy, Lomapäivät %d, Lomapäiviä ei ole asetettu %d';

  GreekDays = 'Κυρ,Δευ,Τρί,Τετ,Πεμ,Παρ,Σαβ';
  GreekMonths = 'Ιανουάριος,Φεβρουάριος,Μάρτιος,Απρίλος,Μάιος,Ιούνιος,Ιούλιος,Αύγουστος,Σεπτέμβριος,Οκτώβριος,Νοέμβριος,Δεκέμβριος';
  GreekTexts = 'Σήμερα είναι,"mmm dd"","" yyyy",Καμία γιορτή,Δεν έχει καμία αργία';

  DBLCLICK_INTERVAL = 300;   // Interval (ms) for detection of a double-click
  DESIGNTIME_PPI = 96;


{ Holiday helpers }

{ Clears the per month holiday buffer }
procedure ClearHolidays(var AHolidays: DWord);
begin
  AHolidays := 0;
end;

{ Set bit for given day to mark the day as a holiday }
procedure AddHoliday(ADay: Integer; var AHolidays: DWord);
begin
  AHolidays := DWord(1 shl ADay) or AHolidays;
end;

{ Check if the bit for the given day is set in AHolidays }
function IsHoliday(ADay: Integer; AHolidays: THolidays): Boolean;
begin
  Result := (AHolidays and DWord(1 shl ADay)) <> 0;
end;


{ TCalSortedDateList }

type
  TDateItem = TDate;
  PDateItem = ^TDateItem;

function CompareDates(P1, P2: Pointer): Integer;
begin
  Result := CompareDate(PDateItem(P1)^, PDateItem(P2)^);
end;

constructor TCalDateList.Create;
begin
  inherited;
  FList := TFPList.Create;
end;

destructor TCalDateList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TCalDateList.AddDate(ADate: TDate);
var
  i: Integer;
  P: PDateItem;
begin
  i := IndexOfDate(ADate);
  if i > -1 then begin
    P := PDateItem(FList.Items[i]);
    Dispose(P);
    FList.Delete(i);
    exit;
  end;

  // Assume that the list is sorted
  for i:= FList.Count-1 downto 0 do begin
    P := PDateItem(FList.Items[i]);
    // Add new date
    if P^ < ADate then begin
      Insert(i+1, ADate);   // meaning: "insert BEFORE index i"
      exit;
    end;
  end;
  Insert(0, ADate);
end;

function TCalDateList.AsArray: TCalDateArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i:=0 to High(Result) do
    Result[i] := Values[i];
end;

procedure TCalDateList.Clear;
var
  i: Integer;
  P: PDateItem;
begin
  for i := FList.Count-1 downto 0 do begin
    P := PDateItem(FList.Items[i]);
    Dispose(P);
    FList.Delete(i);
  end;
  FList.Clear;
end;

procedure TCalDateList.DeleteDate(ADate: TDate);
var
  i: Integer;
  P: PDateItem;
begin
  i := IndexOfDate(ADate);
  if i > -1 then begin
    P := PDateItem(FList.Items[i]);
    Dispose(P);
    FList.Delete(i);
  end;
end;

function TCalDateList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCalDateList.GetDate(AIndex: Integer): TDate;
var
  P: PDateItem;
begin
  P := PDateItem(FList.Items[AIndex]);
  Result := P^;
end;

function TCalDateList.IndexOfDate(ADate: TDate): Integer;
var
  lower, higher, mid, truncADate, truncMidDate: integer;

  function Compare: integer;
  begin
    if (truncMidDate < truncADate) then
      Exit(-1)
    else if (truncMidDate > truncADate) then
      Exit(+1)
    else
      Exit(0);
  end;

begin
  lower := 0;
  higher := Pred(FList.Count);
  truncADate := trunc(ADate);
  while (lower <= higher) do begin
    mid := (lower + higher) shr 1;
    truncMidDate:=trunc(GetDate(mid));
    case Compare of
      -1: lower := Succ(mid);
      +1: higher := Pred(mid);
      0:  Exit(mid);
    end;
  end;
  Exit(-1);
end;

procedure TCalDateList.Insert(AIndex: Integer; ADate: TDate);
var
  P: PDateItem;
begin
  New(P);
  P^ := ADate;
  if AIndex >= FList.Count then
    FList.Add(P)
  else
    FList.Insert(AIndex, P);
end;

procedure TCalDateList.SetDate(AIndex: Integer; AValue: TDate);
var
  P: PDateItem;
begin
  P := PDateItem(FList.Items[AIndex]);
  P^ := AValue;
  Sort;
end;

procedure TCalDateList.Sort;
begin
  FList.Sort(@CompareDates);
end;


{ TCalDrawer }

constructor TCalDrawer.Create(ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas:= ACanvas;
  FTextStyle:= DefTextStyle;
end;

procedure TCalDrawer.CalcSettings;
var
  rem: Integer = 0;
  hSpc: Integer = 0;
  ch: Integer = 0;
  sp: Integer = 0;
  cw: Integer = 0;
  bit: integer=0;
  i, cellWidths, totalSpace, cellHeights,
  adjSpace, borderh, borderv, numRows: integer;
  sz: TSize;
begin
  if (FOwner.BiDiMode = bdLeftToRight) then
    FTextStyle.RightToLeft:= False
  else
    FTextStyle.RightToLeft:= True;
  SetLength(FRowPositions, 0);
  if (coShowTodayRow in FOwner.Options) then
    LastRow := TodayRow
  else
    LastRow := LastDateRow;
  SetLength(FRowPositions, LastRow+1);

  totalspace := Succ(LastCol)*3;
  sz := Size(FBoundsRect);
  cellWidths := sz.cx - totalSpace;
  DivMod(cellWidths, LastCol, cw, rem);
  FCellSize.cx := cw;
  adjSpace := sz.cx - LastCol*cw;
  DivMod(adjSpace, LastCol+1, hSpc, rem);
  borderh := (rem div 2) + 1;
  for i := Low(FColPositions) to High(FColPositions) do
    case FOwner.BiDiMode = bdLeftToRight of
      False : FColPositions[8-i]:= borderh + Pred(i)*cw + hSpc*i;
      True  : FColPositions[i]:= borderh + Pred(i)*cw + hSpc*i;
    end;
  case LastRow of
    LastDateRow : totalSpace := 12;
    TodayRow    : totalSpace := 14;
  end;

  cellHeights := sz.cy - totalSpace;
  numRows := Succ(LastRow);
  DivMod(cellHeights, numRows, ch, rem);
  FCellSize.cy := ch;
  adjSpace := sz.cy - numRows*ch;
  DivMod(adjSpace, totalSpace, sp, rem);
  rem := sz.cy - ch*numRows - totalSpace*sp;
  borderv := rem div 3;
  if (borderv = 0) then
    bit := rem + 1;
  rem := sp shl 1;
  cw := bit + borderv + rem;
  FRowPositions[TopRow] := cw;
  inc(cw, rem);
  FRowPositions[DayRow] := cw + ch;
  for i := FirstDateRow to LastDateRow do
    FRowPositions[i] := cw + i*ch + (i-1)*sp;
  if (LastRow = TodayRow) then
   FRowPositions[TodayRow] := FRowPositions[LastDateRow] + borderv + ch + rem;
end;

procedure TCalDrawer.Draw;
begin
  if not Assigned(FCanvas) then Exit;
  DecodeDate(FOwner.FDate, FThisYear, FThisMonth, FThisDay);
  CalcSettings;
  DrawTopRow;
  DrawDayLabels;
  DrawTodayRow;
  DrawDayCells;  // must be last to avoid resetting the canvas
end;

procedure TCalDrawer.DrawArrow(ARect: TRect; AHead: TArrowhead;
  ADirec: TArrowDirection);
var
  sz: TSize;
  d, ox, oy, half: integer;
  pts: TArrowPoints;
begin
  FCanvas.Pen.Style := psSolid;
  if (FCanvas.Brush.Color <> FOwner.Colors.ArrowColor) then
    FCanvas.Brush.Color:= FOwner.Colors.ArrowColor;
  if (FCanvas.Pen.Color <> FOwner.Colors.ArrowBorderColor) then
    FCanvas.Pen.Color := FOwner.Colors.ArrowBorderColor;
  sz := Size(aRect);
  d := Min(sz.cy, sz.cx) div 3;
  half := d div 2;
  ox := ARect.Left + (sz.cx - d) div 2;
  oy := ARect.Top + (sz.cy - d) div 2;
  case AHead of
   ahSingle:
     begin
       case ADirec of
         adLeft:
           begin
             pts[1]:= Point(ox+d, oy);
             pts[2]:= Point(ox, oy+half);
             pts[3]:= Point(ox+d, oy+d);
           end;
         adRight:
           begin
             pts[1]:= Point(ox, oy);
             pts[2]:= Point(ox, oy+d);
             pts[3]:= Point(ox+d, oy+half);
           end;
       end;
       FCanvas.Polygon(pts);
     end;
   ahDouble:
     case ADirec of
       adLeft:
         begin
           pts[1]:= Point(ox+half-1, oy);
           pts[2]:= Point(ox-1, oy+half);
           pts[3]:= Point(ox+half-1, oy+d);
           FCanvas.Polygon(pts);
           pts[1]:= Point(ox+d, oy);
           pts[2]:= Point(ox+half, oy+half);
           pts[3]:= Point(ox+d, oy+d);
           FCanvas.Polygon(pts);
         end;
       adRight:
         begin
           pts[1]:= Point(ox, oy);
           pts[2]:= Point(ox+half, oy+half);
           pts[3]:= Point(ox, oy+d);
           FCanvas.Polygon(pts);
           pts[1]:= Point(ox+half+1, oy);
           pts[2]:= Point(ox+d+1, oy+half);
           pts[3]:= Point(ox+half+1, oy+d);
           FCanvas.Polygon(pts);
         end;
     end;
  end;
end;

procedure TCalDrawer.DrawDayCells;
var
  remDays: integer = 0;
  startRow: Integer = 0;
  holidays: THolidays = 0;
  r, c, startCol, startSpan: integer;
  rec: TRect;
  s: string;
  dow, y, m, d: word;
  partWeeks: Integer;
  dt, todayDate: TDateTime;
  oldBrush: TBrush;
  oldPen: TPen;
  state: TCalCellStates;
  continueDrawing: Boolean;
begin
  todayDate := Date;
  dow := DayOfWeek(FOwner.FDate);
  c := dow - integer(FOwner.FStartingDayOfWeek);
  if (c < 0) then Inc(c, 7);
  startCol := Succ(c);
  partweeks := FThisDay - startCol;
  DivMod(partWeeks, 7, startRow, remDays);
  if (remDays > 0) then Inc(startRow, 1);
  startspan := startRow*7 + startCol - 1;
  FStartDate := FOwner.FDate - startSpan;
  dt := FStartDate;

  oldBrush := TBrush.Create;
  oldPen := TPen.Create;

  { Get holidays in current month }
  ClearHolidays(holidays);
  if Assigned(FOwner.FOnGetHolidays) then
    FOwner.FOnGetHolidays(FOwner, FThisMonth, FThisYear, holidays);

  for r:= FirstDateRow to LastDateRow do
    for c:= Low(FColPositions) to High(FColPositions) do
    begin
      rec := GetCellAtColRow(c, r);
      DecodeDate(dt, y, m, d);

      { Default canvas }
      FCanvas.Brush.Style := bsSolid;
      FCanvas.Brush.Color := FOwner.Colors.BackgroundColor;
      FCanvas.Pen.Style := psClear;
      FCanvas.Pen.Width := 1;
      FCanvas.Font.Assign(FOwner.Font);
      state := [];

      { Set font of day cells }
      if m = FThisMonth then
      begin
        { Default text color of day numbers }
        FCanvas.Font.Color:= FOwner.Colors.TextColor;
        { Special case: override holidays }
        if (coShowHolidays in FOwner.Options) and IsHoliday(d, holidays) then
        begin
          FCanvas.Font.Color := FOwner.Colors.HolidayColor;
          if coBoldHolidays in FOwner.Options then
            FCanvas.Font.Style := [fsBold];
        end else
        { Special case: override weekend }
        if (coShowWeekend in FOwner.Options) and
           (TDayOfWeek(DayOfWeek(dt)) in FOwner.FWeekendDays) then
        begin
          FCanvas.Font.Color := FOwner.Colors.WeekendColor;
          if coBoldWeekend in FOwner.Options then
            FCanvas.Font.Style := [fsBold];
        end;
      end else
      begin
        { color of days from previous and next months }
        FCanvas.Font.Color:= FOwner.Colors.PastMonthColor;
        Include(state, csOtherMonth);
      end;

      { Set default background color }
      if FOwner.IsSelected(dt) then begin
        FCanvas.Brush.Color:= FOwner.FColors.SelectedDateColor;
        Include(state, csSelectedDay);
      end else
        FCanvas.Brush.Color:= FOwner.Colors.BackgroundColor;

      { Set border pen of "today" cell }
      if (dt = todayDate) and (coShowTodayFrame in FOwner.Options) then
      begin
        FCanvas.Pen.Color := FOwner.Colors.TodayFrameColor;
        FCanvas.Pen.Width := 2;
        FCanvas.Pen.Style := psSolid;
        Include(state, csToday);
      end else
        FCanvas.Pen.Style := psClear;

      { Override canvas properties }
      oldPen.Assign(FCanvas.Pen);
      oldBrush.Assign(FCanvas.Brush);
      if Assigned(FOwner.FOnPrepareCanvas) then
        FOwner.FOnPrepareCanvas(FOwner, FCanvas, y, m, d, state);

      continueDrawing := true;
      if Assigned(FOwner.FOnDrawCell) then
        { Custom-draw the cell }
        FOwner.FOnDrawCell(FOwner, FCanvas, y, m, d, state, rec, continueDrawing);

      if continueDrawing then
      begin
        { Paint the background of the selected date }
        if FOwner.IsSelected(dt) or
          (oldBrush.Color <> FCanvas.Brush.Color) or
          (oldBrush.Style <> FCanvas.brush.Style) or
          (oldPen.Color <> FCanvas.Pen.Color) or
          (oldPen.Style <> FCanvas.Pen.Style) or
          (oldPen.Width <> FCanvas.Pen.Width)
        then
          FCanvas.Rectangle(rec);

        { Paint the frame around the "today" cell }
        if (dt = todayDate) and (coShowTodayFrame in FOwner.Options) then
        begin
          Inc(rec.Top);
          Inc(rec.Bottom);
          FCanvas.Rectangle(rec);
        end;

        { Paint the day number }
        s := IntToStr(d);
        if Assigned(FOwner.FOnGetDayText) then
          FOwner.FOnGetDayText(FOwner, y, m, d, s);
        FCanvas.TextRect(rec, 0, 0, s, FTextStyle);
      end;

      dt:= dt + 1;
    end;  // for c

  oldPen.Free;
  oldBrush.Free;

end;

procedure TCalDrawer.DrawDayLabels;
var
  c, map: integer;
  rec: TRect;
  lbls: TWeekNameArray;
begin
  FCanvas.Font.Color:= FOwner.Colors.TextColor;
  if (coBoldDayNames in FOwner.Options) then
    FCanvas.Font.Style := [fsBold]
  else
    FCanvas.Font.Style := [];
  map := Integer(FOwner.FStartingDayOfWeek);
  for c:= Low(TWeekNameArray) to High(TWeekNameArray) do
  begin
    if (map > High(TWeekNameArray)) then map := Low(TWeekNameArray);
    lbls[c] := FOwner.GetDayName(TDayOfWeek(map));
    inc(map);
  end;
  for c:= Low(FColPositions) to High(FColPositions) do
  begin
    rec := GetCellAtColRow(c, DayRow);
    FCanvas.TextRect(rec, 0, 0, lbls[c], FTextStyle);
  end;
  if (coDayLine in FOwner.Options) then begin
    rec := GetCellAtColRow(GetLeftColIndex, DayRow);
    rec.Right := GetCellAtColRow(GetRightColIndex, DayRow).Right;
    rec.Bottom := rec.Top;
    FCanvas.Pen.Color := FOwner.Colors.DayLineColor;
    FCanvas.Line(rec);
  end;
end;

procedure TCalDrawer.DrawTodayRow;
var
  r1, r2: TRect;
  w1, w2, w3, rem, halfRem: integer;
  s: String;
  ds: String;
begin
  if (LastRow <> TodayRow) then Exit;
  r1 := GetCellAtColRow(2, TodayRow);

  if coUseTopRowColors in FOwner.Options then begin
    if (FCanvas.Font.Color <> FOwner.Colors.TopRowTextColor)
      then FCanvas.Font.Color:= FOwner.Colors.TopRowTextColor;
    FCanvas.Brush.Color := FOwner.Colors.TopRowColor;
    FCanvas.FillRect(r1);
  end else
  if (FCanvas.Font.Color <> FOwner.Colors.TextColor) then
    FCanvas.Font.Color:= FOwner.Colors.TextColor;

  if coBoldToday in FOwner.Options then
    FCanvas.Font.Style := [fsBold] else
    FCanvas.Font.Style := [];

  s:= FOwner.GetDisplayText(dtToday);
  if pos('%s', s) = 0 then begin
    if (coShowTodayName in FOwner.Options) then
      s := Format('%s %s',[s, FOwner.GetDayName(TDayOfWeek(DayOfWeek(Date())))]);
    AppendStr(s, ' ' + FormatDateTime(FOwner.GetDisplayText(dtTodayFormat), Date(), FOwner.FFormatSettings));
  end else begin
    if coShowTodayName in FOwner.Options then
      ds := FormatDateTime(FOwner.GetDisplayText(dtTodayFormatLong), Date(), FOwner.FFormatSettings)
    else
      ds := FormatDateTime(FOwner.GetDisplayText(dtTodayFormat), Date(), FOwner.FFormatSettings);
    s := Format(s, [ds]);
  end;
  w1 := FCanvas.TextWidth('aaa');
  w2 := FCanvas.TextWidth(' ');
  w3 := FCanvas.TextWidth(s);
  rem := Size(r1).cx - w1 - w2 - w3;
  halfRem := rem div 2;
  if (rem < 0) then
  begin
    Inc(r1.Left, halfRem);
    Dec(r1.Right, halfRem);
    rem := 0;
  end;
  r2:= r1;

  r1.Left := r1.Left + halfRem;
  r1.Right := r1.Left + w1;
  InflateRect(r1, 0, -FCellSize.cy div 5);
  if (FCanvas.Pen.Color <> FOwner.Colors.TodayFrameColor) then
    FCanvas.Pen.Color := FOwner.Colors.TodayFrameColor;
  FCanvas.Pen.Width := 2;
  FCanvas.Frame(r1);
  FCanvas.Pen.Width := 1;

  r2.Left := r1.Right + w2;
  r2.Right := r2.Left + w3 + 2;
  if (coBoldToday in FOwner.Options) then
    FCanvas.Font.Style := [fsBold]
  else
    FCanvas.Font.Style := [];
  FCanvas.TextRect(r2, 0, 0, s, FTextStyle);
end;

procedure TCalDrawer.DrawTopRow;
var
  r: TRect;
  s: String;
  dt: TDateTime;
begin
  if coUseTopRowColors in FOwner.Options then begin
    FCanvas.Font.Color:= FOwner.Colors.TopRowTextColor;
    FCanvas.Brush.Color := FOwner.Colors.TopRowColor;
    r := GetCellAtColRow(GetLeftColIndex, TopRow);
    r.Right := GetCellAtColRow(GetRightColIndex, TopRow).Right;
    FCanvas.FillRect(r);
  end else
  if (FCanvas.Font.Color <> FOwner.Colors.TextColor) then
    FCanvas.Font.Color:= FOwner.Colors.TextColor;
  if (coBoldTopRow in FOwner.Options) then
    FCanvas.Font.Style := [fsBold]
  else
    FCanvas.Font.Style := [];

  case (FOwner.BiDiMode = bdLeftToRight) of
    False: begin
             r:= GetCellAtColRow(7, TopRow); DrawArrow(r, ahDouble, adLeft);
             r:= GetCellAtColRow(6, TopRow); DrawArrow(r, ahSingle, adLeft);
             r:= GetCellAtColRow(1, TopRow); DrawArrow(r, ahDouble, adRight);
             r:= GetCellAtColRow(2, TopRow); DrawArrow(r, ahSingle, adRight);
             r:= GetCellAtColRow(3, TopRow);
           end;
    True: begin
            r:= GetCellAtColRow(1, TopRow); DrawArrow(r, ahDouble, adLeft);
            r:= GetCellAtColRow(2, TopRow); DrawArrow(r, ahSingle, adLeft);
            r:= GetCellAtColRow(7, TopRow); DrawArrow(r, ahDouble, adRight);
            r:= GetCellAtColRow(6, TopRow); DrawArrow(r, ahSingle, adRight);
            r:= GetCellAtColRow(3, TopRow);
          end;
  end;
  dt := EncodeDate(FThisYear, FThisMonth, 1);
  s := FormatDateTime(FOwner.GetDisplayText(dtCaptionFormat), dt, FOwner.FFormatSettings);
//  s := FOwner.GetMonthName(FThisMonth) + ' ' + IntToStr(FThisYear);
  FCanvas.TextRect(r, 0, 0, s, FTextStyle);
end;

function TCalDrawer.GetCellAt(aPoint: TPoint): TSize;
var
  x: integer;
begin
  case FOwner.BiDiMode <> bdLeftToRight of
   False:
     for x := Low(FColPositions) to High(FColPositions) do
       if FColPositions[x] >= aPoint.x then
       begin
         Result.cx := x-1;
         Break;
       end else
         Result.cx := LastCol;
   True:
     for x:= High(FColPositions) downto Low(FColPositions) do
       if FColPositions[x] >= aPoint.x then
       begin
         Result.cx := x+1;
         Break;
       end else
         Result.cx := 1;
  end;
  for x := 1 to High(FRowPositions) do
    if FRowPositions[x] >= aPoint.y then
    begin
      Result.cy := x-1;
      Break;
    end
    else
      Result.cy := High(FRowPositions);
end;

function TCalDrawer.GetCellAtColRow(aCol, aRow: integer): TRect;
var
  sz: TSize;
  mid, midmid, midhi, midmidhi, half, fraction: integer;
begin
  sz := GetColRowPosition(aCol, aRow);
  Result.Top := sz.cy;
  Result.Bottom := Result.Top + FCellSize.cy;
  half := FCellSize.cx div 2;
  case aRow of
    TopRow:
      begin
        case (FOwner.BiDiMode = bdLeftToRight) of
          True:
            begin                             // LeftToRight
              mid := FColPositions[2] + half;
              fraction := (mid - FColPositions[1]) div 2;
              midmid := FColPositions[1] + fraction;
              midhi := FColPositions[6] + half;
              midmidhi := midhi + fraction;
            end;
          False:
            begin                            // RightToLeft
              mid := FColPositions[6] + half;
              fraction := (mid - FColPositions[7]) div 2;
              midmid := FColPositions[7] + fraction;
              midhi := FColPositions[2] + half;
              midmidhi := midhi + fraction;
              aCol := 8 - aCol;
            end;
        end;
        case aCol of
          1:
            begin
              Result.Left := sz.cx;
              Result.Right := midmid;
            end;
          2:
            begin
              Result.Left := midmid;
              Result.Right := mid;
            end;
          3..5:
            begin
              Result.Left := mid;
              Result.Right := midhi;
            end;
          6:
            begin
              Result.Right := midmidhi;
              Result.Left := midhi;
            end;
          7:
            begin
              Result.Left := midmidhi;
              Result.Right := midmidhi + fraction;
            end;
        end;
      end;

    TodayRow:
      begin
        Result.Left := GetColRowPosition(GetLeftColIndex, TodayRow).cx;
        Result.Right := GetColRowPosition(GetRightColIndex, TodayRow).cx + FCellSize.cx;
      end;

    else
      Result.Left := sz.cx;
      Result.Right := Result.Left + FCellSize.cx;
  end;
end;

function TCalDrawer.GetColRowPosition(aCol, aRow: integer): TSize;
begin
  Result.cy:= FRowPositions[aRow];
  Result.cx:= FColPositions[aCol];
end;

function TCalDrawer.GetDateOfCell(ACell: TSize): TDate;
var
  diff: Integer;
begin
  if (ACell.cy > 1) and (ACell.cy < 8) then
  begin
    diff := ACell.cx + LastCol * (ACell.cy - 2);
    Result := FStartDate + diff - 1;
  end else
    Result := 0;
end;

function TCalDrawer.GetLeftColIndex: Integer;
begin
  if FOwner.BiDiMode = bdLeftToRight then
    Result := 1
  else
    Result := 7;
end;

procedure TCalDrawer.GetMonthYearRects(var AMonthRect, AYearRect: TRect);
var
  sm, sy: string;
  w: Integer;
  r: TRect;
begin
  AMonthRect := GetCellAtColRow(3, TopRow);
  AYearRect := AMonthRect;
  if (coBoldTopRow in FOwner.Options) then
    FCanvas.Font.Style := [fsBold]
  else
    FCanvas.Font.Style := [];
  sm := FOwner.GetMonthName(FThisMonth);
  sy := IntToStr(FThisYear);
  w := FCanvas.TextWidth(sm + ' ' + sy);
  AMonthRect.Left := (FOwner.Width - w) div 2;
  AMonthRect.Right := AMonthRect.Left + FCanvas.TextWidth(sm);
  AYearRect.Right := (FOwner.Width + w) div 2;
  AYearRect.Left := AYearRect.Right - FCanvas.TextWidth(sy);
  if (FOwner.BiDiMode <> bdLeftToRight) then
  begin
    r := AMonthRect;
    AMonthRect := AYearRect;
    AYearRect := r;
  end;
end;

function TCalDrawer.GetRightColIndex: Integer;
begin
  if FOwner.BiDiMode = bdLeftToRight then
    Result := 7
  else
    Result := 1;
end;

procedure TCalDrawer.GotoDay(ADate: word);
begin
  FOwner.Date := ADate;
end;

procedure TCalDrawer.GotoMonth(AMonth: word);
var
  d: TDate;
begin
  if not TryEncodeDate(FThisYear, AMonth, FThisDay, d) then  // Feb 29 in leap year!
    d := EncodeDate(FThisYear, AMonth, FThisDay);
  FOwner.Date := d;
end;

procedure TCalDrawer.GotoToday;
begin
  FOwner.Date:= Date();
end;

procedure TCalDrawer.GotoYear(AYear: word);
var
  d: TDate;
begin
  if not TryEncodeDate(AYear, FThisMonth, FThisDay, d) then  // Feb 29 in leap year!
    d := EncodeDate(AYear, FThisMonth, FThisDay);
  FOwner.Date := d;
end;

procedure TCalDrawer.LeftClick(APoint: TPoint; Shift: TShiftState);
var
  ppopup: TPoint;
  cell: TSize;
  Rm, Ry: TRect;
  sm: TCalSelMode;
begin
  sm := FOwner.SelMode(Shift);
  cell := GetCellAt(APoint);
  case cell.cy of
    TopRow:
      case cell.cx of
        1: FOwner.Date := IncYear(FOwner.Date, -1);
        2: FOwner.Date := IncMonth(FOwner.Date, -1);
        3..5:
          begin
            GetMonthYearRects(Rm{%H-}, Ry{%H-});
            if PtInRect(Rm, APoint) then begin
              FOwner.PopulateMonthPopupMenu;
              ppopup := FOwner.ClientToScreen(Point(Rm.Left, Rm.Bottom));
              FOwner.FPopupMenu.PopUp(ppopup.x, ppopup.y);
            end;
            if PtInRect(Ry, APoint) then begin
              FOwner.PopulateYearPopupMenu;
              ppopup := FOwner.ClientToScreen(Point(Ry.Left, Ry.Bottom));
              FOwner.FPopupMenu.Popup(ppopup.x, ppopup.y);
            end;
          end;
        6: FOwner.Date := IncMonth(FOwner.Date, +1);
        7: FOwner.Date := IncYear(FOwner.Date, +1);
      end;

    DayRow: ;

    FirstDateRow..LastDateRow :
      FOwner.ChangeDateTo(GetDateOfCell(cell), sm);

    else
      GotoToday;
  end;
end;

procedure TCalDrawer.RightClick;
begin
  if (FOwner.PopupMenu = nil) and Assigned(FOwner.FOnGetHolidays) then
  begin
    FOwner.PopulateHolidayPopupMenu;
    FOwner.FPopupMenu.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
  end;
end;


{ TCalColors }

constructor TCalColors.Create(AOwner: TCalendarLite);
begin
  inherited Create;
  FOwner := AOwner;
  FColors[0] := clSilver;          //  ArrowBorderColor
  FColors[1] := clSilver;          //  ArrowColor
  FColors[2] := clWhite;           //  BackgroundColor
  FColors[3] := clSilver;          //  BorderColor
  FColors[4] := clSilver;          //  DaylineColor
  FColors[5] := clRed;             //  HolidayColor
  FColors[6] := clSilver;          //  PastMonthColor
  FColors[7] := clMoneyGreen;      //  SelectedDateColor
  FColors[8] := clBlack;           //  TextColor
  FColors[9] := clGray;            //  TodayFrameColor
  FColors[10] := clHighlight;      //  TopRowColor
  FColors[11] := clHighlightText;  //  TopRowTextColor
  FColors[12] := clRed;            //  WeekendColor
end;

function TCalColors.GetColor(AIndex: Integer): TColor;
begin
  Result := FColors[AIndex];
end;

procedure TCalColors.SetColor(AIndex: Integer; AValue: TColor);
begin
  if FColors[AIndex] = AValue then exit;
  FColors[AIndex] := AValue;
  FOwner.Invalidate;
end;


{ TCalendarLite }

constructor TCalendarLite.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FFormatSettings := DefaultFormatSettings;
  FSelDates := TCalDateList.Create;
  FColors := TCalColors.Create(self);
  Color := clWhite;
  FStartingDayOfWeek:= dowSunday;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  Constraints.MinHeight := ScaleX(DefMinHeight, DESIGNTIME_PPI);
  Constraints.MinWidth := ScaleY(DefMinWidth, DESIGNTIME_PPI);
  Canvas.Brush.Style := bsSolid;
  TabStop := true;
  SetDefaultDayNames;
//  FCustomDayNames := GetDayNames;
  SetDefaultMonthNames;
//  FCustomMonthNames := GetMonthNames;
  SetDefaultDisplayTexts;
  FCustomDisplayTexts := GetDisplayTexts;
  FPopupMenu := TPopupMenu.Create(Self);
  FCalDrawer := TCalDrawer.Create(Canvas);
  FCalDrawer.FOwner:= Self;
  FDblClickTimer := TTimer.Create(self);
  FDblClickTimer.Enabled := false;
  FDblClickTimer.Interval := DBLCLICK_INTERVAL;
  FDblClickTimer.OnTimer := @TimerExpired;
  FWeekendDays := [dowSunday, dowSaturday];
  FOptions := [coShowTodayFrame, coBoldHolidays, coShowWeekend, coShowHolidays,
               coShowTodayRow];
  SetLanguage(lgEnglish);
  FPrevMouseDate := 0;
  Date := SysUtils.Date;
end;

destructor TCalendarLite.Destroy;
begin
  FreeAndNil(FSelDates);
  FreeAndNil(FColors);
  SetLength(FCalDrawer.FRowPositions, 0);
  FreeAndNil(FCalDrawer);
  inherited Destroy;
end;

procedure TCalendarLite.AddSelectedDate(ADate: TDate);
begin
  FSelDates.AddDate(ADate);
  Invalidate;
end;

procedure TCalendarLite.ChangeDateTo(ADate: TDate; ASelMode: TCalSelMode);
var
  d, d1, d2: TDate;
  oldMonth: Integer;
begin
  oldMonth := MonthOf(FDate);
  FDate := ADate;

  case ASelMode of
    smFirstSingle:
      begin
        FSelDates.Clear;
        FSelDates.AddDate(ADate);
        FPrevDate := ADate;
      end;

    smNextSingle:
      begin
        FSelDates.AddDate(ADate);
        FPrevDate := ADate;
      end;

    smFirstWeek, smNextWeek, smNextWeekRange:
      begin
        if (DayOfWeek(ADate) in [ord(dowSunday), ord(dowSaturday)]) then
          exit;
        if ASelMode = smFirstWeek then
          FSelDates.Clear;
        // Collect all weekdays
        if ASelMode = smNextWeekRange then begin
          if FPRevDate < ADate then begin
            d1 := FPrevDate + 7;
            d2 := ADate;
          end else begin
            d1 := ADate;
            d2 := FPrevDate + 7;
          end;
        end else begin
          d1 := ADate;
          d2 := ADate;
        end;
        while DayOfWeek(d1) <> ord(dowMonday) do d1 := d1 - 1;
        while DayOfWeek(d2) <> ord(dowFriday) do d2 := d2 + 1;
        d := d1;
        while d <= d2 do begin
          if not (DayOfWeek(d) in [ord(dowSunday), ord(dowSaturday)]) then
            FSelDates.AddDate(d);
          d := d + 1;
        end;
        FPrevDate := ADate;
      end;

    smFirstRange, smNextRange:
      begin
        if (ASelMode = smFirstRange) then
          FSelDates.Clear;
        if FPrevDate < ADate then begin
          d1 := FPrevDate + ord(ASelMode = smNextRange);
          d2 := ADate;
        end else begin
          d1 := ADate;
          d2 := FPrevDate - ord(ASelMode = smNextRange);
        end;
        d := d1;
        while (d <= d2) do begin
          FSelDates.AddDate(d);
          d := d + 1;
        end;
      end;
  end;

  DateChange;
  if MonthOf(FDate) <> oldMonth then
    MonthChange;

  with FCalDrawer do begin
    FCanvas.Brush.Color := Colors.BackgroundColor;
    FCanvas.FillRect(FBoundsRect);
  end;
  Invalidate;
end;

procedure TCalendarLite.ClearSelectedDates;
begin
  FSelDates.Clear;
  Invalidate;
end;

procedure TCalendarLite.DateChange;
begin
  if Assigned(FOnDateChange) then
    FOnDateChange(Self);
end;

procedure TCalendarLite.DblClick;
begin
  FDblClickTimer.Enabled := false;
  inherited;
  case FClickButton of
    mbLeft  : FCalDrawer.LeftClick(FClickPoint, FClickShift + [ssDouble]);
    mbRight : ;
  end;
end;

class function TCalendarLite.GetControlClassDefaultSize: TSize;
begin
  Result.cx := ScaleX(DefCalWidth, DESIGNTIME_PPI);
  Result.cy := ScaleY(DefCalHeight, DESIGNTIME_PPI);
end;

function TCalendarLite.GetDayName(ADayOfWeek: TDayOfWeek): String;
begin
  Result := FFormatSettings.ShortDayNames[integer(ADayOfWeek)];
end;

function TCalendarLite.GetDayNames: String;
var
  L: TStrings;
  i: Integer;
begin
  L := TStringList.Create;
  try
    for i:= 1 to 7 do
      L.Add(FFormatSettings.LongDayNames[i] + '|' + FFormatSettings.ShortDayNames[i]);
    Result := L.CommaText;
  finally
    L.Free;
  end;
end;

function TCalendarLite.GetDisplayText(aTextIndex: TDisplayText): String;
begin
  Result := FDisplayTexts[aTextIndex];
end;

function TCalendarLite.GetDisplayTexts: String;
var
  L: TStrings;
  dt: TDisplayText;
begin
  L := TStringList.Create;
  try
    L.StrictDelimiter := true;
    for dt in TDisplayText do L.Add(FDisplayTexts[dt]);
    Result := L.CommaText;
  finally
    L.Free;
  end;
end;

function TCalendarLite.GetMonthName(AMonth: Integer): String;
begin
  Result := FFormatSettings.LongMonthNames[AMonth];
end;

function TCalendarLite.GetMonthNames: String;
var
  L: TStrings;
  i: Integer;
begin
  L := TStringList.Create;
  try
    for i:=1 to 12 do
      L.Add(FFormatSettings.LongMonthNames[i] + '|' + FFormatSettings.ShortMonthNames[i]);
    Result := L.CommaText;
  finally
    L.Free;
  end;
end;

procedure TCalendarLite.HolidayMenuItemClicked(Sender: TObject);
begin
  FCalDrawer.GotoDay(TMenuItem(Sender).Tag);
end;

procedure TCalendarLite.InternalClick;
begin
  case FClickButton of
    mbLeft  : FCalDrawer.LeftClick(FClickPoint, FClickShift);
    mbRight : FCalDrawer.RightClick;
  end;
end;

function TCalendarLite.IsSelected(ADate: TDate): Boolean;
begin
  if FMultiSelect then
    Result := FSelDates.IndexOfDate(ADate) > -1
  else
    Result := (ADate = FDate);
end;

procedure TCalendarLite.KeyDown(var Key: Word; Shift: TShiftState);

  function Delta(Increase: Boolean): Integer;
  begin
    if Increase then Result := +1 else Result := -1;
  end;

var
  sm: TCalSelMode;

begin
  sm := SelMode(Shift);

  case Key of
    VK_UP,
    VK_DOWN  : ChangeDateTo(IncWeek(FDate, Delta(Key = VK_DOWN)), sm);
    VK_LEFT,
    VK_RIGHT : ChangeDateTo(IncDay(FDate, Delta(Key = VK_RIGHT)), sm);
    VK_HOME  : ChangeDateTo(StartOfTheMonth(FDate), sm);
    VK_END   : ChangeDateTo(EndOfTheMonth(FDate), sm);
    VK_PRIOR,
    VK_NEXT  : if not FMultiSelect and (ssCtrl in Shift) then
                 Date := IncYear(FDate, Delta(Key = VK_NEXT)) else
                 Date := IncMonth(FDate, Delta(Key = VK_NEXT));
    else       inherited;
               exit;
  end;

  Key := 0;
  inherited;
end;

procedure TCalendarLite.MonthChange;
begin
  if Assigned(FOnMonthChange) then
    FOnMonthChange(Self);
end;

procedure TCalendarLite.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if not Focused and not(csNoFocus in ControlStyle) then
    SetFocus;

  FClickPoint := Point(X, Y);
  FClickShift := Shift;
  FClickButton := Button;
  if FMultiSelect then
    FDblClickTimer.Enabled := true
  else
    InternalClick;
end;

procedure TCalendarLite.MouseEnter;
begin
  FSavedHint := Hint;
end;

procedure TCalendarLite.MouseLeave;
begin
  HideHintWindow;
  FPrevMouseDate := 0;
end;

procedure TCalendarLite.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  c: TSize;
  dt: TDate;
begin
  inherited MouseMove(Shift, X, Y);

  if ShowHint and Assigned(FCalDrawer) then
  begin
    c := FCalDrawer.GetCellAt(Point(X,Y));
    dt := FCalDrawer.GetDateOfCell(c);
    if (dt > 0) and (dt <> FPrevMouseDate) then begin
      HideHintWindow;
      ShowHintWindow(Point(X, Y), dt);
    end else
    if (dt = 0) then
      HideHintWindow;
    FPrevMouseDate := dt;
  end;
end;


procedure TCalendarLite.MonthMenuItemClicked(Sender: TObject);
begin
  FCalDrawer.GotoMonth(TMenuItem(Sender).Tag);
end;

procedure TCalendarLite.Paint;
var
  r: TRect;
begin
  if Assigned(FCalDrawer) then
  begin
    if ParentColor then
      Colors.BackgroundColor := Parent.Color;

    if ParentFont then
    begin
      if (Parent.Font <> FCalDrawer.FCanvas.Font)
        then FCalDrawer.FCanvas.Font := Parent.Font
      else if (Canvas.Font.Color <> Colors.TextColor)
        then FColors.TextColor := Canvas.Font.Color;
    end;

    case (BiDiMode = bdLeftToRight) of
     False: if not FCalDrawer.FTextStyle.RightToLeft then
              FCalDrawer.FTextStyle.RightToLeft := True;
     True : if FCalDrawer.FTextStyle.RightToLeft then
             FCalDrawer.FTextStyle.RightToLeft := False;
    end;

    Canvas.Brush.Color:= Colors.BackGroundColor;
    Canvas.FillRect(ClientRect);
    if (coShowBorder in FOptions) then
    begin
      if (Canvas.Pen.Color <> FColors.BorderColor) then
        Canvas.Pen.Color := FColors.BorderColor;
      Canvas.Pen.Style := psSolid;
      Canvas.Frame(ClientRect);
    end;

    r:= ClientRect;
    if (coShowBorder in FOptions) then InflateRect(r, -1, -1);
    FCalDrawer.FBoundsRect:= r;
    FCalDrawer.Draw;
  end;

  inherited Paint;
end;

procedure TCalendarLite.PopulateHolidayPopupMenu;
var
  item: TMenuItem;
  m, d, dayCount: Integer;
  population: integer = 0;
  hols: THolidays = 0;
  dt: TDateTime;
  s: String;
begin
  with FPopupMenu.Items do begin
    Clear;
    item:= TMenuItem.Create(Self);
    s := GetDisplayText(dtHolidaysDuring);
    if pos('%d', s) = 0 then
      item.Caption:= s + ' ' + IntToStr(FCalDrawer.FThisYear)
    else
      item.Caption := Format(s, [FCalDrawer.FThisYear]);
    Add(item);
    item:= TMenuItem.Create(Self);
    item.Caption:= '-';
    Add(item);
    for m:= 1 to 12 do
    begin
      ClearHolidays(hols);
      FOnGetHolidays(Self, m, FCalDrawer.FThisYear, hols);
      dayCount:= DaysInAMonth(FCalDrawer.FThisYear, m);
      d := 1;
      repeat
        if IsHoliday(d, hols) then
        begin
          item := TMenuItem.Create(Self);
          inc(population);
          item.Caption:= IntToStr(d) + ' ' + GetMonthName(m);
          if (m = FCalDrawer.FThisMonth) then
            item.Checked := True;
          dt := EncodeDate(FCalDrawer.FThisYear, m, d);
          item.Tag := trunc(dt);
          item.OnClick := @HolidayMenuItemClicked;
          Add(item);
        end;
        inc(d)
      until d > dayCount;
    end;
    Items[0].Enabled := (population <> 0);
    if not Items[0].Enabled then begin
      s := GetDisplayText(dtNoHolidaysDuring);
      if pos('%d', s) = 0 then
        Items[0].Caption := s + ' ' + IntToStr(FCalDrawer.FThisYear)
      else
        Items[0].Caption := Format(s, [FCalDrawer.FThisYear]);
    end;
  end;
end;

procedure TCalendarLite.PopulateMonthPopupMenu;
var
  m: Integer;
  item: TMenuItem;
begin
  with FPopupMenu.Items do begin
    Clear;
    for m := 1 to 12 do
    begin
      item := TMenuItem.Create(self);
      item.Caption := GetMonthName(m);
      item.OnClick := @MonthMenuItemClicked;
      item.Tag := m;
      if m = FCalDrawer.FThisMonth then
        item.Checked := true;
      Add(item);
    end;
  end;
end;

procedure TCalendarLite.PopulateYearPopupMenu;
var
  y: Integer;
  item: TMenuItem;
begin
  with FPopupMenu.Items do begin
    Clear;
    for y := FCalDrawer.FThisYear - 10 to FCalDrawer.FThisYear + 10 do
    begin
      item := TMenuItem.Create(self);
      item.Caption := IntToStr(y);
      item.OnClick := @YearMenuItemClicked;
      item.Tag := y;
      if y = FCalDrawer.FThisYear then
        item.Checked := true;
      if (FCalDrawer.FThisDay = 29) and (FCalDrawer.FThisMonth = 2) and not IsLeapYear(y)
        then item.Enabled:= False;
      Add(item);
    end;
  end;
end;

function TCalendarLite.SelectedDates: TCalDateArray;
begin
  Result := FSelDates.AsArray;
end;

function TCalendarLite.SelMode(Shift: TShiftState): TCalSelMode;
begin
  Result := smFirstSingle;
  if not FMultiSelect then
    exit;

  if (ssDouble in Shift) then begin
    Result := smFirstWeek;
    if (ssCtrl in Shift) and (FPrevDate > 0) then
      Result := smNextWeek
    else if (ssShift in Shift) and (FPrevDate > 0) then
      Result := smNextWeekRange
  end else
  if (ssShift in Shift) then begin
    Result := smFirstRange;
    if (ssCtrl in Shift) and (FPrevDate > 0) then
      Result := smNextRange;
  end else
  if (ssCtrl in Shift) and (FPrevDate > 0) then
    Result := smNextSingle;
end;

procedure TCalendarLite.SetCustomDayNames(const AValue: String);
begin
  FCustomDayNames := AValue;
  if FLanguage = lgCustom then
    SetLanguage(lgCustom);
end;

procedure TCalendarLite.SetCustomDisplayTexts(const AValue: String);
begin
  FCustomDisplayTexts := AValue;
  if FLanguage = lgCustom then
    SetLanguage(lgCustom);
end;

procedure TCalendarLite.SetCustomMonthNames(const AValue: String);
begin
  FCustomMonthNames := AValue;
  if FLanguage = lgCustom then
    SetLanguage(lgCustom);
end;

procedure TCalendarLite.SetDate(AValue: TDateTime);
var
  oldMonth: Integer;
begin
  if FDate = AValue then Exit;
  oldMonth := MonthOf(FDate);
  FDate := AValue;
  FPrevDate := AValue;
  FSelDates.Clear;
  DateChange;
  if MonthOf(FDate) <> oldMonth then
    MonthChange;
  Invalidate;
end;

procedure TCalendarLite.SetDefaultDayNames;
begin
  UseDayName(dowSunday, rsCalSunday);
  UseDayName(dowMonday, rsCalMonday);
  UseDayName(dowTuesday, rsCalTuesday);
  UseDayName(dowWednesday, rsCalWednesday);
  UseDayName(dowThursday, rsCalThursday);
  UseDayName(dowFriday, rsCalFriday);
  UseDayName(dowSaturday, rsCalSaturday);
end;

procedure TCalendarLite.SetDefaultDisplayTexts;
begin
  FDisplayTexts[dtToday] := rsCalTodayIs;
  FDisplayTexts[dtHolidaysDuring] := rsCalHolidaysIn;
  FDisplayTexts[dtNoHolidaysDuring] := rsCalNoHolidaysIn;

  FDisplayTexts[dtTodayFormat] := rsCalTodayFormat;
  FDisplayTexts[dtTodayFormatLong] := rsCalTodayFormatLong;
  FDisplayTexts[dtCaptionFormat] := rsCalCaptionFormat;

  FCustomDisplayTexts := GetDisplayTexts;
end;

procedure TCalendarLite.SetDefaultMonthNames;
begin
  UseMonthName(1, rsCalJanuary);
  UseMonthname(2, rsCalFebruary);
  UseMonthName( 3, rsCalMarch);
  UseMonthName( 4, rsCalApril);
  UseMonthname( 5, rsCalMay);
  UseMonthname( 6, rsCalJune);
  UseMonthname( 7, rsCalJuly);
  UseMonthName( 8, rsCalAugust);
  UseMonthname( 9, rsCalSeptember);
  UseMonthName(10, rsCalOctober);
  UseMonthName(11, rsCalNovember);
  UseMonthName(12, rsCalDecember);
end;

procedure TCalendarLite.SetDisplayTexts(AValue: String);
var
  L: TStrings;
  i: Integer;
begin
  L := TStringList.Create;
  try
    L.StrictDelimiter := True;
    L.CommaText := AValue;
    for i:=0 to L.Count - 1 do begin
      if i >= ord(High(TDisplayText)) then
        exit;
      FDisplayTexts[TDisplayText(i)] := trim(L[i]);
    end;
  finally
    L.Free;
  end;
  Invalidate;
end;

procedure TCalendarLite.SetLanguage(AValue : TLanguage);
begin
  // Don't check for "FLanguage = AValue" because otherwise the code would not
  // execute after being called from the constructor.
  FLanguage := AValue;

  case FLanguage of
    lgEnglish: begin
                 UseDayNames(EnglishDays);
                 UseMonthNames(EnglishMonths);
                 UseDisplayTexts(EnglishTexts);
                 BiDiMode:= bdLeftToRight;
               end;
    lgFrench:  begin
                 UseDayNames(FrenchDays);
                 UseMonthNames(FrenchMonths);
                 UseDisplayTexts(FrenchTexts);
                 BiDiMode:= bdLeftToRight;
               end;
    lgGerman:  begin
                 UseDayNames(GermanDays);
                 UseMonthNames(GermanMonths);
                 UseDisplayTexts(GermamTexts);
                 BiDiMode:= bdLeftToRight;
               end;
    lgHebrew:  begin
                 UseDayNames(HebrewDays);
                 UseMonthNames(HebrewMonths);
                 UseDisplayTexts(HebrewTexts);
                 BiDiMode:= bdRightToLeft;
               end;
    lgSpanish: begin
                 UseDayNames(SpanishDays);
                 UseMonthNames(SpanishMonths);
                 UseDisplayTexts(SpanishTexts);
                 BiDiMode:= bdLeftToRight;
               end;
    lgItalian: begin
                 UseDayNames(ItalianDays);
                 UseMonthNames(ItalianMonths);
                 UseDisplayTexts(ItalianTexts);
                 BiDiMode:= bdLeftToRight;
               end;
    lgPolish:  begin
                 UseDayNames(PolishDays);
                 UseMonthNames(PolishMonths);
                 UseDisplayTexts(PolishTexts);
                 BiDiMode:= bdLeftToRight;
               end;
    lgFinnish: begin
                 UseDayNames(FinnishDays);
                 UseMonthNames(FinnishMonths);
                 UseDisplayTexts(FinnishTexts);
                 BiDiMode := bdLeftToRight;
               end;
    lgGreek:   begin
                 UseDayNames(GreekDays);
                 UseMonthNames(GreekMonths);
                 UseDisplayTexts(GreekTexts);
                 BiDiMode := bdLeftToRight;
               end;
    lgCustom:  begin
                 UseDayNames(FCustomDayNames);
                 UseMonthNames(FCustomMonthNames);
                 UseDisplayTexts(FCustomDisplayTexts);
               end;
  end;

  Invalidate;
end;

procedure TCalendarLite.SetMultiSelect(AValue: Boolean);
begin
  if AValue = FMultiSelect then
    exit;
  FMultiSelect := AValue;
  FSelDates.Clear;
  FSelDates.AddDate(FDate);
  FPrevDate := FDate;
end;

procedure TCalendarLite.SetStartingDayOfWeek(AValue: TDayOfWeek);
begin
  if FStartingDayOfWeek = AValue then Exit;
  FStartingDayOfWeek := AValue;
  Invalidate;
end;

procedure TCalendarLite.SetOptions(AValue: TCalOptions);
begin
  if FOptions = AValue then Exit;
  FOptions := AValue;
  case (coShowTodayRow in FOptions) of
    False: if LastRow <> LastDateRow then LastRow := LastDateRow;
    True : if LastRow <> TodayRow then LastRow := TodayRow;
  end;
  if Length(FCalDrawer.FRowPositions) <> LastRow+1 then
    SetLength(FCalDrawer.FRowPositions, LastRow+1);
  Invalidate;
end;

procedure TCalendarLite.SetWeekendDays(AValue: TDaysOfWeek);
begin
  if FWeekendDays = AValue then Exit;
  FWeekendDays := AValue;
  Invalidate;
end;

{ The DblClickTimer was triggered by a mouse-down event; its purpose is to
  prevent the Click method in addition to the DblClick method. In case of
  a single click the TimerExpired event is reached. In case of a double-click
  the click handled directly by the DblClick }
procedure TCalendarLite.TimerExpired(Sender: TObject);
begin
  FDblClickTimer.Enabled := false;
  InternalClick;
end;

procedure TCalendarlite.UseDayName(ADayOfWeek: TDayOfWeek; const AValue: String);
var
  p: Integer;
  d: Integer;
begin
  if AValue = '' then exit;
  d := ord(ADayOfWeek);
  p := pos('|', AValue);
  if p > 0 then begin
    FFormatSettings.LongDayNames[d] := Trim(Copy(AValue, 1, p-1));
    FFormatSettings.ShortDayNames[d] := Trim(Copy(AValue, p+1, MaxInt));
  end else begin
    FFormatSettings.LongDayNames[d] := Trim(AValue);
    FFormatSettings.ShortDayNames[d] := FFormatSettings.LongDayNames[d];
  end;
end;

procedure TCalendarLite.UseDayNames(const AValue: String);
var
  L: TStrings;
  i, d: Integer;
begin
  L := TStringList.Create;
  try
    L.CommaText := AValue;
    for i:=0 to L.Count-1 do begin
      d := succ(i);
      if d <= 7 then
        UseDayName(TDayOfWeek(d), L[i]);
    end;
  finally
    L.Free;
  end;
end;

procedure TCalendarLite.UseDisplayTexts(const AValue: String);
begin
  SetDisplayTexts(AValue);
end;

procedure TCalendarLite.UseMonthName(AMonth: Integer; const AValue: String);
var
  p: Integer;
begin
  if AValue = '' then
    exit;
  p := pos('|', AValue);
  if p <> 0 then begin
    FFormatSettings.LongMonthNames[AMonth] := Trim(Copy(AValue, 1, p-1));
    FFormatSettings.ShortMonthNames[AMonth] := Trim(Copy(AValue, p+1, MaxInt));
  end else begin
    FFormatSettings.LongMonthNames[AMonth] := Trim(AValue);
    FFormatSettings.ShortMonthNames[AMonth] := FFormatSettings.LongMonthNames[AMonth];
  end;
end;

procedure TCalendarLite.UseMonthNames(const AValue: String);
var
  L: TStrings;
  i, m: Integer;
begin
  L := TStringList.Create;
  try
    L.CommaText := AValue;
    for i:=0 to L.Count - 1 do begin
      m := succ(i);
      if m <= 12 then
        UseMonthName(m, L[i]);
    end;
  finally
    L.Free;
  end;
end;

procedure TCalendarLite.YearMenuItemClicked(Sender: TObject);
begin
  FCalDrawer.GotoYear(TMenuItem(Sender).Tag);
end;

{ Hints }

procedure TCalendarLite.ShowHintWindow(APoint: TPoint; ADate: TDate);
var
  txt: String = '';
  y, m, d: Word;
begin
  if Assigned(FOnHint) then begin
    DecodeDate(ADate, y, m, d);
    FOnHint(Self, y, m, d, txt);
    if Hint <> '' then begin
      if txt = '' then txt := Hint else txt := Hint + LineEnding + txt;
    end;
  end else
    txt := Hint;

  if txt = '' then
    exit;

  APoint := ClientToScreen(APoint);
  Hint := txt;
  Application.Hint := txt;
  Application.ActivateHint(APoint);
end;

procedure TCalendarLite.HideHintWindow;
begin
  Hint := FSavedHint;
  Application.CancelHint;
end;


procedure Register;
begin
  RegisterComponents('Misc', [TCalendarLite]);
end;


end.
