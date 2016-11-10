unit uMainTestCalLite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ExtCtrls, StdCtrls, Spin, Dialogs,
  Controls, CalendarLite;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnFont: TButton;
    cbUseHolidays: TCheckBox;
    cgOptions: TCheckGroup;
    CbArrowBorder: TColorButton;
    CbTodayFrame: TColorButton;
    CbTopRow: TColorButton;
    CbTopRowText: TColorButton;
    CbWeekend: TColorButton;
    CbArrow: TColorButton;
    CbBackground: TColorButton;
    CbBorder: TColorButton;
    CbDayLine: TColorButton;
    CbHolidays: TColorButton;
    CbPastMonth: TColorButton;
    CbSelectedDate: TColorButton;
    CbText: TColorButton;
    CbPrepareCanvas: TCheckBox;
    CbDrawCell: TCheckBox;
    FontDialog: TFontDialog;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LTitle: TLabel;
    LWidth: TLabel;
    lHeight: TLabel;
    PSettings: TPanel;
    rgLanguage: TRadioGroup;
    rgStartingDOW: TRadioGroup;
    seWidth: TSpinEdit;
    seHeight: TSpinEdit;
    procedure BtnFontClick(Sender: TObject);
    procedure CbDrawCellChange(Sender: TObject);
    procedure CbPrepareCanvasChange(Sender: TObject);
    procedure ColorButtonChanged(Sender: TObject);
    procedure cbUseHolidaysChange(Sender: TObject);
    procedure cgOptionsItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure rgLanguageClick(Sender: TObject);
    procedure rgStartingDOWClick(Sender: TObject);
    procedure seHeightChange(Sender: TObject);
    procedure seWidthChange(Sender: TObject);
  private
    copyCal, demoCal: TCalendarLite;
    FNoHolidays: boolean;
    procedure RespondToDateChange(Sender: tObject);
    procedure GetHint(Sender: TObject; AYear, AMonth, ADay: Word; out AHintText: String);
    procedure GetHolidays(Sender: TObject; AMonth, AYear: Integer;  // wp
      var Holidays: THolidays);
    procedure PrepareCanvas(Sender: TObject; AYear, AMonth, ADay: Word;
      AState: TCalCellStates; ACanvas: TCanvas);
    procedure DrawCell(Sender: TObject; AYear, AMonth, ADay: Word;
      AState: TCalCellStates; ARect: TRect; ACanvas: TCanvas;
      var AContinueDrawing: Boolean);
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}


function Easter(year:integer) : TDateTime;  // wp
var
  Day, Month    : integer;
  a,b,c,d,e,m,n : integer;
begin
  case Year div 100 of
    17    : begin m := 23; n := 3; end;
    18    : begin m := 23; n := 4; end;
    19,20 : begin m := 24; n := 5; end;
    21    : begin m := 24; n := 6; end;
    else    raise Exception.Create('Only years above 1700 supported.');
  end;
  a := Year mod 19;
  b := Year mod 4;
  c := Year mod 7;
  d := (19*a + m) mod 30;
  e := (2*b + 4*c + 6*d + n) mod 7;
  day := 22 + d + e;
  Month := 3;
  if Day>31 then begin
    Day := d + e - 9;
    Month := 4;
    if (d=28) and (e=6) and (a>10) then begin
      if day=26 then day := 19;
      if day=25 then day := 18;
    end;
  end;
  result := EncodeDate(year, month, day);
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  opt: TCalOption;
begin
  demoCal:= TCalendarLite.Create(Self);
  demoCal.Parent:= Self;
  demoCal.Left:= 10;
  demoCal.Top:= PSettings.Height + 10;
  demoCal.Width := seWidth.Value;
  demoCal.Height := seHeight.Value;
  demoCal.OnGetHolidays := @GetHolidays;
  demoCal.OnDateChange:= @RespondToDateChange;
  demoCal.OnHint := @GetHint;
  demoCal.ShowHint := true;
  demoCal.Hint := 'Calendar';
  if CbPrepareCanvas.Checked then
    demoCal.OnPrepareCanvas := @PrepareCanvas else
    demoCal.OnPrepareCanvas := nil;
  if CbDrawCell.Checked then
    demoCal.OnDrawCell := @DrawCell else
    demoCal.OnDrawCell := nil;
  FNoHolidays:= False;
  for opt in demoCal.Options do
   if (opt in demoCal.Options) then cgOptions.Checked[integer(opt)] := True;
  seHeight.Value := demoCal.Height;
  seWidth.Value:= demoCal.Width;
  rgStartingDOW.ItemIndex:= integer(demoCal.StartingDayOfWeek)-1;

  copyCal:= TCalendarLite.Create(Self);
  copyCal.Parent := Self;
  copyCal.Width := 270;
  copyCal.Height := 205;
  copyCal.Left := Width - copyCal.Width;
  copyCal.Top := Height - copyCal.Height;
  copyCal.Font.Name := 'Lucida Calligraphy';
  copyCal.Colors.SelectedDateColor := clYellow;
  copyCal.Colors.ArrowBorderColor := clYellow;
  copyCal.Colors.ArrowColor := clYellow;
  copyCal.Colors.TodayFrameColor := clWhite;
  copyCal.Colors.BackgroundColor:= clGradientActiveCaption;
  copyCal.StartingDayOfWeek:= dowSaturday;
  copyCal.OnGetHolidays := @GetHolidays;
  copyCal.Options := copyCal.Options + [coShowBorder,coUseTopRowColors,coDayLine];

  CbArrowBorder.ButtonColor := demoCal.Colors.ArrowBorderColor;
  CbArrow.ButtonColor := demoCal.Colors.ArrowColor;
  CbBackground.ButtonColor := demoCal.Colors.BackgroundColor;
  CbBorder.ButtonColor := demoCal.Colors.BorderColor;
  CbDayLine.ButtonColor := demoCal.Colors.DayLineColor;
  CbHolidays.Buttoncolor := demoCal.colors.HolidayColor;
  CbPastMonth.ButtonColor := demoCal.Colors.PastMonthColor;
  CbSelectedDate.ButtonColor := demoCal.Colors.SelectedDateColor;
  CbText.ButtonColor := demoCal.Colors.TextColor;
  CbTodayFrame.ButtonColor := demoCal.Colors.TodayFrameColor;
  CbTopRow.ButtonColor := demoCal.Colors.TopRowColor;
  CbTopRowText.ButtonColor := democal.Colors.TopRowTextColor;
  CbWeekend.ButtonColor := demoCal.Colors.WeekendColor;
end;

procedure TForm1.rgLanguageClick(Sender: TObject);
begin
  case rgLanguage.ItemIndex of
    0: demoCal.Languages := lgEnglish;
    1: demoCal.Languages := lgFrench;
    2: demoCal.Languages := lgGerman;
    3: demoCal.Languages := lgHebrew;
    4: demoCal.Languages := lgSpanish;
  end;
end;

procedure TForm1.rgStartingDOWClick(Sender: TObject);
begin
  demoCal.StartingDayOfWeek := TDayOfWeek(rgStartingDOW.ItemIndex + 1);
end;

procedure TForm1.seHeightChange(Sender: TObject);
begin
  demoCal.Height := seHeight.Value;
end;

procedure TForm1.seWidthChange(Sender: TObject);
begin
  demoCal.Width := seWidth.Value;
end;

procedure TForm1.ColorButtonChanged(Sender: TObject);
var
  calendar: TCalendarLite;
  col: TColor;
begin
  calendar := demoCal;
  col := (Sender as TColorButton).ButtonColor;
  case (Sender as TColorButton).Name of
    'CbArrowBorder': calendar.Colors.ArrowBorderColor := col;
    'CbArrow': calendar.Colors.ArrowColor := col;
    'CbBackground': calendar.Colors.BackgroundColor := col;
    'CbBorder': calendar.Colors.BorderColor := col;
    'CbDayLine': calendar.Colors.DayLineColor := col;
    'CbHolidays': calendar.Colors.HolidayColor := col;
    'CbPastMonth': calendar.Colors.PastMonthColor := col;
    'CbSelectedDate': calendar.Colors.SelectedDateColor := col;
    'CbText': calendar.Colors.TextColor := col;
    'CbTodayFrame': calendar.Colors.TodayFrameColor := col;
    'CbTopRow': calendar.Colors.TopRowColor := col;
    'CbTopRowText': calendar.Colors.TopRowTextColor := col;
    'CbWeekend': calendar.Colors.WeekendColor := col;
  end;
  calendar.Invalidate;
end;

procedure TForm1.cbUseHolidaysChange(Sender: TObject);
begin
  FNoHolidays := not FNoHolidays;
end;

procedure TForm1.cgOptionsItemClick(Sender: TObject; Index: integer);
var opt: TCalOption;
begin
  opt := TCalOption(Index);
  if (opt in demoCal.Options) then
    demoCal.Options := demoCal.Options - [opt]
  else demoCal.Options := demoCal.Options + [opt];
end;

procedure TForm1.BtnFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(demoCal.Font);
  if FontDialog.Execute then
    demoCal.Font.Assign(FontDialog.Font);
end;

procedure TForm1.CbDrawCellChange(Sender: TObject);
begin
  if CbDrawCell.Checked then
    demoCal.OnDrawCell := @DrawCell else
    demoCal.OnDrawCell := nil;
  demoCal.Invalidate;
end;

procedure TForm1.CbPrepareCanvasChange(Sender: TObject);
begin
  if CbPrepareCanvas.Checked then
    demoCal.OnPrepareCanvas := @PrepareCanvas else
    demoCal.OnPrepareCanvas := nil;
  demoCal.Invalidate;
end;

procedure TForm1.RespondToDateChange(Sender: tObject);
begin
  copyCal.Date:= TCalendarLite(Sender).Date;
end;

procedure TForm1.GetHint(Sender: TObject; AYear, AMonth, ADay: Word;
  out AHintText: String);
var
  dt, e: TDate;
begin
  case AMonth of
    1: if ADay = 1 then AHintText := 'New Year';
   12: if ADay = 25 then AHintText := 'Christmas';
   else
       e := Easter(AYear);
       dt := EncodeDate(AYear, AMonth, ADay);
       if (dt = e) then
         AHintText := 'Easter'
       else if (dt = e + 49) then
         AHintText := 'Whit Sunday';
  end;
end;

procedure TForm1.GetHolidays(Sender: TObject; AMonth, AYear: Integer;
  var Holidays: THolidays);
var
  d, m, y: Word;
  e: TDate;
begin
  ClearHolidays(Holidays);
  if not FNoHolidays then
  begin
    // Fixed holidays
    case AMonth of
      1: AddHoliday(1, Holidays);          // New Year
     12: AddHoliday(25, Holidays);         // Christmas
    end;
    // Easter
    e := Easter(AYear);
    DecodeDate(e, y,m,d);
    if m = AMonth then
      AddHoliday(d, Holidays);
    // Whit Sunday --> 49 days after easter
    DecodeDate(e+49, y,m,d);
    if m = AMonth then
      AddHoliday(d, Holidays);
  end;
end;

procedure TForm1.PrepareCanvas(Sender: TObject; AYear,AMonth,ADay: word;
  AState: TCalCellStates; ACanvas: TCanvas);
begin
  if (ADay = 1) and not (csOtherMonth in AState) then
  begin
    ACanvas.Font.Size := 12;
    ACanvas.Font.Style := [fsUnderline, fsItalic, fsBold];
    ACanvas.Font.Color := clGreen;
    ACanvas.Brush.Color := clSilver;
    ACanvas.Brush.Style := bsFDiagonal;
    ACanvas.Pen.Color := clSilver;
    ACanvas.Pen.Style := psSolid;
  end;
end;

procedure TForm1.DrawCell(Sender: TObject; AYear,AMonth,ADay: Word;
  AState: TCalCellStates; ARect: TRect; ACanvas: TCanvas;
  var AContinueDrawing: Boolean);
var
  bmp: TBitmap;
begin
  if (AMonth = 11) and (ADay = 11) and not (csOtherMonth in AState) then begin
    bmp := TBitmap.Create;
    ImageList1.GetBitmap(0, bmp);
    ACanvas.Draw(ARect.Left, (ARect.Top + ARect.Bottom - bmp.Height) div 2, bmp);
    inc(ARect.Left, bmp.Width + 2);
    ACanvas.TextOut(ARect.Left, (ARect.Top + ARect.Bottom - ACanvas.TextHeight('Tg')) div 2, intToStr(ADay));
    AContinueDrawing := false;  // Skips built-in painting of this day cell
  end;
end;

end.

