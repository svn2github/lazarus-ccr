unit uMainTestCalLite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ExtCtrls, StdCtrls, Spin, CalendarLite;

type

  { TForm1 }

  TForm1 = class(TForm)
    CalendarLite1: TCalendarLite;
    cbUseHolidays: TCheckBox;
    cgOptions: TCheckGroup;
    Label1: TLabel;
    LTitle: TLabel;
    LWidth: TLabel;
    lHeight: TLabel;
    PSettings: TPanel;
    rgLanguage: TRadioGroup;
    rgStartingDOW: TRadioGroup;
    seWidth: TSpinEdit;
    seHeight: TSpinEdit;
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
    procedure GetHolidays(Sender: TObject; AMonth, AYear: Integer;  // wp
      var Holidays: THolidays);
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

uses
  Dialogs;

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
var opt: TCalOption;
begin
  demoCal:= TCalendarLite.Create(Self);
  demoCal.Parent:= Self;
  demoCal.Left:= 10;
  demoCal.Top:= PSettings.Height + 10;
  demoCal.Width := seWidth.Value;
  demoCal.Height := seHeight.Value;
  demoCal.OnGetHolidays := @GetHolidays;
  demoCal.OnDateChange:= @RespondToDateChange;
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

procedure TForm1.RespondToDateChange(Sender: tObject);
begin
  copyCal.Date:= TCalendarLite(Sender).Date;
end;

// wp
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

end.

