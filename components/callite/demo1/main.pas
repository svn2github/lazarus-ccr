unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LclType, Buttons, StdCtrls, DateUtils, CalendarLite;

type

  { TForm1 }

  TForm1 = class(TForm)
    edtYear: TEdit;
    edtMonth: TEdit;
    Label1: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure edtYearKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure edtMonthKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    CalendarLite1: TCalendarLite;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

var
  AYear: Integer;
  AMonth: Integer;
  MonthsList: TStringList;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  CalendarLite1 := TCalendarLite.Create(self);
  with CalendarLite1 do begin
    Parent := self;
    Left := 20;
//    Height := 160;
    Top := 40;
    Width := self.Width - 2*Left;
    Height := label1.Top - Top - 20;
    ParentColor := false;
    Date := 41574;
    DisplayTexts := '"Today is",dd/mm/yyyy,"Holidays during","There are no holidays set for"';
    WeekendDays := [dowSaturday];
    Anchors := [akLeft, akTop, akRight, akBottom];
  end;
  
  MonthsList:= TStringList.Create;
  for I:= 0 to 11 do begin
    MonthsList.Add(AnsiToUTF8(FormatSettings.ShortMonthNames[I+1]));
  end;

  AYear:= YearOf(Now);
  AMonth:= MonthOf(Now)-1;
  edtYear.Caption := IntToStr(AYear);
  edtMonth.Caption := MonthsList[AMonth];
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  edtMonth.Left := Width div 2 - edtMonth.Width - 2;
  edtYear.Left := Width div 2 + 2;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  FreeAndNil(MonthsList);
  Close;
end;

procedure TForm1.edtYearKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_Up   : Inc(AYear);
    VK_Down : Dec(AYear);
  end;
  edtYear.Caption := IntToStr(AYear);
  CalendarLite1.Date := RecodeYear(CalendarLite1.Date,AYear);
end;

procedure TForm1.edtMonthKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_Up   : Inc(AMonth);
    VK_Down : Dec(AMonth);
  end;
  case AMonth of
    -1: AMonth := 11;
    12: AMonth := 0;
  end;
  edtMonth.Text:= MonthsList[AMonth];
  CalendarLite1.Date:= RecodeMonth(CalendarLite1.Date,AMonth+1);
end;

end.

