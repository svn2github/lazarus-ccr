unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VpBaseDS, VpBufDS, VpDayView, VpWeekView, VpMonthView;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Datastore: TVpBufDSDatastore;
    ControlLink: TVpControlLink;
    WeekView: TVpWeekView;
    DayView: TVpDayView;
    MonthView: TVpMonthView;
    combo: TVpResourceCombo;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Datastore := TVpBufDSDatastore.Create(self);
  Datastore.Directory := '.';
  Datastore.AutoCreate := true;
  Datastore.Connected := true;

  DayView := TVpDayview.Create(self);
  DayView.Parent := self;
  DayView.Align := alLeft;

  WeekView := TVpWeekView.Create(self);
  WeekView.Parent := self;
  Weekview.Align := alClient;

  MonthView := TVpMonthView.Create(self);
  MonthView.Parent := self;
  MonthView.Align := alRight;

  Combo := TVpResourceCombo.Create(Self);
  Combo.Parent := Panel1;
  Combo.Left := 8;
  Combo.Top := 8;
  Combo.Width := 200;

  ControlLink := TVpControlLink.Create(self);
  ControlLink.Datastore := Datastore;
  // This establishes the links to all controls that depend on Datastore.
  // Must be called when all dependent controls are created.

  if Datastore.Resources.Count > 0 then
    Datastore.Resource := Datastore.Resources.Items[0];
end;

end.

