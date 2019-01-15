unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VpLEDLabel, VpClock;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnStartStop: TButton;
    CbNewClockFace: TCheckBox;
    CbMilitaryTime: TCheckBox;
    EdCountDownTime: TEdit;
    LblCountDownTime: TLabel;
    LblElapsedTime: TLabel;
    Panel1: TPanel;
    RgDisplayMode: TRadioGroup;
    RgClockMode: TRadioGroup;
    VpClock: TVpClock;
    VpLEDLabel1: TVpLEDLabel;
    procedure AnalogClockCountdownDone(Sender: TObject);
    procedure BtnStartStopClick(Sender: TObject);
    procedure CbMilitaryTimeChange(Sender: TObject);
    procedure CbNewClockFaceChange(Sender: TObject);
    procedure EdCountDownTimeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RgClockModeClick(Sender: TObject);
    procedure RgDisplayModeClick(Sender: TObject);
    procedure VpClockCountdownDone(Sender: TObject);
    procedure VpClockTimeChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.AnalogClockCountdownDone(Sender: TObject);
begin
  ShowMessage('Countdown finished.');
end;

procedure TForm1.VpClockTimeChange(Sender: TObject);
begin
  LblElapsedTime.Caption := Format('Elapsed: %d hrs, %d min, %d sec', [
    VpClock.ElapsedHours, VpClock.ElapsedMinutes, VpClock.ElapsedSeconds
  ]);
end;

procedure TForm1.BtnStartStopClick(Sender: TObject);
var
  isStarted: Boolean;
  willStart: Boolean;
begin
  isStarted := VpClock.Active;
  willStart := not isStarted;

  if willStart and (RgClockMode.ItemIndex = ord(cmCountdownTimer)) then
    RgClockModeClick(nil);

  VpClock.Active := willStart;
  if VpClock.Active then
    BtnStartStop.Caption := 'Stop' else
    BtnStartStop.Caption := 'Start';
end;

procedure TForm1.CbMilitaryTimeChange(Sender: TObject);
var
  t: TDateTime;
begin
  t := VpClock.Time;
  VpClock.DigitalOptions.MilitaryTime := CbMilitaryTime.Checked;
  VpClock.Time := t;
end;

procedure TForm1.CbNewClockFaceChange(Sender: TObject);
begin
  if CbNewClockFace.Checked then begin
    VpClock.AnalogOptions.ClockFace.LoadFromFile('clockface.bmp');
    VpClock.AnalogOptions.HourHandWidth := 2;
    VpClock.AnalogOptions.MinuteHandWidth := 2;
    VpClock.AnalogOptions.SecondHandWidth := 1;
    VpClock.Width := 100;
    VpClock.Height := 100;
  end else begin
    VpClock.AnalogOptions.ClockFace := nil;
    VpClock.AnalogOptions.HourHandWidth := 4;
    VpClock.AnalogOptions.MinuteHandWidth := 3;
    VpClock.AnalogOptions.SecondHandWidth := 1;
    VpClock.Width := 200;
    VpClock.Height := 200;
  end;
  VpClock.AnalogOptions.DrawMarks := not CbNewClockFace.Checked;
  if RgDisplayMode.ItemIndex = ord(dmAnalog) then
    VpClock.Invalidate;
end;

procedure TForm1.EdCountDownTimeChange(Sender: TObject);
var
  t: TTime;
begin
  if VpClock.ClockMode = cmCountDownTimer then
    if TryStrToTime(EdCountdownTime.Text, t) then
      VpClock.Time := t;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  w: Integer;
begin
  RgDisplayMode.AutoSize := false;
  RgClockMode.AutoSize := false;
  w := Max(RgDisplayMode.Width, RgClockMode.Width);
  RgDisplayMode.Width := w;
  RgClockMode.Width := w;

  AutoSize := true;
end;

procedure TForm1.RgClockModeClick(Sender: TObject);
var
  h,m,s,ms: Word;
begin
  VpClock.Active := false;
  BtnStartStop.Caption := 'Start';
  VpClock.ClockMode := TVpClockMode(RgClockMode.ItemIndex);
  case VpClock.ClockMode of
    cmClock:
      begin
        VpClock.Time := now;
        VpClock.Active := true;
        BtnStartStop.Caption := 'Stop';
      end;
    cmTimer:
      VpClock.Time := 0;
    cmCountdownTimer:
      begin
        DecodeTime(StrToTime(EdCountDownTime.Text), h,m,s,ms);
        VpClock.HourOffset := h;
        VpClock.MinuteOffset := m;
        VpClock.SecondOffset := s;
      end;
  end;
  EdCountDownTime.Visible := VpClock.ClockMode = cmCountDownTimer;
  LblCountDownTime.Visible := EdCountDownTime.Visible;
end;

procedure TForm1.RgDisplayModeClick(Sender: TObject);
var
  t: TDateTime;
begin
  t := VpClock.Time;
  VpClock.DisplayMode := TVpClockDisplayMode(RgDisplayMode.ItemIndex);
  case VpClock.DisplayMode of
    dmAnalog:
      CbNewClockFaceChange(nil);
    dmDigital:
      begin
        VpClock.Width := 136;
        VpClock.Height := 30;
      end;
  end;
  CbMilitaryTime.Visible := VpClock.DisplayMode = dmDigital;
  CbNewClockface.Visible := VpClock.DisplayMode = dmAnalog;

  VpClock.Time := t;
end;

procedure TForm1.VpClockCountdownDone(Sender: TObject);
begin
  ShowMessage('Countdown completed.');
end;


end.

