unit main;

//{$DEFINE TICKER}

{$IFDEF LCL}
 {$MODE DELPHI}
 {$IFNDEF WINDOWS}{$UNDEF TICKER}{$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF LCL}
  LCLIntf,
  {$ELSE}
  Windows, Messages, ShellApi,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Spin, A3nalogGauge;

type

  { TMainForm }

  TMainForm = class(TForm)
    StartStopButton: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StyleBox: TComboBox;
    AAModeLabel1: TLabel;
    Timer: TTimer;
    ColorDialog: TColorDialog;
    FPSLabel: TLabel;
    FreqLabel: TLabel;
    FreqEdit: TSpinEdit;

    FaceColorButton: TButton;
    CenterColorButton: TButton;
    CircleColorButton: TButton;
    MinimLabel: TLabel;
    MinimEdit: TSpinEdit;
    MaximLabel: TLabel;
    MaximEdit: TSpinEdit;
    MinColorButton: TButton;
    MidColorButton: TButton;
    MaxColorButton: TButton;
    TicksColorButton: TButton;
    ValueColorButton: TButton;
    CaptionColorButton: TButton;
    ArrowColorButton: TButton;
    MarginColorButton: TButton;
    MarginLabel: TLabel;
    MarginEdit: TSpinEdit;
    CenterLabel: TLabel;
    CenterRadEdit: TSpinEdit;
    CircleLabel: TLabel;
    CircleRadEdit: TSpinEdit;
    ScaleLabel: TLabel;
    ScaleEdit: TSpinEdit;
    AngleLabel: TLabel;
    AngleEdit: TSpinEdit;
    WidthLabel: TLabel;
    WidthEdit: TSpinEdit;
    NumMainLabel: TLabel;
    NumMainEdit: TSpinEdit;
    MainLenLabel: TLabel;
    MainLenEdit: TSpinEdit;
    SubLenLabel: TLabel;
    SubLenEdit: TSpinEdit;
    MarginBox: TCheckBox;
    MainTicksBox: TCheckBox;
    SubticksBox: TCheckBox;
    IndMinBox: TCheckBox;
    IndMidBox: TCheckBox;
    IndMaxBox: TCheckBox;
    CirclesBox: TCheckBox;
    ValuesBox: TCheckBox;
    CenterBox: TCheckBox;
    FrameBox: TCheckBox;
    Draw3DBox: TCheckBox;
    CaptionBox: TCheckBox;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    CloseButton: TButton;
    AboutLabel: TLabel;
    AAModeBox: TComboBox;
    AAModeLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure StyleBoxChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    {$IFDEF TICKER}
    procedure FramesChanged(Sender: TObject);
    {$ENDIF}

    procedure AAModeBoxChange(Sender: TObject);
    procedure FreqEditChange(Sender: TObject);
    procedure FaceColorButtonClick(Sender: TObject);
    procedure CenterColorButtonClick(Sender: TObject);
    procedure CircleColorButtonClick(Sender: TObject);
    procedure MinimEditChange(Sender: TObject);
    procedure MaximEditChange(Sender: TObject);
    procedure MinColorButtonClick(Sender: TObject);
    procedure MidColorButtonClick(Sender: TObject);
    procedure MaxColorButtonClick(Sender: TObject);
    procedure TicksColorButtonClick(Sender: TObject);
    procedure ValueColorButtonClick(Sender: TObject);
    procedure CaptionColorButtonClick(Sender: TObject);
    procedure ArrowColorButtonClick(Sender: TObject);
    procedure MarginColorButtonClick(Sender: TObject);
    procedure MarginEditChange(Sender: TObject);
    procedure CenterRadEditChange(Sender: TObject);
    procedure CircleRadEditChange(Sender: TObject);
    procedure ScaleEditChange(Sender: TObject);
    procedure AngleEditChange(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure NumMainEditChange(Sender: TObject);
    procedure MainLenEditChange(Sender: TObject);
    procedure SubLenEditChange(Sender: TObject);
    procedure MarginBoxClick(Sender: TObject);
    procedure MainTicksBoxClick(Sender: TObject);
    procedure SubticksBoxClick(Sender: TObject);
    procedure IndMinBoxClick(Sender: TObject);
    procedure IndMidBoxClick(Sender: TObject);
    procedure IndMaxBoxClick(Sender: TObject);
    procedure CirclesBoxClick(Sender: TObject);
    procedure ValuesBoxClick(Sender: TObject);
    procedure CenterBoxClick(Sender: TObject);
    procedure FrameBoxClick(Sender: TObject);
    procedure Draw3DBoxClick(Sender: TObject);
    procedure CaptionBoxClick(Sender: TObject);
    procedure CaptionEditChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure AboutLabelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure AboutLabelClick(Sender: TObject);
  private
    AnalogGauge1: TA3nalogGauge;
    AnalogGauge2: TA3nalogGauge;
    AnalogGauge3: TA3nalogGauge;
    FDelta: Double;
    FStartTime: TDateTime;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
 {$DEFINE TICKER}
{$ENDIF}

const
  BASE_CAPTION = 'AntiAliased Analog Gauge demo';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AnalogGauge1 := TA3nalogGauge.Create(self);
  with AnalogGauge1 do begin
    Parent := Panel3;
    Left := 8;
    Top := 16;
    Width := 278;
    Height := 200; //245;
    Anchors := [akLeft, akTop, akBottom];
    Angle := 90;
    Caption := 'mV';
    AntiAliased := aaNone;
    CaptionFont.Style := [fsBold];
  end;
  AnalogGauge2 := TA3nalogGauge.Create(self);
  with AnalogGauge2 do begin
    Parent := Panel3;
    Left := 295;
    Top := 16;
    Width := 185;
    Height := 200;
    Anchors := [akLeft, akRight, akTop, akBottom];
    Caption := 'mV';
    AntiAliased := aaNone;
    CaptionFont.Style := [fsBold];
  end;
  AnalogGauge3 := TA3nalogGauge.Create(self);
  with AnalogGauge3 do begin
    Parent := Panel3;
    Left := 490;
    Top := 16;
    Width := 278;
    Height := 200; //245;
    Anchors := [akRight, akTop, akBottom];
    Angle := 180;
    Caption := 'mV';
    AntiAliased := aaNone;
    CaptionFont.Style := [fsBold];
  end;

  CenterRadEdit.Value := AnalogGauge1.CenterRadius;
  CircleRadEdit.Value := AnalogGauge1.CircleRadius;
  MarginEdit.Value := AnalogGauge1.Margin;
  ScaleEdit.Value := AnalogGauge1.Scale;
  AngleEdit.Value := AnalogGauge1.Angle;
  MinimEdit.Value := AnalogGauge1.IndMinimum;
  MaximEdit.Value := AnalogGauge1.IndMaximum;
  WidthEdit.Value := AnalogGauge1.ArrowWidth;
  NumMainEdit.Value := AnalogGauge1.NumberMainTicks;
  MainLenEdit.Value := AnalogGauge1.LengthMainTicks;
  SubLenEdit.Value := AnalogGauge1.LengthSubTicks;
  MarginBox.Checked := foShowMargin in AnalogGauge1.FaceOptions;
  CirclesBox.Checked := foShowCircles in AnalogGauge1.FaceOptions;
  MainTicksBox.Checked := foShowMainTicks in AnalogGauge1.FaceOptions;
  SubTicksBox.Checked := foShowSubTicks in AnalogGauge1.FaceOptions;
  IndMinBox.Checked := foShowIndicatorMin in AnalogGauge1.FaceOptions;
  IndMidBox.Checked := foShowIndicatorMid in AnalogGauge1.FaceOptions;
  IndMaxBox.Checked := foShowIndicatorMax in AnalogGauge1.FaceOptions;
  ValuesBox.Checked := foShowValues in AnalogGauge1.FaceOptions;
  CenterBox.Checked := foShowCenter in AnalogGauge1.FaceOptions;
  FrameBox.Checked := foShowFrame in AnalogGauge1.FaceOptions;
  Draw3DBox.Checked := foShow3D in AnalogGauge1.FaceOptions;
  CaptionBox.Checked := foShowCaption in AnalogGauge1.FaceOptions;
  CaptionEdit.Text := AnalogGauge1.Caption;
  FreqEdit.Value := Timer.Interval;
  AAModeBox.ItemIndex := 0;
  {$IFDEF TICKER}
  AnalogGauge1.OnFrames := FramesChanged;
  {$ENDIF}

  FStartTime := Now;
end;

procedure TMainForm.StartStopButtonClick(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then begin
    FStartTime := now;
    StartStopButton.Caption := 'Stop';
    AnalogGauge1.Position := 0;
  end else begin
    Caption := BASE_CAPTION + ' (' + FormatDateTime('n:ss.zzz', Now - FStartTime) + ')';
    StartStopButton.Caption := 'Start';
  end;
end;

procedure TMainForm.StyleBoxChange(Sender: TObject);
begin
  AnalogGauge1.Style := TStyle(StyleBox.ItemIndex);
  AnalogGauge2.Style := TStyle(StyleBox.ItemIndex);
  AnalogGauge3.Style := TStyle(StyleBox.ItemIndex);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  V: Double;
begin
  V := AnalogGauge1.Position;
  if FDelta = 0 then FDelta := 1;
  V := V + FDelta;
  if V < 0 then begin
    V := 0;
    FDelta := -FDelta
  end else
  if V > AnalogGauge1.Scale then begin
    V := AnalogGauge1.Scale;
    FDelta := -FDelta
  end;
  AnalogGauge1.Position := V;
  AnalogGauge2.Position := V;
  AnalogGauge3.Position := V;
end;

{$IFDEF TICKER}
procedure TMainForm.FramesChanged(Sender: TObject);
begin
  FPSLabel.Caption := Format('FPS: %d', [AnalogGauge1.Frames]);
end;
{$ENDIF}

procedure TMainForm.AAModeBoxChange(Sender: TObject);
var
  AA: TAntialiased;
begin
  case AAModeBox.ItemIndex of
    1: AA := aaBiline;
    2: AA := aaTriline;
    3: AA := aaQuadral;
    else AA := aaNone
  end;
  AnalogGauge1.AntiAliased := AA;
  AnalogGauge2.AntiAliased := AA;
  AnalogGauge3.AntiAliased := AA;
end;

procedure TMainForm.FreqEditChange(Sender: TObject);
begin
  Timer.Interval := FreqEdit.Value
end;

procedure TMainForm.FaceColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.FaceColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.FaceColor := ColorDialog.Color;
    AnalogGauge2.FaceColor := ColorDialog.Color;
    AnalogGauge3.FaceColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.CenterColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.CenterColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.CenterColor := ColorDialog.Color;
    AnalogGauge2.CenterColor := ColorDialog.Color;
    AnalogGauge3.CenterColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.CircleColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.CircleColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.CircleColor := ColorDialog.Color;
    AnalogGauge2.CircleColor := ColorDialog.Color;
    AnalogGauge3.CircleColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MinimEditChange(Sender: TObject);
begin
  if MinimEdit.Text <> '' then begin
    AnalogGauge1.IndMinimum := MinimEdit.Value;
    AnalogGauge2.IndMinimum := MinimEdit.Value;
    AnalogGauge3.IndMinimum := MinimEdit.Value;
  end;
end;

procedure TMainForm.MaximEditChange(Sender: TObject);
begin
  if MaximEdit.Text <> '' then begin
    AnalogGauge1.IndMaximum := MaximEdit.Value;
    AnalogGauge2.IndMaximum := MaximEdit.Value;
    AnalogGauge3.IndMaximum := MaximEdit.Value;
  end;
end;

procedure TMainForm.MinColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MinColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MinColor := ColorDialog.Color;
    AnalogGauge2.MinColor := ColorDialog.Color;
    AnalogGauge3.MinColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MidColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MidColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MidColor := ColorDialog.Color;
    AnalogGauge2.MidColor := ColorDialog.Color;
    AnalogGauge3.MidColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MaxColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MaxColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MaxColor := ColorDialog.Color;
    AnalogGauge2.MaxColor := ColorDialog.Color;
    AnalogGauge3.MaxColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.TicksColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.TicksColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.TicksColor := ColorDialog.Color;
    AnalogGauge2.TicksColor := ColorDialog.Color;
    AnalogGauge3.TicksColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.ValueColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.ValueColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.ValueColor := ColorDialog.Color;
    AnalogGauge2.ValueColor := ColorDialog.Color;
    AnalogGauge3.ValueColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.CaptionColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.CaptionColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.CaptionColor := ColorDialog.Color;
    AnalogGauge2.CaptionColor := ColorDialog.Color;
    AnalogGauge3.CaptionColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.ArrowColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.ArrowColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.ArrowColor := ColorDialog.Color;
    AnalogGauge2.ArrowColor := ColorDialog.Color;
    AnalogGauge3.ArrowColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MarginColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MarginColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MarginColor := ColorDialog.Color;
    AnalogGauge2.MarginColor := ColorDialog.Color;
    AnalogGauge3.MarginColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MarginEditChange(Sender: TObject);
begin
  if MarginEdit.Text <> '' then begin
    AnalogGauge1.Margin := MarginEdit.Value;
    AnalogGauge2.Margin := MarginEdit.Value;
    AnalogGauge3.Margin := MarginEdit.Value;
  end;
end;

procedure TMainForm.CenterRadEditChange(Sender: TObject);
begin
  if CenterRadEdit.Text <> '' then begin
    AnalogGauge1.CenterRadius := CenterRadEdit.Value;
    AnalogGauge2.CenterRadius := CenterRadEdit.Value;
    AnalogGauge3.CenterRadius := CenterRadEdit.Value;
  end;
end;

procedure TMainForm.CircleRadEditChange(Sender: TObject);
begin
  if CircleRadEdit.Text <> '' then begin
    AnalogGauge1.CircleRadius := CircleRadEdit.Value;
    AnalogGauge2.CircleRadius := CircleRadEdit.Value;
    AnalogGauge3.CircleRadius := CircleRadEdit.Value;
  end;
end;

procedure TMainForm.ScaleEditChange(Sender: TObject);
begin
  if ScaleEdit.Text <> '' then begin
    AnalogGauge1.Scale := ScaleEdit.Value;
    AnalogGauge2.Scale := ScaleEdit.Value;
    AnalogGauge3.Scale := ScaleEdit.Value;
  end;
end;

procedure TMainForm.AngleEditChange(Sender: TObject);
begin
  if AngleEdit.Text <> '' then begin
    AnalogGauge1.Angle := AngleEdit.Value;
    AnalogGauge2.Angle := AngleEdit.Value;
    AnalogGauge3.Angle := AngleEdit.Value;
  end;
end;

procedure TMainForm.WidthEditChange(Sender: TObject);
begin
  if WidthEdit.Text <> '' then begin
    AnalogGauge1.ArrowWidth := WidthEdit.Value;
    AnalogGauge2.ArrowWidth := WidthEdit.Value;
    AnalogGauge3.ArrowWidth := WidthEdit.Value;
  end;
end;

procedure TMainForm.NumMainEditChange(Sender: TObject);
begin
  if NumMainEdit.Text <> '' then begin
    AnalogGauge1.NumberMainTicks := NumMainEdit.Value;
    AnalogGauge2.NumberMainTicks := NumMainEdit.Value;
    AnalogGauge3.NumberMainTicks := NumMainEdit.Value;
  end;
end;

procedure TMainForm.MainLenEditChange(Sender: TObject);
begin
  if MainLenEdit.Text <> '' then begin
    AnalogGauge1.LengthMainTicks := MainLenEdit.Value;
    AnalogGauge2.LengthMainTicks := MainLenEdit.Value;
    AnalogGauge3.LengthMainTicks := MainLenEdit.Value;
  end;
end;

procedure TMainForm.SubLenEditChange(Sender: TObject);
begin
  if SubLenEdit.Text <> '' then begin
    AnalogGauge1.LengthSubTicks := SubLenEdit.Value;
    AnalogGauge2.LengthSubTicks := SubLenEdit.Value;
    AnalogGauge3.LengthSubTicks := SubLenEdit.Value;
  end;
end;

procedure TMainForm.MarginBoxClick(Sender: TObject);
begin
  if MarginBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowMargin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowMargin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowMargin];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowMargin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowMargin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowMargin];
  end;
end;

procedure TMainForm.MainTicksBoxClick(Sender: TObject);
begin
  if MainTicksBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowMainTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowMainTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowMainTicks];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowMainTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowMainTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowMainTicks];
  end;
end;

procedure TMainForm.SubTicksBoxClick(Sender: TObject);
begin
  if SubTicksBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowSubTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowSubTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowSubTicks];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowSubTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowSubTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowSubTicks];
  end;
end;

procedure TMainForm.IndMinBoxClick(Sender: TObject);
begin
  if IndMinBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowIndicatorMin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowIndicatorMin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowIndicatorMin];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowIndicatorMin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowIndicatorMin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowIndicatorMin];
  end;
end;

procedure TMainForm.IndMidBoxClick(Sender: TObject);
begin
  if IndMidBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowIndicatorMid];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowIndicatorMid];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowIndicatorMid];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowIndicatorMid];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowIndicatorMid];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowIndicatorMid];
  end;
end;

procedure TMainForm.IndMaxBoxClick(Sender: TObject);
begin
  if IndMaxBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowIndicatorMax];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowIndicatorMax];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowIndicatorMax];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowIndicatorMax];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowIndicatorMax];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowIndicatorMax];
  end;
end;

procedure TMainForm.CirclesBoxClick(Sender: TObject);
begin
  if CirclesBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowCircles];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowCircles];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowCircles];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowCircles];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowCircles];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowCircles];
  end;
end;

procedure TMainForm.ValuesBoxClick(Sender: TObject);
begin
  if ValuesBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowValues];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowValues];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowValues];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowValues];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowValues];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowValues];
  end;
end;

procedure TMainForm.CenterBoxClick(Sender: TObject);
begin
  if CenterBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowCenter];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowCenter];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowCenter];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowCenter];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowCenter];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowCenter];
  end;
end;

procedure TMainForm.FrameBoxClick(Sender: TObject);
begin
  if FrameBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowFrame];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowFrame];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowFrame];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowFrame];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowFrame];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowFrame];
  end;
end;

procedure TMainForm.Draw3DBoxClick(Sender: TObject);
begin
  if Draw3DBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShow3D];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShow3D];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShow3D];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShow3D];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShow3D];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShow3D];
  end;
end;

procedure TMainForm.CaptionBoxClick(Sender: TObject);
begin
  if CaptionBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [foShowCaption];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [foShowCaption];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [foShowCaption];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [foShowCaption];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [foShowCaption];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [foShowCaption];
  end;
end;

procedure TMainForm.CaptionEditChange(Sender: TObject);
begin
  AnalogGauge1.Caption := CaptionEdit.Text;
  AnalogGauge2.Caption := CaptionEdit.Text;
  AnalogGauge3.Caption := CaptionEdit.Text;
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close
end;

procedure TMainForm.AboutLabelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Control: TLabel;
begin
  Control := Sender as TLabel;
  if (X > 0) and (X < Control.Width) and
     (Y > 0) and (Y < Control.Height) then begin
    {$IFDEF LCL}
    Screen.Cursor := crHandPoint
    {$ELSE}
    Control.Font.Style := Control.Font.Style + [fsUnderLine];
    Control.Cursor := crHandPoint;
    Windows.SetCursor(Screen.Cursors[Control.Cursor]);
    SetCaptureControl(Control);
    {$ENDIF}
  end else begin
    {$IFDEF LCL}
    Screen.Cursor := crDefault;
    {$ELSE}
    Control.Font.Style := Control.Font.Style - [fsUnderLine];
    Control.Cursor := crDefault;
    SetCaptureControl(nil);
    {$ENDIF}
  end;
end;


procedure TMainForm.AboutLabelClick(Sender: TObject);
var
  Control: TLabel;
begin
  Control := Sender as TLabel;
  Control.Font.Style := Control.Font.Style - [fsUnderLine];
  Control.Cursor := crDefault; SetCaptureControl(nil);
  {$IFDEF LCL}
  OpenURL('http://irnis.net/');
  {$ELSE}
  ShellExecute(0, nil, PChar('http://www.irnis.net/'), nil, nil, SW_SHOWDEFAULT);
  {$ENDIF}
end;

end.

