unit JvJanLed;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons;

type
  TLedColor = (lcRed, lcGreen, lcYellow, lcBlue, lcPurple);

  TJvJanLed = class(TGraphicControl)
  private
    { Private declarations }
    FLit: boolean;
    FLedColor: TLedColor;
    procedure SetLit(const AValue: boolean);
    procedure SetLedColor(const AValue: TLedColor);
    {
    procedure KeepSize(Sender: TObject; var ANewWidth, ANewHeight: Integer;
      var AResize: Boolean); }

  protected
    { Protected declarations }
    procedure Paint; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property Lit: boolean read FLit write SetLit default false;
    property LedColor: TLedColor read FLedColor write SetLedColor default lcRed;
  end;


implementation


{ TJvJanLed }

constructor TJvJanLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 12;
  Height := 13;
  FLit := false;
  FLedColor := lcRed;
  {OnCanResize := @KeepSize;}
end;

destructor TJvJanLed.Destroy;
begin
 //mycode
  inherited Destroy;
end;

procedure TJvJanLed.Paint;
var
  surfCol, litCol: TColor;
begin
  if Flit then begin
    case FLedColor of
      lcRed: begin surfCol := clRed; litCol := clWhite; end;
      lcGreen: begin surfCol := clLime; litCol := clWhite; end;
      lcYellow: begin surfCol := clYellow; litCol := clWhite; end;
      lcBlue: begin surfCol := clAqua; litCol := clWhite; end;
      lcPurple: begin surfCol:= clFuchsia; litCol := clWhite; end;
    end;
  end
  else begin
    case FLedColor of
      lcRed: begin surfCol := clMaroon; litCol := clred; end;
      lcGreen: begin surfCol := clGreen; litCol := clLime; end;
      lcYellow: begin surfCol := clOlive; litCol := clYellow; end;
      lcBlue: begin surfCol := clNavy; litCol := clAqua; end;
      lcPurple: begin surfCol := clPurple; litCol := clFuchsia; end;
    end;
  end;
  with Canvas do begin
    Brush.Color := clsilver;
    FillRect(0, 0, 12, 13);
    Brush.Style := bsClear;
    Pen.Color := clGray;
    Ellipse(0, 0, 12, 13);
    Pen.Color := clBlack;
    Brush.Color := surfCol;
    Ellipse(1, 1, 11, 12);
    Pen.Color := clWhite;
    Arc(1, 1, 11, 12, 0, 12, 12, 0);
    Pen.Color := litCol;
    Arc(3, 3, 8, 9, 5, 0, 0, 8);
  end;
end;
                           {
procedure TJvJanLed.KeepSize(Sender: TObject; var ANewWidth, ANewHeight: Integer;
  var AResize: Boolean);
begin
  AResize := True;
  ANewWidth := Width;
  ANewHeight := Height;
end;                        }

procedure TJvJanLed.SetLit(const AValue: boolean);
begin
  if AValue <> FLit then begin
    FLit := AValue;
    Refresh;
  end;
end;

procedure TJvJanLed.SetLedColor(const AValue: TLedColor);
begin
  if AValue <> FLedColor then begin
    FLedColor := AValue;
    Refresh;
  end;
end;

end.
