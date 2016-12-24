unit BColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbTrackBarPicker;

type

  { TBColorPicker }

  TBColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromBlue(b: integer): integer;
    function BlueFromArrowPos(p: integer): integer;
    function GetSelectedColor: TColor;
    procedure SetBlue(b: integer);
    procedure SetGreen(g: integer);
    procedure SetRed(r: integer);
    procedure SetSelectedColor(c: TColor);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Blue: integer read FBlue write SetBlue default 255;
    property Green: integer read FGreen write SetGreen default 128;
    property Red: integer read FRed write SetRed default 128;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
    property Layout default lyVertical;
  end;


implementation

uses
  mbUtils;


{TBColorPicker}

constructor TBColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 1;
  FRed := 128;
  FGreen := 128;
  FBlue := 255;
  FArrowPos := ArrowPosFromBlue(255);
  FChange := false;
  Layout := lyVertical;
  SetBlue(255);
  HintFormat := 'Blue: %value (selected)';
  FManual := false;
  FChange := true;
end;

function TBColorPicker.ArrowPosFromBlue(b: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round((Width - 12) * b / 255);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    a := Round((Height - 12) * (255 - b) / 255 );
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TBColorPicker.BlueFromArrowPos(p: integer): integer;
var
  b: integer;
begin
  if Layout = lyHorizontal then
    b := Round(p * 255 / (Width - 12))
  else
    b := Round(255 - p * 255 / (Height - 12));
  Clamp(b, 0, 255);
  Result := b;
end;

procedure TBColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetBlue(FBlue);
    TBA_MouseMove:
      FBlue := BlueFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FBlue := BlueFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FBlue := BlueFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetBlue(FBlue + Increment);
    TBA_WheelDown:
      SetBlue(FBlue - Increment);
    TBA_VKRight:
      SetBlue(FBlue + Increment);
    TBA_VKCtrlRight:
      SetBlue(255);
    TBA_VKLeft:
      SetBlue(FBlue - Increment);
    TBA_VKCtrlLeft:
      SetBlue(0);
    TBA_VKUp:
      SetBlue(FBlue + Increment);
    TBA_VKCtrlUp:
      SetBlue(255);
    TBA_VKDown:
      SetBlue(FBlue - Increment);
    TBA_VKCtrlDown:
      SetBlue(0);
    else
      inherited;
  end;
end;

function TBColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromBlue(FBlue);
end;

// Note: AValue is restricted to the range 0..255 by the size of the trackbar.
function TBColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := RGB(FRed, FGreen, AValue);
end;

function TBColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := RGB(FRed, FGreen, FBlue)
  else
    Result := GetWebSafe(RGB(FRed, FGreen, FBlue));
end;

function TBColorPicker.GetSelectedValue: integer;
begin
  Result := FBlue;
end;

procedure TBColorPicker.SetBlue(b: integer);
begin
  Clamp(b, 0, 255);
  if FBlue <> b then
  begin
    FBlue := b;
    FArrowPos := ArrowPosFromBlue(b);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TBColorPicker.SetGreen(g: integer);
begin
  Clamp(g, 0, 255);
  if FGreen <> g then
  begin
    FGreen := g;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TBColorPicker.SetRed(r: integer);
begin
  Clamp(r, 0, 255);
  if FRed <> r then
  begin
    FRed := r;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TBColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  if c = GetSelectedColor then
    exit;
  FChange := false;
  SetRed(GetRValue(c));
  SetGreen(GetGValue(c));
  SetBlue(GetBValue(c));
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

end.
