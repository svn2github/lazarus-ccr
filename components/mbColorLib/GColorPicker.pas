unit GColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Graphics, Forms,
  HTMLColors, mbTrackBarPicker;

type
  TGColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromGreen(g: integer): integer;
    function GetSelectedColor: TColor;
    function GreenFromArrowPos(p: integer): integer;
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
    property Red: integer read FRed write SetRed default 128;
    property Green: integer read FGreen write SetGreen default 255;
    property Blue: integer read FBlue write SetBlue default 128;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
    property Layout default lyVertical;
  end;


implementation

uses
  mbUtils;

{TGColorPicker}

constructor TGColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 1;
  FRed := 128;
  FGreen := 255;
  FBlue := 128;
  FArrowPos := ArrowPosFromGreen(255);
  FChange := false;
  Layout := lyVertical;
  SetGreen(255);
  HintFormat := 'Green: %value (selected)';
  FManual := false;
  FChange := true;
end;

function TGColorPicker.ArrowPosFromGreen(g: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*g);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    g := 255 - g;
    a := Round(((Height - 12)/255)*g);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

procedure TGColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetGreen(FGreen);
    TBA_MouseMove:
      FGreen := GreenFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FGreen := GreenFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FGreen := GreenFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetGreen(FGreen + Increment);
    TBA_WheelDown:
      SetGreen(FGreen - Increment);
    TBA_VKRight:
      SetGreen(FGreen + Increment);
    TBA_VKCtrlRight:
      SetGreen(255);
    TBA_VKLeft:
      SetGreen(FGreen - Increment);
    TBA_VKCtrlLeft:
      SetGreen(0);
    TBA_VKUp:
      SetGreen(FGreen + Increment);
    TBA_VKCtrlUp:
      SetGreen(255);
    TBA_VKDown:
      SetGreen(FGreen - Increment);
    TBA_VKCtrlDown:
      SetGreen(0);
    else
      inherited;
  end;
end;

function TGColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromGreen(FGreen);
end;

function TGColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := RGB(FRed, AValue, FBlue);
end;

function TGColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := RGB(FRed, FGreen, FBlue)
  else
    Result := GetWebSafe(RGB(FRed, FGreen, FBlue));
end;

function TGColorPicker.GetSelectedValue: integer;
begin
  Result := FGreen;
end;

function TGColorPicker.GreenFromArrowPos(p: integer): integer;
var
  g: integer;
begin
  if Layout = lyHorizontal then
    g := Round(p/((Width - 12)/255))
  else
    g := Round(255 - p/((Height - 12)/255));
  Clamp(g, 0, 255);
  Result := g;
end;

procedure TGColorPicker.SetBlue(b: integer);
begin
  Clamp(b, 0, 255);
  if FBlue <> b then
  begin
    FBlue := b;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TGColorPicker.SetGreen(g: integer);
begin
  Clamp(g, 0, 255);
  if FGreen <> g then
  begin
    FGreen := g;
    FArrowPos := ArrowPosFromGreen(g);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TGColorPicker.SetRed(r: integer);
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

procedure TGColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FChange := false;
  SetRed(GetRValue(c));
  SetBlue(GetBValue(c));
  SetGreen(GetGValue(c));
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

end.
