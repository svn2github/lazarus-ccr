unit RColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Forms,
  mbTrackBarPicker, HTMLColors, Scanlines;

type

  { TRColorPicker }

  TRColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromRed(r: integer): integer;
    function RedFromArrowPos(p: integer): integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
    procedure SetRed(r: integer);
    procedure SetGreen(g: integer);
    procedure SetBlue(b: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Red: integer read FRed write SetRed default 255;
    property Green: integer read FGreen write SetGreen default 122;
    property Blue: integer read FBlue write SetBlue default 122;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
    property Layout default lyVertical;
  end;


implementation

uses
  mbUtils;


{TRColorPicker}

constructor TRColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 12;
  FRed := 255;
  FGreen := 122;
  FBlue := 122;
  FArrowPos := ArrowPosFromRed(255);
  FChange := false;
  Layout := lyVertical;
  SetRed(255);
  HintFormat := 'Red: %value (selected)';
  FManual := false;
  FChange := true;
end;

function TRColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := RGB(AValue, FGreen, FBlue);
end;

procedure TRColorPicker.SetRed(r: integer);
begin
  Clamp(r, 0, 255);
  if FRed <> r then
  begin
    FRed := r;
    FArrowPos := ArrowPosFromRed(r);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TRColorPicker.SetGreen(g: integer);
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

procedure TRColorPicker.SetBlue(b: integer);
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

function TRColorPicker.ArrowPosFromRed(r: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*r);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    r := 255 - r;
    a := Round(((Height - 12)/255)*r);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TRColorPicker.RedFromArrowPos(p: integer): integer;
var
  r: integer;
begin
  if Layout = lyHorizontal then
    r := Round(p/((Width - 12)/255))
  else
    r := Round(255 - p/((Height - 12)/255));
  Clamp(r, 0, 255);
  Result := r;
end;

function TRColorPicker.GetSelectedColor: TColor;
begin
  if not WebSafe then
    Result := RGB(FRed, FGreen, FBlue)
  else
    Result := GetWebSafe(RGB(FRed, FGreen, FBlue));
end;

function TRColorPicker.GetSelectedValue: integer;
begin
  Result := FRed;
end;

procedure TRColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FChange := false;
  SetGreen(GetGValue(c));
  SetBlue(GetBValue(c));
  SetRed(GetRValue(c));
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function TRColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromRed(FRed);
end;

procedure TRColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetRed(FRed);
    TBA_MouseMove:
      FRed := RedFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FRed := RedFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FRed := RedFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetRed(FRed + Increment);
    TBA_WheelDown:
      SetRed(FRed - Increment);
    TBA_VKRight:
      SetRed(FRed + Increment);
    TBA_VKCtrlRight:
      SetRed(255);
    TBA_VKLeft:
      SetRed(FRed - Increment);
    TBA_VKCtrlLeft:
      SetRed(0);
    TBA_VKUp:
      SetRed(FRed + Increment);
    TBA_VKCtrlUp:
      SetRed(255);
    TBA_VKDown:
      SetRed(FRed - Increment);
    TBA_VKCtrlDown:
      SetRed(0);
    else
      inherited;
  end;
end;

end.
