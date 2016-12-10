unit BColorPicker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, //LMessages,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Forms,
  mbTrackBarPicker, HTMLColors;

type

  { TBColorPicker }

  TBColorPicker = class(TmbTrackBarPicker)
  private
    FRed, FGreen, FBlue: integer;
    function ArrowPosFromBlue(b: integer): integer;
    function BlueFromArrowPos(p: integer): integer;
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
    property Red: integer read FRed write SetRed default 122;
    property Green: integer read FGreen write SetGreen default 122;
    property Blue: integer read FBlue write SetBlue default 255;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
    property Layout default lyVertical;
  end;

procedure Register;

implementation

{$IFDEF FPC}
  {$R BColorPicker.dcr}
{$ENDIF}

uses
  mbUtils;

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TBColorPicker]);
end;

{TBColorPicker}

constructor TBColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 12;
  SetInitialBounds(0, 0, 22, 268);
  {
  Width := 22;
  Height := 268;
  }
  Layout := lyVertical;
  FRed := 122;
  FGreen := 122;
  FBlue := 255;
  FArrowPos := ArrowPosFromBlue(255);
  FChange := false;
  SetBlue(255);
  HintFormat := 'Blue: %value';
  FManual := false;
  FChange := true;
end;

function TBColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := RGB(FRed, FGreen, AValue);
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

function TBColorPicker.ArrowPosFromBlue(b: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*b);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    b := 255 - b;
    a := Round(((Height - 12)/255)*b);
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
    b := Round(p/((Width - 12)/255))
  else
    b := Round(255 - p/((Height - 12)/255));
  Clamp(b, 0, 255);
  Result := b;
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

procedure TBColorPicker.SetSelectedColor(c: TColor);
begin
  if WebSafe then c := GetWebSafe(c);
  FChange := false;
  SetRed(GetRValue(c));
  SetGreen(GetGValue(c));
  SetBlue(GetBValue(c));
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function TBColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromBlue(FBlue);
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

end.
