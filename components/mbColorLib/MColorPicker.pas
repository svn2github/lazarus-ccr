unit MColorPicker;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Forms,
  RGBCMYKUtils, mbTrackBarPicker, HTMLColors; //, Scanlines;

type
  TMColorPicker = class(TmbTrackBarPicker)
  private
    FCyan, FMagenta, FYellow, FBlack: integer;
    function ArrowPosFromMagenta(m: integer): integer;
    function MagentaFromArrowPos(p: integer): integer;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(c: TColor);
    procedure SetCyan(c: integer);
    procedure SetMagenta(m: integer);
    procedure SetYellow(y: integer);
    procedure SetBlack(k: integer);
  protected
    procedure Execute(tbaAction: integer); override;
    function GetArrowPos: integer; override;
    function GetGradientColor(AValue: Integer): TColor; override;
    function GetSelectedValue: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Cyan: integer read FCyan write SetCyan default 0;
    property Magenta: integer read FMagenta write SetMagenta default 255;
    property Yellow: integer read FYellow write SetYellow default 0;
    property Black: integer read FBlack write SetBlack default 0;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
    property Layout default lyVertical;
  end;

implementation

uses
  mbUtils;

{TMColorPicker}

constructor TMColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FGradientWidth := 256;
  FGradientHeight := 1;
  FCyan := 0;
  FMagenta := 255;
  FYellow := 0;
  FBlack := 0;
  FArrowPos := ArrowPosFromMagenta(255);
  FChange := false;
  Layout := lyVertical;
  SetMagenta(255);
  HintFormat := 'Magenta: %value (selected)';
  FManual := false;
  FChange := true;
end;


function TMColorPicker.GetGradientColor(AValue: Integer): TColor;
begin
  Result := CMYKtoColor(FCyan, AValue, FYellow, FBlack);
end;

procedure TMColorPicker.SetMagenta(m: integer);
begin
  Clamp(m, 0, 255);
  if FMagenta <> m then
  begin
    FMagenta := m;
    FArrowPos := ArrowPosFromMagenta(m);
    FManual := false;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TMColorPicker.SetCyan(c: integer);
begin
  Clamp(c, 0, 255);
  if FCyan <> c then
  begin
    FCyan := c;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TMColorPicker.SetYellow(y: integer);
begin
  Clamp(y, 0, 255);
  if FYellow <> y then
  begin
    FYellow := y;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TMColorPicker.SetBlack(k: integer);
begin
  Clamp(k, 0, 255);
  if FBlack <> k then
  begin
    FBlack := k;
    FManual := false;
    CreateGradient;
    Invalidate;
    if FChange and Assigned(OnChange) then OnChange(Self);
  end;
end;

function TMColorPicker.ArrowPosFromMagenta(m: integer): integer;
var
  a: integer;
begin
  if Layout = lyHorizontal then
  begin
    a := Round(((Width - 12)/255)*m);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    m := 255 - m;
    a := Round(((Height - 12)/255)*m);
    if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TMColorPicker.MagentaFromArrowPos(p: integer): integer;
var
  m: integer;
begin
  if Layout = lyHorizontal then
    m := Round(p/((Width - 12)/255))
  else
    m := Round(255 - p/((Height - 12)/255));
  Clamp(m, 0, 255);
  Result := m;
end;

function TMColorPicker.GetSelectedColor: TColor;
begin
  Result := CMYKtoColor(FCyan, FMagenta, FYellow, FBlack);
  if WebSafe then
    Result := GetWebSafe(Result);
end;

function TMColorPicker.GetSelectedValue: integer;
begin
  Result := FMagenta;
end;

procedure TMColorPicker.SetSelectedColor(c: TColor);
var
  cy, m, y, k: integer;
begin
  if WebSafe then c := GetWebSafe(c);
  ColorToCMYK(c, cy, m, y, k);
  FChange := false;
  SetCyan(cy);
  SetYellow(y);
  SetBlack(k);
  SetMagenta(m);
  FManual := false;
  FChange := true;
  if Assigned(OnChange) then OnChange(Self);
end;

function TMColorPicker.GetArrowPos: integer;
begin
  Result := ArrowPosFromMagenta(FMagenta);
end;

procedure TMColorPicker.Execute(tbaAction: integer);
begin
  case tbaAction of
    TBA_Resize:
      SetMagenta(FMagenta);
    TBA_MouseMove:
      FMagenta := MagentaFromArrowPos(FArrowPos);
    TBA_MouseDown:
      FMagenta := MagentaFromArrowPos(FArrowPos);
    TBA_MouseUp:
      FMagenta := MagentaFromArrowPos(FArrowPos);
    TBA_WheelUp:
      SetMagenta(FMagenta + Increment);
    TBA_WheelDown:
      SetMagenta(FMagenta - Increment);
    TBA_VKRight:
      SetMagenta(FMagenta + Increment);
    TBA_VKCtrlRight:
      SetMagenta(255);
    TBA_VKLeft:
      SetMagenta(FMagenta - Increment);
    TBA_VKCtrlLeft:
      SetMagenta(0);
    TBA_VKUp:
      SetMagenta(FMagenta + Increment);
    TBA_VKCtrlUp:
      SetMagenta(255);
    TBA_VKDown:
      SetMagenta(FMagenta - Increment);
    TBA_VKCtrlDown:
      SetMagenta(0);
    else
      inherited;
  end;
end;

end.
