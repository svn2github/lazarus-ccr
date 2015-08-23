{
 /***************************************************************************
                              ColorPalette.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
    Color palette grid with custom palette support.
    The OnColorPick event is fired when user picks a color.
    The LoadPalette procedure loads custom palette.
    Custom palette example:
    
    $COLS 8
    # sets count of palette grid columns

    0,0,0
    # inserts color r,g,b
    255,255,255

    $NONE
    # inserts empty palette grid cell

    $BLENDWB 128,128,128 3
    # creates color gradient white -> color -> black with specified steps

}
unit ColorPalette;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, LResources, Controls, Forms, Graphics, Math,
  LCLType;
  
type

  TPickMode = (
    pmDefault,    // Select color at mouse-down, ColorPick event at mouse-up if at same pos
    pmImmediate,  // Select color and ColorPick event at mouse-down
    pmContinuous  // Select color at mouse-down and mouse-move, ColorPick event at mouse-up
  );

  TPickShiftEnum = (ssLeft, ssRight, ssMiddle);
  TPickShift = set of TPickShiftEnum;

  TPaletteKind = (pkStandardPalette, pkExtendedPalette, pkSystemPalette,
    pkStandardAndSystemPalette, pkExtendedAndSystemPalette,
    pkGradientPalette, pkWebSafePalette, pkWebSafePalette2);

  TColorMouseEvent = procedure (Sender: TObject; AColor: TColor; Shift: TShiftState) of object;
  TColorPaletteEvent = procedure (Sender: TObject; AColor: TColor) of object;

  { TCustomColorPalette }

  TCustomColorPalette = class(TGraphicControl)
  private
    FButtonHeight: Integer;
    FButtonWidth: Integer;
    FCols: Integer;
    FOnColorMouseMove: TColorMouseEvent;
    FOnColorPick: TColorMouseEvent;
    FOnSelectColor: TColorPaletteEvent;
    FRows: Integer;
    FColors: TList;
    FSelectedColor: TColor;
    FSelectedIndex: Integer;
    FPickMode: TPickMode;
    FPickShift: TPickShift;
    FMousePt: TPoint;
    FMouseIndex: Integer;
    FPrevMouseIndex: Integer;
    FStoredShift: TShiftState;
    FShowColorHint: Boolean;
    FShowSelection: Boolean;
    FSavedHint: String;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FPaletteKind: TPaletteKind;
    FGradientSteps: Byte;
    function GetColorCount: Integer;
    function GetColors(AIndex: Integer): TColor;
    function GetPickedColor: TColor;
    procedure SetBorderColor(const AValue: TColor);
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetColors(AIndex: Integer; const AValue: TColor);
    procedure SetCols(AValue: Integer);
    procedure SetGradientSteps(AValue: Byte);
    procedure SetPaletteKind(AValue: TPaletteKind);
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetShowSelection(AValue: Boolean);

  protected
    procedure BlendWBColor(AColor: TColor; Steps: Integer);
    procedure ColorPick(AIndex: Integer; Shift: TShiftState); dynamic;
    procedure ColorMouseMove(AColor: TColor; Shift: TShiftState); dynamic;
    procedure DoAddColor(AColor: TColor); virtual;
    procedure DoColorPick(AColor: TColor; AShift: TShiftState); virtual;
    procedure DoDeleteColor(AIndex: Integer); virtual;
    procedure DoSelectColor(AColor: TColor); virtual;
    function GetHintText(AColor: TColor): String; virtual;
    function IsCorrectShift(Shift: TShiftState): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift:TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure UpdateSize; virtual;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property ColumnCount: Integer read FCols write SetCols;
    property GradientSteps: Byte read FGradientSteps write SetGradientSteps default 3;
    property PaletteKind: TPaletteKind read FPaletteKind write SetPaletteKind default pkStandardPalette;
    property PickMode: TPickMode read FPickMode write FPickMode default pmImmediate;
    property PickShift: TPickShift read FPickShift write FPickShift default [ssLeft];
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default 0;
    property ShowColorHint: Boolean read FShowColorHint write FShowColorHint default true;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default false;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure AddColor(AColor: TColor);
    procedure ClearColors;
    procedure DeleteColor(AIndex: Integer);
    procedure LoadPalette(const FileName: String);
    procedure SavePalette(const FileName: String);
  
    property Colors[Index: Integer]: TColor read GetColors write SetColors;
    property ColorCount: Integer read GetColorCount;
    property PickedColor: TColor read GetPickedColor; deprecated 'Use SelectedColor';
    property SelectedColor: TColor read FSelectedColor;

    property OnSelectColor: TColorPaletteEvent read FOnSelectColor write FOnSelectColor;
    property OnColorPick: TColorMouseEvent read FOnColorPick write FOnColorPick;
    property OnColorMouseMove: TColorMouseEvent read FOnColorMouseMove write FOnColorMouseMove;
    
    property Height stored False;
    property Width stored False;
  end;
  
  TColorPalette = class(TCustomColorPalette)
  published
    // inherited from TCustomColorPalette
    property BorderColor;
    property BorderWidth;
    property ButtonWidth;
    property ButtonHeight;
    property ColumnCount;
    property GradientSteps;
    property PaletteKind;
    property PickMode;
    property PickShift;
    property SelectedIndex;
    property ShowColorHint;
    property ShowSelection;

    property OnColorMouseMove;
    property OnColorPick;
    property OnSelectColor;

    // inherited from TCustomColorPalette's ancestors
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
  end;
  
  procedure Register;


implementation

uses
  LCLIntf;

procedure Register;
begin
  RegisterComponents('Misc', [TColorPalette]);
end;


{ TCustomColorPalette }

constructor TCustomColorPalette.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csFixedWidth, csFixedHeight];

  FColors := TList.Create;
  FBorderColor := clBlack;
  FBorderWidth := 1;
  FButtonWidth := 12;
  FButtonHeight := 12;
  FPrevMouseIndex := -1;
  FPickMode := pmImmediate;
  FPickShift := [ssLeft];
  FShowColorHint := true;
  FGradientSteps := 3;
  FCols := 8;
  SetPaletteKind(pkStandardPalette);

  UpdateSize;
end;

destructor TCustomColorPalette.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TCustomColorPalette.AddColor(AColor: TColor);
begin
  DoAddColor(AColor);
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.BlendWBColor(AColor: TColor; Steps: Integer);
var
  I: Integer;
  R, G, B, NR, NG, NB: Byte;
begin
  RedGreenBlue(AColor, R, G, B);

  for I := 1 to Steps do
  begin
    NR := Round((R * I + 255 * (Steps + 1 - I)) / (Steps + 1));
    NG := Round((G * I + 255 * (Steps + 1 - I)) / (Steps + 1));
    NB := Round((B * I + 255 * (Steps + 1 - I)) / (Steps + 1));
    DoAddColor(RGBToColor(NR, NG, NB));
  end;

  DoAddColor(AColor);

  for I := Steps downto 1 do
  begin
    NR := Round(R * I / (Steps + 1));
    NG := Round(G * I / (Steps + 1));
    NB := Round(B * I / (Steps + 1));
    DoAddColor(RGBToColor(NR, NG, NB));
  end;
end;

procedure TCustomColorPalette.ClearColors;
begin
  FColors.Clear;
end;

procedure TCustomColorPalette.ColorPick(AIndex: Integer; Shift: TShiftState);
var
  c: TColor;
begin
  c := GetColors(AIndex);
  DoColorPick(c, Shift);
  if IsCorrectShift(Shift) then
    SelectedIndex := AIndex;
end;

procedure TCustomColorPalette.ColorMouseMove(AColor: TColor; Shift: TShiftState);
begin
  if Assigned(FOnColorMouseMove) then
    FOnColorMouseMove(Self, AColor, Shift);
end;

procedure TCustomColorPalette.DeleteColor(AIndex: Integer);
begin
  DoDeleteColor(AIndex);
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.DoAddColor(AColor: TColor);
begin
  FColors.Add(Pointer(AColor));
end;

procedure TCustomColorPalette.DoColorPick(AColor: TColor; AShift: TShiftState);
begin
  if Assigned(FOnColorPick) then
    FOnColorPick(Self, AColor, AShift);
end;

procedure TCustomColorPalette.DoDeleteColor(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FColors.Count) then
    exit;
  FColors.Delete(AIndex);
end;

procedure TCustomColorPalette.DoSelectColor(AColor: TColor);
begin
  FSelectedColor := AColor;
  Invalidate;
  if Assigned(FOnSelectColor) then FOnSelectColor(self, AColor);
end;

function TCustomColorPalette.GetColorCount: Integer;
begin
  Result := FColors.Count;
end;

function TCustomColorPalette.GetColors(AIndex: Integer): TColor;
begin
  if (AIndex < 0) or (AIndex >= FColors.Count) then
    Result := clNone
  else
    Result := TColor(PtrUInt(FColors.Items[AIndex]));
end;

function TCustomColorPalette.GetHintText(AColor: TColor): string;
const
  INDENT = '* ';
  MASK = '%sRed: %d'#13'%sGreen: %d'#13'%sBlue: %d';
begin
  if AColor = clNone then
    Result := 'NONE'
  else
  begin
    Result := ColorToString(AColor);
    if (Result[1] = 'c') and (Result[2] = 'l') then
    begin
      Delete(Result, 1, 2);
      Result := Uppercase(Result) + #13 + Format(MASK, [
        INDENT, Red(AColor), INDENT, Green(AColor), INDENT, Blue(AColor)]
      );
    end else
      Result := Format(MASK, ['', Red(AColor), '', Green(AColor), '', Blue(AColor)]);
  end;
end;

function TCustomColorPalette.GetPickedColor: TColor;
begin
  Result := GetColors(FMouseIndex);
end;

function TCustomColorPalette.IsCorrectShift(Shift: TShiftState): Boolean;
begin
  Result := True;
  if (ssLeft in FPickShift) and (Classes.ssLeft in Shift) then exit;
  if (ssRight in FPickShift) and (Classes.ssRight in Shift) then exit;
  if (ssMiddle in FPickShift) and (Classes.ssMiddle in Shift) then exit;
  Result := false;
end;

procedure TCustomColorPalette.LoadPalette(const FileName: String);
var
  F: TextFile;
  Line: String;
  C: TColor;

  function ParseColor(var S: String): TColor;
  var
    R, G, B: Integer;
    I: Integer;
  begin
    R := StrToIntDef(Copy(S, 1, Pos(',', S) - 1), 0);
    Delete(S, 1, Pos(',', S));
    G := StrToIntDef(Copy(S, 1, Pos(',', S) - 1), 0);
    Delete(S, 1, Pos(',', S));

    S := TrimLeft(S);
    I := 1;
    while (I <= Length(S)) and (S[I] in ['0'..'9']) do Inc(I);
    B := StrToIntDef(Copy(S, 1, Pred(I)), 0);
    Delete(S, 1, Pred(I));

    Result := RGBToColor(Max(0, Min(R, 255)), Max(0, Min(G, 255)), Max(0, Min(B, 255)));
  end;

begin
  if not FileExists(FileName) then
    raise Exception.Create(Format('[TCustomColorPalette.LoadPalette] File not found: %s', [FileName]));

  AssignFile(F, FileName);
  try
    Reset(F);

    FColors.Clear;
    FCols := 1;
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Line := Trim(Line);
      if Length(Line) < 2 then Continue;
      if Line[1] = '#' then Continue;
      if Line[1] = '$' then
      begin
        if Copy(Line, 2, 4) = 'NONE' then DoAddColor(clNone);
        if Copy(Line, 2, 4) = 'COLS' then FCols := StrToIntDef(Copy(Line, 6, MaxInt), 8);
        if Copy(Line, 2, 7) = 'BLENDWB' then
        begin
          Delete(Line, 1, 8);
          C := ParseColor(Line);
          BlendWBColor(C, StrToInt(Line));
        end;
      end
      else
        if Pos(',', Line) > 0 then DoAddColor(ParseColor(Line));
    end;
  finally
    Close(F);
  end;

  UpdateSize;
  SelectedIndex := 0;
end;

procedure TCustomColorPalette.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FMousePt.X := X;
  FMousePt.Y := Y;

  X := X div FButtonWidth;
  Y := Y div FButtonHeight;

  FMouseIndex := X + Y * FCols;
  FPrevMouseIndex := FMouseIndex;

  if FMouseIndex < 0 then
    Exit;

  if (FMouseIndex < FColors.Count) then
  begin
    FStoredShift := Shift;       // store for usage by pmDefault at MouseUp
    if FPickMode <> pmDefault then
      ColorPick(FMouseIndex, Shift);
  end;
end;

procedure TCustomColorPalette.MouseEnter;
begin
  FSavedHint := Hint;
  inherited;
end;

procedure TCustomColorPalette.MouseLeave;
begin
  inherited;
  Hint := FSavedHint;
end;

procedure TCustomColorPalette.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  C: TColor;
begin
  inherited;

  FMouseIndex := X div FButtonWidth + (Y div FButtonHeight) * FCols;

  if (FMouseIndex >= 0) and (FMouseIndex < FColors.Count) then
  begin
    C := GetColors(FMouseIndex);
    if ShowHint and FShowColorHint then
    begin
      Hint := GetHintText(c);
      if FMouseIndex <> FPrevMouseIndex then
        Application.ActivateHint(ClientToScreen(Point(X, Y)));
    end;
    if (FMouseIndex <> FPrevMouseIndex) then
    begin
      if C <> clNone then
        ColorMouseMove(C, Shift);
      if FPickMode = pmContinuous then
        ColorPick(FMouseIndex, Shift);
    end;
  end;

  FPrevMouseIndex := FMouseIndex;
end;

procedure TCustomColorPalette.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case FPickMode of
    pmDefault:
      if (FMousePt.X = X) and (FMousePt.Y = Y) then
        ColorPick(FMouseIndex, FStoredShift);
    pmImmediate, pmContinuous:
      begin
        X := X div FButtonWidth;
        Y := Y div FButtonHeight;
        FMouseIndex := X + Y * FCols;
        if (FMouseIndex >= 0) and (FMouseIndex < FColors.Count) and
           (FMouseIndex <> FPrevMouseIndex) then
        begin
          ColorPick(FMouseIndex, Shift);
        end;
      end;
  end;
  FPrevMouseIndex := -1;

  inherited;
end;

procedure TCustomColorPalette.Paint;

  procedure PaintBox(x1, y1, x2, y2: Integer; c: TColor);
  begin
    if c = clNone then
      exit;

    // Fill interior
    Canvas.Brush.Color := c;
    Canvas.FillRect(x1, y1, x2, y2);

    // Paint border
    if (FBorderColor <> clNone) and (FBorderWidth > 0) then
    begin
      x1 := x1 - FBorderWidth div 2 - FBorderWidth mod 2;
      y1 := y1 - FBorderWidth div 2 - FBorderWidth mod 2;
      x2 := x1 + FButtonWidth;
      y2 := y1 + FButtonHeight;
      Canvas.Pen.Color := FBorderColor;
      Canvas.Pen.Width := FBorderWidth;
      Canvas.MoveTo(x1, y1);
      Canvas.LineTo(x2, y1);
      Canvas.LineTo(x2, y2);
      Canvas.LineTo(x1, y2);
      Canvas.LineTo(x1, y1);
    end;
  end;

var
  I, X, Y, W, H, d: Integer;
  c: TColor;
  Rsel: TRect;
begin
  Canvas.Pen.Endcap := pecSquare;

  // Paint color boxes
  X := FBorderWidth;
  Y := FBorderWidth;
  W := FButtonWidth - FBorderWidth;
  H := FButtonHeight - FBorderWidth;
  for I := 0 to pred(FColors.Count) do
  begin
    if I = FSelectedIndex then    // Selected rect of box with selected color
      Rsel := Bounds(X, Y, W, H);
    c := GetColors(I);
    PaintBox(X, Y, X+W, Y+H, c);
    inc(X, FButtonWidth);
    if X >= Width then
    begin
      inc(Y, FButtonHeight);
      X := FBorderWidth;
    end;
  end;

  // Paint selection
  if FShowSelection then
  begin
    d := FBorderWidth div 2;
    if d = 0 then
      c := GetColors(FSelectedIndex) else
      c := ColorToRgb(FBorderColor);
    Canvas.Pen.Color := InvertColor(c);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Style := bsClear;
    InflateRect(Rsel, d, d);
    Canvas.Rectangle(Rsel);
  end;
end;

procedure TCustomColorPalette.SavePalette(const Filename: String);
var
  i: Integer;
  L: TStringList;
  clr: TColor;
  r,g,b: Byte;
begin
  L := TStringList.Create;
  try
    L.Add(Format('$COLS %d', [FCols]));
    for i:=0 to FColors.Count-1 do begin
      clr := Colors[i];
      if clr = clNone then
        L.Add('$NONE')
      else begin
        RedGreenBlue(clr, r,g,b);
        L.Add(Format('%d, %d, %d',[r, g, b]));
      end;
    end;
    L.SaveToFile(FileName);
  finally
    L.Free;
  end;
end;

procedure TCustomColorPalette.SetBorderColor(const AValue: TColor);
begin
  if FBorderColor = AValue then exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TCustomColorPalette.SetBorderWidth(const AValue: Integer);
begin
  if FBorderWidth = AValue then exit;
  FBorderWidth := AValue;
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.SetButtonHeight(const AValue: Integer);
begin
  if FButtonHeight = AValue then Exit;
  FButtonHeight := AValue;
  if FButtonHeight < 1 then FButtonHeight := 1;
  UpdateSize;
end;

procedure TCustomColorPalette.SetButtonWidth(const AValue: Integer);
begin
  if FButtonWidth = AValue then Exit;
  FButtonWidth := AValue;
  if FButtonWidth < 1 then FButtonWidth := 1;
  UpdateSize;
end;

procedure TCustomColorPalette.SetColors(AIndex: Integer; const AValue: TColor);
begin
  FColors.Items[AIndex] := Pointer(AValue);
  Invalidate;
end;

procedure TCustomColorPalette.SetCols(AValue: Integer);
begin
  if AValue = FCols then
    exit;
  FCols := AValue;
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.SetGradientSteps(AValue: Byte);
begin
  if FGradientSteps = AValue then
    exit;
  FGradientSteps := AValue;
  if FPaletteKind = pkGradientPalette then
  begin
    FColors.Clear;
    SetPaletteKind(FPaletteKind);
  end;
end;

procedure TCustomColorPalette.SetPaletteKind(AValue: TPaletteKind);

  function FixHex(hx: String): TColor;
  var
    r, g, b, color: string;
  begin
    r := copy(hx,1,2);
    g := copy(hx,3,2);
    b := copy(hx,5,2);
    Result := StringToColor('$0' + b + g + r);
  end;

const
  STEPS: array[0..4] of byte = (0, 64, 128, 192, 255);
  COLCOUNT: array[TPaletteKind] of Integer = (
     8,  // StandardPalette = 16 standard colors
     4,  // ExtendedPalette = 16 standard colors + 4 extra colors
     5,  // SystemPalette = 25 system colors
     8,  // StandardAndSystemPalette = 16 standard + 25 system colors = 41 colors
     5,  // ExtendedAndSystemPalette = 16 std + 4 extra + 25 system colors = 45 colors
    -1,  // Gradient palette - color count depends on PaletteStep
     6,  // Websafe palette
    14   // Websafe palette #2
  );
var
  i, n: Integer;
  r,g,b: Integer;
begin
  if (FPaletteKind = AValue) and (FColors.Count > 0) then
    exit;

  FPaletteKind := AValue;
  FColors.Clear;

  if FPaletteKind in [pkStandardPalette, pkExtendedPalette,
    pkStandardAndSystemPalette, pkExtendedAndSystemPalette] then
  begin
    DoAddColor(clBlack);       // 16
    DoAddColor(clGray);
    DoAddColor(clMaroon);
    DoAddColor(clOlive);
    DoAddColor(clGreen);
    DoAddColor(clTeal);
    DoAddColor(clNavy);
    DoAddColor(clPurple);
    DoAddColor(clWhite);
    DoAddColor(clSilver);
    DoAddColor(clRed);
    DoAddColor(clYellow);
    DoAddColor(clLime);
    DoAddColor(clAqua);
    DoAddColor(clBlue);
    DoAddColor(clFuchsia);
  end;

  if FPaletteKind in [pkExtendedPalette, pkExtendedAndSystemPalette] then
  begin
    DoAddColor(clMoneyGreen);     // 4
    DoAddColor(clSkyBlue);
    DoAddColor(clCream);
    DoAddColor(clMedGray);
  end;

  if FPaletteKind in [pkSystemPalette, pkStandardAndSystemPalette, pkExtendedAndSystemPalette] then
  begin
    DoAddColor(clScrollBar);            // 25
    DoAddColor(clBackground);
    DoAddColor(clActiveCaption);
    DoAddColor(clInactiveCaption);
    DoAddColor(clMenu);
    DoAddColor(clWindow);
    DoAddColor(clWindowFrame);
    DoAddColor(clMenuText);
    DoAddColor(clWindowText);
    DoAddColor(clCaptionText);
    DoAddColor(clActiveBorder);
    DoAddColor(clInactiveBorder);
    DoAddColor(clAppWorkspace);
    DoAddColor(clHighlight);
    DoAddColor(clHighlightText);
    DoAddColor(clBtnFace);
    DoAddColor(clBtnShadow);
    DoAddColor(clGrayText);
    DoAddColor(clBtnText);
    DoAddColor(clInactiveCaptionText);
    DoAddColor(clBtnHighlight);
    DoAddColor(cl3DDkShadow);
    DoAddColor(cl3DLight);
    DoAddColor(clInfoText);
    DoAddColor(clInfoBk);
  end;

  if FPaletteKind = pkGradientPalette then
  begin
    if FGradientSteps = 0 then n := 1 else n := FGradientSteps;
    for i:= Low(STEPS) to High(STEPS) do BlendWBColor((RGBToColor(255, STEPS[i], 0)), n);
    for i:= High(STEPS) downto Low(STEPS) do BlendWBColor((RGBToColor(STEPS[i], 255, 0)), n);
    for i:= Low(STEPS) to High(STEPS) do BlendWBColor((RGBToColor(0, 255, STEPS[i])), n);
    for i:= High(STEPS) downto Low(STEPS) do BlendWBColor((RGBToColor(0, STEPS[i], 255)), n);
    for i:= Low(STEPS) to High(STEPS) do BlendWBColor((RGBToColor(STEPS[i], 0, 255)), n);
    for i:= Low(STEPS) downto High(STEPS) do BlendWBColor((RGBToColor(0, 255, STEPS[i])), n);
    SetCols(n*2 + 1);
  end;

  if FPaletteKind = pkWebSafePalette then
  begin
    // https://en.wikipedia.org/wiki/Web_colors
    for g := 0 to 5 do
      for b:= 0 to 5 do
        for r:=0 to 5 do
          DoAddColor(RGBToColor(r*$33, g*$33, b*$33));
  end;

  if FPaletteKind = pkWebSafePalette2 then
  begin
    DoAddColor(FixHex('f0f8ff'));       // 140
    DoAddColor(FixHex('faebd7'));
    DoAddColor(FixHex('00ffff'));
    DoAddColor(FixHex('7fffd4'));
    DoAddColor(FixHex('f0ffff'));
    DoAddColor(FixHex('f5f5dc'));

    DoAddColor(FixHex('ffe4c4'));
    DoAddColor(FixHex('000000'));
    DoAddColor(FixHex('ffebcd'));
    DoAddColor(FixHex('0000ff'));
    DoAddColor(FixHex('8a2be2'));
    DoAddColor(FixHex('a52a2a'));

    DoAddColor(FixHex('deb887'));
    DoAddColor(FixHex('5f9ea0'));
    DoAddColor(FixHex('7fff00'));
    DoAddColor(FixHex('d2691e'));
    DoAddColor(FixHex('ff7f50'));
    DoAddColor(FixHex('6495ed'));

    DoAddColor(FixHex('fff8dc'));
    DoAddColor(FixHex('dc143c'));
    DoAddColor(FixHex('00ffff'));
    DoAddColor(FixHex('00008b'));
    DoAddColor(FixHex('008b8b'));
    DoAddColor(FixHex('b8860b'));

    DoAddColor(FixHex('a9a9a9'));
    DoAddColor(FixHex('006400'));
    DoAddColor(FixHex('bdb76b'));
    DoAddColor(FixHex('8b008b'));
    DoAddColor(FixHex('556b2f'));
    DoAddColor(FixHex('ff8c00'));

    DoAddColor(FixHex('9932cc'));
    DoAddColor(FixHex('8b0000'));
    DoAddColor(FixHex('e9967a'));
    DoAddColor(FixHex('8fbc8f'));
    DoAddColor(FixHex('483d8b'));
    DoAddColor(FixHex('2f4f4f'));

    DoAddColor(FixHex('00ced1'));
    DoAddColor(FixHex('9400d3'));
    DoAddColor(FixHex('ff1493'));
    DoAddColor(FixHex('00bfff'));
    DoAddColor(FixHex('696969'));
    DoAddColor(FixHex('1e90ff'));

    DoAddColor(FixHex('b22222'));
    DoAddColor(FixHex('fffaf0'));
    DoAddColor(FixHex('228b22'));
    DoAddColor(FixHex('ff00ff'));
    DoAddColor(FixHex('dcdcdc'));
    DoAddColor(FixHex('f8f8ff'));

    DoAddColor(FixHex('ffd700'));
    DoAddColor(FixHex('daa520'));
    DoAddColor(FixHex('808080'));
    DoAddColor(FixHex('008000'));
    DoAddColor(FixHex('adff2f'));
    DoAddColor(FixHex('f0fff0'));

    DoAddColor(FixHex('ff69b4'));
    DoAddColor(FixHex('cd5c5c'));
    DoAddColor(FixHex('4b0082'));
    DoAddColor(FixHex('fffff0'));
    DoAddColor(FixHex('f0e68c'));
    DoAddColor(FixHex('e6e6fa'));

    DoAddColor(FixHex('fff0f5'));
    DoAddColor(FixHex('7cfc00'));
    DoAddColor(FixHex('fffacd'));
    DoAddColor(FixHex('add8e6'));
    DoAddColor(FixHex('f08080'));
    DoAddColor(FixHex('e0ffff'));

    DoAddColor(FixHex('fafad2'));
    DoAddColor(FixHex('90ee90'));
    DoAddColor(FixHex('d3d3d3'));
    DoAddColor(FixHex('ffb6c1'));
    DoAddColor(FixHex('ffa07a'));
    DoAddColor(FixHex('20b2aa'));

    DoAddColor(FixHex('87cefa'));
    DoAddColor(FixHex('778899'));
    DoAddColor(FixHex('b0c4de'));
    DoAddColor(FixHex('ffffe0'));
    DoAddColor(FixHex('00ff00'));
    DoAddColor(FixHex('32cd32'));

    DoAddColor(FixHex('faf0e6'));
    DoAddColor(FixHex('ff00ff'));
    DoAddColor(FixHex('800000'));
    DoAddColor(FixHex('66cdaa'));
    DoAddColor(FixHex('0000cd'));
    DoAddColor(FixHex('ba55d3'));

    DoAddColor(FixHex('9370db'));
    DoAddColor(FixHex('3cb371'));
    DoAddColor(FixHex('7b68ee'));
    DoAddColor(FixHex('00fa9a'));
    DoAddColor(FixHex('48d1cc'));
    DoAddColor(FixHex('c71585'));

    DoAddColor(FixHex('191970'));
    DoAddColor(FixHex('f5fffa'));
    DoAddColor(FixHex('ffe4e1'));
    DoAddColor(FixHex('ffe4b5'));
    DoAddColor(FixHex('ffdead'));
    DoAddColor(FixHex('000080'));

    DoAddColor(FixHex('fdf5e6'));
    DoAddColor(FixHex('808000'));
    DoAddColor(FixHex('6b8e23'));
    DoAddColor(FixHex('ffa500'));
    DoAddColor(FixHex('ff4500'));
    DoAddColor(FixHex('da70d6'));

    DoAddColor(FixHex('eee8aa'));
    DoAddColor(FixHex('98fb98'));
    DoAddColor(FixHex('afeeee'));
    DoAddColor(FixHex('db7093'));
    DoAddColor(FixHex('ffefd5'));
    DoAddColor(FixHex('ffdab9'));

    DoAddColor(FixHex('cd853f'));
    DoAddColor(FixHex('ffc0cb'));
    DoAddColor(FixHex('dda0dd'));
    DoAddColor(FixHex('b0e0e6'));
    DoAddColor(FixHex('800080'));
    DoAddColor(FixHex('ff0000'));

    DoAddColor(FixHex('bc8f8f'));
    DoAddColor(FixHex('4169e1'));
    DoAddColor(FixHex('8b4513'));
    DoAddColor(FixHex('fa8072'));
    DoAddColor(FixHex('f4a460'));
    DoAddColor(FixHex('2e8b57'));

    DoAddColor(FixHex('fff5ee'));
    DoAddColor(FixHex('a0522d'));
    DoAddColor(FixHex('c0c0c0'));
    DoAddColor(FixHex('87ceeb'));
    DoAddColor(FixHex('6a5acd'));
    DoAddColor(FixHex('708090'));

    DoAddColor(FixHex('fffafa'));
    DoAddColor(FixHex('00ff7f'));
    DoAddColor(FixHex('4682b4'));
    DoAddColor(FixHex('d2b48c'));
    DoAddColor(FixHex('008080'));
    DoAddColor(FixHex('d8bfd8'));

    DoAddColor(FixHex('ff6347'));
    DoAddColor(FixHex('40e0d0'));
    DoAddColor(FixHex('ee82ee'));
    DoAddColor(FixHex('f5deb3'));
    DoAddColor(FixHex('ffffff'));
    DoAddColor(FixHex('f5f5f5'));

    DoAddColor(FixHex('ffff00'));
    DoAddColor(FixHex('9acd32'));
  end;

  if FPaletteKind <> pkGradientPalette then
    SetCols(COLCOUNT[FPaletteKind]);
end;

procedure TCustomColorPalette.SetSelectedIndex(AValue: Integer);
begin
  if FSelectedIndex = AValue then exit;
  if AValue < 0 then
    FSelectedIndex := 0
  else
  if AValue >= FColors.Count then
    FSelectedIndex := FColors.Count-1
  else
    FSelectedIndex := AValue;
  DoSelectColor(GetColors(FSelectedIndex));
end;

procedure TCustomColorPalette.SetShowSelection(AValue: Boolean);
begin
  if FShowSelection = AValue then exit;
  FShowSelection := AValue;
  Invalidate;
end;

procedure TCustomColorPalette.UpdateSize;
begin
  if (FCols = 0) or (FColors.Count = 0) then FRows := 0
  else
    FRows := Ceil(FColors.Count / FCols);

  SetBounds(Left, Top, FCols * FButtonWidth + FBorderWidth, FRows * FButtonHeight + FBorderWidth);
end;


initialization
{$I colorpalette.lrs}

end.

