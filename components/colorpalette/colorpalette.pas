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
    function GetColorCount: Integer;
    function GetColors(AIndex: Integer): TColor;
    function GetPickedColor: TColor;
    procedure SetBorderColor(const AValue: TColor);
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetColors(AIndex: Integer; const AValue: TColor);
    procedure SetCols(AValue: Integer);
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetShowSelection(AValue: Boolean);
  protected
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
    property PickMode: TPickMode read FPickMode write FPickMode default pmDefault;
    property PickShift: TPickShift read FPickShift write FPickShift default [ssLeft];
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default 0;
    property ShowColorHint: Boolean read FShowColorHint write FShowColorHint default true;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default false;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure AddColor(AColor: TColor);
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

constructor TCustomColorPalette.Create(TheOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csFixedWidth, csFixedHeight];

  FColors := TList.Create;
  FBorderColor := clBlack;
  FBorderWidth := 1;
  FButtonWidth := 12;
  FButtonHeight := 12;
  FPrevMouseIndex := -1;
  FPickShift := [ssLeft];
  FShowColorHint := true;

  FCols := 8;

  DoAddColor(clBlack);
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

  procedure BlendWBColor(Color: TColor; Steps: Integer);
  var
    I: Integer;
    R, G, B, NR, NG, NB: Byte;
  begin
    RedGreenBlue(Color, R, G, B);

    for I := 1 to Steps do
    begin
      NR := Round((R * I + 255 * (Steps + 1 - I)) / (Steps + 1));
      NG := Round((G * I + 255 * (Steps + 1 - I)) / (Steps + 1));
      NB := Round((B * I + 255 * (Steps + 1 - I)) / (Steps + 1));
      DoAddColor(RGBToColor(NR, NG, NB));
    end;

    DoAddColor(Color);

    for I := Steps downto 1 do
    begin
      NR := Round(R * I / (Steps + 1));
      NG := Round(G * I / (Steps + 1));
      NB := Round(B * I / (Steps + 1));
      DoAddColor(RGBToColor(NR, NG, NB));
    end;
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
{
procedure TCustomColorPalette.SetShowColorHint(AValue: Boolean);
begin
  if FShowColorHint = AValue then exit;
  if AValue then
    FSavedHint := Hint
  else
    Hint := FSavedHint;
  FShowColorHint := AValue;
end;
 }
procedure TCustomColorPalette.SetShowSelection(AValue: Boolean);
begin
  if FShowSelection = AValue then exit;
  FShowSelection := AValue;
  Invalidate;
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

