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
    pmDefault,   // Select color at mouse-down, ColorPick event at mouse-up if at same pos
    pmImproved,  // Select color and ColorPick event at mouse-down
    pmContinuous // Select color at mouse-down and mouse-move, ColorPick event at mouse-up
  );

  TPickShiftEnum = (ssLeft, ssRight, ssMiddle);
  TPickShift = set of TPickShiftEnum;

  TColorMouseEvent = procedure (Sender: TObject; AColor: TColor; Shift: TShiftState) of object;

  { TCustomColorPalette }

  TCustomColorPalette = class(TGraphicControl)
  private
    FButtonHeight: Integer;
    FButtonWidth: Integer;
    FCols: Integer;
    FOnColorMouseMove: TColorMouseEvent;
    FOnColorPick: TColorMouseEvent;
    FRows: Integer;
    FColors: TList;
    FPickedColor: TColor;
    FPickMode: TPickMode;
    FPickShift: TPickShift;
    FMousePt: TPoint;
    FMouseIndex: Integer;
    FPrevMouseIndex: Integer;
    FStoredShift: TShiftState;
    function GetColorCount: Integer;
    function GetColors(Index: Integer): TColor;
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetColors(Index: Integer; const AValue: TColor);
    procedure SetCols(AValue: Integer);
  protected
    procedure ColorPick(AColor: TColor; Shift: TShiftState); dynamic;
    procedure ColorMouseMove(AColor: TColor; Shift: TShiftState); dynamic;
    procedure DoAddColor(AColor: TColor); virtual;
    procedure DoDeleteColor(AIndex: Integer); virtual;
    function IsCorrectShift(Shift: TShiftState): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift:TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure UpdateSize; virtual;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property ColumnCount: Integer read FCols write SetCols;
    property PickMode: TPickMode read FPickMode write FPickMode default pmDefault;
    property PickShift: TPickShift read FPickShift write FPickShift default [ssLeft];
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  public
    procedure AddColor(AColor: TColor);
    procedure DeleteColor(AIndex: Integer);
    procedure LoadPalette(const FileName: String);
    procedure SavePalette(const FileName: String);
  
    property Colors[Index: Integer]: TColor read GetColors write SetColors;
    property ColorCount: Integer read GetColorCount;
    property PickedColor: TColor read FPickedColor;

    property OnColorPick: TColorMouseEvent read FOnColorPick write FOnColorPick;
    property OnColorMouseMove: TColorMouseEvent read FOnColorMouseMove write FOnColorMouseMove;
    
    property Height stored False;
    property Width stored False;
  end;
  
  TColorPalette = class(TCustomColorPalette)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property Color;
    property ColumnCount;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PickMode;
    property PickShift;
    property PopupMenu;
    property ShowHint;
    property Visible;
    
    property OnChangeBounds;
    property OnClick;
    property OnColorMouseMove;
    property OnColorPick;
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

procedure Register;
begin
  RegisterComponents('Misc', [TColorPalette]);
end;

{ TCustomColorPalette }

procedure TCustomColorPalette.SetButtonHeight(const AValue: Integer);
begin
  if FButtonHeight = AValue then Exit;
  FButtonHeight := AValue;
  if FButtonHeight < 1 then FButtonHeight := 1;
  UpdateSize;
end;

function TCustomColorPalette.GetColorCount: Integer;
begin
  Result := FColors.Count;
end;

function TCustomColorPalette.GetColors(Index: Integer): TColor;
begin
  Result := TColor(PtrUInt(FColors.Items[Index]));
end;

procedure TCustomColorPalette.SetButtonWidth(const AValue: Integer);
begin
  if FButtonWidth = AValue then Exit;
  FButtonWidth := AValue;
  if FButtonWidth < 1 then FButtonWidth := 1;
  UpdateSize;
end;

procedure TCustomColorPalette.SetColors(Index: Integer; const AValue: TColor);
begin
  FColors.Items[Index] := Pointer(AValue);
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

procedure TCustomColorPalette.UpdateSize;
begin
  if (FCols = 0) or (FColors.Count = 0) then FRows := 0
  else
    FRows := Ceil(FColors.Count / FCols);

  SetBounds(Left, Top, FCols * FButtonWidth + 1, FRows * FButtonHeight + 1);
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
    FPickedColor := GetColors(FMouseIndex);
    FStoredShift := Shift;       // store for usage by pmDefault at MouseUp
    if FPickMode <> pmDefault then
      ColorPick(FPickedColor, Shift);
  end;
end;

procedure TCustomColorPalette.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case FPickMode of
    pmDefault:
      if (FMousePt.X = X) and (FMousePt.Y = Y) then
        ColorPick(FPickedColor, FStoredShift);
    pmImproved, pmContinuous:
      begin
        X := X div FButtonWidth;
        Y := Y div FButtonHeight;
        FMouseIndex := X + Y * FCols;
        if (FMouseIndex >= 0) and (FMouseIndex < FColors.Count) and
           (FMouseIndex <> FPrevMouseIndex) then
        begin
          FPickedColor := GetColors(FMouseIndex);
          ColorPick(FPickedColor, Shift);
        end;
      end;
  end;
  FPrevMouseIndex := -1;

  inherited;
end;

procedure TCustomColorPalette.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  C: TColor;
begin
  inherited;
  
  X := X div FButtonWidth;
  Y := Y div FButtonHeight;

  FMouseIndex := X + Y * FCols;
  if (FMouseIndex >= 0) and (FMouseIndex < FColors.Count) and
     (FMouseIndex <> FPrevMouseIndex) then
  begin
    C := GetColors(FMouseIndex);
    if C <> clNone then
      ColorMouseMove(C, Shift);

    if FPickMode = pmContinuous then begin
      FPickedColor := GetColors(FMouseIndex);
      ColorPick(FPickedColor, Shift);
    end;
  end;

  FPrevMouseIndex := FMouseIndex;
end;

function TCustomColorPalette.IsCorrectShift(Shift: TShiftState): Boolean;
var
  ss: TShiftState;
begin
  Result := True;
  if (ssLeft in FPickShift) and (Classes.ssLeft in Shift) then exit;
  if (ssRight in FPickShift) and (Classes.ssRight in Shift) then exit;
  if (ssMiddle in FPickShift) and (Classes.ssMiddle in Shift) then exit;
  Result := false;
end;

procedure TCustomColorPalette.ColorPick(AColor: TColor; Shift: TShiftState);
begin
  if IsCorrectShift(Shift) and Assigned(FOnColorPick) then
    FOnColorPick(Self, AColor, Shift);
end;

procedure TCustomColorPalette.ColorMouseMove(AColor: TColor; Shift: TShiftState);
begin
  if IsCorrectShift(Shift) and Assigned(FOnColorMouseMove) then
    FOnColorMouseMove(Self, AColor, Shift);
end;

constructor TCustomColorPalette.Create(TheOwner: TComponent);
begin
  inherited;
  
  FColors := TList.Create;
  FButtonWidth := 12;
  FButtonHeight := 12;
  FPrevMouseIndex := -1;
  FPickShift := [ssLeft];
  ControlStyle := ControlStyle + [csFixedWidth, csFixedHeight];
  
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

procedure TCustomColorPalette.DoDeleteColor(AIndex: Integer);
begin
  FColors.Delete(AIndex);
end;

procedure TCustomColorPalette.Paint;
var
  I, X, Y: Integer;
  c: TColor;
begin
  Canvas.Pen.Color := clBlack;
  for I := 0 to Pred(FColors.Count) do
  begin
    Y := I div FCols;
    X := I mod FCols;
    c := GetColors(I);
    if c <> clNone then
    begin
      Canvas.Brush.Color := c;
      Canvas.Rectangle(Bounds(X * FButtonWidth, Y * FButtonHeight, FButtonWidth,
        FButtonHeight));
    end;
  end;
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
  Invalidate;
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


initialization
{$I colorpalette.lrs}

end.

