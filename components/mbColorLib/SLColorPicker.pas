unit SLColorPicker;

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
  SysUtils, Classes, Controls, Graphics, Math, RGBHSLUtils,
  Forms, HTMLColors, SelPropUtils, mbColorPickerControl, Scanlines;

type
  TSLColorPicker = class(TmbColorPickerControl)
  private
   FManual: boolean;
   FHue, FSat, FLum: integer;
   FOnChange: TNotifyEvent;
   FChange: boolean;
   FBMP: TBitmap;

   procedure CreateSLGradient;
   procedure DrawMarker(x, y: integer);
   procedure SelectionChanged(x, y: integer);
   procedure UpdateCoords;
   procedure SetHue(h: integer);
   procedure SetSat(s: integer);
   procedure SetLum(l: integer);
  protected
   procedure WebSafeChanged; override;
   function GetSelectedColor: TColor; override;
   procedure SetSelectedColor(c: TColor); override;
   procedure Paint; override;
   procedure Resize; override;
   procedure CreateWnd; override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure CNKeyDown(var Message: {$IFDEF FPC}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF});
     message CN_KEYDOWN;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;

   function GetColorAtPoint(x, y: integer): TColor; override;
   property Manual: boolean read FManual;
  published
   property Hue: integer read FHue write SetHue default 0;
   property Saturation: integer read FSat write SetSat default 0;
   property Luminance: integer read FLum write SetLum default 255;
   property SelectedColor default clWhite;
   property MarkerStyle default msCircle;

   property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

{$IFDEF FPC}
  {$R SLColorPicker.dcr}

uses
  IntfGraphics, fpimage;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TSLColorPicker]);
end;

constructor TSLColorPicker.Create(AOwner: TComponent);
begin
 inherited;
 FBMP := TBitmap.Create;
 FBMP.PixelFormat := pf32bit;
 FBMP.SetSize(256, 256);
 Width := 255;
 Height := 255;
 MaxHue := 360;
 MaxSat := 255;
 MaxLum := 255;
 FHue := 0;
 FSat := 0;
 FLum := 255;
 FChange := true;
 MarkerStyle := msCircle;
end;

destructor TSLColorPicker.Destroy;
begin
 FBMP.Free;
 inherited;
end;

//{$IFDEF DELPHI}
procedure TSLColorPicker.CreateSLGradient;
var
  x, y, skip: integer;
  row: pRGBQuadArray;
  c: TColor;
  {$IFDEF FPC}
  intfimg: TLazIntfImage;
  imgHandle, imgMaskHandle: HBitmap;
  {$ENDIF}
begin
  if FBmp = nil then
  begin
    FBmp := TBitmap.Create;
    FBmp.PixelFormat := pf32bit;
    FBmp.Width := 256;
    FBmp.Height := 256;
  end;

  {$IFDEF FPC}
  intfimg := TLazIntfImage.Create(FBmp.Width, FBmp.Height);
  try
    intfImg.LoadFromBitmap(FBmp.Handle, FBmp.MaskHandle);
  {$ENDIF}
    {
    row := FBMP.ScanLine[0];
    skip := integer(FBMP.ScanLine[1]) - Integer(row);
    }
    for y := 0 to 255 do
    begin
     {$IFDEF FPC}
     row := intfImg.GetDataLineStart(y);
     {$ELSE}
     row := FHSVBmp.Scanline(y);
     {$ENDIF}

     for x := 0 to 255 do
      if not WebSafe then
        row[x] := HSLtoRGBQuad(FHue, x, 255 - y)
      else
      begin
        c := GetWebSafe(RGBTripleToTColor(HSLToRGBTriple(FHue, x, 255 - y)));
        row[x] := RGBtoRGBQuad(GetRValue(c), GetGValue(c), GetBValue(c));
      end;
//      row := pRGBQuadArray(Integer(row) + skip);
    end;
   {$IFDEF FPC}
   intfimg.CreateBitmaps(imgHandle, imgMaskHandle, false);
   FBmp.Handle := imgHandle;
   FBmp.MaskHandle := imgMaskHandle;
 finally
   intfimg.Free;
 end;
 {$ENDIF}
end;

   (*
{$ELSE}
procedure TSLColorPicker.CreateSLGradient;
var
  x, y: Integer;
  c: TColor;
  intfimg: TLazIntfImage;
  imgHandle, imgMaskHandle: HBitmap;
begin
  if FBmp = nil then
  begin
    FBmp := TBitmap.Create;
    FBmp.PixelFormat := pf32Bit;
    FBmp.Width := 256;
    FBmp.Height := 256;
  end;
  intfimg := TLazIntfImage.Create(FBmp.Width, FBmp.Height);
  try
    intfImg.LoadFromBitmap(FBmp.Handle, FBmp.MaskHandle);
    for y := 0 to 255 do      // y = L
      for x := 0 to 255 do    // x = S
      begin
        c := HSLRangeToRGB(FHue, x, 255-y);
        if WebSafe then
          c := GetWebSafe(c);
        intfImg.Colors[x, y] := TColorToFPColor(c);
      end;
    intfimg.CreateBitmaps(imgHandle, imgMaskHandle, false);
    FBmp.Handle := imgHandle;
    FBmp.MaskHandle := imgMaskHandle;
  finally
    intfimg.Free;
  end;
end;
{$ENDIF}
     *)
procedure TSLColorPicker.Resize;
begin
 inherited;
 UpdateCoords;
end;

procedure TSLColorPicker.CreateWnd;
begin
 inherited;
 CreateSLGradient;
 UpdateCoords;
end;

procedure TSLColorPicker.UpdateCoords;
begin
 mdx := MulDiv(FSat, Width, 255);
 mdy := MulDiv(255-FLum, Height, 255);
end;

procedure TSLColorPicker.DrawMarker(x, y: integer);
var
 c: TColor;
begin
 c := not GetColorAtPoint(x, y);
 case MarkerStyle of
  msCircle: DrawSelCirc(x, y, Canvas);
  msSquare: DrawSelSquare(x, y, Canvas);
  msCross: DrawSelCross(x, y, Canvas, c);
  msCrossCirc: DrawSelCrossCirc(x, y, Canvas, c);
 end;
end;

procedure TSLColorPicker.Paint;
begin
 Canvas.StretchDraw(ClientRect, FBMP);
 DrawMarker(mdx, mdy);
end;

procedure TSLColorPicker.SetHue(h: integer);
begin
 if h > 360 then h := 360;
 if h < 0 then h := 0;
 if FHue <> h then
  begin
   FHue := h;
   FManual := false;
   CreateSLGradient;
   UpdateCoords;
   Invalidate;
   if Fchange then
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetSat(s: integer);
begin
 if s > 255 then s := 255;
 if s < 0 then s := 0;
 if FSat <> s then
  begin
   FSat := s;
   FManual := false;
   UpdateCoords;
   Invalidate;
   if Fchange then
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetLum(l: integer);
begin
 if l > 255 then l := 255;
 if l < 0 then l := 0;
 if FLum <> l then
  begin
   FLum := l;
   FManual := false;
   UpdateCoords;
   Invalidate;
   if Fchange then
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SelectionChanged(x, y: integer);
begin
 FChange := false;
// SetSat(MulDiv(255, x, Width));
// SetLum(MulDiv(255, Height - y, Height));
 SetSat(MulDiv(255, x, Width - 1));
 SetLum(MulDiv(255, Height - y -1, Height - 1));
 FChange := true;
end;

procedure TSLColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 inherited;
 {$IFDEF DELPHI}
 ClipCursor(nil);
 {$ENDIF}
 if csDesigning in ComponentState then Exit;
 if (Button = mbLeft) and PtInRect(ClientRect, Point(x, y)) then
  begin
   mdx := x;
   mdy := y;
   SelectionChanged(X, Y);
   FManual := true;
   if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
 R: TRect;
begin
 inherited;
 if csDesigning in ComponentState then Exit;
 if (Button = mbLeft) and PtInRect(ClientRect, Point(x, y)) then
  begin
   mdx := x;
   mdy := y;
   R := ClientRect;
   R.TopLeft := ClientToScreen(R.TopLeft);
   R.BottomRight := ClientToScreen(R.BottomRight);
   {$IFDEF DELPHI}
   ClipCursor(@R);
   {$ENDIF}
   SelectionChanged(X, Y);
   FManual := true;
   if Assigned(FOnChange) then FOnChange(Self);
  end;
 SetFocus;
end;

procedure TSLColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 if csDesigning in ComponentState then Exit;
 if (ssLeft in Shift) and PtInRect(ClientRect, Point(x, y)) then
  begin
   mdx := x;
   mdy := y;
   SelectionChanged(X, Y);
   FManual := true;
   if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TSLColorPicker.SetSelectedColor(c: TColor);
var
 h, s, l: integer;
begin
 if WebSafe then c := GetWebSafe(c);
 FManual := false;
 Fchange := false;
 RGBTripleToHSL(RGBtoRGBTriple(GetRValue(c), GetGValue(c), GetBValue(c)), h, s, l);
 SetHue(h);
 SetSat(s);
 SetLum(l);
 if Fchange then
  if Assigned(FOnChange) then FOnChange(Self);
 FChange := true;
end;

function TSLColorPicker.GetSelectedColor: TColor;
var
 triple: TRGBTriple;
begin
 triple := HSLToRGBTriple(FHue, FSat, FLum);
 if not WebSafe then
  Result := RGBTripleToTColor(triple)
 else
  Result := GetWebSafe(RGBTripleToTColor(triple));
end;

function TSLColorPicker.GetColorAtPoint(x, y: integer): TColor;
var
 triple: TRGBTriple;
begin
 triple := HSLToRGBTriple(FHue, MulDiv(255, x, Width), MulDiv(255, Height - y, Height));
 if not WebSafe then
  Result := RGBTripleToTColor(triple)
 else
  Result := GetWebSafe(RGBTripleToTColor(triple));
end;

procedure TSLColorPicker.CNKeyDown(
  var Message: {$IFDEF FPC}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF} );
var
 Shift: TShiftState;
 FInherited: boolean;
begin
 FInherited := false;
 Shift := KeyDataToShiftState(Message.KeyData);
 if not (ssCtrl in Shift) then
  case Message.CharCode of
   VK_LEFT:
    if not (mdx - 1 < 0) then
     begin
      Dec(mdx, 1);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
    if not (mdx + 1 > Width) then
     begin
      Inc(mdx, 1);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_UP:
    if not (mdy - 1 < 0) then
     begin
      Dec(mdy, 1);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
    if not (mdy + 1 > Height) then
     begin
      Inc(mdy, 1);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
  else
   begin
    FInherited := true;
    inherited;
   end;
  end
 else
  case Message.CharCode of
   VK_LEFT:
    if not (mdx - 10 < 0) then
     begin
      Dec(mdx, 10);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_RIGHT:
    if not (mdx + 10 > Width) then
     begin
      Inc(mdx, 10);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_UP:
    if not (mdy - 10 < 0) then
     begin
      Dec(mdy, 10);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
   VK_DOWN:
    if not (mdy + 10 > Height) then
     begin
      Inc(mdy, 10);
      SelectionChanged(mdx, mdy);
      FManual := true;
      if Assigned(FOnChange) then FOnChange(Self);
     end;
  else
   begin
    FInherited := true;
    inherited;
   end;
  end;
 if not FInherited then
  if Assigned(OnKeyDown) then
   OnKeyDown(Self, Message.CharCode, Shift);
end;

procedure TSLColorPicker.WebSafeChanged;
begin
 inherited;
 CreateSLGradient;
 Invalidate;
end;

end.
