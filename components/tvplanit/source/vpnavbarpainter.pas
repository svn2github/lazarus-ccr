unit VpNavBarPainter;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages, MMSystem,
  {$ENDIF}
  Graphics, Classes, SysUtils, Controls, Buttons,
  VpNavBar;

type
  PRect = ^TRect;

  TVpNavBarPainter = class
  private
    FNavBar: TVpCustomNavBar;

    // Protected properties of the TVpCustomNavBar.
    FActiveFolder: Integer;
    FActiveItem: Integer;
    FBackgroundColor: TColor;
    FBackgroundImage: TBitmap;
    FBackgroundMethod: TVpBackgroundMethod;
    FButtonHeight: Integer;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FDrawingStyle: TVpFolderDrawingStyle;
    FHotFolder: Integer;
    FImages: TImageList;
    FItemFont: TFont;
    FItemSpacing: Integer;
    FSelectedItem: Integer;
    FSelectedItemFont: TFont;
    FShowButtons: Boolean;

    nabItemsRect: PRect;
    nabLastMouseOverItem: Integer;
    nabMouseDown: Boolean;
    nabScrollUpBtn: TSpeedButton;
    nabScrollDownBtn: TSpeedButton;
    nabTopItem: Integer;

    FFolderArea: TRect;
    bkMode: Integer;

    procedure DrawBackground(Canvas: TCanvas; R: TRect);
    procedure DrawCoolTab(Canvas: TCanvas; R: TRect);
    function DrawDefButton(Canvas: TCanvas; R: TRect;
      IsFocused, IsMouseOver: Boolean): TRect;
    function DrawEtchedButton(Canvas: TCanvas; R: TRect;
      IsFocused, IsMouseOver: Boolean): TRect;
    function DrawNavTab(Canvas: TCanvas; const Client: TRect;
      BevelWidth: Integer; TabColor: TColor; TabNumber: Integer;
      CoolTab, IsFocused, IsMouseOver: Boolean): TRect;
    procedure DrawStandardTab(Canvas: TCanvas; R: TRect; TabColor: TColor; TabCount: Integer);

  protected
    procedure DrawActiveFolderItems(Canvas: TCanvas; var CurPos: Integer);
    procedure DrawBottomFolderButtons(Canvas: TCanvas; ARect: TRect;
      var CurPos: Integer);
    function DrawTab(Canvas: TCanvas; R: TRect; ATabIndex: Integer;
      IsFocused, IsMouseOver: Boolean): TRect;
    procedure DrawTopFolderButtons(Canvas: TCanvas; ARect: TRect;
      DrawFolder: Boolean; var CurPos: Integer);

  public
    constructor Create(ANavBar: TVpCustomNavBar);
    procedure Paint;
  end;

function GetLargeIconDisplayName(Canvas: TCanvas; Rect: TRect; const Name: string): string;

implementation

uses
  Themes,
  VpMisc;

type
  TVpNavBarOpener = class(TVpCustomNavBar);

constructor TVpNavBarPainter.Create(ANavBar: TVpCustomNavBar);
begin
  inherited Create;
  FNavBar := ANavBar;

  FActiveFolder := TVpNavBarOpener(FNavBar).ActiveFolder;
  FActiveItem := TVpNavBarOpener(FNavBar).ActiveItem;
  FBackgroundColor := TVpNavBarOpener(FNavBar).BackgroundColor;
  FBackgroundImage := TVpNavBarOpener(FNavBar).BackgroundImage;
  FBackgroundMethod := TVpNavBarOpener(FNavBar).BackgroundMethod;
  FButtonHeight := TVpNavBarOpener(FNavBar).ButtonHeight;
  FClientWidth := TVpNavBarOpener(FNavBar).ClientWidth;
  FClientHeight := TVpNavBarOpener(FNavBar).ClientHeight;
  FDrawingStyle := TVpNavBarOpener(FNavBar).DrawingStyle;
  FHotFolder := TVpNavBarOpener(FNavBar).FHotFolder;
  FImages := TVpNavBarOpener(FNavBar).Images;
  FItemFont := TVpNavBarOpener(FNavBar).FItemFont;
  FItemSpacing := TVpNavBarOpener(FNavBar).FItemSpacing;
  FSelectedItem := TVpNavBarOpener(FNavBar).FSelectedItem;
  FSelectedItemFont := TVpNavBarOpener(FNavBar).FSelectedItemFont;
  FShowButtons := TVpNavBarOpener(FNavBar).FShowButtons;

  // The nabItemsRect is populated in the Paint procedure, and it is needed in
  // the NavBar as well. Therefore we use a pointer here!
  nabItemsRect := @TVpNavBarOpener(FNavBar).nabItemsRect;

  nabLastMouseOverItem := TVpNavBarOpener(FNavBar).nabLastMouseOverItem;
  nabMouseDown := TVpNavBarOpener(FNavBar).nabMouseDown;
  nabScrollUpBtn := TVpNavBarOpener(FNavBar).nabScrollUpBtn;
  nabScrollDownBtn := TVpNavBarOpener(FNavBar).nabScrollDownBtn;
  nabTopItem := TVpNavBarOpener(FNavBar).nabTopItem;

  FFolderArea := TVpNavBarOpener(FNavBar).nabGetFolderArea(FActiveFolder);
end;

{ Draw the items for the active folder }
procedure TVpNavBarPainter.DrawActiveFolderItems(Canvas: TCanvas; var CurPos: Integer);
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
  J: Integer;
  text: String;
  W, H, X: Integer;
  R: TRect;
  Buf: array[0..255] of Char;
  labelWidth: Integer;
  lOffset: Integer;
  bmp: TBitmap;
begin
  folder := FNavBar.Folders[FActiveFolder];

  if folder.FolderType = ftDefault then begin
    if folder.ItemCount = 0 then
      exit;

    Inc(CurPos, 8);

    with nabItemsRect^ do begin
      Top := CurPos;
      Left := 0;
      Right := FNavBar.ClientWidth;
      Bottom := FNavBar.ClientHeight - (FNavBar.FolderCount - FActiveFolder - 1) * FButtonHeight + 1;
    end;

    for J := 0 to folder.ItemCount-1 do begin
      R := TVpNavBtnItem(folder.Items[J]).LabelRect;
      R.Bottom := nabItemsRect^.Bottom + 1;
      TVpNavBtnItem(folder.Items[J]).LabelRect := R;
    end;

    for J := nabTopItem to folder.ItemCount-1 do begin
      if (FSelectedItem = J) then
        Canvas.Font := FSelectedItemFont
      else
        Canvas.Font := FItemFont;

      item := Folder.Items[J];
      { If the caption is empty at designtime then display the item's name instead }
      if (csDesigning in FNavBar.ComponentState) and (item.Caption = '') then
        text := item.Name
      else
        text := item.Caption;

      { Large icons }
      if folder.IconSize = isLarge then begin
        if Assigned(FImages) then begin
          W := FImages.Width + 2;
          H := FImages.Height + 2;
        end else begin
          W := 32;
          H := 32;
        end;
        { glyph is at the top }
        { If an image list is assigned then use the image size.
          If no image list is assinged then assume a 32 x 32 image size. }
        R.Top := CurPos;
        R.Bottom := CurPos + H;
        R.Left := (FNavBar.ClientWidth - W) div 2;
        R.Right := R.Left + W;
        if R.Top > nabItemsRect^.Bottom then
          break;
        item.IconRect := R;

        if FShowButtons then begin
          if FActiveItem = J then begin
            if nabMouseDown then
              Canvas.Pen.Color := clBlack
            else
              Canvas.Pen.Color := clWhite;
            Canvas.MoveTo(R.Left-1, R.Bottom+1);
            Canvas.LineTo(R.Left-1, R.Top-1);
            Canvas.LineTo(R.Right+1, R.Top-1);
            if nabMouseDown then
              Canvas.Pen.Color := clWhite
            else
              Canvas.Pen.Color := clBlack;
            Canvas.LineTo(R.Right+1, R.Bottom+1);
            Canvas.LineTo(R.Left-1, R.Bottom+1);
          end else begin
            Canvas.Pen.Color := FBackgroundColor;
            Canvas.Brush.Color := FBackgroundColor;
          end;

          if Assigned(FImages) and (item.IconIndex >= 0) and (item.IconIndex < FImages.Count) then
            FImages.Draw(Canvas, R.Left + 2, R.Top + 2, item.IconIndex);

          {make the icon's bottom blend into the label's top}
          R := item.IconRect;
          inc(R.Bottom, 4);
          item.IconRect := R;
        end;

        Inc(CurPos, H + 4);

        {now, draw the text}
        R.Top := CurPos;
        R.Bottom := CurPos + FButtonHeight div 2 - 7;
        R.Left := 0;
        R.Right := FNavBar.ClientWidth - 1;
        item.LabelRect := R;
        item.DisplayName := GetLargeIconDisplayName(Canvas, R, text);
        X := Canvas.TextWidth(item.DisplayName);
        R.Left := (FNavBar.ClientWidth - X) div 2;
        if R.Left < 5 then
          R.Left := 5;
        R.Right := R.Left + X;
        if R.Right > FnavBar.ClientWidth - 5 then
          R.Right := FNavBar.ClientWidth - 5;
        item.LabelRect := R;
        if R.Top > nabItemsRect^.Bottom then
          Break;

        StrPLCopy(Buf, item.DisplayName, 255);
        DrawText(Canvas.Handle, Buf, Length(item.DisplayName), R,
          DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_CALCRECT);
        labelWidth := WidthOf(R);
        R.Left := (FNavBar.ClientWidth - labelWidth) div 2;
        R.Right := R.Left + labelWidth + 1;
        item.LabelRect := R;

        bkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
        X := DrawText(Canvas.Handle, Buf, Length(item.DisplayName), R, DT_CENTER or DT_VCENTER or DT_WORDBREAK);
        inc(CurPos, X);
        SetBkMode(Canvas.Handle, bkMode);

        Inc(CurPos, FItemSpacing);
      end
      else
      { Small Icons }
      begin
        W := 16;
        H := 16;
        {glyph is at the left}
        R.Top := CurPos;
        lOffset := abs(Canvas.Font.Height) div 2;
        if lOffset > 8 then
          R.Top := R.Top + lOffset - 8;
        R.Bottom := R.Top + H;
        R.Left := 8;
        R.Right := R.Left + W;
        item.IconRect := R;
        if R.Top > nabItemsRect^.Bottom then
          Break;

        if FShowButtons then begin
          if FActiveItem = J then begin
            if nabMouseDown then
              Canvas.Pen.Color := clBlack
            else
              Canvas.Pen.Color := clWhite;
            Canvas.MoveTo(R.Left-1, R.Bottom+1);
            Canvas.LineTo(R.Left-1, R.Top-1);
            Canvas.LineTo(R.Right+1, R.Top-1);
            if nabMouseDown then
              Canvas.Pen.Color := clWhite
            else
              Canvas.Pen.Color := clBlack;
            Canvas.LineTo(R.Right+1, R.Bottom+1);
            Canvas.LineTo(R.Left-1, R.Bottom+1);
            Canvas.Brush.Color := FBackgroundColor;
          end else begin
            Canvas.Pen.Color := FBackgroundColor;
            Canvas.Brush.Color := FBackgroundColor;
            Canvas.Rectangle(R.Left - 1, R.Top - 1, R.Right + 1, R.Bottom + 1);
          end;
          if Assigned(FImages) then begin
            bmp := TBitmap.Create;
            try
              bmp.Width := FImages.Width;
              bmp.Height := FImages.Height;
              FImages.Draw(bmp.Canvas, 0, 0, item.IconIndex);
              Canvas.BrushCopy(item.IconRect, bmp, Rect(0, 0, bmp.Width, bmp.Height), bmp.Canvas.Pixels[0, bmp.Height-1]);

//TODO:                      DrawBmp.Canvas.BrushCopy(Item.FIconRect, BM,
//                        Rect(0, 0, BM.Width, BM.Height), BM.Canvas.Pixels[0,
//                          BM.Height-1]);
            finally
              bmp.Free;
            end;
          end;
        end;

        {make the icon's right blend into the label's left}
        R := item.IconRect;
        inc(R.Right, 3);
        item.IconRect := R;

        {now, draw the text}
        R.Top := CurPos;
        R.Bottom := CurPos + FButtonHeight div 2 - 7;
        R.Left := item.IconRect.Right;
        X := FNavBar.ClientWidth - R.Left - 7;
        R.Right := R.Left + X;
        item.LabelRect := R;
        if R.Top > nabItemsRect^.Bottom then
          Break;

        R := item.LabelRect;
        item.DisplayName := GetDisplayString(Canvas, Text, 1, WidthOf(R));
        StrPLCopy(Buf, item.DisplayName, 255);
        DrawText(Canvas.Handle, Buf, Length(item.DisplayName), R, DT_LEFT or DT_VCENTER or DT_CALCRECT);
        labelWidth := WidthOf(R);
        R.Right := R.Left + labelWidth + 1;
        item.LabelRect := R;
        DrawText(Canvas.Handle, Buf, Length(item.DisplayName), R, DT_LEFT or DT_VCENTER);

        Inc(CurPos, FItemSpacing);
      end;  { Small icons }
    end;  { for J }
  end;  { if folder.FolderType = ftDefault }
end;

procedure TVpNavBarPainter.DrawBackground(Canvas: TCanvas; R: TRect);
var
  rowStart: Integer;
  lLeft, lHeight, lWidth: Integer;
begin
  if FBackgroundImage.Empty or (FBackgroundMethod = bmNone) then
  begin
    Canvas.Pen.Color := FBackgroundColor;
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end else
  begin
    case FBackgroundMethod of
      bmNormal:
        Canvas.Draw(R.Left, R.Top, FBackgroundImage);
      bmStretch:
        Canvas.StretchDraw(R, FBackgroundImage);
      bmTile:
        begin
          {Tile the background in the default folder}
          rowStart := 0;
          lHeight := FBackgroundImage.Height;
          lWidth := FBackgroundImage.Width;
          lLeft := 0;
          while (rowStart < FNavBar.ClientRect.Bottom) do begin
            while (lLeft < FNavBar.ClientRect.Right) do begin
              Canvas.Draw(R.Left + lLeft, rowStart, FBackgroundImage);
              Inc(lLeft, lWidth);
            end;
            lLeft := 0;
            Inc(rowStart, lHeight)
          end;
        end;
    end;
  end;
end;

{ Draw the folder buttons at the bottom }
procedure TVpNavBarPainter.DrawBottomFolderButtons(Canvas: TCanvas; ARect: TRect;
  var CurPos: Integer);
var
  I: Integer;
  MyRect: TRect;
begin
  MyRect := ARect;

  Canvas.Font := FNavBar.Font;
  SetBkMode(Canvas.Handle, bkMode);
// todo--->  SetBkColor(Canvas.Handle, bkColor);

  case FDrawingStyle of
    dsDefButton:     // Regular button style
      CurPos := FNavBar.ClientHeight - FButtonHeight;
    dsEtchedButton:  // Etched style (Outlook98)
      CurPos := FNavBar.ClientHeight - FButtonHeight - 1;
    dsCoolTab:       // Cool tab
      CurPos := FNavBar.ClientHeight - FButtonHeight;
    dsStandardTab:   // Regular tab
      CurPos := FNavBar.ClientHeight - FButtonHeight;
  end;

  for I := FNavBar.FolderCount-1 downto FActiveFolder+1 do begin
    MyRect.Top := CurPos;
    MyRect.Bottom := CurPos + FButtonHeight;
    FNavBar.Folders[I].Rect := MyRect;

    {Draw the bottom tabs based on the selected style...}
    DrawTab(Canvas, MyRect, I, I = FHotFolder, I = nabLastMouseOverItem);
    Dec(CurPos, FButtonHeight);
  end;
end;


procedure TVpNavBarPainter.DrawCoolTab(Canvas: TCanvas; R: TRect);
{$IFNDEF VERSION4}
var
  Points: array[1..5] of TPoint;
{$ENDIF}
begin
  with Canvas do begin
    Pen.Color := clBlack;

    {Draw the bottom, left line}
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left + 5, R.Bottom - 1);

    {Draw the bottom, left curve}
   {$IFNDEF VERSION4}
    Points[1] := Point(R.Left + 5,  R.Bottom - 1);
    Points[2] := Point(R.Left + 11, R.Bottom - 2);
    Points[3] := Point(R.Left + 12, R.Bottom - 7);
    Points[4] := Point(R.Left + 13, R.Bottom - 9);
    {$IFDEF CBuilder}
    PolyBezier(Points);
    {$ELSE}
    Polyline(Points);
    {$ENDIF}
   {$ELSE}
    PolyBezier([
      Point(R.Left + 5,  R.Bottom - 1),   {StartPoint}
      Point(R.Left + 11, R.Bottom - 2),   {ControlPoint}
      Point(R.Left + 12, R.Bottom - 7),   {ControlPoint}
      Point(R.Left + 13, R.Bottom - 9)    {EndPoint}
    ]);
   {$ENDIF}

    {Draw the left side of the tab}
    MoveTo(R.Left + 13, R.Bottom - 9);
    LineTo(R.Left + 13, R.Top + 9);

    {Draw the top, left corner of the tab}
   {$IFNDEF VERSION4}
    Points[1] := Point(R.Left + 13, R.Top + 9);
    Points[2] := Point(R.Left + 14, R.Top + 7);
    Points[3] := Point(R.Left + 15, R.Top + 2);
    Points[4] := Point(R.Left + 21, R.Top + 1);
    {$IFDEF CBuilder}
    PolyBezier(Points);
    {$ELSE}
    Polyline(Points);
    {$ENDIF}
   {$ELSE}
    PolyBezier([
      Point(R.Left + 13, R.Top + 9),     {StartPoint}
      Point(R.Left + 14, R.Top + 7),     {ControlPoint}
      Point(R.Left + 15, R.Top + 2),     {ControlPoint}
      Point(R.Left + 21, R.Top + 1)      {EndPoint}
    ]);
   {$ENDIF}

    {Draw the top of the tab}
    MoveTo(R.Left + 21,  R.Top + 1);
    LineTo(R.Right - 16, R.Top + 1);

    {Draw the Top, Right corner of the tab}
   {$IFNDEF VERSION4}
    Points[1] := Point(R.Right - 16, R.Top + 1);
    Points[2] := Point(R.Right - 10, R.Top + 2);
    Points[3] := Point(R.Right -  9, R.Top + 7);
    Points[4] := Point(R.Right -  8, R.Top + 9);
    {$IFDEF CBuilder}
    PolyBezier(Points);
    {$ELSE}
    Polyline(Points);
    {$ENDIF}
   {$ELSE}
    PolyBezier([
      Point(R.Right - 16, R.Top + 1),     {StartPoint}
      Point(R.Right - 10, R.Top + 2),     {ControlPoint}
      Point(R.Right -  9, R.Top + 7),     {ControlPoint}
      Point(R.Right -  8, R.Top + 9)      {EndPoint}
    ]);
    {$ENDIF}

    {Draw the right side of the tab}
    MoveTo(R.Right - 8, R.Top + 9);
    LineTo(R.Right - 8, R.Bottom - 9);

    {Draw the bottom, Right curve of the tab which should finish against the
     right side.}
   {$IFNDEF VERSION4}
    Points[1] := Point(R.Right - 8, R.Bottom - 9);
    Points[2] := Point(R.Right - 7, R.Bottom - 7);
    Points[3] := Point(R.Right - 6, R.Bottom - 2);
    Points[4] := Point(R.Right,     R.Bottom - 1);
    {$IFDEF CBuilder}
    Canvas.PolyBezier(Points);
    {$ELSE}
    Canvas.Polyline(Points);
    {$ENDIF}
   {$ELSE}
    PolyBezier([
      Point(R.Right - 8, R.Bottom - 9),   {StartPoint}
      Point(R.Right - 7, R.Bottom - 7),   {ControlPoint}
      Point(R.Right - 6, R.Bottom - 2),   {ControlPoint}
      Point(R.Right,     R.Bottom - 1)    {EndPoint}
    ]);
   {$ENDIF}
  end;
end;

function TVpNavBarPainter.DrawDefButton(Canvas: TCanvas; R: TRect;
  IsFocused, IsMouseOver: Boolean): TRect;
var
  tb: TThemedButton;
  details: TThemedElementDetails;
begin
  {Draw regular buttons}
  if ThemeServices.ThemesEnabled then begin
    if IsMouseOver then
      tb := tbPushButtonHot
    else
    if FNavBar.Focused and nabMouseDown then
      tb := tbPushButtonPressed
    else
      tb := tbPushButtonNormal;
    details := ThemeServices.GetElementDetails(tb);
    ThemeServices.DrawElement(Canvas.Handle, details, R);
    InflateRect(R, -1, -1);
    if IsFocused then OffsetRect(R, -1, -1);  // Focused
  end;
  Result := R;
//TODO:              TR := DrawButtonFace(DrawBmp.Canvas, MyRect, 1, bsNew, False,
//                (I = FHotFolder) and nabMouseDown, False);
end;

{Draw regular etched (Win98 style) buttons}
function TVpNavBarPainter.DrawEtchedButton(Canvas: TCanvas; R: TRect;
  IsFocused, IsMouseOver: Boolean): TRect;
begin
  with Canvas do begin
    Brush.Color := clBtnFace;
    FillRect(R);
    Pen.Color := clBtnShadow;
    Brush.Style := bsClear;
    Rectangle(R.Left, R.Top, R.Right - 1, R.Bottom);
    Pen.Color := clBtnHighlight;
    MoveTo(R.Left + 1, R.Bottom - 2);
    LineTo(R.Left + 1, R.Top + 1);
    LineTo(R.Right - 2, R.Top + 1);
    { Draw border around control. }
    MoveTo(Width - 1, FNavBar.Top);
    LineTo(Width - 1, FNavBar.Height - 1);
    LineTo(0, FNavBar.Height - 1);
    Pen.Color := clWindowFrame;
    MoveTo(Width - 1, R.Bottom);
    LineTo(1, R.Bottom);
    LineTo(1, FNavBar.Height - 1);
  end;
  Result := R;
end;

{DrawNavTab - returns the usable text area inside the tab rect.}
function TVpNavBarPainter.DrawNavTab(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; TabColor: TColor; TabNumber: Integer;
  CoolTab, IsFocused, IsMouseOver: Boolean): TRect;
var
  R: TRect;
begin
  R := Client;

  with Canvas do begin
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    Pen.Color := TabColor;

    {fill the tab area}
    Polygon([
      Point(R.Left, R.Bottom),
      Point(R.Left, R.Top),
      Point(R.Right, R.Top),
      Point(R.Right, R.Bottom)
    ]);

    if CoolTab then
      { --- Draw Cool Tabs --- }
      DrawCoolTab(Canvas, R)
    else
      { --- Draw Standard Tabs --- }
      DrawStandardTab(Canvas, R, TabColor, TabNumber)
  end;

  Result := Rect(Client.Left + 1, Client.Top + 2, Client.Right - 2, Client.Bottom);
  if IsFocused then OffsetRect(Result, -1, -1);
end;

procedure TVpNavBarPainter.DrawStandardTab(Canvas: TCanvas; R: TRect;
  TabColor: TColor; TabCount: Integer);
begin
  dec(R.Top);

  if TabCount > 0 then begin
    Canvas.Brush.Color := TabColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Color := TabColor;

    {fill the tab area}
    Canvas.Polygon([
      Point(R.Left, R.Bottom),
      Point(R.Left, R.Top),
      Point(R.Right, R.Top),
      Point(R.Right, R.Bottom)
    ]);
  end;

  Canvas.Brush.Color := TabColor;
  Canvas.Brush.Style := bsSolid;

  {Draw Tab}
  Canvas.Pen.Color := TabColor;
  Canvas.Polygon([
    Point(R.Left + 10, R.Bottom - 1),
    Point(R.Left + 10, R.Top + 3),
    Point(R.Left + 12, R.Top + 1),
    Point(R.Right - 4, R.Top + 1),
    Point(R.Right - 2, R.Top + 3),
    Point(R.Right - 2, R.Bottom - 1)
  ]);

  {highlight tab}
  Canvas.Pen.Color := clBtnHighlight;
  Canvas.PolyLine([
    Point(R.Left, R.Bottom - 2),
    Point(R.Left + 8, R.Bottom - 2),
    Point(R.Left + 9, R.Bottom - 3),
    Point(R.Left + 9, R.Top + 3),
    Point(R.Left + 11, R.Top + 1),
    Point(R.Right - 1, R.Top + 1)
  ]);

  {draw border}
  Canvas.Pen.Color := clBlack;
  Canvas.PolyLine([
    Point(R.Left, R.Bottom - 1),
    Point(R.Left + 9, R.Bottom - 1),
    Point(R.Left + 10, R.Bottom - 2),
    Point(R.Left + 10, R.Top + 4),
    Point(R.Left + 11, R.Top + 3),
    Point(R.Left + 12, R.Top + 2),
    Point(R.Right - 2, R.Top + 2),
    Point(R.Right - 1, R.Top + 3),
    Point(R.Right - 1, R.Bottom - 1)]);

  {draw shadow}
end;

function TVpNavBarPainter.DrawTab(Canvas: TCanvas; R: TRect; ATabIndex: Integer;
  IsFocused, IsMouseOver: Boolean): TRect;
var
  Buf: array[0..255] of Char;
  TR: TRect;
  Flags: Integer;
  lOffset: Integer;
begin
  case FDrawingStyle of
    dsDefButton:
      Result := DrawDefButton(Canvas, R, IsFocused, IsMouseOver);
    dsEtchedButton:
      Result := DrawEtchedButton(Canvas, R, IsFocused, IsMouseOver);
    dsCoolTab:
      Result := DrawNavTab(Canvas, R, 1, FBackgroundColor, ATabIndex, true, IsFocused, IsMouseOver);
    dsStandardTab:
      {Draw regular old tabs}
      Result := DrawNavTab(Canvas, R, 1, FBackgroundColor, ATabIndex, false, IsFocused, IsMouseOver);
  end;

  TR := Result;

  StrPLCopy(Buf, FNavBar.Folders[ATabIndex].DisplayName, 255);
  Inc(TR.Top);
  Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
  if FNavBar.Folders[ATabIndex].Enabled then begin
    DrawText(Canvas.Handle, Buf, StrLen(Buf), TR, Flags);
    if (ATabIndex = FHotFolder) and not nabMouseDown then begin
      case FDrawingStyle of
        dsDefButton:
          begin { Regular button style. }
            InflateRect(TR, 1, 1);
            inc(TR.Left);
            Canvas.Frame3D(TR, 1, bvRaised);
          end;

        dsEtchedButton:
          begin { Etched style (Outlook98). }
            Canvas.Pen.Color := clWindowFrame;
            Canvas.MoveTo(TR.Right - 2, TR.Top);
            Canvas.LineTo(TR.Right - 2, TR.Bottom - 1);
            Canvas.LineTo(0, TR.Bottom - 1);
            Canvas.Pen.Color := clBtnShadow;
            if ATabIndex = FActiveFolder then
              lOffset := 1
            else
              lOffset := 2;
            Canvas.MoveTo(TR.Right - 3, TR.Top - 2);
            Canvas.LineTo(TR.Right - 3, TR.Bottom - lOffset);
            Canvas.LineTo(1, TR.Bottom - lOffset);
            if ATabIndex = FActiveFolder then
              Canvas.Pixels[1, TR.Bottom - lOffset] := clBtnHighlight;
          end;
      end;  // case
    end;
  end else
  begin
    {use shadow text for inactive folder text}
    Canvas.Font.Color := clHighlightText;
    SetBkMode(Canvas.Handle, OPAQUE);
    DrawText(Canvas.Handle, Buf, -1, TR, Flags);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.Font.Color := clBtnShadow;
    OffsetRect(TR, -2, -1);
    DrawText(Canvas.Handle, Buf, -1, TR, Flags);
    Canvas.Font.Color := FNavBar.Font.Color;
  end;
end;

procedure TVpNavBarPainter.DrawTopFolderButtons(Canvas: TCanvas;
  ARect: TRect; DrawFolder: Boolean; var CurPos: Integer);
var
  I: Integer;
  MyRect: TRect;
  TR: TRect;
begin
  CurPos := 0;
  MyRect := ARect;

  { Draw the folder buttons at the top }
  if DrawFolder then begin
    for I := 0 to FActiveFolder do begin
      MyRect.Top := CurPos;
      MyRect.Bottom := CurPos + FButtonHeight;
      FNavBar.Folders[I].Rect := MyRect;

      {Draw the top tabs based on the selected style...}
      TR := DrawTab(Canvas, MyRect, I, I = FHotFolder, I = nabLastMouseOverItem);
      Inc(CurPos, FButtonHeight);
    end;
  end else begin
    if FDrawingStyle = dsEtchedButton then begin
      { Draw border around control. }
      Canvas.Pen.Color := clBtnHighlight;
      Canvas.MoveTo(FNavBar.Width - 1, FNavBar.Top);
      Canvas.LineTo(FNavBar.Width - 1, FNavBar.Height - 1);
      Canvas.LineTo(0, FNavBar.Height - 1);
      Canvas.Pen.Color := clWindowFrame;
      Canvas.MoveTo(0, FNavBar.Height - 1);
      Canvas.LineTo(0, 1);
      Canvas.LineTo(FNavBar.Width - 2, 1);
    end;
    CurPos := 0;
  end;
end;

procedure TVpNavBarPainter.Paint;
var
  DrawBmp: TBitmap;
  DrawFolder: Boolean;
  TR: TRect;
  CurPos: Integer;
  I: Integer;
  MyRect: TRect;
begin
  MyRect := FNavBar.ClientRect;

  DrawBmp := TBitmap.Create;
  try
    DrawBmp.Width := FClientWidth;
    DrawBmp.Height := FClientHeight;

    DrawBmp.Canvas.Font := FNavBar.Font;
    DrawBmp.Canvas.Pen.Color := FBackgroundColor;
    DrawBmp.Canvas.Brush.Color := FBackgroundColor;

    DrawFolder := (FNavBar.FolderCount > 0);
    if DrawFolder then
      TR := FFolderArea
    else
      TR := FNavBar.ClientRect;

    { Draw background }
    DrawBackground(DrawBmp.Canvas, TR);

    if FNavBar.FolderCount = 0 then begin
      nabScrollUpBtn.Visible := False;
      nabScrollDownBtn.Visible := False;
      Exit;
    end;

    { Draw the folder buttons at the top }
    DrawTopFolderButtons(DrawBmp.Canvas, MyRect, DrawFolder, CurPos);

    { Draw active folder items }
    DrawActiveFolderItems(DrawBmp.Canvas, CurPos);

    { Draw the folder buttons at the bottom }
    DrawBottomFolderButtons(DrawBmp.Canvas, MyRect, CurPos);

  finally
    FNavBar.Canvas.CopyMode := cmSrcCopy;
    FNavBar.Canvas.CopyRect(FNavBar.ClientRect, DrawBmp.Canvas, FNavBar.ClientRect);
    DrawBmp.Free;
  end;
end;





(*
var
  I, J: Integer;
  X: Integer;
  W, H: Integer;
  CurPos: Integer;
  lOffset: Integer;
  BkMode: Integer;
  LabelWidth: Integer;
  Flags: Integer;
  MyRect: TRect;
  TR: TRect;
//  ContainerRect: TRect;
//  FolderType: TVpFolderType;
  BkColor: TColor;
  Folder: TVpNavFolder;
  Item: TVpNavBtnItem;
  DrawBmp: TBitmap;
  Text: string;
  Buf: array[0..255] of Char;
  DrawFolder: Boolean;
  BM: TBitmap;
  RowStart: Integer;
  ILeft: Integer;
  IHeight: Integer;
  IWidth: integer;
  Details: TThemedElementDetails;
  TB: TThemedButton;
begin
  if nabChanging then
    Exit;

  DrawBmp := TBitMap.Create;
  try
    DrawBmp.Width := ClientWidth;
    DrawBmp.Height := ClientHeight;

    DrawBmp.Canvas.Font := Self.Font;
    with DrawBmp.Canvas do begin
      Pen.Color := FBackgroundColor;
      Brush.Color := FBackgroundColor;

      MyRect := ClientRect;

      DrawFolder := (FolderCount > 0);
      if DrawFolder then
        TR := nabGetFolderArea(FActiveFolder)
      else
        TR := ClientRect;

      if FBackgroundImage.Empty or (FBackgroundMethod = bmNone) then
        Rectangle(TR.Left, TR.Top, TR.Right, TR.Bottom)
      else begin
        case FBackgroundMethod of
          bmNormal:
            Draw(TR.Left, TR.Top, FBackgroundImage);
          bmStretch:
            StretchDraw(TR, FBackgroundImage);
          bmTile:
            begin
              {Tile the background in the default folder}
              RowStart := 0;
              IHeight := FBackgroundImage.Height;
              IWidth := FBackgroundImage.Width;
              ILeft := 0;
              while (RowStart < ClientRect.Bottom) do begin
                while (ILeft < ClientRect.Right) do begin
                  Draw(TR.Left + ILeft, RowStart, FBackgroundImage);
                  Inc(ILeft, IWidth);
                end;
                ILeft := 0;
                Inc(RowStart, IHeight)
              end;
            end;
        end;
      end;

      CurPos := 0;
      if FolderCount = 0 then begin
        nabScrollUpBtn.Visible := False;
        nabScrollDownBtn.Visible := False;
        Exit;
      end;

      {draw the folder buttons at the top}
      if DrawFolder then begin
        for I := 0 to FActiveFolder do begin
          MyRect.Top := CurPos;
          MyRect.Bottom := CurPos + FButtonHeight;
          Folders[I].lfRect := MyRect;

          {Draw the top tabs based on the selected style...}
          case FDrawingStyle of
            dsDefButton:
              begin
                {Draw regular buttons}
                if ThemeServices.ThemesEnabled then begin
                  if (I = nabLastMouseOverItem) then
                    TB := tbPushButtonHot
                  else
                  if (I = FHotFolder) and nabMouseDown then
                    TB := tbPushButtonPressed
                  else
                    TB := tbPushButtonNormal;
                  Details := ThemeServices.GetElementDetails(TB);
                  ThemeServices.DrawElement(Handle, details, MyRect);
                  TR := MyRect;
                  InflateRect(TR, -1, -1);
                  if I = FHotFolder then OffsetRect(TR, -1, -1);  // Focused
                end;
//TODO:              TR := DrawButtonFace(DrawBmp.Canvas, MyRect, 1, bsNew, False,
//                (I = FHotFolder) and nabMouseDown, False);
              end;

            dsEtchedButton:
              begin
                {Draw regular etched (Win98 style) buttons}
                Brush.Color := clBtnFace;
                FillRect(MyRect);
                Pen.Color := clBtnShadow;
                Brush.Style := bsClear;
                Rectangle(MyRect.Left, MyRect.Top, MyRect.Right - 1, MyRect.Bottom);
                Pen.Color := clBtnHighlight;
                MoveTo(MyRect.Left + 1, MyRect.Bottom - 2);
                LineTo(MyRect.Left + 1, MyRect.Top + 1);
                LineTo(MyRect.Right - 2, MyRect.Top + 1);
                { Draw border around control. }
                MoveTo(Width - 1, Top);
                LineTo(Width - 1, Height - 1);
                LineTo(0, Height - 1);
                Pen.Color := clWindowFrame;
                MoveTo(Width - 1, MyRect.Bottom);
                LineTo(1, MyRect.Bottom);
                LineTo(1, Height - 1);
                TR := MyRect;
              end;

           dsCoolTab:
             begin
               {Draw cool (Netscape Sidebar style) tabs}
               TR := DrawNavTab(
                 DrawBmp.Canvas,              {Canvas}
                 MyRect,                      {Client Rect}
                 1,                           {Bevel Width}
                 FBackgroundColor,            {Tab Color}
                 I,                           {Tab Number}
                 true,                        {Cool Tabs?}
                 (I = FHotFolder),            {Is Focused}
                 (I = nabLastMouseOverItem)   {MouseOverItem}
               );
             end;

           dsStandardTab:
             begin
               {Draw regular old tabs}
               TR := DrawNavTab(
                 DrawBmp.Canvas,              {Canvas}
                 MyRect,                      {Client Rect}
                 1,                           {Bevel Width}
                 FBackgroundColor,            {Tab Color}
                 I,                           {Tab Number}
                 false,                       {Cool Tabs?}
                 (I = FHotFolder),            {Is Focused}
                 (I = nabLastMouseOverItem)   {MouseOverItem}
               );
            end;

          end;
          StrPLCopy(Buf, Folders[I].lfDisplayName, 255);
          Inc(TR.Top);
          Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
          if Folders[I].Enabled then begin
            DrawText(DrawBmp.Canvas.Handle, Buf, StrLen(Buf), TR, Flags);
            if (I = FHotFolder) and not nabMouseDown then begin
              case FDrawingStyle of
                dsDefButton:
                  begin
                    { Regular button style. }
                    InflateRect(TR,1,1);
                    inc(TR.Left);
                    DrawBmp.Canvas.Frame3D(TR, 1,bvRaised);
                  end;

                dsEtchedButton:
                  begin
                    { Etched style (Outlook98). }
                    Pen.Color := clWindowFrame;
                    MoveTo(TR.Right - 2, TR.Top);
                    LineTo(TR.Right - 2, TR.Bottom - 1);
                    LineTo(0, TR.Bottom - 1);
                    Pen.Color := clBtnShadow;
                    if I = ActiveFolder then
                      lOffset := 1
                    else
                      lOffset := 2;
                    MoveTo(TR.Right - 3, TR.Top - 2);
                    LineTo(TR.Right - 3, TR.Bottom - lOffset);
                    LineTo(1, TR.Bottom - lOffset);
                    if I = ActiveFolder then
                      Pixels[1, TR.Bottom - lOffset] := clBtnHighlight;
                  end;
              end;  // case
            end;
          end else
          begin
            {use shadow text for inactive folder text}
            DrawBmp.Canvas.Font.Color := clHighlightText;
            SetBkMode(Canvas.Handle, OPAQUE);
            DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
            SetBkMode(DrawBmp.Canvas.Handle, TRANSPARENT);
            DrawBmp.Canvas.Font.Color := clBtnShadow;
            OffsetRect(TR, -2, -1);
            DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
            DrawBmp.Canvas.Font.Color := Self.Font.Color;
          end;
          Inc(CurPos, FButtonHeight);
        end;
      end else
      begin
        if FDrawingStyle = dsEtchedButton then begin
          { Draw border around control. }
          Pen.Color := clBtnHighlight;
          MoveTo(Width - 1, Top);
          LineTo(Width - 1, Height - 1);
          LineTo(0, Height - 1);
          Pen.Color := clWindowFrame;
          MoveTo(0, Height - 1);
          LineTo(0, 1);
          LineTo(Width - 2, 1);
        end;
        CurPos := 0;
      end;

//TODO:
{      BkMode := GetBkMode(Handle);
      BkColor := GetBkColor(Handle);
      SetBkColor(Handle, DWord(FBackgroundColor));
      SetBkMode(Handle, TRANSPARENT);
}
      { draw the items for the active folder }
      Folder := Folders[FActiveFolder];

      if Folder.FolderType = ftDefault then
        if Folder.ItemCount > 0 then begin
          Inc(CurPos, 8);
          with nabItemsRect do begin
            Top := CurPos;
            Left := 0;
            Right := ClientWidth;
            Bottom := ClientHeight - (FolderCount - FActiveFolder - 1) * FButtonHeight + 1;
          end;

          for J := 0 to Folder.ItemCount-1 do
            TVpNavBtnItem(Folder.Items[J]).FLabelRect.Bottom := nabItemsRect.Bottom + 1;

          for J := nabTopItem to Folder.ItemCount-1 do begin
            if (FSelectedItem = J) then
              DrawBmp.Canvas.Font := FSelectedItemFont
            else
              DrawBmp.Canvas.Font := FItemFont;

            Item := Folder.Items[J];
            { If the caption is empty at designtime then display the item's }
            { name instead                                                  }
            if (csDesigning in ComponentState) and (Item.Caption = '') then
              Text := Item.Name
            else
              Text := Item.Caption;

            if Folder.IconSize = isLarge then begin {large icons}
              { glyph is at the top }
              with Item.FIconRect do begin
                { If an image list is assigned then use the image }
                { size. If no image list is assinged then assume  }
                { a 32 x 32 image size.                           }
                if Assigned(FImages) then begin
                  W := FImages.Width + 2;
                  H := FImages.Height + 2;
                end else begin
                  W := 32;
                  H := 32;
                end;
                Top := CurPos;
                Bottom := CurPos + H;
                Left := (ClientWidth - W) shr 1;
                Right := Left + W;
                if Top > nabItemsRect.Bottom then
                  Break;

                if FShowButtons then begin
                  if FActiveItem = J then begin
                    if nabMouseDown then
                      Pen.Color := clBlack
                    else
                      Pen.Color := clWhite;
                    MoveTo(Left-1, Bottom+1);
                    LineTo(Left-1, Top-1);
                    LineTo(Right+1, Top-1);
                    if nabMouseDown then
                      Pen.Color := clWhite
                    else
                      Pen.Color := clBlack;
                    LineTo(Right+1, Bottom+1);
                    LineTo(Left-1, Bottom+1);
                  end else begin
                    Pen.Color := FBackgroundColor;
                    Brush.Color := FBackgroundColor;
                  end;
                  if Assigned(FImages) and
                     (Item.IconIndex >= 0) and
                     (Item.IconIndex < FImages.Count) then
                    FImages.Draw(DrawBmp.Canvas, Item.FIconRect.Left + 2,
                      Item.FIconRect.Top + 2, Item.IconIndex);
                  {make the icon's bottom blend into the label's top}
                  Item.FIconRect.Bottom := Item.FIconRect.Bottom + 4;
                end;
              end;
              Inc(CurPos, H + 4);

              {now, draw the text}
              with Item.FLabelRect do begin
                Top := CurPos;
                Bottom := CurPos + (FButtonHeight shl 1) - 7;
                Left := 0;
                Right := ClientWidth - 1;
                Item.liDisplayName := GetLargeIconDisplayName(DrawBmp.Canvas, Item.FLabelRect, Text);
                X := DrawBmp.Canvas.TextWidth(Item.liDisplayName);
                Left := (ClientWidth - X) div 2;
                if Left < 5 then
                  Left := 5;
                Right := Left + X;
                if Right > ClientWidth-5 then
                  Right := ClientWidth-5;
                if Top > nabItemsRect.Bottom then
                  Break;
              end;

              StrPLCopy(Buf, Item.liDisplayName, 255);
              DrawText(DrawBmp.Canvas.Handle, Buf, Length(Item.liDisplayName), Item.FLabelRect,
                DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_CALCRECT);
              LabelWidth := RectWidth(Item.FLabelRect);
              with Item.FLabelRect do begin
                Left := (ClientWidth - LabelWidth) div 2;
                Right := Left + LabelWidth + 1;
              end;
              BkMode := SetBkMode(DrawBmp.Canvas.Handle, TRANSPARENT);
              Inc(CurPos, DrawText(
                DrawBmp.Canvas.Handle, Buf, Length(Item.liDisplayName),
                Item.FLabelRect, DT_CENTER or DT_VCENTER or DT_WORDBREAK)
              );
              SetBkMode(DrawBmp.Canvas.Handle, BkMode);

              Inc(CurPos, FItemSpacing);
            end else begin {small icons}
              {glyph is at the left}
              with Item.FIconRect do begin
                Top := CurPos;
                lOffset := Abs(DrawBmp.Canvas.Font.Height) div 2;
                if lOffset > 8 then
                  Top := Top + lOffset - 8;
                Bottom := Top + 16;
                Left := 8;
                Right := Left + 16;
                if Top > nabItemsRect.Bottom then
                  Break;

                if FShowButtons then begin
                  if FActiveItem = J then begin
                    if nabMouseDown then
                      Pen.Color := clBlack
                    else
                      Pen.Color := clWhite;
                    MoveTo(Left-1, Bottom+1);
                    LineTo(Left-1, Top-1);
                    LineTo(Right+1, Top-1);
                    if nabMouseDown then
                      Pen.Color := clWhite
                    else
                      Pen.Color := clBlack;
                    LineTo(Right+1, Bottom+1);
                    LineTo(Left-1, Bottom+1);
                    Brush.Color := FBackgroundColor;
                  end else begin
                    Pen.Color := FBackgroundColor;
                    Brush.Color := FBackgroundColor;
                    Rectangle(
                      Item.FIconRect.Left - 1,
                      Item.FIconRect.Top - 1,
                      Item.FIconRect.Right + 1,
                      Item.FIconRect.Bottom + 1
                    );
                  end;
                  if Assigned(FImages) then begin
                    BM := TBitmap.Create;
                    try
                      BM.Width := FImages.Width;
                      BM.Height := FImages.Height;
                      FImages.Draw(BM.Canvas, 0, 0, Item.IconIndex);
//TODO:                      DrawBmp.Canvas.BrushCopy(Item.FIconRect, BM,
//                        Rect(0, 0, BM.Width, BM.Height), BM.Canvas.Pixels[0,
//                          BM.Height-1]);
                    finally
                      BM.Free;
                    end;
                  end;
                end;
                {make the icon's right blend into the label's left}
                Item.FIconRect.Right := Item.FIconRect.Right + 3;
              end;

              {now, draw the text}
              with Item.FLabelRect do begin
                Top := CurPos;
                Bottom := CurPos + (FButtonHeight shl 1) -7;
                Left := Item.FIconRect.Right;
                X := Self.ClientWidth - Left - 7;
                Right := Left + X;
                if Top > nabItemsRect.Bottom then
                  Break;
              end;
              Item.liDisplayName :=
                GetDisplayString(DrawBmp.Canvas, Text, 1, RectWidth(Item.FLabelRect));
              StrPLCopy(Buf, Item.liDisplayName, 255);
              DrawText(DrawBmp.Canvas.Handle, Buf, Length(Item.liDisplayName),
                Item.FLabelRect, DT_LEFT or DT_VCENTER or DT_CALCRECT);
              LabelWidth := RectWidth(Item.FLabelRect);
              with Item.FLabelRect do
                Right := Left + LabelWidth + 1;
              DrawText(DrawBmp.Canvas.Handle, Buf, Length(Item.liDisplayName),
                Item.FLabelRect, DT_LEFT or DT_VCENTER);

              Inc(CurPos, FItemSpacing);
            end;
          end;
      end;

      {now, draw the folder buttons at the bottom}
      DrawBmp.Canvas.Font := Self.Font;
      SetBkMode(Handle, BkMode);
      SetBkColor(Handle, BkColor);

      case FDrawingStyle of
        { Regular button style. }
        dsDefButton :
          CurPos := ClientHeight - FButtonHeight;
        { Etched style (Outlook98). }
        dsEtchedButton :
          CurPos := ClientHeight - FButtonHeight - 1;
        { Cool Tab }
        dsCoolTab:
          CurPos := ClientHeight - FButtonHeight;
        { Regular Tab }
        dsStandardTab:
          CurPos := ClientHeight - FButtonHeight;
      end;

      for I := FolderCount-1 downto FActiveFolder+1 do begin
        MyRect.Top := CurPos;
        MyRect.Bottom := CurPos + FButtonHeight;
        Folders[I].lfRect := MyRect;
        case FDrawingStyle of
          dsDefButton :
            begin
              {Regular Old Buttons}
              if ThemeServices.ThemesEnabled then begin
                if (I = nabLastMouseOverItem) then
                  TB := tbPushButtonHot
                else
                if (I = FHotFolder) and nabMouseDown then
                  TB := tbPushButtonPressed
                else
                  TB := tbPushButtonNormal;
                Details := ThemeServices.GetElementDetails(TB);
                ThemeServices.DrawElement(Handle, details, MyRect);
                TR := MyRect;
                InflateRect(TR, -1, -1);
                if I = FHotFolder then OffsetRect(TR, -1, -1);  // Focused
              end;

            //TODO:            TR := DrawButtonFace(DrawBmp.Canvas, MyRect, 1, bsNew, False,
//              (I = FHotFolder) and nabMouseDown, False);
            end;

          dsEtchedButton :
            begin
              {Etched (Outlook98 style) buttons}
              Brush.Color := clBtnFace;
              FillRect(MyRect);
              Pen.Color := clBtnShadow;
              Brush.Style := bsClear;
              Rectangle(MyRect.Left, MyRect.Top, MyRect.Right - 1, MyRect.Bottom);
              Pen.Color := clBtnHighlight;
              MoveTo(MyRect.Left + 1, MyRect.Bottom - 2);
              LineTo(MyRect.Left + 1, MyRect.Top + 1);
              LineTo(MyRect.Right - 2, MyRect.Top + 1);
              Pen.Color := clBtnHighlight;
              MoveTo(Width - 1, 0);
              LineTo(Width - 1, Height);
              TR := MyRect;
            end;

          dsCoolTab:
            begin
              {Draw cool (Netscape Sidebar style) tabs}
              TR := DrawNavTab(
                DrawBmp.Canvas,              {Canvas}
                MyRect,                      {Client Rect}
                1,                           {Bevel Width}
                FBackgroundColor,            {Tab Color}
                I,                           {Tab Number}
                true,                        {Cool Tabs?}
                (I = FHotFolder),             {Is Focused}
                (I = nabLastMouseOverItem)    {MouseOverItem}
              );
          end;

          dsStandardTab:
            begin
              {Draw regular old tabs}
              TR := DrawNavTab(
                DrawBmp.Canvas,              {Canvas}
                MyRect,                      {Client Rect}
                1,                           {Bevel Width}
                FBackgroundColor,            {Tab Color}
                I,                           {Tab Number}
                false,                       {Cool Tabs?}
                (I = FHotFolder),            {Is Focused}
                (I = nabLastMouseOverItem)   {MouseOverItem}
              );
          end;

        end;
        Inc(TR.Top);
        StrPLCopy(Buf, Folders[I].lfDisplayName, 255);
        Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
        if Folders[I].Enabled then begin
          DrawText(DrawBmp.Canvas.Handle, Buf, StrLen(Buf), TR, Flags);
          if (I = FHotFolder) and not nabMouseDown then begin
            case FDrawingStyle of
              dsDefButton:
                begin
                  { Regular button style. }
                  InflateRect(TR,1,1);
                  inc(TR.Left);
                  DrawBmp.Canvas.Frame3D(TR,1,bvRaised);
                end;

              dsEtchedButton :
                begin
                  { Etched (Outlook98 style). }
                  Pen.Color := clWindowFrame;
                  MoveTo(TR.Right - 2, TR.Top);
                  LineTo(TR.Right - 2, TR.Bottom - 1);
                  LineTo(0, TR.Bottom - 1);
                  Pen.Color := clBtnShadow;
                  MoveTo(TR.Right - 3, TR.Top - 2);
                  LineTo(TR.Right - 3, TR.Bottom - 2);
                  LineTo(1, TR.Bottom - 2);
                end;
            end;
          end;
        end else begin
          {use shadow text for inactive folder text}
          DrawBmp.Canvas.Font.Color := clHighlightText;
          SetBkMode(Canvas.Handle, OPAQUE);
          DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
          SetBkMode(DrawBmp.Canvas.Handle, TRANSPARENT);
          DrawBmp.Canvas.Font.Color := clBtnShadow;
          OffsetRect(TR, -2, -1);
          DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
          DrawBmp.Canvas.Font.Color := Self.Font.Color;
        end;
        Dec(CurPos, FButtonHeight);
      end;

      if not (csDesigning in ComponentState) then begin
        {show the top scroll button}
        if nabShowScrollUp then begin
          nabScrollUpBtn.Top := Folders[FActiveFolder].lfRect.Bottom + 5;
          nabScrollUpBtn.Left := ClientWidth - 20;
          nabScrollUpBtn.Visible := True;
        end else
          nabScrollUpBtn.Visible := False;

        {show the bottom scroll button}
        if nabShowScrollDown then begin
          if FActiveFolder = FolderCount-1 then
            {there are no folders beyond the active one}
            nabScrollDownBtn.Top := ClientHeight -20
          else
            nabScrollDownBtn.Top := Folders[FActiveFolder+1].lfRect.Top - 20;
          nabScrollDownBtn.Left := ClientWidth - 20;
          nabScrollDownBtn.Visible := True;
        end else
          nabScrollDownBtn.Visible := False;
      end;

      {if we're dragging, show the drag marker}
      if (nabDragFromItem <> -1) or nabExternalDrag then begin
        if (nabDropY <> -1) then begin
          { Don't draw the drag marker if we're doing external }
          { dragging and the cursor is over an item. }
          if nabExternalDrag then
            if not nabFolderAccept or nabCursorOverItem then
              Exit;
          Pen.Color := clBlack;
          Brush.Color := clBlack;
          MoveTo(5, nabDropY);
          LineTo(ClientWidth - 5, nabDropY);
          DrawBmp.Canvas.Polygon([
            Point(3, nabDropY+4),
            Point(7, nabDropY),
            Point(3, nabDropY-4)
          ]);
          DrawBmp.Canvas.FloodFill(5, nabDropY, clBlack, fsBorder);
          DrawBmp.Canvas.Polygon([
            Point(ClientWidth-3,nabDropY+4),
            Point(ClientWidth-7,nabDropY),
            Point(ClientWidth-3,nabDropY-4)
          ]);
          DrawBmp.Canvas.FloodFill(ClientWidth-5, nabDropY, clBlack, fsBorder);
        end;
      end;
    end;
  finally
    Canvas.CopyMode := cmSrcCopy;
    Canvas.CopyRect(ClientRect, DrawBmp.Canvas, ClientRect);
    DrawBmp.Free;
  end;

  {For container style folders...}

  {Hide the containers for all inactive folders}
  for I := 0 to FFolders.Count - 1 do begin
    if I <> FActiveFolder then begin
      if Folders[i].FolderType = ftContainer then
      with Containers[Folders[i].ContainerIndex] do begin
        Width := 0;
        Height := 0;
        Visible := false;
      end;
    end;
  end;

  Folder := Folders[FActiveFolder];
  TR := nabGetFolderArea(FActiveFolder);

  if Folder.FolderType = ftContainer then
  with Containers[Folder.ContainerIndex] do begin
  {Position and show the folder's container}
    Height := TR.Bottom - TR.Top;
    Top := TR.Top;
    Left := TR.Left;
    Width := TR.Right - TR.Left;
    Visible := true;
    BringToFront;

    for I := 0 to ControlCount - 1 do
      Controls[i].Invalidate;
  end;
end;
*)

{ Given a string, and a rectangle, find the string that can be displayed
  using two lines. Add ellipsis to the end of each line if necessary and
  possible}
function GetLargeIconDisplayName(Canvas: TCanvas; Rect: TRect;
  const Name: string): string;
var
  TestRect: TRect;
  SH, DH: Integer;
  Buf: array[0..255] of Char;
  I: Integer;
  TempName: string;
  Temp2: string;
begin
  TempName := Trim(Name);
  {get single line height}
  with TestRect do begin
    Left := 0;
    Top := 0;
    Right := 1;
    Bottom := 1;
  end;
  SH := DrawText(Canvas.Handle, 'W W', 3, TestRect, DT_SINGLELINE or DT_CALCRECT);

  {get double line height}
  with TestRect do begin
    Left := 0;
    Top := 0;
    Right := 1;
    Bottom := 1;
  end;
  DH := DrawText(Canvas.Handle, 'W W', 3, TestRect, DT_WORDBREAK or DT_CALCRECT);

  {see if the text can fit within the existing rect without growing}
  TestRect := Rect;
  StrPLCopy(Buf, TempName, 255);
  DrawText(Canvas.Handle, Buf, Length(TempName), TestRect, DT_WORDBREAK or DT_CALCRECT);
  I := Pos(' ', TempName);
  if (HeightOf(TestRect) = SH) or (I < 2) then
    Result := GetDisplayString(Canvas, TempName, 1, WidthOf(Rect))
  else begin
    {the first line only has ellipsis if there's only one word on it and
    that word won't fit}
    Temp2 := GetDisplayString(Canvas, Copy(TempName, 1, I-1), 1, WidthOf(Rect));
    if CompareStr(Temp2, Copy(TempName, 1, I-1)) <> 0 then begin
      Result := GetDisplayString(Canvas, Copy(TempName, 1, I-1), 1, WidthOf(Rect)) + ' ' +
        GetDisplayString(Canvas, Copy(TempName, I+1, Length(TempName) - I), 1, WidthOf(Rect));
    end else begin
      {2 or more lines, and the first line isn't getting an ellipsis}
      if (HeightOf(TestRect) = DH) and (WidthOF(TestRect) <= WidthOf(Rect)) then
        {it will fit}
        Result := TempName
      else begin
        {it won't fit, but the first line wraps OK - 2nd line needs an ellipsis}
        TestRect.Right := Rect.Right + 1;
        while (WidthOf(TestRect) > WidthOf(Rect)) or (HeightOf(TestRect) > DH) do
        begin
          if Length(TempName) > 1 then begin
            TestRect := Rect;
            Delete(TempName, Length(TempName), 1);
            TempName := Trim(TempName);
            StrPLCopy(Buf, TempName + '...', 255);
            DrawText(Canvas.Handle, Buf, Length(TempName) + 3, TestRect, DT_WORDBREAK or DT_CALCRECT);
            Result := TempName + '...';
          end else begin
            Result := TempName + '..';
            TestRect := Rect;
            StrPLCopy(Buf, Result, 255);
            DrawText(Canvas.Handle, Buf, Length(Result), TestRect, DT_WORDBREAK or DT_CALCRECT);
            if (WidthOf(TestRect) <= WidthOf(Rect)) and (HeightOf(TestRect) > DH) then
              Break;
            Result := TempName + '.';
            TestRect := Rect;
            StrPLCopy(Buf, Result, 255);
            DrawText(Canvas.Handle, Buf, Length(Result), TestRect, DT_WORDBREAK or DT_CALCRECT);
            if (WidthOf(TestRect) <= WidthOf(Rect)) and (HeightOf(TestRect) > DH) then
              Break;
            Result := TempName;
          end;
        end;
      end;
    end;
  end;
end;

end.

