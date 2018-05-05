{$I vp.inc}

unit VpNavBarPainter;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf, LCLVersion,
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
    FSmallImagesSize: Integer;
    FLargeImagesSize: Integer;

    nabItemsRect: PRect;
    nabLastMouseOverItem: Integer;
    nabMouseDown: Boolean;
    nabScrollUpBtn: TSpeedButton;
    nabScrollDownBtn: TSpeedButton;
    nabTopItem: Integer;

    FFolderArea: TRect;

    procedure DrawBackground(Canvas: TCanvas; R: TRect);

    function DrawCoolTab(Canvas: TCanvas; R: TRect;
      ATabIndex: Integer; ATabColor: TColor): TRect;
    function DrawDefButton(Canvas: TCanvas; R: TRect;
      ATabIndex: Integer): TRect;
    function DrawEtchedButton(Canvas: TCanvas; R: TRect;
      ATabIndex: Integer): TRect;
    function DrawStandardTab(Canvas: TCanvas; R: TRect;
      ATabIndex: Integer; ATabColor: TColor): TRect;

    procedure DrawItemHighlight(Canvas: TCanvas; R: TRect; Enable: Boolean);
    function DrawItemText(Canvas: TCanvas; AItem: TVpNavBtnItem; CurPos: Integer;
      AText: String; AtLargeIcon: Boolean; out AHeight: Integer): Boolean;
    function DrawLargeIcon(Canvas: TCanvas; AItem: TVpNavBtnItem;
      CurPos: Integer): Boolean;
    function DrawSmallIcon(Canvas: TCanvas; AItem: TVpNavBtnItem;
      CurPos: Integer): Boolean;

    function IsFocused(ATabIndex: Integer): Boolean;
    function IsMouseOverFolder(ATabIndex: Integer): Boolean;
    function IsMouseOverItem(ATabIndex: Integer): Boolean;

  protected
    procedure DrawActiveFolderItems(Canvas: TCanvas; var CurPos: Integer);
    procedure DrawBottomFolderButtons(Canvas: TCanvas; ARect: TRect;
      var CurPos: Integer);
    procedure DrawTab(Canvas: TCanvas; R: TRect; ATabIndex: Integer);
    procedure DrawTopFolderButtons(Canvas: TCanvas; ARect: TRect;
      DrawFolder: Boolean; var CurPos: Integer);

    procedure ProcessScrollButtons;

  public
    constructor Create(ANavBar: TVpCustomNavBar);
    procedure Paint;
  end;

function GetLargeIconDisplayName(Canvas: TCanvas; Rect: TRect; const Name: string): string;

implementation

uses
  Math, Themes, imglist,
  VpConst, VpMisc;

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
  FButtonHeight := TVpNavBarOpener(FNavBar).GetRealButtonHeight;
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
const
  BUTTON_DISTANCE = 8;
  LARGE_ICON_TEXT_DISTANCE = 2;
  SMALL_ICON_TEXT_DISTANCE = 6;
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
  J: Integer;
  text: String;
  h: Integer;
  R: TRect;
  dx, dy: Integer;
 {$IFDEF LCL}
  {$IF LCL_FullVersion >= 1090000}
  imgres: TScaledImageListResolution;
  f: Double;
  ppi: Integer;
  {$IFEND}
 {$ENDIF}
begin
  folder := FNavBar.Folders[FActiveFolder];

  // Distance between icon and text, for large icons vertically, for small icons
  // horizontally
  dy := ScaleY(LARGE_ICON_TEXT_DISTANCE, DesignTimeDPI);
  dx := ScaleX(SMALL_ICON_TEXT_DISTANCE, DesignTimeDPI);

  { If an image list is assigned then use the image size.
    If no image list is assinged then assume a 32 x 32 image size.
    The size of the small images is always half size of the large images. }
  if FImages <> nil then begin
   {$IFDEF LCL}{$IF LCL_FullVersion >= 1090000}
    with  TVpNavBarOpener(FNavBar) do begin
      f := FCanvasScaleFactor;
      ppi := Font.PixelsPerInch;
      imgRes := FImages.ResolutionForPPI[FImagesWidth, ppi, f];
    end;
    FLargeImagesSize := imgRes.Width;
   {$ELSE}
    FLargeImagesSize := FImages.Width;
   {$ENDIF}{$ENDIF}
  end else
    FLargeImagesSize := 32;
  FSmallImagesSize := FLargeImagesSize div 2;

  if folder.FolderType = ftDefault then begin
    if folder.ItemCount = 0 then
      exit;

    // Distance of top-most icon to the last upper button
    Inc(CurPos, ScaleY(BUTTON_DISTANCE, DesignTimeDPI));

    with nabItemsRect^ do begin
      Top := CurPos;
      Left := 0;
      Right := FNavBar.ClientWidth;
      Bottom := FNavBar.ClientHeight - (FNavBar.FolderCount - FActiveFolder - 1) * FButtonHeight;
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
      { If the caption is empty at design time display the item's name instead }
      if (csDesigning in FNavBar.ComponentState) and (item.Caption = '') then
        text := item.Name
      else
        text := item.Caption;

      if folder.IconSize = isLarge then begin
        { Large icons }
        if not DrawLargeIcon(Canvas, item, CurPos) then
          Continue;

        {make the icon's bottom blend into the label's top}
        R := item.IconRect;
        inc(R.Bottom, dy);
        item.IconRect := R;
        CurPos := item.IconRect.Bottom;

        {now draw the text}
        if not DrawItemText(Canvas, item, CurPos, text, true, h) then
          Continue;
        Inc(CurPos, FItemSpacing + h);
      end else
      begin
        { Small Icons }
        if not DrawSmallIcon(Canvas, item, CurPos) then
          Continue;

        {make the icon's right blend into the label's left}
        R := item.IconRect;
        inc(R.Right, dx);
        item.IconRect := R;

        {now draw the text}
        if not DrawItemText(Canvas, item, CurPos, text, false, h) then
          Continue;
        Inc(CurPos, FItemSpacing + h);
      end;  { if folder.IconSize ... }
    end;  { for J }
  end;  { if folder.FolderType = ftDefault ... }
end;

procedure TVpNavBarPainter.DrawBackground(Canvas: TCanvas; R: TRect);
var
  rowStart: Integer;
  lLeft, lHeight, lWidth: Integer;
begin
  if FBackgroundImage.Empty or (FBackgroundMethod = bmNone) or (FNavBar.FolderCount = 0) then
  begin
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.FillRect(R.Left, R.Top, R.Right, R.Bottom);
  end else
  begin
    case FBackgroundMethod of
      bmNormal:
        begin
          if (FBackgroundImage.Width < WidthOf(R)) or (FBackgroundImage.Height < HeightOf(R))
          then begin
            Canvas.Brush.Color := FBackgroundColor;
            Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          end;
          Canvas.Draw(R.Left, R.Top, FBackgroundImage);
        end;
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
//  SetBkMode(Canvas.Handle, bkMode);
// todo--->  SetBkColor(Canvas.Handle, bkColor);

  CurPos := FNavBar.ClientHeight - FButtonHeight;

  for I := FNavBar.FolderCount-1 downto FActiveFolder+1 do begin
    MyRect.Top := CurPos;
    MyRect.Bottom := CurPos + FButtonHeight;
    FNavBar.Folders[I].Rect := MyRect;

    {Draw the bottom tabs based on the selected style...}
    DrawTab(Canvas, MyRect, I);
    Dec(CurPos, FButtonHeight);
  end;
end;


{ Draw a "cool" tab button.
  Returns the usable text area inside the tab rect.}
function TVpNavBarPainter.DrawCoolTab(Canvas: TCanvas; R: TRect;
  ATabIndex: Integer; ATabColor: TColor): TRect;
var
  Points: array[1..5] of TPoint;
begin
  Result := R;

  with Canvas do begin
    {Fill the tab area}
    Brush.Style := bsSolid;
    if (ATabIndex = 0) then
      Brush.Color := clBtnFace
    else
      Brush.Color := ATabColor;
    FillRect(R);

    if IsMouseOverFolder(ATabIndex) then
      ;  // do what?

    {Draw the bottom, left line}
    Pen.Color := clBlack;
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left + 5, R.Bottom - 1);

    {Draw the bottom, left curve}
    Points[1] := Point(R.Left + 5,  R.Bottom - 1);  {Start point}
    Points[2] := Point(R.Left + 11, R.Bottom - 2);  {Control point}
    Points[3] := Point(R.Left + 12, R.Bottom - 7);  {Control point}
    Points[4] := Point(R.Left + 13, R.Bottom - 9);  {End point}
    {$IFNDEF VERSION4}
    {$IFDEF CBuilder}
    PolyBezier(Points);
    {$ELSE}
    Polyline(Points);
    {$ENDIF}
   {$ELSE}
    PolyBezier([Points[1], Points[2], Points[3], Points[4]]);
   {$ENDIF}

    {Draw the left side of the tab}
    MoveTo(R.Left + 13, R.Bottom - 9);
    LineTo(R.Left + 13, R.Top + 8);

    {Draw the top, left corner of the tab}
    Points[1] := Point(R.Left + 13, R.Top + 8);  {Start point}
    Points[2] := Point(R.Left + 14, R.Top + 6);  {Control point}
    Points[3] := Point(R.Left + 15, R.Top + 1);  {Control point}
    Points[4] := Point(R.Left + 21, R.Top + 0);  {End point}
   {$IFNDEF VERSION4}
    {$IFDEF CBuilder}
    PolyBezier(Points);
    {$ELSE}
    Polyline(Points);
    {$ENDIF}
  {$ELSE}
    PolyBezier([Points[1], Points[2], Points[3], Points[4]]);
  {$ENDIF}

    {Draw the top of the tab}
    MoveTo(R.Left + 21,  R.Top);
    LineTo(R.Right - 16, R.Top);

    {Draw the top right corner of the tab}
    Points[1] := Point(R.Right - 16, R.Top);
    Points[2] := Point(R.Right - 10, R.Top + 1);
    Points[3] := Point(R.Right -  9, R.Top + 6);
    Points[4] := Point(R.Right -  8, R.Top + 8);
    {$IFNDEF VERSION4}
    {$IFDEF CBuilder}
    PolyBezier(Points);
    {$ELSE}
    Polyline(Points);
    {$ENDIF}
    {$ELSE}
    PolyBezier([Points[1], Points[2], Points[3], Points[4]]);
    {$ENDIF}

    {Draw the right side of the tab}
    MoveTo(R.Right - 8, R.Top + 8);
    LineTo(R.Right - 8, R.Bottom - 9);

    {Draw the bottom, Right curve of the tab which should finish against the
     right side.}
    Points[1] := Point(R.Right - 8, R.Bottom - 9);
    Points[2] := Point(R.Right - 7, R.Bottom - 7);
    Points[3] := Point(R.Right - 6, R.Bottom - 2);
    Points[4] := Point(R.Right,     R.Bottom - 1);
    {$IFNDEF VERSION4}
    {$IFDEF CBuilder}
    Canvas.PolyBezier(Points);
    {$ELSE}
    Canvas.Polyline(Points);
    {$ENDIF}
   {$ELSE}
    PolyBezier([Points[1], Points[2], Points[3], Points[4]]);
   {$ENDIF}

    if ATabIndex = 0 then begin
      Brush.Color := ATabColor;
      FloodFill((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2, clBtnFace, fsSurface);
    end;
  end;

  Result := Rect(R.Left + 1, R.Top + 2, R.Right - 2, R.Bottom);
end;

{ Draw regular buttons
  Returns the usable text area inside the tab rect.}
function TVpNavBarPainter.DrawDefButton(Canvas: TCanvas; R: TRect;
  ATabIndex: Integer): TRect;
var
  tb: TThemedButton;
  details: TThemedElementDetails;
begin
  Result := R;

  if ThemeServices.ThemesEnabled then begin
    // themed button
    if IsMouseOverFolder(ATabIndex) and nabMouseDown then
      tb := tbPushButtonPressed
    else
    if IsMouseOverFolder(ATabIndex) then
      tb := tbPushButtonHot
    else
      tb := tbPushButtonNormal;
    details := ThemeServices.GetElementDetails(tb);
    InflateRect(R, 1, 1);
    ThemeServices.DrawElement(Canvas.Handle, details, R);
  end else
  begin
    // non-themed button
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(R);
    if nabMouseDown and IsMouseOverFolder(ATabIndex) then
    begin
      if R.Top = 0 then R.Top := 1;

      Canvas.Pen.Color := clBtnHighlight;   // bright at bottom/right
      Canvas.MoveTo(R.Left, R.Bottom-1);
      Canvas.LineTo(R.Right-1, R.Bottom-1);
      Canvas.LineTo(R.Right-1, R.Top-1);

      Canvas.Pen.Color := clGray;           // dark at top/left
      Canvas.MoveTo(R.Left, R.Bottom-2);
      Canvas.LineTo(R.Left, R.Top);
      Canvas.LineTo(R.Right-1, R.Top);

      Canvas.Pen.Color := clBtnShadow;      // shadow at top/left
      Canvas.MoveTo(R.Left+1, R.Bottom-2);
      Canvas.LineTo(R.Left+1, R.Top);
      Canvas.LineTo(R.Right-2, R.Top);
    end else
    begin
      Canvas.Pen.Color := clGray;          // bottom/right
      Canvas.MoveTo(R.Left, R.Bottom-1);
      Canvas.LineTo(R.Right-1, R.Bottom-1);
      Canvas.LineTo(R.Right-1, R.Top-1);

      Canvas.Pen.Color := clBtnHighlight;  // top/left
      Canvas.MoveTo(R.Left, R.Bottom-2);
      Canvas.LineTo(R.Left, R.Top);
      Canvas.LineTo(R.Right-1, R.Top);

      Canvas.Pen.Color := clBtnShadow;     // bottom/right shadow
      Canvas.MoveTo(R.Left+1, R.Bottom-2);
      Canvas.LineTo(R.Right-2, R.Bottom-2);
      Canvas.LineTo(R.Right-2, R.Top);
    end;
  end;
end;

{ Draw regular etched (Win98 style) buttons
  Returns the usable text area inside the tab rect.}
function TVpNavBarPainter.DrawEtchedButton(Canvas: TCanvas; R: TRect;
  ATabIndex: Integer): TRect;
begin
  with Canvas do begin
    Brush.Color := clBtnFace;
    FillRect(R);

    Frame3D(R, 1, bvLowered);
    if not IsMouseOverFolder(aTabIndex) then
      Frame3D(R, 1, bvRaised);
    {
   // InflateRect(R, -1, -1);
    if IsMouseOverFolder(ATabIndex) then
      Frame3D(R, 1, bvLowered) else
      Frame3D(r, 1, bvRaised);
      }
  end;
  Result := R;
end;

procedure TVpNavBarPainter.DrawItemHighlight(Canvas: TCanvas; R: TRect;
  Enable: Boolean);
begin
  if Enable then begin
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
    (*
  end else begin
    Canvas.Pen.Color := FBackgroundColor;
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Rectangle(R.Left - 1, R.Top - 1, R.Right + 1, R.Bottom + 1);
    *)
  end;
end;

function TVpNavBarPainter.DrawItemText(Canvas: TCanvas; AItem: TVpNavBtnItem;
  CurPos: Integer; AText: String; AtLargeIcon: Boolean; out AHeight: Integer): Boolean;
const
  HOR_MARGIN = 5;
var
  R: TRect;
  s: String;
  txtWidth: Integer;
  bkMode: Integer;
  horDist: Integer;
begin
  Result := false;
  horDist := ScaleX(HOR_MARGIN, DesignTimeDPI);

  if AtLargeIcon then
  begin
    R.Top := CurPos;
    R.Bottom := CurPos + FButtonHeight div 2 - 7;   // what is -7 good for?
    R.Left := 0;
    R.Right := FNavBar.ClientWidth - 1;
    AItem.LabelRect := R;
    AItem.DisplayName := GetLargeIconDisplayName(Canvas, R, AText);
    txtWidth := Canvas.TextWidth(AItem.DisplayName);
    R.Left := Max(horDist, (FNavBar.ClientWidth - txtWidth) div 2);
    R.Right := Min(R.Left + txtWidth, FNavBar.ClientWidth - hordist);
    AItem.LabelRect := R;
    if R.Top > nabItemsRect^.Bottom then
      Exit;

    s := AItem.DisplayName;
    DrawText(Canvas.Handle, PChar(s), Length(s), R, DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_CALCRECT);
    txtWidth := WidthOf(R);
    R.Left := (FNavBar.ClientWidth - txtWidth) div 2;
    R.Right := R.Left + txtWidth + 1;
    AItem.LabelRect := R;

    bkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    AHeight:= DrawText(Canvas.Handle, PChar(s), Length(s), R, DT_CENTER or DT_VCENTER or DT_WORDBREAK);
    SetBkMode(Canvas.Handle, bkMode);
  end else
  begin
    R.Top := CurPos;
    R.Bottom := CurPos + Canvas.TextHeight('Tg');
//    R.Bottom := CurPos + FButtonHeight div 2 - 7;
    R.Left := AItem.IconRect.Right;
    R.Right := FNavBar.ClientWidth - 2*AItem.IconRect.Left; // - 2*horDist;
//    R.Right := R.Left + FNavBar.ClientWidth - R.Left - ScaleX(7, DesignTimeDPI);
    AItem.LabelRect := R;
    if R.Top > nabItemsRect^.Bottom then
      Exit;

    // Measure size of display string
    R := AItem.LabelRect;
    s := GetDisplayString(Canvas, AText, 1, WidthOf(R));
    AItem.DisplayName := s;
    DrawText(Canvas.Handle, PChar(s), Length(s), R, DT_LEFT or DT_CALCRECT);
    txtWidth := WidthOf(R);
    AHeight := HeightOf(R);
    R.Right := R.Left + txtWidth + 1;
    {$IFDEF MSWINDOWS}
    OffsetRect(R, 0, -1);   // Better centering of text
    {$ENDIF}
    AItem.LabelRect := R;

    bkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.TextOut(R.Left, (R.Top + R.Bottom - AHeight) div 2, s);
//    AHeight := DrawText(Canvas.Handle, PChar(s), Length(s), R, DT_LEFT or DT_VCENTER);
    SetBkMode(Canvas.Handle, bkMode);

    if AHeight < FSmallImagesSize then
      AHeight := FSmallImagesSize;
  end;
  Result := true;
end;

{ Draw a large icon: centered horizontally, text to be drawn underneath icon.
  CurPos is upper edge of the icon. }
function TVpNavBarPainter.DrawLargeIcon(Canvas: TCanvas; AItem: TVpNavBtnItem;
  CurPos: Integer): Boolean;
const
  MARGIN = 2;
var
  W, H: Integer;
  R: TRect;
  dist: Integer;
 {$IFDEF LCL}{$IF LCL_FullVersion >= 1090000}
  imgres: TScaledImageListResolution;
  f: Double;
  ppi: Integer;
 {$ENDIF}{$ENDIF}
begin
  Result := false;

  dist := ScaleX(MARGIN, DesignTimeDPI);
  W := FLargeImagesSize + 2*dist;
  H := FLargeImagesSize + 2*dist;

  R.Top := CurPos;
  R.Bottom := CurPos + H;
  R.Left := (FNavBar.ClientWidth - W) div 2;
  R.Right := R.Left + W;
  if R.Top > nabItemsRect^.Bottom then
    exit;

  AItem.IconRect := R;

  if FShowButtons then begin
    DrawItemHighlight(Canvas, R, FActiveItem = AItem.Index);
    if Assigned(FImages) and (AItem.IconIndex >= 0) and (AItem.IconIndex < FImages.Count) then
    begin
     {$IFDEF LCL}{$IF LCL_FullVersion >= 1090000}
      with TVpNavBarOpener(FNavBar) do begin
        f := FCanvasScalefactor;
        ppi := Font.PixelsPerInch;
      end;
      imgRes := FImages.ResolutionForPPI[FImages.Width, ppi, f];
      imgRes.Draw(Canvas, R.Left + dist, R.Top + dist, AItem.IconIndex);
     {$ELSE}
      FImages.Draw(Canvas, R.Left + dist, R.Top + dist, AItem.IconIndex);
     {$ENDIF}{$ENDIF}
    end;
  end;

  Result := true;
end;

{ Draw a small icon (16x16) }
function TVpNavBarPainter.DrawSmallIcon(Canvas: TCanvas; AItem: TVpNavBtnItem;
  CurPos: Integer): Boolean;
const
  DELTA = 8;
var
  lOffset: Integer;
  R: TRect;
  del: Integer;
 {$IFDEF LCL}{$IF LCL_FullVersion >= 1090000}
  imgres: TScaledImageListResolution;
  f: Double;
  ppi: Integer;
 {$ELSE}
  bmp: TBitmap;
 {$ENDIF}{$ENDIF}
begin
  Result := false;

  {glyph is at the left}
  R.Top := CurPos;
  del := ScaleY(DELTA, DesignTimeDPI);
  lOffset := abs(Canvas.Font.Height) div 2;
  if lOffset > del then
    R.Top := R.Top + lOffset - del;
  R.Bottom := R.Top + FSmallImagesSize;
  R.Left := del;
  R.Right := R.Left + FSmallImagesSize;
  AItem.IconRect := R;
  if R.Top > nabItemsRect^.Bottom then
    Exit;  // Returns false

  if FShowButtons then begin
    DrawItemHighlight(Canvas, R, FActiveItem = AItem.Index);
    if Assigned(FImages) then begin
     {$IFDEF LCL}{$IF LCL_FullVersion >= 1090000}
      with TVpNavBarOpener(FNavBar) do begin
        f := FCanvasScalefactor;
        ppi := Font.PixelsPerInch;
      end;
      imgRes := FImages.ResolutionForPPI[FImages.Width div 2, ppi, f];
      imgRes.Draw(Canvas, R.Left, R.Top, AItem.IconIndex);
     {$ELSE}
      bmp := TBitmap.Create;
      try
        FImages.GetBitmap(AItem.IconIndex, bmp);
        bmp.Transparent := true;
        Canvas.StretchDraw(AItem.IconRect, bmp);
      finally
        bmp.Free;
      end;
      {$ENDIF}{$ENDIF}
    end;
  end;

  Result := true;
end;

{ Draw a "standard" tab button.
  Returns the usable text area inside the tab rect.}
function TVpNavBarPainter.DrawStandardTab(Canvas: TCanvas; R: TRect;
  ATabIndex: Integer; ATabColor: TColor): TRect;
const
  _LEFT_DISTANCE = 10;
  _RIGHT_DISTANCE = 2;
  _RADIUS = 2;
var
  leftDist: Integer;
  rightDist: Integer;
  radius: Integer;
begin
  Result := R;

  leftDist := ScaleX(_LEFT_DISTANCE, DesignTimeDPI);
  rightDist := ScaleX(_RIGHT_DISTANCE, DesignTimeDPI);
  radius := ScaleX(_RADIUS, DesignTimeDPI);

  {fill the tab area}
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(R);

  {fill the tab area}
  if ATabIndex > 0 then begin
    Canvas.Brush.Color := ATabColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Color := ATabColor;
    Canvas.Polygon([
      Point(R.Left, R.Bottom),
      Point(R.Left, R.Top),
      Point(R.Right, R.Top),
      Point(R.Right, R.Bottom)
    ]);
  end;

  {Draw Tab}
  Canvas.Brush.Color := ATabColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := ATabColor;
  Canvas.Polygon([
    Point(R.Left + leftDist,            R.Bottom - 1),                 // 10  -1
    Point(R.Left + leftDist,            R.Top + radius + 1),           // 10   3
    Point(R.Left + leftDist + radius,   R.Top + 1),                    // 12   1
    Point(R.Right - rightDist - radius, R.Top + 1),                    // -4   1
    Point(R.Right - rightDist,          R.Top + radius + 1),           // -2   3
    Point(R.Right - rightDist,          R.Bottom - 1)                  // -2  -1
  ]);
  {
    Point(R.Left + 10, R.Bottom - 1),
    Point(R.Left + 10, R.Top + 3),
    Point(R.Left + 12, R.Top + 1),
    Point(R.Right - 4, R.Top + 1),
    Point(R.Right - 2, R.Top + 3),
    Point(R.Right - 2, R.Bottom - 1)
  ]); }

  {highlight tab}
  Canvas.Pen.Color := clBtnHighlight;
  {
  Canvas.PolyLine([
    Point(R.Left, R.Bottom - 2),
    Point(R.Left + 8, R.Bottom - 2),
    Point(R.Left + 9, R.Bottom - 3),
    Point(R.Left + 9, R.Top + 3),
    Point(R.Left + 11, R.Top + 1),
    Point(R.Right - 1, R.Top + 1)
  ]);}
  Canvas.PolyLine([
    Point(R.Left,                     R.Bottom - radius),             //  0   -2
    Point(R.Left + leftDist - radius, R.Bottom - radius),             //  8   -2
    Point(R.Left + leftdist - 1,      R.Bottom - radius - 1),         //  9   -3
    Point(R.Left + leftDist - 1,      R.Top + radius + 1),            //  9    3
    Point(R.Left + leftDist + 1,      R.Top + 1),                     // 11    1
    Point(R.Right - 1,                R.Top + 1)                      // -1    1
  ]);


  {draw border}
  Canvas.Pen.Color := clBlack;
  {
  Canvas.PolyLine([
    Point(R.Left,      R.Bottom - 1),
    Point(R.Left + 9,  R.Bottom - 1),
    Point(R.Left + 10, R.Bottom - 2),
    Point(R.Left + 10, R.Top + 4),
    Point(R.Left + 11, R.Top + 3),
    Point(R.Left + 12, R.Top + 2),
    Point(R.Right - 2, R.Top + 2),
    Point(R.Right - 1, R.Top + 3),
    Point(R.Right - 1, R.Bottom - 1)
  ]);}
  Canvas.PolyLine([
    Point(R.Left,                R.Bottom - 1),                     //  0    -1
    Point(R.Left + leftDist - 1, R.Bottom - 1),                     //  9    -1
    Point(R.Left + leftDist,     R.Bottom - radius),                // 10    -2
    Point(R.Left + leftdist,     R.Top + radius + 2),               // 10    +4
    Point(R.Left + leftdist + 1, R.Top + radius + 1),               // 11    +3
    Point(R.Left + leftdist + 2, R.Top + radius),                   // 12    +2
    Point(R.Right - radius,      R.Top + radius),                   // -2    +2
    Point(R.Right - radius + 1,  R.Top + radius + 1),               // -1    +3
    Point(R.Right -1,            R.Bottom - 1)                      // -1    -1
  ]);

  Result := Rect(R.Left + 1, R.Top + 2, R.Right - 2, R.Bottom);
end;

procedure TVpNavBarPainter.DrawTab(Canvas: TCanvas; R: TRect; ATabIndex: Integer);
var
  displayTxt: String;
  TR: TRect;
  Flags: Integer;
  folder: TVpNavFolder;
  savedFontstyle: TFontStyles;
begin
  case FDrawingStyle of
    dsDefButton:
      TR := DrawDefButton(Canvas, R, ATabIndex);
    dsEtchedButton:
      TR := DrawEtchedButton(Canvas,  R,ATabIndex);
    dsCoolTab:
      TR := DrawCoolTab(Canvas, R, ATabIndex, FBackgroundColor);
    dsStandardTab:
      TR := DrawStandardTab(Canvas, R, ATabIndex, FBackgroundColor);
  end;

//  if IsMouseOverFolder(ATabIndex) then
//    OffsetRect(TR, -1, -1);

  //inc(TR.Top);

  folder := FNavBar.Folders[ATabIndex];
  displayTxt := folder.DisplayName;

  savedFontstyle := Canvas.Font.Style;
  if folder.Enabled then begin
    SetBkMode(Canvas.Handle, TRANSPARENT);
    if IsMouseOverFolder(ATabIndex) then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
    DrawText(Canvas.Handle, PChar(displayTxt), Length(displayTxt), TR, Flags);

    if IsMouseOverFolder(ATabIndex) and not nabMouseDown then begin
      case FDrawingStyle of
        dsDefButton:
          begin { Regular button style. }
//            InflateRect(TR, 1, 1);
        //    inc(TR.Left);
 //           Canvas.Frame3D(TR, 1, bvRaised);
          end;

        dsEtchedButton:
          begin { Etched style (Outlook98). }
//            InflateRect(TR, 1, 1);
      //      inc(TR.Top);
        //    inc(TR.Left);
  //          Canvas.Frame3D(TR, 1, bvRaised);
            {

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
              }
          end;
      end;  // case
    end;

  end
  else
  begin
    {use shadow text for inactive folder text}
    Canvas.Font.Color := clHighlightText;
    SetBkMode(Canvas.Handle, OPAQUE);
    DrawText(Canvas.Handle, PChar(displayTxt), Length(displayTxt), TR, Flags);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.Font.Color := clBtnShadow;
    OffsetRect(TR, -2, -1);
    DrawText(Canvas.Handle, PChar(displayTxt), Length(displayTxt), TR, Flags);
    Canvas.Font.Color := FNavBar.Font.Color;
  end;
  Canvas.Font.Style := savedFontStyle;
end;

procedure TVpNavBarPainter.DrawTopFolderButtons(Canvas: TCanvas;
  ARect: TRect; DrawFolder: Boolean; var CurPos: Integer);
var
  I: Integer;
  MyRect: TRect;
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
      DrawTab(Canvas, MyRect, I);
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

function TVpNavBarPainter.IsFocused(ATabIndex: Integer): Boolean;
begin
  Result := ATabIndex = FHotFolder;
end;

function TVpNavBarPainter.IsMouseOverFolder(ATabIndex: Integer): Boolean;
begin
  Result := ATabIndex = FHotFolder;
end;

function TVpNavBarPainter.IsMouseOverItem(ATabIndex: Integer): Boolean;
begin
  Result := ATabIndex = nabLastMouseOverItem;
end;

procedure TVpNavBarPainter.Paint;
var
  DrawBmp: TBitmap;
  DrawFolder: Boolean;
  TR: TRect;
  CurPos: Integer = 0;
  MyRect: TRect;
begin
  MyRect := FNavBar.ClientRect;

  DrawBmp := TBitmap.Create;
  try
    DrawBmp.Width := FClientWidth;
    DrawBmp.Height := FClientHeight;
    DrawBmp.Transparent := false;

    DrawBmp.Canvas.Font := FNavBar.Font;
    DrawBmp.Canvas.Pen.Color := FBackgroundColor;
    DrawBmp.Canvas.Brush.Color := FBackgroundColor;

    DrawFolder := (FNavBar.FolderCount > 0);
    if DrawFolder then
      TR := FFolderArea
    else
      TR := FNavBar.ClientRect;

    DrawBackground(DrawBmp.Canvas, TR);

    { Draw background }
    if FNavBar.FolderCount = 0 then begin
      nabScrollUpBtn.Visible := False;
      nabScrollDownBtn.Visible := False;
      FNavBar.Canvas.CopyMode := cmSrcCopy;
      FNavBar.Canvas.CopyRect(MyRect, DrawBmp.Canvas, Rect(0, 0, DrawBmp.Width,DrawBmp.Height));
      Exit;
    end;

    { Draw the folder buttons at the top }
    DrawTopFolderButtons(DrawBmp.Canvas, MyRect, DrawFolder, CurPos);

    { Draw active folder items }
    DrawActiveFolderItems(DrawBmp.Canvas, CurPos);

    { Draw the folder buttons at the bottom }
    DrawBottomFolderButtons(DrawBmp.Canvas, MyRect, CurPos);

    { Copy the buffer bitmap to the control }
    FNavBar.Canvas.CopyMode := cmSrcCopy;
    FNavBar.Canvas.CopyRect(MyRect, DrawBmp.Canvas, Rect(0, 0, DrawBmp.Width, DrawBmp.Height));

    { Show/hide scroll buttons }
    ProcessScrollButtons;

  finally
    DrawBmp.Free;
  end;
end;

procedure TVpNavBarPainter.ProcessScrollButtons;
const
  DISTANCE = 5;
var
  dist: Integer;
  w, h: Integer;
begin
  if not (csDesigning in FNavBar.ComponentState) then begin
    dist := ScaleY(DISTANCE, DesignTimeDPI);

    {show the top scroll button}
    if TVpNavBarOpener(FNavBar).nabShowScrollUp() then begin
      w := nabScrollUpBtn.Width;
      nabScrollUpBtn.Top := FNavBar.Folders[FActiveFolder].Rect.Bottom + dist;
      nabScrollUpBtn.Left := FNavBar.ClientWidth - w - dist;
      nabScrollUpBtn.Visible := True;
    end else
      nabScrollUpBtn.Visible := False;

    {show the bottom scroll button}
    if TVpNavBarOpener(FnavBar).nabShowScrollDown() then begin
      w := nabScrollDownBtn.Width;
      h := nabScrollDownBtn.Height;
      if FActiveFolder = FNavBar.FolderCount-1 then
        {there are no folders beyond the active one}
        nabScrollDownBtn.Top := FNavBar.ClientHeight - h - dist
      else
        nabScrollDownBtn.Top := FNavBar.Folders[FActiveFolder+1].Rect.Top - h - dist;
      nabScrollDownBtn.Left := FNavBar.ClientWidth - w - dist;
      nabScrollDownBtn.Visible := True;
    end else
      nabScrollDownBtn.Visible := False;
  end;
end;

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

