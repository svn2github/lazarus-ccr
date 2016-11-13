unit spkt_Pane;

{$mode delphi}
{.$Define EnhancedRecordSupport}

(*******************************************************************************
*                                                                              *
*  Plik: spkt_Pane.pas                                                         *
*  Opis: Komponent tafli toolbara                                              *
*  Copyright: (c) 2009 by Spook.                                               *
*  License:   Modified LGPL (with linking exception, like Lazarus LCL)         *
'             See "license.txt" in this installation                           *
*                                                                              *
*******************************************************************************)

interface

uses Graphics, Controls, Classes, SysUtils, Math, Dialogs,
     SpkGraphTools, SpkGUITools, SpkMath,
     spkt_Appearance, spkt_Const, spkt_Dispatch, spkt_Exceptions,
     spkt_BaseItem, spkt_Items, spkt_Types;

type TSpkPaneState = (psIdle, psHover);

type TSpkMousePaneElementType = (peNone, pePaneArea, peItem);

     TSpkMousePaneElement = record
                            ElementType : TSpkMousePaneElementType;
                            ElementIndex : integer;
                            end;

     T2DIntRectArray = array of T2DIntRect;
     TSpkPaneItemsLayout = record
                           Rects : T2DIntRectArray;
                           Width : integer;
                           end;

type TSpkPane = class;

     TSpkPane = class(TSpkComponent)
     private
       FPaneState : TSpkPaneState;

       FMouseHoverElement : TSpkMousePaneElement;
       FMouseActiveElement : TSpkMousePaneElement;
     protected
       FCaption : string;
       FRect : T2DIntRect;
       FToolbarDispatch : TSpkBaseToolbarDispatch;
       FAppearance : TSpkToolbarAppearance;
       FImages : TImageList;
       FDisabledImages : TImageList;
       FLargeImages : TImageList;
       FDisabledLargeImages : TImageList;
       FVisible : boolean;
       FItems : TSpkItems;

     // *** Generowanie layoutu elementów ***
       function GenerateLayout: TSpkPaneItemsLayout;

     // *** Obs³uga designtime i DFM ***
       procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
       procedure DefineProperties(Filer : TFiler); override;
       procedure Loaded; override;

     // *** Gettery i settery ***
       procedure SetCaption(const Value: string);
       procedure SetVisible(const Value: boolean);
       procedure SetAppearance(const Value: TSpkToolbarAppearance);
       procedure SetImages(const Value: TImageList);
       procedure SetDisabledImages(const Value : TImageList);
       procedure SetLargeImages(const Value: TImageList);
       procedure SetDisabledLargeImages(const Value : TImageList);
       procedure SetRect(ARect : T2DIntRect);
       procedure SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);
     public
     // *** Konstruktor, destruktor ***
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;

     // *** Obs³uga gryzonia ***
       procedure MouseLeave;
       procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer);
       procedure MouseMove(Shift: TShiftState; X, Y: Integer);
       procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer);

     // *** Geometria i rysowanie ***
       function GetWidth : integer;
       procedure Draw(ABuffer : TBitmap; ClipRect : T2DIntRect);
       function FindItemAt(x, y: integer): integer;

     // *** Obs³uga elementów ***
       procedure FreeingItem(AItem : TSpkBaseItem);

       property ToolbarDispatch : TSpkBaseToolbarDispatch read FToolbarDispatch write SetToolbarDispatch;
       property Appearance : TSpkToolbarAppearance read FAppearance write SetAppearance;
       property Rect : T2DIntRect read FRect write SetRect;
       property Images : TImageList read FImages write SetImages;
       property DisabledImages : TImageList read FDisabledImages write SetDisabledImages;
       property LargeImages : TImageList read FLargeImages write SetLargeImages;
       property DisabledLargeImages : TImageList read FDisabledLargeImages write SetDisabledLargeImages;
       property Items : TSpkItems read FItems;
     published
       property Caption : string read FCaption write SetCaption;
       property Visible : boolean read FVisible write SetVisible;
     end;

type TSpkPanes = class(TSpkCollection)
     private
     protected
       FToolbarDispatch : TSpkBaseToolbarDispatch;
       FAppearance : TSpkToolbarAppearance;
       FImages : TImageList;
       FDisabledImages : TImageList;
       FLargeImages : TImageList;
       FDisabledLargeImages : TImageList;

     // *** Gettery i settery ***
       procedure SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);
       function GetItems(index: integer): TSpkPane; reintroduce;
       procedure SetAppearance(const Value: TSpkToolbarAppearance);
       procedure SetImages(const Value: TImageList);
       procedure SetDisabledImages(const Value : TImageList);
       procedure SetLargeImages(const Value: TImageList);
       procedure SetDisabledLargeImages(const Value : TImageList);
     public
     // *** Dodawanie i wstawianie elementów ***
       function Add : TSpkPane;
       function Insert(index : integer) : TSpkPane;

     // *** Reakcja na zmiany listy ***
       procedure Notify(Item: TComponent; Operation : TOperation); override;
       procedure Update; override;

       property Items[index : integer] : TSpkPane read GetItems; default;
       property ToolbarDispatch : TSpkBaseToolbarDispatch read FToolbarDispatch write SetToolbarDispatch;
       property Appearance : TSpkToolbarAppearance read FAppearance write SetAppearance;
       property Images : TImageList read FImages write SetImages;
       property DisabledImages : TImageList read FDisabledImages write SetDisabledImages;
       property LargeImages : TImageList read FLargeImages write SetLargeImages;
       property DisabledLargeImages : TImageList read FDisabledLargeImages write SetDisabledLargeImages;
     end;

implementation

{ TSpkPane }

procedure TSpkPane.SetRect(ARect: T2DIntRect);

var Pt : T2DIntPoint;
    i : integer;
    Layout : TSpkPaneItemsLayout;

begin
FRect:=ARect;

// Obliczamy layout
Layout:=GenerateLayout;

{$IFDEF EnhancedRecordSupport}
Pt:=T2DIntPoint.create(ARect.left + SpkLayoutSizes.PANE_BORDER_SIZE + SpkLayoutSizes.PANE_LEFT_PADDING, ARect.top + SpkLayoutSizes.PANE_BORDER_SIZE);
{$ELSE}
Pt.create(ARect.left + PaneBorderSize + PaneLeftPadding, ARect.top + PaneBorderSize);
{$ENDIF}

if length(Layout.Rects)>0 then
   begin
   for i := 0 to high(Layout.Rects) do
       FItems[i].Rect:=Layout.Rects[i] + Pt;
   end;
end;

procedure TSpkPane.SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);

begin
  FToolbarDispatch := Value;
  FItems.ToolbarDispatch:=FToolbarDispatch;
end;

constructor TSpkPane.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FPaneState:=psIdle;
  FMouseHoverElement.ElementType:=peNone;
  FMouseHoverElement.ElementIndex:=-1;
  FMouseActiveElement.ElementType:=peNone;
  FMouseActiveElement.ElementIndex:=-1;

  FCaption:='Pane';
  {$IFDEF EnhancedRecordSupport}
  FRect:=T2DIntRect.create(0,0,0,0);
  {$ELSE}
  FRect.create(0,0,0,0);
  {$ENDIF}
  FToolbarDispatch:=nil;
  FAppearance:=nil;
  FImages:=nil;
  FDisabledImages:=nil;
  FLargeImages:=nil;
  FDisabledLargeImages:=nil;

  FVisible:=true;

  FItems:=TSpkItems.Create(self);
  FItems.ToolbarDispatch:=FToolbarDispatch;
  FItems.Appearance:=FAppearance;
end;

procedure TSpkPane.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Items',FItems.ReadNames,FItems.WriteNames,true);
end;

destructor TSpkPane.Destroy;
begin
  FItems.Free;

  inherited Destroy;
end;

procedure TSpkPane.Draw(ABuffer: TBitmap; ClipRect: T2DIntRect);

var x: Integer;
    y: Integer;
    BgFromColor, BgToColor, CaptionColor, FontColor, BorderLightColor,
    BorderDarkColor : TColor;
    i: Integer;

begin
// W niektórych warunkach nie jesteœmy w stanie rysowaæ:
// * Brak dyspozytora
if FToolbarDispatch=nil then
   exit;
// * Brak appearance
if FAppearance=nil then
   exit;

if FPaneState = psIdle then
   begin
   // psIdle
   BgFromColor:=FAppearance.Pane.GradientFromColor;
   BgToColor:=FAppearance.Pane.GradientToColor;
   CaptionColor:=FAppearance.Pane.CaptionBgColor;
   FontColor:=FAppearance.Pane.CaptionFont.Color;
   BorderLightColor:=FAppearance.Pane.BorderLightColor;
   BorderDarkColor:=FAppearance.Pane.BorderDarkColor;
   end else
       begin
       // psHover
       BgFromColor:=TColorTools.Brighten(FAppearance.Pane.GradientFromColor,20);
       BgToColor:=TColorTools.Brighten(FAppearance.Pane.GradientToColor,20);
       CaptionColor:=TColorTools.Brighten(FAppearance.Pane.CaptionBgColor,20);
       FontColor:=TColorTools.Brighten(FAppearance.Pane.CaptionFont.Color,20);
       BorderLightColor:=TColorTools.Brighten(FAppearance.Pane.BorderLightColor,20);
       BorderDarkColor:=TColorTools.Brighten(FAppearance.Pane.BorderDarkColor,20);
       end;

// T³o
TGuiTools.DrawRoundRect(ABuffer.Canvas,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntRect.Create(FRect.left,
                                          FRect.top,
                                          FRect.right - PaneBorderHalfSize,
                                          FRect.Bottom - PaneBorderHalfSize),
                        {$ELSE}
                        Create2DIntRect(FRect.left,
                                          FRect.top,
                                          FRect.right - PaneBorderHalfSize,
                                          FRect.Bottom - PaneBorderHalfSize),
                        {$ENDIF}
                        PaneCornerRadius,
                        BgFromColor,
                        BgToColor,
                        FAppearance.Pane.GradientType,
                        ClipRect);

// T³o etykiety tafli
TGuiTools.DrawRoundRect(ABuffer.Canvas,
                        {$IFDEF EnhancedRecordSupport}
                        T2DIntRect.Create(FRect.Left,
                                          FRect.Bottom - PaneCaptionHeight - PaneBorderHalfSize,
                                          FRect.right - PaneBorderHalfSize,
                                          FRect.bottom - PaneBorderHalfSize),
                        {$ELSE}
                        Create2DIntRect(FRect.Left,
                                          FRect.Bottom - PaneCaptionHeight - PaneBorderHalfSize,
                                          FRect.Right - PaneBorderHalfSize,
                                          FRect.Bottom - PaneBorderHalfSize),
                        {$ENDIF}
                        PaneCornerRadius,
                        CaptionColor,
                        clNone,
                        bkSolid,
                        ClipRect,
                        false,
                        false,
                        true,
                        true);

// Etykieta tafli
ABuffer.Canvas.Font.assign(FAppearance.Pane.CaptionFont);
x:=FRect.left + (FRect.width - ABuffer.Canvas.TextWidth(FCaption)) div 2;
y:=FRect.Bottom - PaneBorderSize - PaneCaptionHeight + 1 +
   (PaneCaptionHeight - ABuffer.Canvas.TextHeight('Wy')) div 2;

TGUITools.DrawText(ABuffer.Canvas,
                   x,
                   y,
                   FCaption,
                   FontColor,
                   ClipRect);

// Ramki
TGUITools.DrawAARoundFrame(ABuffer,
                           {$IFDEF EnhancedRecordSupport}
                           T2DIntRect.create(FRect.left+1,
                                             FRect.top+1,
                                             FRect.Right,
                                             FRect.bottom),
                           {$ELSE}
                           Create2DIntRect(FRect.left+1,
                                             FRect.top+1,
                                             FRect.Right,
                                             FRect.bottom),
                           {$ENDIF}
                           PaneCornerRadius,
                           BorderLightColor,
                           ClipRect);
TGUITools.DrawAARoundFrame(ABuffer,
                           {$IFDEF EnhancedRecordSupport}
                           T2DIntRect.create(FRect.left,
                                             FRect.top,
                                             FRect.Right-1,
                                             FRect.bottom-1),
                           {$ELSE}
                           Create2DIntRect(FRect.left,
                                             FRect.top,
                                             FRect.Right-1,
                                             FRect.bottom-1),
                           {$ENDIF}
                           PaneCornerRadius,
                           BorderDarkColor,
                           ClipRect);

// Elementy
if FItems.Count>0 then
   for i := 0 to FItems.Count - 1 do
       begin
       if FItems[i].Visible then
          Fitems[i].Draw(ABuffer, ClipRect);
       end;
end;

function TSpkPane.FindItemAt(x, y : integer) : integer;

var i : integer;

begin
result:=-1;
i:=FItems.count-1;
while (i>=0) and (result=-1) do
      begin
      if FItems[i].Visible then
         begin
         {$IFDEF EnhancedRecordSupport}
         if FItems[i].Rect.Contains(T2DIntVector.create(x,y)) then
         {$ELSE}
         if FItems[i].Rect.Contains(x,y) then
         {$ENDIF}
            result:=i;
         end;
      dec(i);
      end;
end;

procedure TSpkPane.FreeingItem(AItem: TSpkBaseItem);
begin
FItems.RemoveReference(AItem);
end;

function TSpkPane.GenerateLayout: TSpkPaneItemsLayout;

type TLayoutRow = array of integer;
     TLayoutColumn = array of TLayoutRow;
     TLayout = array of TLayoutColumn;

var Layout : TLayout;
    CurrentColumn : integer;
    CurrentRow : integer;
    CurrentItem : integer;
    c, r, i: Integer;
    ItemTableBehaviour : TSpkItemTableBehaviour;
    ItemGroupBehaviour : TSpkItemGroupBehaviour;
    ItemSize : TSpkItemSize;
    ForceNewColumn : boolean;
    LastX : integer;
    MaxRowX : integer;
    ColumnX : integer;
    rows: Integer;
    ItemWidth: Integer;
    tmpRect : T2DIntRect;

begin
setlength(result.Rects,FItems.count);
result.Width:=0;

if FItems.count=0 then
   exit;

// Notatka: algorytm jest skonstruowany w ten sposób, ¿e trójka: CurrentColumn,
//          CurrentRow oraz CurrentItem wskazuje na element, którego jeszcze nie
//          ma (zaraz za ostatnio dodanym elementem).

setlength(Layout,1);
CurrentColumn:=0;

setlength(Layout[CurrentColumn],1);
CurrentRow:=0;

setlength(Layout[CurrentColumn][CurrentRow],0);
CurrentItem:=0;

ForceNewColumn:=false;

for i := 0 to FItems.count - 1 do
    begin
    ItemTableBehaviour := FItems[i].GetTableBehaviour;
    ItemSize := FItems[i].GetSize;

    // Rozpoczêcie nowej kolumny?
    if (i=0) or
       (ItemSize = isLarge) or
       (ItemTableBehaviour = tbBeginsColumn) or
       ((ItemTableBehaviour = tbBeginsRow) and (CurrentRow = 2)) or
       (ForceNewColumn) then
       begin
       // Jeœli ju¿ jesteœmy na pocz¹tku nowej kolumny, nie ma nic do roboty.
       if (CurrentRow<>0) or (CurrentItem<>0) then
          begin
          setlength(Layout, length(Layout)+1);
          CurrentColumn:=high(Layout);

          setlength(Layout[CurrentColumn], 1);
          CurrentRow:=0;

          setlength(Layout[CurrentColumn][CurrentRow],0);
          CurrentItem:=0;
          end;
       end else
    // Rozpoczêcie nowego wiersza?
    if (ItemTableBehaviour = tbBeginsRow) then
       begin
       // Jeœli ju¿ jesteœmy na pocz¹tku nowego wiersza, nie ma nic do roboty.
       if CurrentItem <> 0 then
          begin
          setlength(Layout[CurrentColumn], length(Layout[CurrentColumn])+1);
          inc(CurrentRow);
          CurrentItem:=0;
          end;
       end;

    ForceNewColumn:=ItemSize = isLarge;

    // Jeœli element jest widoczny, dodajemy go w aktualnej kolumnie i aktualnym
    // wierszu.
    if FItems[i].Visible then
       begin
       setlength(Layout[CurrentColumn][CurrentRow], length(Layout[CurrentColumn][CurrentRow])+1);
       Layout[CurrentColumn][CurrentRow][CurrentItem]:=i;

       inc(CurrentItem);
       end;
    end;

// W tym miejscu mamy gotowy layout. Teraz trzeba obliczyæ pozycje i rozmiary
// Rectów.

// Najpierw wype³niamy je pustymi danymi, które zape³ni¹ miejsce elementów
// niewidocznych.
{$IFDEF EnhancedRecordSupport}
for i := 0 to FItems.count - 1 do
    result.Rects[i]:=T2DIntRect.create(-1, -1, -1, -1);
{$ELSE}
for i := 0 to FItems.count - 1 do
    result.Rects[i].create(-1, -1, -1, -1);
{$ENDIF}

MaxRowX:=0;

// Teraz iterujemy po layoucie, ustalaj¹c recty.
if length(Layout)>0 then
   for c := 0 to high(Layout) do
       begin
       if c>0 then
          begin
          LastX:=MaxRowX + PaneColumnSpacer;
          MaxRowX:=LastX;
          end
       else
          begin
          LastX:=MaxRowX;
          end;

       ColumnX:=LastX;

       rows:=length(Layout[c]);
       if rows>0 then
          for r := 0 to rows - 1 do
              begin
              LastX:=ColumnX;

              if length(Layout[c][r])>0 then
                 for i := 0 to high(Layout[c][r]) do
                     begin
                     ItemGroupBehaviour:=FItems[Layout[c][r][i]].GetGroupBehaviour;
                     ItemSize:=FItems[Layout[c][r][i]].GetSize;
                     ItemWidth:=FItems[Layout[c][r][i]].GetWidth;

                     if ItemSize = isLarge then
                        begin
                        tmpRect.top:=PaneFullRowTopPadding;
                        tmpRect.bottom:=tmpRect.top + PaneFullRowHeight - 1;
                        tmpRect.left:=LastX;
                        tmpRect.right:=LastX + ItemWidth - 1;

                        LastX:=tmpRect.right + 1;
                        if LastX>MaxRowX then
                           MaxRowX:=LastX;
                        end
                     else
                        begin
                        if ItemGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
                           begin
                           tmpRect.Left:=LastX;
                           tmpRect.right:=tmpRect.Left + ItemWidth - 1;
                           end
                        else
                           begin
                           // Jeœli element nie jest pierwszy, musi zostaæ
                           // odsuniêty marginesem od poprzedniego
                           if i>0 then
                              tmpRect.Left:=LastX + PaneGroupSpacer else
                              tmpRect.Left:=LastX;
                           tmpRect.right:=tmpRect.Left + ItemWidth - 1;
                           end;

                        {$REGION 'Obliczanie tmpRect.top i bottom'}
                        case rows of
                             1 : begin
                                 tmpRect.top:=PaneOneRowTopPadding;
                                 tmpRect.bottom:=tmpRect.top + PaneRowHeight - 1;
                                 end;
                             2 : begin
                                 case r of
                                      0 : begin
                                          tmpRect.top:=PaneTwoRowsTopPadding;
                                          tmpRect.bottom:=tmpRect.top + PaneRowHeight - 1;
                                          end;
                                      1 : begin
                                          tmpRect.top:=PaneTwoRowsTopPadding + PaneRowHeight + PaneTwoRowsVSpacer;
                                          tmpRect.bottom:=tmpRect.top + PaneRowHeight - 1;
                                          end;
                                 end;
                                 end;
                             3 : begin
                                 case r of
                                      0 : begin
                                          tmpRect.top:=PaneThreeRowsTopPadding;
                                          tmpRect.bottom:=tmpRect.top + PaneRowHeight - 1;
                                          end;
                                      1 : begin
                                          tmpRect.top:=PaneThreeRowsTopPadding + PaneRowHeight + PaneThreeRowsVSpacer;
                                          tmpRect.bottom:=tmpRect.top + PaneRowHeight - 1;
                                          end;
                                      2 : begin
                                          tmpRect.top:=PaneThreeRowsTopPadding + 2 * PaneRowHeight + 2 * PaneThreeRowsVSpacer;
                                          tmpRect.bottom:=tmpRect.top + PaneRowHeight - 1;
                                          end;
                                 end;
                                 end;
                        end;
                        {$ENDREGION}

                        LastX:=tmpRect.right + 1;
                        if LastX>MaxRowX then
                           MaxRowX:=LastX;
                        end;

                     Result.Rects[Layout[c][r][i]]:=tmpRect;
                     end;
              end;
       end;
// W tym miejscu MaxRowX wskazuje na pierwszy piksel za najbardziej wysuniêtym
// w prawo elementem - ergo jest równy szerokoœci ca³ego layoutu.
Result.Width:=MaxRowX;
end;

procedure TSpkPane.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
inherited;

if FItems.Count>0 then
   for i := 0 to FItems.Count - 1 do
       Proc(FItems.Items[i]);
end;

function TSpkPane.GetWidth: integer;

var tmpBitmap : TBitmap;
    PaneCaptionWidth, PaneElementsWidth : integer;
    TextW : integer;
    ElementsW : integer;
    Layout : TSpkPaneItemsLayout;

begin
// Przygotowywanie...
result:=-1;
if FToolbarDispatch=nil then
   exit;
if FAppearance=nil then
   exit;

tmpBitmap:=FToolbarDispatch.GetTempBitmap;
if tmpBitmap=nil then
   exit;
tmpBitmap.Canvas.font.assign(FAppearance.Pane.CaptionFont);

// *** Minimalna szerokoœæ tafli (tekstu) ***
TextW:=tmpBitmap.Canvas.TextWidth(FCaption);
PaneCaptionWidth := 2*PaneBorderSize + 2*PaneCaptionHMargin + TextW;

// *** Szerokoœæ elementów tafli ***
Layout:=GenerateLayout;
ElementsW:=Layout.Width;
PaneElementsWidth:=PaneBorderSize + PaneLeftPadding + ElementsW + PaneRightPadding + PaneBorderSize;

// *** Ustawianie szerokoœci tafli ***
result:=max(PaneCaptionWidth, PaneElementsWidth);
end;

procedure TSpkPane.Loaded;
begin
  inherited;
  if FItems.ListState = lsNeedsProcessing then
     FItems.ProcessNames(self.Owner);
end;

procedure TSpkPane.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
if FMouseActiveElement.ElementType = peItem then
   begin
   if FMouseActiveElement.ElementIndex<>-1 then
      FItems[FMouseActiveElement.ElementIndex].MouseDown(Button, Shift, X, Y);
   end else
if FMouseActiveElement.ElementType = pePaneArea then
   begin
   FPaneState:=psHover;
   end else
if FMouseActiveElement.ElementType = peNone then
   begin
   if FMouseHoverElement.ElementType = peItem then
      begin
      if FMouseHoverElement.ElementIndex<>-1 then
         begin
         FMouseActiveElement.ElementType:=peItem;
         FMouseActiveElement.ElementIndex:=FMouseHoverElement.ElementIndex;

         FItems[FMouseHoverElement.ElementIndex].MouseDown(Button, Shift, X, Y);
         end
      else
         begin
         FMouseActiveElement.ElementType:=pePaneArea;
         FMouseActiveElement.ElementIndex:=-1;
         end;
      end else
   if FMouseHoverElement.ElementType = pePaneArea then
      begin
      FMouseActiveElement.ElementType:=pePaneArea;
      FMouseActiveElement.ElementIndex:=-1;

      // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia.
      end;
   end;
end;

procedure TSpkPane.MouseLeave;
begin
if FMouseActiveElement.ElementType = peNone then
   begin
   if FMouseHoverElement.ElementType = peItem then
      begin
      if FMouseHoverElement.ElementIndex<>-1 then
         FItems[FMouseHoverElement.ElementIndex].MouseLeave;
      end else
   if FMouseHoverElement.ElementType = pePaneArea then
      begin
      // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia.
      end;
   end;

FMouseHoverElement.ElementType:=peNone;
FMouseHoverElement.ElementIndex:=-1;

// Niezale¿nie od tego, który element by³ aktywny / pod mysz¹, trzeba
// wygasiæ HotTrack.
if FPaneState<>psIdle then
   begin
   FPaneState:=psIdle;
   if assigned(FToolbarDispatch) then
      FToolbarDispatch.NotifyVisualsChanged;
   end;
end;

procedure TSpkPane.MouseMove(Shift: TShiftState; X, Y: Integer);

var i : integer;
    NewMouseHoverElement : TSpkMousePaneElement;

begin
// MouseMove jest wywo³ywany tylko, gdy tafla jest aktywna, b¹dŸ gdy
// mysz rusza siê wewn¹trz jej obszaru. Wobec tego zawsze nale¿y
// w tej sytuacji zapaliæ HotTrack.

if FPaneState = psIdle then
   begin
   FPaneState:=psHover;
   if assigned(FToolbarDispatch) then
      FToolbarDispatch.NotifyVisualsChanged;
   end;

// Szukamy obiektu pod mysz¹
i:=FindItemAt(x, y);
if i<>-1 then
   begin
   NewMouseHoverElement.ElementType:=peItem;
   NewMouseHoverElement.ElementIndex:=i;
   end else
if (X>=FRect.left) and (Y>=FRect.top) and
   (X<=FRect.right) and (Y<=FRect.bottom) then
   begin
   NewMouseHoverElement.ElementType:=pePaneArea;
   NewMouseHoverElement.ElementIndex:=-1;
   end else
       begin
       NewMouseHoverElement.ElementType:=peNone;
       NewMouseHoverElement.ElementIndex:=-1;
       end;

if FMouseActiveElement.ElementType = peItem then
   begin
   if FMouseActiveElement.ElementIndex<>-1 then
      FItems[FMouseActiveElement.ElementIndex].MouseMove(Shift, X, Y);
   end else
if FMouseActiveElement.ElementType = pePaneArea then
   begin
   // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia
   end else
if FMouseActiveElement.ElementType = peNone then
   begin
   // Jeœli element pod mysz¹ siê zmienia, informujemy poprzedni element o
   // tym, ¿e mysz opuszcza jego obszar
   if (NewMouseHoverElement.ElementType<>FMouseHoverElement.ELementType) or
      (NewMouseHoverElement.ElementIndex<>FMouseHoverElement.ElementIndex) then
      begin
      if FMouseHoverElement.ElementType = peItem then
         begin
         if FMouseHoverElement.ElementIndex<>-1 then
            FItems[FMouseHoverElement.ElementIndex].MouseLeave;
         end else
      if FMouseHoverElement.ElementType = pePaneArea then
         begin
         // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia
         end;
      end;

   if NewMouseHoverElement.ElementType = peItem then
      begin
      if NewMouseHoverElement.ElementIndex<>-1 then
         FItems[NewMouseHoverElement.ElementIndex].MouseMove(Shift, X, Y);
      end else
   if NewMouseHoverElement.ElementType = pePaneArea then
      begin
      // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia
      end;
   end;

FMouseHoverElement:=NewMouseHoverElement;
end;

procedure TSpkPane.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);

var ClearActive : boolean;

begin
ClearActive:=not(ssLeft in Shift) and not(ssMiddle in Shift) and not(ssRight in Shift);

if FMouseActiveElement.ElementType = peItem then
   begin
   if FMouseActiveElement.ElementIndex<>-1 then
      FItems[FMouseActiveElement.ElementIndex].MouseUp(Button, Shift, X, Y);
   end else
if FMouseActiveElement.ElementType = pePaneArea then
   begin
   // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia.
   end;

if ClearActive and
   (FMouseActiveElement.ElementType<>FMouseHoverElement.ElementType) or
   (FMouseActiveElement.ElementIndex<>FMouseHoverElement.ElementIndex) then
   begin
   if FMouseActiveElement.ElementType = peItem then
      begin
      if FMouseActiveElement.ElementIndex<>-1 then
         FItems[FMouseActiveElement.ElementIndex].MouseLeave;
      end else
   if FMouseActiveElement.ElementType = pePaneArea then
      begin
      // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia.
      end;

   if FMouseHoverElement.ElementType = peItem then
      begin
      if FMouseActiveElement.ElementIndex<>-1 then
         FItems[FMouseActiveElement.ElementIndex].MouseMove(Shift, X, Y);
      end else
   if FMouseHoverElement.ElementType = pePaneArea then
      begin
      // Placeholder, jeœli zajdzie potrzeba obs³ugi tego zdarzenia.
      end else
   if FMouseHoverElement.ElementType = peNone then
      begin
      if FPaneState<>psIdle then
         begin
         FPaneState:=psIdle;
         if assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
         end;
      end;
   end;

if ClearActive then
   begin
   FMouseActiveElement.ElementType:=peNone;
   FMouseActiveElement.ElementIndex:=-1;
   end;
end;

procedure TSpkPane.SetAppearance(const Value: TSpkToolbarAppearance);
begin
  FAppearance := Value;
  FItems.Appearance := Value;
end;

procedure TSpkPane.SetCaption(const Value: string);
begin
  FCaption := Value;
  if assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkPane.SetDisabledImages(const Value: TImageList);
begin
  FDisabledImages := Value;
  FItems.DisabledImages:=FDisabledImages;
end;

procedure TSpkPane.SetDisabledLargeImages(const Value: TImageList);
begin
  FDisabledLargeImages := Value;
  FItems.DisabledLargeImages:=FDisabledLargeImages;
end;

procedure TSpkPane.SetImages(const Value: TImageList);
begin
  FImages := Value;
  FItems.Images:=FImages;
end;

procedure TSpkPane.SetLargeImages(const Value: TImageList);
begin
  FLargeImages := Value;
  FItems.LargeImages:=FLargeImages;
end;

procedure TSpkPane.SetVisible(const Value: boolean);
begin
  FVisible := Value;

  if assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyItemsChanged;
end;

{ TSpkPanes }

function TSpkPanes.Add: TSpkPane;
begin
  Result:=TSpkPane.Create(FRootComponent);
  Result.Parent:=FRootComponent;
  AddItem(Result);
end;

function TSpkPanes.GetItems(index: integer): TSpkPane;
begin
  result:=TSpkPane(inherited Items[index]);
end;

function TSpkPanes.Insert(index: integer): TSpkPane;

var Owner, Parent : TComponent;
  i: Integer;

begin
if (index<0) or (index>self.Count) then
   raise InternalException.create('TSpkPanes.Insert: Nieprawid³owy indeks!');

if FRootComponent<>nil then
   begin
   Owner:=FRootComponent.Owner;
   Parent:=FRootComponent;
   end
else
   begin
   Owner:=nil;
   Parent:=nil;
   end;

result:=TSpkPane.Create(Owner);
result.Parent:=Parent;

if FRootComponent<>nil then
   begin
   i:=0;
   while FRootComponent.Owner.FindComponent('SpkPane'+inttostr(i))<>nil do
         inc(i);

   result.Name:='SpkPane'+inttostr(i);
   end;
   
InsertItem(index,result);
end;

procedure TSpkPanes.Notify(Item: TComponent;
  Operation : TOperation);
begin
  inherited Notify(Item, Operation);

  case Operation of
       opInsert: begin
                 // Ustawienie dyspozytora na nil spowoduje, ¿e podczas
                 // przypisywania w³asnoœci nie bêd¹ wo³ane metody Notify*
                 TSpkPane(Item).ToolbarDispatch:=nil;

                 TSpkPane(Item).Appearance:=FAppearance;
                 TSpkPane(Item).Images:=FImages;
                 TSpkPane(Item).DisabledImages:=FDisabledImages;
                 TSpkPane(Item).LargeImages:=FLargeImages;
                 TSpkPane(Item).DisabledLargeImages:=FDisabledLargeImages;
                 TSpkPane(Item).ToolbarDispatch:=FToolbarDispatch;
                 end;
       opRemove: begin
                 if not(csDestroying in Item.ComponentState) then
                    begin
                    TSpkPane(Item).ToolbarDispatch:=nil;
                    TSpkPane(Item).Appearance:=nil;
                    TSpkPane(Item).Images:=nil;
                    TSpkPane(Item).DisabledImages:=nil;
                    TSpkPane(Item).LargeImages:=nil;
                    TSpkPane(Item).DisabledLargeImages:=nil;
                    end;
                 end;
  end;
end;

procedure TSpkPanes.SetImages(const Value: TImageList);
var
  I: Integer;
begin
  FImages := Value;
  if self.Count>0 then
     for I := 0 to self.count - 1 do
         Items[i].Images:=Value;
end;

procedure TSpkPanes.SetLargeImages(const Value: TImageList);
var
  I: Integer;
begin
  FLargeImages := Value;
  if self.Count>0 then
     for I := 0 to self.count - 1 do
         Items[i].LargeImages:=Value;
end;

procedure TSpkPanes.SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);

var i : integer;

begin
  FToolbarDispatch := Value;
  if self.Count>0 then
     for i := 0 to self.count - 1 do
         Items[i].ToolbarDispatch:=FToolbarDispatch;
end;

procedure TSpkPanes.SetAppearance(const Value: TSpkToolbarAppearance);

var i: Integer;

begin
  FAppearance := Value;
  if self.Count>0 then
     for i := 0 to self.count - 1 do
         Items[i].Appearance:=FAppearance;

  if FToolbarDispatch<>nil then
     FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkPanes.SetDisabledImages(const Value: TImageList);

var I: Integer;

begin
  FDisabledImages := Value;
  if self.Count>0 then
     for I := 0 to self.count - 1 do
         Items[i].DisabledImages:=Value;
end;

procedure TSpkPanes.SetDisabledLargeImages(const Value: TImageList);

var
  I: Integer;
begin
  FDisabledLargeImages := Value;
  if self.Count>0 then
     for I := 0 to self.count - 1 do
         Items[i].DisabledLargeImages:=Value;
end;

procedure TSpkPanes.Update;
begin
  inherited Update;

  if assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyItemsChanged;
end;


end.
