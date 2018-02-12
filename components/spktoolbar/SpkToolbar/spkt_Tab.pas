unit spkt_Tab;

{$mode delphi}
{.$Define EnhancedRecordSupport}

(*******************************************************************************
*                                                                              *
*  File:        spkt_Tab.pas                                                   *
*  Description: Toolbar component tab                                          *
*  Copyright:   (c) 2009 by Spook.                                             *
*  License:     Modified LGPL (with linking exception, like Lazarus LCL)       *
'               See "license.txt" in this installation                         *
*                                                                              *
*******************************************************************************)

interface

uses
  Graphics, Controls, Classes, SysUtils,
  SpkMath,
  spkt_Appearance, spkt_Const, spkt_Dispatch, spkt_Exceptions,
  spkt_Pane, spkt_Types;

type
  TSpkTab = class;

  TSpkMouseTabElementType = (etNone, etTabArea, etPane);

  TSpkMouseTabElement = record
    ElementType: TSpkMouseTabElementType;
    ElementIndex: integer;
  end;

  TSpkTabAppearanceDispatch = class(TSpkBaseAppearanceDispatch)
  private
    FTab: TSpkTab;
  public
    // *** Constructor ***
    constructor Create(ATab: TSpkTab);

    // *** Implementation of methods inherited from TSpkBaseTabDispatch ***
    procedure NotifyAppearanceChanged; override;
  end;

  TSpkTab = class(TSpkComponent)
  private
    FAppearanceDispatch: TSpkTabAppearanceDispatch;
    FAppearance: TSpkToolbarAppearance;
    FMouseHoverElement: TSpkMouseTabElement;
    FMouseActiveElement: TSpkMouseTabElement;
    FOnClick: TNotifyEvent;

  protected
    FToolbarDispatch: TSpkBaseToolbarDispatch;
    FCaption: string;
    FVisible: boolean;
    FOverrideAppearance: boolean;
    FCustomAppearance: TSpkToolbarAppearance;
    FPanes: TSpkPanes;
    FRect: T2DIntRect;
    FImages: TImageList;
    FDisabledImages: TImageList;
    FLargeImages: TImageList;
    FDisabledLargeImages: TImageList;
    FImagesWidth: Integer;
    FLargeImagesWidth: Integer;

    // *** Sets the appropriate appearance tiles ***
    procedure SetPaneAppearance; inline;

    // *** Sheet search ***
    function FindPaneAt(x, y: integer): integer;

    // *** Designtime and LFM support ***
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;

    // *** Getters and setters ***
    procedure SetCaption(const Value: string);
    procedure SetCustomAppearance(const Value: TSpkToolbarAppearance);
    procedure SetOverrideAppearance(const Value: boolean);
    procedure SetVisible(const Value: boolean);
    procedure SetAppearance(const Value: TSpkToolbarAppearance);
    procedure SetImages(const Value: TImageList);
    procedure SetDisabledImages(const Value: TImageList);
    procedure SetLargeImages(const Value: TImageList);
    procedure SetDisabledLargeImages(const Value: TImageList);
    procedure SetImagesWidth(const Value: Integer);
    procedure SetLargeImagesWidth(const Value: Integer);
    procedure SetRect(ARect: T2DIntRect);
    procedure SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);

  public
    // *** Constructor, destructor ***
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // *** Geometry, sheet service, drawing ***
    function AtLeastOnePaneVisible: boolean;
    procedure Draw(ABuffer: TBitmap; AClipRect: T2DIntRect);

    // *** Mouse support ***
    procedure MouseLeave;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    // *** Dispatcher event handling ***
    procedure NotifyAppearanceChanged;

    // *** Support for elements ***
    procedure FreeingPane(APane: TSpkPane);

    procedure ExecOnClick;

    property ToolbarDispatch: TSpkBaseToolbarDispatch read FToolbarDispatch write SetToolbarDispatch;
    property Appearance: TSpkToolbarAppearance read FAppearance write SetAppearance;
    property Panes: TSpkPanes read FPanes;
    property Rect: T2DIntRect read FRect write SetRect;
    property Images: TImageList read FImages write SetImages;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property LargeImages: TImageList read FLargeImages write SetLargeImages;
    property DisabledLargeImages: TImageList read FDisabledLargeImages write SetDisabledLargeImages;
    property ImagesWidth: Integer read FImagesWidth write SetImagesWidth;
    property LargeImagesWidth: Integer read FLargeImagesWidth write SetLargeImagesWidth;

  published
    property CustomAppearance: TSpkToolbarAppearance read FCustomAppearance write SetCustomAppearance;
    property Caption: string read FCaption write SetCaption;
    property OverrideAppearance: boolean read FOverrideAppearance write SetOverrideAppearance default false;
    property Visible: boolean read FVisible write SetVisible default true;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TSpkTabs = class(TSpkCollection)
  protected
    FToolbarDispatch: TSpkBaseToolbarDispatch;
    FAppearance: TSpkToolbarAppearance;
    FImages: TImageList;
    FDisabledImages: TImageList;
    FLargeImages: TImageList;
    FDisabledLargeImages: TImageList;
    FImagesWidth: Integer;
    FLargeImagesWidth: Integer;
    procedure SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);
    function GetItems(AIndex: integer): TSpkTab; reintroduce;
    procedure SetAppearance(const Value: TSpkToolbarAppearance);
    procedure SetImages(const Value: TImageList);
    procedure SetDisabledImages(const Value: TImageList);
    procedure SetLargeImages(const Value: TImageList);
    procedure SetDisabledLargeImages(const Value: TImageList);
    procedure SetImagesWidth(const Value: Integer);
    procedure SetLargeImagesWidth(const Value: Integer);
  public
    function Add: TSpkTab;
    function Insert(AIndex: integer): TSpkTab;
    procedure Notify(Item: TComponent; Operation: TOperation); override;
    procedure Update; override;

    property Items[index: integer]: TSpkTab read GetItems; default;
    property ToolbarDispatch: TSpkBaseToolbarDispatch read FToolbarDispatch write SetToolbarDispatch;
    property Appearance: TSpkToolbarAppearance read FAppearance write SetAppearance;
    property Images: TImageList read FImages write SetImages;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property LargeImages: TImageList read FLargeImages write SetLargeImages;
    property DisabledLargeImages: TImageList read FDisabledLargeImages write SetDisabledLargeImages;
    property ImagesWidth: Integer read FImagesWidth write SetImagesWidth;
    property LargeImagesWidth: Integer read FLargeImagesWidth write SetLargeImagesWidth;
  end;


implementation

{ TSpkTabDispatch }

constructor TSpkTabAppearanceDispatch.Create(ATab: TSpkTab);
begin
  inherited Create;
  FTab := ATab;
end;

procedure TSpkTabAppearanceDispatch.NotifyAppearanceChanged;
begin
  if Assigned(FTab) then
    FTab.NotifyAppearanceChanged;
end;


{ TSpkTab }

constructor TSpkTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppearanceDispatch := TSpkTabAppearanceDispatch.Create(self);
  FMouseHoverElement.ElementType := etNone;
  FMouseHoverElement.ElementIndex := -1;
  FMouseActiveElement.ElementType := etNone;
  FMouseActiveElement.ElementIndex := -1;
  FCaption := 'Tab';
  FVisible := true;
  FCustomAppearance := TSpkToolbarAppearance.Create(FAppearanceDispatch);
  FPanes := TSpkPanes.Create(self);
  FPanes.ToolbarDispatch := FToolbarDispatch;
  FPanes.ImagesWidth := FImagesWidth;
  FPanes.LargeImagesWidth := FLargeImagesWidth;
  {$IFDEF EnhancedRecordSupport}
  FRect := T2DIntRect.Create(0,0,0,0);
  {$ELSE}
  FRect.Create(0,0,0,0);
  {$ENDIF}
  SetPaneAppearance;
end;

destructor TSpkTab.Destroy;
begin
  FPanes.Free;
  FCustomAppearance.Free;
  FAppearanceDispatch.Free;
  inherited Destroy;
end;

function TSpkTab.AtLeastOnePaneVisible: boolean;
var
  i: integer;
  PaneVisible: boolean;
begin
  Result := (FPanes.Count > 0);
  if Result then
  begin
    PaneVisible := false;
    i := FPanes.Count - 1;
    while (i >= 0) and not PaneVisible do
    begin
      PaneVisible := FPanes[i].Visible;
      dec(i);
    end;
    Result := Result and PaneVisible;
  end;
end;

procedure TSpkTab.SetRect(ARect: T2DIntRect);
var
  x, i: integer;
  tw: integer;
  tmpRect: T2DIntRect;
begin
  FRect := ARect;
  if AtLeastOnePaneVisible then
  begin
    x := ARect.left;
    for i := 0 to FPanes.Count - 1 do
      if FPanes[i].Visible then
      begin
        tw := FPanes[i].GetWidth;
        tmpRect.Left := x;
        tmpRect.Top := ARect.Top;
        tmpRect.Right := x + tw - 1;
        tmpRect.Bottom := ARect.bottom;
        FPanes[i].Rect := tmpRect;
        x := x + tw + TabPaneHSpacing;
      end
      else
      begin
        {$IFDEF EnhancedRecordSupport}
        FPanes[i].Rect := T2DIntRect.Create(-1,-1,-1,-1);
        {$ELSE}
        FPanes[i].Rect.Create(-1,-1,-1,-1);
        {$ENDIF}
      end;
  end;
end;

procedure TSpkTab.SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);
begin
  FToolbarDispatch := Value;
  FPanes.ToolbarDispatch := FToolbarDispatch;
end;

procedure TSpkTab.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Panes', FPanes.ReadNames, FPanes.WriteNames, true);
end;


procedure TSpkTab.Draw(ABuffer: TBitmap; AClipRect: T2DIntRect);
var
  LocalClipRect: T2DIntRect;
  i: integer;
begin
  if AtLeastOnePaneVisible then
    for i := 0 to FPanes.Count - 1 do
      if FPanes[i].visible then
      begin
        if AClipRect.IntersectsWith(FPanes[i].Rect, LocalClipRect) then
          FPanes[i].Draw(ABuffer, LocalClipRect);
      end;
end;

procedure TSpkTab.ExecOnClick;
begin
  if Assigned(FOnClick) then
    FOnClick(self);
end;

function TSpkTab.FindPaneAt(x, y: integer): integer;
var
  i: integer;
begin
  Result := -1;
  i := FPanes.Count - 1;
  while (i >= 0) and (Result = -1) do
  begin
    if FPanes[i].Visible then
    begin
      {$IFDEF EnhancedRecordSupport}
      if FPanes[i].Rect.Contains(T2DIntVector.Create(x,y)) then
      {$ELSE}
      if FPanes[i].Rect.Contains(x,y) then
      {$ENDIF}
        Result := i;
    end;
    dec(i);
  end;
end;

procedure TSpkTab.FreeingPane(APane: TSpkPane);
begin
  FPanes.RemoveReference(APane);
end;

procedure TSpkTab.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FPanes.Count - 1 do
    Proc(FPanes.Items[i]);
end;

procedure TSpkTab.Loaded;
begin
  inherited;
  if FPanes.ListState = lsNeedsProcessing then
    FPanes.ProcessNames(self.Owner);
end;

procedure TSpkTab.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FMouseActiveElement.ElementType = etPane then
  begin
    if FMouseActiveElement.ElementIndex <> -1 then
      FPanes[FMouseActiveElement.ElementIndex].MouseDown(Button, Shift, X, Y);
  end else
  if FMouseActiveElement.ElementType = etTabArea then
  begin
   // Placeholder, if there is a need to handle this event.
  end else
  if FMouseActiveElement.ElementType = etNone then
  begin
    if FMouseHoverElement.ElementType = etPane then
    begin
      if FMouseHoverElement.ElementIndex <> -1 then
      begin
        FMouseActiveElement.ElementType := etPane;
        FMouseActiveElement.ElementIndex := FMouseHoverElement.ElementIndex;
        FPanes[FMouseHoverElement.ElementIndex].MouseDown(Button, Shift, X, Y);
      end
      else
      begin
        FMouseActiveElement.ElementType := etTabArea;
        FMouseActiveElement.ElementIndex := -1;
      end;
    end else
    if FMouseHoverElement.ElementType = etTabArea then
    begin
      FMouseActiveElement.ElementType := etTabArea;
      FMouseActiveElement.ElementIndex := -1;
      // Placeholder, if there is a need to handle this event.
    end;
  end;
end;

procedure TSpkTab.MouseLeave;
begin
  if FMouseActiveElement.ElementType = etNone then
  begin
    if FMouseHoverElement.ElementType = etPane then
    begin
      if FMouseHoverElement.ElementIndex <> -1 then
        FPanes[FMouseHoverElement.ElementIndex].MouseLeave;
    end else
    if FMouseHoverElement.ElementType = etTabArea then
    begin
      // Placeholder, if there is a need to handle this event.
    end;
  end;

  FMouseHoverElement.ElementType := etNone;
  FMouseHoverElement.ElementIndex := -1;
end;

procedure TSpkTab.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  NewMouseHoverElement: TSpkMouseTabElement;
begin
  // We're looking for an object under the mouse
  i := FindPaneAt(X, Y);
  if i <> -1 then
  begin
    NewMouseHoverElement.ElementType := etPane;
    NewMouseHoverElement.ElementIndex := i;
  end else
  if (X >= FRect.left) and (Y >= FRect.top) and
     (X <= FRect.right) and (Y <= FRect.bottom) then
  begin
    NewMouseHoverElement.ElementType := etTabArea;
    NewMouseHoverElement.ElementIndex := -1;
  end else
  begin
    NewMouseHoverElement.ElementType := etNone;
    NewMouseHoverElement.ElementIndex := -1;
  end;

  if FMouseActiveElement.ElementType = etPane then
  begin
    if FMouseActiveElement.ElementIndex <> -1 then
    begin
      FPanes[FMouseActiveElement.ElementIndex].MouseMove(Shift, X, Y);
    end;
  end else
  if FMouseActiveElement.ElementType = etTabArea then
  begin
    // Placeholder, if there is a need to handle this event
  end else
  if FMouseActiveElement.ElementType = etNone then
  begin
    // If the item under the mouse changes, we inform the previous element
    // that the mouse leaves its area
    if (NewMouseHoverElement.ElementType <> FMouseHoverElement.ElementType) or
      (NewMouseHoverElement.ElementIndex <> FMouseHoverElement.ElementIndex) then
    begin
      if FMouseHoverElement.ElementType = etPane then
      begin
        if FMouseHoverElement.ElementIndex <> -1 then
          FPanes[FMouseHoverElement.ElementIndex].MouseLeave;
      end else
      if FMouseHoverElement.ElementType = etTabArea then
      begin
        // Placeholder, if there is a need to handle this event
      end;
    end;

    if NewMouseHoverElement.ElementType = etPane then
    begin
      if NewMouseHoverElement.ElementIndex <> -1 then
        FPanes[NewMouseHoverElement.ElementIndex].MouseMove(Shift, X, Y);
    end else
    if NewMouseHoverElement.ElementType = etTabArea then
    begin
      // Placeholder, if there is a need to handle this event
    end;
  end;

  FMouseHoverElement := NewMouseHoverElement;
end;

procedure TSpkTab.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ClearActive: boolean;
begin
  ClearActive := not (ssLeft in Shift) and not (ssMiddle in Shift) and not (ssRight in Shift);

  if FMouseActiveElement.ElementType = etPane then
  begin
    if FMouseActiveElement.ElementIndex <> -1 then
      FPanes[FMouseActiveElement.ElementIndex].MouseUp(Button, Shift, X, Y);
  end else
  if FMouseActiveElement.ElementType = etTabArea then
  begin
    // Placeholder, if there is a need to handle this event.
  end;
   
  if ClearActive and
    (FMouseActiveElement.ElementType <> FMouseHoverElement.ElementType) or
    (FMouseActiveElement.ElementIndex <> FMouseHoverElement.ElementIndex) then
  begin
    if FMouseActiveElement.ElementType = etPane then
    begin
      if FMouseActiveElement.ElementIndex <> -1 then
        FPanes[FMouseActiveElement.ElementIndex].MouseLeave;
    end else
    if FMouseActiveElement.ElementType = etTabArea then
    begin
      // Placeholder, if there is a need to handle this event.
    end;

    if FMouseHoverElement.ElementType = etPane then
    begin
      if FMouseHoverElement.ElementIndex <> -1 then
        FPanes[FMouseHoverElement.ElementIndex].MouseMove(Shift, X, Y);
    end else
    if FMouseHoverElement.ElementType = etTabArea then
    begin
      // Placeholder, if there is a need to handle this event.
    end;
  end;

  if ClearActive then
  begin
    FMouseActiveElement.ElementType := etNone;
    FMouseActiveElement.ElementIndex := -1;
  end;
end;

procedure TSpkTab.NotifyAppearanceChanged;
begin
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyAppearanceChanged;
end;

procedure TSpkTab.SetCustomAppearance(const Value: TSpkToolbarAppearance);
begin
  FCustomAppearance.Assign(Value);
end;

procedure TSpkTab.SetDisabledImages(const Value: TImageList);
begin
  FDisabledImages := Value;
  FPanes.DisabledImages := Value;
end;

procedure TSpkTab.SetDisabledLargeImages(const Value: TImageList);
begin
  FDisabledLargeImages := Value;
  FPanes.DisabledLargeImages := Value;
end;

procedure TSpkTab.SetImages(const Value: TImageList);
begin
  FImages := Value;
  FPanes.Images := Value;
end;

procedure TSpkTab.SetImagesWidth(const Value: Integer);
begin
  FImagesWidth := Value;
  FPanes.ImagesWidth := Value;
end;

procedure TSpkTab.SetLargeImages(const Value: TImageList);
begin
  FLargeImages := Value;
  FPanes.LargeImages := Value;
end;

procedure TSpkTab.SetLargeImagesWidth(const Value: Integer);
begin
  FLargeImagesWidth := Value;
  FPanes.LargeImagesWidth := Value;
end;

procedure TSpkTab.SetAppearance(const Value: TSpkToolbarAppearance);
begin
  FAppearance := Value;
  SetPaneAppearance;
  if FToolbarDispatch <> nil then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkTab.SetCaption(const Value: string);
begin
  FCaption := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkTab.SetOverrideAppearance(const Value: boolean);
begin
  FOverrideAppearance := Value;
  SetPaneAppearance;
  if FToolbarDispatch <> nil then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkTab.SetPaneAppearance;
begin
  if FOverrideAppearance then
    FPanes.Appearance := FCustomAppearance
  else
    FPanes.Appearance := FAppearance;
  // The method plays the role of a macro - therefore it does not
  // notify the dispatcher about the change.
end;

procedure TSpkTab.SetVisible(const Value: boolean);
begin
  FVisible := Value;
  if FToolbarDispatch <> nil then
    FToolbarDispatch.NotifyItemsChanged;
end;


{ TSpkTabs }

function TSpkTabs.Add: TSpkTab;
begin
  Result := TSpkTab.create(FRootComponent);
  Result.Parent := FRootComponent;
  AddItem(Result);
end;

function TSpkTabs.GetItems(AIndex: integer): TSpkTab;
begin
  Result := TSpkTab(inherited Items[AIndex]);
end;

function TSpkTabs.Insert(AIndex: integer): TSpkTab;
var
  lOwner, lParent: TComponent;
  i: Integer;
begin
  if (AIndex < 0) or (AIndex >= self.Count) then
    raise InternalException.Create('TSpkTabs.Insert: Invalid index!');

  if FRootComponent<>nil then
  begin
    lOwner := FRootComponent.Owner;
    lParent := FRootComponent;
  end
  else
  begin
    lOwner := nil;
    lParent := nil;
  end;

  Result := TSpkTab.Create(lOwner);
  Result.Parent := lParent;

  if FRootComponent<>nil then
  begin
    i := 0;
    while FRootComponent.Owner.FindComponent('SpkTab'+IntToStr(i)) <> nil do
      inc(i);

    Result.Name := 'SpkTab' + IntToStr(i);
  end;
  InsertItem(AIndex, Result);
end;

procedure TSpkTabs.Notify(Item: TComponent; Operation: TOperation);
begin
  inherited Notify(Item, Operation);

  case Operation of
    opInsert:
      begin
        // Setting the dispatcher to nil will cause that during the
        // ownership assignment, the Notify method will not be called
        TSpkTab(Item).ToolbarDispatch := nil;
        TSpkTab(Item).Appearance := self.FAppearance;
        TSpkTab(Item).Images := self.FImages;
        TSpkTab(Item).DisabledImages := self.FDisabledImages;
        TSpkTab(Item).LargeImages := self.FLargeImages;
        TSpkTab(Item).DisabledLargeImages := self.FDisabledLargeImages;
        TSpkTab(Item).ImagesWidth := self.FImagesWidth;
        TSpkTab(Item).LargeImagesWidth := self.FLargeImagesWidth;
        TSpkTab(Item).ToolbarDispatch := self.FToolbarDispatch;
      end;
    opRemove:
      if not(csDestroying in Item.ComponentState) then
      begin
        TSpkTab(Item).ToolbarDispatch := nil;
        TSpkTab(Item).Appearance := nil;
        TSpkTab(Item).Images := nil;
        TSpkTab(Item).DisabledImages := nil;
        TSpkTab(Item).LargeImages := nil;
        TSpkTab(Item).DisabledLargeImages := nil;
  //      TSpkTab(Item).ImagesWidth := 0;
//        TSpkTab(Item).LargeImagesWidth := 0;
      end;
  end;
end;

procedure TSpkTabs.SetAppearance(const Value: TSpkToolbarAppearance);
var
  i: Integer;
begin
  FAppearance := Value;
  for i := 0 to self.Count - 1 do
    self.Items[i].Appearance := FAppearance;
end;

procedure TSpkTabs.SetDisabledImages(const Value: TImageList);
var
  i: Integer;
begin
  FDisabledImages := Value;
  for i := 0 to self.Count - 1 do
    Items[i].DisabledImages := Value;
end;

procedure TSpkTabs.SetDisabledLargeImages(const Value: TImageList);
var
  i: Integer;
begin
  FDisabledLargeImages := Value;
  for i := 0 to self.count - 1 do
    Items[i].DisabledLargeImages := Value;
end;

procedure TSpkTabs.SetImages(const Value: TImageList);
var
  i: Integer;
begin
  FImages := Value;
  for i := 0 to self.Count - 1 do
    Items[i].Images := Value;
end;

procedure TSpkTabs.SetImagesWidth(const Value: Integer);
var
  i: Integer;
begin
  FImagesWidth := Value;
  for i := 0 to Count - 1 do
    Items[i].ImagesWidth := Value;
end;

procedure TSpkTabs.SetLargeImages(const Value: TImageList);
var
  i: Integer;
begin
  FLargeImages := Value;
  for i := 0 to self.Count - 1 do
    Items[i].LargeImages := Value;
end;

procedure TSpkTabs.SetLargeImagesWidth(const Value: Integer);
var
  i: Integer;
begin
  FLargeImagesWidth := Value;
  for i := 0 to Count - 1 do
    Items[i].LargeImagesWidth := Value;
end;

procedure TSpkTabs.SetToolbarDispatch(const Value: TSpkBaseToolbarDispatch);
var
  i: integer;
begin
  FToolbarDispatch := Value;
  for i := 0 to self.Count - 1 do
    self.Items[i].ToolbarDispatch := FToolbarDispatch;
end;

procedure TSpkTabs.Update;
begin
  inherited Update;

  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyItemsChanged;
end;


end.
