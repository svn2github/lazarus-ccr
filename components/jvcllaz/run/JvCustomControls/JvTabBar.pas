{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTabBar.pas, released on 2004-12-23.

The Initial Developer of the Original Code is Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTabBar;

{$MODE objfpc}{$H+}
{.$DEFINE JVCLThemesEnabled}

interface

uses
  LCLIntf, LCLType, LMessages, LCLVersion, Types,
  Graphics, Controls, Forms, ImgList, Menus, Buttons,
  ExtCtrls, SysUtils, Classes, Contnrs, Themes;

type
  TJvCustomTabBar = class;
  TJvTabBarItem = class;

  TJvTabBarOrientation = (toTop, toBottom);
  TJvTabBarScrollButtonKind = (sbScrollLeft, sbScrollRight);
  TJvTabBarScrollButtonState = (sbsHidden, sbsNormal, sbsHot, sbsPressed, sbsDisabled);

  TJvGetModifiedEvent = procedure(Sender: TJvTabBarItem; var Modified: Boolean) of object;
  TJvGetEnabledEvent = procedure(Sender: TJvTabBarItem; var Enabled: Boolean) of object;

  IPageList = interface
    ['{6BB90183-CFB1-4431-9CFD-E9A032E0C94C}']
    function CanChange(AIndex: Integer): Boolean;
    procedure SetActivePageIndex(AIndex: Integer);
    function GetPageCount: Integer;
    function GetPageCaption(AIndex: Integer): string;
    procedure AddPage(const ACaption: string);
    procedure DeletePage(Index: Integer);
    procedure MovePage(CurIndex, NewIndex: Integer);
    procedure PageCaptionChanged(Index: Integer; const NewCaption: string);
  end;

  TJvTabBarItem = class(TCollectionItem)
  private
    FLeft: Integer; // used for calculating DisplayRect
    FImageIndex: TImageIndex;
    FEnabled: Boolean;
    FVisible: Boolean;
    FTag: Integer;
    FData: TObject;
    FHint: TCaption;
    FName: string;
    FCaption: TCaption;
    FImages: TCustomImageList;
    FModified: Boolean;
    FPopupMenu: TPopupMenu;
    FOnGetEnabled: TJvGetEnabledEvent;
    FOnGetModified: TJvGetModifiedEvent;
    FShowHint: Boolean;
    FAutoDeleteDatas: TObjectList;
    function GetEnabled: Boolean;
    function GetModified: Boolean;
    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetClosing: Boolean;
    procedure SetModified(const Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetSelected(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetName(const Value: string);
    procedure SetVisible(const Value: Boolean);
    function GetTabBar: TJvCustomTabBar;
    function GetSelected: Boolean;
    function GetDisplayRect: TRect;
    function GetHot: Boolean;
  protected
    procedure Changed; virtual;
    procedure SetIndex(Value: Integer); override;
    procedure Notification(Component: TComponent; Operation: TOperation); virtual;
    property Name: string read FName write SetName;
  public
    constructor Create(ACollection: Classes.TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetImages: TCustomImageList;
    function CanSelect: Boolean;
    function GetNextVisible: TJvTabBarItem;
    function GetPreviousVisible: TJvTabBarItem;
    procedure MakeVisible;
    function AutoDeleteData: TObjectList;
    property Data: TObject read FData write FData;
    property TabBar: TJvCustomTabBar read GetTabBar;
    property DisplayRect: TRect read GetDisplayRect;
    property Hot: Boolean read GetHot;
    property Closing: Boolean read GetClosing;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Selected: Boolean read GetSelected write SetSelected default False;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Modified: Boolean read GetModified write SetModified default False;
    property Hint: TCaption read FHint write FHint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowHint: Boolean read FShowHint write FShowHint default True;
    property OnGetModified: TJvGetModifiedEvent read FOnGetModified write FOnGetModified;
    property OnGetEnabled: TJvGetEnabledEvent read FOnGetEnabled write FOnGetEnabled;
  end;

  TJvTabBarItems = class(TOwnedCollection)
  private
    function GetTabBar: TJvCustomTabBar;
    function GetItem(Index: Integer): TJvTabBarItem;
    procedure SetItem(Index: Integer; const Value: TJvTabBarItem);
  protected
    function Find(const AName: string): TJvTabBarItem;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    function IndexOf(Item: TJvTabBarItem): Integer;
    procedure EndUpdate; override;
    property Items[Index: Integer]: TJvTabBarItem read GetItem write SetItem; default;
    property TabBar: TJvCustomTabBar read GetTabBar;
  end;

  TJvTabBarPainterOptionType = (poPaintsHotTab, poBottomScrollButtons);
  TJvTabBarPainterOptions = set of TJvTabBarPainterOptionType;

  TJvTabBarPainter = class(TComponent)
  private
    FOnChangeList: TList;
  protected
    procedure AutoSize; virtual;
    procedure Changed; virtual;
    procedure DrawBackground(Canvas: TCanvas; TabBar: TJvCustomTabBar; R: TRect); virtual; abstract;
    procedure DrawDivider(Canvas: TCanvas; LeftTab: TJvTabBarItem; R: TRect); virtual; abstract;
    procedure DrawMoveDivider(Canvas: TCanvas; Tab: TJvTabBarItem; MoveLeft: Boolean); virtual; abstract;
    procedure DrawScrollButton(Canvas: TCanvas; TabBar: TJvCustomTabBar; Button: TJvTabBarScrollButtonKind;
      State: TJvTabBarScrollButtonState; R: TRect); virtual;
    procedure DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect); virtual; abstract;
    function GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect): TRect; virtual; abstract;
    function GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer; virtual; abstract;
    function GetPixelsPerInch: Integer; virtual; abstract;
    function GetRealImageSize(ATab: TJvTabBarItem): TSize;
    procedure GetScrollButtons(TabBar: TJvCustomTabBar; var LeftButton, RightButton: TRect); {virtual; reserved for future use }
    function GetTabBar(ATab: TJvTabBarItem): TJvCustomTabBar;
    function GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize; virtual; abstract;
    function Options: TJvTabBarPainterOptions; virtual; abstract;
    function Scale96(AValue: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvModernTabBarPainter = class(TJvTabBarPainter)
  private
    FFont: TFont;
    FDisabledFont: TFont;
    FSelectedFont: TFont;
    FColor: TColor;
    FTabColor: TColor;
    FControlDivideColor: TColor;
    FBorderColor: TColor;
    FModifiedCrossColor: TColor;
    FCloseRectColor: TColor;
    FCloseRectColorDisabled: TColor;
    FCloseCrossColorDisabled: TColor;
    FCloseCrossColorSelected: TColor;
    FCloseCrossColor: TColor;
    FCloseColor: TColor;
    FCloseColorSelected: TColor;
    FDividerColor: TColor;
    FMoveDividerColor: TColor;
    FTabHeight: Integer;
    FTabWidth: Integer;
    procedure SetCloseRectColorDisabled(const Value: TColor);
    procedure SetCloseColor(const Value: TColor);
    procedure SetCloseColorSelected(const Value: TColor);
    procedure SetCloseCrossColor(const Value: TColor);
    procedure SetCloseCrossColorDisabled(const Value: TColor);
    procedure SetCloseRectColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetModifiedCrossColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetControlDivideColor(const Value: TColor);
    procedure SetTabColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure FontChanged(Sender: TObject);
    procedure SetDividerColor(const Value: TColor);
    procedure SetCloseCrossColorSelected(const Value: TColor);
    procedure SetTabHeight(Value: Integer);
    procedure SetTabWidth(Value: Integer);
  protected
    procedure AutoSize; override;
    procedure DrawBackground(Canvas: TCanvas; TabBar: TJvCustomTabBar; R: TRect); override;
    procedure DrawDivider(Canvas: TCanvas; LeftTab: TJvTabBarItem; R: TRect); override;
    procedure DrawMoveDivider(Canvas: TCanvas; Tab: TJvTabBarItem; MoveLeft: Boolean); override;
    procedure DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem; ATabRect: TRect); override;
    function GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem; ATabRect: TRect): TRect; override;
    function GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer; override;
    function GetPixelsPerInch: Integer; override;
    function GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize; override;
    function Options: TJvTabBarPainterOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TabColor: TColor read FTabColor write SetTabColor default clBtnFace;
    property Color: TColor read FColor write SetColor default clWindow;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property ControlDivideColor: TColor read FControlDivideColor write SetControlDivideColor default clBlack;
    property ModifiedCrossColor: TColor read FModifiedCrossColor write SetModifiedCrossColor default clRed;
    property CloseColorSelected: TColor read FCloseColorSelected write SetCloseColorSelected default $F4F4F4;
    property CloseColor: TColor read FCloseColor write SetCloseColor default clWhite;
    property CloseCrossColorSelected: TColor read FCloseCrossColorSelected write SetCloseCrossColorSelected default clBlack;
    property CloseCrossColor: TColor read FCloseCrossColor write SetCloseCrossColor default $5D5D5D;
    property CloseCrossColorDisabled: TColor read FCloseCrossColorDisabled write SetCloseCrossColorDisabled default $ADADAD;
    property CloseRectColor: TColor read FCloseRectColor write SetCloseRectColor default $868686;
    property CloseRectColorDisabled: TColor read FCloseRectColorDisabled write SetCloseRectColorDisabled default $D6D6D6;
    property DividerColor: TColor read FDividerColor write SetDividerColor default $99A8AC;
    property MoveDividerColor: TColor read FMoveDividerColor write FMoveDividerColor default clBlack;
    property TabHeight: Integer read FTabHeight write SetTabHeight default 0;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 0;
    property Font: TFont read FFont write SetFont;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
  end;
  TJvTabBarModernPainter = TJvModernTabBarPainter; // TJvModernTabBarPainter should have been named TJvTabBarModernPainter

  TJvTabBarItemEvent = procedure(Sender: TObject; Item: TJvTabBarItem) of object;
  TJvTabBarSelectingEvent = procedure(Sender: TObject; Item: TJvTabBarItem; var AllowSelect: Boolean) of object;
  TJvTabBarClosingEvent = procedure(Sender: TObject; Item: TJvTabBarItem; var AllowClose: Boolean) of object;
  TJvTabBarCloseQueryEvent = procedure(Sender: TObject; Item: TJvTabBarItem; var CanClose: Boolean) of object;
  TJvTabBarScrollButtonClickEvent = procedure(Sender: TObject; Button: TJvTabBarScrollButtonKind) of object;

  TJvTabBarScrollButtonInfo = record
    State: TJvTabBarScrollButtonState;
    Rect: TRect;
    ExState: Boolean;
  end;

  TJvCustomTabBar = class(TCustomControl)
  private
    FTabs: TJvTabBarItems;
    FPainter: TJvTabBarPainter;
    FDefaultPainter: TJvTabBarPainter;
    FChangeLink: TChangeLink;
    FCloseButton: Boolean;
    FRightClickSelect: Boolean;
    FImages: TCustomImageList;
    FImagesWidth: Integer;
    FHotTracking: Boolean;
    FHotTab: TJvTabBarItem;
    FSelectedTab: TJvTabBarItem;
    FClosingTab: TJvTabBarItem;
    FLastInsertTab: TJvTabBarItem;
    FMouseDownClosingTab: TJvTabBarItem;
    FMargin: Integer;
    FAutoFreeClosed: Boolean;
    FAllowUnselected: Boolean;
    FSelectBeforeClose: Boolean;
    FPageList: TCustomControl;

    FOnTabClosing: TJvTabBarClosingEvent;
    FOnTabSelected: TJvTabBarItemEvent;
    FOnTabSelecting: TJvTabBarSelectingEvent;
    FOnTabCloseQuery: TJvTabBarCloseQueryEvent;
    FOnTabClosed: TJvTabBarItemEvent;
    FOnTabMoved: TJvTabBarItemEvent;
    FOnChange: TNotifyEvent;

    // scrolling
    FLeftIndex: Integer;
    FLastTabRight: Integer;
    FRequiredWidth: Integer;
    FBarWidth: Integer;
    FBtnLeftScroll: TJvTabBarScrollButtonInfo;
    FBtnRightScroll: TJvTabBarScrollButtonInfo;
    FScrollButtonBackground: TBitmap;
    FHint: TCaption;
    FFlatScrollButtons: Boolean;
    FAllowTabMoving: Boolean;
    FOrientation: TJvTabBarOrientation;
    FOnScrollButtonClick: TJvTabBarScrollButtonClickEvent;
    FPageListTabLink: Boolean;

    FRepeatTimer: TTimer;
    FScrollRepeatedClicked: Boolean;
    FOnLeftTabChange: TNotifyEvent;

    function GetHeight: Integer;
    function GetLeftTab: TJvTabBarItem;
    procedure SetLeftTab(Value: TJvTabBarItem);
    procedure SetSelectedTab(Value: TJvTabBarItem);
    procedure SetTabs(Value: TJvTabBarItems);
    procedure SetPainter(Value: TJvTabBarPainter);
    procedure SetImages(Value: TCustomImageList);
    procedure SetCloseButton(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure SetHotTab(Tab: TJvTabBarItem);
    procedure SetClosingTab(Tab: TJvTabBarItem);
    procedure UpdateScrollButtons;
    function FindSelectableTab(Tab: TJvTabBarItem): TJvTabBarItem;
    procedure SetHint(const Value: TCaption);
    procedure SetFlatScrollButtons(const Value: Boolean);
    procedure SetPageList(const Value: TCustomControl);
    procedure SetOrientation(const Value: TJvTabBarOrientation);
    procedure TimerExpired(Sender: TObject);
    procedure SetHeight(AValue: Integer);
 {$IF LCL_FullVersion >= 1090000}
  private
    procedure SetImagesWidth(const AValue: Integer);
  protected
    property ImagesWidth: Integer read FImagesWidth write SetImagesWidth default 0;
{$ENDIF}
  protected
    procedure CalcTabsRects;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure DrawScrollBarGlyph(ACanvas: TCanvas; X, Y: Integer; ALeft, Disabled: Boolean);
    procedure Paint; override;
    procedure PaintScrollButtons;
    procedure PaintTab(ACanvas: TCanvas; Tab: TJvTabBarItem); virtual;
    procedure Resize; override;

    class function GetControlClassDefaultSize: TSize;
    function GetTabHeight(Tab: TJvTabBarItem): Integer;
    function GetTabWidth(Tab: TJvTabBarItem): Integer;

    function CurrentPainter: TJvTabBarPainter;
    procedure Notification(Component: TComponent; Operation: TOperation); override;

    function TabClosing(Tab: TJvTabBarItem): Boolean; virtual;
    function TabCloseQuery(Tab: TJvTabBarItem): Boolean; virtual;
    procedure TabClosed(Tab: TJvTabBarItem); virtual;
    function TabSelecting(Tab: TJvTabBarItem): Boolean; virtual;
    procedure TabSelected(Tab: TJvTabBarItem); virtual;
    procedure TabMoved(Tab: TJvTabBarItem); virtual;
    procedure Changed; virtual;
    procedure ImagesChanged(Sender: TObject); virtual;
    procedure ScrollButtonClick(Button: TJvTabBarScrollButtonKind); virtual;
    procedure LeftTabChanged; virtual;

    procedure DragOver(Source: TObject; X: Integer; Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DragCanceled; override;

    function ScrollButtonsMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer): Boolean; virtual;
    function ScrollButtonsMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer): Boolean; virtual;
    function ScrollButtonsMouseMove(Shift: TShiftState; X: Integer; Y: Integer): Boolean; virtual;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure CMMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddTab(const ACaption: string): TJvTabBarItem;
    function FindTab(const ACaption: string): TJvTabBarItem; // returns the first tab with the given Caption
    function TabAt(X, Y: Integer): TJvTabBarItem;
    function MakeVisible(Tab: TJvTabBarItem): Boolean;
    function FindData(Data: TObject): TJvTabBarItem;
    function CloseTab(ATab: TJvTabBarItem): Boolean;

    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;

    property PageListTabLink: Boolean read FPageListTabLink write FPageListTabLink default False; // if true the PageList's Pages[] are kept in sync with the Tabs
    property PageList: TCustomControl read FPageList write SetPageList;
    property Painter: TJvTabBarPainter read FPainter write SetPainter;
    property Images: TCustomImageList read FImages write SetImages;
    property Tabs: TJvTabBarItems read FTabs write SetTabs;

    // Status
    property SelectedTab: TJvTabBarItem read FSelectedTab write SetSelectedTab;
    property LeftTab: TJvTabBarItem read GetLeftTab write SetLeftTab;
    property HotTab: TJvTabBarItem read FHotTab;
    property ClosingTab: TJvTabBarItem read FClosingTab;

    // Options
    property Orientation: TJvTabBarOrientation read FOrientation write SetOrientation default toTop;
    property CloseButton: Boolean read FCloseButton write SetCloseButton default True;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default True;
    property HotTracking: Boolean read FHotTracking write FHotTracking default False;
    property AutoFreeClosed: Boolean read FAutoFreeClosed write FAutoFreeClosed default True;
    property AllowUnselected: Boolean read FAllowUnselected write FAllowUnselected default False;
    property SelectBeforeClose: Boolean read FSelectBeforeClose write FSelectBeforeClose default False;
    property Margin: Integer read FMargin write SetMargin default 6;
    property FlatScrollButtons: Boolean read FFlatScrollButtons write SetFlatScrollButtons default True;
    property Height read GetHeight write SetHeight;
    property Hint: TCaption read FHint write SetHint;
    property AllowTabMoving: Boolean read FAllowTabMoving write FAllowTabMoving default False;

    // Events

    { With OnTabClosing you can prevent the close button [X] in the tab from shrinking.
      If you want to ask the user you should use OnTabCloseQuery }
    property OnTabClosing: TJvTabBarClosingEvent read FOnTabClosing write FOnTabClosing;
    property OnTabCloseQuery: TJvTabBarCloseQueryEvent read FOnTabCloseQuery write FOnTabCloseQuery;
    property OnTabClosed: TJvTabBarItemEvent read FOnTabClosed write FOnTabClosed;
    property OnTabSelecting: TJvTabBarSelectingEvent read FOnTabSelecting write FOnTabSelecting;
    property OnTabSelected: TJvTabBarItemEvent read FOnTabSelected write FOnTabSelected;
    property OnTabMoved: TJvTabBarItemEvent read FOnTabMoved write FOnTabMoved;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScrollButtonClick: TJvTabBarScrollButtonClickEvent read FOnScrollButtonClick write FOnScrollButtonClick;
    property OnLeftTabChange: TNotifyEvent read FOnLeftTabChange write FOnLeftTabChange;
  end;

  TJvTabBar = class(TJvCustomTabBar)
  published
    property Align default alTop;
    property AutoSize default true;
    property BorderSpacing;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property ShowHint default False;
    property Height;
    property Hint;
    property Visible;
    property Enabled;

    property Orientation;
    property CloseButton;
    property RightClickSelect;
    property HotTracking;
    property AutoFreeClosed;
    property AllowUnselected;
    property SelectBeforeClose;
    property Margin;
    property FlatScrollButtons;
    property AllowTabMoving;

    property PageListTabLink;
    property PageList;
    property Painter;
    property Images;
   {$IF LCL_FullVersion >= 1090000}
    property ImagesWidth;
   {$ENDIF}
    property Tabs;

    property OnTabClosing;
    property OnTabCloseQuery;
    property OnTabClosed;
    property OnTabSelecting;
    property OnTabSelected;
    property OnTabMoved;
    property OnChange;
    property OnLeftTabChange;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnContextPopup;

    property OnClick;
    property OnDblClick;

    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;

    property OnStartDock;
    property OnEndDock;
  end;


implementation

uses
  JvJVCLUtils;

const
  WHEEL_DELTA = 120;

  // Pixels at 96 ppi:
  LEFT_MARGIN = 4;
  RIGHT_MARGIN = 6;
  TEXT_MARGIN_LEft = 2;
  TEXT_MARGIN_RIGHT = 4;
  TOP_MARGIN = 4;
  BOTTOM_MARGIN = 4;
  CLOSE_BUTTON_SIZE = 12;
  CROSS_MARGIN = 3;


function DrawButtonFace(ACanvas: TCanvas; const ARect: TRect; AFlat: Boolean;
//  BevelWidth: Integer; Style: TButtonStyle; IsRounded,
  IsDown, IsFocused: Boolean): TRect;
var
  R: TRect;
begin
  R := ARect;

  ACanvas.Pen.Style := psSolid;
  if AFlat then begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(R);
    if IsDown then begin
      ACanvas.Pen.Color := clBlack;
      ACanvas.Line(R.Left, R.Top, R.Left, R.Bottom-1);  // left
      ACanvas.Line(R.Left, R.Top, R.Right-1, R.Top);    // top
      ACanvas.Pen.Color := clBtnHighlight;
      ACanvas.Line(R.Right-1, R.Top, R.Right-1, R.Bottom-1);    // right
      ACanvas.Line(R.Left, R.Bottom-1, R.Right-1, R.Bottom-1);  // bottom
      InflateRect(R, -1, -1);
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.Line(R.Left, R.Top, R.Left, R.Bottom-1);  // left
      ACanvas.Line(R.Left, R.Top, R.Right-1, R.Top);    // top
    end else begin
      ACanvas.Pen.Color := clBlack;
      ACanvas.Line(R.Right-1, R.Top, R.Right-1, R.Bottom-1);    // right
      ACanvas.Line(R.Left, R.Bottom-1, R.Right-1, R.Bottom-1);  // bottom
      dec(R.Right);
      dec(R.Bottom);
      ACanvas.Pen.Color := clBtnHighlight;
      ACanvas.Line(R.Left, R.Top, R.Left, R.Bottom-1);  // left
      ACanvas.Line(R.Left, R.Top, R.Right-1, R.Top);    // top
      Inc(R.Top);
      Inc(R.Left);
      ACanvas.Line(R.Right-1, R.Top, R.Right-1, R.Bottom-1);    // right
      ACanvas.Line(R.Left, R.Bottom-1, R.Right-1, R.Bottom-1);  // bottom
    end;
  end else begin
    ACanvas.Pen.Color := clWindowFrame;
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Rectangle(R);
    if IsFocused then begin
      InflateRect(R, 1, 1);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Pen.Color := clBlack;
      ACanvas.Rectangle(R);
    end;
    InflateRect(R, -1, -1);
    if not IsDown then begin
      ACanvas.Pen.Color := clBtnHighlight;
      ACanvas.Line(R.Left, R.Top, R.Left, R.Bottom);  // left
      ACanvas.Line(R.Left, R.Top, R.Right, R.Top);    // top
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.Line(R.Right, R.Top, R.Right, R.Bottom);    // right
      ACanvas.Line(R.Left, R.Bottom, R.Right, R.Bottom);  // bottom
    end else begin
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.MoveTo(R.Left, R.Bottom-1);
      ACanvas.LineTo(R.Left, R.Top);
      ACanvas.LineTo(R.Right, R.Top);
    end;
  end;

  Result := Rect(ARect.Left + 1, ARect.Top + 1, ARect.Right - 2, ARect.Bottom - 2);
  if IsDown then OffsetRect(Result, 1, 1);
end;



//=== { TJvCustomTabBar } ====================================================

constructor TJvCustomTabBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csOpaque] {+ [csDesignInteractive]};

  FTabs := TJvTabBarItems.Create(Self, TJvTabBarItem);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := @ImagesChanged;

  FOrientation := toTop;
  FRightClickSelect := True;
  FCloseButton := True;
  FAutoFreeClosed := True;
  FFlatScrollButtons := True;
  FMargin := 6;
  Align := alTop;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  AutoSize := true;
end;

destructor TJvCustomTabBar.Destroy;
begin
  // these events are too dangerous during object destruction
  FOnTabSelected := nil;
  FOnTabSelecting := nil;
  FOnChange := nil;

  Painter := nil;
  Images := nil;
  FChangeLink.Free;
  FTabs.Free;
  FTabs := nil;
  FScrollButtonBackground.Free;
  FScrollButtonBackground := nil;

  inherited Destroy;
end;

procedure TJvCustomTabBar.LeftTabChanged;
begin
  if Assigned(FOnLeftTabChange) then
    FOnLeftTabChange(Self);
end;

procedure TJvCustomTabBar.Loaded;
begin
  inherited Loaded;
  SelectedTab := FindSelectableTab(nil);
  UpdateScrollButtons;
end;

procedure TJvCustomTabBar.Notification(Component: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(Component, Operation);
  if Operation = opRemove then
  begin
    if Component = FPainter then
      Painter := nil
    else
    if Component = FImages then
      Images := nil
    else
    if Component = FPageList then
      PageList := nil;
  end;
  if FTabs <> nil then
    for I := Tabs.Count - 1 downto 0 do
      Tabs[I].Notification(Component, Operation);
end;

procedure TJvCustomTabBar.DrawScrollBarGlyph(ACanvas: TCanvas; X, Y: Integer; ALeft, Disabled: Boolean);

  procedure OffsetPt(var Pt: TPoint; X, Y: Integer);
  begin
    Pt := Point(Pt.X + X, Pt.Y + Y);
  end;

const
  W = 4;
  H = 7;
var
  Pts: array [0..2] of TPoint;
  savedBrush: TBrush;
  savedPen: TPen;
begin
  savedBrush := TBrush.Create;
  savedPen := TPen.Create;
  try
    savedBrush.Assign(ACanvas.Brush);
    savedPen.Assign(ACanvas.Pen);

    if ALeft then
    begin
      Pts[0] := Point(X + W - 1, Y + 0);
      Pts[1] := Point(X + W - 1, Y + H - 1);
      Pts[2] := Point(X + 0, Y + (H - 1) div 2);
    end
    else
    begin
      Pts[0] := Point(X + 0, Y + 0);
      Pts[1] := Point(X + 0, Y + H - 1);
      Pts[2] := Point(X + W - 1, Y + (H - 1) div 2);
    end;
    ACanvas.Brush.Style := bsSolid;
    if Disabled then
    begin
      ACanvas.Brush.Color := clWhite;
      OffsetPt(Pts[0], 1, 1);
      OffsetPt(Pts[1], 1, 1);
      OffsetPt(Pts[2], 1, 1);
    end
    else
      ACanvas.Brush.Color := clBlack;

    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.Polygon(Pts);
    if Disabled then
    begin
      ACanvas.Brush.Color := clGray;
      OffsetPt(Pts[0], -1, -1);
      OffsetPt(Pts[1], -1, -1);
      OffsetPt(Pts[2], -1, -1);
      ACanvas.Pen.Color := ACanvas.Brush.Color;
      ACanvas.Polygon(Pts);
    end;
  finally
    ACanvas.Pen.Assign(savedPen);
    ACanvas.Brush.Assign(savedBrush);
    savedPen.Free;
    savedBrush.Free;
  end;
end;

procedure TJvCustomTabBar.SetTabs(Value: TJvTabBarItems);
begin
  if Value <> FTabs then
    FTabs.Assign(Value);
end;

procedure TJvCustomTabBar.SetPainter(Value: TJvTabBarPainter);
begin
  if Value <> FPainter then
  begin
    if FPainter <> nil then
      FPainter.FOnChangeList.Extract(Self);
    ReplaceComponentReference(Self, Value, tComponent(FPainter));
    if FPainter <> nil then
    begin
      FreeAndNil(FDefaultPainter);
      FPainter.FOnChangeList.Add(Self);
      if Parent <> nil then
        UpdateScrollButtons;
    end;

    if not (csDestroying in ComponentState) then
      Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetImages(Value: TCustomImageList);
begin
  if ReplaceImageListReference(Self, Value, FImages, FChangeLink) then
    if not (csDestroying in ComponentState) then
      Invalidate;
end;

procedure TJvCustomTabBar.SetImagesWidth(const AValue: Integer);
begin
  if AValue = FImagesWidth then exit;
  FImagesWidth := AValue;
  Invalidate;
end;

procedure TJvCustomTabBar.SetCloseButton(Value: Boolean);
begin
  if Value <> FCloseButton then
  begin
    FCloseButton := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetMargin(Value: Integer);
begin
  if Value <> FMargin then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetSelectedTab(Value: TJvTabBarItem);
begin
  if Value <> FSelectedTab then
  begin
    if (Value <> nil) and not Value.CanSelect then
      Exit;

    if TabSelecting(Value) then
    begin
      FSelectedTab := Value;
      if not (csDestroying in ComponentState) then
        Invalidate;
      MakeVisible(FSelectedTab);
      TabSelected(FSelectedTab);
    end;
  end;
end;

function TJvCustomTabBar.CurrentPainter: TJvTabBarPainter;
begin
  Result := FPainter;
  if Result = nil then
  begin
    if FDefaultPainter = nil then
      FDefaultPainter := TJvModernTabBarPainter.Create(Self);
    Result := FDefaultPainter;
  end;
end;

function TJvCustomTabBar.TabClosing(Tab: TJvTabBarItem): Boolean;
begin
  Result := True;
  if Assigned(FOnTabClosing) then
    FOnTabClosing(Self, Tab, Result);
end;

function TJvCustomTabBar.TabCloseQuery(Tab: TJvTabBarItem): Boolean;
begin
  Result := True;
  if Assigned(FOnTabCloseQuery) then
    FOnTabCloseQuery(Self, Tab, Result);
end;

procedure TJvCustomTabBar.TabClosed(Tab: TJvTabBarItem);
begin
  if AutoFreeClosed and not (csDesigning in ComponentState) then
    Tab.Visible := False;
  try
    if Assigned(FOnTabClosed) then
      FOnTabClosed(Self, Tab);
  finally
    // Do not double release if somebody "accidentally" released the Tab in TabClosed even if AutoFreeClosed is true
    if AutoFreeClosed and not (csDesigning in ComponentState) and (FTabs.IndexOf(Tab) <> -1) then
      Tab.Free;
  end;
end;

function TJvCustomTabBar.TabSelecting(Tab: TJvTabBarItem): Boolean;
begin
  Result := True;
  if Assigned(FOnTabSelecting) then
    FOnTabSelecting(Self, Tab, Result);
end;

procedure TJvCustomTabBar.TabSelected(Tab: TJvTabBarItem);
var
  PageListIntf: IPageList;
begin
  if (PageList <> nil) and Supports(PageList, IPageList, PageListIntf) then
  begin
    if Tab <> nil then
      PageListIntf.SetActivePageIndex(Tab.Index)
    else
      PageListIntf.SetActivePageIndex(-1);
    PageListIntf := nil; // who knows what OnTabSelected does with the PageList
  end;
  if Assigned(FOnTabSelected) then
    FOnTabSelected(Self, Tab);
end;

function TJvCustomTabBar.FindSelectableTab(Tab: TJvTabBarItem): TJvTabBarItem;
var
  Index: Integer;
begin
  Result := Tab;
  if (Result <> nil) and not Result.CanSelect then
  begin
    if AllowUnselected then
      Result := nil
    else
    begin
      Index := Result.Index + 1;
      while Index < Tabs.Count do
      begin
        if Tabs[Index].CanSelect then
          Break;
        Inc(Index);
      end;
      if Index >= Tabs.Count then
      begin
        Index := Result.Index - 1;
        while Index >= 0 do
        begin
          if Tabs[Index].CanSelect then
            Break;
          Dec(Index);
        end;
      end;
      if Index >= 0 then
        Result := Tabs[Index]
      else
        Result := nil;
    end;
  end;
  if not AllowUnselected and not (Result <> nil) then
  begin
    // try to find a selectable tab
    for Index := 0 to Tabs.Count - 1 do
      if Tabs[Index].CanSelect then
      begin
        Result := Tabs[Index];
        Break;
      end;
  end;
end;

procedure TJvCustomTabBar.Changed;
begin
  if not (csDestroying in ComponentState) then
  begin
    // The TabSelected tab is now no more selectable
    SelectedTab := FindSelectableTab(SelectedTab);
    if Tabs.UpdateCount = 0 then
    begin
      Invalidate;
      if Assigned(FOnChange) then
        FOnChange(Self);
      UpdateScrollButtons;
    end;
  end;
end;

procedure TJvCustomTabBar.ImagesChanged(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

procedure TJvCustomTabBar.TabMoved(Tab: TJvTabBarItem);
begin
  if Assigned(FOnTabMoved) then
    FOnTabMoved(Self, Tab);
end;

procedure TJvCustomTabBar.DragOver(Source: TObject; X: Integer; Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  InsertTab: TJvTabBarItem;
begin
  if AllowTabMoving then
  begin
    InsertTab := TabAt(X, Y);
    if InsertTab = nil then
      if (LeftTab <> nil) and (X < LeftTab.FLeft) then
        InsertTab := LeftTab
      else
      if Tabs.Count > 0 then
        InsertTab := Tabs[Tabs.Count - 1];

    Accept := (Source = Self) and (SelectedTab <> nil) and (InsertTab <> SelectedTab) and (InsertTab <> nil);
    if Accept then
    begin
      if InsertTab <> FLastInsertTab then
      begin
        if FLastInsertTab <> nil then
          Repaint;
        { Paint MoveDivider }
        FLastInsertTab := InsertTab;
        CurrentPainter.DrawMoveDivider(Canvas, InsertTab, InsertTab.Index < SelectedTab.Index);
      end;
      { inherited DrawOver sets Accept to False if no event handler is assigned. }
      if Assigned(OnDragOver) then
        OnDragOver(Self, Source, X, Y, State, Accept);
      Exit;
    end
    else
    if FLastInsertTab <> nil then
    begin
      Repaint;
      FLastInsertTab := nil;
    end;
  end;
  inherited DragOver(Source, X, Y, State, Accept);
end;

procedure TJvCustomTabBar.DragCanceled;
begin
  if FLastInsertTab <> nil then
    Repaint;
  FLastInsertTab := nil;
  inherited DragCanceled;
end;

procedure TJvCustomTabBar.DragDrop(Source: TObject; X: Integer; Y: Integer);
var
  InsertTab: TJvTabBarItem;
begin
  if AllowTabMoving and (Source = Self) and (SelectedTab <> nil) then
  begin
    InsertTab := TabAt(X, Y);
    if InsertTab = nil then
      if (LeftTab <> nil) and (X < LeftTab.FLeft) then
        InsertTab := LeftTab
      else
        InsertTab := Tabs[Tabs.Count - 1];
    if InsertTab <> nil then
    begin
      SelectedTab.Index := InsertTab.Index;
      TabMoved(SelectedTab);
      SelectedTab.MakeVisible;
      UpdateScrollButtons;
    end;
  end
  else
  if FLastInsertTab <> nil then
    Repaint;
  FLastInsertTab := nil;
  inherited DragDrop(Source, X, Y);
end;

procedure TJvCustomTabBar.CMMouseLeave(var Msg: TLMessage);
begin
  SetHotTab(nil);
  inherited;
end;

procedure TJvCustomTabBar.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
  Msg.Result := 1;
end;

function TJvCustomTabBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Result := True;

    if SelectedTab = nil then
      SelectedTab := LeftTab;
    if SelectedTab = nil then
      Exit; // nothing to do

    WheelDelta := WheelDelta div WHEEL_DELTA;
    while WheelDelta <> 0 do
    begin
      if WheelDelta < 0 then
      begin
        if SelectedTab.GetNextVisible <> nil then
          SelectedTab := SelectedTab.GetNextVisible
        else
          Break;
      end
      else
      begin
        if SelectedTab.GetPreviousVisible <> nil then
          SelectedTab := SelectedTab.GetPreviousVisible
        else
          Break;
      end;

      if WheelDelta < 0 then
        Inc(WheelDelta)
      else
        Dec(WheelDelta);
    end;
  end;
end;

procedure TJvCustomTabBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Tab: TJvTabBarItem;
  LastSelected: TJvTabBarItem;
begin
  if ScrollButtonsMouseDown(Button, Shift, X, Y) then
    Exit;

  if Button = mbLeft then
  begin
    FMouseDownClosingTab := nil;
    SetClosingTab(nil); // no tab should be closed

    LastSelected := SelectedTab;
    Tab := TabAt(X, Y);
    if Tab <> nil then
      SelectedTab := Tab;

    if (Tab <> nil) and (Tab = SelectedTab) then
      if CloseButton and (not SelectBeforeClose or (SelectedTab = LastSelected)) then
      begin
        if PtInRect(CurrentPainter.GetCloseRect(Canvas, Tab, Tab.DisplayRect), Point(X, Y)) then
        begin
          if TabClosing(Tab) then
          begin
            if FTabs.IndexOf(Tab) = -1 then
              Tab := nil; // We should not keep a reference if somebody "accidentally" released the Tab in TabClosing
            FMouseDownClosingTab := Tab;
            SetClosingTab(Tab);
          end;
          inherited MouseDown(Button, Shift, X, Y);
          Exit;
        end;
      end;
    if (FClosingTab = nil) and AllowTabMoving and
       ([ssLeft, ssMiddle, ssRight] * Shift = [ssLeft]) then
      BeginDrag(False);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomTabBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TPoint;
  Tab: TJvTabBarItem;
begin
  if ScrollButtonsMouseUp(Button, Shift, X, Y) then
    Exit;

  try
    if RightClickSelect and not (PopupMenu <> nil) and (Button = mbRight) then
    begin
      Tab := TabAt(X, Y);
      if Tab <> nil then
        SelectedTab := Tab;
      if (Tab <> nil) and (Tab.PopupMenu <> nil) then
      begin
        Pt := ClientToScreen(Point(X, Y));
        Tab.PopupMenu.Popup(Pt.X, Pt.Y);
      end;
    end
    else
    if Button = mbLeft then
    begin
      if (FClosingTab <> nil) and CloseButton then
      begin
        CalcTabsRects;
        if PtInRect(CurrentPainter.GetCloseRect(Canvas, FClosingTab, FClosingTab.DisplayRect), Point(X, Y)) then
        begin
          if TabCloseQuery(FClosingTab) then
            TabClosed(FClosingTab)
        end;
      end;
    end;
  finally
    FMouseDownClosingTab := nil;
    SetClosingTab(nil);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomTabBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Tab: TJvTabBarItem;
  NewHint: TCaption;
begin
  CalcTabsRects; // maybe inefficent
  if ScrollButtonsMouseMove(Shift, X, Y) then
    Exit;

  Tab := TabAt(X, Y);
  if HotTracking and ([ssLeft, ssMiddle, ssRight] * Shift = []) then
    SetHotTab(Tab);

  if CloseButton and (FMouseDownClosingTab <> nil) and (ssLeft in Shift) then
  begin
    if PtInRect(CurrentPainter.GetCloseRect(Canvas, FMouseDownClosingTab,
      FMouseDownClosingTab.DisplayRect), Point(X, Y)) then
      SetClosingTab(FMouseDownClosingTab)
    else
      SetClosingTab(nil)
  end;

  if (Tab <> nil) and Tab.ShowHint then
    NewHint := Tab.Hint
  else
    NewHint := FHint;

  if NewHint <> inherited Hint then
  begin
    Application.CancelHint;
    ShowHint := False;
    ShowHint := True;
    inherited Hint := NewHint;
  end;

  inherited MouseMove(Shift, X, Y);
end;

function TJvCustomTabBar.ScrollButtonsMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;

  function HandleButton(Kind: TJvTabBarScrollButtonKind; var State: TJvTabBarScrollButtonState;
    X, Y: Integer; const R: TRect): Boolean;
  begin
    Result := PtInRect(R, Point(X, Y));
    case State of
      sbsNormal, sbsHot:
        begin
          if Result then
          begin
            State := sbsPressed;
            PaintScrollButtons;

            if FRepeatTimer = nil then
              FRepeatTimer := TTimer.Create(Self);
            FRepeatTimer.OnTimer := @TimerExpired;
            FRepeatTimer.Interval := 400;
            FRepeatTimer.Enabled := True;
            FRepeatTimer.Tag := Integer(Kind);
            FScrollRepeatedClicked := False;
          end;
        end;
    end;
  end;

begin
  Result := False;
  if (FBtnLeftScroll.State <> sbsHidden) then
    Result := HandleButton(sbScrollLeft, FBtnLeftScroll.State, X, Y, FBtnLeftScroll.Rect);
  if not Result and (FBtnRightScroll.State <> sbsHidden) then
    Result := HandleButton(sbScrollRight, FBtnRightScroll.State, X, Y, FBtnRightScroll.Rect);
end;

function TJvCustomTabBar.ScrollButtonsMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;

  function HandleButton(var ExState: Boolean; var State: TJvTabBarScrollButtonState;
    X, Y: Integer; const R: TRect): Boolean;
  begin
    Result := PtInRect(R, Point(X, Y));
    case State of
      sbsNormal:
        begin
          if Result then
          begin
            State := sbsHot;
            PaintScrollButtons;
            Result := True;
          end;
        end;
      sbsPressed:
        begin
          if not Result then
          begin
            ExState := True;
            State := sbsNormal;
            PaintScrollButtons;
            State := sbsPressed;
          end
          else
          begin
            if ExState then
            begin
              ExState := False;
              PaintScrollButtons;
            end;
          end;
        end;
      sbsHot:
        begin
          if not Result then
          begin
            State := sbsNormal;
            PaintScrollButtons;
          end;
        end;
    end;
  end;

begin
  Result := False;
  if (FBtnLeftScroll.State <> sbsHidden) then
    Result := HandleButton(FBtnLeftScroll.ExState, FBtnLeftScroll.State, X, Y, FBtnLeftScroll.Rect);
  if not Result and (FBtnRightScroll.State <> sbsHidden) then
    Result := HandleButton(FBtnRightScroll.ExState, FBtnRightScroll.State, X, Y, FBtnRightScroll.Rect);
end;

function TJvCustomTabBar.ScrollButtonsMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;

  function HandleButton(Kind: TJvTabBarScrollButtonKind; var State: TJvTabBarScrollButtonState;
    X, Y: Integer; const R: TRect): Boolean;
  begin
    Result := PtInRect(R, Point(X, Y));
    case State of
      sbsPressed:
        begin
          FreeAndNil(FRepeatTimer);
          State := sbsNormal;
          PaintScrollButtons;
          if Result and not FScrollRepeatedClicked then
            ScrollButtonClick(Kind);
          FScrollRepeatedClicked := False;
        end;
    end;
  end;

begin
  Result := False;
  if (FBtnLeftScroll.State <> sbsHidden) then
    Result := HandleButton(sbScrollLeft, FBtnLeftScroll.State, X, Y, FBtnLeftScroll.Rect);
  if not Result and (FBtnRightScroll.State <> sbsHidden) then
    Result := HandleButton(sbScrollRight, FBtnRightScroll.State, X, Y, FBtnRightScroll.Rect);
end;

procedure TJvCustomTabBar.TimerExpired(Sender: TObject);
var
  Kind: TJvTabBarScrollButtonKind;
  State: TJvTabBarScrollButtonState;
begin
  FRepeatTimer.Interval := 100;
  Kind := TJvTabBarScrollButtonKind(FRepeatTimer.Tag);
  case Kind of
    sbScrollLeft:
      State := FBtnLeftScroll.State;
    sbScrollRight:
      State := FBtnRightScroll.State;
  else
    Exit;
  end;

  if (State = sbsPressed) and Enabled {and MouseCapture} then
  begin
    try
      FScrollRepeatedClicked := True;
      ScrollButtonClick(Kind);
      case Kind of
        sbScrollLeft:
          if not (FBtnLeftScroll.State in [sbsHidden, sbsDisabled]) then
            FBtnLeftScroll.State := sbsPressed;
        sbScrollRight:
          if not (FBtnRightScroll.State in [sbsHidden, sbsDisabled]) then
            FBtnRightScroll.State := sbsPressed;
      end;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end
  else
    FreeAndNil(FRepeatTimer);
end;

procedure TJvCustomTabBar.SetHotTab(Tab: TJvTabBarItem);
begin
  if (csDestroying in ComponentState) or not HotTracking then
    FHotTab := nil
  else
  if Tab <> FHotTab then
  begin
    FHotTab := Tab;
    if poPaintsHotTab in CurrentPainter.Options then
      Paint;
  end;
end;

function TJvCustomTabBar.CloseTab(ATab: TJvTabBarItem): Boolean;
begin
  Result := False;
  if ATab <> nil then
  begin
     FClosingTab := ATab;
    try
      Result := TabCloseQuery(FClosingTab);
      if Result then
        TabClosed(FClosingTab);
    finally
      FClosingTab := nil;
    end;
  end;
end;

function TJvCustomTabBar.AddTab(const ACaption: string): TJvTabBarItem;
begin
  Result := TJvTabBarItem(Tabs.Add);
  Result.Caption := ACaption;
end;

function TJvCustomTabBar.FindTab(const ACaption: string): TJvTabBarItem;
var
  i: Integer;
begin
  for i := 0 to Tabs.Count - 1 do
    if ACaption = Tabs[i].Caption then
    begin
      Result := Tabs[i];
      Exit;
    end;
  Result := nil;
end;

procedure TJvCustomTabBar.CalcTabsRects;
var
  I, X: Integer;
  Tab: TJvTabBarItem;
  Offset: Integer;
  Index: Integer;
begin
  if csDestroying in ComponentState then
    Exit;

  Offset := 0;
  X := Margin;  // adjust for scrolled area
  Index := 0;
  for I := 0 to Tabs.Count - 1 do
  begin
    Tab := Tabs[I];
    if Tab.Visible then
    begin
      Tab.FLeft := X;
      Inc(X, GetTabWidth(Tab));
      Inc(X, CurrentPainter.GetDividerWidth(Canvas, Tab));
      if Index < FLeftIndex then
      begin
        Inc(Offset, X); // this tab is placed too left.
        X := 0;
        Tab.FLeft := -Offset - 10;
      end;
      Inc(Index);
    end
    else
      Tab.FLeft := -1;
  end;

  FRequiredWidth := X + Offset;
  FLastTabRight := X;
end;

procedure TJvCustomTabBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  tabSize: TSize;
  imgSize: TSize;
  h: Integer;
 {$IF LCL_FullVersion >= 1090000}
  imgRes: TScaledImageListResolution;
 {$ENDIF}
begin
  CurrentPainter.AutoSize;

  // Text height
  Canvas.Font.Assign(Font);
  PreferredHeight := Canvas.TextHeight('Tg');

  // Icon height
  if FImages <> nil then begin
   {$IF LCL_FullVersion >= 1090000}
    imgRes := FImages.ResolutionForPPI[FImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
    h := imgRes.Height;
    if imgRes.Height > PreferredHeight then
      PreferredHeight := imgRes.Height;
   {$ELSE}
    h := Images.Height;
   {$ENDIF}
    if h > PReferredHeight then
      PreferredHeight := h;
  end;

  // Close button height
  if FCloseButton then begin
    h := Scale96ToForm(CLOSE_BUTTON_SIZE);
    if h > PreferredHeight then
      PreferredHeight := h;
  end;

  // Margins
  inc(PreferredHeight, Scale96ToForm(TOP_MARGIN) + Scale96ToForm(BOTTOM_MARGIN));
end;

procedure TJvCustomTabBar.Paint;
var
  I: Integer;
  Bmp: TBitmap;
  R: TRect;
begin
  CalcTabsRects;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;
    CurrentPainter.DrawBackground(Bmp.Canvas, Self, ClientRect);
    if (FBtnLeftScroll.State <> sbsHidden) and (FBtnRightScroll.State <> sbsHidden) then
    begin
      if FScrollButtonBackground = nil then
        FScrollButtonBackground := TBitmap.Create;
      FScrollButtonBackground.Width := Bmp.Width - FBarWidth;
      FScrollButtonBackground.Height := Bmp.Height;
      R := Rect(FBarWidth, 0, Bmp.Width, Bmp.Height);
      FScrollButtonBackground.Canvas.CopyRect(Rect(0, 0, FScrollButtonBackground.Width, R.Bottom), Bmp.Canvas, R);
      PaintScrollButtons;
      if FBarWidth > 0 then
        Bmp.Width := FBarWidth;
    end;

    if FBarWidth > 0 then
      for I := 0 to Tabs.Count - 1 do
        if Tabs[I].Visible then
          PaintTab(Bmp.Canvas, Tabs[I]);
    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TJvCustomTabBar.PaintTab(ACanvas: TCanvas; Tab: TJvTabBarItem);
var
  R: TRect;
begin
  if csDestroying in ComponentState then
    Exit;

  if Tab.Visible then
  begin
    R := Tab.DisplayRect;
    if (R.Right >= 0) and (R.Left < FBarWidth) then
    begin
      CurrentPainter.DrawTab(ACanvas, Tab, R);
      R.Left := R.Right;
      R.Right := R.Left + CurrentPainter.GetDividerWidth(ACanvas, Tab) - 1;
      CurrentPainter.DrawDivider(ACanvas, Tab, R);
    end;
  end;
end;

procedure TJvCustomTabBar.PaintScrollButtons;
begin
  if not HandleAllocated then
    exit;

  if (FScrollButtonBackground = nil) and Visible then
    Paint
  else // paint scroll button's background and the buttons
    Canvas.Draw(FBarWidth, 0, FScrollButtonBackground);

  CurrentPainter.DrawScrollButton(Canvas, Self, sbScrollLeft, FBtnLeftScroll.State, FBtnLeftScroll.Rect);
  CurrentPainter.DrawScrollButton(Canvas, Self, sbScrollRight, FBtnRightScroll.State, FBtnRightScroll.Rect);
end;

class function TJvCustomTabBar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 100;
  Result.CY := 24;
end;

function TJvCustomTabBar.GetTabHeight(Tab: TJvTabBarItem): Integer;
begin
  Result := CurrentPainter.GetTabSize(Canvas, Tab).cy;
end;

function TJvCustomTabBar.GetTabWidth(Tab: TJvTabBarItem): Integer;
begin
  Result := CurrentPainter.GetTabSize(Canvas, Tab).cx;
end;

function TJvCustomTabBar.TabAt(X, Y: Integer): TJvTabBarItem;
var
  I: Integer;
  Pt: TPoint;
begin
  if (FBtnLeftScroll.State = sbsHidden) or (X < FBarWidth) then
  begin
    CalcTabsRects;
    Pt := Point(X, Y);
    for I := 0 to Tabs.Count - 1 do
      if PtInRect(Tabs[I].DisplayRect, Pt) then
      begin
        Result := Tabs[I];
        Exit;
      end;
  end;
  Result := nil;
end;

procedure TJvCustomTabBar.SetClosingTab(Tab: TJvTabBarItem);
begin
  if Tab <> FClosingTab then
  begin
    FClosingTab := Tab; // this tab should be TabClosed
    Paint;
  end;
end;

function TJvCustomTabBar.GetLeftTab: TJvTabBarItem;
begin
  if (Tabs <> nil) and (FLeftIndex < Tabs.Count) then
  begin
    Result := Tabs[FLeftIndex];
    if not Result.Visible then
      Result := Result.GetNextVisible;
  end
  else
    Result := nil;
end;

procedure TJvCustomTabBar.SetLeftTab(Value: TJvTabBarItem);
var
  Index: Integer;
  Tab: TJvTabBarItem;
begin
  Index := 0;
  if Value <> nil then
  begin
    // find first visible before or at Value.Index
    if (Tabs <> nil) and (Tabs.Count > 0) and (Value <> Tabs[0]) then
    begin
      while Index < Tabs.Count do
      begin
        Tab := Tabs[Index].GetNextVisible;
        if Tab = nil then
        begin
          Index := FLeftIndex; // do not change
          Break;
        end
        else
        begin
          Index := Tab.Index;
          if Tab.Index >= Value.Index then
            Break;
        end;
      end;
      if Index >= Tabs.Count then
        Index := FLeftIndex; // do not change
    end;
  end;
  if Index <> FLeftIndex then
  begin
    FLeftIndex := Index;
    Invalidate;
    UpdateScrollButtons;
    LeftTabChanged;
  end;
end;

procedure TJvCustomTabBar.UpdateScrollButtons;
const
  State: array[Boolean] of TJvTabBarScrollButtonState = (sbsDisabled, sbsNormal);
  BtnSize = 12;
begin
  CalcTabsRects;
  if (FRequiredWidth < ClientWidth) or ((FLeftIndex = 0) and
    (FLastTabRight <= ClientWidth)) then
  begin
    FBtnLeftScroll.State := sbsHidden;
    FBtnRightScroll.State := sbsHidden;
    FLeftIndex := 0;
    FBarWidth := ClientWidth;
    Invalidate;
  end
  else
  begin
    FBtnLeftScroll.State := sbsNormal;
    FBtnRightScroll.State := sbsNormal;

    if poBottomScrollButtons in CurrentPainter.Options then
    begin
      FBtnLeftScroll.Rect := Bounds(ClientWidth - BtnSize * 2 - 1 - 1,
        ClientHeight - BtnSize - 2, BtnSize, BtnSize);
      FBtnRightScroll.Rect := Bounds(FBtnLeftScroll.Rect.Right,
        ClientHeight - BtnSize - 2, BtnSize, BtnSize);
    end
    else
    begin
      FBtnLeftScroll.Rect := Bounds(ClientWidth - BtnSize * 2 - 1 - 1, 2, BtnSize, BtnSize);
      FBtnRightScroll.Rect := Bounds(FBtnLeftScroll.Rect.Right, 2, BtnSize, BtnSize);
    end;
    if not FlatScrollButtons then
      OffsetRect(FBtnRightScroll.Rect, -1, 0);

    //CurrentPainter.GetScrollButtons(Self, FBtnLeftScroll.Rect, FBtnRightScroll.Rect);

    FBarWidth := FBtnLeftScroll.Rect.Left - 2;

    FBtnLeftScroll.State := State[FLeftIndex > 0];
    FBtnRightScroll.State := State[FLastTabRight >= ClientWidth];

    PaintScrollButtons;
  end;
end;

procedure TJvCustomTabBar.Resize;
begin
  UpdateScrollButtons;
  inherited Resize;
end;

procedure TJvCustomTabBar.ScrollButtonClick(Button: TJvTabBarScrollButtonKind);
begin
  if Button = sbScrollLeft then
  begin
    if FBtnLeftScroll.State in [sbsHidden, sbsDisabled] then
      Exit;
    Dec(FLeftIndex);
  end
  else
  if Button = sbScrollRight then
  begin
    if FBtnRightScroll.State in [sbsHidden, sbsDisabled] then
      Exit;
    Inc(FLeftIndex);
  end;
  UpdateScrollButtons;
  Invalidate;
  if Assigned(FOnScrollButtonClick) then
    FOnScrollButtonClick(Self, Button);
  LeftTabChanged;
end;

function TJvCustomTabBar.MakeVisible(Tab: TJvTabBarItem): Boolean;
var
  R: TRect;
  LastLeftIndex: Integer;
  AtLeft: Boolean;
begin
  Result := False;
  if (Tab = nil) or not Tab.Visible then
    Exit;

  LastLeftIndex := FLeftIndex;
  if FBarWidth > 0 then
  begin
    AtLeft := False;
    repeat
      CalcTabsRects;
      R := Tab.DisplayRect;
      if (R.Right > FBarWidth) and not AtLeft then
        Inc(FLeftIndex)
      else
      if R.Left < 0 then
      begin
        Dec(FLeftIndex);
        AtLeft := True; // prevent an endless loop
      end
      else
        Break;
    until FLeftIndex = Tabs.Count - 1;
  end
  else
    FLeftIndex := 0;
  if (R.Left < 0) and (FLeftIndex > 0) then
    Dec(FLeftIndex); // bar is too small
  if FLeftIndex <> LastLeftIndex then
  begin
    UpdateScrollButtons;
    Invalidate;
    LeftTabChanged;
  end;
end;

function TJvCustomTabBar.FindData(Data: TObject): TJvTabBarItem;
var
  I: Integer;
begin
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Data = Data then
    begin
      Result := Tabs[I];
      Exit;
    end;
  Result := nil;
end;

procedure TJvCustomTabBar.SetHint(const Value: TCaption);
begin
  if Value <> FHint then
    FHint := Value;
end;

procedure TJvCustomTabBar.SetFlatScrollButtons(const Value: Boolean);
begin
  if Value <> FFlatScrollButtons then
  begin
    FFlatScrollButtons := Value;
    FBtnLeftScroll.State := sbsHidden;
    FBtnRightScroll.State := sbsHidden;
    UpdateScrollButtons;
  end;
end;

function TJvCustomTabBar.GetHeight: Integer;
begin
  Result := inherited Height;
end;

procedure TJvCustomTabBar.SetHeight(AValue: Integer);
begin
  if AValue = GetHeight then exit;
  AutoSize := false;
  inherited Height := AValue;
end;

procedure TJvCustomTabBar.SetPageList(const Value: TCustomControl);
var
  PageListIntf: IPageList;
begin
  if Value <> FPageList then
  begin
    if Value <> nil then
    begin
      if not Supports(Value, IPageList, PageListIntf) then
        Exit;
      if SelectedTab <> nil then
        PageListIntf.SetActivePageIndex(SelectedTab.Index)
      else
        PageListIntf.SetActivePageIndex(0);
      PageListIntf := nil;
    end;
    if FPageList <> nil then
      FPageList.RemoveFreeNotification(Self);
    FPageList := Value;
    if FPageList <> nil then
      FPageList.FreeNotification(Self);
  end;
end;

procedure TJvCustomTabBar.SetOrientation(const Value: TJvTabBarOrientation);
begin
  if Value <> FOrientation then
  begin
    FOrientation := Value;
    CalcTabsRects;
    Repaint;
  end;
end;

//=== { TJvTabBarItem } ======================================================

constructor TJvTabBarItem.Create(ACollection: Classes.TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
  FEnabled := True;
  FVisible := True;
  FShowHint := True;
end;

destructor TJvTabBarItem.Destroy;
begin
  PopupMenu := nil;
  Visible := False; // CanSelect returns false
  FAutoDeleteDatas.Free;
  inherited Destroy;
end;

procedure TJvTabBarItem.Assign(Source: TPersistent);
begin
  if Source is TJvTabBarItem then
  begin
    with TJvTabBarItem(Source) do
    begin
      Self.FImageIndex := FImageIndex;
      Self.FEnabled := FEnabled;
      Self.FVisible := FVisible;
      Self.FTag := FTag;
      Self.FData := FData;
      Self.FHint := FHint;
      Self.FShowHint := FShowHint;
      Self.FName := FName;
      Self.FCaption := FCaption;
      Self.FModified := FModified;
      Self.FImages := FImages;
      Changed;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTabBarItem.Notification(Component: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    if Component = PopupMenu then
      PopupMenu := nil;
end;

procedure TJvTabBarItem.Changed;
begin
  TabBar.Changed;
end;

function TJvTabBarItem.GetDisplayRect: TRect;
begin
  if not Visible then
    Result := Rect(-1, -1, -1, -1)
  else
  begin
    if FLeft = -1 then
      TabBar.CalcTabsRects; // not initialized

    case TabBar.Orientation of
      toBottom:
        Result := Rect(
          FLeft,
          0,
          FLeft + TabBar.GetTabWidth(Self),
          0 + TabBar.GetTabHeight(Self)
        );
    else
      // toTop
      Result := Rect(
        FLeft,
        TabBar.ClientHeight - TabBar.GetTabHeight(Self),
        FLeft + TabBar.GetTabWidth(Self),
        TabBar.ClientHeight
      );
    end;
  end;
end;

function TJvTabBarItem.GetHot: Boolean;
begin
  Result := (TabBar.HotTab = Self);
end;

function TJvTabBarItem.GetImages: TCustomImageList;
begin
  Result := TabBar.Images;
end;

function TJvTabBarItem.GetSelected: Boolean;
begin
  Result := TabBar.SelectedTab = Self;
end;

function TJvTabBarItem.GetTabBar: TJvCustomTabBar;
begin
  Result := (GetOwner as TJvTabBarItems).TabBar;
end;

procedure TJvTabBarItem.SetCaption(const Value: TCaption);
var
  PageListIntf: IPageList;
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    if TabBar.PageListTabLink and (TabBar.PageList <> nil) and
       not (csLoading in TabBar.ComponentState) and
       Supports(TabBar.PageList, IPageList, PageListIntf)
    then
      PageListIntf.PageCaptionChanged(Index, FCaption);
    Changed;
  end;
end;

procedure TJvTabBarItem.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TJvTabBarItem.SetImageIndex(const Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TJvTabBarItem.SetName(const Value: string);
begin
  if (Value <> FName) and (TJvTabBarItems(Collection).Find(Value) = nil) then
    FName := Value;
end;

procedure TJvTabBarItem.SetSelected(const Value: Boolean);
begin
  if Value then
    TabBar.SelectedTab := Self;
end;

procedure TJvTabBarItem.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    FLeft := -1; // discard
    Changed;
  end;
end;

function TJvTabBarItem.CanSelect: Boolean;
begin
  Result := Visible and Enabled;
end;

function TJvTabBarItem.GetNextVisible: TJvTabBarItem;
var
  I: Integer;
begin
  for I := Index + 1 to TabBar.Tabs.Count - 1 do
    if TabBar.Tabs[I].Visible then
    begin
      Result := TabBar.Tabs[I];
      Exit;
    end;
  Result := nil;
end;

function TJvTabBarItem.GetPreviousVisible: TJvTabBarItem;
var
  I: Integer;
begin
  for I := Index - 1 downto 0 do
    if TabBar.Tabs[I].Visible then
    begin
      Result := TabBar.Tabs[I];
      Exit;
    end;
  Result := nil;
end;

function TJvTabBarItem.AutoDeleteData: TObjectList;
begin
  if FAutoDeleteDatas = nil then
    FAutoDeleteDatas := TObjectList.Create;
  Result := FAutoDeleteDatas;
end;

function TJvTabBarItem.GetClosing: Boolean;
begin
  Result := TabBar.ClosingTab = Self;
end;

procedure TJvTabBarItem.SetModified(const Value: Boolean);
begin
  if Value <> FModified then
  begin
    FModified := Value;
    Changed;
  end;
end;

procedure TJvTabBarItem.SetPopupMenu(const Value: TPopupMenu);
begin
  if Value <> FPopupMenu then
  begin
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(TabBar);
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(TabBar);
  end;
end;

procedure TJvTabBarItem.MakeVisible;
begin
  TabBar.MakeVisible(Self);
end;

function TJvTabBarItem.GetEnabled: Boolean;
begin
  Result := FEnabled;
  if Assigned(FOnGetEnabled) then
    FOnGetEnabled(Self, Result);
end;

function TJvTabBarItem.GetModified: Boolean;
begin
  Result := FModified;
  if Assigned(FOnGetModified) then
    FOnGetModified(Self, Result);
end;

procedure TJvTabBarItem.SetIndex(Value: Integer);
var
  PageListIntf: IPageList;
  LastIndex: Integer;
begin
  LastIndex := Index;
  inherited SetIndex(Value);
  if TabBar.PageListTabLink and (LastIndex <> Index) and (TabBar.PageList <> nil) and
     not (csLoading in TabBar.ComponentState) and
     Supports(TabBar.PageList, IPageList, PageListIntf) then
    PageListIntf.MovePage(LastIndex, Index);
  Changed;
end;

//=== { TJvTabBarItems } =====================================================

procedure TJvTabBarItems.EndUpdate;
begin
  inherited EndUpdate;
  if UpdateCount = 0 then
    TabBar.Changed;
end;

function TJvTabBarItems.Find(const AName: string): TJvTabBarItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJvTabBarItems.GetTabBar: TJvCustomTabBar;
begin
  Result := GetOwner as TJvCustomTabBar;
end;

function TJvTabBarItems.GetItem(Index: Integer): TJvTabBarItem;
begin
  Result := TJvTabBarItem(inherited Items[Index]);
end;

procedure TJvTabBarItems.SetItem(Index: Integer; const Value: TJvTabBarItem);
begin
  if Value <> GetItem(Index) then
    GetItem(Index).Assign(Value);
end;

procedure TJvTabBarItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
var
  PageListIntf: IPageList;
begin
  inherited Notify(Item, Action);
  if Action in [cnExtracting, cnDeleting] then
  begin
    // unselect the item to delete
    if TabBar.SelectedTab = Item then
      TabBar.SelectedTab := nil;
    if TabBar.HotTab = Item then
      TabBar.SetHotTab(nil);
    if TabBar.FMouseDownClosingTab = Item then
      TabBar.FMouseDownClosingTab := nil;
    if TabBar.ClosingTab = Item then
      TabBar.FClosingTab := nil;
    if TabBar.FLastInsertTab = Item then
      TabBar.FLastInsertTab := nil;
    if not (csDestroying in TabBar.ComponentState) and (TabBar.LeftTab = Item) then
      TabBar.LeftTab := TabBar.LeftTab.GetPreviousVisible;
  end;
  if TabBar.PageListTabLink and (TabBar.PageList <> nil) and
     not (csLoading in TabBar.ComponentState) and
     Supports(TabBar.PageList, IPageList, PageListIntf) then
  begin
    case Action of
      cnAdded:
        PageListIntf.AddPage(TJvTabBarItem(Item).Caption);
      cnExtracting, cnDeleting:
        PageListIntf.DeletePage(TJvTabBarItem(Item).Index);
    end;
  end;
  TabBar.Changed;
end;

function TJvTabBarItems.IndexOf(Item: TJvTabBarItem): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = Item then
      Exit;
  Result := -1;
end;

//=== { TJvTabBarPainter } ===================================================

constructor TJvTabBarPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnChangeList := TList.Create;
end;

destructor TJvTabBarPainter.Destroy;
begin
  inherited Destroy; // invokes TJvTabBar.Notification that accesses FOnChangeList
  FOnChangeList.Free;
end;

procedure TJvTabBarPainter.AutoSize;
begin
end;

procedure TJvTabBarPainter.Changed;
var
  i: Integer;
begin
  for i := 0 to FOnChangeList.Count - 1 do
    TJvCustomTabBar(FOnChangeList[i]).ImagesChanged(Self);
end;

function TJvTabBarPainter.GetRealImageSize(ATab: TJvTabBarItem): TSize;
{$IF LCL_FullVersion >= 1090000}
var
  imgRes: TScaledImageListResolution;
  tabBar: TJvCustomTabBar;
  f: Double;
  ppi: Integer;
begin
  tabBar := GetTabBar(ATab);
  f := tabBar.GetCanvasScaleFactor;
  ppi := GetPixelsPerInch;
  imgRes := ATab.GetImages.ResolutionForPPI[tabBar.ImagesWidth, ppi, f];
  Result.CX := imgRes.Width;
  Result.CY := imgRes.Height;
end;
{$ELSE}
begin
  Result.CX := ATab.GetImages.Width;
  Result.CY := ATab.GetImages.Height;
end;
{$ENDIF}

procedure TJvTabBarPainter.GetScrollButtons(TabBar: TJvCustomTabBar; var LeftButton, RightButton: TRect);
begin
  { reserved for future use }
end;

function TJvTabBarPainter.GetTabBar(ATab: TJvTabBarItem): TJvCustomTabBar;
begin
  Result := TJvTabBarItems(ATab.Collection).TabBar;
end;

procedure TJvTabBarPainter.DrawScrollButton(Canvas: TCanvas;
  TabBar: TJvCustomTabBar; Button: TJvTabBarScrollButtonKind;
  State: TJvTabBarScrollButtonState; R: TRect);
{$IFDEF JVCLThemesEnabled}
const
  SCROLL: array[TJvTabBarScrollButtonKind, TJvTabBarScrollButtonState] of TThemedScrollbar = (
  // sbsHidden              sbsNormal              sbsHot              sbsPressed              sbsDisabled
    (tsArrowBtnLeftNormal,  tsArrowBtnLeftNormal,  tsArrowBtnLeftHot,  tsArrowBtnLeftPressed,  tsArrowBtnLeftDisabled),
    (tsArrowBtnRightNormal, tsArrowBtnRightNormal, tsArrowBtnRightHot, tsArrowBtnRightPressed, tsArrowBtnRightDisabled)
  );
var
  details: TThemedElementDetails;
{$ENDIF JVCLThemesEnabled}
begin
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then begin
    details := ThemeServices.GetElementDetails(SCROLL[Button, State]);
    ThemeServices.DrawElement(Canvas.Handle, details, R, nil);
  end else
  {$ENDIF JVCLThemesEnabled}
  begin
    DrawButtonFace(Canvas, R, TabBar.FlatScrollButtons, State = sbsPressed, false);
    if State = sbsPressed then
      OffsetRect(R, 1, 1);
    TabBar.DrawScrollBarGlyph(Canvas,
      R.Left + (R.Right - R.Left - 4) div 2,
      R.Top + (R.Bottom - R.Top - 7) div 2,
      Button = sbScrollLeft, State = sbsDisabled);
  end;
end;

function TJvTabBarPainter.Scale96(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, GetPixelsPerInch, 96);
end;


//=== { TJvModernTabBarPainter } =============================================

constructor TJvModernTabBarPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FDisabledFont := TFont.Create;
  FSelectedFont := TFont.Create;

  FFont.Color := clWindowText;
  FDisabledFont.Color := clGrayText;
  FSelectedFont.Assign(FFont);

  FFont.OnChange := @FontChanged;
  FDisabledFont.OnChange := @FontChanged;
  FSelectedFont.OnChange := @FontChanged;

  FTabColor := clBtnFace;
  FColor := clWindow;
  FBorderColor := clSilver;
  FControlDivideColor := clBlack;

  FModifiedCrossColor := clRed;
  FCloseColorSelected := $F4F4F4;
  FCloseColor := clWhite;
  FCloseCrossColorSelected := clBlack;
  FCloseCrossColor := $5D5D5D;
  FCloseCrossColorDisabled := $ADADAD;
  FCloseRectColor := $868686;
  FCloseRectColorDisabled := $D6D6D6;
  FDividerColor := $99A8AC;
  FMoveDividerColor := clBlack;
end;

destructor TJvModernTabBarPainter.Destroy;
begin
  FFont.Free;
  FDisabledFont.Free;
  FSelectedFont.Free;
  inherited Destroy;
end;

procedure TJvModernTabBarPainter.AutoSize;
begin
  FTabHeight := 0;
  FTabWidth := 0;
end;

procedure TJvModernTabBarPainter.DrawBackground(Canvas: TCanvas; TabBar: TJvCustomTabBar; R: TRect);
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(R);

    Brush.Style := bsClear;
    Pen.Color := BorderColor;
    Pen.Width := 1;
    if TabBar.Orientation = toBottom then
    begin
      MoveTo(0, R.Bottom - 1);
      LineTo(0, 0);
      Pen.Color := ControlDivideColor;
      LineTo(R.Right - 1, 0);
      Pen.Color := BorderColor;
      LineTo(R.Right - 1, R.Bottom - 1);
      LineTo(0, R.Bottom - 1);
    end
    else // toTop
    begin
      MoveTo(0, R.Bottom - 1);
      LineTo(0, 0);
      LineTo(R.Right - 1, 0);
      LineTo(R.Right - 1, R.Bottom - 1);
      Pen.Color := ControlDivideColor;
      LineTo(0, R.Bottom - 1);
    end;
  end;
end;

procedure TJvModernTabBarPainter.DrawDivider(Canvas: TCanvas; LeftTab: TJvTabBarItem; R: TRect);
begin
  if not LeftTab.Selected then
  begin
    if (LeftTab.TabBar.SelectedTab = nil) or
      (LeftTab.GetNextVisible <> LeftTab.TabBar.SelectedTab) then
    begin
      with Canvas do
      begin
        Pen.Color := DividerColor;
        Pen.Width := 1;
        MoveTo(R.Right - 1, R.Top + 3);
        LineTo(R.Right - 1, R.Bottom - 3);
      end;
    end;
  end;
end;

procedure TJvModernTabBarPainter.DrawMoveDivider(Canvas: TCanvas; Tab: TJvTabBarItem; MoveLeft: Boolean);
var
  R: TRect;
begin
  with Canvas do
  begin
    R := Tab.DisplayRect;
    Inc(R.Top, 4);
    Dec(R.Bottom, 2);
    if MoveLeft then
    begin
      Dec(R.Left);
      R.Right := R.Left + 4
    end
    else
    begin
      Dec(R.Right, 1);
      R.Left := R.Right - 4;
    end;
    Brush.Color := MoveDividerColor;
    FillRect(R);
  end;
end;

procedure TJvModernTabBarPainter.DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem;
  ATabRect: TRect);
var
  R, CloseR: TRect;
  ts: TTextStyle;
  margin: Integer;
  x, y: Integer;
  imgsize: TSize;
 {$IF LCL_FullVersion >= 1090000}
  imageRes: TScaledImageListResolution;
  f: Double;
  ppi: Integer;
  tabBar: TJvCustomTabBar;
 {$ENDIF}
begin
  R := ATabRect;

  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;

    if Tab.Selected then
    begin
      Brush.Style := bsSolid;
      Brush.Color := TabColor;
      FillRect(R);

      Pen.Color := ControlDivideColor;
      if Tab.TabBar.Orientation = toBottom then
      begin
        MoveTo(R.Left, R.Top);
        LineTo(R.Left, R.Bottom - 1);
        LineTo(R.Right - 1, R.Bottom - 1);
        LineTo(R.Right - 1, R.Top - 1{end});
      end
      else // toTop
      begin
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Left, R.Top);
        LineTo(R.Right - 1, R.Top);
        LineTo(R.Right - 1, R.Bottom - 1 + 1{end});
      end;
    end;

    if Tab.Enabled and not Tab.Selected and Tab.Hot then
    begin
      // hot
      Pen.Color := DividerColor;
      MoveTo(R.Left, R.Top);
      LineTo(R.Right - 1 - 1, R.Top);
    end;

    inc(R.Left, Scale96(LEFT_MARGIN));
    dec(R.Right, Scale96(RIGHT_MARGIN));

    if Tab.TabBar.CloseButton then
    begin
      CloseR := GetCloseRect(Canvas, Tab, ATabRect);

      // close button color
      if Tab.Selected then
        Brush.Color := CloseColorSelected
      else
        Brush.Color := CloseColor;
      Pen.Color := CloseRectColor;
      if not Tab.Enabled then
        Pen.Color := CloseRectColorDisabled;

      if Tab.Closing then
        // shrink
        Rectangle(CloseR.Left + 1, CloseR.Top + 1, CloseR.Right - 1, CloseR.Bottom - 1)
      else
        Rectangle(CloseR);

      if Tab.Modified then
        Pen.Color := ModifiedCrossColor
      else
      if Tab.Selected and not Tab.Closing then
        Pen.Color := CloseCrossColorSelected
      else
      if Tab.Enabled then
        Pen.Color := CloseCrossColor
      else
        Pen.Color := CloseCrossColorDisabled;
      Pen.Width := 2;

      { Draw close cross }
      margin := Scale96(CROSS_MARGIN);
      Line(CloseR.Left + margin, CloseR.Top + margin, CloseR.Right - margin - 1, CloseR.Bottom - margin - 1);
      Line(CloseR.Left + margin, CloseR.Bottom - margin - 1, CloseR.Right - margin - 1, CloseR.Top + margin);
      {
      // remove intersection
      if Tab.Modified then
        FillRect(Rect(CloseR.Left + 5, CloseR.Top + 4, CloseR.Right - 5, CloseR.Bottom - 4));
      }
      R.Right := CloseR.Left - Scale96(TEXT_MARGIN_RIGHT);
    end;

    { Draw image from image list }
    if (Tab.ImageIndex <> -1) and (Tab.GetImages <> nil) then
    begin
      imgsize := GetRealImageSize(Tab);
      x := R.Left;
      y := (R.Top + R.Bottom - imgSize.CY) div 2;
     {$IF LCL_FullVersion >= 1090000}
      tabBar := GetTabBar(Tab);
      f := tabBar.GetCanvasScalefactor;
      ppi := GetPixelsPerInch;
      if Tab.GetImages <> nil then
        imageRes := Tab.GetImages.ResolutionForPPI[tabBar.ImagesWidth, ppi, f];
      imageRes.Draw(Canvas, x, y, Tab.ImageIndex, tab.Enabled);
     {$ELSE}
       Tab.GetImages.Draw(Canvas, x, y, Tab.ImageIndex, Tab.Enabled);
      {$ENDIF}
      Inc(R.Left, imgSize.CX + Scale96(TEXT_MARGIN_LEFT));
    end;

    if Tab.Enabled then
    begin
      if Tab.Selected then
        Font.Assign(Self.SelectedFont)
      else
        Font.Assign(Self.Font);
    end
    else
      Font.Assign(Self.DisabledFont);

    Brush.Style := bsClear;
    ts := TextStyle;
    ts.EndEllipsis := true;
    ts.Clipping := true;

    TextRect(R, R.Left, (R.Top + R.Bottom - TextHeight('Tg')) div 2, Tab.Caption, ts);
  end;
end;

function TJvModernTabBarPainter.GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem;
  ATabRect: TRect): TRect;
var
  btnSize: TSize;
begin
  btnSize := Size(Scale96(CLOSE_BUTTON_SIZE), Scale96(CLOSE_BUTTON_SIZE));
  Result.Right := ATabRect.Right - Scale96(RIGHT_MARGIN);
  Result.Left := Result.Right - btnSize.CX;
  Result.Top := (ATabRect.Top + ATabRect.Bottom - btnSize.CY) div 2;
  Result.Bottom := Result.Top + btnSize.CY;
end;

function TJvModernTabBarPainter.GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer;
begin
  Result := 1;
end;

function TJvModernTabBarPainter.GetPixelsPerInch: Integer;
begin
  Result := Font.PixelsPerInch;
end;

function TJvModernTabBarPainter.GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize;
var
  w, h: Integer;
  imgSize: TSize;
begin
  if Tab.Enabled then
  begin
    if Tab.Selected then
      Canvas.Font.Assign(SelectedFont)
    else
      Canvas.Font.Assign(Font)
  end
  else
    Canvas.Font.Assign(DisabledFont);

  // Measure text
  if Tab.Caption = '' then
    Result := Size(0, Canvas.TextHeight('Tg'))
  else
    Result := Canvas.TextExtent(Tab.Caption);
  inc(Result.CX, Scale96(LEFT_MARGIN) + Scale96(RIGHT_MARGIN));

  // Extend width by close button
  if Tab.TabBar.CloseButton then begin
    w := Scale96(CLOSE_BUTTON_SIZE);
    inc(Result.CX, w + Scale96(TEXT_MARGIN_RIGHT));
    h := w;
    if Result.CY < h then
      Result.CY := h;
  end;

  // Extend width and height by image
  if (Tab.ImageIndex <> -1) and (Tab.GetImages <> nil) then begin
    imgSize := GetRealImageSize(Tab);
    inc(Result.CX, imgSize.CX + Scale96(TEXT_MARGIN_LEFT));
    if Result.CY < imgSize.CY then
      Result.CY := imgSize.CY;
  end;
  inc(Result.CY, Scale96(TOP_MARGIN) + Scale96(BOTTOM_MARGIN));

  // Override width if TabWidth/TabHeight is fixed.
  if TabWidth > 0 then
    Result.CX := TabWidth;
  if TabHeight > 0 then
    Result.CY := TabHeight;
end;

function TJvModernTabBarPainter.Options: TJvTabBarPainterOptions;
begin
  Result := [poPaintsHotTab];
end;

procedure TJvModernTabBarPainter.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TJvModernTabBarPainter.SetBorderColor(const Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetControlDivideColor(const Value: TColor);
begin
  if Value <> FControlDivideColor then
  begin
    FControlDivideColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetModifiedCrossColor(const Value: TColor);
begin
  if Value <> FModifiedCrossColor then
  begin
    FModifiedCrossColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetTabColor(const Value: TColor);
begin
  if Value <> FTabColor then
  begin
    FTabColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseColor(const Value: TColor);
begin
  if Value <> FCloseColor then
  begin
    FCloseColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseColorSelected(const Value: TColor);
begin
  if Value <> FCloseColorSelected then
  begin
    FCloseColorSelected := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseCrossColor(const Value: TColor);
begin
  if Value <> FCloseCrossColor then
  begin
    FCloseCrossColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseCrossColorDisabled(const Value: TColor);
begin
  if Value <> FCloseCrossColorDisabled then
  begin
    FCloseCrossColorDisabled := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseCrossColorSelected(const Value: TColor);
begin
  if Value <> FCloseCrossColorSelected then
  begin
    FCloseCrossColorSelected := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseRectColor(const Value: TColor);
begin
  if Value <> FCloseRectColor then
  begin
    FCloseRectColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseRectColorDisabled(const Value: TColor);
begin
  if Value <> FCloseRectColorDisabled then
  begin
    FCloseRectColorDisabled := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetDividerColor(const Value: TColor);
begin
  if Value <> FDividerColor then
  begin
    FDividerColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetTabHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FTabHeight then
  begin
    FTabHeight := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetTabWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FTabWidth then
  begin
    FTabWidth := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetFont(const Value: TFont);
begin
  if Value <> FFont then
    FFont.Assign(Value);
end;

procedure TJvModernTabBarPainter.SetDisabledFont(const Value: TFont);
begin
  if Value <> FDisabledFont then
    FDisabledFont.Assign(Value);
end;

procedure TJvModernTabBarPainter.SetSelectedFont(const Value: TFont);
begin
  if Value <> FSelectedFont then
    FSelectedFont.Assign(Value);
end;


end.
