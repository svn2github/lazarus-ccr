{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOLBar.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Outlook style control. Simpler than TJvLookout)
   Hierarchy:
    TJvCustomOutlookBar
      Pages: TJvOutlookBarPages
        Page: TJvOutlookBarPage
          Buttons: TJvOutlookBarButtons
            Button: TJvOutlookBarButton

Known Issues:
  VISTA/THEMING CHANGES: WARREN POSTMA, NOV 2007 :
                  Vista paint fix, and support for completely user decided color
                  schemes, such as white on black, for  low-visibility-users
                  (high contrast black on white) support.
                  Outlook bar buttons now have color properties (instead of
                  assuming we will use the clBtnFace type system colors)
-----------------------------------------------------------------------------}
// $Id$

unit JvOutlookBar;

{$mode objfpc}{$H+}


interface

uses
  LCLType, LCLIntf, LMessages, Types, LCLVersion,
  SysUtils, Classes, ActnList,
  Buttons, Controls, Graphics, ImgList, Forms, StdCtrls, ExtCtrls, Themes,
  JvJCLUtils, JvComponent;

const
  CM_CAPTION_EDITING = CM_BASE + 756;
  CM_CAPTION_EDIT_ACCEPT = CM_CAPTION_EDITING + 1;
  CM_CAPTION_EDIT_CANCEL = CM_CAPTION_EDITING + 2;

type
  TJvBarButtonSize = (olbsLarge, olbsSmall);
  TJvCustomOutlookBar = class;
  TJvOutlookBarButton = class;

  TJvOutlookBarButtonActionLink = class(TActionLink)
  private
    FClient: TJvOutlookBarButton;
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    property Client: TJvOutlookBarButton read FClient write FClient;
  public
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
  end;

  TJvOutlookBarButtonActionLinkClass = class of TJvOutlookBarButtonActionLink;
  TJvOutlookBarButton = class(TCollectionItem)
  private
    FActionLink: TJvOutlookBarButtonActionLink;
    FImageIndex: TImageIndex;
    FCaption: TCaption;
    FTag: NativeInt;
    FDown: Boolean;
    FEnabled: Boolean;
    FAutoToggle: Boolean;
    FOnClick: TNotifyEvent;
    FLinkedObject: TObject;
    procedure SetCaption(const Value: TCaption);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetDown(const Value: Boolean);
    procedure Change;
    procedure SetEnabled(const Value: Boolean);
    procedure SetAction(Value: TBasicAction);
  protected
    function GetDisplayName: string; override;
    function GetActionLinkClass: TJvOutlookBarButtonActionLinkClass; dynamic;
    function GetAction: TBasicAction; virtual;
    procedure DoActionChange(Sender: TObject);
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
  public
    constructor Create(ACollection: Classes.TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Click; dynamic;
    procedure EditCaption;
    function GetOutlookBar: TJvCustomOutlookBar;

    // A property for user's usage, allowing to link an object to the button
    property LinkedObject: TObject read FLinkedObject write FLinkedObject;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Tag: NativeInt read FTag write FTag;
    property Down: Boolean read FDown write SetDown default False;
    property AutoToggle: Boolean read FAutoToggle write FAutoToggle;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TJvOutlookBarButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookBarButton;
    procedure SetItem(Index: Integer; const Value: TJvOutlookBarButton);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvOutlookBarButton;
    procedure Assign(Source: TPersistent); override;
    function IndexOf(AButton: TJvOutlookBarButton): Integer;
    function Insert(Index: Integer): TJvOutlookBarButton;
    property Items[Index: Integer]: TJvOutlookBarButton read GetItem write SetItem; default;
  end;

  TJvOutlookBarPage = class(TCollectionItem)
  private
    FPicture: TPicture;
    FCaption: TCaption;
    FColor: TColor;
    FButtonSize: TJvBarButtonSize;
    FParentButtonSize: Boolean;
    FParentFont: Boolean;
    FParentColor: Boolean;
    FTopButtonIndex: Integer;
    FButtons: TJvOutlookBarButtons;
    FFont: TFont;
    FDownFont: TFont;
    FImageIndex: TImageIndex;
    FAlignment: TAlignment;
    FEnabled: Boolean;
    FLinkedObject: TObject;
    procedure SetButtonSize(const Value: TJvBarButtonSize);
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
    procedure SetPicture(const Value: TPicture);
    procedure Change;
    procedure SetParentButtonSize(const Value: Boolean);
    procedure SetParentColor(const Value: Boolean);
    procedure SetTopButtonIndex(const Value: Integer);
    procedure SetButtons(const Value: TJvOutlookBarButtons);
    procedure SetParentFont(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetAlignment(const Value: TAlignment);
    procedure DoFontChange(Sender: TObject);
    procedure SetDownFont(const Value: TFont);
    function GetDownButton: TJvOutlookBarButton;
    function GetDownIndex: Integer;
    procedure SetDownButton(Value: TJvOutlookBarButton);
    procedure SetDownIndex(Value: Integer);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure DoPictureChange(Sender: TObject);
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: Classes.TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure EditCaption;
    function GetOutlookBar: TJvCustomOutlookBar;
    property DownButton: TJvOutlookBarButton read GetDownButton write SetDownButton;
    property DownIndex: Integer read GetDownIndex write SetDownIndex;
    // A property for user's usage, allowing to link an objet to the page.
    property LinkedObject: TObject read FLinkedObject write FLinkedObject;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Buttons: TJvOutlookBarButtons read FButtons write SetButtons;
    property ButtonSize: TJvBarButtonSize read FButtonSize write SetButtonSize;
    property Caption: TCaption read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clDefault;
    property DownFont: TFont read FDownFont write SetDownFont;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Font: TFont read FFont write SetFont;
    property Picture: TPicture read FPicture write SetPicture;
    property ParentButtonSize: Boolean read FParentButtonSize write SetParentButtonSize default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default False;
    property ParentColor: Boolean read FParentColor write SetParentColor;
    property TopButtonIndex: Integer read FTopButtonIndex write SetTopButtonIndex;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TJvOutlookBarPages = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookBarPage;
    procedure SetItem(Index: Integer; const Value: TJvOutlookBarPage);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvOutlookBarPage;
    function IndexOf(APage: TJvOutlookBarPage): Integer;
    function Insert(Index: Integer): TJvOutlookBarPage;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvOutlookBarPage read GetItem write SetItem; default;
  end;

  TOutlookBarPageChanging = procedure(Sender: TObject; Index: Integer; var AllowChange: Boolean) of object;
  TOutlookBarPageChange = procedure(Sender: TObject; Index: Integer) of object;
  TOutlookBarButtonClick = procedure(Sender: TObject; Index: Integer) of object;
  TOutlookBarEditCaption = procedure(Sender: TObject; var NewText: string;
    Index: Integer; var Allow: Boolean) of object;

  TJvOutlookBarCustomDrawStage = (odsBackground, odsPageButton, odsPage, odsButton, odsButtonFrame);
  TJvOutlookBarCustomDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
    AStage: TJvOutlookBarCustomDrawStage; AIndex: Integer; ADown, AInside: Boolean; var DefaultDraw: Boolean) of object;

  TJvPageBtnProps = class
  private
    FOwner: TJvCustomOutlookBar;
    FHighlight: TColor;
    FFace: TColor;
    FShadow: TColor;
    FDkShadow: TColor;
    FBorderWidth: Integer;
    procedure SetDkShadow(const Value: TColor);
    procedure SetFace(const Value: TColor);
    procedure SetHighlight(const Value: TColor);
    procedure SetShadow(const Value: TColor);
    procedure SetBorderWidth(const Value: INteger);
  public
    constructor Create(owner:TJvCustomOUtlookBar);
  public
    property Shadow:TColor      read FShadow write SetShadow        default clBtnShadow;
    property Highlight:TColor   read FHighlight write SetHighlight  default clBtnHighlight;
    property DkShadow:TColor    read FDkShadow write SetDkShadow    default cl3DDkShadow;
    property Face:TColor        read FFace write SetFace            default clBtnFace;
    property BorderWidth : Integer read FBorderWidth write SetBorderWidth default 1;
  end;

  TJvCustomOutlookBar = class(TJvCustomControl)
  private
    FPageBtnProps: TJvPageBtnProps;
    FUpButton: TSpeedButton;
    FDownButton: TSpeedButton;
    FPages: TJvOutlookBarPages;
    FLargeChangeLink: TChangeLink;
    FSmallChangeLink: TChangeLink;
    FPageChangeLink: TChangeLink;
    FActivePageIndex: Integer;
    FButtonSize: TJvBarButtonSize;
    FLargeImages: TCustomImageList;
    FLargeImagesWidth: Integer;
    FSmallImages: TCustomImageList;
    FSmallImagesWidth: Integer;
    FPageButtonHeight: Integer;
    FNextActivePage: Integer;
    FPressedPageBtn: Integer;
    FHotPageBtn: Integer;
    FThemedBackGround: Boolean;
    FThemed: Boolean;
    FOnPageChange: TOutlookBarPageChange;
    FOnPageChanging: TOutlookBarPageChanging;
    FButtonRect: TRect;
    FLastButtonIndex: Integer;
    FPressedButtonIndex: Integer;
    FOnButtonClick: TOutlookBarButtonClick;
    FPopUpObject: TObject;
    FEdit: TCustomEdit;
    FOnEditButton: TOutlookBarEditCaption;
    FOnEditPage: TOutlookBarEditCaption;
    FOnCustomDraw: TJvOutlookBarCustomDrawEvent;
    FPageImages: TCustomImageList;
    FPageImagesWidth: Integer;
    FDisabledFontColor1: TColor;
    FDisabledFontColor2: TColor;
    FWordWrap: Boolean;
    function GetActivePage: TJvOutlookBarPage;
    function GetActivePageIndex: Integer;
    function IsStoredPageButtonHeight: Boolean;
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetButtonSize(const Value: TJvBarButtonSize);
    procedure SetDisabledFontColor1(const Value: TColor);
    procedure SetDisabledFontColor2(const Value: TColor);
    procedure SetLargeImages(const Value: TCustomImageList);
    procedure SetPageButtonHeight(const Value: Integer);
    procedure SetPageImages(const Value: TCustomImageList);
    procedure SetPages(const Value: TJvOutlookBarPages);
    procedure SetSmallImages(const Value: TCustomImageList);
    procedure SetThemed(const Value: Boolean);
    procedure SetThemedBackground(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
    procedure CMCaptionEditAccept(var Msg: TLMessage); message CM_CAPTION_EDIT_ACCEPT;
    procedure CMCaptionEditCancel(var Msg: TLMessage); message CM_CAPTION_EDIT_CANCEL;
    procedure CMCaptionEditing(var Msg: TLMessage); message CM_CAPTION_EDITING;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    function CalcPageButtonHeight: Integer;
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure ColorChanged; override;
   {$IF LCL_FullVersion >= 1080000}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
   {$ENDIF}
    procedure DoButtonClick(Index: Integer); virtual;
    procedure DoButtonEdit(NewText: string; B: TJvOutlookBarButton);
    procedure DoChangeLinkChange(Sender: TObject);
    procedure DoContextPopup( MousePos: TPoint; var Handled: Boolean); override;
    function DoCustomDraw(ARect: TRect; Stage: TJvOutlookBarCustomDrawStage;
      Index: Integer; Down, Inside: Boolean): Boolean; virtual;
    function DoDrawBackGround: Boolean;
    function DoDrawButton(ARect: TRect; Index: Integer; Down, Inside: Boolean): Boolean;
    function DoDrawButtonFrame(ARect: TRect; Index: Integer; Down, Inside: Boolean): Boolean;
    function DoDrawPage(ARect: TRect; Index: Integer): Boolean;
    function DoDrawPageButton(ARect: TRect; Index: Integer; Down: Boolean): Boolean;
    procedure DoDwnClick(Sender: TObject);
    function DoPageChanging(Index: Integer): Boolean; virtual;
    procedure DoPageChange(Index: Integer); virtual;
    procedure DoPageEdit(NewText: string; P: TJvOutlookBarPage);
    procedure DoUpClick(Sender: TObject);
    (*
    {$IF LCL_FullVersion >= 1090000}
    function DoEraseBackground(ACanvas: TCanvas; Param: LPARAM): Boolean; override;
    {$ENDIF}
    *)
    procedure DrawArrowButtons(Index: Integer);
    procedure DrawBottomPages(StartIndex: Integer);
    procedure DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: Integer);
    procedure DrawButtons(Index: Integer);
    procedure DrawCurrentPage(PageIndex: Integer);
    procedure DrawPageButton(R: TRect; Index: Integer; Pressed: Boolean);
    function DrawPicture(R: TRect; Picture: TPicture): Boolean;
    function DrawTopPages: Integer;

    procedure FontChanged; override;
    class function GetControlClassDefaultSize: TSize; override;
    function GetButtonFrameRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetButtonHeight(PageIndex, ButtonIndex: Integer): Integer;
    function GetButtonRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetButtonTextRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetButtonTextSize(PageIndex, ButtonIndex: Integer): TSize;
    function GetButtonTopHeight(PageIndex, ButtonIndex: Integer): Integer;
    function GetPageButtonRect(Index: Integer): TRect;
    function GetPageTextRect(Index: Integer): TRect;
    function GetPageRect(Index: Integer): TRect;
    function GetRealImageSize(AImageList: TCustomImageList; AImagesWidth: Integer): TSize;
    function IsThemedStored: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure RedrawRect(R: TRect; Erase: Boolean = False);
    procedure Resize; override;
    property PopUpObject: TObject read FPopUpObject write FPopUpObject;
    property UpButton: TSpeedButton read FUpButton;
    property DownButton: TSpeedButton read FDownButton;
    property BorderStyle default bsSingle;
//    property Font;
    property Color default clBtnShadow;
    property Pages: TJvOutlookBarPages read FPages write SetPages;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property PageImages: TCustomImageList read FPageImages write SetPageImages;
    property ButtonSize: TJvBarButtonSize read FButtonSize write SetButtonSize default olbsLarge;
    property PageButtonHeight: Integer read FPageButtonHeight write SetPageButtonHeight stored IsStoredPageButtonHeight;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex default 0;
    property Themed: Boolean read FThemed write SetThemed stored IsThemedStored;
    property ThemedBackground: Boolean read FThemedBackGround write SetThemedBackground default True;
    property PageBtnProps: TJvPageBtnProps read FPageBtnProps;
    property DisabledFontColor1: TColor read FDisabledFontColor1 write SetDisabledFontColor1; //clWhite;
    property DisabledFontColor2: TColor read FDisabledFontColor2 write SetDisabledFontColor2; //clGrayText;
    property OnPageChanging: TOutlookBarPageChanging read FOnPageChanging write FOnPageChanging;
    property OnPageChange: TOutlookBarPageChange read FOnPageChange write FOnPageChange;
    property OnButtonClick: TOutlookBarButtonClick read FOnButtonClick write FOnButtonClick;
    property OnEditButton: TOutlookBarEditCaption read FOnEditButton write FOnEditButton;
    property OnEditPage: TOutlookBarEditCaption read FOnEditPage write FOnEditPage;
    property OnCustomDraw: TJvOutlookBarCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;

  {$IF LCL_FullVersion >= 1090000}
  private
    procedure SetLargeImagesWidth(const AValue: Integer);
    procedure SetPageImagesWidth(const AValue: Integer);
    procedure SetSmallImagesWidth(const AValue: Integer);
  protected
    property LargeImagesWidth: Integer read FLargeImagesWidth write SetLargeImagesWidth default 0;
    property SmallImagesWidth: Integer read FSmallImagesWidth write SetSmallImagesWidth default 0;
    property PageImagesWidth: Integer read FPageImagesWidth write SetPageImagesWidth default 0;
 {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitiateAction; override;
   {$IF LCL_FullVersion >= 1080000}
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
   {$ENDIF}
    function GetButtonAtPos(P: TPoint): TJvOutlookBarButton;
    function GetPageButtonAtPos(P: TPoint): TJvOutlookBarPage;
  public
    property ActivePage: TJvOutlookBarPage read GetActivePage;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  end;

  TJvOutlookBar = class(TJvCustomOutlookBar)
  public
    property DisabledFontColor1;
    property DisabledFontColor2;
    property PageBtnProps;
    property PopUpObject;

  published
    property Action;
    property ActivePageIndex;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property ButtonSize;
    property ChildSizing;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property LargeImages;
    property Pages;
    property PageButtonHeight;
    property PageImages;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SmallImages;
    property TabOrder;
    property TabStop;
    property Themed;
    property ThemedBackground;
    property Visible;
    property Width;
    property WordWrap;
    property OnButtonClick;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButton;
    property OnEditPage;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPageChange;
    property OnPageChanging;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
   {$IF LCL_FullVersion >= 1090000}
    property LargeImagesWidth;
    property SmallImagesWidth;
    property PageImagesWidth;
   {$ENDIF}
  end;


implementation

uses
  Math,
  JvThemes,
  JvConsts, JvJVCLUtils;

{$R ..\..\resource\jvoutlookbar.res}

type
  THackOutlookBar = class(TJvCustomOutlookBar);

const
  cTextMargins = 3;
  cMinTextWidth = 32;
  cButtonLeftOffset = 4;
  cButtonTopOffset = 2;
  cInitRepeatPause = 400;
  cRepeatPause = 100;

  UP_DOWN_DEFAULT_SIZE = 14;

  (*
{$IFDEF MSWINDOWS}
function JclCheckWinVersion(Major, Minor: Integer): Boolean;
begin
  Result := CheckWin32Version(Major, Minor);
end;
{$ENDIF}

function IsVista:Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := JclCheckWinVersion(6, 0);
  {$ELSE}
  Result := false;
  {$ENDIF}
end;   *)

function MethodsEqual(const Method1, Method2: TMethod): Boolean;
begin
  Result := (Method1.Code = Method2.Code) and (Method1.Data = Method2.Data);
end;

function GetUniquePageName(OLBar: TJvCustomOutlookBar): string;
const
  cPrefix = 'JvOutlookBarPage';
  cTemplate = cPrefix + '%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to THackOutlookBar(OLBar).Pages.Count - 1 do
      if AnsiSameText(THackOutlookBar(OLBar).Pages[I].Caption, S) then
        Exit;
    Result := True;
  end;

begin
  Result := cPrefix;
  if OLBar <> nil then
    for K := 1 to MaxInt - 1 do
    begin
      Tmp := Format(cTemplate, [K]);
      if IsUnique(Tmp) then
      begin
        Result := Tmp;
        Exit;
      end;
    end;
end;

function GetUniqueButtonName(OLBar: TJvCustomOutlookBar): string;
const
  cPrefix = 'JvOutlookBarButton';
  cTemplate = cPrefix + '%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;
    for I := 0 to THackOutlookBar(OLBar).Pages.Count - 1 do
      for J := 0 to THackOutlookBar(OLBar).Pages[I].Buttons.Count - 1 do
        if AnsiSameText(THackOutlookBar(OLBar).Pages[I].Buttons[J].Caption, S) then
          Exit;
    Result := True;
  end;

begin
  Result := cPrefix;
  if OLBar <> nil then
    for K := 1 to MaxInt - 1 do
    begin
      Tmp := Format(cTemplate, [K]);
      if IsUnique(Tmp) then
      begin
        Result := Tmp;
        Exit;
      end;
    end;
end;

function HighDpi_Suffix: String;
begin
  Result := '';
  if Screen.SystemFont.PixelsPerInch >= 168 then
    Result := Result + '_200'
  else
  if Screen.SystemFont.PixelsPerInch >= 120 then
    Result := Result + '_150';
end;


//=== { TJvOutlookBarEdit } ==================================================

type
  TJvOutlookBarEdit = class(TCustomEdit)
  private
    FCanvas: TControlCanvas;
    procedure WMNCPaint(var Msg: TLMessage); message LM_NCPAINT;
    procedure EditAccept;
    procedure EditCancel;
    function GetCanvas: TCanvas;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor CreateInternal(AOwner: TComponent; AParent: TWinControl; AObject: TObject);
    destructor Destroy; override;
    procedure ShowEdit(const AText: string; R: TRect);
    property Canvas: TCanvas read GetCanvas;
  end;

constructor TJvOutlookBarEdit.CreateInternal(AOwner: TComponent;
  AParent: TWinControl; AObject: TObject);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  AutoSize := True;
  Visible := False;
  Parent := AParent;
  BorderStyle := bsNone;
  ParentFont := False;
  Tag := NativeInt(AObject);
end;

destructor TJvOutlookBarEdit.Destroy;
begin
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvOutlookBarEdit.EditAccept;
begin
  Parent.Perform(CM_CAPTION_EDIT_ACCEPT, WPARAM(Self), LPARAM(Tag));
  Hide;
end;

procedure TJvOutlookBarEdit.EditCancel;
begin
  Parent.Perform(CM_CAPTION_EDIT_CANCEL, WPARAM(Self), LPARAM(Tag));
  Hide;
end;

function TJvOutlookBarEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvOutlookBarEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        Key := 0;
        EditAccept;
        if Handle = GetCapture then
          ReleaseCapture;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
      end;
    VK_ESCAPE:
      begin
        Key := 0;
        if Handle = GetCapture then
          ReleaseCapture;
        EditCancel;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvOutlookBarEdit.KeyPress(var Key: Char);
begin
  if Key = Cr then
    Key := #0; // remove beep
  inherited KeyPress(Key);
end;

procedure TJvOutlookBarEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not PtInRect(ClientRect, Point(X, Y)) or ((Button = mbRight) and Visible) then
  begin
    if Handle = GetCapture then
      ReleaseCapture;
    EditCancel;
//    Screen.Cursor := crDefault;
//    FEdit.Hide;
//    FEdit.Free;
//    FEdit := nil;
  end
  else
  begin
    ReleaseCapture;
//    Screen.Cursor := crIBeam;
    SetCapture(Handle);
  end;
end;

procedure TJvOutlookBarEdit.ShowEdit(const AText: string; R: TRect);
begin
  Hide;
  Text := AText;
  SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Show;
  SetCapture(Handle);
  SelStart := 0;
  SelLength := Length(Text);
  SetFocus;
end;


procedure TJvOutlookBarEdit.WMNCPaint(var Msg: TLMessage);
begin
  if csDestroying in ComponentState then
    Exit;
  GetCanvas; // make Delphi 5 compiler happy // andreas
  inherited;
(*
  DC := GetWindowDC(Handle);
  try
    FCanvas.Handle := DC;
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);

    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    FCanvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,FCanvas.Brush.Handle);
    InflateRect(RW,-1,-1);

{    FCanvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,FCanvas.Brush.Handle);
    InflateRect(RW,-1,-1);

    FCanvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,FCanvas.Brush.Handle);
    InflateRect(RW,-1,-1); }

    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  finally
    ReleaseDC(Handle, DC);
  end;
  *)
end;

              (*
//=== { TJvRepeatButton } ====================================================

type
  // auto-repeating button using a timer (stolen from Borland's Spin.pas sample component)
  TJvRepeatButton = class(TSpeedButton) //TJvExSpeedButton)
  private
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure VisibleChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
  end;

procedure TJvRepeatButton.VisibleChanged;
begin
  inherited VisibleChanged;
  if not Visible then
    FreeAndNil(FRepeatTimer);
end;

destructor TJvRepeatButton.Destroy;
begin
  inherited Destroy;
end;

procedure TJvRepeatButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FRepeatTimer = nil then
    FRepeatTimer := TTimer.Create(Self);
  FRepeatTimer.OnTimer := @TimerExpired;
  FRepeatTimer.Interval := cInitRepeatPause;
  FRepeatTimer.Enabled := True;
end;

procedure TJvRepeatButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FreeAndNil(FRepeatTimer);
end;

procedure TJvRepeatButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := cRepeatPause;
  if (FState = bsDown) and MouseCapture then
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
end;
*)

//=== { TJvOutlookBarButtonActionLink } ======================================

procedure TJvOutlookBarButtonActionLink.AssignClient(AClient: TObject);
begin
  Client := AClient as TJvOutlookBarButton;
end;

function TJvOutlookBarButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (Client.Caption = (Action as TCustomAction).Caption);
end;

function TJvOutlookBarButtonActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (Client.Enabled = (Action as TCustomAction).Enabled);
end;

function TJvOutlookBarButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (Client.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TJvOutlookBarButtonActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    MethodsEqual(TMethod(Client.OnClick), TMethod(Action.OnExecute));
end;

procedure TJvOutlookBarButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    Client.Caption := Value;
end;

procedure TJvOutlookBarButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    Client.Enabled := Value;
end;

procedure TJvOutlookBarButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    Client.ImageIndex := Value;
end;

procedure TJvOutlookBarButtonActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    Client.OnClick := Value;
end;

//=== { TJvOutlookBarButton } ================================================

constructor TJvOutlookBarButton.Create(ACollection: Classes.TCollection);
begin
  inherited Create(ACollection);
  FEnabled := True;
end;

destructor TJvOutlookBarButton.Destroy;
var
  OBPage: TJvOutlookBarPage;
  OB: TJvOutlookBar;
begin
  OBPage := TJvOutlookBarPage(TJvOutlookBarButtons(Self.Collection).Owner);
  OB := TJvOutlookBar(TJvOutlookBarPages(OBPage.Collection).Owner);
  if Assigned(OB) then
  begin
    if OB.FPressedButtonIndex = Index then
      OB.FPressedButtonIndex := -1;
    if OB.FLastButtonIndex = Index then
      OB.FLastButtonIndex := -1;
    OB.Invalidate;
  end;

  // Mantis 3688
  FActionLink.Free;
  inherited Destroy;
end;

procedure TJvOutlookBarButton.Assign(Source: TPersistent);
begin
  if Source is TJvOutlookBarButton then
  begin
    Caption := TJvOutlookBarButton(Source).Caption;
    ImageIndex := TJvOutlookBarButton(Source).ImageIndex;
    Down := TJvOutlookBarButton(Source).Down;
    AutoToggle := TJvOutlookBarButton(Source).AutoToggle;
    Tag := TJvOutlookBarButton(Source).Tag;
    Enabled := TJvOutlookBarButton(Source).Enabled;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvOutlookBarButton.Change;
begin
  if (Collection <> nil) and (TJvOutlookBarButtons(Collection).Owner <> nil) and
    (TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection <> nil) and
    (TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection).Owner) <> nil)
  then
    TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection).Owner).Invalidate;
end;

procedure TJvOutlookBarButton.EditCaption;
begin
  SendMessage(TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection).Owner).Handle,
    CM_CAPTION_EDITING, WPARAM(Self), 0);
end;

function TJvOutlookBarButton.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TJvOutlookBarButton.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookBarButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarButton.SetDown(const Value: Boolean);
var
  I: Integer;
begin
  if Value <> FDown then
  begin
    FDown := Value;
    if FDown then
      for I := 0 to TJvOutlookBarButtons(Collection).Count - 1 do
        if TJvOutlookBarButtons(Collection).Items[I] <> Self then
          TJvOutlookBarButtons(Collection).Items[I].Down := False;
    Change;
  end;
end;

procedure TJvOutlookBarButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Change;
  end;
end;

procedure TJvOutlookBarButton.Click;
begin
  // Mantis 3689
  { Call OnClick if assigned and not equal to associated action's OnExecute.
    If associated action's OnExecute assigned then call it, otherwise, call
    OnClick. }
  if Assigned(FOnClick) and Assigned(Action) and (@FOnClick <> @Action.OnExecute) then
    FOnClick(Self)
  else
  if (GetOutlookBar <> nil) and (FActionLink <> nil) and not (csDesigning in GetOutlookBar.ComponentState) then
    FActionLink.Execute(GetOutlookBar)
  else
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TJvOutlookBarButton.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

function TJvOutlookBarButton.GetActionLinkClass: TJvOutlookBarButtonActionLinkClass;
begin
  Result := TJvOutlookBarButtonActionLink;
end;

procedure TJvOutlookBarButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or Self.Enabled then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

procedure TJvOutlookBarButton.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChange(Sender, False);
end;

procedure TJvOutlookBarButton.SetAction(Value: TBasicAction);
begin
  if (FActionLink <> nil) and (FActionLink.Action <> nil) then
    FActionLink.Action.RemoveFreeNotification(GetOutlookBar);
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := @DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    if GetOutlookBar <> nil then
      Value.FreeNotification(GetOutlookBar); // delegates notification to owner!
  end;
end;

function TJvOutlookBarButton.GetOutlookBar: TJvCustomOutlookBar;
begin
  if TJvOutlookBarButtons(Collection).Owner is TJvOutlookBarPage then
    Result := TJvOutlookBarPage(TJvOutlookBarButtons(Collection).Owner).GetOutlookBar
  else
    Result := nil;
end;

//=== { TJvOutlookBarButtons } ===============================================

constructor TJvOutlookBarButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvOutlookBarButton);
end;

function TJvOutlookBarButtons.Add: TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Add);
  Result.Caption := GetUniqueButtonName(Result.GetOutlookBar);
  Result.DisplayName := Result.Caption;
end;

procedure TJvOutlookBarButtons.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarButtons then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvOutlookBarButtons(Source).Count - 1 do
        Add.Assign(TJvOutlookBarButtons(Source)[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvOutlookBarButtons.GetItem(Index: Integer): TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Items[Index]);
end;

function TJvOutlookBarButtons.IndexOf(AButton: TJvOutlookBarButton): Integer;
begin
  for Result := 0 to Count-1 do
    if AButton = GetItem(Result) then exit;
  Result := -1;
end;

function TJvOutlookBarButtons.Insert(Index: Integer): TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Insert(Index));
end;

procedure TJvOutlookBarButtons.SetItem(Index: Integer;
  const Value: TJvOutlookBarButton);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarButtons.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Owner <> nil then
    TJvOutlookBarPage(Owner).Changed(False);
end;


//=== { TJvOutlookBarPage } ==================================================

constructor TJvOutlookBarPage.Create(ACollection: Classes.TCollection);
begin
  inherited Create(ACollection);
  FFont := TFont.Create;
  FFont.OnChange := @DoFontChange;
  FDownFont := TFont.Create;
  FDownFont.OnChange := @DoFontChange;
  FParentColor := True;
  FPicture := TPicture.Create;
  FPicture.OnChange := @DoPictureChange;
  FAlignment := taCenter;
  FImageIndex := -1;
  FEnabled := True;
  FButtons := TJvOutlookBarButtons.Create(Self);
  if (ACollection <> nil) and (TJvOutlookBarPages(ACollection).Owner <> nil) then
  begin
    FButtonSize := TJvCustomOutlookBar(TJvOutlookBarPages(ACollection).Owner).ButtonSize;
    Font := TJvCustomOutlookBar(TJvOutlookBarPages(ACollection).Owner).Font;
    DownFont := Font;
  end else
  begin
    FButtonSize := olbsLarge;
  end;
  FColor := clDefault;
  Font.Color := clWhite;
  FParentButtonSize := True;
end;

destructor TJvOutlookBarPage.Destroy;
begin
  FButtons.Free;
  FPicture.Free;
  FFont.Free;
  FDownFont.Free;
  inherited Destroy;
end;

procedure TJvOutlookBarPage.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarPage then
  begin
    Caption := TJvOutlookBarPage(Source).Caption;
    Picture := TJvOutlookBarPage(Source).Picture;
    Color := TJvOutlookBarPage(Source).Color;
    DownFont.Assign(TJvOutlookBarPage(Source).DownFont);
    ButtonSize := TJvOutlookBarPage(Source).ButtonSize;
    ParentButtonSize := TJvOutlookBarPage(Source).ParentButtonSize;
    ParentColor := TJvOutlookBarPage(Source).ParentColor;
    Enabled := TJvOutlookBarPage(Source).Enabled;
    Buttons.Clear;
    for I := 0 to TJvOutlookBarPage(Source).Buttons.Count - 1 do
      Buttons.Add.Assign(TJvOutlookBarPage(Source).Buttons[I]);
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvOutlookBarPage.Change;
begin
  if (Collection <> nil) and (TJvOutlookBarPages(Collection).UpdateCount = 0) then
    TJvOutlookBarPages(Collection).Update(Self);
end;

procedure TJvOutlookBarPage.SetTopButtonIndex(const Value: Integer);
begin
  if (FTopButtonIndex <> Value) and (Value >= 0) and (Value < Buttons.Count) then
  begin
    FTopButtonIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtons(const Value: TJvOutlookBarButtons);
begin
  FButtons.Assign(Value);
  Change;
end;

procedure TJvOutlookBarPage.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtonSize(const Value: TJvBarButtonSize);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    if not (csReading in TComponent(TJvOutlookBarPages(Collection).Owner).ComponentState) then
      FParentButtonSize := False;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FParentColor := False;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FParentFont := False;
end;

procedure TJvOutlookBarPage.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvOutlookBarPage.SetParentButtonSize(const Value: Boolean);
begin
  if FParentButtonSize <> Value then
  begin
    FParentButtonSize := Value;
    if Value then
    begin
      FButtonSize := (TJvOutlookBarPages(Collection).Owner as TJvCustomOutlookBar).ButtonSize;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then
    begin
      FColor := (TJvOutlookBarPages(Collection).Owner as TJvCustomOutlookBar).Color;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentFont(const Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    if Value then
      Font := (TJvOutlookBarPages(Collection).Owner as TJvCustomOutlookBar).Font;
    FParentFont := Value;
  end;
end;

procedure TJvOutlookBarPage.EditCaption;
begin
  SendMessage(TCustomControl(TJvOutlookBarPages(Collection).Owner).Handle, CM_CAPTION_EDITING, WPARAM(Self), 1);
end;

function TJvOutlookBarPage.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

function TJvOutlookBarPage.GetOutlookBar: TJvCustomOutlookBar;
begin
  if TJvOutlookBarPages(Collection).Owner is TJvCustomOutlookBar then
    Result := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).Owner)
  else
    Result := nil;
end;

procedure TJvOutlookBarPage.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetDownFont(const Value: TFont);
begin
  if Value <> FDownFont then
    FDownFont.Assign(Value);
end;

procedure TJvOutlookBarPage.DoFontChange(Sender: TObject);
begin
  Change;
  if Sender <> FDownFont then
    FParentFont := False;
end;

function TJvOutlookBarPage.GetDownButton: TJvOutlookBarButton;
var
  lIndex: Integer;
begin
  lIndex := DownIndex;
  if lIndex <> -1 then
    Result := Buttons[lIndex]
  else
    Result := nil;
end;

procedure TJvOutlookBarPage.SetDownButton(Value: TJvOutlookBarButton);
begin
  if Value = nil then
    DownIndex := -1
  else
    DownIndex := Value.Index;
end;

function TJvOutlookBarPage.GetDownIndex: Integer;
begin
  for Result := 0 to Buttons.Count - 1 do
    if Buttons[Result].Down then
      Exit;
  Result := -1;
end;

procedure TJvOutlookBarPage.SetDownIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < Buttons.Count) then
    Buttons[Value].Down := True;
end;


//=== { TJvOutlookBarPages } =================================================

constructor TJvOutlookBarPages.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvOutlookBarPage);
end;

function TJvOutlookBarPages.Add: TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Add);
  Result.Caption := GetUniquePageName(Result.GetOutlookBar);
  Result.DisplayName := Result.Caption;
end;

procedure TJvOutlookBarPages.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarPages then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvOutlookBarPages(Source).Count - 1 do
        Add.Assign(TJvOutlookBarPages(Source)[I]);
    finally
      EndUpdate
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvOutlookBarPages.GetItem(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Items[Index]);
end;

function TJvOutlookBarPages.IndexOf(APage: TJvOutlookBarPage): Integer;
begin
  for Result := 0 to Count-1 do
    if APage = GetItem(Result) then exit;
  Result := -1;
end;

function TJvOutlookBarPages.Insert(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Insert(Index));
end;

procedure TJvOutlookBarPages.SetItem(Index: Integer;
  const Value: TJvOutlookBarPage);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarPages.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Owner <> nil then
    TJvCustomOutlookBar(Owner).Repaint;
end;

                                   (*
//=== { TJvThemedTopBottomButton } ===========================================

type
  TJvThemedTopBottomButton = class(TJvRepeatButton)
  private
    FIsUpBtn: Boolean;
  protected
    procedure Paint; override;
//    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message LM_ERASEBKGND;
  end;

procedure TJvThemedTopBottomButton.Paint;
var
  Button: TThemedScrollBar;
  Details: TThemedElementDetails;
begin
  if csDestroying in ComponentState then
    Exit;

  if StyleServices.Enabled and (not Flat) then
  begin
    if not Enabled then
      Button := tsArrowBtnUpDisabled
    else
    if FState in [bsDown, bsExclusive] then
      Button := tsArrowBtnUpPressed
    else
    if MouseInControl then
      Button := tsArrowBtnUpHot
    else
      Button := tsArrowBtnUpNormal;

    if not FIsUpBtn then
      Button := TThemedScrollBar(Ord(tsArrowBtnDownNormal) + Ord(Button) - Ord(tsArrowBtnUpNormal));

    Details := StyleServices.GetElementDetails(Button);

    StyleServices.DrawElement(Canvas.Handle, Details, ClientRect, nil); //@ClipRect);
  end
  else
    inherited Paint;
end;
{
procedure TJvThemedTopBottomButton.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
  Msg.Result := 1;
end;
}
*)

//=== { TJvCustomOutlookBar } ================================================

constructor TJvCustomOutlookBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWordWrap := True;
  FPageBtnProps := TJvPageBtnProps.Create(self);
  DoubleBuffered := True;
  FThemed := StyleServices.Enabled;
  FThemedBackground := true;

  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
  FDisabledFontColor1 := clWhite;
  FDisabledFontColor2 := clGrayText;

  FUpButton := TSpeedButton.Create(self); //TJvRepeatButton.Create(Self);
  with FUpButton do
  begin
    Parent := Self;
    Visible := False;
    Transparent := False;
    OnClick := @DoUpClick;
    if csDesigning in ComponentState then
      Top := -1000;
  end;

  FDownButton := TSpeedButton.Create(Self); //TJvRepeatButton.Create(Self);
  with FDownButton do
  begin
    Parent := Self;
    Visible := False;
    Transparent := False;
    OnClick := @DoDwnClick;
    if csDesigning in ComponentState then
      Top := -1000;
  end;

  FPages := TJvOutlookBarPages.Create(Self);
  FLargeChangeLink := TChangeLink.Create;
  FLargeChangeLink.OnChange := @DoChangeLinkChange;
  FSmallChangeLink := TChangeLink.Create;
  FSmallChangeLink.OnChange := @DoChangeLinkChange;
  FPageChangeLink := TChangeLink.Create;
  FPageChangeLink.OnChange := @DoChangeLinkChange;
  FEdit := TJvOutlookBarEdit.CreateInternal(Self, Self, nil);
  FEdit.Top := -1000;

  // set up defaults
  Color := clBtnShadow;
  BorderStyle := bsSingle;

  FButtonSize := olbsLarge;
  FPageButtonHeight := 0;
  FPressedPageBtn := -1;
  FNextActivePage := -1;
  FLastButtonIndex := -1;
  FPressedButtonIndex := -1;
  FHotPageBtn := -1;
  FThemedBackGround := True;
  ActivePageIndex := 0;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TJvCustomOutlookBar.Destroy;
begin
//  FEdit.Free;
  FLargeChangeLink.Free;
  FSmallChangeLink.Free;
  FPageChangeLink.Free;
  FPages.Free;
  FPageBtnProps.Free;
  inherited Destroy;
end;

function TJvCustomOutlookBar.CalcPageButtonHeight: Integer;
var
  OldFont: HFONT;
begin
  OldFont := SelectObject(Canvas.Handle, Canvas.Font.Handle);
  try
    Canvas.Font.Assign(Font);
    if Canvas.Font.IsDefault then
      Canvas.Font := Screen.SystemFont;
    if FPageButtonHeight = 0 then
      Result := Canvas.TextHeight('Tg') + Scale96ToForm(4)
    else
      Result := FPageButtonHeight;
  finally
    SelectObject(Canvas.Handle, OldFont);
  end;
end;

procedure TJvCustomOutlookBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited;
  PreferredWidth := 100;
  PreferredHeight := 220;
  if (FPageButtonHeight = 0) and HandleAllocated then
    FPageButtonHeight := Canvas.TextHeight('Tg') + 4;
end;

procedure TJvCustomOutlookBar.DoDwnClick(Sender: TObject);
begin
  if FDownButton.Visible then
    with Pages[ActivePageIndex] do
      if TopButtonIndex < Buttons.Count then
        TopButtonIndex := TopButtonIndex + 1;
end;

procedure TJvCustomOutlookBar.DoUpClick(Sender: TObject);
begin
  if FUpButton.Visible then
    with Pages[ActivePageIndex] do
      if TopButtonIndex > 0 then
        TopButtonIndex := TopButtonIndex - 1;
end;

procedure TJvCustomOutlookBar.DoChangeLinkChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomOutlookBar.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I, J: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FLargeImages then
      LargeImages := nil
    else
    if AComponent = FSmallImages then
      SmallImages := nil
    else
    if AComponent = FPageImages then
      PageImages := nil;
    if (AComponent is TBasicAction) and not (csDestroying in ComponentState) then
    begin
      for I := 0 to Pages.Count - 1 do
        for J := 0 to Pages[I].Buttons.Count - 1 do
          if AComponent = Pages[I].Buttons[J].Action then
            Pages[I].Buttons[J].Action := nil;
    end;
  end;
end;

{ Warren modified this so you can have some weird page button colors that aren't standard windows colors }
procedure TJvCustomOutlookBar.DrawPageButton(R: TRect; Index: Integer; Pressed: Boolean);
var
  SavedDC, ATop: Integer;
  SavedColor: TColor;
  Flags: Cardinal;
  HasImage: Boolean;
  Details: TThemedElementDetails;
  margin: Integer;
  {$IF LCL_FullVersion >= 1090000}
  pageImageRes: TScaledImageListResolution;
  f: Double;
  ppi: Integer;
  {$ENDIF}
begin
  Assert(Assigned(FPageBtnProps));
  ATop := R.Top + 1;

  // Background and frame
  if Themed then begin
    if Pressed then
      Details := StyleServices.GetElementDetails(tbPushButtonPressed)
    else
    if Index = FHotPageBtn then
      Details := StyleServices.GetElementDetails(tbPushButtonHot)
    else
      Details := StyleServices.GetElementDetails(tbPushButtonNormal);
    InflateRect(R, 1, 1);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end else
  if Pressed then
  begin
    if BorderStyle = bsNone then
      Frame3D(Canvas, R, FPageBtnProps.Shadow, FPageBtnProps.Highlight,   FPageBtnProps.BorderWidth)
    else
    begin
      Frame3D(Canvas, R, FPageBtnProps.DkShadow, FPageBtnProps.Highlight, FPageBtnProps.BorderWidth);
      Frame3D(Canvas, R, FPageBtnProps.Shadow,   FPageBtnProps.Face,      FPageBtnProps.BorderWidth);
    end;
  end
  else
  begin
    if BorderStyle = bsNone then
      Frame3D(Canvas, R, FPageBtnProps.Highlight, FPageBtnProps.Shadow,    FPageBtnProps.BorderWidth)
    else
    begin
      Frame3D(Canvas, R, FPageBtnProps.Highlight, FPageBtnProps.DkShadow,  FPageBtnProps.BorderWidth);
      Frame3D(Canvas, R, FPageBtnProps.Face,      FPageBtnProps.Shadow,    FPageBtnProps.BorderWidth);
    end;
  end;

  // Icon
  Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  HasImage := Assigned(PageImages) and (Pages[Index].ImageIndex >= 0) and (Pages[Index].ImageIndex < PageImages.Count);
  SavedDC := SaveDC(Canvas.Handle);
  try
    margin := Scale96ToForm(4);
    if HasImage then begin
     {$IF LCL_FullVersion >= 1090000}
      f := GetCanvasScalefactor;
      ppi := Font.PixelsPerInch;
      if FPageImages <> nil then
        pageImageRes := FPageImages.ResolutionForPPI[FPageImagesWidth, ppi, f];
      pageImageRes.Draw(Canvas, margin, ATop, Pages[Index].ImageIndex, Pages[Index].Enabled);
     {$ELSE}
      PageImages.Draw(Canvas, margin, ATop, Pages[Index].ImageIndex, Pages[Index].Enabled);
     {$ENDIF}
    end;
    case Pages[Index].Alignment of
      taLeftJustify:
        begin
          if HasImage then
            Inc(R.Left, PageImages.Width + 2*margin)
          else
            Inc(R.Left, margin);
          Flags := DT_LEFT or DT_VCENTER or DT_SINGLELINE;
        end;
      taCenter:
        if HasImage then
          Inc(R.Left, PageImages.Width + margin);
      taRightJustify:
        begin
          if HasImage then
            Inc(R.Left, PageImages.Width + 2*margin);
          Dec(R.Right, margin);
          Flags := DT_RIGHT or DT_VCENTER or DT_SINGLELINE;
        end;
    end;
  finally
    RestoreDC(Canvas.Handle, SavedDC);
  end;

  // Text
  SetBkMode(Canvas.Handle, TRANSPARENT);
  OffsetRect(R, 0, -1);
  SavedColor := Canvas.Font.Color;
  try
    if Themed then begin
      if not Pages[Index].Enabled then begin
        OffsetRect(R, 1, 1);
        Details := StyleServices.GetElementDetails(tbPushButtonPressed)
      end;
      StyleServices.DrawText(Canvas, Details, Pages[Index].Caption, R, Flags or DT_END_ELLIPSIS, 0);
    end else begin
      if not Pages[Index].Enabled then
      begin
        OffsetRect(R, 1, 1);
        Canvas.Font.Color := FDisabledFontColor1; //clWhite;
        DrawText(Canvas, Pages[Index].Caption, -1, R, Flags or DT_END_ELLIPSIS);
        OffsetRect(R, -1, -1);
        Canvas.Font.Color := FDisabledFontColor2; //clGrayText;
      end;
      DrawText(Canvas, Pages[Index].Caption, -1, R, Flags or DT_END_ELLIPSIS);
    end;
  finally
    Canvas.Font.Color := SavedColor;
  end;
end;

function TJvCustomOutlookBar.DrawTopPages: Integer;
var
  R: TRect;
  I: Integer;
  ToolBar: TThemedToolBar;
  Details: TThemedElementDetails;
  ClipRect: TRect;
  pgBtnHeight: Integer;
begin
  Result := -1;
  if csDestroying in ComponentState then
    Exit;

  R := GetPageButtonRect(0);
  pgBtnHeight := R.Bottom - R.Top;

  for I := 0 to Pages.Count - 1 do
  begin
    if DoDrawPageButton(R, I, FPressedPageBtn = I) then
    begin
      if Themed then // Warren changed.
      begin
        if (FPressedPageBtn = I) or (FHotPageBtn = I) then
          ToolBar := ttbButtonPressed
        else
          ToolBar := ttbButtonHot;
        Details := StyleServices.GetElementDetails(ToolBar);

        if BorderStyle = bsNone then
        begin
          ClipRect := R;
          InflateRect(R, 1, 1);
          StyleServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
          InflateRect(R, -1, -1);
        end else
          StyleServices.DrawElement(Canvas.Handle, Details, R);

        { Determine text color }
        if FPressedPageBtn = I then
          ToolBar := ttbButtonPressed
        else if FHotPageBtn = I then
          ToolBar := ttbButtonHot
        else
          ToolBar := ttbButtonNormal;
        Details := StyleServices.GetElementDetails(ToolBar);
      end else
      begin
        Canvas.Brush.Color := PageBtnProps.Face;// clBtnFace;
        Canvas.FillRect(R);
      end;
      DrawPageButton(R, I, FPressedPageBtn = I);
    end;
    OffsetRect(R, 0, pgBtnHeight);
    if I >= ActivePageIndex then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := Pages.Count - 1;
end;

{ Draw the buttons inside each page }
procedure TJvCustomOutlookBar.DrawButtons(Index: Integer);
var
  I: Integer;
  R, R2, R3: TRect;
  C: TColor;
  SavedDC: Integer;
  flags: Integer;
  Details: TThemedElementDetails;
  dist: Integer;
  {$IF LCL_FullVersion >= 1090000}
  LargeImageRes, SmallImageRes: TScaledImageListResolution;
  f: Double;
  ppi: Integer;
  {$ENDIF}
begin
  if csDestroying in ComponentState then
    Exit;
  if (Index < 0) or (Index >= Pages.Count) or (Pages[Index].Buttons = nil) or
    (Pages[Index].Buttons.Count <= 0)
  then
    Exit;

 {$IF LCL_FullVersion >= 1090000}
  f := GetCanvasScalefactor;
  ppi := Font.PixelsPerInch;
  if FLargeImages <> nil then
    LargeImageRes := FLargeImages.ResolutionForPPI[FLargeImagesWidth, ppi, f];
  if FSmallImages <> nil then
    smallImageRes := FSmallImages.ResolutionForPPI[SmallImagesWidth, ppi, f];
 {$ENDIF}

  R2 := GetPageRect(Index);
  R := GetButtonRect(Index, Pages[Index].TopButtonIndex);
  C := Canvas.Pen.Color;
  try
    Canvas.Brush.Style := bsClear;
    for I := Pages[Index].TopButtonIndex to Pages[Index].Buttons.Count - 1 do
    begin
      if Pages[Index].Buttons[I].Down then
        DrawButtonFrame(Index, I, I);

      if DoDrawButton(R, I, Pages[Index].Buttons[I].Down, I = FLastButtonIndex) then
        case Pages[Index].ButtonSize of
          olbsLarge:
            begin
              SavedDC := SaveDC(Canvas.Handle);
              try
                if LargeImages <> nil then begin
                  dist := Scale96ToForm(4);
                 {$IF LCL_FullVersion >= 1090000}
                  largeImageRes.Draw(Canvas,
                    R.Left + ((R.Right - R.Left) - largeImageRes.Width) div 2,
                    R.Top + dist,
                    Pages[Index].Buttons[I].ImageIndex,
                    Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled
                  );
                 {$ELSE}
                  LargeImages.Draw(Canvas,
                    R.Left + ((R.Right - R.Left) - LargeImages.Width) div 2,
                    R.Top + dist,
                    Pages[Index].Buttons[I].ImageIndex,
                    Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled
                  );
                 {$ENDIF}
                end;
              finally
                RestoreDC(Canvas.Handle, SavedDC);
              end;
              R3 := GetButtonTextRect(ActivePageIndex, I);
              SetBkMode(Canvas.Handle, TRANSPARENT);
              if FWordWrap and (LargeImages <> nil) then
                Flags := DT_WORDBREAK or DT_CENTER or DT_VCENTER
              else
                Flags := DT_EXPANDTABS or DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS;
              if Pages[Index].Buttons[I].Down then
                Canvas.Font.Assign(Pages[Index].DownFont)
              else
                Canvas.Font.Assign(Pages[Index].Font);
              if Themed and (Pages[Index].Color = clDefault) then begin
                if Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled then
                  Details := StyleServices.GetElementDetails(ttbButtonNormal)
                else
                  Details := StyleServices.GetElementDetails(ttbButtonDisabled);
                StyleServices.DrawText(Canvas, Details, Pages[Index].Buttons[I].Caption, R3, Flags, 0);
              end else begin
                if not Pages[Index].Enabled or not Pages[Index].Buttons[I].Enabled then
                begin
                  if ColorToRGB(Pages[Index].Color) = ColorToRGB(clGrayText) then
                    Canvas.Font.Color := FPageBtnProps.Face //clBtnFace
                  else
                    Canvas.Font.Color := clGrayText;
                end;
                DrawText(Canvas.Handle, PChar(Pages[Index].Buttons[I].Caption), -1, R3, Flags);
              end;
            end;

          olbsSmall:
            begin
              SavedDC := SaveDC(Canvas.Handle);
              try
                if SmallImages <> nil then begin
                  dist := Scale96ToForm(2);
                 {$IF LCL_FullVersion >= 1090000}
                  smallImageRes.Draw(Canvas,
                    R.Left + dist, R.Top + dist,
                    Pages[Index].Buttons[I].ImageIndex,
                    Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled
                  );
                 {$ELSE}
                  SmallImages.Draw(Canvas,
                    R.Left + dist, R.Top + dist,
                    Pages[Index].Buttons[I].ImageIndex,
                    Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled
                  );
                 {$ENDIF}
                end;
              finally
                RestoreDC(Canvas.Handle, SavedDC);
              end;
              R3 := GetButtonTextRect(ActivePageIndex, I);
//              InflateRect(R3, -Scale96ToForm(4), 0);
              SetBkMode(Canvas.Handle, TRANSPARENT);
              Flags := DT_EXPANDTABS or DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_NOCLIP or DT_EDITCONTROL;
              if Pages[Index].Buttons[I].Down then
                Canvas.Font.Assign(Pages[Index].DownFont)
              else
                Canvas.Font.Assign(Pages[Index].Font);
              if Themed and (Pages[Index].Color = clDefault) then
              begin
                if Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled then
                  Details := StyleServices.GetElementDetails(ttbButtonNormal)
                else
                  Details := StyleServices.GetElementDetails(ttbButtonDisabled);
                StyleServices.DrawText(Canvas, Details, Pages[Index].Buttons[I].Caption, R3, Flags, 0);
              end else
              begin
                if not Pages[Index].Enabled or not Pages[Index].Buttons[I].Enabled then
                begin
                  if ColorToRGB(Pages[Index].Color) = ColorToRGB(clGrayText) then
                    Canvas.Font.Color := FPageBtnProps.Face//clBtnFace
                  else
                    Canvas.Font.Color := clGrayText;
                end;
                DrawText(Canvas.Handle, PChar(Pages[Index].Buttons[I].Caption), -1, R3, Flags);
              end;
            end;
        end;
      OffsetRect(R, 0, GetButtonHeight(Index, I));
      if R.Top >= R2.Bottom then
        Break;
    end;
  finally
    Canvas.Font := Self.Font;
    Canvas.Pen.Color := C;
  end;
end;

procedure TJvCustomOutlookBar.DrawArrowButtons(Index: Integer);
var
  R: TRect;
  h, w, margin, delta: Integer;
  png: TPortableNetworkGraphic;
  resName: String;
begin
  if csDestroying in ComponentState then
    Exit;
  if (Index < 0) or (Index >= Pages.Count) or (Pages[Index].Buttons = nil) or
    (Pages[Index].Buttons.Count <= 0) then
  begin
    FUpButton.Visible := False;
    FDownButton.Visible := False;
  end
  else
  begin
    R := GetPageRect(Index);
    h := Scale96ToForm(UP_DOWN_DEFAULT_SIZE-1);
    w := Scale96ToForm(UP_DOWN_DEFAULT_SIZE);
    margin := Scale96ToForm(4);
    delta := h + margin;
    FUpButton.Visible := (Pages.Count > 0) and
      (R.Top < R.Bottom - delta) and
      (Pages[Index].TopButtonIndex > 0);
    FDownButton.Visible := (Pages.Count > 0) and
      (R.Top < R.Bottom - delta) and
      (R.Bottom - R.Top < GetButtonTopHeight(Index, Pages[Index].Buttons.Count - 1) + GetButtonHeight(Index, Pages[Index].Buttons.Count - 1));
  // remove the last - ButtonHeight to show arrow
  // button when the bottom of the last button is beneath the edge
  end;

  if UpButton.Visible then begin
    UpButton.SetBounds(ClientWidth - w - margin, R.Top + margin, w, h);
    if (UpButton.Glyph.Width = 0) then begin
      png := TPortableNetworkGraphic.Create;
      try
        resName := 'jvcustomoutlookbaruparrow' + HighDPI_Suffix;
        png.LoadFromResourceName(HInstance, resName);
        UpButton.Glyph.Assign(png);
      finally
        png.Free;
      end;
  end
  else
  if csDesigning in ComponentState then
    UpButton.Top := -1000;

  end;
  if DownButton.Visible then begin
    DownButton.SetBounds(ClientWidth - w - margin, R.Bottom - margin - h, w, h);
    png := TPortableNetworkGraphic.Create;
    try
      resName := 'jvcustomoutlookbardownarrow' + HighDPI_Suffix;
      png.LoadFromResourceName(HInstance, resName);
      DownButton.Glyph.Assign(png);
    finally
      png.Free;
    end;
  end
  else
  if csDesigning in ComponentState then
    DownButton.Top := -1000;
  UpButton.Enabled := UpButton.Visible and Pages[Index].Enabled;
  DownButton.Enabled := DownButton.Visible and Pages[Index].Enabled;
end;

function TJvCustomOutlookBar.DrawPicture(R: TRect; Picture: TPicture): Boolean;
var
  Bmp: TBitmap;
begin
  Result := Assigned(Picture) and Assigned(Picture.Graphic) and not Picture.Graphic.Empty;
  if csDestroying in ComponentState then
    Exit;
  if Result then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Assign(Picture.Graphic);
      Canvas.Brush.Bitmap := Bmp;
      Canvas.FillRect(R);
      Canvas.Brush.Bitmap := nil;
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TJvCustomOutlookBar.DrawCurrentPage(PageIndex: Integer);
var
  R: TRect;
  AColor: TColor;
  Details: TThemedElementDetails;
begin
  if csDestroying in ComponentState then
    Exit;
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or (Pages[PageIndex].Buttons = nil) then
    Exit;

  R := GetPageRect(PageIndex);
  AColor := Canvas.Brush.Color;
  try
    Canvas.Brush.Color := Pages[PageIndex].Color;
    Canvas.Font := Self.Font;
    if DoDrawPage(R, PageIndex) then
    begin
      if not DrawPicture(R, Pages[PageIndex].Picture) then
      begin
        if (Canvas.Brush.Color = clDefault) and ThemedBackground and Themed then
        begin
          Details := StyleServices.GetElementDetails(tebNormalGroupBackground); //tebHeaderBackgroundNormal);
          StyleServices.DrawElement(Canvas.Handle, Details, R);
        end
        else
        begin
          if Canvas.Brush.Color = clDefault then
            Canvas.Brush.Color := Self.Color;
          Canvas.FillRect(R);
        end;
      end;
    end;
    DrawButtonFrame(ActivePageIndex, FLastButtonIndex, FPressedButtonIndex);
    DrawButtons(PageIndex);
  finally
    Canvas.Brush.Color := AColor;
    Canvas.Brush.Style := bsClear;
    SetBkMode(Canvas.Handle, TRANSPARENT);
  end;
  DrawArrowButtons(PageIndex);
end;

procedure TJvCustomOutlookBar.DrawBottomPages(StartIndex: Integer);
var
  R: TRect;
  I: Integer;
  Details: TThemedElementDetails;
  ClipRect: TRect;
  ToolBar: TThemedToolBar;
  pgBtnHeight: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  R := GetPageButtonRect(Pages.Count - 1);
  pgBtnHeight := R.Bottom - R.Top;
  for I := Pages.Count - 1 downto StartIndex do
  begin
    if DoDrawPageButton(R, I, FPressedPageBtn = I) then
    begin
      if Themed then
      begin
        if (FPressedPageBtn = I) or (FHotPageBtn = I) then
          ToolBar := ttbButtonPressed
        else
          ToolBar := ttbButtonHot;
        Details := StyleServices.GetElementDetails(ToolBar);

        if BorderStyle = bsNone then
        begin
          ClipRect := R;
          InflateRect(R, 1, 1);
          StyleServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
          InflateRect(R, -1, -1);
        end else
          StyleServices.DrawElement(Canvas.Handle, Details, R);

        { Determine text color }
        if FPressedPageBtn = I then
          ToolBar := ttbButtonPressed
        else if FHotPageBtn = I then
          ToolBar := ttbButtonHot
        else
          ToolBar := ttbButtonNormal;
        Details := StyleServices.GetElementDetails(ToolBar);
      end else
      begin
        Canvas.Brush.Color := FPageBtnProps.Face;//clBtnFace;
        Canvas.FillRect(R);
      end;
      DrawPageButton(R, I, FPressedPageBtn = I);
    end;
    OffsetRect(R, 0, -pgBtnHeight);
  end;
end;

function TJvCustomOutlookBar.GetPageButtonAtPos(P: TPoint): TJvOutlookBarPage;
var
  I: Integer;
begin
  // TODO: rewrite more optimal (no loop)
  for I := 0 to Pages.Count - 1 do
  begin
    if PtInRect(GetPageButtonRect(I), P) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function TJvCustomOutlookBar.GetPageButtonRect(Index: Integer): TRect;
var
  pgBtnHeight: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= Pages.Count) then
    Exit;
  pgBtnHeight := CalcPageButtonHeight;
  Result := Rect(0, 0, ClientWidth, pgBtnHeight);
  if Index <= ActivePageIndex then
    OffsetRect(Result, 0, pgBtnHeight * Index)
  else
    OffsetRect(Result, 0, (ClientHeight - pgBtnHeight * (Pages.Count - Index)));
end;

function TJvCustomOutlookBar.GetPageTextRect(Index: Integer): TRect;
var
  dist: Integer;
begin
  Result := GetPageButtonRect(Index);
  dist := Scale96ToForm(2);
  InflateRect(Result, -dist, -dist);
end;

function TJvCustomOutlookBar.GetButtonTextSize(
  PageIndex, ButtonIndex: Integer): TSize;
var
  R: TRect;
  DC: HDC;
  S: string;
  OldFont: HFONT;
  txtMargins, minTxtWidth: Integer;
begin
  DC := Canvas.Handle;
  OldFont := SelectObject(DC, Canvas.Font.Handle);
  try
    Canvas.Font.Assign(Pages[PageIndex].Font);
    S := Pages[PageIndex].Buttons[ButtonIndex].Caption;
    if (Pages[PageIndex].ButtonSize = olbsLarge) and FWordWrap then
    begin
      txtMargins := Scale96ToForm(cTextMargins);
      minTxtWidth := Scale96ToForm(cMinTextWidth);
      R := Rect(0, 0, Max(ClientWidth - (2 * txtMargins), minTxtWidth), 0);
      Result.cy := DrawText(DC, PChar(S), Length(S), R, DT_WORDBREAK or DT_CALCRECT or DT_CENTER or DT_VCENTER);
      Result.cx := R.Right;
    end else
      Result := Canvas.TextExtent(S);
  finally
    SelectObject(DC, OldFont);
  end;
end;

function TJvCustomOutlookBar.GetPageRect(Index: Integer): TRect;
var
  pgBtnHeight: Integer;
begin
  if (Index < 0) or (Index >= Pages.Count) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    pgBtnHeight := CalcPageButtonHeight;
    Result := Rect(
      0,
      pgBtnHeight * Index + pgBtnHeight,
      ClientWidth,
      ClientHeight - (Pages.Count - Index) * PgBtnHeight + pgBtnHeight
    );
  end;
end;

function TJvCustomOutlookBar.GetRealImageSize(AImageList: TCustomImageList;
  AImagesWidth: Integer): TSize;
{$IF LCL_FullVersion >= 1090000}
var
  imgRes: TScaledImageListResolution;
begin
  imgRes := AImageList.ResolutionForPPI[AImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
  Result.CX := imgRes.Width;
  Result.CY := imgRes.Height;
end;
{$ELSE}
begin
  Result.CX := AImageList.Width;
  Result.CY := AImageList.Height;
end;
{$ENDIF}

function TJvCustomOutlookBar.GetButtonAtPos(P: TPoint): TJvOutlookBarButton;
var
  I: Integer;
  R, B: TRect;
begin
  // this always returns the button in the visible part of the active page (if any)
  Result := nil;
  if (ActivePageIndex < 0) or (ActivePageIndex >= Pages.Count) then
    Exit;
  R := GetPageRect(ActivePageIndex);
  for I := Pages[ActivePageIndex].TopButtonIndex to Pages[ActivePageIndex].Buttons.Count - 1 do
  begin
    B := GetButtonRect(ActivePageIndex, I);
    if PtInRect(B, P) then
    begin
      Result := Pages[ActivePageIndex].Buttons[I];
      Exit;
    end;
    if B.Top >= R.Bottom then
      Break;
  end;
end;

function TJvCustomOutlookBar.GetButtonRect(PageIndex, ButtonIndex: Integer): TRect;
var
  H, W: Integer;
  dist: Integer;
  leftOffs, topOffs: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then
    Exit;
  H := GetButtonHeight(PageIndex, ButtonIndex);
  topOffs := Scale96ToForm(cButtonTopOffset);
  leftOffs := Scale96ToForm(cButtonLeftOffset);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if FLargeImages <> nil then
      begin
        W := GetRealImageSize(FLargeImages, FLargeImagesWidth).CX;
        dist := Scale96ToForm(4);
        Result := Rect(0, 0, Max(W, GetButtonTextSize(PageIndex, ButtonIndex).cx) + dist, H);
        OffsetRect(Result, (ClientWidth - (Result.Right - Result.Left)) div 2, topOffs);
      end else
        Result := Rect(0, 0, ClientWidth, cButtonTopOffset + H);

    olbsSmall:
      if FSmallImages <> nil then
      begin
        W := GetRealImageSize(FSmallImages, FSmallImagesWidth).CX;
        dist := Scale96ToForm(8);
        Result := Rect(0, 0, W + GetButtonTextSize(PageIndex, ButtonIndex).cx + dist, H);
        OffsetRect(Result, leftOffs, topOffs);
      end else
        Result := Rect(0, 0, ClientWidth, topOffs + H);
  end;
  OffsetRect(Result, 0, GetButtonTopHeight(PageIndex, ButtonIndex) + GetPageRect(PageIndex).Top);
end;

function TJvCustomOutlookBar.GetButtonFrameRect(PageIndex, ButtonIndex: Integer): TRect;
var
  imgSize: TSize;
  delta: Integer;
  btnTopOffs, btnLeftOffs: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then
    Exit;
  btnTopOffs := Scale96ToForm(cButtonTopOffset);
  btnLeftOffs := Scale96ToForm(cButtonLeftOffset);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if FLargeImages <> nil then
      begin
        imgSize := GetRealImageSize(FLargeImages, FLargeImagesWidth);
        delta := Scale96ToForm(6);
        Result := Rect(0, 0, imgSize.CX + delta, imgSize.CY + delta);
        OffsetRect(Result,
          (ClientWidth - (Result.Right - Result.Left)) div 2,
          btnTopOffs + GetButtonTopHeight(PageIndex, ButtonIndex) + GetPageRect(PageIndex).Top + 1
        );
      end else
      begin
        Result := Rect(0, 0, ClientWidth, GetButtonHeight(PageIndex, ButtonIndex));
        OffsetRect(Result, 0,
          btnTopOffs + GetButtonTopHeight(PageIndex, ButtonIndex) + GetPageRect(PageIndex).Top + 1);
      end;

    olbsSmall:
      if FSmallImages <> nil then
      begin
        imgSize := GetRealImageSize(FSmallImages, FSmallImagesWidth);
        delta := Scale96ToForm(4);
        Result := Rect(0, 0, imgSize.CX + delta, imgSize.CY + delta);
        OffsetRect(Result,
          btnLeftOffs,
          btnTopOffs + GetButtonTopHeight(PageIndex, ButtonIndex) + GetPageRect(PageIndex).Top
        );
      end else
      begin
        Result := Rect(0, 0, ClientWidth, GetButtonHeight(PageIndex, ButtonIndex));
        OffsetRect(Result,
          0,
          btnTopOffs + GetButtonTopHeight(PageIndex, ButtonIndex) + GetPageRect(PageIndex).Top
        );
      end;
  end;
end;

function TJvCustomOutlookBar.GetButtonTextRect(PageIndex, ButtonIndex: Integer): TRect;
var
  textSize, imgSize: TSize;
  ButtonHeight: Integer;
  dist2, dist4: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if Pages[PageIndex].Buttons.Count <= ButtonIndex then
    Exit;
  Result := GetButtonRect(PageIndex, ButtonIndex);
  dist2 := Scale96ToForm(2);
  dist4 := Scale96ToForm(4);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if FLargeImages <> nil then
      begin
        Result.Top := Result.Bottom - GetButtonTextSize(PageIndex, ButtonIndex).CY - dist2;
        OffsetRect(Result, 0, -dist4);
      end;
    olbsSmall:
      if FSmallImages <> nil then
      begin
        textSize := GetButtonTextSize(PageIndex, ButtonIndex);
        imgSize := GetRealImageSize(FSmallImages, FSmallImagesWidth);
        ButtonHeight := GetButtonHeight(PageIndex, ButtonIndex);
        Result.Left := imgSize.CX + Scale96ToForm(14);
        Result.Top := Result.Top + (ButtonHeight - textSize.cy) div 2;
        Result.Bottom := Result.Top + textSize.cy + dist2;
        Result.Right := Result.Left + textSize.cx + dist4;
        OffsetRect(Result, 0, -(ButtonHeight - (Result.Bottom - Result.Top)) div 4);
      end else
        InflateRect(Result, -dist4, 0);
  end;
end;

function TJvCustomOutlookBar.IsThemedStored: Boolean;
begin
  Result := not StyleServices.Enabled;
end;

procedure TJvCustomOutlookBar.Paint;
var
  I: Integer;
  R: TRect;
  Details: TThemedElementDetails;
  ClipRect: TRect;
  Rgn: HRGN;
begin
  if csDestroying in ComponentState then
    Exit;

  Canvas.Font := Self.Font;
  if Canvas.Font.IsDefault then
    Canvas.Font := Screen.SystemFont;
  Canvas.Brush.Color := Self.Color;
  if Pages.Count = 0 then // we only need to draw the background when there are no pages
  begin
    if ThemedBackground and Themed then
    begin
      R := ClientRect;
      ClipRect := R;
      InflateRect(R, 1, 0);
      Details := StyleServices.GetElementDetails(tebNormalGroupBackground);
      StyleServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
    end
    else
    begin
      if DoDrawBackGround then
        Canvas.FillRect(ClientRect);
    end;
  end
  else
    I := 1;

  {
  if IsVista then   // Warren Vista paint bug workaround
    Canvas.FillRect(ClientRect);
  }

  SetBkMode(Canvas.Handle, TRANSPARENT);
  I := DrawTopPages;
  if I >= 0 then
  begin
    Rgn := 0;
    try
      if Pages.Count > 1 then
      begin
        // Button icons are not allowed to be painted into the bottom pages panels
        R := GetPageButtonRect(I + 1);
        Rgn := CreateRectRgn(0, 0, 1, 1);
        GetClipRgn(Canvas.Handle, Rgn);
        ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, ClientHeight);
      end;
      DrawCurrentPage(I);
    finally
      if Rgn <> 0 then
      begin
        SelectClipRgn(Canvas.Handle, Rgn);
        DeleteObject(Rgn);
      end;
    end;
  end;
  DrawBottomPages(I + 1);
end;

function TJvCustomOutlookBar.DoPageChanging(Index: Integer): Boolean;
begin
  Result := True;
  if (Index > -1) and Assigned(FOnPageChanging) then
    FOnPageChanging(Self, Index, Result);
end;

procedure TJvCustomOutlookBar.DoPageChange(Index: Integer);
begin
  if (Index > -1) and Assigned(FOnPageChange) then
    FOnPageChange(Self, Index);
end;

procedure TJvCustomOutlookBar.DoButtonClick(Index: Integer);
begin
  if (Index > -1) then
  begin
    with ActivePage.Buttons[Index] do
    begin
      if AutoToggle then
        Down := not Down;
      Click;
    end;
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, Index);
  end;
end;

procedure TJvCustomOutlookBar.SetActivePageIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FPages.Count) then
  begin
    FPressedPageBtn := -1; // reset cache
    // remove old button info
    FLastButtonIndex := -1;
    FPressedButtonIndex := -1;
    FButtonRect := Rect(0, 0, 0, 0);
    if FActivePageIndex <> Value then
    begin
      if not DoPageChanging(Value) then
        Exit;
      FActivePageIndex := Value;
      DoPageChange(Value);
    end;
    Invalidate;
  end;
end;

{ -- wp
procedure TJvCustomOutlookBar.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;
}

procedure TJvCustomOutlookBar.SetButtonSize(const Value: TJvBarButtonSize);
var
  I: Integer;
begin
  FButtonSize := Value;
  Pages.BeginUpdate;
  try
    for I := 0 to Pages.Count - 1 do
      if Pages[I].ParentButtonSize then
      begin
        Pages[I].ParentButtonSize := False;
        Pages[I].ParentButtonSize := True; // reset flag
      end;
  finally
    Pages.EndUpdate; // calls invalidate
  end;
end;

procedure TJvCustomOutlookBar.SetDisabledFontColor1(const Value: TColor); {Warren add}
begin
  FDisabledFontColor1 := Value;
end;

procedure TJvCustomOutlookBar.SetDisabledFontColor2(const Value: TColor); {Warren add}
begin
  FDisabledFontColor2 := Value;
end;

procedure TJvCustomOutlookBar.SetLargeImages(const Value: TCustomImageList);
begin
  if ReplaceImageListReference(Self, Value, FLargeImages, FLargeChangeLink) then
    Invalidate;
end;

{$IF LCL_FullVersion >= 1090000}
procedure TJvCustomOutlookBar.SetLargeImagesWidth(const AValue: Integer);
begin
  if AValue = FLargeImagesWidth then exit;
  FLargeImagesWidth := AValue;
  Invalidate;
end;
{$ENDIF}

function TJvCustomOutlookBar.IsStoredPageButtonHeight: Boolean;
begin
  Result := FPageButtonHeight <> 0;
end;

procedure TJvCustomOutlookBar.SetPageButtonHeight(const Value: Integer);
begin
  if FPageButtonHeight <> Value then
  begin
    FPageButtonHeight := Value;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetPages(const Value: TJvOutlookBarPages);
begin
  FPages.Assign(Value); // Assign calls Invalidate
end;

procedure TJvCustomOutlookBar.SetSmallImages(const Value: TCustomImageList);
begin
  if ReplaceImageListReference(Self, Value, FSmallImages, FSmallChangeLink) then
    Invalidate;
end;

{$IF LCL_FullVersion >= 1090000}
procedure TJvCustomOutlookBar.SetSmallImagesWidth(const AValue: Integer);
begin
  if AValue = FSmallImagesWidth then exit;
  FSmallImagesWidth := AValue;
  Invalidate;
end;
{$ENDIF}

procedure TJvCustomOutlookBar.SetThemed(const Value: Boolean);
begin
  if Value and (not ThemeServices.ThemesEnabled) then
  { Warren added ability to theme/detheme this component for yourself instead of just checking if XP is themed.}
    exit;
  FThemed := Value;
  Invalidate;
end;

procedure TJvCustomOutlookBar.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: Integer);
var
  R: TRect;
  Details: TThemedElementDetails;
begin
  if csDestroying in ComponentState then
    Exit;
  if (ButtonIndex < 0) or (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < Pages[PageIndex].TopButtonIndex) then
    Exit;
  R := GetButtonFrameRect(PageIndex, ButtonIndex);
  if DoDrawButtonFrame(R, ButtonIndex, (PressedIndex = ButtonIndex) or Pages[PageIndex].Buttons[ButtonIndex].Down, True) then
  begin
    if Themed then
    begin
      if (PressedIndex = ButtonIndex) or (Pages[PageIndex].Buttons[ButtonIndex].Down) then
        Details := ThemeServices.GetElementDetails(ttbButtonPressed)
      else
        Details := ThemeServices.GetElementDetails(ttbButtonHot);
      ThemeServices.DrawElement(Canvas.Handle, Details, R);
    end
    else
    begin
      if (PressedIndex = ButtonIndex) or (Pages[PageIndex].Buttons[ButtonIndex].Down) then
        Frame3D(Canvas, R, clBlack, clWhite, 1)
      else
        Frame3D(Canvas, R, clWhite, clBlack, 1);
    end;
  end;
end;

procedure TJvCustomOutlookBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
    Exit;
  P := GetPageButtonAtPos(Point(X, Y));
  if (P <> nil) and (P.Enabled) and (P.Index <> FNextActivePage) then
  begin
    FNextActivePage := P.Index;
    if FNextActivePage <> ActivePageIndex then
    begin // draw button pressed
      FPressedPageBtn := FNextActivePage;
      RedrawRect(GetPageButtonRect(FNextActivePage));
    end;
    Exit;
  end
  else
  begin
    if (FNextActivePage > -1) and Pages[FNextActivePage].Enabled then
      RedrawRect(GetPageButtonRect(FNextActivePage));
    FNextActivePage := -1;
    FPressedPageBtn := -1;
  end;
  B := GetButtonAtPos(Point(X, Y));
  if (B <> nil) and B.Enabled and (Pages[ActivePageIndex].Enabled) then
  begin
    FLastButtonIndex := B.Index;
    FPressedButtonIndex := B.Index;
    FButtonRect := GetButtonFrameRect(ActivePageIndex, B.Index);
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
  R: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  { TODO -oJv :
    1. check whether the mouse is down on a page button and whether the mouse has moved from
    the currently pressed page button }
  P := GetPageButtonAtPos(Point(X, Y));
  if Themed then
  begin
    if ((P = nil) and (FHotPageBtn >= 0)) or (Assigned(P) and (P.Index <> FHotPageBtn)) then
    begin
      if FHotPageBtn >= 0 then
      begin
        R := GetPageButtonRect(FHotPageBtn);
        RedrawRect(R);
      end;
      if Assigned(P) then
        FHotPageBtn := P.Index
      else
        FHotPageBtn := -1;
      if FHotPageBtn >= 0 then
      begin
        R := GetPageButtonRect(FHotPageBtn);
        RedrawRect(R);
      end;
    end;
  end;

  if FPressedPageBtn > -1 then
  begin
    if (P = nil) or (P.Index <> FPressedPageBtn) then
    begin
      R := GetPageButtonRect(FPressedPageBtn);
      RedrawRect(R);
      FPressedPageBtn := -1;
    end;
  end
  else
  if (P <> nil) and (P.Index <> ActivePageIndex) and P.Enabled then
  begin
    if P.Index = FNextActivePage then
    begin
      FPressedPageBtn := FNextActivePage;
      RedrawRect(GetPageButtonRect(FPressedPageBtn));
      Exit;
    end;
  end;
  // TODO: check for button highlight
  B := GetButtonAtPos(Point(X, Y));
  if (B <> nil) and B.Enabled and (Pages[ActivePageIndex].Enabled) then
  begin
    if B.Index <> FLastButtonIndex then
    begin
      RedrawRect(FButtonRect, True);
      FButtonRect := GetButtonFrameRect(ActivePageIndex, B.Index);
      RedrawRect(FButtonRect);
      FLastButtonIndex := B.Index;
    end;
  end
  else
  begin
    if FLastButtonIndex > -1 then
      RedrawRect(FButtonRect);
    FLastButtonIndex := -1;
    FButtonRect := Rect(0, 0, 0, 0);
  end;
end;

procedure TJvCustomOutlookBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbRight then
    Exit;
  if (FNextActivePage > -1) and (FNextActivePage <> ActivePageIndex) then
  begin
    P := GetPageButtonAtPos(Point(X, Y));
    if (P <> nil) and (P.Index = FNextActivePage) then
      ActivePageIndex := FNextActivePage;
  end;
  FNextActivePage := -1;

  B := GetButtonAtPos(Point(X, Y));
  if B <> nil then
  begin
    if B.Index = FPressedButtonIndex then
      DoButtonClick(FPressedButtonIndex);
    FLastButtonIndex := B.Index;
    FPressedButtonIndex := -1;
    FButtonRect := GetButtonFrameRect(ActivePageIndex, FLastButtonIndex);
    RedrawRect(FButtonRect);
  end
  else
  begin
    FButtonRect := GetButtonFrameRect(ActivePageIndex, FLastButtonIndex);
    FLastButtonIndex := -1;
    FPressedButtonIndex := -1;
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  RedrawRect(FButtonRect);
  inherited MouseEnter(Control);
end;

procedure TJvCustomOutlookBar.MouseLeave(Control: TControl);
var
  R: TRect;
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
  RedrawRect(FButtonRect);
  FPressedPageBtn := -1;
  FLastButtonIndex := -1;
  if Themed and (FHotPageBtn >= 0) then
  begin
    R := GetPageButtonRect(FHotPageBtn);
    RedrawRect(R);
    FHotPageBtn := -1;
  end;
end;

function TJvCustomOutlookBar.GetButtonTopHeight(PageIndex,
  ButtonIndex: Integer): Integer;
var
  I: integer;
begin
  Result := 0;
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
     (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then
    Exit;

  if (Pages[PageIndex].ButtonSize = olbsLarge) and FWordWrap then
    for I := Pages[PageIndex].TopButtonIndex to ButtonIndex - 1 do
      Result := Result + GetButtonHeight(PageIndex, I)
  else
    Result := (ButtonIndex - Pages[PageIndex].TopButtonIndex) * GetButtonHeight(PageIndex, ButtonIndex);
end;

function TJvCustomOutlookBar.GetButtonHeight(PageIndex, ButtonIndex: Integer): Integer;
var
  TM: TTextMetric;
  textSize: TSize;
  imgSize: TSize;
  OldFont: HFONT;
  LargeOffset: Integer;
  SmallOffset: Integer;
begin
  OldFont := SelectObject(Canvas.Handle, Canvas.Font.Handle);
  try
    Canvas.Font.Assign(Font);
    GetTextMetrics(Canvas.Handle, TM{%H-});
    Result := TM.tmHeight + TM.tmExternalLeading;
    if (PageIndex >= 0) and (PageIndex < Pages.Count) then
    begin
      textSize := GetButtonTextSize(PageIndex, ButtonIndex);
      largeOffset := Scale96ToForm(8);
      smallOffset := Scale96ToForm(4);
      case Pages[PageIndex].ButtonSize of
        olbsLarge:
          if FLargeImages <> nil then begin
            imgSize := GetRealImageSize(FLargeImages, FLargeImagesWidth);
            Result := Max(Result, imgSize.CY + textSize.CY + largeOffset)
          end else
            Result := textSize.cy + largeOffset;

        olbsSmall:
          if SmallImages <> nil then begin
            imgSize := GetRealImageSize(FSmallImages, FSmallImagesWidth);
            Result := Max(imgSize.CY, textSize.cy) + smallOffset
          end else
            Result := textSize.cy + smallOffset;
      end;
    end;
    Inc(Result, smallOffset);
  finally
    SelectObject(Canvas.Handle, OldFont);
  end;
end;

(*
{$IF LCL_FullVersion >= 1090000}
function TJvCustomOutlookBar.DoEraseBackground(ACanvas: TCanvas; Param: LPARAM): Boolean;
begin
  // don't redraw background: we always fill it anyway
  Result := True;
end;
{$ENDIF}
*)

procedure TJvCustomOutlookBar.RedrawRect(R: TRect; Erase: Boolean = False);
begin
  InvalidateRect(Handle, @R, Erase);
end;

procedure TJvCustomOutlookBar.CMCaptionEditing(var Msg: TLMessage);
var
  R: TRect;
  B: TJvOutlookBarButton;
  P: TJvOutlookBarPage;
begin
  TJvOutlookBarEdit(FEdit).Tag := NativeInt(Msg.WParam);
//  TJvOutlookBarEdit(FEdit).Font.Name := Pages[ActivePageIndex].Font.Name;
//  TJvOutlookBarEdit(FEdit).Font.Size := Pages[ActivePageIndex].Font.Size;
  case Msg.LParam of
    0: // button
      begin
        B := TJvOutlookBarButton(Msg.WParam);
        R := GetButtonTextRect(ActivePageIndex, B.Index);
        R.Left := Max(R.Left, 0);
        R.Right := ClientWidth; //Min(R.Right, ClientWidth);
        TJvOutlookBarEdit(FEdit).ShowEdit(B.Caption, R);
      end;
    1: // page
      begin
        P := TJvOutlookBarPage(Msg.WParam);
        R := GetPageTextRect(P.Index);
        TJvOutlookBarEdit(FEdit).ShowEdit(P.Caption, R);
      end;
  end;
end;

procedure TJvCustomOutlookBar.DoContextPopup( MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPersistent;
begin
  P := GetPageButtonAtPos(MousePos);
  if Assigned(P) then
    PopUpObject := P
  else
  begin
    P := GetButtonAtPos(MousePos);
    if Assigned(P) then
      PopUpObject := P;
  end;
  if P = nil then
    PopUpObject := Self;
  inherited DoContextPopup(MousePos, Handled);
end;

procedure TJvCustomOutlookBar.DoButtonEdit(NewText: string; B: TJvOutlookBarButton);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnEditButton) then
    FOnEditButton(Self, NewText, B.Index, Allow);
  if Allow then
    B.Caption := NewText;
end;

procedure TJvCustomOutlookBar.DoPageEdit(NewText: string; P: TJvOutlookBarPage);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnEditPage) then
    FOnEditPage(Self, NewText, P.Index, Allow);
  if Allow then
    P.Caption := NewText;
end;

procedure TJvCustomOutlookBar.CMCaptionEditAccept(var Msg: TLMessage);
begin
  with Msg do
  begin
    if TObject(LParam) is TJvOutlookBarButton then
      DoButtonEdit(TJvOutlookBarEdit(WParam).Text, TJvOutlookBarButton(LParam))
    else
    if TObject(LParam) is TJvOutlookBarPage then
      DoPageEdit(TJvOutlookBarEdit(WParam).Text, TJvOutlookBarPage(LParam));
  end;
end;

procedure TJvCustomOutlookBar.CMCaptionEditCancel(var Msg: TLMessage);
begin
{  with Msg do
  begin
    if TObject(LParam) is TJvOutlookBarButton then
      DoButtonEditCancel(TJvOutlookBarButton(LParam))
    else TObject(LParam) is TJvOutlookBarPage then
      DoPageEditCancel(TJvOutlookBarPage(LParam));
  end;
  }
end;

function TJvCustomOutlookBar.GetActivePage: TJvOutlookBarPage;
begin
  if (FActivePageIndex > -1) and (FActivePageIndex < FPages.Count) then
    Result := FPages[FActivePageIndex]
  else
    Result := nil;
end;

function TJvCustomOutlookBar.GetActivePageIndex: Integer;
begin
  if (FActivePageIndex < 0) or (FActivePageIndex >= FPages.Count) then
    FActivePageIndex := 0;
  Result := FActivePageIndex;
end;

procedure TJvCustomOutlookBar.SetThemedBackground(const Value: Boolean);
begin
  if Value <> FThemedBackGround then
  begin
    FThemedBackGround := Value;
    if Themed then
      Invalidate;
    {
    if ([csDesigning, csLoading] * ComponentState = []) and Themed then
      Repaint;
     }
  end;
end;

procedure TJvCustomOutlookBar.ColorChanged;
var
  I: Integer;
begin
  inherited ColorChanged;
  for I := 0 to Pages.Count - 1 do
    if Pages[I].ParentColor then
    begin
      Pages[I].ParentColor := False;
      Pages[I].ParentColor := True; // reset flag
    end;
end;

procedure TJvCustomOutlookBar.FontChanged;
var
  I: Integer;
begin
  inherited FontChanged;
  for I := 0 to FPages.Count - 1 do
    if FPages[I].ParentFont then
    begin //set the font of the buttons as well
      FPages[I].ParentFont := False;
      FPages[I].Font := Self.Font;
      FPages[I].ParentFont := True; // reset flag
    end;
end;

class function TJvCustomOutlookBar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 100;
  Result.CY := 220;
end;

procedure TJvCustomOutlookBar.CMDialogChar(var Msg: TCMDialogChar);
var
  I: Integer;
begin
  if CanFocus then
  begin
  // first check the buttons on the active page, then check the pages
    if (ActivePage <> nil) and (ActivePage.Enabled) then
    begin
      for I := 0 to ActivePage.Buttons.Count - 1 do
        if ActivePage.Buttons[I].Enabled and IsAccel(Msg.CharCode, ActivePage.Buttons[I].Caption) then
        begin
          Msg.Result := 1;
          DoButtonClick(I);
          Exit;
        end;
    end;

    for I := 0 to Pages.Count - 1 do
      if Pages[I].Enabled and IsAccel(Msg.CharCode, Pages[I].Caption) then
      begin
        Msg.Result := 1;
        ActivePageIndex := I;
        Exit;
      end;
  end;
  inherited;
end;

{$IF LCL_FullVersion >= 1080000}
procedure TJvCustomOutlookBar.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    DisableAutoSizing;
    try
      if IsStoredPageButtonHeight then
        FPageButtonHeight := round(FPageButtonHeight * AYProportion);
    finally
      EnableAutoSizing;
    end;
  end;
end;

procedure TJvCustomOutlookBar.FixDesignFontsPPI(const ADesignTimePPI: Integer);
var
  i: Integer;
begin
  inherited;
  for i:=0 to Pages.Count-1 do begin
    DoFixDesignFontPPI(Pages[i].Font, ADesignTimePPI);
    DoFixDesignFontPPI(Pages[i].DownFont, ADesignTimePPI);
  end;
end;
{$ENDIF}

function TJvCustomOutlookBar.DoCustomDraw(ARect: TRect; Stage: TJvOutlookBarCustomDrawStage;
  Index: Integer; Down, Inside: Boolean): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, Canvas, ARect, Stage, Index, Down, Inside, Result);
end;

function TJvCustomOutlookBar.DoDrawBackGround: Boolean;
begin
  Result := DoCustomDraw(ClientRect, odsBackground, -1, False, False);
end;

function TJvCustomOutlookBar.DoDrawButton(ARect: TRect; Index: Integer; Down, Inside: Boolean): Boolean;
begin
  Result := DoCustomDraw(ARect, odsButton, Index, Down, Inside);
end;

function TJvCustomOutlookBar.DoDrawButtonFrame(ARect: TRect; Index: Integer;
  Down, Inside: Boolean): Boolean;
begin
  Result := DoCustomDraw(ARect, odsButtonFrame, Index, Down, Inside);
end;

function TJvCustomOutlookBar.DoDrawPage(ARect: TRect; Index: Integer): Boolean;
begin
  Result := DoCustomDraw(ARect, odsPage, Index, False, Index = ActivePageIndex);
end;

function TJvCustomOutlookBar.DoDrawPageButton(ARect: TRect; Index: Integer; Down: Boolean): Boolean;
begin
  Result := DoCustomDraw(ARect, odsPageButton, Index, Down, Index = ActivePageIndex);
end;

procedure TJvOutlookBarPage.DoPictureChange(Sender: TObject);
begin
  Change;
end;

procedure TJvCustomOutlookBar.SetPageImages(const Value: TCustomImageList);
begin
  if ReplaceImageListReference(Self, Value, FPageImages, FPageChangeLink) then
    Invalidate;
end;

{$IF LCL_FullVersion >= 1090000}
procedure TJvCustomOutlookBar.SetPageImagesWidth(const AValue: Integer);
begin
  if AValue = FPageImagesWidth then exit;
  FPageImagesWidth := AValue;
  Invalidate;
end;
{$ENDIF}

procedure TJvCustomOutlookBar.InitiateAction;
var
  I, J: Integer;
begin
  inherited InitiateAction;
  for I := 0 to Pages.Count - 1 do
    for J := 0 to Pages[I].Buttons.Count - 1 do
      Pages[I].Buttons[J].ActionChange(Pages[I].Buttons[J].Action, csLoading in ComponentState);
end;

procedure TJvCustomOutlookBar.Resize;
begin
  if HandleAllocated then Invalidate;
  inherited;
end;


//---- Warren added page button properties Nov 2008

constructor TJvPageBtnProps.Create(owner: TJvCustomOUtlookBar);
begin
  FOwner := owner;
  FShadow := clBtnShadow;
  FHighlight := clBtnHighlight;
  FDkShadow := cl3DDkShadow;
  FFace := clBtnFace;
  FBorderWidth := 1;
end;

procedure TJvPageBtnProps.SetBorderWidth(const Value: INteger);
begin
  FBorderWidth := Value;
end;

procedure TJvPageBtnProps.SetDkShadow(const Value: TColor);
begin
  FDkShadow := Value;
end;

procedure TJvPageBtnProps.SetFace(const Value: TColor);
begin
  FFace := Value;
end;

procedure TJvPageBtnProps.SetHighlight(const Value: TColor);
begin
  FHighlight := Value;
end;

procedure TJvPageBtnProps.SetShadow(const Value: TColor);
begin
  FShadow := Value;
end;

end.
