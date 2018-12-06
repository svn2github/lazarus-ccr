{*********************************************************}
{*                VPCONTACTGRID.PAS 1.03                 *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I vp.inc}

unit VpContactGrid;

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Controls, ExtCtrls, StdCtrls, Forms, Menus,
  VpConst, VpBase, VpBaseDS, VpMisc, VpData, VpSR, VpCanvasUtils;

const
  MaxColumns = 100;  { An arbitrary number representing the maximum number of }
                     { columns allowed in the ContactGrid.  Change it at will }

type
  { Stores location and index of the vertical bars              }
  { These must be in their own type block for BCB compatibility }
  TVpBarRec = packed record
    Rec    : TRect;
    Index  : Integer;
  end;

  TVpContactRec = packed record
    Index       : Integer;
    Contact     : Pointer;
    CompanyRect : TRect;
    EMailRect   : TRect;
    WholeRect   : TRect;
    HeaderRect  : TRect;
    AddressRect : TRect;
    CSZRect     : TRect;
    Phone1Rect  : TRect;
    Phone2Rect  : TRect;
    Phone3Rect  : TRect;
    Phone4Rect  : TRect;
    Phone5Rect  : TRect;
  end;

type
  TVpBarArray = array of TVpBarRec;
  TVpContactArray = array of TVpContactRec;

  { forward declarations }
  TVpContactGrid = class;
  TVpContactGridState = (gsNormal, gsColSizing);

  { InPlace Editor }
  TVpCGInPlaceEdit = class(TCustomEdit)
  protected{private}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    Field: string;
    constructor Create(AOwner: TComponent); override;
    procedure Move(const Loc: TRect; Redraw: Boolean);
  end;

  TVpContactHeadAttr = class(TPersistent)
  protected{private}
    FGrid: TVpContactGrid;
    FFont: TVpFont;
    FColor: TColor;
    FBordered: Boolean;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TVpFont);
    procedure SetBordered(Value: Boolean);
  public
    constructor Create(AOwner: TVpContactGrid);
    destructor Destroy; override;
    property Grid: TVpContactGrid read FGrid;
  published
    property Color: TColor read FColor write SetColor;
    property Font: TVpFont read FFont write SetFont;
    property Bordered: Boolean read FBordered write SetBordered;
  end;

  { Contact Grid }
  TVpContactGrid = class(TVpLinkableControl)
  private
    FComponentHint: TTranslateString;
    FHintMode: TVpHintMode;
  protected{ private }
    FColumnWidth       : Integer;
    FColor             : TColor;
    FBarColor          : TColor;
    FBarWidth          : Integer;
    FAllowInPlaceEdit  : Boolean;
    FScrollBars        : TScrollStyle;
    FContactHeadAttr   : TVpContactHeadAttr;
    FDrawingStyle      : TVpDrawingStyle;
    FContactIndex      : Integer;
    FPrintNumColumns   : Integer;
    FActiveContact     : TVpContact;
    FDefaultPopup      : TPopupMenu;
    FSortBy            : TVpContactSort;
    { contact variables }
    FOwnerDrawContact  : TVpOwnerDrawContactEvent;
    FBeforeEdit        : TVpEditContactEvent;
    FAfterEdit         : TVpContactEvent;
    FOwnerEditContact  : TVpEditContactEvent;
    FOnClickContact    : TVpContactEvent;
    FOnColWidthChange  : TVpCGColWidthChangeEvent;                    
    FVisibleContacts   : Integer;
    FContactsBefore    : Integer;
    FContactsAfter     : Integer;
    { internal variables }
    cgLastXPos         : Integer;
    cgCol1RecCount     : Word;
    cgDragBarNumber    : Integer;
    cgNewColWidth      : Integer;
    cgBarArray         : TVpBarArray;
    cgResizeBarArray   : TVpBarArray;
    cgContactArray     : TVpContactArray;
    cgGridState        : TVpContactGridState;
    cgHitPoint         : TPoint;
    cgClickPoint       : TPoint;
    cgClickTimer       : TTimer;
    cgLoaded           : Boolean;
    cgRowHeight        : Integer;
    cgInPlaceEditor    : TVpCGInPlaceEdit;
    cgCreatingEditor   : Boolean;
    cgPainting         : Boolean;
    cgColCount         : Integer;
    cgVScrollDelta     : Integer;
    FOldCursor : TCursor;
    FMouseContactIndex: Integer;

    { property methods }
    function GetBarWidth: Integer;
    procedure SetBarWidth(Value: Integer);
    procedure SetBarColor(Value: TColor);
    procedure SetContactIndex(Value: Integer);
    procedure SetColumnWidth(Value: Integer);
    procedure SetDrawingStyle(const Value: TVpDrawingStyle);
    procedure SetColor(Value: TColor); reintroduce;
    procedure SetHScrollPos;
    procedure SetPrintNumColumns(const v: Integer);
    procedure SetSortBy(const Value: TVpContactSort);
    procedure SetDataStore(const Value: TVpCustomDataStore); override;

    { internal methods }
    procedure cgCalcRowHeight;
    procedure cgEditInPlace(Sender: TObject);
    procedure cgHookUp;
    function ContactIsVisible(AIndex: Integer): Boolean;
    procedure Paint; override;
    procedure Loaded; override;
    procedure cgScrollHorizontal(Rows: Integer);
    procedure cgSetActiveContactByCoord(Pnt: TPoint);
    procedure cgSpawnContactEditDialog(IsNewContact: Boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetContactIndexByCoord(Pnt: TPoint): Integer;
    function GetDisplayEMailField(AContact: TVpContact): String;
    function GetDisplayEMailValue(AContact: TVpContact): String;
    procedure SetDisplayEMailValue(AContact: TVpContact; AEMail: String);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ScrollIntoView;
    procedure PopupAddContact(Sender: TObject);
    procedure PopupAddVCards(Sender: TObject);
    procedure PopupDeleteContact(Sender: TObject);
    procedure PopupEditContact(Sender: TObject);
    procedure EditContact;
    procedure EndEdit(Sender: TObject);
    procedure InitializeDefaultPopup;

    { message handlers }
    {$IFNDEF LCL}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Msg: TWMSetCursor);
    procedure WMLButtonDblClk(var Msg : TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure VpDataStoreChanged (var Msg : TMessage); message Vp_DataStoreChanged;
    {$ELSE}
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMNCHitTest(var Msg: TLMNCHitTest); message LM_NCHITTEST;
    procedure WMLButtonDblClk(var Msg : TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMKillFocus(var Msg : TLMKillFocus); message LM_KILLFOCUS;
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$ENDIF}

    { Hints }
    function BuildHintString(AContact: TVpContact): String;
    procedure ShowHintWindow(APoint: TPoint; AContactIndex: Integer);
    procedure HideHintWindow;
    procedure SetHint(const AValue: TTranslateString); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadLanguage;
    procedure LinkHandler(Sender: TComponent; NotificationType: TVpNotificationType;
      const Value: Variant); override;
    function GetCityStateZipFormat: String;
    function GetControlType: TVpItemType; override;
    procedure DeleteActiveContact(Verify: Boolean);
    procedure PaintToCanvas(ACanvas: TCanvas; ARect: TRect; Angle: TVpRotationAngle);
    procedure RenderToCanvas(RenderCanvas: TCanvas; RenderIn: TRect;
      Angle: TVpRotationAngle; Scale: Extended; RenderDate: TDateTime;
      StartLine, StopLine: Integer; UseGran: TVpGranularity;
      DisplayOnly: Boolean); override;

    { - Added to support the buttonbar component.                         }
    function SelectContactByName(const Name: String): Boolean;

    {$IF VP_LCL_SCALING = 2}
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
    {$ELSEIF VP_LCL_SCALING = 1}
    procedure ScaleFontsPPI(const AProportion: Double); override;
    {$ENDIF}

    property ActiveContact: TVpContact read FActiveContact;
    property ContactIndex: Integer read FContactIndex write SetContactIndex;

  published
    property Align;
    property Anchors;
   {$IFDEF LCL}
   property BorderSpacing;
   {$ENDIF}
    property TabStop;
    property TabOrder;
    property AllowInPlaceEditing: Boolean
     read FAllowInPlaceEdit write FAllowInPlaceEdit;
    property BarWidth: Integer read GetBarWidth write SetBarWidth;
    property BarColor: TColor read FBarColor write SetBarColor;
    property Color: TColor read FColor write SetColor;
    property ColumnWidth: Integer read FColumnWidth write SetColumnWidth;
    property ContactHeadAttributes: TVpContactHeadAttr
      read FContactHeadAttr write FContactHeadAttr;
    property DrawingStyle: TVpDrawingStyle
      read FDrawingStyle write SetDrawingStyle;
    property HintMode: TVpHintMode
      read FHintMode write FHintMode default hmPlannerHint;
    property PrintNumColumns: Integer
      read FPrintNumColumns write SetPrintNumColumns default 3;
    property SortBy: TVpContactSort
      read FSortBy write SetSortBy default csLastFirst;

    { events }
    property BeforeEdit: TVpEditContactEvent
      read FBeforeEdit write FBeforeEdit;
    property AfterEdit: TVpContactEvent
      read FAfterEdit write FAfterEdit;
    property OnOwnerEditContact: TVpEditContactEvent
      read FOwnerEditContact write FOwnerEditContact;
    property OnColWidthChange: TVpCGColWidthChangeEvent
      read FOnColWidthChange write FOnColWidthChange;                 
    property OnContactChange: TVpContactEvent
      read FOnClickContact write FOnClickContact;
  end;

implementation

uses
  SysUtils, DateUtils, Dialogs,
  VpVCard, VpContactEditDlg, VpContactGridPainter;


(*****************************************************************************)
{ TVpContactHeadAttr }
(*****************************************************************************)
constructor TVpContactHeadAttr.Create(AOwner: TVpContactGrid);
begin
  inherited Create;
  FGrid := AOwner;
  FFont := TVpFont.Create(AOwner);
  FFont.Assign(FGrid.Font);
  FColor := clSilver;
  FBordered := true;
end;
{=====}

destructor TVpContactHeadAttr.Destroy;
begin
  FFont.Free;
end;
{=====}

procedure TVpContactHeadAttr.SetBordered(Value: Boolean);
begin
  if Value <> FBordered then begin
    FBordered := Value;
    Grid.Invalidate;
  end;
end;
{=====}

procedure TVpContactHeadAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    Grid.Invalidate;
  end;
end;
{=====}

procedure TVpContactHeadAttr.SetFont(Value: TVpFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    Grid.Invalidate;
  end;
end;
{=====}


{ TVpCGInPlaceEdit }

constructor TVpCGInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  field := '';
  TabStop := False;
  //BorderStyle := bsNone;
  {$IFDEF VERSION4}
//  DoubleBuffered := False;
  {$ENDIF}
  { make it tiny }
  Height := 1;
  Width := 1;
end;

procedure TVpCGInPlaceEdit.Move(const Loc: TRect; Redraw: Boolean);
const
  dy = 2;
begin
  CreateHandle;
  Redraw := Redraw or not IsWindowVisible(Handle);
  Invalidate;
  with Loc do begin
   {$IFDEF LCL}
    SetBounds(Left, Top-dy, Right-Left, Bottom - Top);
   {$ELSE}
    SetWindowPos(Handle, HWND_TOP, Left, Top-dy, Right-Left, Bottom-Top, SWP_NOREDRAW);
   {$ENDIF}
  end;
  if Redraw then Invalidate;
  SetFocus;
end;

procedure TVpCGInPlaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style{ or ES_MULTILINE};
end;

procedure TVpCGInPlaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Grid: TVpContactGrid;
begin
  Grid := TVpContactGrid(Owner);

  case Key of
    VK_RETURN:
      begin
        Key := 0;
        Grid.EndEdit(Self);
        Grid.SetFocus;
      end;

    VK_UP:
      begin
        Grid.EndEdit(Self);
        Grid.ContactIndex := Grid.ContactIndex - 1;
        Key := 0;
        Grid.SetFocus;
      end;

    VK_DOWN:
      begin
        Grid.EndEdit(Self);
        Grid.ContactIndex := Grid.ContactIndex + 1;
        Key := 0;
        Grid.SetFocus;
      end;

    VK_ESCAPE:
      begin
        Hide;
        Key := 0;
        Grid.SetFocus;
      end;
    else
      inherited;
  end;
end;


(*****************************************************************************)
{ TVpContactGrid }

constructor TVpContactGrid.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  HintWindowClass := TVpHintWindow;

  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  cgGridState := gsNormal;

  { Create internal classes and stuff }
  cgClickTimer := TTimer.Create(self);
  FContactHeadAttr  := TVpContactHeadAttr.Create(Self);

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
//  DoubleBuffered        := true;
  {$ENDIF}
  FVisibleContacts      := 0;
  FAllowInPlaceEdit     := true;
  FContactsBefore       := 0;
  FContactsAfter        := 0;
  cgCol1RecCount        := 0;
  cgClickPoint          := Point (0, 0);
  cgClickTimer.Enabled  := false;
  cgClickTimer.Interval := ClickDelay;
  cgClickTimer.OnTimer  := cgEditInPlace;
  cgCreatingEditor      := false;
  FDrawingStyle         := ds3d;
  cgPainting            := false;
  FColor                := clWindow;
  FBarColor             := clSilver;
  BarWidth              := 3;
  FColumnWidth          := 145;
  FContactIndex         := -1;
  FPrintNumColumns      := 3;

  { initialize the bar arrays. }
  SetLength(cgBarArray, MaxColumns);
  for I := 0 to pred(Length(cgBarArray)) do begin
    cgBarArray[I].Rec := Rect(-1, -1, -1, -1);
    cgBarArray[I].Index := -1;
  end;

  SetLength(cgResizeBarArray, MaxColumns);
  for I := 0 to pred(Length(cgResizeBarArray)) do begin
    cgResizeBarArray[I].Rec := Rect(-1, -1, -1, -1);
    cgResizeBarArray[I].Index := -1;
  end;

  cgDragBarNumber := -1;
  FMouseContactIndex := -1;

  { size }
  Height := 299;
  Width  := 225;

  FDefaultPopup := TPopupMenu.Create(Self);
  InitializeDefaultPopup;

  cgHookUp;
end;
{=====}

destructor TVpContactGrid.Destroy;
begin
  if (HandleAllocated) and (Assigned (DataStore)) and
     (not (csDesigning in ComponentState))
  then
    DataStore.DeregisterWatcher(Handle);

  cgClickTimer.Free;
  FContactHeadAttr.Free;
  FDefaultPopup.Free;
  FreeAndNil(cgInplaceEditor);

  inherited;
end;

function TVpContactGrid.BuildHintString(AContact: TVpContact): String;
const
  SPACE = '   ';
var
  list: TStrings;
  s: String;
begin
  Result := '';
  if AContact = nil then
    exit;

  list := TStringList.Create;
  try
    if (AContact.LastName <> '') or (AContact.FirstName <> '') then begin
      s := AssembleName(AContact);
      if AContact.Title <> '' then
      s := s + ', ' + AContact.Title;
      list.Add(s);
      list.Add('');
    end;
    if AContact.Category > -1 then
      list.Add(RSCategoryLbl + ' ' + CategoryLabel(TVpCategoryType(AContact.Category)));
    if AContact.Birthdate > 0 then begin
      list.Add(Format('%s %s', [RSBirthdateLbl, FormatDateTime('ddddd', AContact.Birthdate)]));
      list.Add(Format('%s %d', [RSAgeLbl, YearsBetween(Date(), AContact.Birthdate)]));
    end;

    if AContact.ContainsWorkData then
    begin
      if list.Count > 0 then
        list.Add('');
      list.Add(Format('--- %s ---', [RSUppercaseWORK]));
      if AContact.Company <> '' then
        list.Add(RSCompanyLbl + ' ' + AContact.Company);
      if AContact.Department <> '' then
        list.Add(RSDepartmentLbl + ' ' + AContact.Department);
      if AContact.Job_Position <> '' then
        list.Add(RSPositionLbl + ' ' + AContact.Job_Position);
      if AContact.Anniversary > 0 then
        list.Add(Format('%s %s', [RSAnniversaryLbl, FormatDateTime('ddddd', AContact.Anniversary)]));
      if (AContact.Address1 <> '') or (AContact.Zip1 <> '') or (AContact.City1 <> '') then begin
        list.Add(RSAddressLbl);
        if AContact.Address1 <> '' then
          list.Add(SPACE + AContact.Address1);
        s := AssembleCSZ(AContact, 1, GetCityStateZipFormat);
        if s <> '' then
          list.Add(SPACE + s);
      end;
    end;

    if AContact.ContainsHomeData then
    begin
      if list.Count > 0 then
        list.Add('');
      list.Add(Format('--- %s ---', [RSUppercaseHOME]));
      if (AContact.Address2 <> '') or (AContact.Zip2 <> '') or (AContact.City2 <> '') then
      begin
        list.Add(RSAddressLbl);
        if AContact.Address1 <> '' then
          list.Add(SPACE + AContact.Address2);
        s := AssembleCSZ(AContact, 2, GetCityStateZipFormat);
        if s <> '' then
          list.Add(SPACE + s);
      end;
    end;

    if AContact.ContainsContactData then
    begin
      if list.Count > 0 then
        list.Add('');
      list.Add(Format('--- %s ---', [RSUppercaseCONTACT]));
      if (AContact.Phone1 <> '') or (AContact.Phone2 <> '') or (AContact.Phone3 <> '') or
         (AContact.Phone4 <> '') or (AContact.Phone5 <> '')
      then begin
        list.Add(RSPhoneFax + ':');
        if AContact.Phone1 <> '' then
          list.Add(SPACE + PhoneLabel(TVpPhoneType(AContact.PhoneType1)) + ': ' + AContact.Phone1);
        if AContact.Phone2 <> '' then
          list.Add(SPACE + PhoneLabel(TVpPhoneType(AContact.PhoneType2)) + ': ' + AContact.Phone2);
        if AContact.Phone3 <> '' then
          list.Add(SPACE + PhoneLabel(TVpPhoneType(AContact.PhoneType3)) + ': ' + AContact.Phone3);
        if AContact.Phone4 <> '' then
          list.Add(SPACE + PhoneLabel(TVpPhoneType(AContact.PhoneType4)) + ': ' + AContact.Phone4);
        if AContact.Phone5 <> '' then
          list.Add(SPACE + PhoneLabel(TVpPhoneType(AContact.PhoneType5)) + ': ' + AContact.Phone5);
      end;
      if (AContact.EMail1 <> '') or (AContact.EMail2 <> '') or (AContact.EMail3 <> '')
      then begin
        list.Add(RSEmail + ':');
        if AContact.EMail1 <> '' then
          list.Add(SPACE + EMailLabel(TVpEMailType(AContact.EMailType1)) + ': ' + AContact.EMail1);
        if AContact.EMail2 <> '' then
          list.Add(SPACE + EMailLabel(TVpEMailType(AContact.EMailType2)) + ': ' + AContact.EMail2);
        if AContact.EMail3 <> '' then
          list.Add(SPACE + EMailLabel(TVpEMailType(AContact.EMailType2)) + ': ' + AContact.EMail3);
      end;
      if (AContact.Website1 <> '') or (AContact.Website2 <> '')
      then begin
        list.Add(RSWebSites + ':');
        if AContact.Website1 <> '' then
          list.Add(SPACE + WebsiteLabel(TVpWebsiteType(AContact.WebsiteType1)) + ': ' + AContact.Website1);
        if AContact.Website2 <> '' then
          list.Add(SPACE + WebsiteLabel(TVpWebsiteType(AContact.WebsiteType2)) + ': ' + AContact.Website2);
      end;
    end;

    if (AContact.Custom1 <> '') or (AContact.Custom2 <> '') or
       (AContact.Custom3 <> '') or (AContact.Custom4 <> '') then
    begin
      if list.Count > 0 then
        list.Add('');
      list.Add(Format('--- %s ---', [RSUppercaseCUSTOM]));
      if AContact.Custom1 <> '' then
        list.Add(AContact.Custom1);
      if AContact.Custom2 <> '' then
        list.Add(AContact.Custom2);
      if AContact.Custom3 <> '' then
        list.Add(Acontact.Custom3);
      if AContact.Custom4 <> '' then
        list.Add(AContact.Custom4);
    end;

    if AContact.Notes <> '' then begin
      if list.Count > 0 then
        list.Add('');
      list.Add(Format('--- %s ---', [RSUppercaseNOTES]));
      s := WrapText(AContact.Notes, MAX_HINT_WIDTH);
      s := StripLastLineEnding(s);
      list.Add(s);
    end;

    Result := list.Text;
  finally
    list.Free;
  end;
end;

procedure TVpContactGrid.LoadLanguage;
begin
  FDefaultPopup.Items.Clear;
  InitializeDefaultPopup;
end;

procedure TVpContactGrid.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  Unused(Value);
  case NotificationType of
    neDataStoreChange : Invalidate;
    neInvalidate      : Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.cgHookUp;
var
  I: Integer;
begin
  { If the component is being dropped on a form at designtime, then }
  { automatically hook up to the first datastore component found    }
  if csDesigning in ComponentState then
    for I := 0 to pred(Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpCustomDataStore) then begin
        DataStore := TVpCustomDataStore(Owner.Components[I]);
        Exit;
      end;
    end;
end;
{=====}

function TVpContactGrid.ContactIsVisible(AIndex: Integer): Boolean;
var
  rec: TVpContactRec;
begin
  rec := cgContactArray[AIndex];
  Result := (rec.WholeRect.Left >= 0) and (rec.WholeRect.Right <= ClientWidth);
end;

procedure TVpContactGrid.Loaded;
begin
  inherited;
  cgLoaded := true;
end;
{=====}

function TVpContactGrid.GetCityStateZipFormat: String;
begin
  if ControlLink <> nil then
    Result := ControlLink.CityStateZipFormat else
    Result := '';
end;

function TVpContactGrid.GetControlType : TVpItemType;
begin
  Result := itContacts;
end;
{=====}

procedure TVpContactGrid.DeleteActiveContact(Verify: Boolean);
var
  Str: string;
  DoIt: Boolean;
begin
  DoIt := not Verify;
  if FActiveContact <> nil then begin
    if FActiveContact.FirstName <> '' then
      Str := FActiveContact.FirstName + ' ' + FActiveContact.LastName
    else
      Str := FActiveContact.LastName;

    if Verify then
      DoIt := (MessageDlg(Format(RSConfirmDeleteContact, [Str]) + #13#10#10 + RSPermanent,
        mtConfirmation, [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      FActiveContact.Deleted := true;
      FActiveContact := nil;
      DataStore.PostContacts;
      Invalidate;
    end;
  end;
end;
{=====}

{$IFDEF LCL}
function TVpContactGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TVpContactGrid.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then begin
    cgScrollHorizontal(1);
    Invalidate;
    Result := True;
  end;
end;

function TVpContactGrid.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then begin
    cgScrollHorizontal(-1);
    Invalidate;
    Result := True;
  end;
end;
{$ENDIF}

procedure TVpContactGrid.Paint;
begin
  RenderToCanvas(Canvas, Rect(0, 0, Width, Height), ra0, 1, Now, -1, -1, gr30Min, False);
end;
{=====}

procedure TVpContactGrid.PaintToCanvas(ACanvas: TCanvas; ARect: TRect;
  Angle: TVpRotationAngle);
begin
  RenderToCanvas(ACanvas, ARect, Angle, 1, Now, -1, -1, gr30Min, True);
end;
{=====}

procedure TVpContactGrid.RenderToCanvas(RenderCanvas: TCanvas;
  RenderIn: TRect; Angle: TVpRotationAngle; Scale: Extended;
  RenderDate: TDateTime; StartLine, StopLine: Integer;
  UseGran: TVpGranularity; DisplayOnly: Boolean);
var
  painter: TVpContactGridPainter;
begin
  cgPainting := true;
  painter := TVpContactGridPainter.Create(Self, RenderCanvas);
  try
    painter.RenderToCanvas(RenderIn, Angle, Scale, RenderDate, StartLine,
      StopLine, UseGran, DisplayOnly);
  finally
    painter.Free;
    cgPainting := false;
  end;
end;

procedure TVpContactGrid.ScrollIntoView;
begin
  if ContactIsVisible(FContactIndex) then
    exit;

  if FContactIndex < FContactsBefore then begin
    FContactsBefore := FContactIndex;
    Invalidate;
  end else begin
    if FContactIndex > FContactsBefore + FVisibleContacts - 2 then begin
      FContactsBefore := FContactIndex - FVisibleContacts + 2;
      Invalidate;
    end;
    while ContactIndex > FContactsBefore + FVisibleContacts - 2 do begin
      inc(FContactsBefore);
      Invalidate;
    end;
  end;
end;

{ Introduced to support the buttonbar component                           !!.02}
function TVpContactGrid.SelectContactByName(const Name: String): Boolean;
var
  Contact: TVpContact;
  ItemIndex: Integer;
begin
  result := false;
  if (DataStore <> nil) and (DataStore.Resource <> nil) then
  begin
    Contact := DataStore.Resource.Contacts.FindContactByName(Name, True);
    if (Contact <> nil) then begin
      FActiveContact := Contact;
      ItemIndex := DataStore.Resource.Contacts.ContactsList.IndexOf(Contact);
      if (ItemIndex > FContactsBefore + FVisibleContacts) or (ItemIndex <= FContactsBefore)
      then begin
        if ItemIndex = 0 then
          FContactsBefore := 0
        else
          FContactsBefore := ItemIndex - 1;
      end;
      result := true;
      Invalidate;
    end;
  end;
end;
{=====}                                                                  

procedure TVpContactGrid.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.cgCalcRowHeight;
var
  SaveFont: TFont;
  Temp: Integer;
begin
  { Calculates row height based on the largest of the RowHead's Minute font,
    the standard client font, and a sample character string. }
  SaveFont := Canvas.Font;
  Canvas.Font.Assign(FContactHeadAttr.Font);
  cgRowHeight := Canvas.TextHeight(TallShortChars);
  Canvas.Font.Assign(SaveFont);
  Temp := Canvas.TextHeight(TallShortChars);
  if Temp > cgRowHeight then
    cgRowHeight := Temp;
  cgRowHeight := cgRowHeight + TextMargin * 2;
  Canvas.Font.Assign(SaveFont);
end;
{=====}

procedure TVpContactGrid.SetDrawingStyle(const Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpContactGrid.SetBarColor(Value: TColor);
begin
  if FBarColor <> Value then begin
    FBarColor := Value;
    Invalidate;
  end;
end;
{=====}

function TVpContactGrid.GetBarWidth: Integer;
begin
  result := FBarWidth - (ExtraBarWidth * 2);
end;
{=====}

procedure TVpContactGrid.SetBarWidth(Value: Integer);
begin
  if (Value > 0) and (FBarWidth + (ExtraBarWidth * 2) <> Value) then begin
    FBarWidth := Value + (ExtraBarWidth * 2);
    Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.SetContactIndex(Value: Integer);
begin
  FContactIndex := Value;
  if (DataStore <> nil) and (DataStore.Resource <> nil) then
    FActiveContact := DataStore.Resource.Contacts.GetContact(FContactIndex)
  else
    FContactIndex := -1;
end;
{=====}

procedure TVpContactGrid.SetColumnWidth(Value: Integer);
begin
  if (Value > 49) and (FColumnWidth <> Value) then begin
    FColumnWidth := Value;
    if Assigned(OnColWidthChange) then                                
      OnColWidthChange(self, Value);                                  
    Invalidate;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpContactGrid.WMSize(var Msg: TWMSize);
{$ELSE}
procedure TVpContactGrid.WMSize(var Msg: TLMSize);
{$ENDIF}
begin
  inherited;
  { Reset the list }
  FContactsBefore := 0;
  FContactsAfter := 0;
  { force a repaint }
  Invalidate;
end;
{=====}

procedure TVpContactGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or WS_HSCROLL;
{$IFNDEF LCL}
    WindowClass.style := CS_DBLCLKS;
{$ENDIF}
  end;
end;
{=====}

procedure TVpContactGrid.CreateWnd;
begin
  inherited;
  cgCalcRowHeight;
  SetHScrollPos;
end;

procedure TVpContactGrid.ShowHintWindow(APoint: TPoint; AContactIndex: Integer);
var
  txt: String;
  contact: TVpContact;
begin
  HideHintWindow;
  case FHintMode of
    hmPlannerHint:
      begin
        if (AContactIndex = -1) or (Datastore = nil) or (Datastore.Resource = nil) then
          exit;
        contact := TVpContact(cgContactArray[AContactIndex].Contact);
        txt := BuildHintString(contact);
      end;
    hmComponentHint:
      txt := FComponentHint;
  end;
  if (txt <> '') and not (csDesigning in ComponentState) and
     not ((cgInplaceEditor <> nil) and cgInplaceEditor.Visible)
  then begin
    Hint := txt;
    Application.Hint := txt;
    Application.ActivateHint(ClientToScreen(APoint), true);
  end;
end;

procedure TVpContactGrid.HideHintWindow;
begin
  Application.CancelHint;
end;

procedure TVpContactGrid.SetHint(const AValue: TTranslateString);
begin
  inherited;
  if FHintMode = hmComponentHint then
    FComponentHint := AValue;
end;

procedure TVpContactGrid.MouseEnter;
begin
  FMouseContactIndex := -1;
end;

procedure TVpContactGrid.MouseLeave;
begin
  HideHintWindow;
end;

procedure TVpContactGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
var
  I: Integer;
  Sizing: Boolean;
  ClientOrigin: TPoint;
begin
  inherited;

  if Button = mbLeft then begin
    Sizing := false;

    cgClickPoint := Point(X, Y);
    if not Focused then
      SetFocus;

    if not (csDesigning in ComponentState) then begin
      { Don't allow column dragging at designtime }
      for I := 0 to pred(Length(cgBarArray)) do begin
        if PointInRect(cgClickPoint, cgBarArray[I].Rec) then begin
          Sizing := true;
          Break;
        end
      end;

      if Sizing then begin
        cgGridState := gsColSizing;
        cgLastXPos := cgClickPoint.X;
        cgNewColWidth := ColumnWidth;
      end else begin
        cgGridState := gsNormal;
        cgSetActiveContactByCoord(cgClickPoint);
        if AllowInPlaceEditing then
          cgClickTimer.Enabled := true;
      end;
    end;
  end else
  if Button = mbRight then begin
    HideHintWindow;
    if not Assigned (PopupMenu) then begin
      if not Focused then
        SetFocus;
      cgClickPoint := Point(X, Y);
      cgSetActiveContactByCoord(cgClickPoint);
      cgClickTimer.Enabled := False;
      ClientOrigin := GetClientOrigin;

      if not Assigned(FActiveContact) then
        for i := 0 to FDefaultPopup.Items.Count - 1 do begin
          if (FDefaultPopup.Items[i].Tag = 1) or ReadOnly then
            FDefaultPopup.Items[i].Enabled := False;
        end
      else
        for i := 0 to FDefaultPopup.Items.Count - 1 do
          FDefaultPopup.Items[i].Enabled := True;

      FDefaultPopup.Popup(cgClickPoint.x + ClientOrigin.x, cgClickPoint.y + ClientOrigin.y);
    end;
  end;
end;

procedure TVpContactGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  J, I, idx: Integer;
begin
  if cgGridState = gsNormal then begin
    inherited MouseMove(Shift, X, Y);
    if ShowHint then begin
      idx := GetContactIndexByCoord(Point(X, Y));
      if idx = -1 then
        HideHintWindow
      else
      if FMouseContactIndex <> idx then begin
        ShowHintWindow(Point(X, Y), idx);
        FMouseContactIndex := idx;
      end;
    end;
  end
  else
  begin
    { Column sizing happens here...}
    { if the in-place editor is active then kill it. }
    if Assigned(cgInplaceEditor) and cgInPlaceEditor.Visible then
      EndEdit(self);

    if cgDragBarNumber = -1 then begin
      for I := 0 to pred(Length(cgResizeBarArray)) do begin
        if (I = 0) and (cgResizeBarArray[I].Rec.Left = -1) then begin
          for J := 0 to pred(Length(cgBarArray)) do begin
            if cgBarArray[J].Rec.Left = -1 then
              Break;
            if PointInRect(Point(X, Y), cgBarArray[J].Rec) then begin
              cgDragBarNumber := cgBarArray[J].Index;
              Break;
            end;
          end;
        end;
        if cgResizeBarArray[I].Rec.Left = -1 then
          Break;
        if PointInRect(Point(X, Y), cgResizeBarArray[I].Rec) then begin
          cgDragBarNumber := cgResizeBarArray[I].Index;
          Break;
        end;
      end;
    end;

    if cgDragBarNumber > -1 then begin
      cgNewColWidth := (X div (cgDragBarNumber + 1)) - (FBarWidth div 2);
      { Prevent the columns from being dragged closed or past the right }
      { side of the client area }
      if (cgNewColWidth <= 50) then
        cgNewColWidth := 50
      else if (cgNewColWidth >= Width - 50) then
        cgNewColWidth := Width - 50;

      Invalidate;
    end;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpContactGrid.WMNCHitTest(var Msg: TWMNCHitTest);
{$ELSE}
procedure TVpContactGrid.WMNCHitTest(var Msg: TLMNCHitTest);
{$ENDIF}
var
  OverBar: Boolean;
  I: Integer;
begin
  DefaultHandler(Msg);
  if not (csDesigning in ComponentState) then begin
    OverBar := false;
    cgHitPoint := ScreenToClient(SmallPointToPoint(Msg.Pos));
    for I := 0 to pred(Length(cgBarArray)) do begin
      if cgBarArray[I].Rec.Left = -1 then
        Break;
      if PointInRect(cgHitPoint, cgBarArray[I].Rec) then begin
        OverBar := true;
        Break;
      end;
    end;
    if OverBar then begin
      if Cursor <> crHSplit then FOldCursor := Cursor;
      Cursor := crHSplit
    end else
      Cursor := FOldCursor;
  end;
end;
{=====}

{$IFNDEF LCL}
procedure TVpContactGrid.WMSetCursor(var Msg: TWMSetCursor);
var
  Cur: HCURSOR;
begin
  Cur := 0;
  with Msg do begin
    if HitTest = HTCLIENT then
      if cgGridState = gsColSizing then
        Cur := Screen.Cursors[crHSplit];
  end;
  if Cur <> 0 then SetCursor(Cur)
  else inherited;
end;
{$ENDIF}
{=====}

{$IFNDEF LCL}
procedure TVpContactGrid.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
{$ELSE}
procedure TVpContactGrid.WMLButtonDblClk(var Msg: TLMLButtonDblClk);
{$ENDIF}
begin
  if not CheckCreateResource then                                      
    Exit;                                                              

  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;

  inherited;
  cgClickTimer.Enabled := false;
  { if the mouse was pressed down in the client area, then select the cell. }
  if not focused then SetFocus;

  { The mouse click landed inside the client area }
  cgSetActiveContactByCoord(Point(Msg.XPos, Msg.YPos));
  { See if we hit an active contact }
  if FActiveContact <> nil then begin
    { edit this contact }
    cgSpawnContactEditDialog(False);
  end else begin
    { we must want to create a new contact }
    FActiveContact := DataStore.Resource.Contacts.AddContact(
      DataStore.GetNextID(ContactsTableName));
    { Allow the user to fill in all the new information }
    cgSpawnContactEditDialog(True);
  end;
end;

{=====}

{$IFNDEF LCL}
procedure TVpContactGrid.WMKillFocus(var Msg : TWMKillFocus);
{$ELSE}
procedure TVpContactGrid.WMKillFocus(var Msg : TLMKillFocus);
{$ENDIF}
begin
  Unused(Msg);
  if Assigned(cgInplaceEditor) and not cgInplaceEditor.Visible then
    Invalidate;
end;
{=====}

procedure TVpContactGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if cgGridState = gsColSizing then begin
    cgGridState := gsNormal;
    cgDragBarNumber := -1;
    ColumnWidth := cgNewColWidth;
  end;
end;
{=====}

procedure TVpContactGrid.cgSpawnContactEditDialog(IsNewContact: Boolean);
var
  AllowIt: Boolean;
  Dlg : TVpContactEditDialog;
begin
  AllowIt := false;
  if Assigned(FOwnerEditContact) then
    FOwnerEditContact(self, FActiveContact, IsNewContact, DataStore.Resource, AllowIt)
  else
  begin
    Dlg := TVpContactEditDialog.Create(Owner);
    try
      Dlg.DataStore := DataStore;
      Dlg.ControlLink := ControlLink;
      AllowIt := Dlg.Execute(FActiveContact);
    finally
      Dlg.Free;
    end;
  end;
  if AllowIt then begin
    if FActiveContact.Changed = true then                                
      DataStore.PostContacts;                                            
  end else
  begin
    if IsNewContact then begin
      DataStore.Resource.Contacts.DeleteContact(FActiveContact);
      FActiveContact := nil;
    end;
    DataStore.PostContacts;
  end;
  Invalidate;
end;
{=====}

procedure TVpContactGrid.cgEditInPlace(Sender: TObject);
begin
  { this is the timer contact which spawns an in-place editor }
  { if the contact is doublecliked before this timer fires, then the }
  { contact is edited in a dialog based editor. }
  cgClickTimer.Enabled := false;
  EditContact;
end;
{=====}

procedure TVpContactGrid.EditContact;
var
  AllowIt: Boolean;
  field: string;
  I: Integer;
begin
  field := '';
  AllowIt := true;
  { call the user defined BeforeEdit contact }
  if Assigned(FBeforeEdit) then
    FBeforeEdit(Self, FActiveContact, false, DataStore.Resource, AllowIt);

  if AllowIt then begin
    { find the field to edit }
    for I := 0 to pred(Length(cgContactArray)) do begin
      { find the active contact in the contactarray...}
      if (PointInRect(cgClickPoint, cgContactArray[I].WholeRect)) then begin
        FActiveContact := cgContactArray[I].Contact;
        with cgContactArray[I] do begin
          if PointInRect(cgClickPoint, AddressRect) then
            field := 'Address'
          else if PointInRect(cgClickPoint, CompanyRect) then
            field := 'Company'
          else if PointInRect(cgClickPoint, EMailRect) then
            field := 'EMail'
          else if PointInRect(cgClickPoint, CSZRect) then
            field := 'CSZ'
          else if PointInRect(cgClickPoint, Phone1Rect) then
            field := 'Phone1'
          else if PointInRect(cgClickPoint, Phone2Rect) then
            field := 'Phone2'
          else if PointInRect(cgClickPoint, Phone3Rect) then
            field := 'Phone3'
          else if PointInRect(cgClickPoint, Phone4Rect) then
            field := 'Phone4'
          else if PointInRect(cgClickPoint, Phone5Rect) then
            field := 'Phone5';

          if field <> '' then begin
            { create and spawn the in-place editor }
            if cgInplaceEditor = nil then begin
              cgInPlaceEditor := TVpCGInPlaceEdit.Create(Self);
              cgInPlaceEditor.Parent := self;
              cgInPlaceEditor.OnExit := EndEdit;
            end;
            cgInplaceEditor.Show;

            { edit address }
            if field = 'Address' then begin
              cgInPlaceEditor.Field := 'Address1';
              cgInPlaceEditor.Move(AddressRect, true);
              cgInPlaceEditor.Text := FActiveContact.Address1;
            end;
            { edit company }
            if field = 'Company' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(CompanyRect, true);
              cgInPlaceEditor.Text := FActiveContact.Company;
            end;
            { edit CSZ }
            if field = 'CSZ' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(CSZRect, true);
              cgInPlaceEditor.Text := FActiveContact.City1 + ', ' + FActiveContact.State1
                + ' ' + FActiveContact.Zip1;
            end;
            { edit email }
            if field = 'EMail' then begin
              cgInPlaceEditor.Field := GetDisplayEMailField(FActiveContact);
              cgInPlaceEditor.Move(EMailRect, true);
              cgInPlaceEditor.Text := GetDisplayEMailValue(FActiveContact);
            end;
            { edit Phone1 }
            if field = 'Phone1' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone1Rect, true);
              cgInPlaceEditor.Text := FActiveContact.Phone1;
            end;
            { edit Phone2 }
            if field = 'Phone2' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone2Rect, true);
              cgInPlaceEditor.Text := FActiveContact.Phone2;
            end;
            { edit Phone3 }
            if field = 'Phone3' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone3Rect, true);
              cgInPlaceEditor.Text := FActiveContact.Phone3;
            end;
            { edit Phone4 }
            if field = 'Phone4' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone4Rect, true);
              cgInPlaceEditor.Text := FActiveContact.Phone4;
            end;
            { edit Phone5 }
            if field = 'Phone5' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone5Rect, true);
              cgInPlaceEditor.Text := FActiveContact.Phone5;
            end;
          end;
        end;
      end;
    end;
    if (cgInPlaceEditor <> nil) and cgInplaceEditor.Visible then
      cgInPlaceEditor.SelectAll;
  end;
end;
{=====}

procedure TVpContactGrid.EndEdit(Sender: TObject);
var
  City, State, Zip: string;
begin
  if Assigned(cgInPlaceEditor) and cgInPlaceEditor.Visible then begin
    {Address}
    if cgInPlaceEditor.field = 'Address' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Address1 then begin
        FActiveContact.Address1 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Company}
    else if cgInPlaceEditor.field = 'Company' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Company then begin
        FActiveContact.Company := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {EMail}
    else if cgInPlaceEditor.field = 'EMail' then begin
      if cgInPlaceEditor.Text <> FActiveContact.EMail1 then begin
        SetDisplayEMailValue(FActiveContact, cgInplaceEditor.Text);
        FActiveContact.Changed := true;
      end;
    end
    {City, State, Zip}
    else if cgInPlaceEditor.field = 'CSZ' then begin
      ParseCSZ(cgInPlaceEditor.Text, City, State, Zip);
      if (City <> FActiveContact.City1) or
         (State <> FActiveContact.State1) or
         (Zip <> FActiveContact.Zip1) then
      begin
        FActiveContact.City1 := City;
        FActiveContact.State1 := State;
        FActiveContact.Zip1 := Zip;
        FActiveContact.Changed := true;
      end;
    end
    {Phone1}
    else if cgInPlaceEditor.field = 'Phone1' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone1 then begin
        FActiveContact.Phone1 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone2}
    else if cgInPlaceEditor.field = 'Phone2' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone2 then begin
        FActiveContact.Phone2 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone3}
    else if cgInPlaceEditor.field = 'Phone3' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone3 then begin
        FActiveContact.Phone3 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone4}
    else if cgInPlaceEditor.field = 'Phone4' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone4 then begin
        FActiveContact.Phone4 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone5}
    else if cgInPlaceEditor.field = 'Phone5' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone5 then begin
        FActiveContact.Phone5 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end;

    cgInplaceEditor.Hide;
//    FreeAndNil(cgInPlaceEditor);

    if FActiveContact.Changed then begin
      DataStore.PostContacts;
      if Assigned(FAfterEdit) then
        FAfterEdit(self, FActiveContact);
    end;
  end;
  Invalidate;
end;
{=====}

procedure TVpContactGrid.InitializeDefaultPopup;
var
  NewItem: TMenuItem;
begin
  if RSContactPopupAdd <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSContactPopupAdd;
    NewItem.OnClick := PopupAddContact;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RsContactPopupAddVCards <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSContactPopupAddVCards;
    NewItem.OnClick := PopupAddVCards;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSContactPopupEdit <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSContactPopupEdit;
    NewItem.OnClick := PopupEditContact;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;

  if RSContactPopupDelete <> '' then begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := RSContactPopupDelete;
    NewItem.OnClick := PopupDeleteContact;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add(NewItem);
  end;
end;
{=====}

procedure TVpContactGrid.PopupAddContact(Sender: TObject);
var
  id: Integer;
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if not CheckCreateResource then                                      
    Exit;                                                              
  if not Assigned (DataStore) then                                     
    Exit;                                                              
  if not Assigned (DataStore.Resource) then                            
    Exit;                                                              
  { we must want to create a new contact }
  id := DataStore.GetNextID(ContactsTableName);
  FActiveContact := DataStore.Resource.Contacts.AddContact(id);
  { Allow the user to fill in all the new information }
  cgSpawnContactEditDialog(True);
end;
{=====}

procedure TVpContactGrid.PopupAddVCards(Sender: TObject);
var
  dlg: TOpenDialog;
  vcards: TVpVCards;
  i: Integer;
  fn: String;
  id: Integer;
begin
  if ReadOnly or (not CheckCreateResource) or
     (not Assigned(Datastore)) or (not Assigned(Datastore.Resource))
  then
    exit;

  dlg := TOpenDialog.Create(nil);
  try
    dlg.Title := RSLoadVCardsTitle;
    dlg.Filter := RSVCardFilter;
    dlg.FileName := '';
    dlg.Options := dlg.Options + [ofAllowMultiSelect, ofFileMustExist];
    if dlg.Execute then begin
      Screen.Cursor := crHourGlass;
      Application.ProcessMessages;
      vcards := TVpVCards.Create;
      try
        for fn in dlg.Files do begin
          vcards.LoadFromFile(fn);
          for i := 0 to vcards.Count-1 do begin
            id := DataStore.GetNextID (ContactsTableName);
            FActiveContact := Datastore.Resource.Contacts.AddContact(id);
            FActiveContact.LoadFromVCard(vcards[i]);
            Datastore.PostContacts;
            DataStore.NotifyDependents;
          end;
        end;
        Invalidate;
      finally
        vcards.Free;
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TVpContactGrid.PopupDeleteContact (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveContact <> nil then
    DeleteActiveContact (True);
end;
{=====}

procedure TVpContactGrid.PopupEditContact (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveContact <> nil then
    { edit this contact }
    cgSpawnContactEditDialog(False);
end;
{=====}

procedure TVpContactGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  PopupPoint: TPoint;
  contactCount: Integer;
begin
  contactCount := DataStore.Resource.Contacts.Count;
  case Key of
    VK_UP:
      if ContactIndex > 0 then
        ContactIndex := ContactIndex - 1;
    VK_DOWN:
      if ContactIndex < contactCount - 1 then
        ContactIndex := ContactIndex + 1;
    VK_HOME:
      ContactIndex := 0;
    VK_END:
      ContactIndex := contactCount - 1;
    VK_RIGHT:
      if ContactIndex + cgCol1RecCount <= contactCount - 1 then
        ContactIndex := ContactIndex + cgCol1RecCount
      else
        ContactIndex := contactCount - 1;
    VK_LEFT:
      if ContactIndex - cgCol1RecCount <= 0 then
        ContactIndex := 0
      else
        ContactIndex := ContactIndex - cgCol1RecCount;
    VK_DELETE:
      DeleteActiveContact (true);
{$IFNDEF LCL}
    VK_TAB:
      if ssShift in Shift then
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, False))
      else
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, True));
{$ENDIF}
    VK_F10:
      if (ssShift in Shift) and not Assigned(PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
    VK_APPS:
      if not Assigned(PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup(PopupPoint.x + 10, PopupPoint.y + 10);
      end;
    else
      inherited;
      exit;
  end;

  Invalidate;
  ScrollIntoView;
  Key := 0;
  inherited;
end;
{=====}

{$IFNDEF LCL}
procedure TVpContactGrid.WMHScroll(var Msg: TWMHScroll);
{$ELSE}
procedure TVpContactGrid.WMHScroll(var Msg: TLMHScroll);
{$ENDIF}
begin
  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;

  { for simplicity, bail out of editing while scrolling. }
  EndEdit(Self);
  if Assigned(cgInplaceEditor) and cgInplaceEditor.Visible then
    Exit;

  case Msg.ScrollCode of
    SB_LINELEFT:
      cgScrollHorizontal(-1);
    SB_LINERIGHT:
      cgScrollHorizontal(1);
    SB_PAGELEFT:
      cgScrollHorizontal(-1);
    SB_PAGERIGHT:
      cgScrollHorizontal(1);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        if (Msg.Pos > FContactsBefore) and (FContactsAfter = 0) then Exit;
        FContactsBefore := Msg.Pos;
        if (FContactsBefore = 1) and (cgCol1RecCount = 1) then
          FContactsBefore := 0;
        if FContactsBefore >= DataStore.Resource.Contacts.Count then
          FContactsBefore := DataStore.Resource.Contacts.Count - cgCol1RecCount;
      end;
  end;
  Invalidate;
end;
{=====}

{$IFNDEF LCL}
procedure TVpContactGrid.VpDataStoreChanged (var Msg : TMessage);
begin
  { The DataStore's Resource may not have been property set (that is
    the DataStore existed, but there was no resource.  Force the sortby
    on the contacts here }
  if Assigned (DataStore) then 
    if Assigned (DataStore.Resource) then
      DataStore.Resource.Contacts.ContactSort := SortBy;
end;
{$ENDIF}
{=====}

procedure TVpContactGrid.cgScrollHorizontal(Rows: Integer);
begin
  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;
    
  if (Rows < 0) and (FContactsBefore > 0) then
    FContactsBefore := FContactsBefore - cgCol1RecCount
  else if (Rows > 0) and (FContactsAfter > 0) then
    FContactsBefore := FContactsBefore + cgCol1RecCount;

  if FContactsBefore >= DataStore.Resource.Contacts.Count then
    FContactsBefore := DataStore.Resource.Contacts.Count - cgCol1RecCount;

  if FContactsBefore < 0 then FContactsBefore := 0;
end;
{=====}

procedure TVpContactGrid.SetHScrollPos;
var
  SI: TScrollInfo;
begin
  if (not HandleAllocated) or (DataStore = nil) or (DataStore.Resource = nil)
    or (csDesigning in ComponentState)
  then Exit;

  with SI do begin
    cbSize := SizeOf(SI);
    fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    nMin := 1;
    nMax := DataStore.Resource.Contacts.Count;
    nPage := FVisibleContacts;
    if FContactsAfter = 0 then
      nPos := DataStore.Resource.Contacts.Count
    else
      nPos := FContactsBefore;
    nTrackPos := nPos;
  end;
  SetScrollInfo(Handle, SB_HORZ, SI, True);
end;
{=====}

procedure TVpContactGrid.SetPrintNumColumns (const v : Integer);
begin
  if v <> FPrintNumColumns then begin
    FPrintNumColumns := v;
    if Assigned (FControlLink) then
      FControlLink.Printer.NotifyLinked;
  end;
end;
{=====}

procedure TVpContactGrid.SetDataStore (const Value : TVpCustomDataStore);
begin
  if (Assigned (DataStore)) and (not (csDesigning in ComponentState)) then
    DataStore.DeregisterWatcher (Handle);

  inherited SetDataStore (Value);

  if (Assigned (DataStore)) and (not (csDesigning in ComponentState)) then
    DataStore.RegisterWatcher (Handle);

  if not Assigned (DataStore) then
    Exit;
  if not Assigned (DataStore.Resource) then
    Exit;
  DataStore.Resource.Contacts.ContactSort := SortBy;
end;
{=====}

procedure TVpContactGrid.SetSortBy(const Value: TVpContactSort);
begin
  if Value <> FSortBy then begin
    FSortBy := Value;
    if not Assigned(DataStore) then
      Exit;
    if not Assigned(DataStore.Resource) then
      Exit;
    DataStore.Resource.Contacts.ContactSort := FSortBy;
    cgClickTimer.Enabled := False;
    FActiveContact := nil;
    Invalidate;
  end;
end;
{=====}

function TVpContactGrid.GetContactIndexByCoord(Pnt: TPoint): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to High(cgContactArray) do
    if PointInRect(Pnt, cgContactArray[i].WholeRect) then begin
      Result := i;
      exit;
    end;
end;

procedure TVpContactGrid.cgSetActiveContactByCoord(Pnt: TPoint);
var
  I: integer;
begin
  FActiveContact := nil;
  for I := 0 to pred(Length(cgContactArray)) do begin
    { if the point is in an active contact...}
    if PointInRect(Pnt, cgContactArray[I].WholeRect) then begin
      { Set ActiveContact to the selected one }
      FContactIndex := I;
      FActiveContact := TVpContact(cgContactArray[I].Contact);
      Break;
    end;
  end;
  if (FActiveContact <> nil) then begin
    if Assigned(FOnClickContact) then
      FOnClickContact(Self, FActiveContact);
  end;
  Invalidate;
end;

function TVpContactGrid.GetDisplayEMailValue(AContact: TVpContact): String;
begin
  if AContact = nil then
    Result := ''
  else begin
    Result := AContact.EMail1;
    if Result = '' then Result := AContact.EMail2;
    if Result = '' then Result := AContact.EMail3;
  end;
end;

function TVpContactGrid.GetDisplayEMailField(AContact: TVpContact): String;
begin
  if (AContact.EMail1 <> '') then
    Result := 'EMail1'
  else if (AContact.EMail2 <> '') then
    Result := 'EMail2'
  else if (AContact.EMail3 <> '') then
    Result := 'EMail3'
  else
    Result := 'EMail1';
end;

procedure TVpContactGrid.SetDisplayEMailValue(AContact: TVpContact; AEMail: String);
begin
  if (AContact.EMail1 <> '') then
    AContact.EMail1 := AEMail
  else if (AContact.EMail2 <> '') then
    AContact.EMail2 := AEMail
  else if (AContact.EMail3 <> '') then
    AContact.EMail3 := AEMail;
end;

{$IF VP_LCL_SCALING = 2}
procedure TVpContactGrid.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
  inherited;
  DoScaleFontPPI(ContactHeadAttributes.Font, AToPPI, AProportion);
end;
{$ELSEIF VP_LCL_SCALING = 1}
procedure TVpContactGrid.ScaleFontsPPI(const AProportion: Double);
begin
  inherited;
  DoScaleFontPPI(ContactHeadAttributes.Font, AProportion);
end;
{$ENDIF}

end.
