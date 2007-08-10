unit rxlookup;

{$I rx.inc}

interface

uses
  LCLType, LCLProc, LCLIntf,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  DB, EditBtn, DBGrids, StdCtrls, Buttons, LMessages, DbCtrls, GraphType,
  dbutils, RxDbGrid, rxpopupunit;

type
  TRxCustomDBLookupCombo = class;

  { TLookupSourceLink }
  TDataSourceLink = class(TDataLink)
  private
    FDataControl:TRxCustomDBLookupCombo;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  end;

  { TLookupSourceLink }

  TLookupSourceLink = class(TDataLink)
  private
    FDataControl: TRxCustomDBLookupCombo;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetChanged; override;
  end;

  { TPopupWindow }

  TPopupWindow = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;
  { TCustomDBLookupEdit }

  TCustomDBLookupEdit = class(TEditButton)
  private
    FDropDownCount: Integer;
    FDropDownWidth: Integer;
    FLookupDisplayIndex: Integer;
    FLookupField: string;
    FLookupDisplay: string;
    FLookupSource:TDataSource;
    //
    FPopupWindow:TPopupWindow;
    FList:TDBGrid;
    FPopupVisible:boolean;
    FSaveAfterScroll:TDataSetNotifyEvent;
    FFieldList:TStringList;
    function GetLookupSource: TDataSource;
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetLookupDisplay(const AValue: string);
    procedure SetLookupDisplayIndex(const AValue: Integer);
    procedure SetLookupField(const AValue: string);
    procedure SetLookupSource(AValue: TDataSource);
    //
    procedure ShowList;
    procedure HideList;
    procedure ShowPopUp;
    procedure DoMouseUp(Sender: TOBject; AButton: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoAfterScroll(DataSet: TDataSet);
    procedure DoPopupWindowDeactivate(Sender: TObject);
    procedure DoPopupWindowKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    procedure DoButtonClick (Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 8;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property LookupDisplay: string read FLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read FLookupDisplayIndex write SetLookupDisplayIndex default 0;
    property LookupField: string read FLookupField write SetLookupField;
    property LookupSource: TDataSource read FLookupSource write SetLookupSource;
    property PopupVisible:boolean read FPopupVisible;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TRxLookupEdit = class(TCustomDBLookupEdit)
  published
    property DropDownCount;
    property DropDownWidth;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupField;
    property LookupSource;
  end;

  { TRxCustomDBLookupCombo }
  TRxCustomDBLookupCombo = class (TCustomControl)
  private
    //FDataLink:TFieldDataLink;
    FDataLink:TDataSourceLink;
    FDataFieldName: string;
    FDataField :TField;
    //
    FLookupDataLink:TLookupSourceLink;
    FLocateObject:TLocateObject;
    FLookupField: string;
    FLookupDisplay: string;
    FDisplayField:TField;
    FKeyField:TField;
    FLookupDisplayIndex: Integer;
    FListActive:boolean;
    //
    FEmptyItemColor: TColor;
    FEmptyValue: string;
    FOnChange: TNotifyEvent;
    FPopUpFormOptions: TPopUpFormOptions;
    //
    FRxPopUpForm:TPopUpForm;
    FFieldList:TStringList;
    FValuesList:TStringList;
    FValue:string;
    //Visual
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput : Boolean;
    FOnButtonClick : TNotifyEvent;
    FReadOnly: boolean;
    FDisplayAll: boolean;
    function GetDataSource: TDataSource;
    function GetDisplayAll: Boolean;
    function GetDropDownCount: Integer;
    function GetDropDownWidth: Integer;
    function GetLookupSource: TDataSource;
    function GetMinHeight: Integer;
    function GetBorderSize: Integer;
    procedure CheckButtonVisible;
    function GetButtonWidth: Integer;
    function GetFlat: Boolean;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: Integer;
    function GetPopupVisible: boolean;
    procedure SetButtonNeedsFocus(const AValue: Boolean);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetDataFieldName(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetDisplayAll(const AValue: Boolean);
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetDropDownWidth(const AValue: Integer);
    procedure SetEmptyItemColor(const AValue: TColor);
    procedure SetEmptyValue(const AValue: string);
    procedure SetFlat(const AValue: Boolean);
    procedure SetGlyph(const AValue: TBitmap);
    procedure SetLookupDisplay(const AValue: string);
    procedure SetLookupDisplayIndex(const AValue: Integer);
    procedure SetLookupField(const AValue: string);
    procedure SetLookupSource(const AValue: TDataSource);
    procedure SetNumGlyphs(const AValue: Integer);
    procedure SetPopUpFormOptions(const AValue: TPopUpFormOptions);
    procedure SetReadOnly(const AValue: boolean);
    function StoreEmpty: boolean;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure CMExit(var Message:TLMessage); message CM_EXIT;
    procedure PaintDisplayValues(ACanvas: TCanvas; R: TRect; ALeft: Integer);
    procedure CheckNotCircular;
    procedure DisplayValueChanged;
    procedure DataLinkActiveChanged;
    procedure DataLinkRecordChanged(Field: TField);
    procedure UpdateFieldValues;
    procedure ShowList;
    procedure SetValueKey(const Value: string);
    procedure UpdateKeyValue;
    procedure KeyValueChanged;
    procedure UpdateData;
    procedure OnClosePopup(AResult:boolean);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); dynamic;
    procedure SetParent(AParent: TWinControl); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoPositionButton; virtual;
    procedure DoChange; virtual;
    procedure DoButtonClick(Sender: TObject); virtual;
    Procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure Paint; override;

    procedure LookupDataSetChanged; virtual;
    procedure ListLinkActiveChanged; virtual;

    //
    property Button: TSpeedButton read FButton;
    property ButtonWidth : Integer read GetButtonWidth write SetButtonWidth;
    property ButtonOnlyWhenFocused : Boolean Read FButtonNeedsFocus Write SetButtonNeedsFocus;
    property DirectInput : Boolean read FDirectInput write FDirectInput Default True;
    property DisplayAllFields: Boolean read GetDisplayAll write SetDisplayAll default False;
    property Flat : Boolean read GetFlat write SetFlat;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property ReadOnly:boolean read FReadOnly write SetReadOnly;
    property EmptyValue: string read FEmptyValue write SetEmptyValue stored StoreEmpty;
    property EmptyItemColor: TColor read FEmptyItemColor write SetEmptyItemColor default clWindow;
    //data
    property PopUpFormOptions:TPopUpFormOptions read FPopUpFormOptions write SetPopUpFormOptions;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount default 8;
    property DropDownWidth: Integer read GetDropDownWidth write SetDropDownWidth default 0;
    property LookupDisplay: string read FLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read FLookupDisplayIndex write SetLookupDisplayIndex default 0;
    property LookupField: string read FLookupField write SetLookupField;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PopupVisible:boolean read GetPopupVisible;
  end;
  
  { TRxDBLookupCombo }
  TRxDBLookupCombo = class(TRxCustomDBLookupCombo)
  published
    property AutoSize;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    Property ButtonWidth;
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    Property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property PopUpFormOptions;
    Property Flat;
    property Font;
    property Glyph;
    property EmptyValue;
    property EmptyItemColor;
//    property MaxLength;
    property NumGlyphs;
    Property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{    property Width default 100;
    property Height default 23;}

    property DisplayAllFields;
    property DropDownCount;
    property DropDownWidth;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupField;
    property LookupSource;
  end;
  
function CreateArrowBitmap:TBitmap;
implementation
uses VCLUtils, Math;

function CreateArrowBitmap:TBitmap;
begin
  Result:=Graphics.TBitmap.Create;
  Result.LoadFromLazarusResource('btn_downarrow');
end;

{ TCustomDBLookupEdit }

function TCustomDBLookupEdit.GetLookupSource: TDataSource;
begin
  Result:=FLookupSource;
end;

procedure TCustomDBLookupEdit.SetDropDownCount(const AValue: Integer);
begin
  if FDropDownCount=AValue then exit;
  if AValue>50 then
    FDropDownCount:=50
  else
  if AValue<0 then
    FDropDownCount:=8
  else
    FDropDownCount:=AValue;
end;

procedure TCustomDBLookupEdit.SetLookupDisplay(const AValue: string);
var
  S1, S2:string;
  K:integer;
begin
  if FLookupDisplay=AValue then exit;
  FLookupDisplay:=AValue;
  FFieldList.Clear;
  S2:=AValue;
  while S2<>'' do
  begin
    K:=Pos(';', S2);
    if K>0 then
    begin
      S1:=Copy(S2, 1, K-1);
      Delete(S2, 1, K);
    end
    else
    begin
      S1:=S2;
      S2:='';
    end;
    FFieldList.Add(S1);
  end;
end;

procedure TCustomDBLookupEdit.SetLookupDisplayIndex(const AValue: Integer);
begin
  if FLookupDisplayIndex=AValue then exit;
  FLookupDisplayIndex:=AValue;
end;

procedure TCustomDBLookupEdit.SetLookupField(const AValue: string);
begin
  if FLookupField = AValue then exit;
  FLookupField:=AValue;
end;

procedure TCustomDBLookupEdit.SetLookupSource(AValue: TDataSource);
begin
  if LookupSource = AValue then exit;
  FLookupSource:=AValue;
end;

procedure TCustomDBLookupEdit.ShowList;
var
  i,W:integer;
  GC:TColumn;
begin
  if Assigned(FLookupSource) and
     Assigned(FLookupSource.DataSet) and
     FLookupSource.DataSet.Active then
  if not FPopupVisible then
  begin
    ShowPopUp;
    FList.Columns.Clear;
    W:=16;
    for I:=0 to FFieldList.Count-1 do
    begin;
      GC:=TDbGridColumns(FList.Columns).Add;
      GC.Field:=FLookupSource.DataSet.FieldByName(FFieldList[i]);
      if (W+GC.Field.DisplayWidth * FList.Canvas.TextWidth('W')) > FList.Width then
        GC.Width:=FList.Width - W
      else
        GC.Width:=GC.Field.DisplayWidth * FList.Canvas.TextWidth('W');
      W:=W+GC.Width + 4;
    end;
    FPopupVisible:=true;
  end;
end;

procedure TCustomDBLookupEdit.HideList;
begin
  if Assigned(FPopupWindow) then
  begin
    FList.Parent:=nil;
    FPopupWindow.Close;
    FPopupVisible:=false;
    FLookupSource.DataSet.AfterScroll:=FSaveAfterScroll;
    FList.DataSource:=nil;
    FPopupWindow:=nil;
  end;
end;

procedure TCustomDBLookupEdit.ShowPopUp;
var
  R:TPoint;
begin
  if not Assigned(FList) then
      FList:=TDBGrid.Create(Owner);
  FPopupWindow:=TPopupWindow.Create(Application);
  FList.Parent:=FPopupWindow;

  FList.Align:=alClient;
  FList.OnMouseUp:=@DoMouseUp;
  FList.DataSource:=FLookupSource;
  FList.Options:=FList.Options -
      [dgTabs, dgEditing, dgTitles, dgIndicator, dgColumnResize, dgRowLines] + [dgRowSelect];
  FSaveAfterScroll:=FLookupSource.DataSet.AfterScroll;
  FLookupSource.DataSet.AfterScroll:=@DoAfterScroll;

  R.X:=Left;
  R.Y:=Top+Height;
  R:=Parent.ClientToScreen(R);
  FPopupWindow.Top:=R.Y;
  FPopupWindow.Left:=R.X;
  FPopupWindow.Width:=Width+Button.Width;
  FPopupWindow.OnDeactivate:=@DoPopupWindowDeactivate;
  FPopupWindow.OnKeyDown:=@DoPopupWindowKeyDown;
  if FDropDownCount>0 then
    FPopupWindow.Height:=FList.Canvas.TextHeight('W')*FDropDownCount
  else
    FPopupWindow.Height:=FList.Canvas.TextHeight('W')*8;
  FPopupWindow.ShowModal;
end;

procedure TCustomDBLookupEdit.DoMouseUp(Sender: TOBject; AButton: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (X>0) and (Y>0) and (X<FList.ClientWidth) and (Y<Flist.ClientHeight) then
    HideList;
end;

procedure TCustomDBLookupEdit.DoAfterScroll(DataSet: TDataSet);
var
  S:string;
begin
  S:=FFieldList[FLookupDisplayIndex];
  Text:=FLookupSource.DataSet.FieldByName(S).AsString;
  if Assigned(FSaveAfterScroll) then
     FSaveAfterScroll(DataSet);
end;

procedure TCustomDBLookupEdit.DoPopupWindowDeactivate(Sender: TObject);
begin
  HideList;
end;

procedure TCustomDBLookupEdit.DoPopupWindowKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    vk_Return: HideList;
  else
    FList.KeyDown(Key, Shift);
    exit;
  end;
  Key:=0;
end;

procedure TCustomDBLookupEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  if PopupVisible then
    HideList
  else
    ShowList;
end;

procedure TCustomDBLookupEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_RETURN]) and PopupVisible then
  begin
    if Key=VK_RETURN then HideList
    else
      Flist.KeyDown(Key, Shift);
    Key := 0;
  end
  else
  if (Key = VK_DOWN) and ((ssAlt in Shift) or (ssCtrl in Shift)) then
  begin
    ShowList;
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
{  FIgnoreChange := (SelLength > 0) or (Key = VK_BACK);}
  if not (PopupVisible or ReadOnly) and (Key in [VK_UP, VK_DOWN]) and (Shift = []) then
  begin
    case Key of
      VK_UP: if not FLookupSource.DataSet.BOF then FLookupSource.DataSet.Prior;
      VK_DOWN: if not FLookupSource.DataSet.EOF then FLookupSource.DataSet.Next;
    end;
    Text:=FLookupSource.DataSet.FieldByName(FFieldList[FLookupDisplayIndex]).AsString;
    Key:=0;
  end;
end;

constructor TCustomDBLookupEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownCount:=8;
  FFieldList:=TStringList.Create;
  Glyph:=CreateArrowBitmap;
  ButtonWidth:=15;
end;

destructor TCustomDBLookupEdit.Destroy;
begin
  FFieldList.Clear;
  FreeAndNil(FFieldList);
  inherited Destroy;
end;

{ TPopupWindow }

constructor TPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle:=bsNone;
end;

{ TRxCustomDBLookupCombo }

function TRxCustomDBLookupCombo.GetMinHeight: Integer;
begin
  Result := 15{DefaultTextHeight} + GetBorderSize + 3;
end;

function TRxCustomDBLookupCombo.GetDisplayAll: Boolean;
begin
  Result := FDisplayAll;
end;

function TRxCustomDBLookupCombo.GetDropDownCount: Integer;
begin
  Result:=FPopUpFormOptions.DropDownCount
end;

function TRxCustomDBLookupCombo.GetDropDownWidth: Integer;
begin
  Result:=FPopUpFormOptions.DropDownWidth;
end;

function TRxCustomDBLookupCombo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TRxCustomDBLookupCombo.GetLookupSource: TDataSource;
begin
  Result:=FLookupDataLink.DataSource;
end;

function TRxCustomDBLookupCombo.GetBorderSize: Integer;
{var
  Params: TCreateParams;
  R: TRect;}
begin
{  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  Result := R.Bottom - R.Top;}
  Result := 3;
end;

procedure TRxCustomDBLookupCombo.CheckButtonVisible;
begin
  if Assigned(FButton) then
    FButton.Visible:=(csdesigning in ComponentState) or
                     (Visible and (Focused or not FButtonNeedsFocus));
end;

function TRxCustomDBLookupCombo.GetButtonWidth: Integer;
begin
  if Assigned(FButton) then Result:=FButton.Width
  else Result:=0;
end;

function TRxCustomDBLookupCombo.GetFlat: Boolean;
begin
  if Assigned(FButton) then Result:=FButton.Flat
  else Result:=false;
end;

function TRxCustomDBLookupCombo.GetGlyph: TBitmap;
begin
  if Assigned(FButton) then Result:=FButton.Glyph
  else Result:=nil;
end;

function TRxCustomDBLookupCombo.GetNumGlyphs: Integer;
begin
  if Assigned(FButton) then Result:=FButton.NumGlyphs
  else Result:=0;
end;

function TRxCustomDBLookupCombo.GetPopupVisible: boolean;
begin
  Result:=Assigned(FRxPopUpForm);
end;

procedure TRxCustomDBLookupCombo.SetButtonNeedsFocus(const AValue: Boolean);
begin
  if FButtonNeedsFocus<>AValue then
  begin
    FButtonNeedsFocus:=AValue;
    CheckButtonVisible;
  end;
end;

procedure TRxCustomDBLookupCombo.SetButtonWidth(const AValue: Integer);
begin
  if Assigned(FButton) then
    FButton.Width:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetDataFieldName(const AValue: string);
begin
  if FDataFieldName <> AValue then
  begin
    FDataFieldName := AValue;
    DataLinkActiveChanged;
  end;
end;

procedure TRxCustomDBLookupCombo.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  if AValue <> nil then AValue.FreeNotification(Self);
end;

procedure TRxCustomDBLookupCombo.SetDisplayAll(const AValue: Boolean);
begin
  if FDisplayAll <> AValue then
  begin
    FDisplayAll := AValue;
    Invalidate;
  end;
end;

procedure TRxCustomDBLookupCombo.SetDropDownCount(const AValue: Integer);
begin
  FPopUpFormOptions.DropDownCount:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetDropDownWidth(const AValue: Integer);
begin
  FPopUpFormOptions.DropDownWidth:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetEmptyItemColor(const AValue: TColor);
begin
  if FEmptyItemColor=AValue then exit;
  FEmptyItemColor:=AValue;
  if not (csReading in ComponentState) then
    Invalidate;
end;

procedure TRxCustomDBLookupCombo.SetEmptyValue(const AValue: string);
begin
  if FEmptyValue=AValue then exit;
  FEmptyValue:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetFlat(const AValue: Boolean);
begin
  if Assigned(FButton) then
    FButton.Flat:=AValue;
  Invalidate;
end;

procedure TRxCustomDBLookupCombo.SetGlyph(const AValue: TBitmap);
begin
  if Assigned(FButton) then
    FButton.Glyph:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetLookupDisplay(const AValue: string);
var
  S1, S2:string;
  K:integer;
begin
  if FLookupDisplay=AValue then exit;
  FLookupDisplay:=AValue;
  FFieldList.Clear;
  S2:=AValue;
  while S2<>'' do
  begin
    K:=Pos(';', S2);
    if K>0 then
    begin
      S1:=Copy(S2, 1, K-1);
      Delete(S2, 1, K);
    end
    else
    begin
      S1:=S2;
      S2:='';
    end;
    FFieldList.Add(S1);
  end;
  DisplayValueChanged;
end;

procedure TRxCustomDBLookupCombo.SetLookupDisplayIndex(const AValue: Integer);
begin
  if FLookupDisplayIndex=AValue then exit;
  FLookupDisplayIndex:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetLookupField(const AValue: string);
begin
  FLookupField:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetLookupSource(const AValue: TDataSource);
begin
  FLookupDataLink.DataSource:=AValue;
  FLocateObject.DataSet:=FLookupDataLink.DataSet;
end;

procedure TRxCustomDBLookupCombo.SetNumGlyphs(const AValue: Integer);
begin
  if Assigned(FButton) then
    FButton.NumGlyphs:=AValue;
end;

procedure TRxCustomDBLookupCombo.SetPopUpFormOptions(
  const AValue: TPopUpFormOptions);
begin
  FPopUpFormOptions.Assign(AValue);
end;

procedure TRxCustomDBLookupCombo.SetReadOnly(const AValue: boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
end;

function TRxCustomDBLookupCombo.StoreEmpty: boolean;
begin
  Result:=true;
end;

procedure TRxCustomDBLookupCombo.WMSetFocus(var Message: TLMSetFocus);
begin
  FButton.Visible:=True;
  inherited WMSetFocus(Message);
  Invalidate;
end;

procedure TRxCustomDBLookupCombo.WMKillFocus(var Message: TLMKillFocus);
begin
  if FButtonNeedsFocus then
    FButton.Visible:=false;
  inherited WMKillFocus(Message);
  Invalidate;
end;

procedure TRxCustomDBLookupCombo.CMExit(var Message: TLMessage);
begin
  inherited;
end;

procedure TRxCustomDBLookupCombo.PaintDisplayValues(ACanvas: TCanvas; R: TRect;
  ALeft: Integer);
var
  I, LastIndex, TxtWidth: Integer;
  X, W, ATop, ARight: Integer;
  S: string;
  F:TField;
begin
  if FValuesList.Count=0 then exit;
  if ColorToRGB(Self.Color) <> ColorToRGB(clBtnFace) then
    ACanvas.Pen.Color := clBtnFace
  else ACanvas.Pen.Color := clBtnShadow;
//  LastIndex := FFieldList.Count-1;
  LastIndex := FValuesList.Count-1;
  TxtWidth := ACanvas.TextWidth('M');
  ATop := Max(0, (HeightOf(R) - ACanvas.TextHeight('Xy')) div 2);
  ARight := R.Right;
  Inc(R.Left, ALeft);
  for I := 0 to LastIndex do
  begin
    F:=LookupSource.DataSet.FieldByName(FFieldList[i]);
    S := FValuesList[i];// F.DisplayText;

    if FPopUpFormOptions.Columns.Count>i then
      W := FPopUpFormOptions.Columns[i].Width
    else
    begin
      W := F.DisplayWidth;
      if I < LastIndex then
        W := W * TxtWidth + 4
      else
        W := ARight - R.Left;
    end;
    
    X := 2;
    R.Right := R.Left + W;
    case F.AlignMent of
      taRightJustify: X := W - ACanvas.TextWidth(S) - 3;
      taCenter: X := (W - ACanvas.TextWidth(S)) div 2;
    end;
    ACanvas.TextRect(R, R.Left + Max(0, X), ATop, S);
    Inc(R.Left, W);
    if I < LastIndex then
    begin
      ACanvas.MoveTo(R.Right, R.Top);
      ACanvas.LineTo(R.Right, R.Bottom);
      Inc(R.Left);
    end;
    if R.Left >= ARight then
      Break;
  end;
end;

procedure TRxCustomDBLookupCombo.CheckNotCircular;
begin
{  if FDataLink.Active and ((DataSource = LookupSource) or
    (FDataLink.DataSet = FLookupLink.DataSet)) then
    _DBError(SCircularDataLink);}
end;

procedure TRxCustomDBLookupCombo.DisplayValueChanged;
begin
  FDisplayField:=nil;
  if FLookupDataLink.Active and (FLookupDisplay <> '') then
  begin
    FDisplayField := FLookupDataLink.DataSet.FieldByName(FFieldList[FLookupDisplayIndex]);
    if PopupVisible then
    begin
//      UpdateData;
      UpdateFieldValues;
    end;
  end;
end;


procedure TRxCustomDBLookupCombo.DataLinkActiveChanged;
begin
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := FDataLink.DataSet.FieldByName(FDataFieldName);
  end
  else
  begin
    FDataField := nil;
  end;
  DataLinkRecordChanged(nil);
end;

procedure TRxCustomDBLookupCombo.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FDataField) then
  begin
    if FDataField <> nil then
    begin
      SetValueKey(FDataField.AsString);
    end
    else
      SetValueKey(FEmptyValue);
  end
{  else
    SetValueKey(Field.AsString);}
end;

procedure TRxCustomDBLookupCombo.UpdateFieldValues;
var
  i:integer;
  F:TField;
begin
  FValuesList.Clear;
  if Assigned(FDataField) then
  begin
    if FDataField.IsNull then
      FValuesList.Add(FEmptyValue)
    else
    if FLookupDataLink.Active then
      for i:=0 to FFieldList.Count-1 do
      begin
        F:=FLookupDataLink.DataSet.FieldByName(FFieldList[i]);
        FValuesList.Add(F.DisplayText);
      end;
  end;
end;

procedure TRxCustomDBLookupCombo.ShowList;
var
  i,c,W:integer;
  GC:TColumn;
  F, F1:TField;
begin
  if Assigned(FLookupDataLink.DataSet) and (FLookupDataLink.DataSet.Active) then
    if not PopupVisible then
    begin

      if FDataField <> nil then FValue := FDataField.AsString
      else FValue := FEmptyValue;
      if Assigned(FDataField) and not FDataField.IsNull then
         FLocateObject.Locate(FLookupField, FValue, true, false)
      else
      if FLookupDataLink.Active then
        FLookupDataLink.DataSet.First;

      FRxPopUpForm:=ShowRxDBPopUpForm(Self, FLookupDataLink.DataSet, @OnClosePopup,
        FPopUpFormOptions, FLookupDisplay, LookupDisplayIndex, ButtonWidth);
    end
end;


procedure TRxCustomDBLookupCombo.SetValueKey(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    FLocateObject.Locate(FLookupField, FValue, true, false);
    KeyValueChanged;
  end;
end;

procedure TRxCustomDBLookupCombo.UpdateKeyValue;
begin
  if FDataField <> nil then FValue := FDataField.AsString
  else FValue := FEmptyValue;
  if not FDataField.IsNull then
     FLocateObject.Locate(FLookupField, FValue, true, false);
  KeyValueChanged;
end;

procedure TRxCustomDBLookupCombo.KeyValueChanged;
begin
  UpdateFieldValues;
  Invalidate;
  DoChange;
end;

procedure TRxCustomDBLookupCombo.UpdateData;
begin
  if FLookupDataLink.Active and Assigned(FDataField) then
  begin
    if FKeyField.IsNull then FDataField.Clear
    else FDataField.AsString:=FKeyField.AsString;
  end;
end;

procedure TRxCustomDBLookupCombo.OnClosePopup(AResult: boolean);
begin
  if Assigned(FRxPopUpForm) and AResult and (pfgColumnResize in FPopUpFormOptions.Options) then
    FillPopupWidth(FPopUpFormOptions, FRxPopUpForm);
    
  FRxPopUpForm:=nil;
  if AResult then
  begin
    FDataLink.Edit;
    UpdateData;
  end;
  SetFocus;
  if (Owner is TWinControl) then
    TWinControl(Owner).Repaint
  else
    Parent.Repaint;
end;

procedure TRxCustomDBLookupCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_RETURN, VK_HOME, VK_END]) and PopupVisible then
  begin
    FRxPopUpForm.KeyDown(Key, Shift);
{    if Key=VK_RETURN then
      HideList
    else
      Flist.KeyDown(Key, Shift);
    Key := 0;}
  end
  else
  if not PopupVisible then
  begin
    if (Key = VK_DOWN) and ((ssAlt in Shift) or (ssCtrl in Shift)) then
    begin
      ShowList;
      Key := 0;
    end
    else
    if (Key = VK_ESCAPE) and (not FDataField.IsNull) then
    begin
      FDataField.Clear;
      UpdateKeyValue;
      Key:=0;
    end;
  end;
  inherited KeyDown(Key, Shift);
  if FLookupDataLink.Active and FDataLink.Active and not (PopupVisible or ReadOnly) then
  begin
    if (Key in [VK_UP, VK_DOWN]) and (Shift = []) then
    begin
      FDataLink.Edit;
      if not FDataField.IsNull then
         FLocateObject.Locate(FLookupField, FDataField.AsString, true, false);
      case Key of
        VK_UP: if not FLookupDataLink.DataSet.BOF then
                 FLookupDataLink.DataSet.Prior;
        VK_DOWN: if not FLookupDataLink.DataSet.EOF then
                 FLookupDataLink.DataSet.Next;
      end;
      FDataLink.UpdateRecord;
      KeyValueChanged;
      Key:=0;
    end
  end;
end;

procedure TRxCustomDBLookupCombo.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if PopupVisible then
    FRxPopUpForm.KeyPress(Key);
end;

procedure TRxCustomDBLookupCombo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
  begin
    FButton.Parent := Parent;
    CheckButtonVisible;
  end;
end;

procedure TRxCustomDBLookupCombo.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  if not (csReading in ComponentState) and (Height < GetMinHeight) then
    Height := GetMinHeight
  else
  begin
    if (csDesigning in ComponentState) then
      if (Height < GetMinHeight) then
        Height := GetMinHeight;
  end;

  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  DoPositionButton;
end;

procedure TRxCustomDBLookupCombo.DoPositionButton;
begin
  if FButton <> nil then
    FButton.SetBounds(Left+Width, Top, FButton.Width, Height);
end;

procedure TRxCustomDBLookupCombo.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRxCustomDBLookupCombo.DoButtonClick(Sender: TObject);
begin
  if not FReadOnly then
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);

{  if PopupVisible then
    HideList
  else}
    ShowList;
end;

procedure TRxCustomDBLookupCombo.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
end;

procedure TRxCustomDBLookupCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TRxCustomDBLookupCombo.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);
  CheckButtonVisible;
end;

procedure TRxCustomDBLookupCombo.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);
  if FButton<>nil then
    FButton.Enabled:=Enabled;
end;

procedure TRxCustomDBLookupCombo.Paint;
var
  Selected:boolean;
  R, ImageRect: TRect;
  X, Flags, TextMargin: Integer;
  AText: string;
  Bmp: TBitmap;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := Focused and (not (csPaintCopy in ControlState)) and  (not PopupVisible);
  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end
  else
  if not Enabled and NewStyleControls then
    Canvas.Font.Color := clGrayText;

  SetRect(R, 0, 0, ClientWidth, ClientHeight);
  if Flat then
  begin
    Canvas.Frame3d(R, 3, bvLowered);
  end
  else
  begin
    RxFrame3D(Canvas, R, clWindowFrame, clBtnHighlight, 1);
    RxFrame3D(Canvas, R, clBtnShadow, clBtnFace, 1);
  end;
  
  TextMargin := 0;
  if ClientWidth > 4 then
  begin
    SetRect(R, 2, 2, ClientWidth - 2, ClientHeight - 2);
    if TextMargin > 0 then Inc(TextMargin);
    X := 2 + TextMargin;
{    if not (FPopupVisible and (FDataList.FSearchText <> '')) and not DrawList then
      case Alignment of
        taRightJustify: X := W - Canvas.TextWidth(AText) - 6;
        taCenter: X := (W + TextMargin - Canvas.TextWidth(AText)) div 2;
      end;}
    Bmp := TBitmap.Create;
    try
      with Bmp.Canvas do
      begin
        Font := Self.Canvas.Font;
        Brush := Self.Canvas.Brush;
        Pen := Self.Canvas.Pen;
      end;
      Bmp.Width := WidthOf(R);
      Bmp.Height := HeightOf(R);
      ImageRect := Rect(0, 0, WidthOf(R), HeightOf(R));
      Bmp.Canvas.FillRect(ImageRect);
      if FDisplayAll then
        PaintDisplayValues(Bmp.Canvas, ImageRect, TextMargin)
      else
      begin
        if Assigned(FDataField) and FDataField.IsNull then
        begin
          Bmp.Canvas.Brush.Color:=FEmptyItemColor;
          Bmp.Canvas.FillRect(ImageRect);
          AText:=FEmptyValue
        end
        else
        if FValuesList.Count>0 then
          AText:=FValuesList[FLookupDisplayIndex]//FLookupDataLink.DataSet.FieldByName(FFieldList[FLookupDisplayIndex]).DisplayText;
        else
          AText:='';
        Bmp.Canvas.TextRect(ImageRect, X, Max(0, (HeightOf(R) - Canvas.TextHeight(AText)) div 2), AText);
      end;
{      if Image <> nil then
      begin
        ImageRect.Right := ImageRect.Left + TextMargin + 2;
        DrawPicture(Bmp.Canvas, ImageRect, Image);
      end;}
      Canvas.Draw(R.Left, R.Top, Bmp);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TRxCustomDBLookupCombo.LookupDataSetChanged;
begin
  if PopupVisible then
  begin
    UpdateFieldValues;
    Invalidate;
  end;
end;

procedure TRxCustomDBLookupCombo.ListLinkActiveChanged;
var
  DataSet: TDataSet;
  ResultField: TField;
begin
  FListActive := False;
  FKeyField := nil;
  FDisplayField := nil;
  DataSet:=nil;
  if FLookupDataLink.Active and (FLookupField <> '') then
  begin
    CheckNotCircular;
    DataSet := FLookupDataLink.DataSet;
    FKeyField := DataSet.FieldByName(FLookupField);
    FListActive := True;
  end;
  FLocateObject.DataSet := DataSet;


  if FListActive and Assigned(FDataField) then UpdateKeyValue
  else KeyValueChanged;
end;

constructor TRxCustomDBLookupCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 23;
  Width := 100;
  FFieldList := TStringList.Create;
  FValuesList:= TStringList.Create;
  FLocateObject:=CreateLocate(nil);
  FPopUpFormOptions:=TPopUpFormOptions.Create;
  
  //Lookup
  FLookupDataLink:=TLookupSourceLink.Create;
  FLookupDataLink.FDataControl:=Self;

  //Data
  FDataLink:=TDataSourceLink.Create;
  FDataLink.FDataControl:=Self;


  FButton := TSpeedButton.Create(Self);
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  CheckButtonVisible;
  FButton.OnClick := @DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  ControlStyle := ControlStyle - [csSetCaption];
  FDirectInput := True;
  ParentColor:=false;
  //
  Color:=clWindow;
  FEmptyItemColor:=clWindow;
  Glyph:=CreateArrowBitmap;
  ButtonWidth:=15;
  Ctl3D:=true;
  TabStop:=true;
end;

destructor TRxCustomDBLookupCombo.Destroy;
begin
  FreeAndNil(FLocateObject);
  FreeAndNil(FDataLink);
  FreeAndNil(FLookupDataLink);
  FreeAndNil(FButton);
  FFieldList.Clear;
  FreeAndNil(FFieldList);
  FreeAndNil(FValuesList);
  FreeAndNil(FPopUpFormOptions);
  inherited Destroy;
end;


{ TDataSourceLink }

procedure TDataSourceLink.ActiveChanged;
begin
  if FDataControl <> nil then
    FDataControl.DataLinkActiveChanged;
end;

procedure TDataSourceLink.LayoutChanged;
begin
  inherited LayoutChanged;
end;

procedure TDataSourceLink.FocusControl(Field: TFieldRef);
begin
  inherited FocusControl(Field);
end;

procedure TDataSourceLink.RecordChanged(Field: TField);
begin
  if FDataControl <> nil then
    FDataControl.DataLinkRecordChanged(Field);
end;

procedure TDataSourceLink.UpdateData;
begin
  if FDataControl <> nil then
     FDataControl.UpdateData;
end;

{ TLookupSourceLink }

procedure TLookupSourceLink.ActiveChanged;
begin
  if FDataControl <> nil then
    FDataControl.ListLinkActiveChanged;
end;

procedure TLookupSourceLink.LayoutChanged;
begin
  if FDataControl <> nil then
    FDataControl.ListLinkActiveChanged;
end;

procedure TLookupSourceLink.DataSetChanged;
begin
  if FDataControl <> nil then
    FDataControl.LookupDataSetChanged;
end;

end.
