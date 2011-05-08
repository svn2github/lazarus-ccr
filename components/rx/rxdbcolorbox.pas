unit RxDBColorBox;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ColorBox,
  DbCtrls, DB, LMessages, LCLType;

type

  { TRxCustomDBColorBox }

  TRxCustomDBColorBox = class(TCustomColorBox)
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure LayoutChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    function IsReadOnly: boolean;
  protected
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    { Published declarations }
  end;

  TRxDBColorBox = class(TRxCustomDBColorBox)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    property DataField;
    property DataSource;
    property ReadOnly;

    property DefaultColorColor;
    property NoneColorColor;
    property Selected;
    property Style;
    property OnGetColors;

    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemWidth;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

implementation

type
  TFieldDataLinkHack = class(TFieldDataLink)
  end;


{ TRxCustomDBColorBox }

procedure TRxCustomDBColorBox.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in [ftString, ftInteger, ftLargeint]) then
  begin
    if FDatalink.Field.DataType in [ftString] then
    begin
      if FDatalink.Field.AsString<>'' then
      try
        Selected:=StringToColor(FDatalink.Field.AsString)
      except
        Selected:=clNone;
      end
      else
        Selected:=clNone;
    end
    else
    if FDataLink.Field.DataType in [ftInteger, ftLargeint] then
    begin
      try
        Selected:=TColor(FDatalink.Field.AsInteger);
      except
        Selected:=clNone;
      end;
    end;
  end
  else
  begin
    Selected := clNone;
  end;
end;

function TRxCustomDBColorBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TRxCustomDBColorBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TRxCustomDBColorBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TRxCustomDBColorBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRxCustomDBColorBox.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TRxCustomDBColorBox.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TRxCustomDBColorBox.SetReadOnly(const AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := AValue;
end;

procedure TRxCustomDBColorBox.UpdateData(Sender: TObject);
begin
  if FDataLink.Field.DataType in [ftString] then
    FDataLink.Field.AsString := ColorToString(Selected)
  else
  if FDataLink.Field.DataType in [ftInteger, ftLargeint] then
    FDataLink.Field.AsInteger := Integer(Selected);
end;

procedure TRxCustomDBColorBox.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

procedure TRxCustomDBColorBox.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Selected := clNone;
    FDataLink.Reset;
  end;
end;

procedure TRxCustomDBColorBox.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TRxCustomDBColorBox.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

function TRxCustomDBColorBox.IsReadOnly: boolean;
begin
  Result := true;
  if FDatalink.Active and (not Self.ReadOnly) then
    Result := (Field = nil) or Field.ReadOnly;
end;

procedure TRxCustomDBColorBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key=VK_ESCAPE then
  begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if Key=VK_DELETE then
  begin
    if not IsReadOnly then
      FDatalink.Edit;
  end
  else
  if Key=VK_TAB then
  begin
    if FDataLink.CanModify and FDatalink.Editing then
      FDataLink.UpdateRecord;
  end;
end;

procedure TRxCustomDBColorBox.Change;
begin
  FDatalink.Edit;
  FDataLink.Modified;
  inherited Change;
end;

procedure TRxCustomDBColorBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TRxCustomDBColorBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TRxCustomDBColorBox.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset;
end;

procedure TRxCustomDBColorBox.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset
  else
    TFieldDataLinkHack(FDatalink).UpdateData;
end;

constructor TRxCustomDBColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnLayoutChange := @LayoutChange;
end;

destructor TRxCustomDBColorBox.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.
