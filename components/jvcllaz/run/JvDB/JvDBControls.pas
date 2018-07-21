{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBCtrl.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

  Lazarus port: Michal Gawrycki

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

    === NEW IN JVCL 3.0 ==
        TJvDBMaskEdit is a new control, added by Warren Postma.

    Major Issues:
        EditMask property enables operation as masked edit, which doesn't
        work properly in a Control Grid, yet, if you set the EditMask.
        You can use it as a generic editor control inside a control grid.
          -- Warren Postma (warrenpstma att hotmail dott com)
-----------------------------------------------------------------------------}
// $Id$

unit JvDBControls;

interface

uses
  JvBaseEdits, DB, DBCtrls, Classes, LMessages, GroupedEdit;

type

  { TJvDBEbEdit }

  TJvDBEbEdit = class(TJvEbEdit)
    procedure WMCut(var Msg: TLMessage); message LM_CUT;
    procedure WMPaste(var Msg: TLMessage); message LM_PASTE;
  end;

  { TJvDBCalcEdit }

  TJvDBCalcEdit = class(TJvCalcEdit)
  private
    FDataLink: TFieldDataLink;
    FDefaultParams: Boolean;
    //Polaris
    FLEmptyIsNull: Boolean;
    FEmptyIsNull: Boolean;
    procedure SetEmptyIsNull(AValue: Boolean);
    function GetZeroEmpty: Boolean;
    procedure SetZeroEmpty(AValue: Boolean);
    function StoreEmptyIsNull: Boolean;
    //Polaris
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDefaultParams(AValue: Boolean);
    procedure UpdateFieldData(Sender: TObject);
    procedure CMGetDataLink(var Msg: TLMessage); message CM_GETDATALINK;
    function GetReadOnly: Boolean; reintroduce;
    procedure SetReadOnly(AValue: Boolean); reintroduce;
  protected
    function GetEditorClassType: TGEEditClass; override;
    procedure AcceptValue(AValue: Double); override;
    procedure DoExit; override;
    function GetDisplayText: string; override;
    procedure EditChange; override;
    procedure SetText(const AValue: string); override;

    procedure DataChanged; override; //Polaris

    function EditCanModify: Boolean; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure EditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EditKeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Reset; override;
    //Polaris
    procedure Loaded; override;
    //Polaris
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFieldParams;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
    property Value;
  published
    property Align;
    property DecimalPlaceRound;

    property Action;
    property AutoSize;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultParams: Boolean read FDefaultParams write SetDefaultParams default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Alignment;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property CheckOnExit;
    property Color;
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property ImageIndex;
    property Images;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    //Polaris
    property EmptyIsNull: Boolean read FEmptyIsNull write SetEmptyIsNull stored StoreEmptyIsNull;
    property ZeroEmpty: Boolean read GetZeroEmpty write SetZeroEmpty default True;
    //Polaris
    property OnButtonClick;
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
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  SysUtils, LCLType, JvConsts, JvJCLUtils, Math, FmtBCD, Variants;

function IsNullOrEmptyStringField(Field: TField): Boolean;
begin
  Result := Field.IsNull or ((Field is TStringField) and (Trim(Field.AsString) = ''));
end;

{ TJvDBEbEdit }

procedure TJvDBEbEdit.WMCut(var Msg: TLMessage);
begin
  if Owner is TJvDBCalcEdit then
    with Owner as TJvDBCalcEdit do
      FDataLink.Edit;
  inherited;
end;

procedure TJvDBEbEdit.WMPaste(var Msg: TLMessage);
begin
  if Owner is TJvDBCalcEdit then
    with Owner as TJvDBCalcEdit do
      FDataLink.Edit;
  inherited;
end;

//=== { TJvDBCalcEdit } ======================================================

constructor TJvDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Polaris
  FEmptyIsNull := ZeroEmpty;
  FLEmptyIsNull := True;
  //Polaris
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnEditingChange := @EditingChange;
  FDataLink.OnUpdateData := @UpdateFieldData;
  inherited ReadOnly := True;
end;

destructor TJvDBCalcEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBCalcEdit.Loaded;
begin
  inherited Loaded;
  FLEmptyIsNull := True;
end;

procedure TJvDBCalcEdit.SetEmptyIsNull(AValue: Boolean);
begin
  if AValue <> FEmptyIsNull then
  begin
    FEmptyIsNull := AValue;
    if csLoading in ComponentState then
      FLEmptyIsNull := False;
  end;
end;

function TJvDBCalcEdit.GetZeroEmpty: Boolean;
begin
  Result := inherited ZeroEmpty;
end;

procedure TJvDBCalcEdit.SetZeroEmpty(AValue: Boolean);
begin
  inherited ZeroEmpty := AValue;
  if FLEmptyIsNull then
    SetEmptyIsNull(ZeroEmpty)
end;

function TJvDBCalcEdit.StoreEmptyIsNull: Boolean;
begin
  Result := FEmptyIsNull <> ZeroEmpty;
end;

//Polaris

procedure TJvDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBCalcEdit.EditKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and
    ((Key = VK_DELETE) and (Shift * KeyboardShiftStates = [])) or
    ((Key = VK_INSERT) and (Shift * KeyboardShiftStates = [ssShift])) then
    FDataLink.Edit;
end;

procedure TJvDBCalcEdit.EditKeyPress(var Key: Char);
begin
  inherited EditKeyPress(Key);
  case Key of
    CtrlH, CtrlV, CtrlX, #32..High(Char):
        FDataLink.Edit;
    Cr: if FDataLink.CanModify then
      FDataLink.UpdateRecord;
    Esc:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TJvDBCalcEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if Result and (FDataLink.Field <> nil) then
    Result := FDataLink.Field.IsValidChar(Key);
end;

function TJvDBCalcEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

function TJvDBCalcEdit.GetDisplayText: string;
begin
  if FDataLink.Field = nil then
  begin
    if csDesigning in ComponentState then
      Result := Format('(%s)', [Name])
    else
      Result := '';
  end
  else
  //Polaris Result := inherited GetDisplayText;
  if FDataLink.Field.IsNull then
    Result := ''
  else
    Result := inherited GetDisplayText;
  //Polaris
end;

procedure TJvDBCalcEdit.Reset;
begin
  FDataLink.Reset;
  inherited Reset;
end;

procedure TJvDBCalcEdit.EditChange;
begin
  if not Formatting then
    FDataLink.Modified;
  inherited EditChange;
end;

procedure TJvDBCalcEdit.SetText(const AValue: string);
begin
  if not ReadOnly then
    inherited SetText(AValue);
end;

//Polaris
procedure TJvDBCalcEdit.DataChanged;
begin
  inherited;
  if Assigned(FDataLink) and Assigned(FDataLink.Field) {and DecimalPlaceRound} then
  begin
    EditText := DisplayText;
    try
      if EditText <> '' then
        if (StrToFloat(TextToValText(EditText)) = 0) and ZeroEmpty then
          EditText := '';
    except
    end;
  end;
end;
//Polaris

function TJvDBCalcEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBCalcEdit.SetDataSource(AValue: TDataSource);
begin
  if FDataLink.DataSource <> AValue then
  begin
    if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    begin
      if FDataLink.DataSource <> nil then
        FDataLink.DataSource.RemoveFreeNotification(Self);
      FDataLink.DataSource := AValue;
    end;
    if AValue <> nil then
      AValue.FreeNotification(Self);
    UpdateFieldParams;
  end;
end;

function TJvDBCalcEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBCalcEdit.SetDataField(const AValue: string);
begin
  if FDataLink.FieldName <> AValue then
  begin
    FDataLink.FieldName := AValue;
    UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.SetDefaultParams(AValue: Boolean);
begin
  if DefaultParams <> AValue then
  begin
    FDefaultParams := AValue;
    if FDefaultParams then
      UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.UpdateFieldParams;
begin
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field is TNumericField then
    begin
      if TNumericField(FDataLink.Field).DisplayFormat <> '' then
        DisplayFormat := TNumericField(FDataLink.Field).DisplayFormat;
      Alignment := TNumericField(FDataLink.Field).Alignment;
    end;
    if FDataLink.Field is TLargeintField then
    begin
      MaxValue := TLargeintField(FDataLink.Field).MaxValue;
      MinValue := TLargeintField(FDataLink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end
    else
    if FDataLink.Field is TIntegerField then
    begin
      MaxValue := TIntegerField(FDataLink.Field).MaxValue;
      MinValue := TIntegerField(FDataLink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end
    else
    if FDataLink.Field is TBCDField then
    begin
      MaxValue := TBCDField(FDataLink.Field).MaxValue;
      MinValue := TBCDField(FDataLink.Field).MinValue;
    end
    else
    if FDataLink.Field is TFloatField then
    begin
      MaxValue := TFloatField(FDataLink.Field).MaxValue;
      MinValue := TFloatField(FDataLink.Field).MinValue;
        //Polaris      DecimalPlaces := TFloatField(FDataLink.Field).Precision;
      DecimalPlaces := Min(DecimalPlaces, TFloatField(FDataLink.Field).Precision);
    end
    else
    if FDataLink.Field is TBooleanField then
    begin
      MinValue := 0;
      MaxValue := 1;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end;
  end;
end;

function TJvDBCalcEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBCalcEdit.SetReadOnly(AValue: Boolean);
begin
  FDataLink.ReadOnly := AValue;
end;

function TJvDBCalcEdit.GetEditorClassType: TGEEditClass;
begin
  Result := TJvDBEbEdit;
end;

procedure TJvDBCalcEdit.AcceptValue(AValue: Double);
begin
  FDataLink.Field.Value := CheckValue(AValue, False);
  DataChange(nil);
end;

function TJvDBCalcEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBCalcEdit.DataChange(Sender: TObject);
begin
  if FDefaultParams then
    UpdateFieldParams;
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field.IsNull then
    begin
      Self.Value := 0.0;
      EditText := '';
    end
    else
    if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      Self.AsInteger := FDataLink.Field.AsInteger
    else
    if FDataLink.Field.DataType = ftBoolean then
      Self.AsInteger := Ord(FDataLink.Field.AsBoolean)
    else
    if FDataLink.Field is TLargeintField then
      Self.Value := TLargeintField(FDataLink.Field).AsLargeInt
    else
      Self.Value := FDataLink.Field.AsFloat;
    DataChanged;
  end
  else
  begin
    if csDesigning in ComponentState then
    begin
      Self.Value := 0;
      EditText := Format('(%s)', [Name]);
    end
    else
      Self.Value := 0;
  end;
end;

procedure TJvDBCalcEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBCalcEdit.UpdateFieldData(Sender: TObject);
begin
  inherited UpdateData;
  //Polaris  if (Value = 0) and ZeroEmpty then FDataLink.Field.Clear
  if (Trim(Text) = '') and FEmptyIsNull then
    FDataLink.Field.Clear
      //if (Value = 0) and ZeroEmpty then
//  FDataLink.Field.Clear
  else

  case FDataLink.Field.DataType of
    ftSmallint,
    ftInteger,
    ftWord:
      begin
        FDataLink.Field.AsInteger := Self.AsInteger;
      end;
    ftBoolean:
      begin
        FDataLink.Field.AsBoolean := Boolean(Self.AsInteger);
      end;
    ftFMTBcd,
    ftBCD:
      begin
        FDataLink.Field.AsBCD := DoubleToBCD(Self.Value)
      end;
    else
      begin
        FDataLink.Field.AsFloat := Self.Value;
      end;
  end;
end;

procedure TJvDBCalcEdit.CMGetDataLink(var Msg: TLMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

// Polaris
procedure TJvDBCalcEdit.DoExit;
begin
  if Modified then
  try
    CheckRange;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  inherited DoExit;
end;

function TJvDBCalcEdit.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(AAction);
end;

function TJvDBCalcEdit.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or (FDataLink <> nil) and
    FDataLink.UpdateAction(AAction);
end;

end.

