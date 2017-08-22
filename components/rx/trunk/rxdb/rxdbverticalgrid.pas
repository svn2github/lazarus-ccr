{ rxdbgrid unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$I rx.inc}

unit rxdbverticalgrid;

interface

uses
  Classes, SysUtils, Grids, Graphics, Controls, DB, Menus;

type
  TRxDBVerticalGridOption = (rxvgColumnTitle);
  TRxDBVerticalGridOptions = set of TRxDBVerticalGridOption;

  TRxDBVerticalGridRowStyle = (rxvrData, rxvrStaticText);


type
  TRxCustomDBVerticalGrid = class;
  TRxDBVerticalGridRow = class;
  TRxDBVerticalGridRowsEnumerator = class;

  { TRxDBVerticalGridDataLink }

  TRxDBVerticalGridDataLink = class(TDataLink)
  private
    FDataControl:TRxCustomDBVerticalGrid;
  protected
    constructor Create(ADataControl:TRxCustomDBVerticalGrid);
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  end;

  { TRxDBVerticalGridRowTitle }

  TRxDBVerticalGridRowTitle = class(TPersistent)
  private
    FFont: TFont;
    FRow: TRxDBVerticalGridRow;
    FisDefaultTitleFont : Boolean;
    FIsDefaultCaption: boolean;

    FAlignment: ^TAlignment;
    FColor: ^TColor;
    FCaption: PChar;

    function GetAlignment: TAlignment;
    function GetCaption: TCaption;
    function GetColor: TColor;
    function GetFont: TFont;
    function IsAlignmentStored: Boolean;
    function IsCaptionStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure FontChanged(Sender: TObject);
    property IsDefaultFont: boolean read FIsDefaultTitleFont;
  protected
    function GetDefaultCaption: string; virtual;
    function GetDefaultAlignment: TAlignment;
    function GetDefaultColor: TColor;
  public
    constructor Create(ARow: TRxDBVerticalGridRow);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure FillTitleDefaultFont;
    function IsDefault: boolean;
    property Row: TRxDBVerticalGridRow read FRow;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
  end;

  { TRxDBVerticalGridRow }

  TRxDBVerticalGridRow = class(TCollectionItem)
  private
    FButtonStyle: TColumnButtonStyle;
    FColor: ^TColor;
    FAlignment: ^TAlignment;
    FPopupMenu: TPopupMenu;
    FRowHeight: PInteger;

    FFieldName: String;
    FDisplayFormat : String;
    FDisplayFormatChanged: boolean;
    FField: TField;
    FGroupName: string;
    FKeyList: TStringList;
    FPickList: TStringList;

    FFont: TFont;
    FImageList: TImageList;
    FisDefaultFont: Boolean;
    FReadOnly: Boolean;
    FRowTitle: TRxDBVerticalGridRowTitle;
    FStaticText: string;
    FStyle: TRxDBVerticalGridRowStyle;
    FWordWrap: Boolean;

    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetDisplayFormat: string;
    function GetField: TField;
    function GetFont: TFont;
    function GetGrid: TRxCustomDBVerticalGrid;
    function GetKeyList: TStrings;
    function GetPickList: TStrings;
    function GetRowHeight: Integer;
    function IsAlignmentStored: Boolean;
    function IsColorStored: Boolean;
    function IsDisplayFormatStored: Boolean;
    function IsFontStored: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetButtonStyle(AValue: TColumnButtonStyle);
    procedure SetColor(AValue: TColor);
    procedure SetDisplayFormat(AValue: string);
    procedure SetField(AValue: TField);
    procedure SetFieldName(AValue: String);
    procedure SetFont(AValue: TFont);
    procedure SetGroupName(AValue: string);
    procedure SetImageList(AValue: TImageList);
    procedure SetKeyList(AValue: TStrings);
    procedure SetPickList(AValue: TStrings);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetRowHeight(AValue: Integer);
    procedure SetRowTitle(AValue: TRxDBVerticalGridRowTitle);
    procedure LinkField;
    procedure ApplyDisplayFormat;
    procedure FontChanged(Sender: TObject);
    procedure KeyListChanged(Sender: TObject);
    procedure SetStaticText(AValue: string);
    procedure SetStyle(AValue: TRxDBVerticalGridRowStyle);
    procedure SetWordWrap(AValue: Boolean);
  protected
    function  GetDisplayName: string; override;
    function  GetDefaultAlignment : TAlignment; virtual;
    function  GetDefaultRowHeight : integer;
    procedure RowChanged;
    function  GetDefaultDisplayFormat: string;
    property  IsDefaultFont: boolean read FIsDefaultFont;
    function  GetDefaultColor: TColor; virtual;
    //property GroupName:string read FGroupName write SetGroupName;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure FillDefaultFont;
    procedure Assign(Source: TPersistent); override;
    property  Field: TField read GetField write SetField;
    property  Grid: TRxCustomDBVerticalGrid read GetGrid;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle default cbsAuto;
    property FieldName: String read FFieldName write SetFieldName;
    property RowTitle:TRxDBVerticalGridRowTitle read FRowTitle write SetRowTitle;

    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat stored IsDisplayFormatStored;
    property StaticText:string read FStaticText write SetStaticText;

    property ImageList: TImageList read FImageList write SetImageList;
    property KeyList: TStrings read GetKeyList write SetKeyList;
    property PickList: TStrings read GetPickList write SetPickList;
    property Style : TRxDBVerticalGridRowStyle read FStyle write SetStyle default rxvrData;
    property WordWrap : Boolean read FWordWrap write SetWordWrap;
    property RowHeight : Integer read GetRowHeight write SetRowHeight;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly;
    property PopupMenu : TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

  { TRxDBVerticalGridRows }

  TRxDBVerticalGridRows = class(TOwnedCollection)
  private
    function GetRow(Index: Integer): TRxDBVerticalGridRow;
    procedure SetRow(Index: Integer; AValue: TRxDBVerticalGridRow);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    procedure LinkField;
    function Add: TRxDBVerticalGridRow;
    property Items[Index: Integer]: TRxDBVerticalGridRow read GetRow write SetRow; default;
    function GetEnumerator: TRxDBVerticalGridRowsEnumerator;
  end;

  { TRxDBVerticalGridRowsEnumerator }

  TRxDBVerticalGridRowsEnumerator = class
  private
    FList: TRxDBVerticalGridRows;
    FPosition: Integer;
  public
    constructor Create(AList: TRxDBVerticalGridRows);
    function GetCurrent: TRxDBVerticalGridRow;
    function MoveNext: Boolean;
    property Current: TRxDBVerticalGridRow read GetCurrent;
  end;

  { TRxCustomDBVerticalGrid }

  TRxCustomDBVerticalGrid = class(TCustomGrid)
  private
    FDataLink: TRxDBVerticalGridDataLink;
    FOptions: TRxDBVerticalGridOptions;
    FReadOnly: Boolean;
    FRows: TRxDBVerticalGridRows;
    FLinkActive:integer;
    function GetDataCoumn: TGridColumn;
    function GetDataSource: TDataSource;
    function GetLabelCoumn: TGridColumn;
    procedure SetDataCoumn(AValue: TGridColumn);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetLabelCoumn(AValue: TGridColumn);
    procedure SetOptions(AValue: TRxDBVerticalGridOptions);
    procedure SetRows(AValue: TRxDBVerticalGridRows);
    procedure RecordChanged;
    procedure RowsChanged(aRow: TRxDBVerticalGridRow);
  protected
    procedure DrawCell(aCol,aRow:Integer; aRect:TRect; aState:TGridDrawState); override;
    procedure DrawDataCell(aCol, aRow:Integer; aRect:TRect; aState:TGridDrawState; AGridRow: TRxDBVerticalGridRow);
    procedure PrepareCanvas(aCol, aRow: Integer; aState:TGridDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure LinkActive(Value: Boolean); virtual;
    procedure BeginLink;
    procedure EndLink;

    property Rows:TRxDBVerticalGridRows read FRows write SetRows;
    property Options:TRxDBVerticalGridOptions read FOptions write SetOptions;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property LabelCoumn:TGridColumn read GetLabelCoumn write SetLabelCoumn;
    property DataCoumn:TGridColumn read GetDataCoumn write SetDataCoumn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TRxDBVerticalGrid = class(TRxCustomDBVerticalGrid)
  published
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoEdit;
    property AutoFillColumns;
    property BiDiMode;
    property BorderColor;
    property BorderSpacing;
    property BorderStyle;
    property CellHintPriority;
    property Color;
    property DataCoumn;
    property DataSource;
    property DefaultRowHeight;
    property FixedColor;
    property FixedHotColor;
    property FocusColor;
    property LabelCoumn;
    property Options;
    property ReadOnly;
    property Rows;
    property TabStop;
    property TitleStyle;
  end;

implementation
uses rxlclutils;

{ TRxDBVerticalGridRowsEnumerator }

constructor TRxDBVerticalGridRowsEnumerator.Create(AList: TRxDBVerticalGridRows
  );
begin
  FList := AList;
  FPosition := -1;
end;

function TRxDBVerticalGridRowsEnumerator.GetCurrent: TRxDBVerticalGridRow;
begin
  Result := FList[FPosition];
end;

function TRxDBVerticalGridRowsEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TRxDBVerticalGridRows }

function TRxDBVerticalGridRows.GetRow(Index: Integer): TRxDBVerticalGridRow;
begin
  Result:=TRxDBVerticalGridRow(inherited Items[Index]);
end;

procedure TRxDBVerticalGridRows.SetRow(Index: Integer;
  AValue: TRxDBVerticalGridRow);
begin
  Items[Index].Assign(AValue);
end;

procedure TRxDBVerticalGridRows.Update(Item: TCollectionItem);
begin
  (Owner as TRxCustomDBVerticalGrid).RowsChanged(TRxDBVerticalGridRow(Item));
end;

procedure TRxDBVerticalGridRows.LinkField;
var
  FGrid: TRxCustomDBVerticalGrid;
  i: Integer;
begin
  FGrid:=TRxCustomDBVerticalGrid(Owner);
  FGrid.BeginLink;
  for i:=0 to Count-1 do
    Items[i].LinkField;
  FGrid.EndLink;
end;

function TRxDBVerticalGridRows.Add: TRxDBVerticalGridRow;
begin
  Result:=inherited Add as TRxDBVerticalGridRow;
end;

function TRxDBVerticalGridRows.GetEnumerator: TRxDBVerticalGridRowsEnumerator;
begin
  Result:=TRxDBVerticalGridRowsEnumerator.Create(Self);
end;

{ TRxDBVerticalGridDataLink }

constructor TRxDBVerticalGridDataLink.Create(
  ADataControl: TRxCustomDBVerticalGrid);
begin
  inherited Create;
  FDataControl:=ADataControl;
end;

procedure TRxDBVerticalGridDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  if Active then
  else
  begin
    BufferCount:=0;

  end;
  FDataControl.LinkActive(Active);
end;

procedure TRxDBVerticalGridDataLink.LayoutChanged;
begin
  inherited LayoutChanged;
end;

procedure TRxDBVerticalGridDataLink.FocusControl(Field: TFieldRef);
begin
  inherited FocusControl(Field);
end;

procedure TRxDBVerticalGridDataLink.RecordChanged(Field: TField);
begin
  inherited RecordChanged(Field);
  FDataControl.RecordChanged;
end;

procedure TRxDBVerticalGridDataLink.UpdateData;
begin
  inherited UpdateData;
end;

{ TRxDBVerticalGridRowTitle }

function TRxDBVerticalGridRowTitle.GetAlignment: TAlignment;
begin
  if FAlignment = nil then
    result := GetDefaultAlignment
  else
    result := FAlignment^;
end;

function TRxDBVerticalGridRowTitle.GetCaption: TCaption;
begin
  if (FCaption = nil) and FIsDefaultCaption then
    result := GetDefaultCaption
  else
    result := FCaption;
end;

function TRxDBVerticalGridRowTitle.GetColor: TColor;
begin
  if FColor = nil then
    result := GetDefaultColor
  else
    result := FColor^;
end;

function TRxDBVerticalGridRowTitle.GetFont: TFont;
begin
  Result := FFont;
end;

function TRxDBVerticalGridRowTitle.IsAlignmentStored: Boolean;
begin
  result := FAlignment <> nil;
end;

function TRxDBVerticalGridRowTitle.IsCaptionStored: Boolean;
begin
  result := Assigned(FCaption);
end;

function TRxDBVerticalGridRowTitle.IsColorStored: Boolean;
begin
  result := FColor <> nil;
end;

function TRxDBVerticalGridRowTitle.IsFontStored: Boolean;
begin
  result := not IsDefaultFont;
end;

procedure TRxDBVerticalGridRowTitle.SetAlignment(AValue: TAlignment);
begin
  if Falignment = nil then
  begin
    if AValue = GetDefaultAlignment then
      exit;
    New(Falignment)
  end
  else
  if FAlignment^ = AValue then
    exit;
  FAlignment^ := AValue;
  FRow.RowChanged;
end;

procedure TRxDBVerticalGridRowTitle.SetCaption(AValue: TCaption);
begin
  if (FCaption=nil) or (AValue<>StrPas(FCaption)) then
  begin
    if FCaption<>nil
    then
      StrDispose(FCaption);
    FCaption := StrNew(PChar(AValue));
    FIsDefaultCaption := false;
    FRow.RowChanged;
  end;
end;

procedure TRxDBVerticalGridRowTitle.SetColor(AValue: TColor);
begin
  if FColor=nil then
  begin
    if AValue = GetDefaultColor then
      exit;
    New(FColor)
  end
  else
  if FColor^=AValue then
    exit;
  FColor^ := AValue;
  FRow.RowChanged;
end;

procedure TRxDBVerticalGridRowTitle.SetFont(AValue: TFont);
begin
  if not FFont.IsEqual(AValue) then
    FFont.Assign(AValue);
end;

procedure TRxDBVerticalGridRowTitle.FontChanged(Sender: TObject);
begin
  FisDefaultTitleFont := False;
  FRow.RowChanged;
end;

function TRxDBVerticalGridRowTitle.GetDefaultCaption: string;
begin
  if Row.FieldName<>'' then
  begin
    if Row.FField<>nil then
      Result := Row.FField.DisplayName
    else
      Result := Row.FieldName;
  end
  else
    Result := 'Title'
end;

function TRxDBVerticalGridRowTitle.GetDefaultAlignment: TAlignment;
begin
  result := taLeftJustify
end;

function TRxDBVerticalGridRowTitle.GetDefaultColor: TColor;
begin
  if FRow.Grid <> nil then
    result := FRow.Grid.FixedColor
  else
    result := clWindow;
end;

constructor TRxDBVerticalGridRowTitle.Create(ARow: TRxDBVerticalGridRow);
begin
  inherited Create;
  FRow:=ARow;

  FIsDefaultTitleFont := True;
  FFont := TFont.Create;
  FillTitleDefaultFont;
  FFont.OnChange := @FontChanged;
{  FImageIndex := -1;
  FOldImageIndex := -1;
  FImageLayout := blGlyphRight;
  FIsDefaultCaption := true;}
end;

destructor TRxDBVerticalGridRowTitle.Destroy;
begin
  FreeAndNil(FFont);
  if Assigned(FAlignment) then Dispose(FAlignment);
  if Assigned(FColor) then Dispose(FColor);
  if Assigned(FCaption) then StrDispose(FCaption); //DisposeStr(FCaption);
//  if FLayout<>nil then Dispose(FLayout);
  inherited Destroy;
end;

procedure TRxDBVerticalGridRowTitle.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRxDBVerticalGridRowTitle then
  begin
    Alignment:=TRxDBVerticalGridRowTitle(Source).Alignment;
    Caption:=TRxDBVerticalGridRowTitle(Source).Caption;
    Color:=TRxDBVerticalGridRowTitle(Source).Color;
    Font:=TRxDBVerticalGridRowTitle(Source).Font;
  end;
end;

procedure TRxDBVerticalGridRowTitle.FillTitleDefaultFont;
var
  AGrid: TRxCustomDBVerticalGrid;
begin
  AGrid :=  FRow.Grid;
  if AGrid<>nil then
    FFont.Assign( AGrid.TitleFont )
  else
    FFont.Assign( FRow.Font );
  FIsDefaultTitleFont := True;
end;

function TRxDBVerticalGridRowTitle.IsDefault: boolean;
begin
  Result :=  (FAlignment = nil) and (FColor = nil) and (FCaption = nil) and
    IsDefaultFont {and (FLayout = nil) and
    (FImageIndex = 0) and (FImageLayout = blGlyphRight)};
end;

{ TRxDBVerticalGridRow }

function TRxDBVerticalGridRow.GetField: TField;
begin
  if (FFieldName<>'') and (FField = nil) then
    LinkField;
  result := FField;
end;

function TRxDBVerticalGridRow.GetFont: TFont;
begin
  Result := FFont;
end;

function TRxDBVerticalGridRow.GetGrid: TRxCustomDBVerticalGrid;
begin
  Result:= TRxCustomDBVerticalGrid(TRxDBVerticalGridRows(Collection).Owner);
end;

function TRxDBVerticalGridRow.GetKeyList: TStrings;
begin
  Result:=FKeyList;
end;

function TRxDBVerticalGridRow.GetPickList: TStrings;
begin
  Result:=FPickList;
end;

function TRxDBVerticalGridRow.GetRowHeight: Integer;
begin
  if FRowHeight = nil then
    result := GetDefaultRowHeight
  else
    result := FRowHeight^;
end;

function TRxDBVerticalGridRow.IsAlignmentStored: Boolean;
begin
  result := FAlignment <> nil;
end;


function TRxDBVerticalGridRow.IsColorStored: Boolean;
begin
  result := FColor <> nil;
end;

function TRxDBVerticalGridRow.GetDisplayFormat: string;
begin
  if not FDisplayFormatChanged then
    Result := GetDefaultDisplayFormat
  else
    result := FDisplayFormat;
end;

function TRxDBVerticalGridRow.GetColor: TColor;
begin
  if FColor=nil then
    result := GetDefaultColor
  else
    result := FColor^
end;

function TRxDBVerticalGridRow.GetAlignment: TAlignment;
begin
  if FAlignment=nil then
    Result := GetDefaultAlignment
  else
    Result := FAlignment^;
end;

function TRxDBVerticalGridRow.IsDisplayFormatStored: Boolean;
begin
  Result := FDisplayFormatChanged;
end;

function TRxDBVerticalGridRow.IsFontStored: Boolean;
begin
  result := not FisDefaultFont;
end;

procedure TRxDBVerticalGridRow.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = nil then
  begin
    if AValue=GetDefaultAlignment then
      exit;
    New(FAlignment);
  end
  else
  if FAlignment^ = AValue then
    exit;
  FAlignment^ := AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetButtonStyle(AValue: TColumnButtonStyle);
begin
  if FButtonStyle=AValue then Exit;
  FButtonStyle:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetColor(AValue: TColor);
begin
  if FColor = nil then
  begin
    if AValue=GetDefaultColor then
      exit;
    New(FColor)
  end
  else
  if FColor^ = AValue then
   exit;
  FColor^ := AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetDisplayFormat(AValue: string);
begin
  if (not FDisplayFormatChanged)or(CompareText(AValue, FDisplayFormat)<>0) then
  begin
    FDisplayFormat := AValue;
    FDisplayFormatChanged:=True;
    RowChanged;
  end;
end;

procedure TRxDBVerticalGridRow.SetField(AValue: TField);
begin
  if FField <> AValue then
  begin
    FField := AValue;
    if FField<>nil then
      FFieldName := FField.FieldName;
    RowChanged;
  end;
end;

procedure TRxDBVerticalGridRow.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then exit;
  FFieldName:=AValue;
  LinkField;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetFont(AValue: TFont);
begin
  if not FFont.IsEqual(AValue) then
  begin
    FFont.Assign(AValue);
    RowChanged;
  end;
end;

procedure TRxDBVerticalGridRow.SetGroupName(AValue: string);
begin
  if FGroupName=AValue then Exit;
  FGroupName:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetImageList(AValue: TImageList);
begin
  if FImageList=AValue then Exit;
  FImageList:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetKeyList(AValue: TStrings);
begin
  FKeyList.Assign(AValue);
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetPickList(AValue: TStrings);
begin
  FPickList.Assign(AValue);
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetPopupMenu(AValue: TPopupMenu);
begin
  if FPopupMenu=AValue then Exit;
  FPopupMenu:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly=AValue then Exit;
  FReadOnly:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetRowHeight(AValue: Integer);
var
  FGrid: TRxCustomDBVerticalGrid;
begin
  if FRowHeight = nil then
  begin
    if AValue=GetDefaultRowHeight then
      exit;
    New(FRowHeight);
  end
  else if FRowHeight^ = AValue then
    exit;
  FRowHeight^ := AValue;
  FGrid:=Grid;
  if Index + FGrid.FixedRows < FGrid.RowCount then
    FGrid.RowHeights[Index + FGrid.FixedRows]:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetRowTitle(AValue: TRxDBVerticalGridRowTitle);
begin
  FRowTitle.Assign(AValue);
  RowChanged;
end;

procedure TRxDBVerticalGridRow.LinkField;
var
  FGrid: TRxCustomDBVerticalGrid;
begin
  FGrid:=Grid;
  if (FGrid <> nil) and FGrid.FDataLink.Active then
  begin
    Field := FGrid.FDataLink.DataSet.FindField(FFieldName);
    ApplyDisplayFormat;
  end
  else
    Field := nil;
end;

procedure TRxDBVerticalGridRow.ApplyDisplayFormat;
begin
  if (FField <> nil) and FDisplayFormatChanged then
  begin
    if (FField is TNumericField) then
      TNumericField(FField).DisplayFormat := DisplayFormat
    else
    if (FField is TDateTimeField) then
      TDateTimeField(FField).DisplayFormat := DisplayFormat;
  end;
end;

procedure TRxDBVerticalGridRow.FontChanged(Sender: TObject);
begin
  FisDefaultFont := False;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.KeyListChanged(Sender: TObject);
begin
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetStaticText(AValue: string);
begin
  if FStaticText=AValue then Exit;
  FStaticText:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetStyle(AValue: TRxDBVerticalGridRowStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  RowChanged;
end;

procedure TRxDBVerticalGridRow.SetWordWrap(AValue: Boolean);
begin
  if FWordWrap=AValue then Exit;
  FWordWrap:=AValue;
end;

function TRxDBVerticalGridRow.GetDisplayName: string;
begin
  if RowTitle.Caption<>'' then
    Result := RowTitle.Caption
  else
    Result := 'GridColumn';
end;

function TRxDBVerticalGridRow.GetDefaultAlignment: TAlignment;
begin
{  if ButtonStyle in [cbsCheckboxColumn,cbsButtonColumn] then
    result := taCenter
  else}
    result := taLeftJustify;
end;

function TRxDBVerticalGridRow.GetDefaultRowHeight: integer;
begin
  Result:=Grid.DefaultRowHeight;
end;

procedure TRxDBVerticalGridRow.RowChanged;
begin
  Changed(False);
  //FWidthChanged := False;
end;

function TRxDBVerticalGridRow.GetDefaultDisplayFormat: string;
begin
  Result := '';
  if FField<>nil then
  begin
    if FField is TNumericField then
      Result := TNumericField(FField).DisplayFormat
    else
    if FField is TDateTimeField then
      Result := TDateTimeField(FField).DisplayFormat
  end;
end;

function TRxDBVerticalGridRow.GetDefaultColor: TColor;
var
  FGrid: TRxCustomDBVerticalGrid;
begin
  FGrid := Grid;
  if FGrid<>nil then
    result := FGrid.Color
  else
    result := clWindow
end;

constructor TRxDBVerticalGridRow.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FRowTitle:=TRxDBVerticalGridRowTitle.Create(Self);
  FKeyList:=TStringList.Create;
  FKeyList.OnChange:=@KeyListChanged;
  FPickList:=TStringList.Create;
  FPickList.OnChange:=@KeyListChanged;

  FIsDefaultFont := True;
  FFont := TFont.Create;
  FillDefaultFont;
  FFont.OnChange := @FontChanged;
  FButtonStyle:=cbsAuto;
  FStyle:=rxvrData;
end;

destructor TRxDBVerticalGridRow.Destroy;
begin
  FreeAndNil(FRowTitle);
  FreeAndNil(FFont);
  FreeAndNil(FKeyList);
  FreeAndNil(FPickList);
  if Assigned(FColor) then Dispose(FColor);
  if Assigned(FAlignment) then Dispose(FAlignment);
  if Assigned(FRowHeight) then Dispose(FRowHeight);

  inherited Destroy;
end;

procedure TRxDBVerticalGridRow.FillDefaultFont;
var
  AGrid: TRxCustomDBVerticalGrid;
begin
  AGrid := Grid;
  if (AGrid<>nil) then
  begin
    FFont.Assign(AGrid.Font);
    FIsDefaultFont := True;
  end;
end;

procedure TRxDBVerticalGridRow.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRxDBVerticalGridRow then
  begin
    FFieldName:=TRxDBVerticalGridRow(Source).FFieldName;
    FGroupName:=TRxDBVerticalGridRow(Source).FGroupName;
    FDisplayFormat:=TRxDBVerticalGridRow(Source).FDisplayFormat;
    FButtonStyle:=TRxDBVerticalGridRow(Source).ButtonStyle;
    Alignment:=TRxDBVerticalGridRow(Source).Alignment;
    RowTitle:=TRxDBVerticalGridRow(Source).RowTitle;

    FImageList:=TRxDBVerticalGridRow(Source).FImageList;
    KeyList:=TRxDBVerticalGridRow(Source).KeyList;
    PickList:=TRxDBVerticalGridRow(Source).PickList;
    FWordWrap:=TRxDBVerticalGridRow(Source).WordWrap;
    RowHeight:=TRxDBVerticalGridRow(Source).RowHeight;
    ReadOnly:=TRxDBVerticalGridRow(Source).ReadOnly;
//    PopupMenu:=TRxDBVerticalGridRow(Source).PopupMenu;
    Style:=TRxDBVerticalGridRow(Source).Style;
  end;
end;

{ TRxCustomDBVerticalGrid }

procedure TRxCustomDBVerticalGrid.SetRows(AValue: TRxDBVerticalGridRows);
begin
  FRows.Assign(AValue);
end;

procedure TRxCustomDBVerticalGrid.RecordChanged;
begin
  Invalidate;
end;

procedure TRxCustomDBVerticalGrid.RowsChanged(aRow: TRxDBVerticalGridRow);
begin
  if FLinkActive > 0 then Exit;

  if Assigned(aRow) then
  else
  begin
    if FRows.Count <> RowCount - FixedRows then
      RowCount:=FRows.Count + FixedRows;
  end;
end;

procedure TRxCustomDBVerticalGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  FRow: TRxDBVerticalGridRow;
begin
  inherited DrawCell(aCol, aRow, aRect, aState);

  if (rxvgColumnTitle in FOptions) and (aRow = 0) then
  begin
    if aCol = 0 then
      DrawCellText(aCol, aRow, aRect, aState, LabelCoumn.Title.Caption)
    else
      DrawCellText(aCol, aRow, aRect, aState, DataCoumn.Title.Caption)
  end
  else
  begin
    FRow:=nil;
    if FRows.Count > aRow - FixedRows then
      FRow:=FRows[aRow - FixedRows];

    if Assigned(FRow) then
    begin
      if aCol = 0 then
      begin
        DrawCellText(aCol,aRow, aRect, aState, FRow.RowTitle.Caption);
      end
      else
      if (aCol = 1) then
        DrawDataCell(aCol,aRow, aRect, aState, FRow);
    end;
  end;
end;

procedure TRxCustomDBVerticalGrid.DrawDataCell(aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState; AGridRow: TRxDBVerticalGridRow);
begin
  if AGridRow.Style = rxvrData then
  begin
    if Assigned(AGridRow.Field) then
      WriteTextHeader(Canvas, aRect, AGridRow.Field.DisplayText, AGridRow.Alignment);
  end
  else
  if AGridRow.Style = rxvrStaticText then
    if AGridRow.StaticText <> '' then
      WriteTextHeader(Canvas, aRect, AGridRow.StaticText, AGridRow.Alignment);
end;

procedure TRxCustomDBVerticalGrid.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
var
  FRow: TRxDBVerticalGridRow;
  CurrentTextStyle: TTextStyle;
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if (aRow - FixedRows >= 0) and (FRows.Count > aRow - FixedRows) then
  begin
    FRow:=FRows[aRow - FixedRows];
    if Assigned(FRow) then
    begin
      CurrentTextStyle:=Canvas.TextStyle;
      if aCol = 0 then
      begin
        CurrentTextStyle.Alignment := BidiFlipAlignment(FRow.RowTitle.Alignment, UseRightToLeftAlignment);
        if ([gdSelected, gdFocused] * aState = []) then
          Canvas.Brush.Color := FRow.RowTitle.Color
      end
      else
      if aCol = 1 then
        if ([gdSelected, gdFocused] * aState = []) then
          Canvas.Brush.Color := FRow.Color;
      Canvas.TextStyle := CurrentTextStyle;
    end;
  end;
end;

procedure TRxCustomDBVerticalGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  R: TRxDBVerticalGridRow;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent is TImageList then
    begin
      for R in Rows do
        if R.ImageList = TImageList(AComponent) then
          R.ImageList:=nil;
    end
    else
    if AComponent is TPopupMenu then
    begin
      for R in Rows do
        if R.PopupMenu = TPopupMenu(AComponent) then
          R.PopupMenu:=nil;
    end;
  end;
end;

procedure TRxCustomDBVerticalGrid.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Cell: TGridCoord;
  FRow1: TRxDBVerticalGridRow;
  R: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = mbRight then
  begin
    Cell := MouseCoord(X, Y);
    if (Cell.X = 1) and (Cell.Y >= FixedRows) and (Cell.Y - FixedRows < FRows.Count) then
    begin
      FRow1:=FRows[Cell.Y - FixedRows];

      if Assigned(FRow1) and Assigned(FRow1.PopupMenu) then
      begin
        R:=ClientToScreen(Point(X, Y));
        FRow1.PopupMenu.PopUp(R.X, R.Y);
      end;
    end;
  end;
end;

procedure TRxCustomDBVerticalGrid.LinkActive(Value: Boolean);
begin
  BeginLink;
  FRows.LinkField;
  EndLink;
  Invalidate;
end;

procedure TRxCustomDBVerticalGrid.BeginLink;
begin
  Inc(FLinkActive);
end;

procedure TRxCustomDBVerticalGrid.EndLink;
begin
  if FLinkActive > 0 then
    Dec(FLinkActive);

  if FLinkActive = 0 then
    RowsChanged(nil);
end;

procedure TRxCustomDBVerticalGrid.SetOptions(AValue: TRxDBVerticalGridOptions);
var
  O: TGridOptions;
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;

//  O:=inherited Options;
  if rxvgColumnTitle in FOptions then
  begin
    RowCount:=RowCount + 1;
    FixedRows:=1;
  end
  else
  begin
    RowCount:=RowCount - 1;
    FixedRows:=0;
  end;
end;

function TRxCustomDBVerticalGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
end;

function TRxCustomDBVerticalGrid.GetDataCoumn: TGridColumn;
begin
  Result:=Columns[1];
end;

function TRxCustomDBVerticalGrid.GetLabelCoumn: TGridColumn;
begin
  Result:=Columns[0];
end;

procedure TRxCustomDBVerticalGrid.SetDataCoumn(AValue: TGridColumn);
begin
  Columns[1].Assign(AValue);
end;

procedure TRxCustomDBVerticalGrid.SetDataSource(AValue: TDataSource);
begin
  if AValue = FDatalink.Datasource then Exit;
//  RenewColWidths;
  FDataLink.DataSource := AValue;
//  UpdateActive;
end;

procedure TRxCustomDBVerticalGrid.SetLabelCoumn(AValue: TGridColumn);
begin
  Columns[0].Assign(AValue);
end;

constructor TRxCustomDBVerticalGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:=TRxDBVerticalGridDataLink.Create(Self);
  FRows:=TRxDBVerticalGridRows.Create(Self, TRxDBVerticalGridRow);

  //ColCount:=2;
  FixedCols:=0;
  FixedRows:=0;
  Columns.Clear;
  Columns.Add;
  Columns.Add;
  Columns[0].ReadOnly:=true;

  inherited Options :=
    [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
     goSmoothScroll, goTabs, goEditing, goDrawFocusSelected,
     goColSizing ];
end;

destructor TRxCustomDBVerticalGrid.Destroy;
begin
  FreeAndNil(FRows);
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.

