unit sRowHeightForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Spin, ExtCtrls, fpsTypes, fpspreadsheet;

type

  { TRowHeightForm }

  TRowHeightForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CbUnits: TComboBox;
    EdRowHeight: TFloatSpinEdit;
    LblRowHeight: TLabel;
    RbDefault: TRadioButton;
    RbAuto: TRadioButton;
    RbCustom: TRadioButton;
    procedure CbUnitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RowHeightTypeChanged(Sender: TObject);
  private
    FWorkbook: TsWorkbook;
    FOldUnits: TsSizeUnits;
    function GetRowHeight: Single;
    function GetRowHeightType: TsRowHeightType;
    function GetUnits: TsSizeUnits;
    procedure SetRowHeight(AValue: Single);
    procedure SetRowHeightType(AValue: TsRowHeightType);
    procedure SetUnits(AValue: TsSizeUnits);
    procedure SetWorkbook(AValue: TsWorkbook);
  protected

  public
    procedure SetData(AWorkbook: TsWorkbook; ARowHeight: single;
      ARowHeightType: TsRowHeightType);
    property RowHeight: Single read GetRowHeight;
    property RowHeightType: TsRowHeightType read GetRowHeightType;
    property Units: TsSizeUnits read GetUnits;
  end;

var
  RowHeightForm: TRowHeightForm;

implementation

{$R *.lfm}

uses
  fpsPatches;

{ TRowHeightForm }

procedure TRowHeightForm.CbUnitsChange(Sender: TObject);
begin
  if FWorkbook <> nil then
    EdRowHeight.Value := FWorkbook.ConvertUnits(EdRowHeight.Value, FOldUnits, GetUnits);
  FOldUnits := GetUnits;
end;

procedure TRowHeightForm.FormCreate(Sender: TObject);
begin
  CbUnits.Items.Clear;
  CbUnits.Items.AddObject('Lines', TObject(PtrInt(ord(suLines))));
  CbUnits.Items.AddObject('mm', TObject(PtrInt(ord(suMillimeters))));
  CbUnits.Items.AddObject('cm', TObject(PtrInt(ord(suCentimeters))));
  CbUnits.Items.AddObject('Points', TObject(PtrInt(ord(suPoints))));
  CbUnits.Items.AddObject('Inches', TObject(PtrInt(ord(suInches))));
end;

function TRowHeightForm.GetRowHeight: Single;
begin
  Result := EdRowHeight.Value;
end;

function TRowHeightForm.GetRowHeightType: TsRowHeightType;
begin
  if RbDefault.Checked then
    Result := rhtDefault
  else if RbAuto.Checked then
    Result := rhtAuto
  else
  Result := rhtCustom;
end;

function TRowHeightForm.GetUnits: TsSizeUnits;
begin
  if CbUnits.ItemIndex = -1 then
    Result := FWorkbook.Units else
    Result := TsSizeUnits(IntPtr(CbUnits.Items.Objects[CbUnits.ItemIndex]));
end;

procedure TRowHeightForm.RowHeightTypeChanged(Sender: TObject);
begin
  LblRowHeight.Enabled := RbCustom.Checked;
  EdRowHeight.Enabled := RbCustom.Checked;
  CbUnits.Enabled := RbCustom.Checked;
end;

procedure TRowHeightForm.SetData(AWorkbook: TsWorkbook; ARowHeight: Single;
  ARowHeightType: TsRowHeightType);
begin
  SetWorkbook(AWorkbook);
  SetRowHeight(ARowHeight);
  SetUnits(AWorkbook.Units);
  SetRowHeightType(ARowHeightType);
end;

procedure TRowHeightForm.SetRowHeight(AValue: Single);
begin
  EdRowHeight.Value := AValue;
end;

procedure TRowHeightForm.SetRowHeightType(AValue: TsRowHeightType);
begin
  RbDefault.Checked := AValue = rhtDefault;
  RbAuto.Checked := AValue= rhtAuto;
  RbCustom.Checked := AValue = rhtCustom;
  RowHeightTypeChanged(nil);
end;

procedure TRowHeightForm.SetUnits(AValue: TsSizeUnits);
var
  i: Integer;
begin
  FOldUnits := GetUnits;
  for i:=0 to CbUnits.Items.Count-1 do
    if TsSizeUnits(IntPtr(CbUnits.Items.Objects[i])) = AValue then
    begin
      CbUnits.ItemIndex := i;
      exit;
    end;
end;

procedure TRowHeightForm.SetWorkbook(AValue: TsWorkbook);
begin
  FWorkbook := AValue;
  FOldUnits := FWorkbook.Units;
end;

end.

