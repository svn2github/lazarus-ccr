unit sColWidthForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Spin, ExtCtrls, fpsTypes, fpspreadsheet;

type

  { TColWidthForm }

  TColWidthForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CbUnits: TComboBox;
    EdColWidth: TFloatSpinEdit;
    LblColWidth: TLabel;
    RbDefault: TRadioButton;
    RbCustom: TRadioButton;
    procedure CbUnitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ColWidthTypeChanged(Sender: TObject);
  private
    FWorkbook: TsWorkbook;
    FOldUnits: TsSizeUnits;
    function GetColWidth: Single;
    function GetColWidthType: TsColWidthType;
    function GetUnits: TsSizeUnits;
    procedure SetColWidth(AValue: Single);
    procedure SetColWidthType(AValue: TsColWidthType);
    procedure SetUnits(AValue: TsSizeUnits);
    procedure SetWorkbook(AValue: TsWorkbook);
  protected

  public
    procedure SetData(AWorkbook: TsWorkbook; AColWidth: single;
      AColWidthType: TsColWidthType);
    property ColWidth: Single read GetColWidth;
    property ColWidthType: TsColWidthType read GetColWidthType;
    property Units: TsSizeUnits read GetUnits;
  end;

var
  ColWidthForm: TColWidthForm;

implementation

{$R *.lfm}

{ TColWidthForm }

procedure TColWidthForm.CbUnitsChange(Sender: TObject);
begin
  if FWorkbook <> nil then
    EdColWidth.Value := FWorkbook.ConvertUnits(EdColWidth.Value, FOldUnits, GetUnits);
  FOldUnits := GetUnits;
end;

procedure TColWidthForm.FormCreate(Sender: TObject);
begin
  CbUnits.Items.Clear;
  CbUnits.Items.AddObject('Characters', TObject(PtrInt(ord(suChars))));
  CbUnits.Items.AddObject('mm', TObject(PtrInt(ord(suMillimeters))));
  CbUnits.Items.AddObject('cm', TObject(PtrInt(ord(suCentimeters))));
  CbUnits.Items.AddObject('Points', TObject(PtrInt(ord(suPoints))));
  CbUnits.Items.AddObject('Inches', TObject(PtrInt(ord(suInches))));
end;

function TColWidthForm.GetColWidth: Single;
begin
  Result := EdColWidth.Value;
end;

function TColWidthForm.GetColWidthType: TsColWidthType;
begin
  if RbDefault.Checked then
    Result := cwtDefault
  else
    Result := cwtCustom;
end;

function TColWidthForm.GetUnits: TsSizeUnits;
begin
  if CbUnits.ItemIndex = -1 then
    Result := FWorkbook.Units else
    Result := TsSizeUnits(IntPtr(CbUnits.Items.Objects[CbUnits.ItemIndex]));
end;

procedure TColWidthForm.ColWidthTypeChanged(Sender: TObject);
begin
  LblColWidth.Enabled := RbCustom.Checked;
  EdColWidth.Enabled := RbCustom.Checked;
  CbUnits.Enabled := RbCustom.Checked;
end;

procedure TColWidthForm.SetData(AWorkbook: TsWorkbook; AColWidth: Single;
  AColWidthType: TsColWidthType);
begin
  SetWorkbook(AWorkbook);
  SetColWidth(AColWidth);
  SetUnits(AWorkbook.Units);
  SetColWidthType(AColWidthType);
end;

procedure TColWidthForm.SetColWidth(AValue: Single);
begin
  EdColWidth.Value := AValue;
end;

procedure TColWidthForm.SetColWidthType(AValue: TsColWidthType);
begin
  RbDefault.Checked := AValue = cwtDefault;
  RbCustom.Checked := AValue = cwtCustom;
  ColWidthTypeChanged(nil);
end;

procedure TColWidthForm.SetUnits(AValue: TsSizeUnits);
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

procedure TColWidthForm.SetWorkbook(AValue: TsWorkbook);
begin
  FWorkbook := AValue;
  FOldUnits := FWorkbook.Units;
end;

end.

