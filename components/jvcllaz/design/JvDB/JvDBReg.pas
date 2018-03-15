unit JvDBReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, PropEdits, DBPropEdits;

procedure Register;

implementation

{$R ../../resource/jvdbreg.res}

uses
  Classes, JvDsgnConsts, {JvDBSearchCombobox,} JvDBSearchEdit, JvDBHTLabel; //, JvDBTreeView;

procedure Register;
const
//  cDataField = 'DataField';
//  cKeyField = 'KeyField';
//  cListField = 'ListField';
//  cDisplayField = 'DisplayField';
//  cListKeyField = 'ListKeyField';
  cMasterField = 'MasterField';
  cDetailField = 'DetailField';
  cIconField = 'IconField';
  cItemField = 'ItemField';
//  cLookupField = 'LookupField';
// cSectionField = 'SectionField';
//  cValueField = 'ValueField';
//  cEditControls = 'EditControls';
//  cSortedField = 'SortedField';
//  cSortMarker = 'SortMarker';

begin
  RegisterComponents(RsPaletteJvclDB, [     // was: TsPaletteDBVisual
    TJvDBSearchEdit,
 //   TJvDBSearchCombobox,
    TJvDBHtLabel
  //  TJvDBTreeView
  ]);
                (*
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cItemField, TFieldProperty); //TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cMasterField, TFieldProperty); //TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cDetailField, TFieldProperty); //TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cIconField, TFieldProperty); //TJvDataFieldProperty);
  *)
end;

end.

