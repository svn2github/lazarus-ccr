unit mbReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

//{$R mbReg.res}

uses
  RColorPicker, GColorPicker, BColorPicker,
  RAxisColorPicker, GAxisColorPicker, BAxisColorPicker,
  CColorPicker, YColorPicker, MColorPicker, KColorPicker,
  HRingPicker,
  HColorPicker, SColorPicker, LColorPicker, VColorPicker,
  HSColorPicker, HSVColorPicker, HSLColorPicker, HSLRingPicker,
  SLColorPicker, SLHColorPicker,
  CIEAColorPicker, CIEBColorPicker, CIELColorPicker,
  HexaColorPicker, mbColorPreview, mbColorList, mbColorTree, mbColorPalette,
  mbOfficeColorDialog, mbDeskPickerButton,
  LResources;

procedure Register;
begin
  RegisterComponents('mbColor Lib', [
    TRColorPicker, TGColorPicker, TBColorPicker,
    TRAxisColorPicker, TGAxisColorPicker, TBAxisColorPicker,
    TCColorPicker, TYColorPicker, TMColorPicker, TKColorPicker,
    THRingPicker,
    THColorPicker, TSColorPicker, TLColorPicker, TVColorPicker,
    THSColorPicker, THSVColorPicker, THSLColorPicker, THSLRingPicker,
    TSLColorPicker, TSLHColorPicker,
    TCIEAColorPicker, TCIEBColorPicker, TCIELColorPicker,
    THexaColorPicker, TmbColorPreview, TmbColorList, TmbColorTree, TmbColorPalette,
    TmbOfficeColorDialog, TmbDeskPickerButton]);
end;


{$IFDEF FPC}
initialization
  {$I mbReg.lrs}
{$ENDIF}

end.
