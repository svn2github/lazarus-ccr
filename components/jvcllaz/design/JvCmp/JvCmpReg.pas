unit JvCmpReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvcmpreg.res}

uses
  Classes, PropEdits,
  JvDsgnConsts,
//  JvEnterTab,
  JvSpellChecker;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
//    TJvEnterAsTab,
    TJvSpellChecker
  ]);
end;

end.

