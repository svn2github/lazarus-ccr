unit JvRuntimeDesignReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
procedure Register;

implementation

{$R ../../resource/JvRuntimeDesign.res}

uses
  JvDsgnConsts, JvDesignSurface;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [//RsPaletteRuntimeDesign, [
    TJvDesignSurface, 
    TJvDesignScrollBox, 
    TJvDesignPanel
  ]);
end;

end.

