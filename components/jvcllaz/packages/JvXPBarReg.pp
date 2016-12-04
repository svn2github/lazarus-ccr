unit JvXPBarReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

uses
  JvXPBar, JvXPContainer, JvXPButtons;

procedure Register;
begin
  RegisterComponents('JvXP', [
    TJvXPBar,
    TJvXPContainer,
    TJvXPButton
  ]);
end;

initialization
  {$I JvXPBarLaz.lrs}
  
end.

