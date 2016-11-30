unit JvXPBarReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

uses
  JvXPBar, JvXPContainer;

procedure Register;
begin
  RegisterComponents('JvXP',[TJvXPBar, TJvXPContainer]);
end;

initialization
  {$I JvXPBarLaz.lrs}
  
end.

