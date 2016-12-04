unit JvXPBarReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

//{$R ../resource/JvXPCore.res}

uses
  JvXPBar, JvXPContainer, JvXPButtons, JvXPCheckCtrls;

procedure Register;
begin
  RegisterComponents('JvXPCtrls', [
    TJvXPBar,
    TJvXPContainer,
    TJvXPButton,
    TJvXPCheckbox
  ]);
end;

initialization
  {$I ../resource/JvXPBarLaz.lrs}
  
end.

