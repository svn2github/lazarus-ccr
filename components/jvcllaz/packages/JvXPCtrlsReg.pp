unit JvXPCtrlsReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

uses
  JvXPBar, JvXPContainer, JvXPButtons, JvXPCheckCtrls;

procedure Register;
begin
  RegisterComponents('JvXPCtrls', [
    TJvXPBar,
    TJvXPContainer,
    TJvXPButton,
    TJvXPToolButton,
    TJvXPCheckbox
  ]);
end;

initialization
  {$I ../resource/JvXPCtrlsLaz.lrs}
  
end.

