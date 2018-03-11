unit JvPageCompsReg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

{$R ..\..\resource\jvpagecompsreg.res}

uses
  JvDsgnConsts, JvNavigationPane;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [  // was: RsPaletteNavPane
    TJvNavigationPane,
    TJvNavIconButton,
    TJvNavPanelButton, TJvNavPanelHeader, TJvNavPanelDivider,
    TJvOutlookSplitter,
    TJvNavPaneStyleManager, TJvNavPaneToolPanel
  ]);
end;
                                      {
initialization
  {$I ..\..\resource\JvNavigationPaneLaz.lrs}
                                       }
end.

