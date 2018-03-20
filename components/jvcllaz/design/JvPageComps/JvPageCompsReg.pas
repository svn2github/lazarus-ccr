unit JvPageCompsReg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

{$R ..\..\resource\jvpagecompsreg.res}

uses
  ImgList, PropEdits,
  JvDsgnConsts,
  JvNavigationPane, JvNavPaneEditors;

procedure Register;
const
  cImageIndex = 'ImageIndex';
begin
  RegisterComponents(RsPaletteJvcl, [  // was: RsPaletteNavPane
    TJvNavigationPane,
    TJvNavIconButton,
    TJvNavPanelButton, TJvNavPanelHeader, TJvNavPanelDivider,
    TJvOutlookSplitter,
    TJvNavPaneStyleManager, TJvNavPaneToolPanel
  ]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelPage, cImageIndex, TJvNavPanePageImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelHeader, cImageIndex, TJvNavPanelHeaderImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelButton, cImageIndex, TJvNavPanelButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavIconButton, cImageIndex, TJvNavIconButtonImageIndexProperty);
end;

end.

