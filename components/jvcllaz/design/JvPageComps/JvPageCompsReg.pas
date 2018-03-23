unit JvPageCompsReg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

{$R ..\..\resource\jvpagecompsreg.res}

uses
  ImgList,
  PropEdits, ComponentEditors,
  JvDsgnConsts,
  JvNavigationPane, JvNavPaneEditors,
  JvPageList,JvPageListEditors, JvPageListTreeView;

procedure Register;
const
  cImageIndex = 'ImageIndex';
begin
  // JvNavigationPanel
  RegisterComponents(RsPaletteJvcl, [  // was: RsPaletteNavPane
    TJvNavigationPane,
    TJvNavIconButton,
    TJvNavPanelButton, TJvNavPanelHeader, TJvNavPanelDivider,
    TJvOutlookSplitter,
    TJvNavPaneStyleManager, TJvNavPaneToolPanel
  ]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelPage, cImageIndex,
    TJvNavPanePageImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelHeader, cImageIndex,
    TJvNavPanelHeaderImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelButton, cImageIndex,
    TJvNavPanelButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavIconButton, cImageIndex,
    TJvNavIconButtonImageIndexProperty);

  // JvPageList
  RegisterComponents(RsPaletteJvcl, [  // was: RsPaletteListComboTree
    TJvPageList]);
  RegisterClasses([TJvPageList, TJvStandardPage]);
  RegisterComponentEditor(TJvCustomPageList, TJvCustomPageListEditor);  // was: TJvCustomPageEditor
  RegisterComponentEditor(TJvCustomPage, TJvCustomPageEditor);
  RegisterPropertyEditor(TypeInfo(TJvShowDesignCaption), nil, '',
    TJvShowDesignCaptionProperty);
{
  RegisterPropertyEditor(TypeInfo(TJvCustomPage),
    TJvCustomPageList, cActivePage, TJvActivePageProperty);
}

  // JvPageTree
  RegisterComponents(RsPaletteJvcl, [  // was: TsPaletteListComboTree
    TJvSettingsTreeView, TJvPageListTreeView
  ]);
  RegisterClasses([TJvSettingsTreeView, TJvPageListTreeView]);

  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvSettingsTreeImages, '',
    TJvSettingsTreeImagesProperty);

end;

end.

