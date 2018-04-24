unit JvCustomReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvcustomreg.res}

uses
  Classes, ImgList, Controls, LResources, PropEdits, GraphPropEdits, ComponentEditors,
  JvDsgnConsts,
  JvOutlookBar, JvOutlookBarEditors,
  JvTabBar, JvTabBarXPPainter,
  JvThumbImage, JvThumbnails, JvThumbViews,
  JvTimeLine, JvTMTimeLine, JvTimeLineEditor;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvTabBar, TJvModernTabBarPainter, TJvTabBarXPPainter,
    TJvOutlookBar,
    TJvThumbView, TJvThumbnail, TJvThumbImage,
    TJvTimeLine,
    TJvTMTimeLine
  ]);

  // Timeline
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TJvTimeLine, 'FirstVisibledate', TDatePropertyEditor);

  // OutlookBar
  {RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages), TJvCustomOutlookBar,
    '', TJvOutlookBarPagesProperty);}
  RegisterPropertyEditor(TypeInfo(Integer), TJvCustomOutlookBar,
    'ActivePageIndex', TJvOutlookBarActivePageProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarButton,
    'ImageIndex', TJvOutlookBarButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarPage,
    'ImageIndex', TJvOutlookBarPageImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvOutlookBarPage,
    'Caption', TJvOutlookBarCaptionProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvOutlookBarButton,
    'Caption', TJvOutlookBarCaptionProperty);
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarEditor);

  // Thumbnails
  RegisterPropertyToSkip(TJvThumbnail, 'ClientWidth', 'Redundant', '');
  RegisterPropertyToSkip(TJvThumbnail, 'ClientHeight', 'Redundant', '');

end;

end.

