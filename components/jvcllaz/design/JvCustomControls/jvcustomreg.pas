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
  JvTimeLine, JvTMTimeline, JvTimeLineEditor;

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

  // Outlookbar
    (*
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages), TJvCustomOutlookBar,
    '', TJvOutlookBarPagesProperty);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons), TJvOutlookBarPage,
    '', TJvOutlookBarPagesProperty);
    *)
  RegisterPropertyEditor(TypeInfo(Integer), TJvCustomOutlookBar,
    'ActivePageIndex', TJvOutlookBarActivePageProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarButton,
    'ImageIndex', TJvOutlookBarButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarPage,
    'ImageIndex', TJvOutlookBarPageImageIndexProperty);

  // Thumbnails
  RegisterPropertyToSkip(TJvThumbnail, 'ClientWidth', 'Redundant', '');
  RegisterPropertyToSkip(TJvThumbnail, 'ClientHeight', 'Redundant', '');

end;

end.

