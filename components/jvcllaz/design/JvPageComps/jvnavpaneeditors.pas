unit JvNavPaneEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ImgList,
  PropEdits, GraphPropEdits;

type
  TJvNavPanePageImageIndexProperty = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

  TJvNavPanelHeaderImageIndexProperty = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

  TJvNavPanelButtonImageIndexProperty = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

  TJvNavIconButtonImageIndexProperty = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

implementation

uses
  JvPageList, JvNavigationPane;

function TJvNavPanePageImageIndexProperty.GetImageList: TCustomImageList;
var
  P: TJvNavigationPane;
begin
  P := TJvNavigationPane(TJvNavPanelPage(GetComponent(0)).PageList);
  if P = nil then
    Result := nil
  else
  if P.SmallImages <> nil then // small images fit better into the OI, so prefer those
    Result := P.SmallImages
  else
    Result := P.LargeImages;
end;

//=== { TJvNavPanelHeaderImageIndexProperty } ================================

function TJvNavPanelHeaderImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := TJvNavPanelHeader(GetComponent(0)).Images;
end;

//=== { TJvNavPanelButtonImageIndexProperty } ================================

function TJvNavPanelButtonImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := TJvNavPanelButton(GetComponent(0)).Images;
end;

//=== { TJvNavIconButtonImageIndexProperty } =================================

function TJvNavIconButtonImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := TJvNavIconButton(GetComponent(0)).Images;
end;

end.

