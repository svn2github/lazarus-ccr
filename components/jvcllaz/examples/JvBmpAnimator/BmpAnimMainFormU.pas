{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit BmpAnimMainFormU;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, ComCtrls, StdCtrls, ImgList, JvBmpAnimator;

type
  TBmpAnimMainForm = class(TForm)
    EdFrame: TEdit;
    UDFrame: TUpDown;
    BtnOnOff: TButton;
    BmpAnimator: TJvBmpAnimator;
    EdSpeed: TEdit;
    UDSpeed: TUpDown;
    LblFrame: TLabel;
    LblSpeed: TLabel;
    Images: TImageList;
    CbTransparent: TCheckBox;
    procedure BtnOnOffClick(Sender: TObject);
    procedure EdFrameChange(Sender: TObject);
    procedure EdSpeedChange(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
  end;

var
  BmpAnimMainForm: TBmpAnimMainForm;

implementation

{$R *.lfm}

procedure TBmpAnimMainForm.BtnOnOffClick(Sender: TObject);
begin
  with BmpAnimator do
  begin
    Active := not Active;
    if not Active then
      Position := 0;
  end;
end;

procedure TBmpAnimMainForm.CbTransparentClick(Sender: TObject);
begin
  BmpAnimator.Transparent := CbTransparent.Checked;
end;

procedure TBmpAnimMainForm.EdFrameChange(Sender: TObject);
begin
  if not BmpAnimator.Active then
    BmpAnimator.Position := UDFrame.Position;
end;

procedure TBmpAnimMainForm.EdSpeedChange(Sender: TObject);
begin
  try
    BmpAnimator.Speed := StrToInt(EdSpeed.Text);
  except
    BmpAnimator.Speed := 15;
  end;
end;

end.
