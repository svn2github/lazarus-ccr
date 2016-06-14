{*********************************************************}
{*                  VPABOUT.PAS 1.03                     *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpAbout;

interface

uses
  {$IFDEF LCL}
  LMessages, LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows,Messages,
  {$ENDIF}
  Forms, Graphics, Controls, Dialogs, StdCtrls, ExtCtrls,
  {$IFDEF VERSION6}
  {$IFNDEF LCL}
  DesignIntf, DesignEditors,
  {$ELSE}
  PropEdits,
  LazarusPackageIntf,
  {$ENDIF}
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  Classes, SysUtils;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Bevel2: TBevel;
    Panel1: TPanel;
    Image1: TImage;
    ProgramName: TLabel;
    VisitUsLabel: TLabel;
    GeneralNewsgroupsLabel: TLabel;
    lblTurboLink: TLabel;
    lblHelp: TLabel;
    CopyrightLabel: TLabel;
    RightsReservedLabel: TLabel;
    OKButton: TButton;
    Bevel3: TBevel;
    lblGeneralDiscussion: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure lblLinkMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblLinkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IsServer : boolean;
  end;

  TVpAboutProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes;
      override;
    procedure Edit;
      override;
  end;

implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
{$IFNDEF LCL}
  ShellAPI,
{$ENDIF}
  VpConst, VpSR;

const
  TURBO_LINK_URL = 'http://sourceforge.net/projects/tpvplanit/';
  HELP_URL = 'http://sourceforge.net/forum/forum.php?forum_id=241880';
  NEWS_SPECIFIC_URL = 'news://news.turbopower.com/turbopower.public.support.visualplanit';
  GENERAL_DISCUSSION_URL = 'http://sourceforge.net/forum/forum.php?forum_id=241879';


{*** TVpAboutProperty ***}

function TVpAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TVpAboutProperty.Edit;
begin
  with TfrmAbout.Create(Application) do begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;


{ FrmAbout }

procedure TfrmAbout.FormActivate(Sender: TObject);
const
{$IFDEF LCL}
  COPYRIGHT = '©';
{$ELSE}
  COPYRIGHT = #169
{$ENDIF}
var
  Year, Junk: Word;
begin
  ProgramName.Caption := VpProductName + ' ' + VpVersionStr;
  DecodeDate(Now, Year, junk, junk);
  CopyrightLabel.Caption := Format('%s Copyright 2000 - %d, TurboPower Software Company.',
    [COPYRIGHT, Year]);

  lblTurboLink.Cursor := crHandPoint;
  lblHelp.Cursor := crHandPoint;
  lblGeneralDiscussion.Cursor := crHandPoint;
end;

procedure TfrmAbout.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  lblTurboLink.Font.Style := [];
end;

procedure TfrmAbout.lblLinkClick(Sender: TObject);
var
  url: String;
begin
  //  if Sender = lblNewsSpecific then url := NEWS_SPECIFIC_URL else
  if Sender = lblHelp then url := HELP_URL else
  if Sender = lblGeneralDiscussion then url := GENERAL_DISCUSSION_URL else
  if Sender = lblTurboLink then url := TURBO_LINK_URL else
  exit;
{$IFDEF LCL}
  if not OpenUrl(url)
{$ELSE}
  if ShellExecute(0, 'open', PChar(url), '', '', SW_SHOWNORMAL) <= 32
{$ENDIF}
  then
    ShowMessage(RSBrowserError);
end;

procedure TfrmAbout.lblLinkMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
end;


end.
  
