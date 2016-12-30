unit umainform;

{ Foobot Interrogator

  Copyright (C)2016 Gordon Bamber minsadorada@charcodelvalle.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{
== VERSION HISTORY ==
V0.1.0.0: Intial version by minesadorada
V0.1.1.0: ??
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Menus, ExtCtrls, ComCtrls, ExtDlgs,
  ucryptini, dateutils, ulogin, udataform, foobot_utility;

type
  { Tmainform }

  Tmainform = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    CalendarDialog1: TCalendarDialog;
    cmd_Close: TBitBtn;
    cmd_FetchData: TButton;
    cmd_GetIdentity: TButton;
    grp_daterange: TGroupBox;
    GroupBox3: TGroupBox;
    lbl_fromdate: TLabel;
    lbl_to: TLabel;
    lbl_toDate: TLabel;
    MainMenu1: TMainMenu;
    mnu_helpAbout: TMenuItem;
    mnu_help: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_file: TMenuItem;
    rg_intervalAverageBy: TRadioGroup;
    rg_interval: TRadioGroup;
    rg_mode: TRadioGroup;
    sb: TStatusBar;
    spd_fromdate: TSpeedButton;
    spd_todate: TSpeedButton;
    tv_Identity: TTreeView;
    procedure ApplicationProperties1Hint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cmd_FetchDataClick(Sender: TObject);
    procedure cmd_GetIdentityClick(Sender: TObject);
    procedure cmd_testClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure rg_intervalAverageBySelectionChanged(Sender: TObject);
    procedure rg_intervalSelectionChanged(Sender: TObject);
    procedure rg_modeSelectionChanged(Sender: TObject);
    procedure spd_fromdateClick(Sender: TObject);
    procedure spd_todateClick(Sender: TObject);
    procedure tv_IdentityClick(Sender: TObject);
  private
    sFoobotUserName: string;
    sFoobotPassword: string;
    iLastIntervalSeconds: integer;
    iLastAverageBySeconds: integer;
    iStartTimeSeconds, iEndTimeSeconds: int64;
    function PopulateIdentityTreeView: boolean;
  public
    INI: TCryptIniFile;
    CurrentFoobot: integer;
    FetchType: TDataFetchType;

  end;


var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }

procedure Tmainform.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  Caption := Application.Title;
  INI := TCryptINIFile.Create(GetAppConfigFile(False));
  if INI.IsVirgin then
  begin
    INI.WriteIdent('Gordon Bamber', '(c)2016', 'GPLV2',
      'minesadorada@charcodelvalle.com', True);
    // PUT YOUR SECRET API KEY HERE IF YOU LIKE
    // INI.WriteString('Foobot', 'Secret Key',
    //  '');
  end;
  if not INI.VerifyIdent('41d10218d247980fc5e871b6b7844483') then
  begin
    ShowMessage(Application.Title +
      ' has been tampered wth.  Please re-install from a trusted source.');
    Application.Terminate;
  end;
  CurrentFoobot := 0;
  Hint := 'Welcome to ' + Application.Title;
  sb.SimpleText := Hint;
  FetchType := dfLast;
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  FreeAndNil(INI);
end;

procedure Tmainform.FormShow(Sender: TObject);
begin
  loginform.showmodal;
  sFoobotUserName := INI.ReadString('Foobot', 'Foobot User', 'myname@myserver.com');
  sFoobotPassword := INI.ReadString('Foobot', 'Foobot Password', 'password');
end;

procedure Tmainform.cmd_testClick(Sender: TObject);
begin
end;

function Tmainform.PopulateIdentityTreeView: boolean;
var
  iCount: integer;
  mainnode, node: TTreeNode;
begin
  Result := False;
  if FoobotIdentityObject.FoobotIdentityList.Count > 0 then
  begin
    // TTreeView
    TV_Identity.Items.Add(nil, 'All Foobots'); // Root
    try
      // Loop through all the detected Foobot instances
      for iCount := 0 to Pred(FoobotIdentityObject.FoobotIdentityList.Count) do
      begin
        mainnode := TV_Identity.Items[iCount];
        node := TV_Identity.Items.AddChild(mainnode,
          Format('Foobot #%d', [Succ(iCount)]));
        TV_Identity.Items.AddChild(node, 'Name: ' +
          FoobotIdentityObject.FoobotIdentityList.Items[iCount].Name);
        TV_Identity.Items.AddChild(node, 'UserID: ' +
          Format('%d', [FoobotIdentityObject.FoobotIdentityList.Items[
          iCount].userID]));
        TV_Identity.Items.AddChild(node, 'Mac: ' +
          FoobotIdentityObject.FoobotIdentityList.Items[iCount].mac);
        TV_Identity.Items.AddChild(node, 'uuID: ' +
          FoobotIdentityObject.FoobotIdentityList.Items[iCount].uuid);
        node.Expanded := False;
        Result := True;
      end;
    except
      On E: Exception do
        showmessagefmt('PopulateIdentityTreeView: Failed because %s', [E.Message]);
    end;
  end;
end;

procedure Tmainform.cmd_GetIdentityClick(Sender: TObject);
var
  sSecretKey: string;
begin
  sSecretKey := INI.ReadString('Foobot', 'Secret Key', '');
  if FetchFoobotIdentity(sFoobotUserName, sSecretKey) then
    if PopulateIdentityTreeView then
    begin
      cmd_GetIdentity.Enabled := False;
      tv_Identity.Hint := 'Click on a Foobot instance in the panel to interrogate it';
      tv_Identity.ShowHint := True;
    end;
end;

procedure Tmainform.ApplicationProperties1Hint(Sender: TObject);
begin
  if Application.Hint <> '' then
    sb.SimpleText := Application.Hint
  else
    sb.SimpleText := mainform.hint;
end;

procedure Tmainform.Button1Click(Sender: TObject);
begin
  FetchAuthenticationKey(sFoobotUserName, sFoobotPassword);
end;

procedure Tmainform.cmd_FetchDataClick(Sender: TObject);
var
  sSecretKey: string;
begin
  sSecretKey := INI.ReadString('Foobot', 'Secret Key', '');

  if FetchFoobotData(FetchType, CurrentFoobot, iLastIntervalSeconds,
    iLastAverageBySeconds, iStartTimeSeconds, iEndTimeSeconds, sSecretKey) then
  begin
    //DEBUG FoobotDataObject.SaveToFile('FoobotDataObject.json');
    dataform.ShowModal;
  end;
end;

procedure Tmainform.mnu_fileExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tmainform.mnu_helpAboutClick(Sender: TObject);
var
  s: string;
begin
  s := Application.Title + LineEnding;
  s += 'Version: ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_APPVERSION, '') +
    LineEnding + LineEnding;
  s += INI.ReadUnencryptedString('ProgramInfo', IDENT_COPYRIGHT, '');
  s += ' by ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_AUTHOR, '') + LineEnding;
  s += 'Licence: ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_LICENSE, '') +
    LineEnding;
  s += 'Made with LCL v ' + INI.ReadUnencryptedString('ProgramInfo',
    IDENT_LCLVERSION, '');
  s += ' FPC v ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_FPCVERSION, '') +
    LineEnding;
  s += 'Compiled ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_LASTCOMPILED, '') +
    LineEnding;
  s += ' for ' + INI.ReadUnencryptedString('ProgramInfo', IDENT_TARGET, '');
  MessageDlg('About ' + Application.Title, s,
    mtInformation, [mbOK], 0);
end;

procedure Tmainform.rg_intervalAverageBySelectionChanged(Sender: TObject);
begin
  case rg_intervalAverageBy.ItemIndex of
    0:
    begin
      if FetchType = dfStartEnd then
      begin
        MessageDlg(Application.Title, 'Setting minimum average = Hourly',
          mtError, [mbOK], 0);
        iLastAverageBySeconds := 3600;
      end
      else
        iLastAverageBySeconds := 0;
    end;
    1: iLastAverageBySeconds := 3600;
    2: iLastAverageBySeconds := 8 * 3600;
    3: iLastAverageBySeconds := 24 * 3600;
    4: iLastAverageBySeconds := iLastIntervalSeconds;
  end;
end;

procedure Tmainform.rg_intervalSelectionChanged(Sender: TObject);
begin
  case rg_interval.ItemIndex of
    0: iLastIntervalSeconds := 0;
    1: iLastIntervalSeconds := 3600;
    2: iLastIntervalSeconds := 2 * 3600;
    3: iLastIntervalSeconds := 4 * 3600;
    4: iLastIntervalSeconds := 8 * 3600;
  end;
end;

procedure Tmainform.rg_modeSelectionChanged(Sender: TObject);
begin
  case rg_mode.ItemIndex of
    0:
    begin
      FetchType := dfLast;
      rg_interval.Enabled := True;
      grp_daterange.Enabled := False;
    end;
    1:
    begin
      FetchType := dfStartEnd;
      rg_interval.Enabled := False;
      grp_daterange.Enabled := True;
    end;
  end;
end;

procedure Tmainform.spd_fromdateClick(Sender: TObject);
begin
  if CalendarDialog1.Execute then
  begin
    iStartTimeSeconds := DateTimeToUnix(CalendarDialog1.Date);
    lbl_fromdate.Caption := FormatDateTime('dd/mm/yyyy', CalendarDialog1.Date);
  end;
end;

procedure Tmainform.spd_todateClick(Sender: TObject);
begin
  if CalendarDialog1.Execute then
  begin
    iEndTimeSeconds := DateTimeToUnix(CalendarDialog1.Date);
    lbl_todate.Caption := FormatDateTime('dd/mm/yyyy', CalendarDialog1.Date);

  end;
end;

procedure Tmainform.tv_IdentityClick(Sender: TObject);
var
  node: TTreeNode;
begin
  if tv_Identity.Items.Count > 0 then
  begin
    node := tv_Identity.Selected;
    if not Assigned(Node) then
      Exit;
    if node.Level = 1 then
    begin
      CurrentFoobot := node.Index; // Zero-based
      cmd_FetchData.Enabled := True;
      cmd_FetchData.Font.Style := [fsBold];
      rg_mode.Enabled := True;
      rg_interval.Enabled := True;
      rg_intervalAverageBy.Enabled := True;
    end
    else
    begin
      cmd_FetchData.Enabled := False;
      cmd_FetchData.Font.Style := [];
    end;
  end;
end;


end.
