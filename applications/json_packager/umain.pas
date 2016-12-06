unit umain;
 { OnlinePackageManager Update JSON Editor

  Copyright (C)2016 usernames lainz, minesadorada, GetMem @ http://forum.lazarus.freepascal.org/index.php

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

  Version History
  ===============
  0.0.0.0 Original code by lainz
  ..to 0.1.6.0 Refactored and updated (minesadorada)
  0.1.7.0: Bugfix (lainz)
  0.1.8.0: Config file change (minesadorada)
  0.1.9.0: Error check for duplicate lpk entries (minesadorada)
  0.1.10.0: Exception handling for Load + Save (minesadorada)
  0.1.11.0: Cleaned up code formatting etc. (minesadorada)
  0.1.12.0: Rename Global DownloadURL to DownloadZipURL (minesadorada)
  0.1.13.0: Renamed TPackageData ->  TUpdatePackageData (GetMem)
            Renamed TPackageFiles -> TUpdatePackageFiles (GetMem)
            Comment out Self.AutoAdjustLayout line in Form.Create (GetMem)
            Removed StrUtils from uses (minesadorada)
            Fixed memory leaks with CFG and slErrorList (minesadorada)
            Moved inline procedure CreateUniqueINI to separate function
            Added Const C_DEBUGMESSAGES=TRUE/FALSE
  0.1.14.0: Various changes (GetMem)
            BugFix: FormCloseQuery
  0.1.15.0: BugFix: File/Save didn't add the '.json' suffix in Linux
            Addition: After Loading, run validation tests
  0.1.16.0: ??
 }
{$mode objfpc}{$H+}

interface

{DefaultTranslator}
uses
  Classes, Forms, Controls, StdCtrls, Menus, ActnList, StdActns, Grids,
  Graphics, Buttons, fileutil, LazFileUtils, fileinfo, ugenericcollection, fpjsonrtti,
  Dialogs, LCLTranslator, PopupNotifier, SysUtils, inifiles,
  lclintf, lclVersion;

CONST C_DEBUGMESSAGES=FALSE;

type

  
  { TUpdatePackageFiles }

  TUpdatePackageFiles = class(TCollectionItem)
  private
    FName: string;
    FVersion: string;
  published
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
  end;

  TPackageFilesList = specialize TGenericCollection<TUpdatePackageFiles>;

  { TUpdatePackageData }

  TUpdatePackageData = class(TPersistent)
  private
    FDownloadZipURL: string;
    FForceUpdate: boolean;
    FName: string;
  public
    constructor Create;
  published
    property Name: string read FName write FName;
    property ForceUpdate: boolean read FForceUpdate write FForceUpdate;
    property DownloadZipURL: string read FDownloadZipURL write FDownloadZipURL;
  end;

  { TUpdatePackage }

  TUpdatePackage = class(TPersistent)
  private
    FUpdatePackageData: TUpdatePackageData;
    FUpdatePackageFiles: TPackageFilesList;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(AFileName: string): boolean;
    function SaveToFile(AFileName: string): boolean;
  published
    property UpdatePackageData: TUpdatePackageData read FUpdatePackageData write FUpdatePackageData;
    property UpdatePackageFiles: TPackageFilesList read FUpdatePackageFiles write FUpdatePackageFiles;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    ActionList1: TActionList;
    cmd_Close: TBitBtn;
    cmd_save: TBitBtn;
    btnAdd: TButton;
    btnRemove: TButton;
    cbForceUpdate: TCheckBox;
    editName: TEdit;
    editDownloadZipURL: TEdit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    lblPackageFiles: TLabel;
    lblName: TLabel;
    lblDownloadZipURL: TLabel;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    LoadItem: TMenuItem;
    mnu_fileNew: TMenuItem;
    mnu_helpDisableWarnings: TMenuItem;
    mnu_lang_es: TMenuItem;
    mnu_lang_en: TMenuItem;
    mnu_lang: TMenuItem;
    mnu_helpAbout: TMenuItem;
    mnu_helpShowHints: TMenuItem;
    mnu_help: TMenuItem;
    mnu_fileSave: TMenuItem;
    mypopup: TPopupNotifier;
    SaveAsItem: TMenuItem;
    sb_editName: TSpeedButton;
    spd_CheckURL: TSpeedButton;
    stringPackageFiles: TStringGrid;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure cbForceUpdateMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadItemClick(Sender: TObject);
    procedure mnu_fileNewClick(Sender: TObject);
    procedure mnu_fileSaveClick(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure mnu_helpDisableWarningsClick(Sender: TObject);
    procedure mnu_helpShowHintsClick(Sender: TObject);
    procedure mnu_lang_enClick(Sender: TObject);
    procedure mnu_lang_esClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure sb_editNameClick(Sender: TObject);
    procedure spd_CheckURLClick(Sender: TObject);
  private
    { private declarations }
    JSONPackage: TUpdatePackage;
    bForceSaveAs, bShowPopupHints, bDisableWarnings, bDirty, bIsVirgin: boolean;
    sJSONFilePath: string;
    sUpdateDirectory, sZipDirectory: string;
    slErrorList: TStrings;
    CFG: TIniFile;
    INIFilePath:String;
    function ValidationFailed: boolean;
    procedure CtrlShowPopup(Sender: TObject);
    procedure CtrlHidePopup(Sender: TObject);
    procedure CtrlSetUpPopupHandlers;
    procedure CtrlMakeDirty(Sender: TObject);
    function FoundADuplicateLPK: boolean;
    Function CreateUniqueINI(var aCount: integer):Boolean;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}
resourcestring
  rsOneOfTheReq1 =
    'One of the required fields is missing or wrong.';
  rsOneOfTheReqn =
    'One or more of the required fields are missing or wrong.';
  rsSavedOK = 'Saved OK';
  rsSaveUnsucces = 'Save unsuccessful';
  rsOverwrite = 'Overwrite';
  rsTurnHintsOff = '(You can toggle these hints on/off in the Help menu)';
  rsHelpAndInfor = 'Help and Information';
  rsAbout = 'About';
  rsUpdate = 'Update';
  rsFileMayBeUns = 'JSON may be unsaved. Are you sure you want to quit?';
  rsMypackagenam = 'mypackagename.zip';
  rsHttpWwwUpdat = 'http://www.updatesite.com/myupdate/mypackagename.zip';
  rsLpkFileName = 'lpk FileName';
  rsVersion0000 = 'Version (n.n.n.n)';
  rsFixThenTryAg = 'Fix, then try again.';
  rsUpdateZipNam = '- Update zip name is too short or missing';
  rsDownloadZipURLI = '- Download URL is too short or missing';
  rsThereAreNoLp = '- There are no .lpk files in the list yet';
  rsUpdateZipNam2 = '- Update zip name missing extension ".zip"';
  rsDownloadZipURLI2 = '- Download URL is incomplete';
  rsDownloadZipURLS = '- Download URL should start with "http"';
  rsDownloadZipURLD = '- Download URL does not contain the zipfile name';
  rsWouldYouLike = 'Would you like to copy %s to the %s folder?';
  rsSWasSuccessf = '%s was successfully copied to the %s folder';
  rsSorryCopyOpe = 'Sorry - copy operation was unsuccessful';
  rsCompiledWith2 = 'Compiled with FPC V:%s and Lazarus V:%d.%d%s for the %s -' +
    ' %s platform%s%s';
  rsVersionEntry = 'Version Entry #%d is absent';
  rsVersionEntry2 = 'Version entry #%d needs to be of the form n.n.n.n';
  rsTheLpkEntryD = 'The .lpk entry #%d is missing the .lpk extension';
  rsTheLpkEntryD2 = 'The .lpk entry #%d is is absent';
  rsThisOptionSh =
    'This option should only be used for crucial updates or bug-fixed packages. Are you OK with that?';
  rsLanguageChan = 'Language changed to "%s".';
  rsSorryThisLan = 'Sorry, this language is unavailable at this time.';
  rsYouMayNeedTo = '(You may need to restart the app to see the change)';
  rsThereAreOneO = '- There are one or more .lpk entries with the same name.%s'
    + '- Every .lpk entry must have a unique name.';
  rsUpdateJsonSF = 'Update file "%s" failed to load correctly.';

{ TUpdatePackageData }

constructor TUpdatePackageData.Create;
begin
  FName := '';
  FForceUpdate := False;
  FDownloadZipURL := '';
end;

{ TfrmMain }
procedure TfrmMain.CtrlMakeDirty(Sender: TObject);
begin
  bDirty := True;
end;

procedure TfrmMain.CtrlHidePopup(Sender: TObject);
// Get rid of highlighting
begin
  mypopup.Hide;
  slErrorList.Clear;
  if Sender.ClassName <> 'TLabel' then
    TControl(Sender).Color := clWindow;
end;

procedure TfrmMain.CtrlShowPopup(Sender: TObject);
// Use the control's Hint property to populate the popup text
begin
  if not bShowPopupHints then
    exit;
  MyPopup.Text := '';
  MyPopup.Title := '';
  if (Sender.InheritsFrom(TControl) = False) then
    exit;

  myPopup.Text := TControl(Sender).Hint;
  if (MyPopup.Text <> '') then
  begin
    mypopup.Title := rsHelpAndInfor;
    mypopup.Text := mypopup.Text;
    if bIsVirgin then
      mypopup.Text := mypopup.Text + LineEnding + rsTurnHintsOff;
    mypopup.showatpos(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TfrmMain.CtrlSetUpPopupHandlers;
// Use different handlers for some controls
var
  iCount: integer;
begin
  with frmMain do
  begin
    for iCount := 0 to Pred(ControlCount) do
    begin
      if (Controls[iCount].InheritsFrom(TControl) = False) then
        continue;
      if (Controls[iCount] is TEdit) then
      begin
        TEdit(Controls[iCount]).OnMouseEnter := @CtrlShowPopup;
        TEdit(Controls[iCount]).OnMouseLeave := @CtrlHidePopup;
        TEdit(Controls[iCount]).OnClick := @CtrlHidePopup;
        TEdit(Controls[iCount]).OnEditingDone := @CtrlMakeDirty;
      end;
      if (Controls[iCount] is TCheckBox) then
      begin
        TCheckBox(Controls[iCount]).OnMouseEnter := @CtrlShowPopup;
        TCheckBox(Controls[iCount]).OnMouseLeave := @CtrlHidePopup;
        TCheckBox(Controls[iCount]).OnClick := @CtrlHidePopup;
        TCheckBox(Controls[iCount]).OnEditingDone := @CtrlMakeDirty;
      end;
      if (Controls[iCount] is TStringGrid) then
      begin
        TStringGrid(Controls[iCount]).OnMouseEnter := @CtrlShowPopup;
        TStringGrid(Controls[iCount]).OnMouseLeave := @CtrlHidePopup;
        TStringGrid(Controls[iCount]).OnClick := @CtrlHidePopup;
        TStringGrid(Controls[iCount]).OnEditingDone := @CtrlMakeDirty;
      end;
      if (Controls[iCount] is TLabel) then
      begin
        TLabel(Controls[iCount]).OnMouseEnter := @CtrlShowPopup;
        TLabel(Controls[iCount]).OnMouseLeave := @CtrlHidePopup;
        TLabel(Controls[iCount]).OnClick := @CtrlHidePopup;
      end;
      if (Controls[iCount] is TButton) then
      begin
        TButton(Controls[iCount]).OnMouseEnter := @CtrlShowPopup;
        TButton(Controls[iCount]).OnMouseLeave := @CtrlHidePopup;
      end;
      if (Controls[iCount] is TSpeedButton) then
      begin
        TSpeedButton(Controls[iCount]).OnMouseEnter := @CtrlShowPopup;
        TSpeedButton(Controls[iCount]).OnMouseLeave := @CtrlHidePopup;
      end;
      if (Controls[iCount] is TBitBtn) then
      begin
        TBitBtn(Controls[iCount]).OnMouseEnter := @CtrlShowPopup;
        TBitBtn(Controls[iCount]).OnMouseLeave := @CtrlHidePopup;
      end;
    end;
  end;
end;


function TfrmMain.FoundADuplicateLPK: boolean;
var
  TempStringList: TStrings;
  iCount: integer;
begin
  Result := False;
  TempStringList := TStringList.Create;
  try
    for iCount := 0 to Pred(stringPackageFiles.RowCount) do
    begin
      if TempStringlist.IndexOf(stringPackageFiles.Cells[0, iCount]) = -1 then
        TempStringList.Add(stringPackageFiles.Cells[0, iCount])
      else
        Result := True;
    end;
  finally
    TempStringList.Free;
  end;
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
begin
  stringPackageFiles.RowCount := stringPackageFiles.RowCount + 1;
end;

procedure TfrmMain.btnRemoveClick(Sender: TObject);
begin
  if stringPackageFiles.RowCount > 1 then
    stringPackageFiles.RowCount := stringPackageFiles.RowCount - 1;
end;

procedure TfrmMain.cbForceUpdateMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  s: string;
begin
  if bDisableWarnings then
    exit;
  if cbForceUpdate.Checked then
  begin
    s := rsThisOptionSh;
    if MessageDlg(s, mtConfirmation, [mbOK, mbAbort], 0, mbAbort) = mrAbort then
      cbForceUpdate.Checked := False;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
  if ((bDirty = True) AND (bDisableWarnings=FALSE)) then
  begin
    if MessageDlg(rsFileMayBeUns, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrNo then
      CanClose := False;
  end
  else
  begin
    CFG.WriteBool('Options', 'Virgin', False);
    CFG.WriteBool('Options', 'DiableWarnings', bDisableWarnings);
    CFG.UpdateFile;
  end;
end;
function TfrmMain.CreateUniqueINI(var aCount: integer):Boolean;
// Recursively loop until correct INI found, or new one created
begin
  Result:=FALSE;
  INIFilePath := GetAppConfigFile(False) + IntToStr(aCount);
  CFG := TIniFile.Create(INIFilePath);
  CFG.CacheUpdates:=TRUE;
  if CFG.ReadString('Options', 'AppPath', ProgramDirectory) <> ProgramDirectory then
  begin
    FreeAndNil(CFG); // Ditch the old one
    Inc(aCount);
    Result:=TRUE;
    CreateUniqueINI(aCount); // Make a new one
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  sLang: string;
  iIniCount:Integer;
begin
  Self.AutoAdjustLayout(lapAutoAdjustForDPI, Self.DesignTimeDPI,
    Screen.PixelsPerInch, Self.Width, ScaleX(Self.Width, Self.DesignTimeDPI));
  // Enable AutoSize again to get correct Height
  editName.AutoSize := True;
  editDownloadZipURL.AutoSize := True;
  // Furniture
  Caption := Application.Title;
  Icon := Application.Icon;
  {$IFNDEF IGNOREPICTURE}
  MyPopup.Icon := TPicture(Application.Icon);
  {$ENDIF}
  editname.Text := rsMypackagenam;
  editDownloadZipURL.Text := rsHttpWwwUpdat;
  stringPackageFiles.Columns[0].Title.Caption := rsLpkFileName;
  stringPackageFiles.Columns[1].Title.Caption := rsVersion0000;
  // Defaults
  slErrorList := TStringList.Create;
  bForceSaveAs := True;
  bShowPopupHints := True;
  // Encourage the user to maintain an updates folder
  sUpdateDirectory := ProgramDirectory + 'updates';
  if not FileExistsUTF8(sUpdateDirectory) then
    if not ForceDirectoriesUTF8(ProgramDirectory + 'updates') then
      sUpdateDirectory := ProgramDirectory;
  // Enable options persistence
  // If program location is different, create a new CFG file
  // Because each component's location might be different
  iIniCount:=0;
  If CreateUniqueINI(iIniCount) then
    CFG.WriteString('Options', 'AppPath', ProgramDirectory);
  CFG.UpdateFile;
  If C_DEBUGMESSAGES=TRUE then
    ShowMessageFmt('Inifile=%s, Count=%d',[INIFilePath,iIniCount]);
  // Pop-up hints (show on first run, then not again unless the user chooses)
  bIsVirgin := CFG.ReadBool('Options', 'Virgin', True);
  bShowPopupHints := bIsVirgin;
  mnu_helpShowHints.Checked := bShowPopupHints;
  // Override here if the user has re-enabled them
  bShowPopupHints := CFG.ReadBool('Options', 'ShowPopupHints', bShowPopupHints);
  mnu_helpShowHints.Checked := bShowPopupHints;
  bDisableWarnings := CFG.ReadBool('Options', 'DiableWarnings', False);
  mnu_helpDisableWarnings.Checked := bDisableWarnings;
  CtrlSetUpPopupHandlers; // Set the Hint property of various controls to show a Popup
  // Language translation
  sLang := CFG.ReadString('Options', 'Language', 'en'); // First default is English
  SetDefaultLang(sLang, 'locale', True);
  // Add more translations here
  if sLang = 'en' then
    mnu_lang_en.Checked := True;
  if sLang = 'es' then
    mnu_lang_es.Checked := True;

  bDirty := False; // No effect :(
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
    CFG.Free;
    slErrorList.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  bDirty := False;
end;

procedure TfrmMain.LoadItemClick(Sender: TObject);
var
  i: integer;
begin
  FileOpen1.Dialog.InitialDir :=
    CFG.ReadString('Options', 'LastLoadedJSONPath', sUpdateDirectory);
  FileOpen1.Dialog.Filter := 'JSON|*.json';
  if FileOpen1.Dialog.Execute then
  begin
    sJSONFilePath := FileOpen1.Dialog.Filename;
    CFG.WriteString('Options', 'LastLoadedJSONPath', ExtractFileDir(sJSONFilePath));
    JSONPackage := TUpdatePackage.Create;
    try
      if JSONPackage.LoadFromFile(FileOpen1.Dialog.FileName) then
      begin
        editName.Text := JSONPackage.UpdatePackageData.Name;
        editDownloadZipURL.Text := JSONPackage.UpdatePackageData.DownloadZipURL;
        cbForceUpdate.Checked := JSONPackage.UpdatePackageData.ForceUpdate;
        stringPackageFiles.RowCount := JSONPackage.UpdatePackageFiles.Count + 1;
        for i := 0 to JSONPackage.UpdatePackageFiles.Count - 1 do
        begin
          stringPackageFiles.Cells[0, i + 1] := JSONPackage.UpdatePackageFiles.Items[i].Name;
          stringPackageFiles.Cells[1, i + 1] := JSONPackage.UpdatePackageFiles.Items[i].Version;
        end;
          if ValidationFailed then
          begin
            if (slErrorList.Count > 1) then
              ShowMessage(Format(rsUpdateJsonSF, [ExtractFileName(sJSONFilePath)]) + LineEnding
              + LineEnding + rsOneOfTheReqn + LineEnding + slErrorList.Text +
                LineEnding + rsFixThenTryAg)
            else
              ShowMessage(Format(rsUpdateJsonSF,[ExtractFileName(sJSONFilePath)]) + LineEnding
              + LineEnding + rsOneOfTheReq1 + LineEnding + slErrorList.Text +
                LineEnding + rsFixThenTryAg);
            Exit;
          end;
      end
      else
        ShowMessageFmt('There was a problem loading "%s" - is it corrupted or in the wrong format?',
          [ExtractFilename(FileOpen1.Dialog.FileName)]);
    finally
      JSONPackage.Free;
    end;
  end;
end;

procedure TfrmMain.mnu_fileNewClick(Sender: TObject);
begin
  editname.Text := rsMypackagenam;
  editDownloadZipURL.Text := rsHttpWwwUpdat;
  cbForceUpdate.Checked := False;
  stringPackageFiles.RowCount := 1;
  sJSONFilePath := '';
  sZipDirectory := '';
end;

procedure TfrmMain.mnu_fileSaveClick(Sender: TObject);
begin
  bForceSaveAs := False;
  SaveAsItem.Click;
  bForceSaveAs := True;
end;

procedure TfrmMain.mnu_helpAboutClick(Sender: TObject);
var
  s: string;
  Quad: TVersionQuad;
  VInfo: TFileVersionInfo;
  EqualsPos: integer;
begin
  s := Application.Title + LineEnding;
  if GetProgramVersion(Quad) then
    s += 'Version: ' + VersionQuadToStr(Quad) + LineEnding;
  Vinfo := TFileVersionInfo.Create(Application);
  try
    Vinfo.Filter.Add('LegalCopyright'); // Set In Project/Options/Version Info
    Vinfo.Filter.Add('FileDescription'); // Set In Project/Options/Version Info
    Vinfo.ReadFileInfo;
    if VInfo.VersionStrings.Count > 0 then
    begin
      EqualsPos := Pos('=', VInfo.VersionStrings[1]); // Copyright
      if (EqualsPos > 0) then
        s += RightStr(VInfo.VersionStrings[1], Length(VInfo.VersionStrings[1]) -
          EqualsPos) + LineEnding;
    end;
    s+=Format(rsCompiledWith2,
    [{$I %FPCVERSION%},lcl_major,lcl_minor,LineEnding,{$I %FPCTARGETCPU%},{$I %FPCTARGETOS%},LineEnding,LineEnding]);
    if VInfo.VersionStrings.Count > 1 then
    begin
      EqualsPos := Pos('=', VInfo.VersionStrings[0]); // File Deswcription
      if (EqualsPos > 0) then
        s += RightStr(VInfo.VersionStrings[0], Length(VInfo.VersionStrings[0]) -
          EqualsPos) + LineEnding;

    end;
  finally
    Vinfo.Free;
  end;
  MessageDlg(rsAbout + ' ' + Application.Title, s, mtInformation, [mbOK], 0);
end;

procedure TfrmMain.mnu_helpDisableWarningsClick(Sender: TObject);
begin
  bDisableWarnings := not bDisableWarnings;
  mnu_helpDisableWarnings.Checked := bDisableWarnings;
  CFG.WriteBool('Options', 'DiableWarnings', bDisableWarnings);
end;

procedure TfrmMain.mnu_helpShowHintsClick(Sender: TObject);
begin
  bShowPopupHints := mnu_helpShowHints.Checked;
  CFG.WriteBool('Options', 'ShowPopupHints', bShowPopupHints);
end;

procedure TfrmMain.mnu_lang_enClick(Sender: TObject);
begin
  SetDefaultLang('en', 'locale', True);
  if Length(GetDefaultLang) > 0 then
  begin
    ShowMessageFmt(rsLanguageChan, [GetDefaultLang]);
    CFG.WriteString('Options', 'Language', GetDefaultLang);
    mnu_lang_en.Checked := True;
  end
  else
  begin
    mnu_lang_en.Checked := False;
    ShowMessage(rsSorryThisLan + LineEnding + rsYouMayNeedTo);
  end;
end;

procedure TfrmMain.mnu_lang_esClick(Sender: TObject);
begin
  SetDefaultLang('es', 'locale', True);
  if Length(GetDefaultLang) > 0 then
  begin
    ShowMessageFmt(rsLanguageChan, [GetDefaultLang]);
    CFG.WriteString('Options', 'Language', GetDefaultLang);
    mnu_lang_es.Checked := True;
  end
  else
  begin
    mnu_lang_es.Checked := False;
    ShowMessage(rsSorryThisLan);
  end;
end;

function TfrmMain.ValidationFailed: boolean;
  // Add checks as needed here
var
  iCount: integer;
  Quad: TVersionQuad; // fileinfo unit
begin
  Result := False;
  // Check Zipname and URL http:// length
  if (Length(editName.Text) < 5) then
  begin
    editName.Color := clYellow;
    slErrorList.Add(rsUpdateZipNam);
    Result := True;
  end;
  // URL implausable?
  if (Length(editDownloadZipURL.Text) < 10) then
  begin
    slErrorList.Add(rsDownloadZipURLI);
    editDownloadZipURL.Color := clYellow;
    Result := True;
  end;
  // No lpk file?
  if (stringPackageFiles.RowCount = 1) then
  begin
    slErrorList.Add(rsThereAreNoLp);
    stringPackageFiles.Color := clYellow;
    Result := True;
  end;
  // Remembered to type 'zip'?
  if (Length(editName.Text) > 4) then
    if (RightStr(LowerCase(editName.Text), 4) <> '.zip') then
    begin
      slErrorList.Add(rsUpdateZipNam2);
      editName.Color := clYellow;
      Result := True;
    end;
  // A full URL?
  if ((Length(editDownloadZipURL.Text) > 0) and (RightStr(editDownloadZipURL.Text, 1) = '/'))
  then
  begin
    slErrorList.Add(rsDownloadZipURLI2);
    editDownloadZipURL.Color := clYellow;
    Result := True;
  end;

  if ((Length(editDownloadZipURL.Text) > 4) and
    (LeftStr(LowerCase(editDownloadZipURL.Text), 4) <> 'http')) then
  begin
    slErrorList.Add(rsDownloadZipURLS);
    editDownloadZipURL.Color := clYellow;
    Result := True;
  end;

  if (Pos(Lowercase(editName.Text), Lowercase(editDownloadZipURL.Text)) = 0) then
  begin
    slErrorList.Add(rsDownloadZipURLD);
    editDownloadZipURL.Color := clYellow;
    Result := True;
  end;

  // Check package files entries
  if (stringPackageFiles.RowCount > 1) then
    for iCount := 1 to stringPackageFiles.RowCount - 1 do
    begin
      if (Length(stringPackageFiles.Cells[0, iCount]) = 0) then
      begin
        slErrorList.Add(Format(rsTheLpkEntryD2, [iCount]));
        stringPackageFiles.Color := clYellow;
        Result := True;
      end;
      if (Length(stringPackageFiles.Cells[1, iCount]) = 0) then
      begin
        slErrorList.Add(Format(rsVersionEntry, [iCount]));
        stringPackageFiles.Color := clYellow;
        Result := True;
      end;
      if (TryStrToVersionQuad(stringPackageFiles.Cells[1, iCount], Quad) = False) then
      begin
        slErrorList.Add(Format(rsVersionEntry2, [iCount]));
        stringPackageFiles.Color := clYellow;
        Result := True;
      end;
      if (RightStr(LowerCase(stringPackageFiles.Cells[0, iCount]), 4) <> '.lpk') then
      begin
        slErrorList.Add(Format(rsTheLpkEntryD, [iCount]));
        stringPackageFiles.Color := clYellow;
        Result := True;
      end;
    end;

  // Check for duplicate .lpk entries
  if FoundADuplicateLPK then
  begin
    stringPackageFiles.Color := clYellow;
    slErrorList.Add(Format(rsThereAreOneO, [LineEnding]));
    Result := True;
  end;
end;

procedure TfrmMain.SaveAsItemClick(Sender: TObject);
var
  i: integer;
begin
  if ValidationFailed then
  begin
    if (slErrorList.Count > 1) then
      ShowMessage(rsOneOfTheReqn + LineEnding + slErrorList.Text +
        LineEnding + rsFixThenTryAg)
    else
      ShowMessage(rsOneOfTheReq1 + LineEnding + slErrorList.Text +
        LineEnding + rsFixThenTryAg);
    Exit;
  end;
  if bForceSaveAs or (sJSONFilePath = '') then
  begin
    FileSaveAs1.Dialog.InitialDir := sUpdateDirectory;
    FileSaveAs1.Dialog.FileName := 'update_' + ExtractFilenameOnly(editName.Text) + '.json';
    if FileSaveAs1.Dialog.Execute then
      sJSONFilePath := FileSaveAs1.Dialog.FileName
    else
      Exit;
  end;

  JSONPackage := TUpdatePackage.Create;
  try
    JSONPackage.UpdatePackageData.Name := editName.Text;
    JSONPackage.UpdatePackageData.DownloadZipURL := editDownloadZipURL.Text;
    JSONPackage.UpdatePackageData.ForceUpdate := cbForceUpdate.Checked;

    for i := 1 to stringPackageFiles.RowCount - 1 do
    begin
      with JSONPackage.UpdatePackageFiles.Add do
      begin
        Name := stringPackageFiles.Cells[0, i];
        Version := stringPackageFiles.Cells[1, i];
      end;
    end;
    if FileExistsUTF8(sJSONFilePath) and (bDisableWarnings = False) then
    begin
      if MessageDlg(rsOverwrite + ' ' + sJSONFilePath + '?', mtConfirmation,
        [mbYes, mbNo], 0, mbYes) = mrYes then
        if JSONPackage.SaveToFile(sJSONFilePath) then
          ShowMessage(sJSONFilePath + ' ' + rsSavedOK);
    end
    else
    if JSONPackage.SaveToFile(sJSONFilePath) then
      ShowMessage(sJSONFilePath + rsSavedOK)
    else
      ShowMessage(rsSaveUnsucces);
    bDirty := False;
  finally
    JSONPackage.Free;
  end;
end;

procedure TfrmMain.sb_editNameClick(Sender: TObject);
var
  s: string;
begin
  FileOpen1.Dialog.InitialDir :=
    CFG.ReadString('Options', 'LastLoadedZipPath', sZipDirectory);
  FileOpen1.Dialog.Filter := rsUpdate + ' ZIP|*.zip';
  if FileOpen1.Dialog.Execute then
  begin
    // Offer to copy to /updates?
    sZipDirectory := ExtractFileDir(FileOpen1.Dialog.Filename);
    CFG.WriteString('Options', 'LastLoadedZipPath', sZipDirectory);
    s := ExtractFileName(FileOpen1.Dialog.Filename);
    editName.Text := s;
    if MessageDlg(Format(rsWouldYouLike, [s, sUpdateDirectory]),
      mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrYes then
    begin
      if CopyFile(FileOpen1.Dialog.Filename, sUpdateDirectory +
        DirectorySeparator + s) then
        ShowMessageFmt(rsSWasSuccessf, [s, sUpdateDirectory])
      else
        ShowMessage(rsSorryCopyOpe);
    end;
  end;
end;

procedure TfrmMain.spd_CheckURLClick(Sender: TObject);
// Show a popup notification because it takes time to open a browser window
var
  bTemp: boolean;
begin
  if OpenURL(editDownloadZipURL.Text) then
  begin
    bTemp := bShowPopupHints;
    bShowPopupHints := True;
    CtrlShowPopup(spd_CheckURL);
    bShowPopupHints := bTemp;
  end;
end;

{ TPackage }

constructor TUpdatePackage.Create;
begin
  FUpdatePackageData := TUpdatePackageData.Create;
  FUpdatePackageFiles := TPackageFilesList.Create;
end;

destructor TUpdatePackage.Destroy;
var
  c: TCollectionItem;
begin
  FUpdatePackageData.Free;
  for c in FUpdatePackageFiles do
    c.Free;
  FUpdatePackageFiles.Free;
  inherited Destroy;
end;

function TUpdatePackage.LoadFromFile(AFileName: string): boolean;
var
  DeStreamer: TJSONDeStreamer;
  s: TStringList;
begin
  Result := True;
  s := TStringList.Create;
  try
    s.LoadFromFile(AFileName);
    DeStreamer := TJSONDeStreamer.Create(nil);
    try
      DeStreamer.JSONToObject(s.Text, Self);
    except
      // Eat the exception
      On E: Exception do
        Result := False;
    end;
  finally
    DeStreamer.Free;
    s.Free;
  end;
end;

function TUpdatePackage.SaveToFile(AFileName: string): boolean;
var
  Streamer: TJSONStreamer;
  s: TStringList;
begin
  Result := True;
  s := TStringList.Create;
  try
    Streamer := TJSONStreamer.Create(nil);
    Streamer.Options := Streamer.Options + [jsoUseFormatString];
    s.AddText(Streamer.ObjectToJSONString(Self));
    try
      s.SaveToFile(AFileName);
    except
      // Eat the exception
      On E: Exception do
        Result := False;
    end;
  finally
    Streamer.Free;
    s.Free;
  end;
end;

end.
