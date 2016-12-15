unit umain;

{$DEFINE PO_BUILTINRES}// Use built-in resources for .po files
// If compiling with Laz Version < 1.7 then use lazres to make a translate.lrs from the .po files
// If compiling Laz V >=1.7 then add the .po files in Project/Options/resources
// Without this DEFINE, include the /locale folder in the distribution

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
            Moved inline procedure CreateUniqueINI to separate function (minesadorada)
            Added Const C_DEBUGMESSAGES=TRUE/FALSE (minesadorada)
  0.1.14.0: Various changes (GetMem)
            BugFix: FormCloseQuery(minesadorada)
  0.1.15.0: BugFix: File/Save didn't add the '.json' suffix in Linux (minesadorada)
            Addition: After Loading, run validation tests(minesadorada)
  0.1.16.0: Renamed ForceUpdate to ForceNotify (GetMem/minesadorada)
  0.1.17.0: po files stored in executable's resources (minesadorada)
            Use Project/Options/Resources in Laz 1.7+ to add the .po files
            or.. Use (Gl)LazRes to make a file 'translate.lrs' in older Laz (minesadorada)
            This can be disabled by commenting out $DEFINE PO_BUILTINRES
            This system means you do not have to deploy the /locale folder - just the executable.
            Portability: On startup, it will make a unique cfg file in the GetAppConfig folder based
            on the executable's location on disk, so you can have copies of jsoneditor
            in each component's dev folder that uses its own config file, language and updates folder.
  0.1.18.0: Bugfix: Linux path error when creating locale folder (minesadorada)
  0.1.19.0: Added IntrnalVersion integer field to json (getmem/minesadorada)
            Added SpinEdit to control the above  (minesadorada)
            In Laz 1.7 DPIAwareness configured
  0.2.0.0:  Refactored GUI(minesadorada)
  0.2.1.0:  Added scrollbox to contain package info (GetMem)
  0.2.2.0:  Hints and Validation updated (minesadorada)
  0.2.3.0:  ResourceStrings Updated (minesadorada)
  0.2.4.0:  Bugfix: regression error: DisableInOPM (minesadorada)
  0.2.5.0:  BugFix: regression error: CreateUniqueINIFile (minesadorada)
  0.2.6.0:  Added feature - Help menu/AutoLoad Last File
  0.2.7.0:  ??
 }
{$mode objfpc}{$H+}

interface

{DefaultTranslator not used}
uses
  Classes, Forms, Controls, StdCtrls, Menus, ActnList, StdActns,
  Graphics, Buttons, fileutil, LazFileUtils, fileinfo, ugenericcollection,
  fpjsonrtti, Dialogs, LCLTranslator, PopupNotifier, SysUtils, inifiles,
  lclintf, lclVersion, LResources, Spin, {$IFDEF PO_BUILTINRES}LazUTF8Classes{$ENDIF};

const
  C_DEBUGMESSAGES = False; // TRUE ONLY IN DEV MODE!

type


  { TUpdatePackageFiles }

  TUpdatePackageFiles = class(TCollectionItem)
  private
    FName: string;
    FVersion: string;
    FForceNotify: boolean;
    FInternalVersion: integer;
  published
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property ForceNotify: boolean read FForceNotify write FForceNotify;
    property InternalVersion: integer read FInternalVersion write FInternalVersion;
  end;

  TPackageFilesList = specialize TGenericCollection<TUpdatePackageFiles>;

  { TUpdatePackageData }

  TUpdatePackageData = class(TPersistent)
  private
    FDownloadZipURL: string;
    FDisableInOPM: boolean;
    FName: string;
  public
    constructor Create;
  published
    property Name: string read FName write FName;
    property DownloadZipURL: string read FDownloadZipURL write FDownloadZipURL;
    property DisableInOPM: boolean read FDisableInOPM write FDisableInOPM;
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
    property UpdatePackageData: TUpdatePackageData
      read FUpdatePackageData write FUpdatePackageData;
    property UpdatePackageFiles: TPackageFilesList
      read FUpdatePackageFiles write FUpdatePackageFiles;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    ActionList1: TActionList;
    chk_DisableInOPM: TCheckBox;
    cmd_Close: TBitBtn;
    cmd_save: TBitBtn;
    cmd_AddPackageFile: TButton;
    cmd_RemoveLastPackageFile: TButton;
    edt_UpdateZipName: TEdit;
    edt_DownloadZipURL: TEdit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    lbl_PackageFiles: TLabel;
    lbl_UpdateZipName: TLabel;
    lbl_DownloadZipURL: TLabel;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    LoadItem: TMenuItem;
    mnu_helpAutoloadLastFile: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_fileNew: TMenuItem;
    mnu_helpDisableWarnings: TMenuItem;
    mnu_lang_es: TMenuItem;
    mnu_lang_en: TMenuItem;
    mnu_lang: TMenuItem;
    mnu_helpAbout: TMenuItem;
    mnu_helpShowHints: TMenuItem;
    mnu_help: TMenuItem;
    mnu_fileSave: TMenuItem;
    popup_hint: TPopupNotifier;
    SaveAsItem: TMenuItem;
    sb_editName: TSpeedButton;
    sb_PackageFiles: TScrollBox;
    spd_CheckURL: TSpeedButton;
    procedure chk_DisableInOPMMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure cmd_AddPackageFileClick(Sender: TObject);
    procedure cmd_RemoveLastPackageFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadItemClick(Sender: TObject);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_fileNewClick(Sender: TObject);
    procedure mnu_fileSaveClick(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure mnu_helpAutoloadLastFileClick(Sender: TObject);
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
    bForceSaveAs, bShowPopupHints, bDisableWarnings,bAutoLoadLast,
      bDirty, bIsVirgin: boolean;
    sJSONFilePath: string;
    sUpdateDirectory, sZipDirectory: string;
    slErrorList: TStrings;
    CFG: TIniFile;
    INIFilePath: string;
    // Start of Package Information controls
    ArrayGrpBox: array of TGroupBox;
    ArrayLblPackageFileName: array of TLabel;
    ArrayEdtPackageFileName: array of TEdit;
    ArrayLblPackageVersion: array of TLabel;
    ArraySpinEditV1: array of TSpinEdit;
    ArraySpinEditV2: array of TSpinEdit;
    ArraySpinEditV3: array of TSpinEdit;
    ArraySpinEditV4: array of TSpinEdit;
    ArrayChkBoxForceNotify: array of TCheckBox;
    ArraySpinEditInternalVersion: array of TSpinEdit;
    ArrayLblPackageInternalVersion: array of TLabel;
    // End of Package Information controls
    iNumLpkFilesVisible: integer;
    procedure LoadJSONFromFile(sFileName:String);
    procedure AddPackageFileToList;
    procedure RemovePackageFileFromList;
    procedure ResetPackageFileControlsToOne;
    procedure AddNewControlArray;
    procedure DestroyControlArrays;
    procedure RemoveLastControlArray;
    procedure ProcessNotify(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CtrlShowPopup(Sender: TObject);
    procedure CtrlHidePopup(Sender: TObject);
    procedure CtrlSetUpPopupHandlers;
    procedure CtrlMakeDirty(Sender: TObject);
    procedure CreateUniqueINI(var aCount: integer);
    function ValidationFailed: boolean;
    function FoundADuplicateLPK: boolean;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;
  sPoPath_en, sPoPath_es: string;
{$IFDEF PO_BUILTINRES}
  aLRes: TLResource;
  aSS: TStringListUTF8;
  S: TResourceStream;
  F: TFileStream;
{$ENDIF}

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
  rsMypackagelpk = 'mypackagename.lpk';
  rsHttpWwwUpdat = 'http://www.updatesite.com/myupdate/mypackagename.zip';
  rsFixThenTryAg = 'Fix, then try again.';
  rsUpdateZipNam = '- Update zip name is too short or missing';
  rsDownloadZipURLI = '- Download URL is too short or missing';
  rsUpdateZipNam2 = '- Update zip name missing extension ".zip"';
  rsDownloadZipURLI2 = '- Download URL is incomplete';
  rsDownloadZipURLS = '- Download URL should start with "http"';
  rsDownloadZipURLD = '- Download URL does not contain the zipfile name';
  rsWouldYouLike = 'Would you like to copy %s to the %s folder?';
  rsSWasSuccessf = '%s was successfully copied to the %s folder';
  rsSorryCopyOpe = 'Sorry - copy operation was unsuccessful';
  rsCompiledWith2 = 'Compiled with FPC V:%s and Lazarus V:%d.%d%s for the %s -' +
    ' %s platform%s%s';
  rsTheLpkEntryD = 'The .lpk entry #%d is missing the .lpk extension';
  rsTheLpkEntryD2 = 'The .lpk entry #%d is is absent';
  rsThisOptionSh =
    'This option should only be used for crucial updates or bug-fixed packages.';
  rsLanguageChan = 'Language changed to "%s".';
  rsSorryThisLan = 'Sorry, this language is unavailable at this time.';
  rsYouMayNeedTo = '(You may need to restart the app to see the change)';
  rsThereAreOneO = '- There are one or more .lpk entries with the same name.%s' +
    '- Every .lpk entry must have a unique name.';
  rsUpdateJsonSF = 'Update file "%s" failed to load correctly.';
  rsNotifyUpdate = 'Notify Update';
  rsUseInCombina = 'Use in combination with';
  rsPackageDInfo = 'Package #%d Information';
  rsThePackageFi = 'The package filename (No path e.g. package.lpk)';
  rsVersion = 'Version: ';
  rsFormatIsNNNN = 'Package version:%sFormat is: n.n.n.n';
  rsCheckThisIfY = 'Check this if you don''t want to increment the package '
    + 'version';
  rsInternalVers = 'Internal Version: ';
  rsFilename = 'Filename: ';
  rsThisWillDisa = 'This will disable your package in Online Package Manager!%'
    + 'sAre you SURE you want to do this?';
  rsThereWasAPro = 'There was a problem loading "%s" - is it corrupted or in '
    + 'the wrong format?';
  rsVersionForPa = 'Version for package %d is zero';
  rsInternalVers2 = 'Internal version number should not be Zero%s';
  rsOpeningYourB = 'Opening your browser...';

{ TUpdatePackageData }

constructor TUpdatePackageData.Create;
begin
  FName := '';
  FDisableInOPM := False;
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
  popup_hint.Hide;
  slErrorList.Clear;
  if Sender.ClassName <> 'TLabel' then
    TControl(Sender).Color := clWindow;
end;

procedure TfrmMain.CtrlShowPopup(Sender: TObject);
// Use the control's Hint property to populate the popup text
begin
  if not bShowPopupHints then
    exit;
  popup_hint.Text := '';
  popup_hint.Title := '';
  if (Sender.InheritsFrom(TControl) = False) then
    exit;

  popup_hint.Text := TControl(Sender).Hint;
  if (popup_hint.Text <> '') then
  begin
    popup_hint.Title := rsHelpAndInfor;
    popup_hint.Text := popup_hint.Text;
    if bIsVirgin then
      popup_hint.Text := popup_hint.Text + LineEnding + rsTurnHintsOff;
    popup_hint.showatpos(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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
{     // (Kept for reference)
      // Iterate through the children of TScrollBox
      if (Controls[iCount] is TGroupBox) then
        // Iterate through the children of GroupBox
        for jCount := 0 to Pred(TGroupBox(Controls[iCount]).ControlCount) do
        begin
          if TGroupBox(Controls[iCount]).Controls[jCount] is TSpinEdit then
          begin
            TSpinEdit(TGroupBox(Controls[iCount]).Controls[jCount]).OnMouseEnter :=
              @CtrlShowPopup;
          end;
        end;
}
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

procedure TfrmMain.DestroyControlArrays;
var
  i: integer;
begin
  // Callked on Form_Destroy
  for i := 0 to High(ArrayGrpBox) do
  begin
    FreeAndNil(ArraySpinEditInternalVersion[i]);
    FreeAndNil(ArrayLblPackageInternalVersion[i]);
    FreeAndNil(ArrayChkBoxForceNotify[i]);
    FreeAndNil(ArraySpinEditV4[i]);
    FreeAndNil(ArraySpinEditV3[i]);
    FreeAndNil(ArraySpinEditV2[i]);
    FreeAndNil(ArraySpinEditV1[i]);
    FreeAndNil(ArrayLblPackageVersion[i]);
    FreeAndNil(ArrayEdtPackageFileName[i]);
    FreeAndNil(ArrayLblPackageFileName[i]);
    FreeAndNil(ArrayGrpBox[i]);
  end;
end;

procedure TfrmMain.AddNewControlArray;
{For reference:
ArrayGrpBox:Array of TGroupBox;
// Line 1
ArrayLblPackageFileName:Array of TLabel;
ArrayEdtPackageFileName:Array of TEdit;
ArrayLblPackageVersion:Array of TLabel;
ArraySpinEditV1:Array of TSpinEdit;
ArraySpinEditV2:Array of TSpinEdit;
ArraySpinEditV3:Array of TSpinEdit;
ArraySpinEditV4:Array of TSpinEdit;
// Line 2
ArrayChkBoxForceNotify:Array of TCheckBox;
ArrayLblPackageInternalVersion:Array of TLabel;
ArraySpinEditInternalVersion:Array of TSpinEdit;
SetBounds(Left,Top,Width,Height);
}
begin
  SetLength(ArrayGrpBox, Succ(iNumLpkFilesVisible));
  ArrayGrpBox[iNumLpkFilesVisible] := TGroupBox.Create(Self);

  SetLength(ArrayLblPackageFileName, Succ(iNumLpkFilesVisible));
  SetLength(ArrayEdtPackageFileName, Succ(iNumLpkFilesVisible));
  SetLength(ArrayLblPackageVersion, Succ(iNumLpkFilesVisible));
  SetLength(ArraySpinEditV1, Succ(iNumLpkFilesVisible));
  SetLength(ArraySpinEditV2, Succ(iNumLpkFilesVisible));
  SetLength(ArraySpinEditV3, Succ(iNumLpkFilesVisible));
  SetLength(ArraySpinEditV4, Succ(iNumLpkFilesVisible));
  SetLength(ArrayChkBoxForceNotify, Succ(iNumLpkFilesVisible));
  SetLength(ArrayLblPackageInternalVersion, Succ(iNumLpkFilesVisible));
  SetLength(ArraySpinEditInternalVersion, Succ(iNumLpkFilesVisible));

  with ArrayGrpBox[iNumLpkFilesVisible] do
  begin
    Caption := Format(rsPackageDInfo, [Succ(iNumLpkFilesVisible)]);
    if (iNumLpkFilesVisible > 0) then
      SetBounds(8, ArrayGrpBox[Pred(iNumLpkFilesVisible)].Top +
        ArrayGrpBox[Pred(iNumLpkFilesVisible)].Height + 10, frmMain.Width - 16, 100)
    else
      SetBounds(8, cmd_AddPackageFile.Top + cmd_AddPackageFile.Height + 10,
        frmMain.Width - 16, 100);
    Visible := False;
    Tag := Pred(iNumLpkFilesVisible);
    // Label - Package name
    ArrayLblPackageFileName[iNumLpkFilesVisible] := TLabel.Create(nil);
    with ArrayLblPackageFileName[iNumLpkFilesVisible] do
    begin
      Caption := rsFilename;
      SetBounds(8, 10, 50, 23);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      Hint := Format('%s%s%s', [rsFilename, LineEnding, rsThePackageFi]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // EditBox - Package name
    ArrayEdtPackageFileName[iNumLpkFilesVisible] := TEdit.Create(nil);
    with ArrayEdtPackageFileName[iNumLpkFilesVisible] do
    begin
      Text := rsMypackagelpk;
      SetBounds(64, 8, 256, 23);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      OnEditingDone := @CtrlMakeDirty;
      Hint := Format('%s%s%s', [rsFilename, LineEnding, rsThePackageFi]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // Label - Package Version
    ArrayLblPackageVersion[iNumLpkFilesVisible] := TLabel.Create(nil);
    with ArrayLblPackageVersion[iNumLpkFilesVisible] do
    begin
      Caption := rsVersion;
      SetBounds(330, 10, 50, 23);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      Hint := Format(rsFormatIsNNNN, [LineEnding]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // SpinEdit V1
    ArraySpinEditV1[iNumLpkFilesVisible] := TSpinEdit.Create(nil);
    with ArraySpinEditV1[iNumLpkFilesVisible] do
    begin
      Value := 0;
      SetBounds(380, 8, 40, 20);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      OnChange := @CtrlMakeDirty;
      Hint := Format(rsFormatIsNNNN, [LineEnding]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // SpinEdit V2
    ArraySpinEditV2[iNumLpkFilesVisible] := TSpinEdit.Create(nil);
    with ArraySpinEditV2[iNumLpkFilesVisible] do
    begin
      Value := 0;
      SetBounds(430, 8, 40, 20);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      OnChange := @CtrlMakeDirty;
      Hint := Format(rsFormatIsNNNN, [LineEnding]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // SpinEdit V3
    ArraySpinEditV3[iNumLpkFilesVisible] := TSpinEdit.Create(nil);
    with ArraySpinEditV3[iNumLpkFilesVisible] do
    begin
      Value := 0;
      SetBounds(480, 8, 40, 20);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      OnChange := @CtrlMakeDirty;
      Hint := Format(rsFormatIsNNNN, [LineEnding]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // SpinEdit V4
    ArraySpinEditV4[iNumLpkFilesVisible] := TSpinEdit.Create(nil);
    with ArraySpinEditV4[iNumLpkFilesVisible] do
    begin
      Value := 0;
      SetBounds(530, 8, 40, 20);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      OnChange := @CtrlMakeDirty;
      Hint := Format(rsFormatIsNNNN, [LineEnding]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // ChkBox Notify
    ArrayChkBoxForceNotify[iNumLpkFilesVisible] := TCheckBox.Create(nil);
    with ArrayChkBoxForceNotify[iNumLpkFilesVisible] do
    begin
      Checked := False;
      Caption := rsNotifyUpdate;
      SetBounds(8, 50, 40, 20);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      OnMouseUp := @ProcessNotify;
      OnEditingDone := @CtrlMakeDirty;
      Hint := Format('%s:%s%s', [rsNotifyUpdate, LineEnding, rsCheckThisIfY]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // Label Internal version
    ArrayLblPackageInternalVersion[iNumLpkFilesVisible] := TLabel.Create(nil);
    with ArrayLblPackageInternalVersion[iNumLpkFilesVisible] do
    begin
      Caption := rsInternalVers;
      SetBounds(160, 50, 40, 23);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      Hint := Format('%s%s%s %s',
        [rsInternalVers, LineEnding, rsUseInCombina, rsNotifyUpdate]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // SpinEdit Internal Version
    ArraySpinEditInternalVersion[iNumLpkFilesVisible] := TSpinEdit.Create(nil);
    with ArraySpinEditInternalVersion[iNumLpkFilesVisible] do
    begin
      Value := 1;
      SetBounds(260, 48, 40, 20);
      Visible := True;
      Tag := Pred(iNumLpkFilesVisible);
      OnMouseEnter := @CtrlShowPopup;
      OnMouseLeave := @CtrlHidePopup;
      OnClick := @CtrlHidePopup;
      OnChange := @CtrlMakeDirty;
      Hint := Format('%s%s%s %s',
        [rsInternalVers, LineEnding, rsUseInCombina, rsNotifyUpdate]);
      Parent := ArrayGrpBox[iNumLpkFilesVisible];
    end;
    // This sets the subcontrols up correctly
    Parent := sb_PackageFiles;
  end;
end;

procedure TfrmMain.RemoveLastControlArray;
// Uses iLpkFilesCount
var
  iLast: integer;
begin
  iLast := High(ArrayGrpBox);
  // Makes the group control invisible
  ArrayGrpBox[iLast].Parent := nil;

  // Tidy up memory
  FreeAndNil(ArrayChkBoxForceNotify[iLast]);
  FreeAndNil(ArrayLblPackageInternalVersion[iLast]);
  FreeAndNil(ArraySpinEditInternalVersion[iLast]);
  FreeAndNil(ArraySpinEditV4[iLast]);
  FreeAndNil(ArraySpinEditV3[iLast]);
  FreeAndNil(ArraySpinEditV2[iLast]);
  FreeAndNil(ArraySpinEditV1[iLast]);
  FreeAndNil(ArrayLblPackageVersion[iLast]);
  FreeAndNil(ArrayEdtPackageFileName[iLast]);
  FreeAndNil(ArrayLblPackageFileName[iLast]);
  FreeAndNil(ArrayGrpBox[iLast]);

  // Tidy up control array lengths
  SetLength(ArrayChkBoxForceNotify, iLast);
  SetLength(ArrayLblPackageInternalVersion, iLast);
  SetLength(ArraySpinEditInternalVersion, iLast);
  SetLength(ArraySpinEditV4, iLast);
  SetLength(ArraySpinEditV3, iLast);
  SetLength(ArraySpinEditV2, iLast);
  SetLength(ArraySpinEditV1, iLast);
  SetLength(ArrayLblPackageVersion, iLast);
  SetLength(ArrayEdtPackageFileName, iLast);
  SetLength(ArrayLblPackageFileName, iLast);
  SetLength(ArrayGrpBox, iLast);
end;

procedure TfrmMain.AddPackageFileToList;
begin
  AddNewControlArray; // Contruct another one

  // Makes it visible and aligns it
  ArrayGrpBox[High(ArrayGrpBox)].Visible := True;
  ArrayGrpBox[High(ArrayGrpBox)].Align := alTop;
  Inc(iNumLpkFilesVisible);// Note: = Succ(High(Array))
  CtrlSetUpPopupHandlers;
  Refresh;
end;

procedure TfrmMain.ResetPackageFileControlsToOne;
// Used in File/New
var
  iCount: integer;
begin
  if (iNumLpkFilesVisible = 1) then
    exit;
  for iCount := iNumLpkFilesVisible downto 1 do
  begin
    RemovePackageFileFromList;
  end;
end;

procedure TfrmMain.RemovePackageFileFromList;
begin
  // Always have one set of controls present
  if (iNumLpkFilesVisible > 1) then
  begin
    RemoveLastControlArray;
    Dec(iNumLpkFilesVisible);
    CtrlSetUpPopupHandlers;
  end;
end;

function TfrmMain.FoundADuplicateLPK: boolean;
  // Add lpk entries one-by-one to a temp stringlist looking for a duplicate
var
  TempStringList: TStrings;
  iCount: integer;
begin
  Result := False;
  TempStringList := TStringList.Create;
  try
    for iCount := 0 to High(ArrayEdtPackageFileName) do
    begin
      if TempStringlist.IndexOf(ArrayEdtPackageFileName[iCount].Text) = -1 then
        TempStringList.Add(ArrayEdtPackageFileName[iCount].Text)
      else
        Result := True;
    end;
  finally
    TempStringList.Free;
  end;
end;

procedure TfrmMain.ProcessNotify(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Special hint
  if not bShowPopupHints then
    exit;
  popup_hint.Text := rsThisOptionSh;
  popup_hint.Title := rsHelpAndInfor;
  popup_hint.Text := popup_hint.Text;
  popup_hint.showatpos(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmMain.cmd_AddPackageFileClick(Sender: TObject);
begin
  AddPackageFileToList;
end;

procedure TfrmMain.chk_DisableInOPMMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  // Warn user about DisableInOPM
  if chk_DisableInOPM.Checked = True then
    if MessageDlg(Format(rsThisWillDisa, [LineEnding]),
      mtWarning, [mbYes, mbNo], 0, mbNo) = mrNo then
      chk_DisableInOPM.Checked := False;
end;

procedure TfrmMain.cmd_RemoveLastPackageFileClick(Sender: TObject);
begin
  RemovePackageFileFromList;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
  if ((bDirty = True) and (bDisableWarnings = False)) then
  begin
    if MessageDlg(rsFileMayBeUns, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrNo then
      CanClose := False;
  end;
  if CanClose = True then
  begin
    CFG.WriteBool('Options', 'Virgin', False); // Suppresses PopUp hints on next run
    CFG.WriteBool('Options', 'DiableWarnings', bDisableWarnings);
    CFG.UpdateFile;
  end;
end;

procedure TfrmMain.CreateUniqueINI(var aCount: integer);
// Recursively loop until correct INI found, or new one created
// Based on Executable's path location
begin
  INIFilePath := GetAppConfigFile(False) + IntToStr(aCount);
  If C_DEBUGMESSAGES then ShowMessage(INIFilePath);
  if FileExistsUTF8(INIFilePath) then
  begin
    CFG := TIniFile.Create(INIFilePath);
    CFG.CacheUpdates := True;
    if CFG.ReadString('Options', 'AppPath', 'unknown') = ProgramDirectory then
    begin
      Exit;
    end
    else
    begin
      FreeAndNil(CFG); // Ditch the old one
      Inc(aCount);
      CreateUniqueINI(aCount); // Make a new one
    end;
  end
  else
  begin
    CFG := TIniFile.Create(INIFilePath);
    CFG.CacheUpdates := True;
    CFG.WriteString('Options', 'AppPath', ProgramDirectory);
    Exit;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  sLang: string;
  iIniCount: integer;
begin
  // Enable AutoSize again to get correct Height
  edt_UpdateZipName.AutoSize := True;
  edt_DownloadZipURL.AutoSize := True;
  // Furniture
  Caption := Application.Title;
  Icon := Application.Icon;
  {$IFNDEF IGNOREPICTURE}
  popup_hint.Icon := TPicture(Application.Icon);
  {$ENDIF}
  edt_UpdateZipName.Text := rsMypackagenam;
  edt_DownloadZipURL.Text := rsHttpWwwUpdat;
  // Defaults
  slErrorList := TStringList.Create;
  bForceSaveAs := True;
  bShowPopupHints := True;
  iNumLpkFilesVisible := 0;
  // Encourage the user to maintain an updates folder
  sUpdateDirectory := ProgramDirectory + 'updates';
  if not FileExistsUTF8(sUpdateDirectory) then
    if not ForceDirectoriesUTF8(ProgramDirectory + 'updates') then
      sUpdateDirectory := ProgramDirectory;
  // Enable options persistence
  // If program location is different, create a new CFG file
  // Because each component's location might be different
  iIniCount := 0;
  // First time run anywhere on the system
  INIFilePath := GetAppConfigFile(False) + '0';
  if not FileExistsUTF8(INIFilePath) then
  begin
    CFG := TIniFile.Create(INIFilePath);
    CFG.WriteString('Options', 'AppPath', ProgramDirectory);
  end
  else // Make a new INI if this is a new location
    CreateUniqueINI(iIniCount);

  CFG.UpdateFile;

  if C_DEBUGMESSAGES = True then // Dev only
    ShowMessageFmt('Inifile=%s, Count=%d', [INIFilePath, iIniCount]);

  // Pop-up hints (show on first run, then not again unless the user chooses)
  bIsVirgin := CFG.ReadBool('Options', 'Virgin', True);
  bShowPopupHints := bIsVirgin;
  mnu_helpShowHints.Checked := bShowPopupHints;

  // On startup, default to FALSE
  bAutoLoadLast:=CFG.ReadBool('Options', 'AutoLoadLastFile',FALSE);
  mnu_helpAutoloadLastFile.Checked:=bAutoLoadLast;
  sJSONFilePath:=CFG.ReadString('Options','LastSavedJSON','unknown');
  if C_DEBUGMESSAGES = True then // Dev only
    ShowMessage(sJSONFilePath);
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
  // No memory leak!
  DestroyControlArrays;
  CFG.Free;
  slErrorList.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  bDirty := False;
  AddPackageFileToList;
  If sJSONFilePath <> 'unknown' then
    If (FileExistsUTF8(sJSONFilePath)) AND  (bAutoLoadLast=TRUE) then
     LoadJSONFromFile(sJSONFilePath);

end;
procedure TfrmMain.LoadJSONFromFile(sFileName:String);
var
  i: integer;
  Quad: TVersionQuad;
begin
  ResetPackageFileControlsToOne; // So iNumLpkFilesVisible=1
      JSONPackage := TUpdatePackage.Create;
    try
      if JSONPackage.LoadFromFile(sFileName) then
      begin
        edt_UpdateZipName.Text := JSONPackage.UpdatePackageData.Name;
        edt_DownloadZipURL.Text := JSONPackage.UpdatePackageData.DownloadZipURL;
        chk_DisableInOPM.Checked := JSONPackage.UpdatePackageData.DisableInOPM;
        for i := 0 to JSONPackage.UpdatePackageFiles.Count - 1 do
        begin
          if (i > 0) then
            AddPackageFileToList;
          ArrayEdtPackageFileName[i].Text :=
            JSONPackage.UpdatePackageFiles.Items[i].Name;
          if fileinfo.TryStrToVersionQuad(
            JSONPackage.UpdatePackageFiles.Items[i].Version, Quad) then
          begin
            ArraySpinEditV1[i].Value := Quad[1];
            ArraySpinEditV2[i].Value := Quad[2];
            ArraySpinEditV3[i].Value := Quad[3];
            ArraySpinEditV4[i].Value := Quad[4];
          end;
          ArrayChkBoxForceNotify[i].Checked :=
            JSONPackage.UpdatePackageFiles.Items[i].ForceNotify;
          ArraySpinEditInternalVersion[i].Value :=
            JSONPackage.UpdatePackageFiles.Items[i].InternalVersion;
        end;
        if ValidationFailed then
        begin
          if (slErrorList.Count > 1) then
            ShowMessage(Format(rsUpdateJsonSF, [ExtractFileName(sJSONFilePath)]) +
              LineEnding + LineEnding + rsOneOfTheReqn + LineEnding +
              slErrorList.Text + LineEnding + rsFixThenTryAg)
          else
            ShowMessage(Format(rsUpdateJsonSF, [ExtractFileName(sJSONFilePath)]) +
              LineEnding + LineEnding + rsOneOfTheReq1 + LineEnding +
              slErrorList.Text + LineEnding + rsFixThenTryAg);
          Exit;
        end;
      end
      else
        ShowMessageFmt(rsThereWasAPro,
          [ExtractFilename(sFileName)]);
    finally
      JSONPackage.Free;
    end;
end;

procedure TfrmMain.LoadItemClick(Sender: TObject);
begin
  FileOpen1.Dialog.InitialDir :=
    CFG.ReadString('Options', 'LastLoadedJSONPath', sUpdateDirectory);
  FileOpen1.Dialog.Filter := 'JSON|*.json';
  if FileOpen1.Dialog.Execute then
  begin
    sJSONFilePath := FileOpen1.Dialog.Filename;
    CFG.WriteString('Options', 'LastLoadedJSONPath', ExtractFileDir(sJSONFilePath));
    ResetPackageFileControlsToOne; // So iNumLpkFilesVisible=1
    LoadJSONFromFile(sJSONFilePath);
  end;
end;

procedure TfrmMain.mnu_fileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnu_fileNewClick(Sender: TObject);
begin
  edt_UpdateZipName.Text := rsMypackagenam;
  edt_DownloadZipURL.Text := rsHttpWwwUpdat;
  sJSONFilePath := '';
  sZipDirectory := '';
  chk_DisableInOPM.Checked := False;
  ResetPackageFileControlsToOne;
  ArrayEdtPackageFileName[0].Text := rsMypackagelpk;
  ArraySpinEditV1[0].Value := 0;
  ArraySpinEditV2[0].Value := 0;
  ArraySpinEditV3[0].Value := 0;
  ArraySpinEditV4[0].Value := 0;
  ArrayChkBoxForceNotify[0].Checked := False;
  ArraySpinEditInternalVersion[0].Value := 0;
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
    // Comment line below out for JEDI source prettification
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

procedure TfrmMain.mnu_helpAutoloadLastFileClick(Sender: TObject);
begin
  bAutoLoadLast:= NOT bAutoLoadLast;
  mnu_helpAutoloadLastFile.Checked:=bAutoLoadLast;
  CFG.WriteBool('Options', 'AutoLoadLastFile',bAutoLoadLast);
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
    //  SetDefaultLang(''); // Back to default?
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
    SetDefaultLang(''); // Back to DefaultTranslator
    ShowMessage(rsSorryThisLan);
  end;
end;

function TfrmMain.ValidationFailed: boolean;
  // Add checks as needed here
var
  iCount: integer;
begin
  Result := False;
  // Check Zipname and URL http:// length
  if (Length(edt_UpdateZipName.Text) < 5) then
  begin
    edt_UpdateZipName.Color := clYellow;
    slErrorList.Add(rsUpdateZipNam);
    Result := True;
  end;
  // URL implausable?
  if (Length(edt_DownloadZipURL.Text) < 10) then
  begin
    slErrorList.Add(rsDownloadZipURLI);
    edt_DownloadZipURL.Color := clYellow;
    Result := True;
  end;
  // Remembered to type 'zip'?
  if (Length(edt_UpdateZipName.Text) > 4) then
    if (RightStr(LowerCase(edt_UpdateZipName.Text), 4) <> '.zip') then
    begin
      slErrorList.Add(rsUpdateZipNam2);
      edt_UpdateZipName.Color := clYellow;
      Result := True;
    end;
  // A full URL?
  if ((Length(edt_DownloadZipURL.Text) > 0) and
    (RightStr(edt_DownloadZipURL.Text, 1) = '/')) then
  begin
    slErrorList.Add(rsDownloadZipURLI2);
    edt_DownloadZipURL.Color := clYellow;
    Result := True;
  end;
  // URL starts with 'http' ?
  if ((Length(edt_DownloadZipURL.Text) > 4) and
    (LeftStr(LowerCase(edt_DownloadZipURL.Text), 4) <> 'http')) then
  begin
    slErrorList.Add(rsDownloadZipURLS);
    edt_DownloadZipURL.Color := clYellow;
    Result := True;
  end;
  // URL contains zipfile name?
  if (Pos(Lowercase(edt_UpdateZipName.Text),
    Lowercase(edt_DownloadZipURL.Text)) = 0) then
  begin
    slErrorList.Add(rsDownloadZipURLD);
    edt_DownloadZipURL.Color := clYellow;
    Result := True;
  end;

  // Check package files entries
  for iCount := 0 to High(ArrayGrpBox) do
  begin
    // Is package name empty?
    if Length(ArrayEdtPackageFileName[iCount].Text) = 0 then
    begin
      slErrorList.Add(Format(rsTheLpkEntryD2, [Succ(iCount)]));
      ArrayEdtPackageFileName[iCount].Color := clYellow;
      Result := True;
    end;
    // Does it end with 'lpk'
    if (RightStr(LowerCase(ArrayEdtPackageFileName[iCount].Text), 4) <> '.lpk') then
    begin
      slErrorList.Add(Format(rsTheLpkEntryD, [Succ(iCount)]));
      ArrayEdtPackageFileName[iCount].Color := clYellow;
      Result := True;
    end;
    // Is the version number zero
    if (ArraySpinEditV1[iCount].Value = 0) and
      (ArraySpinEditV2[iCount].Value = 0) and
      (ArraySpinEditV3[iCount].Value = 0) and
      (ArraySpinEditV4[iCount].Value = 0) then
    begin
      slErrorList.Add(Format(rsVersionForPa, [Succ(iCount)]));
      ArraySpinEditV1[iCount].Color := clYellow;
      ArraySpinEditV2[iCount].Color := clYellow;
      ArraySpinEditV3[iCount].Color := clYellow;
      ArraySpinEditV4[iCount].Color := clYellow;
      Result := True;
    end;
    // Check Forcenotify version isn't zero
    if ArrayChkBoxForceNotify[iCount].Checked = True then
      if ArraySpinEditInternalVersion[iCount].Value = 0 then
      begin
        ArraySpinEditInternalVersion[iCount].Color := clYellow;
        slErrorList.Add(Format(rsInternalVers2, [LineEnding]));
        Result := True;
      end;
    // Check for duplicate .lpk entries
    if FoundADuplicateLPK then
    begin
      ArrayEdtPackageFileName[iCount].Color := clYellow;
      slErrorList.Add(Format(rsThereAreOneO, [LineEnding]));
      Result := True;
    end;
  end;
end;

procedure TfrmMain.SaveAsItemClick(Sender: TObject);
var
  i: integer;
  Quad: TVersionQuad;
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
    FileSaveAs1.Dialog.FileName :=
      'update_' + ExtractFilenameOnly(edt_UpdateZipName.Text) + '.json';

    if FileSaveAs1.Dialog.Execute then
       begin
          sJSONFilePath := FileSaveAs1.Dialog.FileName;
       end
    else
      Exit;
  end;

  JSONPackage := TUpdatePackage.Create;
  try
    JSONPackage.UpdatePackageData.Name := edt_UpdateZipName.Text;
    JSONPackage.UpdatePackageData.DownloadZipURL := edt_DownloadZipURL.Text;
    JSONPackage.UpdatePackageData.DisableInOPM := chk_DisableInOPM.Checked;
    for i := 0 to High(ArrayGrpBox) do
    begin
      with JSONPackage.UpdatePackageFiles.Add do
      begin
        Name := ArrayEdtPackageFileName[i].Text;
        Quad[1] := ArraySpinEditV1[i].Value;
        Quad[2] := ArraySpinEditV2[i].Value;
        Quad[3] := ArraySpinEditV3[i].Value;
        Quad[4] := ArraySpinEditV4[i].Value;
        Version := VersionQuadToStr(Quad);
        ForceNotify := ArrayChkBoxForceNotify[i].Checked;
        InternalVersion := ArraySpinEditInternalVersion[i].Value;
      end;
    end;
    if FileExistsUTF8(sJSONFilePath) and (bDisableWarnings = False) then
    begin
      if MessageDlg(rsOverwrite + ' ' + sJSONFilePath + '?', mtConfirmation,
        [mbYes, mbNo], 0, mbYes) = mrYes then
        if JSONPackage.SaveToFile(sJSONFilePath) then
          begin
           ShowMessage(sJSONFilePath + ' ' + rsSavedOK);
           CFG.WriteString('Options','LastSavedJSON',sJSONFilePath);
          end;
    end
    else
    if JSONPackage.SaveToFile(sJSONFilePath) then
    begin
      ShowMessage(sJSONFilePath + rsSavedOK);
      CFG.WriteString('Options','LastSavedJSON',sJSONFilePath);
    end
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
    edt_UpdateZipName.Text := s;
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
  sOldHint: string;
begin
  if OpenURL(edt_DownloadZipURL.Text) then
  begin
    bTemp := bShowPopupHints;
    sOldHint := spd_CheckURL.Hint;
    spd_CheckURL.Hint := rsOpeningYourB;
    bShowPopupHints := True;
    CtrlShowPopup(spd_CheckURL);
    spd_CheckURL.Hint := sOldHint;
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

// Use embedded .po resources if not distributed with executable
// Update for more languages
initialization
{$IFDEF PO_BUILTINRES}
  sPoPath_en := ProgramDirectory + 'locale' + PathDelim + ExtractFilenameOnly(
    Application.EXEName) + '.en.po';
  sPoPath_es := ProgramDirectory + 'locale' + PathDelim + ExtractFilenameOnly(
    Application.EXEName) + '.es.po';
  if (lcl_major > 0) and (lcl_minor > 6) then // Can't use a LazVersion $DEFINE :(
  begin
    // This uses a resource file added via Project/Options (Laz 1.7+)
    if not FileExistsUTF8(sPoPath_en) then
    begin
      // create a resource stream which points to the po file
      S := TResourceStream.Create(HInstance, 'JSONEDITOR.EN', MakeIntResource(10));
      try
        ForceDirectory(ProgramDirectory + 'locale');
        F := TFileStream.Create(sPoPath_en, fmCreate);
        try
          F.CopyFrom(S, S.Size); // copy data from the resource stream to file stream
        finally
          F.Free; // destroy the file stream
        end;
      finally
        S.Free; // destroy the resource stream
      end;
    end;
    if not FileExistsUTF8(sPoPath_es) then
    begin
      S := TResourceStream.Create(HInstance, 'JSONEDITOR.ES', MakeIntResource(10));
      try
        ForceDirectory(ProgramDirectory + 'locale');
        F := TFileStream.Create(sPoPath_es, fmCreate);
        try
          F.CopyFrom(S, S.Size);
        finally
          F.Free
        end;
      finally
        S.Free;
      end;
    end;
  end
  else
  begin // Older version of laz
    // This uses an lrs file generated from lazres
    // Can't disable this with a LazVersion $DEFINE :(
{$I translate.lrs}
    if not FileExistsUTF8(sPoPath_es) then
    begin
      aLRes := LazarusResources.Find('jsoneditor.es');
      if assigned(aLRes) then
      begin
        ForceDirectory(ProgramDirectory + 'locale');
        aSS := TStringListUTF8.Create;
        try
          Ass.Add(aLRes.Value);
          aSS.SaveToFile(sPoPath_es);
        finally
          aSS.Free;
        end;
      end;
    end;
    if not FileExistsUTF8(sPoPath_en) then
    begin
      aLRes := LazarusResources.Find('jsoneditor.en');
      if assigned(aLRes) then
      begin
        ForceDirectory(ProgramDirectory + 'locale');
        aSS := TStringListUTF8.Create;
        try
          Ass.Add(aLRes.Value);
          aSS.SaveToFile(sPoPath_en);
        finally
          aSS.Free;
        end;
      end;
    end;
  end;
{$ENDIF}
end.
