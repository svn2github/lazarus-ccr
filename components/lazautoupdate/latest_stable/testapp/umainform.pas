unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls,
  Buttons, StdCtrls,LazFileUtils,FileUtil,
  ulazautoupdate,eventlog;
CONST
{$IFDEF WINDOWS}
   {$IFDEF CPU32}
      C_VERSIONSINNAME = 'testappwin32.ini';
      C_ZIPFILENAME = 'testappwin32.zip';
      C_LogFileName = 'testappwin32log.txt';
   {$ENDIF}
   {$IFDEF CPU64}
      C_VERSIONSINNAME = 'testappwin64.ini';
      C_ZIPFILENAME = 'testappwin64.zip';
      C_LogFileName = 'testappwin64log.txt';
   {$ENDIF}
{$ENDIF}
  {$IFDEF LINUX}
   {$IFDEF CPU32}
      C_VERSIONSINNAME = 'testapplinux32.ini';
      C_ZIPFILENAME = 'testapplinux32.zip';
      C_LogFileName = 'testapplinux32log.txt';
   {$ENDIF}
   {$IFDEF CPU64}
      C_VERSIONSINNAME = 'testapplinux64.ini';
      C_ZIPFILENAME = 'testapplinux64.zip';
      C_LogFileName = 'testapplinux64log.txt';
   {$ENDIF}
{$ENDIF}

type

  { Tmainform }

  Tmainform = class(TForm)
    cmd_SilentUpdate: TButton;
    cmd_AutoUpdate: TButton;
    cmd_updateToNewVersion: TButton;
    cmd_DownloadNewVersion: TButton;
    cmd_NewVersionAvailable: TButton;
    cmd_close: TBitBtn;
    lbl_Version: TLabel;
    LazAutoUpdate1: TLazAutoUpdate;
    StatusBar1: TStatusBar;
    procedure cmd_AutoUpdateClick(Sender: TObject);
    procedure cmd_DownloadNewVersionClick(Sender: TObject);
    procedure cmd_NewVersionAvailableClick(Sender: TObject);
    procedure cmd_SilentUpdateClick(Sender: TObject);
    procedure cmd_updateToNewVersionClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LazAutoUpdate1DebugEvent(Sender: TObject; lauMethodName,
      lauMessage: string);
    procedure LazAutoUpdate1Downloaded(Sender: TObject; ResultCode,
      BytesDownloaded: integer);
    procedure LazAutoUpdate1NewVersionAvailable(Sender: TObject;
      Newer: boolean; OnlineVersion: string);
  private
     Logger: TEventLog;
     procedure WriteAndLog(szText: string);
  public

  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }
procedure Tmainform.WriteAndLog(szText: string);
begin
  Logger.Info(szText);
end;

procedure Tmainform.FormCreate(Sender: TObject);
begin
  LazAutoUpdate1.DebugMode:=TRUE;
  LazAutoUpdate1.VersionsININame:=C_VERSIONSINNAME;
  LazAutoUpdate1.ZipfileName:=C_ZIPFILENAME;
  lbl_Version.Caption:='Version: ' + LazAutoUpdate1.AppVersion;
  Caption:=Application.Title;
  if FileExistsUTF8(C_LogFileName) then
    DeleteFile(C_LogFileName);
  Logger := TEventLog.Create(nil);
  Logger.LogType := ltFile;
  Logger.FileName := C_LogFileName;
  Logger.Active := True;
  Logger.Info('Start of Log');
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  Logger.Info('End of Log');
  FreeAndNil(Logger);
end;

procedure Tmainform.cmd_NewVersionAvailableClick(Sender: TObject);
begin
  LazAutoUpdate1.NewVersionAvailable;
end;

procedure Tmainform.cmd_SilentUpdateClick(Sender: TObject);
begin
   LazAutoUpdate1.SilentUpdate;
end;

procedure Tmainform.cmd_updateToNewVersionClick(Sender: TObject);
begin
   LazAutoUpdate1.UpdateToNewVersion;
end;

procedure Tmainform.FormActivate(Sender: TObject);
begin
   LazAutoUpdate1.ShowWhatsNewIfAvailable;
end;

procedure Tmainform.cmd_DownloadNewVersionClick(Sender: TObject);
begin
    LazAutoUpdate1.DownloadNewVersion;
end;

procedure Tmainform.cmd_AutoUpdateClick(Sender: TObject);
begin
  LazAutoUpdate1.AutoUpdate;
end;

procedure Tmainform.LazAutoUpdate1DebugEvent(Sender: TObject; lauMethodName,
  lauMessage: string);
begin
   StatusBar1.SimpleText:='Debug Message: (' + lauMethodName + ') ' + lauMessage;
   WriteAndLog(StatusBar1.SimpleText);
end;

procedure Tmainform.LazAutoUpdate1Downloaded(Sender: TObject; ResultCode,
  BytesDownloaded: integer);
begin
  StatusBar1.SimpleText:=Format('Downloaded. StatusCode=%d BytesDownloaded=%d',
  [ResultCode,BytesDownloaded]);
  WriteAndLog(StatusBar1.SimpleText);
end;

procedure Tmainform.LazAutoUpdate1NewVersionAvailable(Sender: TObject;
  Newer: boolean; OnlineVersion: string);
begin
  If Newer then
   StatusBar1.SimpleText:='New version available. Online Version is ' + OnlineVersion
  else
    StatusBar1.SimpleText:='Online version is not newer. Online Version is ' + OnlineVersion;

  WriteAndLog(StatusBar1.SimpleText);

end;

end.

