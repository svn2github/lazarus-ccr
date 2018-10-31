program cmd;

(*
= Version 0.0.1.
{
 = cmd.exe replacement
 == Windows only! ==
 = Purpose:
 == To frustrate tech support scammers
 = Documentation:
 == see readme.txt file distributed with this application
 = License:
 == Copyright (C)2018 Gordon Bamber minesadorada AT charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

*)
{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp, { you can add units after this }
  strutils,
  registry;

type

  { TMyCmd }

  TMyCmd = class(TCustomApplication)
  private
    fCurrDir: string;
    fCurrDrive: string;
    fCurrFiledate: TDateTime;
    fCommand: string;
    fUserInput: string;
    fNumFiles: integer;
    fTotalSize: int64;
    fregistry: TRegistry;
    // Get/Set TheCurrDir property
    function GetTheCurrDir: string;
    procedure SetTheCurrDir(AValue: string);

    procedure WaitABit; //Blocking pause
    procedure CDDotDot; // Deal with cd.. command
    procedure ChangeDir(Avalue: string); // Deal with cd and mkdir commands
    procedure WriteDirectoryListing; // Listing is semi-random each time
    function FetchNewFakeDirDate: string;
    function FetchNewFakeFilesize: string;
    procedure WriteFakeNetstat; // Entries are the same each time
    procedure SetAutoRun(bCreateOrDelete: boolean); // If set, then real cmd.exe will automatically run this cmd.exe
    procedure DisplayReadme; // either cmd -h or type 'help' at prompt
  protected
    procedure DoRun; override; // Add new commands in this procedure
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    // Property tracks the fake current directory displayed at the prompt
    property TheCurrDir: string read GetTheCurrDir write SetTheCurrDir;
  end;

const
  // Hardcoded
  C_FULLPROMPT = 'Microsoft Windows [Version 10.0.17134.345]' +
    LineEnding + '(c) 2018 Microsoft Corporation. All rights reserved.' +
    LineEnding + LineEnding;

  C_BADCOMMAND =
    '''%s''  is not recognized as an internal or external command,%soperable program or batch file.'
    + LineEnding + LineEnding;
  C_DIRDATEFORMAT = 'ddddd  hh:nn';
  C_REG_AUTORUN = '\Software\Microsoft\Command Processor'; //HKEY_CURRENT_USER

  //DEPRECATED:  C_FullPrompt = 'Microsoft Windows [Version %d.%d.%d.%d]' + LineEnding +
  //    '(c) 2018 Microsoft Corporation. All rights reserved.' + LineEnding + LineEnding;

  { TMyCmd }

  procedure TMyCmd.DisplayReadme;
  // Displays readme.txt file in same folder as this app
  var
    F: TextFile;
    s: string;
    ct: integer;
  begin
    // Is readme.txt missing?
    if not FileExists('readme.txt') then
    begin
      WriteLn('Help file ''readme.txt'' is missing');
      exit;
    end;
    // OK. Now read and display;
    try
      System.Assign(F, 'readme.txt');
      Reset(F);
      ct := 0;
      while not EOF(F) do
      begin
        Inc(ct);
        if ct mod 15 = 0 then // Show 15 lines per screen
        begin
          WriteLn;
          WriteLn('Press any key to continue');
          Readln;
        end
        else
        begin
          // Read a line, then display a line
          ReadLn(F, s);
          WriteLn(s);
        end;
      end;
    finally
      Close(F);
    end;
  end;

  procedure TMyCmd.SetAutoRun(bCreateOrDelete: boolean);
  begin
    fRegistry.RootKey := HKEY_CURRENT_USER;
    if bCreateOrDelete = True then
    begin
      fregistry.OpenKey(C_REG_AUTORUN, True);
      fRegistry.WriteString('Autorun', EXEname);
      fregistry.CloseKey;
    end
    else
    begin
      fregistry.OpenKey(C_REG_AUTORUN, True);
      fregistry.DeleteValue('Autorun');
      fregistry.CloseKey;
    end;
  end;

  procedure TMyCmd.WaitABit;
  begin
    Sleep(200);
  end;

  function TMyCmd.FetchNewFakeFilesize: string;
    // 18 chars right aligned
  var
    fl: double;
  begin
    fl := Random * 1000000;
    Result := Format('%.0n', [fl]);
    Result := PadLeft(Result, 18);
    Inc(fNumFiles);
    Inc(fTotalSize, ROUND(fl));
  end;

  function TMyCmd.FetchNewFakeDirDate: string;

  begin
    fCurrFileDate := fCurrFileDate - Random * 20;
    DateTimeToString(Result, C_DIRDATEFORMAT, fCurrFileDate, []);
  end;

  procedure TMyCmd.WriteDirectoryListing;
  var
    fOdds: single;
  begin
    fCurrFiledate := Now();
    fOdds := 0.8;
    fNumFiles := 0;
    fTotalSize := 0;

    WriteLn;
    WriteLn(' Volume in drive ' + Upcase(fCurrDrive) + ' is WINDOWS');
    WriteLn(' Volume Serial Number is 84A5-5539');
    WriteLn;
    WriteLn(' Directory of ' + fCurrDir);
    WriteLn;
    if Length(fCurrDir) > 3 then
    begin
      WriteLn(FetchNewFakeDirDate + '    <DIR>          .');
      WriteLn(FetchNewFakeDirDate + '    <DIR>          ..');
    end;
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + '    <DIR>          Private');
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + '    <DIR>          Banking');
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + '    <DIR>          Logins');
    if LeftStr(Upcase(fCurrDir), 8) = 'C:\USERS' then
    begin
      if (Random > fOdds) then
        WriteLn(FetchNewFakeDirDate + '    <DIR>          Contacts');
      if (Random > fOdds) then
        WriteLn(FetchNewFakeDirDate + '    <DIR>          Documents');
      if (Random > fOdds) then
        WriteLn(FetchNewFakeDirDate + '    <DIR>          Downloads');
      if (Random > fOdds) then
        WriteLn(FetchNewFakeDirDate + '    <DIR>          Pictures');
    end;
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + FetchNewFakeFilesize + ' readme.txt');
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + FetchNewFakeFilesize + ' bank details.doc');
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + FetchNewFakeFilesize + ' accounts.xls');
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + FetchNewFakeFilesize + ' passwords.doc');
    if (Random > fOdds) then
      WriteLn(FetchNewFakeDirDate + FetchNewFakeFilesize + ' ');
    WriteLn(Format('             %d file(s)         %d bytes', [fNumFiles, fTotalSize]));
    WriteLn;
  end;

  procedure TMyCmd.WriteFakeNetstat;
  begin
    WriteLn;
    WriteLn('Active Connections');
    WriteLn;
    WriteLn('  Proto  Local Address          Foreign Address        State');
    WriteLn('  TCP    192.168.0.9:49682      ec2-18-211-19-105:https  CLOSE_WAIT');
    WriteLn('  TCP    192.168.0.9:49876      40.67.248.104:https    ESTABLISHED');
    WriteLn('  TCP    192.168.0.9:53636      8.36.80.215:https      TIME_WAIT');
    WaitABit;
    WaitABit;
    WaitABit;
    WaitABit;
    WriteLn('  TCP    192.168.0.9:53957      54.239.21.139:https    ESTABLISHED');
    WriteLn('  TCP    192.168.0.9:53958      s3-us-west-2-w:https   ESTABLISHED');
    WriteLn('  TCP    192.168.0.9:53959      54.239.21.125:https    ESTABLISHED');
    WaitABit;
    WaitABit;
    WriteLn('  TCP    192.168.0.9:53960      54.239.31.63:https     ESTABLISHED');
    WriteLn('  TCP    192.168.0.9:53961      s3-us-west-2-w:https   ESTABLISHED');
    WriteLn('  TCP    192.168.0.9:53962      54.239.31.63:https     ESTABLISHED');
    WaitABit;
    WriteLn('  TCP    192.168.0.9:53963      a104-83-194-139:https  ESTABLISHED');
    WaitABit;
    WaitABit;
    WaitABit;
    WriteLn('  TCP    192.168.0.9:53964      a104-83-194-139:https  ESTABLISHED');
    WaitABit;
    WaitABit;
    WriteLn('  TCP    192.168.0.9:53965      a104-83-194-139:https  ESTABLISHED');
    WriteLn('  TCP    192.168.0.9:53966      server-52-85-46-242:http  ESTABLISHED');
    WriteLn('  TCP    192.168.0.9:53967      a84-53-129-220:http    TIME_WAIT');
    WaitABit;
    WriteLn('  TCP    192.168.0.9:53968      93.184.220.29:http     ESTABLISHED');
    WaitABit;
    WaitABit;
    WriteLn('  TCP    192.168.0.9:53969      104.18.25.243:http     ESTABLISHED');
    WriteLn;
  end;


  procedure TMyCmd.ChangeDir(Avalue: string);
  var
    s: string;
  begin
    s := GetTheCurrDir;
    if Length(AValue) > 0 then
    begin
      SetTheCurrDir(s + '\' + AValue);
      fCurrDrive := LeftStr(fCurrDir, 1);
    end;
  end;

  procedure TMyCmd.CDDotDot;
  // Deal with cd.. command by changing fake Current Directory
  var
    s: string;
  begin
    s := GetTheCurrDir;
    if RPos('\', s) > 0 then
    begin
      SetTheCurrDir(LeftStr(fCurrDir, RPos('\', s)));
      fCurrDrive := LeftStr(fCurrDir, 1);

    end;
  end;

  procedure TMyCmd.SetTheCurrDir(AValue: string);
  begin
    if fCurrDir <> AValue then
      fCurrDir := AValue;
    fCurrDrive := LeftStr(fCurrDir, 1);
  end;

  function TMyCmd.GetTheCurrDir: string;
  begin
    Result := ExcludeTrailingBackslash(fCurrDir);
  end;

  procedure TMyCmd.DoRun;
  var
    ErrorMsg, s: string;
    ct: integer;
    Parsed: boolean;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;


    { add your program here }
    // Deprecated:
    // Write(Format(C_FULLPROMPT,[Win32Platform,Win32MajorVersion,Win32MinorVersion,Win32BuildNumber]) + TheCurrDir + '>');
    Randomize; // For random datetimes, odds etc used in dir listings

    // Show header info and command prompt
    Write(C_FULLPROMPT + TheCurrDir + '>'); //hardcoded for windows 10

    // Grab input
    ReadLn(fUserInput);
    Log(etInfo, 'Scammer typed ''%s''', [fUserInput]);
    // Does nothing unless DoLog virtual procedure is overridden
    fCommand := UpCase(fUserInput);
    Parsed := False;

    // Enter command loop
    while fCommand <> 'EXIT' do
    begin
      // Parse various commands
      // TODO: other commands

      //Special commands
      //SetAutoRun
      if (fCommand = 'SETAUTORUN') and (Parsed = False) then
      begin
        Parsed := True;
        SetAutoRun(True);
        WriteLn('AutoRun set to ' + EXEName = ' successfully');
      end;

      //DelAutoRun
      if (fCommand = 'DELAUTORUN') and (Parsed = False) then
      begin
        Parsed := True;
        SetAutoRun(False);
        WriteLn('AutoRun key deleted successfully');
      end;

      // Show help
      if (fCommand = 'HELP') and (Parsed = False) then
      begin
        Parsed := True;
        WriteHelp;
        WriteLn;
      end;
      // format:  Do a fake format of the drive
      if (Pos('FORMAT', fCommand) > 0) and (Parsed = False) then
      begin
        Parsed := True;
        WriteLn('This command will erase the contents of the specified disk.');
        WriteLn('WARNING: This action cannot be undone. Are you sure? Y/N');
        ReadLn(s);
        if UpCase(s) = 'Y' then
        begin
          Write('Please wait. Formatting..');
          for ct := 1 to 30 do
          begin
            WaitABit;
            Write('.');
          end;
          Writeln('Format complete');
        end
        else
          Writeln('Command canceled.');
        WriteLn;
      end;

      // syskey: Pretend to encrypt the system database
      if (Pos('SYSKEY', fCommand) > 0) and (Parsed = False) then
      begin
        Parsed := True;
        WriteLn('WARNING: The syskey utility will encrypt your system database');
        WriteLn('The operation cannot be undone. Type ''yes'' to continue');
        ReadLn(s);
        if UpCase(s) = 'YES' then
        begin
          WriteLn('Type in the new password:');
          ReadLn(s);
          Write('Please wait. Encrypting..');
          for ct := 1 to 30 do
          begin
            WaitABit;
            Write('.');
          end;
          Writeln('Syskey encryption complete.  Restart the computer to complete the operation');
        end
        else
          Writeln('Syskey command canceled.');
        WriteLn;
      end;

      // netstat
      if (Pos('NETSTAT', fCommand) > 0) and (Parsed = False) then
      begin
        Parsed := True;
        WriteFakeNetstat;
        WriteLn('Scan foreign addresses for hackers? Y/N');
        ReadLn(s);
        if UpCase(s) = 'Y' then
        begin
          Write('Please wait. Scanning connections..');
          for ct := 1 to 10 do
          begin
            WaitABit;
            Write('.');
          end;
          Writeln('Complete.');
          WriteLn('Scan reports that all current connections are safe');
        end
        else
          Writeln('WARNING: Scan was intentionally canceled - please run netstat command again.');
        WriteLn;
      end;

      // del, delete deltree and erase
      if ((Pos('DEL', fCommand) > 0) or (Pos('ERASE', fCommand) > 0)) and
        (Parsed = False) then
      begin
        Parsed := True;
        WriteLn('This command will delete files.  Are you sure? Y/N');
        ReadLn(s);
        if UpCase(s) = 'Y' then
        begin
          Write('Please wait. Deleting files..');
          for ct := 1 to 10 do
          begin
            WaitABit;
            Write('.');
          end;
          Writeln('Complete');
        end
        else
          Writeln('Command canceled.');
      end;

      // Go To drive Root
      if (fCommand = 'CD\') and (Parsed = False) then
      begin
        Parsed := True;
        SetTheCurrDir(fCurrDrive + ':\');
      end;

      // Change Drive
      if ((Pos(':', fCommand) > 0) and (Parsed = False)) then
      begin
        Parsed := True;
        SetTheCurrDir(LeftStr(fCommand, 2));
      end;

      // tree and dir
      // Construct fake listing (random contents)
      // Force a 'scan for viruses'
      // Proclaim everything is tickety-boo
      if ((Pos('TREE', fCommand) > 0) or (Pos('DIR', fCommand) > 0)) and
        (Parsed = False) then
      begin
        Parsed := True;
        WriteDirectoryListing;
        WriteLn('Scan this folder for infections? Y/N');
        ReadLn;
        WriteLn('Please wait. Scanning for viruses and trojans');
        for ct := 1 to 20 do
        begin
          WaitABit;
          Write('.');
        end;
        WriteLn('System scanned');
        WriteLn('Viruses detected: 0');
        WriteLn('Trojans detected: 0');
        WriteLn('Contents of ' + fCurrDir + ' are clean and not infected.' +
          LineEnding + LineEnding);
      end;


      if (fCommand = 'CD..') and (Parsed = False) then
      begin
        CDDotDot; // Change fake current directory to its fake parent
        Parsed := True;
      end;

      // Change to another fake folder
      if (Pos('CD', fCommand) > 0) and (Parsed = False) then
      begin
        Parsed := True;
        ChangeDir(MidStr(fUserInput, 4, Length(fUserInput)));
      end;

      // Pretend to make a folder
      if (Pos('MKDIR', fCommand) > 0) and (Parsed = False) then
      begin
        Parsed := True;
      end;

      // Unrecognised command fallback
      if (Parsed=FALSE) AND (length(fUserInput) > 0) then
        WriteLn(Format(C_BADCOMMAND, [fUserInput, LineEnding]));

      // Show prompt
      Write(TheCurrDir + '>');

      // Fetch the next command
      ReadLn(fUserInput);
      fCommand := UpCase(fUserInput);
      Parsed := False;
      // Back to start of loop
      // user types 'exit' to exit loop
    end;

    // stop program loop
    Terminate;
  end;

  constructor TMyCmd.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
    Title := 'C:\WINDOWS\system32\cmd.exe';
    SetTheCurrDir(GetUserDir); // Set up fake Current Directory to a real one
    fCurrDrive := LeftStr(TheCurrDir, 1);
    fregistry := TRegistry.Create;
  end;

  destructor TMyCmd.Destroy;
  begin
    fregistry.Free;
    inherited Destroy;
  end;

  procedure TMyCmd.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
    DisplayReadme;
    writeln('Press any key to continue');
    readln;
  end;

var
  Application: TMyCmd;

{$R *.res}

begin
  Application := TMyCmd.Create(nil);
  Application.Title:='Command';
  Application.Run;
  Application.Free;
end.
