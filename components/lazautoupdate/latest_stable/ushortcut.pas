unit ushortcut;
{
License
=======
LazAutoUpdate (c)2015 Gordon Bamber (minesadorada@charcodelvalle.com)

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


Linux Shortcut Info
===================

1. FreeDesktop Valid Categories
===============================
AudioVideo                  Application for presenting, creating, or processing multimedia (audio/video)
Audio                       An audio application  Desktop entry must include AudioVideo as well
Audio                       A video application  Desktop entry must include AudioVideo as well
Development                 An application for development
Education                   Educational software
Game                        A game
Graphics                    Application for viewing, creating, or processing graphics
Network                     Network application such as a web browser
Office                      An office type application
Science                     Scientific software
Settings                    Settings applications  Entries may appear in a separate menu or as part of a "Control Center"
System                      System application, "System Tools" such as say a log viewer or network monitor
Utility                     Small utility application, "Accessories"

2. Example Desktop File
=======================
[Desktop Entry]
Version=1.0
Type=Application
Name=Foo Viewer
Comment=The best viewer for Foo objects available!
TryExec=fooview
Exec=fooview %F
Icon=fooview
MimeType=image/x-foo;
Actions=Gallery;Create;

[Desktop Action Gallery]
Exec=fooview --gallery
Name=Browse Gallery

[Desktop Action Create]
Exec=fooview --create-new
Name=Create a new Foo!
Icon=fooview-new
}
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, LazUTF8, FileUtil, LazFileUtils
  {$IFDEF LINUX}, process{$ENDIF}
  {$IFDEF WINDOWS}, Windows, shlobj {for special folders}, ActiveX,
  ComObj, ShellAPI{$ENDIF}  ;

function CreateDesktopShortCut(Target, TargetArguments, ShortcutName,
  IconFileName, Category: string): boolean;

implementation

{$IFDEF UNIX}
//Adapted from sysutils; Unix/Linux only
function XdgConfigHome: string;
{ Follows base-dir spec,
  see [http://freedesktop.org/Standards/basedir-spec].
  Always ends with PathDelim. }
begin
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result = '') then
    Result := IncludeTrailingPathDelimiter(ExpandFileNameUTF8('~')) +
      '.config' + DirectorySeparator
  else
    Result := IncludeTrailingPathDelimiter(Result);
end;

{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
function CreateDesktopShortCut(Target, TargetArguments, ShortcutName,
  IconFileName, Category: string): boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  InFolder: array[0..MAX_PATH] of char;
  LinkName: WideString;
begin
  Result := True;
  // Simple failure check
  if not FileExistsUTF8(Target) then
    Result := False;
  if Result = False then
    Exit;

  try
    { Creates an instance of IShellLink }
    IObject := CreateComObject(CLSID_ShellLink);
    ISLink := IObject as IShellLink;
    IPFile := IObject as IPersistFile;

    ISLink.SetPath(PChar(Target));
    ISLink.SetArguments(PChar(TargetArguments));
    ISLink.SetWorkingDirectory(PChar(ExtractFilePath(Target)));
    // ISLink.SetIconLocation(Pchar(ExtractFilePath(Target) + IconFileName));
    { Get the desktop location }
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
    SHGetPathFromIDList(PIDL, InFolder);
    LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName + '.lnk';

    { Get rid of any existing shortcut first }
    SysUtils.DeleteFile(LinkName);

    { Create the link }
    IPFile.Save(PWChar(LinkName), False);
  except
    Result := False;
  end;
end;

{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
function CreateDesktopShortCut(Target, TargetArguments, ShortcutName,
  IconFileName, Category: string): boolean;
var
  XdgDesktopContent: TStringList;
  XdgDesktopFile: string;
  Aprocess: TProcess;
begin
  // Suceed by default:
  Result := True;
  // Simple failure checks
  if not FileExistsUTF8(Target) then
    Result := False;
  if not FileExistsUTF8(ExtractFilePath(Target) + IconFileName) then
    Result := False;
  if ShortCutName = '' then
    Result := False;
  if Result = False then
    Exit;
  if Category = '' then Category := 'Utility';

  XdgDesktopFile := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'fpcup-' + shortcutname + '.desktop';
  XdgDesktopContent := TStringList.Create;
  try
    XdgDesktopContent.Add('[Desktop Entry]');
    XdgDesktopContent.Add('Encoding=UTF-8');
    XdgDesktopContent.Add('Type=Application');
    XdgDesktopContent.Add('Icon=' + ExtractFilePath(Target) + IconFileName);
    XdgDesktopContent.Add('Exec=' + Target + ' ' + TargetArguments);
    XdgDesktopContent.Add('Name=' + ShortcutName);
    XdgDesktopContent.Add('Category=' + Category + ';');
    // We're going to try and call xdg-desktop-icon
    // this may fail if shortcut exists already
    AProcess := TProcess.Create(nil);
    try
      try
        XdgDesktopContent.SaveToFile(XdgDesktopFile);
        AProcess.Parameters.Add(XdgDesktopFile);
        Aprocess.Executable := 'xdg-desktop-icon install';
        Aprocess.WaitOnExit(2000);
        Aprocess.Execute;
        //OperationSucceeded:=(ExecuteCommand('xdg-desktop-icon install ' + XdgDesktopFile,false)=0);
      except
        Result := False;
      end;
    finally
      AProcess.Free;
    end;
    if Result = False then
      // Temp file is no longer needed....
      try
        DeleteFile(XdgDesktopFile);
      finally
        // Swallow, let filesystem maintenance clear it up
      end;
  finally
    XdgDesktopContent.Free;
  end;
end;

{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
procedure DeleteDesktopShortcut(ShortcutName: string);
var
  PIDL: PItemIDList;
  InFolder: array[0..MAX_PATH] of char;
  LinkName: WideString;
begin
  { Get the desktop location }
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder);
  LinkName := IncludeTrailingPathDelimiter(InFolder) + ShortcutName + '.lnk';
  SysUtils.DeleteFile(LinkName);
end;

{$ENDIF MSWINDOWS}
end.
