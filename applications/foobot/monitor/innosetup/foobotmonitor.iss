[Setup]
AppName=Foobot Monitor
AppVersion=0.2.4.0
DefaultDirName={pf}\foobotmonitor
DefaultGroupName=Foobot
UninstallDisplayIcon={app}\foobotmonitor.exe
Compression=lzma2
SolidCompression=yes
OutputDir=.
ArchitecturesInstallIn64BitMode=x64
LicenseFile=COPYING.GPL.txt
OutputBaseFilename=setup_foobotmonitor
SetupIconFile=.\foobotmonitor.ico
AppPublisher="Open source software"
AlwaysShowDirOnReadyPage=yes

[Files]
; Place all x64 files here
Source: "..\compiled\win64\foobotmonitor64.exe"; DestDir: "{app}"; DestName: "foobotmonitor.exe"; Check: Is64BitInstallMode
; Place all x86 files here, first one should be marked 'solidbreak'
Source: "..\compiled\win32\foobotmonitor.exe"; DestDir: "{app}"; Flags: solidbreak; Check: not Is64BitInstallMode
; Place all common files here, first one should be marked 'solidbreak'
; For SSL support
Source: "..\libeay32.dll";DestDir: "{app}"; Flags: solidbreak;
Source: "..\ssleay32.dll";DestDir: "{app}";
 
[Icons]
Name: "{group}\Foobot"; Filename: "{app}\foobotmonitor.exe"

[Run]
Filename: "{app}\foobotmonitor.exe"; WorkingDir: "{app}"; Flags: postinstall runascurrentuser; Description: "Start Foobot Monitor now"; StatusMsg: "Start Foobot Monitor now"
