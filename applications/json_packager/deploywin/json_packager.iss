[Setup]
AppName=External OPM JSON package Editor
AppVersion=0.1.13.0
DefaultDirName={pf}\OPMUtilities
DefaultGroupName=OPM
UninstallDisplayIcon={app}\jsoneditor.exe
Compression=lzma2
SolidCompression=yes
OutputDir=.
; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
; On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64
; Note: We don't set ProcessorsAllowed because we want this
; installation to run on all architectures (including Itanium,
; since it's capable of running 32-bit code too).
LicenseFile=COPYING.GPL.txt
SetupIconFile=.\jsonpackage.ico
MinVersion=0,5.01
AppComments="Source code for this app is available on request"
AppContact="lainz or minesadorada at http://forum.lazarus.freepascal.org/"
AppCopyright="(c)2016 lainz and minesadorada"
OutputBaseFilename=setup_jsoneditor
AppPublisher="Open source software"
AlwaysShowDirOnReadyPage=yes
UsePreviousAppDir=no
InfoBeforeFile="readmepreinstall.txt"
InfoAfterFile="readme.txt"

[Files]
; Install MyProg-x64.exe if running in 64-bit mode (x64; see above),
; MyProg.exe otherwise.
; Place all x64 files here
Source: "jsoneditor64.exe"; DestDir: "{app}"; DestName: "jsoneditor.exe"; Check: Is64BitInstallMode
; Place all x86 files here, first one should be marked 'solidbreak'
Source: "jsoneditor.exe"; DestDir: "{app}"; Check: not Is64BitInstallMode; Flags: solidbreak
; Place all common files here, first one should be marked 'solidbreak'
; Source: "readmepreinstall.txt"; DestDir: "{app}"; Flags: isreadme
; Source: "readme.txt"; DestDir: "{app}"; Flags: isreadme
Source: ".\locale\jsoneditor.en.po"; DestDir: "{app}\locale";  Flags: solidbreak
Source: ".\locale\jsoneditor.es.po"; DestDir: "{app}\locale";

[Icons]
Name: "{group}\External OPM JSON package Editor"; Filename: "{app}\jsoneditor.exe"
Name: "{group}\{cm:UninstallProgram, External OPM JSON package Editor}"; Filename: "{uninstallexe}"

[Run]
Filename: "{app}\jsoneditor.exe"; WorkingDir: "{app}"; Flags: postinstall runascurrentuser; Description: "Start the JSON Editor now"; StatusMsg: "Start the JSON Editor now"

[Dirs]
Name: "{app}\locale"
