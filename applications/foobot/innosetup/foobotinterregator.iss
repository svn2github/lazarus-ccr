; -- 64BitTwoArch.iss --
; Demonstrates how to install a program built for two different
; architectures (x86 and x64) using a single installer: on a "x86"
; edition of Windows the x86 version of the program will be
; installed but on a "x64" edition of Windows the x64 version will
; be installed.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=Foobot Interregator
AppVersion=0.0.2.0
DefaultDirName={pf}\foobotinterrogator
DefaultGroupName=Foobot Interregator
UninstallDisplayIcon={app}\foobotinterrogator.exe
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
OutputBaseFilename=foobotinterrogator_setup
SetupIconFile=..\foobot.ico
AppPublisher="Open source software"
AlwaysShowDirOnReadyPage=yes

[Files]
; Install MyProg-x64.exe if running in 64-bit mode (x64; see above),
; MyProg.exe otherwise.
; Place all x64 files here
Source: "..\compiled\win64\foobotinterrogator64.exe"; DestDir: "{app}"; DestName: "foobotinterrogator.exe"; Check: Is64BitInstallMode
; Place all x86 files here, first one should be marked 'solidbreak'
Source: "..\compiled\win32\foobotinterrogator.exe"; DestDir: "{app}"; Flags: solidbreak; Check: not Is64BitInstallMode
; Place all common files here, first one should be marked 'solidbreak'

[Icons]
Name: "{group}\Foobot Interrogator"; Filename: "{app}\foobotinterrogator.exe"

[Run]
Filename: "{app}\foobotinterrogator.exe"; WorkingDir: "{app}"; Flags: postinstall runascurrentuser; Description: "Start Foobot Interregator now"; StatusMsg: "Start Foobot Interregator now"
