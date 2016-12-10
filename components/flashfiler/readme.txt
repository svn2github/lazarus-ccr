--------------------------------------------------------------------------------
About
--------------------------------------------------------------------------------
This is a Lazarus port of the TurboPower FlashFiler Database. 
I used the version tpflashfiler_2_13 from SourceForge
(https://sourceforge.net/projects/tpflashfiler/).

Detailed help and documentation files are located there.
More port infos are in sourcelaz\LazConvertReadMe.txt


--------------------------------------------------------------------------------
Preparation
--------------------------------------------------------------------------------
Download the server binaries from 
https://sourceforge.net/projects/tpflashfiler/files/tpflashfiler/2.13/tpflashfiler_bin.zip/download
and store them in the folder server_files.


--------------------------------------------------------------------------------
Installation
--------------------------------------------------------------------------------
Use package file lazff2.lpk from folder packages.


--------------------------------------------------------------------------------
Usage
--------------------------------------------------------------------------------
1.) Start server_files\ffserver.exe
2.) Make 2 db-aliases in ffserver [ffserver-Menu > Config > Aliases ...]
	Alias:		Path:
	mythicdb 	yourfolder\flashfiler\examples\mythicdb
	Tutorial	yourfolder\flashfiler\examples
3.) Open FlashFiler Server General Configuration Dialog
	[ffserver-Menu > Config > General ...]
4.) In configuration dialog Enter for Server name: 
	local
	then Click Ok.
5.) Now the server "local" appears in Servers listview. Click on it and start it.
6.) Now open any example from examples-folder and compile, run and enjoy it.
	Attention: EmbeddedServer-Examples don't work!
	

--------------------------------------------------------------------------------
Changes
--------------------------------------------------------------------------------
State of the Lazarus port:
10.12.2016: Client components are Working. Server components has error so you need server binaries compiled with delphi.


ToDo:
Solve server components error. The error is located in fflldict.pas-file in procedure TffDataDictionary.ReadFromStream(S : TStream); 
It is stream reading error with caused by functions ReadString and ReadInteger.
I could not solve it, maybe someone with better skills can do it.


--------------------------------------------------------------------------------
License
--------------------------------------------------------------------------------
Same as TurboPower FlashFiler (MPL 1.1.)


--------------------------------------------------------------------------------
Author
--------------------------------------------------------------------------------
Turbo Power
Lazarus Port Soner a.


--------------------------------------------------------------------------------
Version
--------------------------------------------------------------------------------
tpflashfiler_2_13-20161210
