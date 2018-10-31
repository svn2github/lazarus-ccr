Fake cmd console application by minesadorada
============================================

Installation
============
The command processor (cmd.exe) is located in your Windows\System32 folder.

There are 2 ways to replace it with this fake anti-scammer version.
1. Make a backup of your real cmd.exe file, and copy over it with this version.
   * You will need to be logged on as administrator in order to do this.
   * Only do this in a Virtual Machine!
   * The tech scammer has no access to the real cmd.exe - so it is safer.
or   
2. Copy the file cmd.com to your Windows\System32 folder. (you can rename cmd.exe to cmd.com and Windows doesn't complain)
   * Windows will always prefer to run a .com file before an .exe file.
   * If you type 'cmd' in the run dialog (Win key + r), the cmd.com (fake app) will run.
   * To run the real cmd.exe, just type 'cmd.exe' in the run dialog.
   * There is a small risk that the tech scammer wont be lazy, and types the full 'cmd.exe' command.
     * Normally they type 'cmd' or ask you to type 'cmd' - which will run the fake cmd.com app.

Purpose
=======
This is designed to frustrate and fool Tech Support scammers.

If you let them connect remotely to your computer then they typically run DOS (cmd) commands like:

1. netstat - the list of usually perfectly legitimate TCP connections are declared by the tech scammer to be 'hackers'
   who have 'taken control of your system' as a 'proof' that your computer is infected with viruses.
   
   This version of cmd.exe shows a random list of connections, then offers to 'Scan foreign addresses for hackers'
   The scan always shows all the connections are safe of course.
   If the tech scanner refuses the scan, then 'WARNING: Scan was intentionally canceled - please run netstat command again' is displayed.
   
 2. dir or tree - this innocent command to list all the files in the current folder is used bt the tech scammer as a distraction.
    He/she waits for the listing to finish, then copy/pastes a fake warning message at the end which is supposed to scare the user.
	
	Two can play that game :)
	This version of cmd.exe shows simulated directory listing with random folders and filenames (different every time).
	Some of the folders and files will look very tempting to the tech scammer (e.g. bank details.doc, paswwords.doc)
	Unfortunately (for the scammer) at the end of the listing the 'helpful' prompt 'Scan this folder for infections? Y/N' is shown.
	Any key at all will start the scan, which of course will report all is clean and safe. The scammers warning message is made impotent.
	
3. cd.. or cd\ or D:, E: etc - the tech scammer may attempt to change the current folder or drive before listing files etc.
   
   This version of cmd.exe starts in the genuine users folder for your system, but of course this is a distraction to convince the tech scammer.
   He/she can try to navigate using directory commands, but everything is fake.  All that is happening is that the prompt text changes.
   At no time will this cmd.exe allow genuine access to your disk - its all an illusion to frustrate the scammer.
   
4. format, syskey del, delete, erase - if the tech scammer wants to 'punish' you, they may start cmd.exe and try to use it
   to trash your system and/or erase files and folders.

   Look and laugh! This version of cmd.exe will happily accept the commands, and appears to do the tech scammers ugly work
   but..
   Its all a fantasy of course.  Your system is safe because this version of cmd.exe is just for show, and cannot access your system.
   Whilst the tech scammer is 'formatting', 'deleting' etc all kinds of scary messages are shown but nothing actually happens.

 Other fake cmd.exe replacements sometimes try to be amusing with wacky responses to DOS commands.  Even the stupid tech scammers
 will soon realise you are faking them, and wheres the fun in that?
 This version tries to be realistic, but frustrating.  It is designed to waste their time, and keep your system safe.
 
 Replacing your windows\system32\cmd.exe with this fake version in your virtual machine will hopefully give you a laugh when the tech scammer
 uses it, and will probably convince the technically incompetent ones (most of them) that they are accessing the genuine article.
 
 This version of cmd.exe reports the system as 'Windows 10', and has all the correct versioninfo that shows in Task Manager. Icon is from the genuine cmd.exe.
 It was written using lazarus/free pascal.
 Released under LGPLv2 license.
 
======================
:minesadorada Nov 2018
 

   