================================================================================
TurboPower SysTools
================================================================================

Table of contents
-----------------

1.  Introduction
2.  Package names
3.  Installation


================================================================================

1. Introduction
---------------

SysTools is a library of utility routines and classes initially for Borland
Delphi and C++Builder. It was extended to support also Lazarus and FPC.
It includes 1-D and 2-D bar codes, sorting, money routines, logging, 
high-precision math, run-time math expression analyzer, and much more.

This is a source-only release of TurboPower SysTools for Lazarus only. 


================================================================================

2. Package names
----------------

laz_systools.lpk is the runtime package with all standard routines and 
classes which work in a platform independent way.

laz_systools_design.lpk is the corresponding designtime package.

laz_systoolsdb.lpk is the runtime package with the data-aware barcode 
components.
laz_systoolsdb_design.lpk is the related designtime package.

laz_systoolswin.lpk is the runtime package with all routines and classes which
could not be converted to become cross-platform and therefore work only 
under Windows.
laz_systoolswin_design.lpk is the related designtime package.


==============================================

3. Installation
---------------

To install TurboPower SysTools into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\systools).

  2. Start Lazarus.

  3. Select the menu command "Package" > "Open package file (*.lpk)"
  
  4. Navigate to the folder into which the release files were unzipped
     and open laz_systools.lpk. Compile it.
     
  5. Open laz_systools_design.lpk. Compile it. Then click "Use" > "Install".
     Do not rebuild the IDE here.
     
  6. Repeat steps 4 and 5 with the packages laz_systoolsdb.lpk and
     laz_systoolsdb_design.lpk.
     
  7. Repeat steps 4 and 5 with the packages laz_systoolswin.lpk and
     laz_systoolswin_design.lpk.
     
  8. In each case, always COMPILE the runtime package and "USE" > "INSTALL"
     the designtime package (the one with "_design" appended).
     
  9. After the last designtime package, confirm to rebuild the IDE.
  
 10. After some time, Lazarus restarts. You find the new components in the 
     palette "systools".
     
