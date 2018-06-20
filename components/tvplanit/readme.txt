TurboPower Visual PlanIt


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1   Release 1.0.3
4.2   Release 1.0.4
4.3   Release 1.0.6
4.4   Releases 1.0.8 and 1.0.10
4.5   Release 1.2.0
5.  Additional help
5.1   Original TurboPower documentation
5.2   Wiki page

================================================================================


1. Introduction

Visual PlanIt is a set of synchronized, data-aware components for
adding time, task, & contact management capabilities to applications
written in Borland Delphi & C++Builder. Get that Outlook look & feel
without the hassle.


================================================================================

2. Package names

These are the TurboPower Visual PlanIt packages available for the Lazarus 
version:

  laz_visualplanit.lpk             --- runtime package for the common components
  laz_visualplanit_design.lpk      --- designtime package
  laz_visualplanit_zeos.lpk        --- runtime package for the zeos datastore
  laz_visualplanit_zeos_design.lpk --- designtime package for the zeos datastore
  
The old naming scheme of the original Delphi components is discontinued.


================================================================================

3. Installation

To install TurboPower Visual PlanIt into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\vplanit).

  2. Start Lazarus
  
  3. Go to "Package" / "Load package file .lpk" and select the run-time package
     laz_visualplanit.lpk. Compile.
     
  4. Open the designtime package laz_visualplanit_design.lpk. 
     Click "Use" > "Install". If you want to install the addon package for 
     zeos components decline to rebuild the IDE and contine with 5, 
     otherwise go to 7.
     
  5. Open the zeos runtime package laz_visualplanit_zeos.lpi. Compile.
  
  6. Open the zeos designtime package laz_visualplanit_design.lpi. 
     Click "Use" > "Install".
     
  7. Confirm to rebuild the IDE.
  
  8. After some time the IDE will restart with the new components installed.
  
================================================================================


4. Version history

4.1 Release 1.0.3

  Please note that the following issues are from Bugzilla. These
  bugs were not exported to SourceForge.

  Bug fixes
  ------------------------------------------------------------------------------
  3547 - List Index out of Bounds error
  3589 - Needs OnDblClick Event
  3877 - ContactGrid won't scroll to a newly selected contact if it is
         out of view.
  3979 - FlexDataStore bug
  4021 - TVpTask.SetChanged marks Events dirty instead of taks. (duh!)
  4076 - VPDBISAMDataStore needs an AfterPost event.
  4078 - 12 and 24 hour display backward in the Events
  4079 - Using the DBIsamDataStore, recurring events show up under all
         resources.
  4080 - De Piggify the DBIsamDataStore component.
  
4.2 Release 1.0.4

  This release is Lazarus-only. It won't compile under Delphi any more. 
  - Replace hard-coded MS Sans Serif font by "default" to use the system font
  - Fix crashes due to inplace editors being destroyed.
  - Replace lrs resources by res resources. FPC 2.4 or greater required!
  - Add FlexDatastore and its component editor to package.
  - Rename package to laz_visualplanit.
  - Add new event field "Location". Rename some fields for more consistency. 
  - Add datastores for TBufDataset, Sqlite3, ZEOS, Firebird
  - Add non-database datastores for xml and ini files
  - Fix NavBar, PrintPreview, VpClock etc.
  - Translate user interface by means of po files
  - DPI-aware and translation-tolerant form layout
  - Add new sample projects
  
4.3 Release 1.0.6

  - Add mORMot datastore
  - Add drag and drop of events to DayView and WeekView
  - Add hint support to DayView, WeekView, MonthView, and Contact Grid
  - New contact fields for
    - three email addresses (in total)
    - two websites
    - 2nd address (work address, home address)
    - department
    and add them to the contact editor; redesigned contact editor.
  - Activate task fields Priority and Category in task editor.
  - Holiday support
  - Separate design-time and run-time packages
  
4.4 Releases 1.0.8 and 1.0.10

  - Bug fixes
  - JSON datastore
  - Improved integration of the LCL scaling of Lazarus 1.8
  - VpNavBar component editor working in Lazarus version
  
4.5 Release 1.2.0
 
  - New properties ShowNavButtons, FixedDate, RowHeight, RowLinesStep, 
    SimpleRowTime of VpDayView
  - VpContactGrid can import contacts from vCard files (*.vcf)
  - VpDayView/VpWeekView and VpTaskList can import events and tasks,
    respectively, from iCalendar files (*.ical, *.ics)
  - New property CaptionStyle of VpContactButtons.

  
================================================================================


5.  Additional help


5.1   Original TurboPower documentation

Get TurboPower's original 300-page pdf file from the official 
SourceForge site of the Delphi version 
https://sourceforge.net/projects/tpvplanit/files/tpvplanit_docs/


5.2   Wiki page

The wiki site http://wiki.freepascal.org/Turbopower_Visual_PlanIt contains a
short documentation of the Lazarus specific version together with a "Getting
started".



