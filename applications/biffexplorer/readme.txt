BIFF Explorer
--------------------------------------------------------------------------------

"BIFF Explorer" is a tool to peek into the internal structure of a binary
Excel file in biff format.

It displays a list of the BIFF records contained in the xls file along with 
name and explanation as described in various documentation files (see "About"). 
Selecting one of the records loads its bytes into a simple hex viewer; for the 
most important records I tried to decipher the contents of the hex values and 
display their meaning in a grid and a memo (page "Analysis"). For the other 
records select a byte in the hex viewer, and the program will display the 
contents of that byte and the following ones as integer, double, string 
(page "Values").

For compiling, note that the program requires the following packages:

- "KControls" (ccr version)
- "lclextensions" (use version from Online-Package Manager)
- "VirtualTreeview4" (version 4.8.7.4, install using the Online-Package manager)
