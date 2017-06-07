This folder contains the palette icons of the TurboPower VisualPlanIt components.
The icons with appended _150 and _200 are magnifited with respect to the
icons without appended number by factors 150% and 200%, respectively; they are 
used for screens at higher resolutions.

The icons are created from several gimp source files in the image_sources
directory; they are based on selected images taken from the
FatCow icon collection (http://www.fatcow.com/free-icons, license 
Creative Commons Attribution 3.0). Some images are modified (upscaled, recolored,
etc).

In order to create the .res file for the package, run the batch file 
"make_res.bat". Adapt it such that the lazres program can be found. The sources
of lazres are in the folder "tools" of the Lazarus installation; compile it 
if not yet done.

The batch file, obviously, is for Windows, but a Linux shell script should work
accordingly). 
