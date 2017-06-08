This folder contains the images used by the Lazarus version of the
TurboPower VisualPlanIt components:

- folder "palette": component palette icons, designtime only

- folder "components": images used internally by the components, runtime and
  designtime

See these subfolders for details on the origin of the images.

In order to create the .res files for the package, run the batch file 
"make_res.bat". Adapt it such that the lazres program can be found. The sources
of lazres are in the folder "tools" of the Lazarus installation; compile it 
if not yet done.

The batch file, obviously, is for Windows only, but a Linux shell script can be
written accordingly. 
