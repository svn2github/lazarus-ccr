The folder "image_sources" is planned to store source files from which icons
or glyphs used by Lazarus or packages are derived. This is particularly 
important because, starting with version 1.8, Lazarus supports images with
150% and 200% magnification relativ to the standard size. Although the
IDE can upscale the standard image size to the new sizes a loss in image
quality will be observed which can be considerable in some cases. To avoid this,
these images can be redrawn. If icons and glyphs (normally png files) are
generated from vector files, or if multiple images are
combined into a multi-layer "bitmap" file (e.g. by using gimp), then these 
source files can be stored here. If changes will be needed later the png images
needed by the resource building scripts can easily be regenerated this way.

The folder contains two sub-folders
- lazarus: this one is intended for images distributed along with lazarus
- ccr: this one is for images used by any of the ccr components or applications.

To facilitate finding of images and components/applications further subfolders 
should be introduced to resemble the directory tree of the corresponding
installation.

The folder of every "destination" (component package, or application) should 
contain a readme file explaining the source of images  taken from third-party 
collections.
