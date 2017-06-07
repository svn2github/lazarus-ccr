lazres ..\source\design\vpreg.res @reg_palette_list.txt
lazres ..\source\addons\zeos\vpregzeos.res @reg_palette_zeos_list.txt

goto :end

..\lazres ..\vpreg.res @..\reg_files.txt
..\lazres ..\vpregzeos.res @..\regzeos_files.txt
cd ..\res
..\lazres ..\vpbasepng.res @..\regbasepng_files.txt
cd ..

@echo Copy *.RES to SOURCE folder (CTRL-BREAK -- NO, ENTER -- YES)
@pause
copy vpreg.res ..\source
copy vpregzeos.res ..\source\addons\zeos
copy vpbasepng.res ..\source



:end