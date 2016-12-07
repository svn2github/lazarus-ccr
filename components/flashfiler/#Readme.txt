TurboPower FlashFiler2 Lazarus port
Used original version:tpflashfiler_2_13 from SourceForge
https://sourceforge.net/projects/tpflashfiler/


Port infos are in sourcelaz\LazConvertReadMe.txt

Lazaruspackage is in folder packages: lazff2.lpk

Look the image for folderstructre. I zipped only changed files. Other files are located on sourceforge.
In finalversion will be inclued all and published on github/sourceforge/..

***
NO MORE BORLAND CODE, It uses now TExprParser from  fssql.
	sourcelaz\lazdbcommon.pas ->since 2016.05.04: (lazcommon.pas and lazconsts.pas)
	sourcelaz\LazDbComSqlTimSt.pas	<--- used in lazdbcommon.pas
    To disable Delphi units define in ffdefine.inc: (compiles without delphi units)
	{$DEFINE DONTUSEDELPHIUNIT} //Disables in ffdb.pas the function TffDataSet.dsCreateLookupFilter
								//if it called then it raises exception!
**************								

FOR EXAMPLES configure server (flashfiler\bin\ffserver.exe) and 
make 2 aliases in [ffserver-Menu > Config > Aliases ... ]
	Alias:		Path:
	mythicdb 	yourfolder\flashfiler\examples\mythicdb
	Tutorial	yourfolder\flashfiler\examples
	
	
THERE IS TEXPRPARSER in:
  -JVCL JvExprParser.pas
  -TXQuery  QExprYacc.pas with MozillaPublicLicense
	
Have fun!

Soner A.