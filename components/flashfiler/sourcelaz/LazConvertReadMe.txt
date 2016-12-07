== TurboPower FlashFiler2Lazarus Port========
Ported from: 	Soner A.
State: 			Client and Server compiles without error.
				Client Engine working
				ServerEngine has error.(Use from compiled one from delphi)

Search in Source for "fpc" or "soner" to look changes.

**********
I substitute LazDbCommon.pas with lazcommon.pas!

NO MORE BORLAND CODE, It uses now TExprParser from  fssql.
    USED UNITS WITH BORLAND CODE:
	LazDbCommon.pas 	for TExprParser			used by ffdb.pas
	LazDbComSqlTimSt.pas						used only by LazDbCommon.pas (original name from Delphi is: SqlTimSt.pas)
	
	NEW_24.04.2016 you can compile now without delphi unit. It was used only in one "useless" function!
	Define in ffdefine.inc:
	{$DEFINE DONTUSEDELPHIUNIT} //Disables in ffdb.pas the function TffDataSet.dsCreateLookupFilter
								//if it called then it raises exception!
***********								
	
== TODO ====
1. It must be tested more. I am new to FlashFiler. I did not used it until yesterday.
2. Some component, property editors and experts for formular desgin must be ported. 
3. You should convert pred(variable) to variable-1 because of pred(word=0) error!
  they used it excessive (1144x!)

== Substituted classes ========
This classes/types/procedures aren't exist in Lazarus/Freepascal, I changed them:
unit		Original from ff2	New for Lazarus-port
--------	----------------	--------------------
ffclcoln	IDesigner 			TIDesigner;
			IDesignerSelections TComponent; //IDesignerSelections dont exist on laz
			TDesignerSelections TComponent;
			FDesigner.SetSelections(SelList);	FDesigner.SelectOnlyThisComponent(SelList); //soner es gibt ken setselections
for others search for fpc in sourcelaz-folder.

== BUGS/ISSUES	========
FIRST: I ported very fast, the "real" code for db is good ported but i had problems 
with the compents editors and experts because i don't know anything about that for lazarus.

1.[SOLVED, I USED IT FALSE]
it works but still error on start of programm, am I using it false? Why working Delphi examples with lazarus seamless and mine don't?]
MAY BE WRONG, test it again, i can play with original example in lazarus without problems

2.[SOLVED, I USED IT FALSE]
Design Editor: If you put TffDataBase and set Property DatabaseName to any value i.e. "mydatabase",
than "mydatabase" should be local alias and it should be shown at TffTable.DatabaseName. But it doesn't.
I think the problem can be:
	in ffdb.pas 
		-FieldDefList, FieldDefList.IndexOf(FullName); //class, function
	or in designeditors ffclreg, ffclreng..
	
3.	[SOLVED, I USED IT FALSE]
If you make with delphi example app (like in examples order) and import it to lazarus than it works, but if you make it with lazarus then it doesnot work.


4. [SOLVED -> all definied in ffclreg.dcr, delphi support images from base class but lazarus didn't:
   TffLegacyTransport is in ffclreg.dcr as baseclass: TFFBASETRANSPORT]
    I could not found some components images for the component palette:
	TffServerEngine
	TffServerCommandHandler
	TffLegacyTransport
	TffEventlog
	(all other has it in ffclreg.dcr)

5. [SOLVED] fpc makes pred(word=0) = 0 but delphi -1. (Look at ffdb.pas TffBaseTable.dsGetIndexInfo;)
  
6. [SOLVED]  
   You must set TffTable.IndexName to Valid else Lazarus will freeze!	
   An don't set TffTable.IndexName to "Sequential Access Index", Lazarus will be crash!
   I appears also on runtime of application

7. In fpc doesn't exists TWriter.Flushbuffer, so I made in ffclreng.pas hackclass TBinaryObjectWriterHack

8. 
EmbeddedSErver (TffServerEngine) don't works, because in fpc-classes TReader.ReadString can't read some string-types.
Unicode failure? Look examples\LazEmbeddedServer 


== Fast notices during converting/porting to lazarus ========
0. -------------------------
I replaced  ffdb.ReSizePersistentFields;  FieldDefList with Fielddefs because fpc doesn't has FieldDefList

1. -------------------------
ffclcoln.pas		ist parameter editor. i removed this from package because it is not converted to laz and removed from uses of ffclreg,

SelectComponentList()
//IDesignerSelections dont exist on laz
FDesigner.SetSelections(SelList); dont exist on laz

2. -------------------------
These Component editors or experts aren't converted and aren't used in lpk.
ffclver.pas  -version.property editor useles for programm dont converted
ffclexpt.pas -FlashFiler: TFFEngineManager Expert


3.  -------------------------
ffclreg.pas
Some Property editors and conditions (see below) disabled.
procedure TffServerEngineProperty.GetValueList(List: TStrings);
...
        if (Cmpnt is TffBaseServerEngine) and
       {$ifndef fpc} Designer.IsComponentLinkable(Cmpnt) and {$endif} //Soner don't exits on lazarus

{$ifndef fpc} //soner ParamEditor not converted
{ TffCollectionProperty }	   

  {register the experts}
  {$ifndef fpc} //Soner: I don't know how to do with lazarus
  RegisterCustomModule(TffBaseEngineManager, TCustomModule);
  RegisterLibraryExpert(TffEngineManagerWizard.Create);
  {$endif}

{$ifndef fpc} //don't converted
{$endif}

4. -------------------------
added some code from delphi look: lazsqltimst.pas, lazdbcommon.pas, (lazvclfuncs.pas, lazdbconsts.pas) 

5. -------------------------
Flashfiler typen
fftWideChar
fftWideString