#!/bin/bash
#надо скопировать rx.inc в текущий каталог, иначе не соберём (глюк fpdoc)
cp ../rx.inc rx.inc 
  --input=../curredit.pas --descr=curredit.xml \
  --input=../dbdateedit.pas --descr=rxfpc.xml \
  --input=../dbutils.pas --descr=dbutils.xml \
  --input=../duallist.pas --descr=duallist.xml \
  --input=../folderlister.pas --descr=folderlister.xml \
  --input=../rxctrls.pas --descr=rxctrls.xml \
  --input=../rxdice.pas --descr=rxdice.xml \
  --input=../rxlookup.pas --descr=rxlookup.xml \
  --input=../rxlogin.pas --descr=rxlogin.xml \
  --input=../rxtoolbar.pas --descr=rxtoolbar.xml \
  --input=../rxspin.pas --descr=rxfpc.xml \
  --input=../rxclock.pas --descr=rxclock.xml \
  --input=../rxmemds.pas --descr=rxmemds.xml \
  --input=../rxswitch.pas --descr=rxswitch.xml \
  --input=../tooledit.pas --descr=tooledit.xml \
  --input=../pickdate.pas --descr=pickdate.xml \
  --input=../rxversinfo.pas --descr=rxversinfo.xml \
  --input=../rxtimeedit.pas --descr=RxTimeEdit.xml \
  --input=../rxdbcomb.pas --descr=rxdbcomb.xml \
  --input=../rxdbtimeedit.pas --descr=rxfpc.xml \
  --input=../rxdbgrid.pas --descr=rxdbgrid.xml \
  --input=../rxdbgrid_columsunit.pas --descr=rxfpc.xml \
  --input=../rxdbgrid_findunit.pas --descr=rxfpc.xml \
  --input=../rxdbctrls.pas --descr=rxfpc.xml \
  --input=../rxdbspinedit.pas --descr=rxfpc.xml \
  --input=../rxaboutformunit.pas --descr=rxfpc.xml \
  --input=../rxaboutdialog.pas --descr=rxaboutdialog.xml \
  --input=../dateutil.pas --descr=dateutil.xml \
  --input=../rxfileutils.pas --descr=rxFileUtils.xml \
  --input=../rxdbgridexportspreadsheet_paramsunit.pas --descr=rxfpc.xml \
  --input=../rxdbgridexportspreadsheet.pas --descr=rxfpc.xml \
  --input=../rxcloseformvalidator.pas --descr=rxcloseformvalidator.xml \
  --input=../rxmdi.pas --descr=rxmdi.xml \
  --input=../rxviewspanel.pas --descr=RxViewsPanel.xml \
  --input=../rxdbgridfootertools.pas --descr=rxdbgridfootertools.xml \
  --input=../rxdbgridfootertools_setup.pas --descr=rxdbgridfootertools_setup.xml \
  --input=../autopanel.pas --descr=autopanel.xml \
  --input=../boxprocs.pas --descr=boxprocs.xml \
  --input=../rxspin.pas --descr=rxspin \
  --input=../vclutils.pas --descr=vclutils.xml \
  --input=../rxdbgridexportpdf.pas --descr=rxdbgridexportpdf.xml \
  --input=../rxinipropstorage.pas --descr=rxinipropstorage.xml 


fpdoc --package=rxdbgrid_print --format=html --index-colcount=4 --hide-protected \
  --input=../rxdbgridprintgrid.pas --descr=rxdbgridprintgrid.xml

fpdoc --package=rxdbgrid_export_spreadsheet --format=html --index-colcount=4 --hide-protected \
  --input=../rxdbgridexportspreadsheet.pas --descr=rxdbgridexportspreadsheet.xml \ 
  --input=../rxdbgridexportspreadsheet_paramsunit.pas rxdbgridexportspreadsheet_paramsunit.xml
  
fpdoc --package=rx_sort_zeos --format=html --index-colcount=4 --hide-protected \
  --input=..exsortzeos.pas --descr=exsortzeos.xml \
  --input=../rxsortzeos.pas --descr=rxsortzeos.xml