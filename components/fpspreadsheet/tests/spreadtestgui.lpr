program spreadtestgui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, datetests, stringtests, numberstests,
  manualtests, testsutility, internaltests, formattests, colortests, fonttests,
  optiontests, numformatparsertests, formulatests, rpnFormulaUnit,
  emptycelltests;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

