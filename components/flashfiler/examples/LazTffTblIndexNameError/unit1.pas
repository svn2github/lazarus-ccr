unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, DbCtrls, DBGrids, fflllgcy, ffsreng, ffclreng, ffdb;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    ffClient1: TffClient;
    ffDatabase1: TffDatabase;
    ffLegacyTransport1: TffLegacyTransport;
    FFRemoteServerEngine1: TFFRemoteServerEngine;
    ffSession1: TffSession;
    ffTable1: TffTable;
    ToolBar1: TToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 2016.04.25 SOLVED (pred(0)=0 error look at ffdb.TffBaseTable.dsGetIndexInfo;)
  // if TffTable.IndexName='' then TffTable.Active:=True; causes exception!
  ffTable1.IndexName:='';//<-- 1.
  //ffTable1.IndexName:='Sequential Access Index';//test
  ffTable1.Active:=True; //<-- 2. Exception
  Caption:='test';

{Result of one Debug session
ffllbase.pas
first -->
  Zeile 6227
  rwpGate.Lock
  (rwpGate is TffPadLock)
then  -->
  Row 6377
  Called very often (enless? until Timeout?)

  procedure TffPadLock.Lock;
  begin
    if IsMultiThread then begin
      EnterCriticalSection(plCritSect);
      inc(plCount);
    end;
  end;

Forget next lines, they are secundary errors (timeout, while debugging) :
Current debug run (stop, trace...) i get this error:
"Timed out waitig for reply"

then ---> ffdtmsq.pas
	row 195
	aTail^.dmnNext := aNode;
	"aTail is nil"
}
end;

end.

