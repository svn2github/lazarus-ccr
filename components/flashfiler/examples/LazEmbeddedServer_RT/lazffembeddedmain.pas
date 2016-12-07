unit LazFFEmbeddedMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DbCtrls, DBGrids, ffsreng, ffdb;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    ToolBar1: TToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    ffClient1: TffClient;
    ffDatabase1: TffDatabase;
    ffServerEngine1: TffServerEngine;
    ffSession1: TffSession;
    ffTable1: TffTable;	
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var ServerFolder, DBFolder :string;
begin
  //Change Folders to your install
  ServerFolder:= 'D:\AppDev\TDLite\Comps\flashfiler\bin\';
  DBFolder    := 'D:\AppDev\TDLite\Comps\flashfiler\examples\mythicdb\';

  ffServerEngine1:= TffServerEngine.Create(self);
   ffServerEngine1.ConfigDir := ServerFolder;
   //ffServerEngine1.NoAutoSaveCfg:=true;
   //ffServerEngine1.CollectGarbage := True;
   ffServerEngine1.Startup;                 //error excepts at 3.run in
                                            //ffsreng.pas
                                            //LIne 6838: Dictionary.ReadFromFile(DataFile, aTI);

  ffClient1:= TffClient.Create(self);
   ffClient1.ClientName := 'FFClient_69729904';
   ffClient1.ServerEngine := ffServerEngine1;

  ffSession1:= TffSession.Create(self);
   ffSession1.ClientName := 'FFClient_69729904';
   ffSession1.SessionName := 'FFSession_69795446';

  ffDatabase1:= TffDatabase.Create(self);
   ffDatabase1.AliasName := DBFolder;
   ffDatabase1.DatabaseName := 'FFDB_282722134'; //-->Starts server if not already started

   ffDatabase1.SessionName := 'FFSession_69795446';

  ffTable1:= TffTable.Create(self);
   ffTable1.DatabaseName := 'FFDB_282722134';
   //ffTable1.FieldDefs := <>;
   ffTable1.FilterOptions := [];
   ffTable1.SessionName := 'FFSession_69795446';
   ffTable1.TableName := 'customer';

  DataSource1.DataSet:=ffTable1;

  //ffServerEngine1.Startup;
  //ffClient1.Active:=true;
  //ffSession1.Active:=true;
  ffDatabase1.Connected:=true;
  ffTable1.Active:=true;

end;

end.

