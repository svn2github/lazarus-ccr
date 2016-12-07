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
    ffClient1: TffClient;
    ffDatabase1: TffDatabase;
    ffServerEngine1: TffServerEngine;
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
  //Embeddedserver don't work in classes.pas the function TReader.ReadString
  //raises "Invalid Value for property" because fpc-classes can't handle some string property
  //program stops in fflldict.pas procedure TffDataDictionary.ReadFromStream(S : TStream);
  ffDatabase1.Connected:=true;
  ffTable1.Active:=true;
end;

end.

