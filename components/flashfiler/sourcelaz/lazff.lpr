//convert helper, project only for compiling, can be deleted now, there is lazpackage in packages folder
//compiled: 58units, max units: 84x
//converted in 6std
{hinweise
1. I replaced  ffdb.ReSizePersistentFields;  FieldDefList with Fielddefs because fpc doesn't has FieldDefList
Look also LazConvertReadMe.txt
}
program lazff;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  classes, forms,
  ffdb,
  fflllgcy,
  ffclreng;

type
  { TForm1 }
  TForm1 = class(TForm)
  public
    ffSess: TffSession;
    CustomerTable: TffTable;
    ffClient: TffClient;
    ffRSE: TFFRemoteServerEngine;
    ltMain: TffLegacyTransport;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FormShow(Sender: TObject);
  end;

var Form1: TForm1;

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:='FlashFiler2 for Lazarus';

  //ff2
  ltMain:= TffLegacyTransport.Create(self);
   ltMain.Enabled:=true;

  ffRSE:= TFFRemoteServerEngine.Create(self);
   ffRSE.Transport := ltMain;

  ffClient:= TffClient.Create(Self);
   ffClient.ClientName := 'ffClient';
    ffClient.ServerEngine := ffRSE;

  ffSess:= TffSession.Create(Self);
   ffSess.ClientName := 'ffClient';
   ffSess.SessionName := 'ExCust';

  CustomerTable:= TffTable.Create(self);
   CustomerTable.DatabaseName := 'Tutorial';
   CustomerTable.IndexName := 'ByID';
   CustomerTable.SessionName := 'ExCust';
   CustomerTable.TableName := 'ExCust';
   CustomerTable.Timeout := 10000;

  OnShow:=@FormShow;
end;

destructor TForm1.Destroy;
begin
  inherited Destroy;
end;

procedure TForm1.FormShow(Sender: TObject);
var aPath : string;
const csAlias = 'Tutorial';
begin
  ffSess.Open;
  if not ffSess.IsAlias(csAlias) then begin
    ffSess.AddAlias(csAlias, 'D:\AppDev\TDLite\Comps\flashfiler\examples',False);
    {aPath := ExtractFilePath(Application.ExeName);
    if aPath[Length(aPath)] <> '\' then  aPath := aPath + '\';
    { Path should point to the folder containing the Mythic tables. }
    ffSess.AddAlias(csAlias, aPath + '..', False);}
  end;
end;

begin
  RequireDerivedFormResource:=false;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

