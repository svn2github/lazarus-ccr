unit vpregsqlite3;
  { Registration unit for the SQLite3 connection using SqlDb }

{$I Vp.INC}            // Compiler version defines
{$R vpregsqlite3.res}   // Palette glyphs

interface

uses
 {$IFDEF DELPHI}
 // Windows,
  {$IFDEF VERSION6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
 {$ENDIF}
  Classes;

procedure Register;

implementation

uses
  VpSqlite3DS;

procedure Register;
begin
  RegisterComponents('Visual PlanIt', [TVpSqlite3Datastore]);
end;

end.
