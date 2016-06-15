unit VpRegZEOS;
  {-Registration unit for the ZEOS database components}

{$I Vp.INC}    { Compiler Version Defines }
//{$R VpReg.RES} { Palette Glyphs           }

interface

uses
  Windows, Dialogs,
 {$IFDEF FPC}
  PropEdits,
 {$ELSE}
  {$IFDEF VERSION6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
 {$ENDIF}
  ZPropertyEditor,
  Classes, Controls, TypInfo;
                                    (*
type
  { Implements a property editor for VpZEOSDatastore.Database property. }
  TVPZeosDatabasePropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  { Implements a property editor for VpZEOSDatastore.LibLocation property. }
  TVPZeosLibLocationPropertyEditor = class(TZLibLocationPropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  {** Implements a property editor for ZConnection.ClientCodePage property. }
  TVpZeosClientCodepagePropertyEditor = class (TZClientCodePagePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;
                                      *)

procedure Register;

implementation

uses
  VpZeosDS;

procedure Register;
begin
  RegisterComponents('Visual PlanIt', [TVpZeosDatastore]);
                               (*
  RegisterPropertyEditor(TypeInfo(string), TVpZEOSDatastore, 'Protocol', TZProtocolPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TVpZEOSDatastore, 'Database', TVpZeosDatabasePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TVpZEOSDatastore, 'LibraryLocation', TVpZeosLibLocationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TVpZEOSDatastore, 'ClientCodepage', TVpZeosClientCodePagePropertyEditor);
  *)
end;

    (*
{ TVpZeosDatabasePropertyEditor }

function TVpZeosDatabasePropertyEditor.GetZComponent: TPersistent;
begin
  if (GetComponent(0) is TVpZeosDatastore) then
    Result := (GetComponent(0) as TVpZeosDatastore).Connection;
end;

{ TVpZeosLibLocationPropertyEditor }

function TVpZeosLibLocationPropertyEditor.GetZComponent: TPersistent;
begin
  if (GetComponent(0) is TVpZeosDatastore) then
    Result := (GetComponent(0) as TVpZeosDatastore).Connection;
end;

{ TVpZeosClientCodePagePropertyEditor }

function TVpZeosClientCodePagePropertyEditor.GetZComponent: TPersistent;
begin
  if (GetComponent(0) is TVpZeosDatastore) then
    Result := (GetComponent(0) as TVpZeosDatastore).Connection;
end;
      *)

end.
