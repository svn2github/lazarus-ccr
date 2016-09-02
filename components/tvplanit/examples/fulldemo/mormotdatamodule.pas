unit mormotdatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Dialogs, ExtCtrls, VpmORMotDS;

type

  { TDemoDM }

  TDemoDM = class(TDataModule)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    FNoServer: Boolean;
    procedure ReadCmdLine;
  public
    Datastore: TVpmORMotDatastore;
    constructor Create(AOwner: TComponent); override;
  end;

var
  DemoDM: TDemoDM;

implementation

{$R *.lfm}

constructor TDemoDM.Create(AOwner: TComponent);
begin
  inherited;

  ReadCmdLine;

  Datastore := TVpmORMotDatastore.Create(self);

  with Datastore do
  begin
    // if the HostIP is set, it will look for a running server on this IP address when connecting.
    // leave blank (comment out) for a local (and private) database

    if FNoServer then
      HostIP := '' else
      HostIP := 'localhost';
    Directory := 'data';
    Connected := true;

    if (Length(HostIP) > 0) and (not Connected) then
    begin
      MessageDlg('Cannot connect with server', mtError, [mbOk], 0);
      Application.Terminate;
    end;
  end;

  Timer1.Enabled := true;
end;

procedure TDemoDM.ReadCmdLine;
var
  s: String;
  i: Integer;
begin
  for i:=1 to ParamCount do begin
    s := lowercase(ParamStr(i));
    if (s[1] = '-') or (s[1] = '/') then begin
      Delete(s, 1, 1);
      if s = 'noserver' then
        FNoServer := true;
    end;
  end;
end;

procedure TDemoDM.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  Datastore.CheckUpdate;
  Timer1.Enabled := True;
end;


end.

