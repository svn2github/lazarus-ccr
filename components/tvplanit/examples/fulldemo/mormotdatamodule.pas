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
    FHostIP: String;
    procedure ReadCmdLine;
  public
    Datastore: TVpmORMotDatastore;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;
  end;

var
  DemoDM: TDemoDM;

implementation

{$R *.lfm}

const
  // IP address of AWS (amazon web service) tvplanit demoserver
  AWS_HOST_IP = '54.194.211.233';


constructor TDemoDM.Create(AOwner: TComponent);
begin
  inherited;

  ReadCmdLine;

  Datastore := TVpmORMotDatastore.Create(self);

  with Datastore do
  begin

    // if the HostIP is set, it will look for a running server on this IP
    // address when connecting.
    // leave blank (comment out) for a local (and private) database
    HostIP := FHostIP;
    HostPort := '8888';
    EnableLogging := True;

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

destructor TDemoDM.Destroy;
begin
  Timer1.Enabled := false;
  inherited;
end;

{ Use to commandline to switch between different servers.
  - noserver  --> don't use server, for a local (and private) database)
  - localhost --> use server on local system
  - (empty)   --> use AWS (amazon web service) tvplanit demo server
  - else      --> specify ip address of server }
procedure TDemoDM.ReadCmdLine;
var
  s: String;
  i: Integer;
begin
  FHostIP := AWS_HOST_IP;
  for i:=1 to ParamCount do begin
    s := lowercase(ParamStr(i));
    if s = 'noserver' then
      FHostIP := ''
    else
      FHostIP := s;
  end;
end;

procedure TDemoDM.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  Datastore.CheckUpdate;
  Timer1.Enabled := True;
end;


end.

