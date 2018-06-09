unit bufdsdatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VpBufDS;

type

  { TDemoDM }

  TDemoDM = class(TDataModule)
    Datastore: TVpBufDSDataStore;
  private

  public
//    constructor Create(AOwner: TComponent); override;

  end;

var
  DemoDM: TDemoDM;

implementation

{$R *.lfm}

end.

