unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, mvgeonames,
  mvMapViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    MapView1: TMapView;
    GeoNames: TMVGeoNames;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  mvTypes;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  MapView1.DoubleBuffered := true;
  MapView1.Zoom := 7;
  GeoNames.LocationName := 'New York';
  MapView1.Center := GeoNames.Search(MapView1.DownloadEngine);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  MapView1.Active := true;
end;

end.

