unit usplash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { Tsplashform }

  Tsplashform = class(TForm)
    img: TImage;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  splashform: Tsplashform;

implementation

{$R *.lfm}

{ Tsplashform }

procedure Tsplashform.FormCreate(Sender: TObject);
begin
end;

procedure Tsplashform.FormActivate(Sender: TObject);
var jpg:TJPEGImage;
begin
     jpg:=TJPEGImage.Create;
     try
        jpg.LoadFromResourceName(HInstance,'SPLASHIMAGE');
        img.Picture.Jpeg:=jpg;
     finally
        jpg.Free;
     end;
end;

end.

