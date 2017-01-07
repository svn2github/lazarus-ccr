unit usplash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { Tsplashform }

  Tsplashform = class(TForm)
    img: TImage;
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
var jpg:TJPEGImage;
begin
     jpg:=TJPEGImage.Create;
     try
        jpg.LoadFromResourceName(HInstance,'SPLASHIMAGE');
        Img.Canvas.Draw(0,0,jpg);
     finally
        jpg.Free;
     end;
end;

end.

