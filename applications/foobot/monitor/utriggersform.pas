unit utriggersform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { Ttriggersform }

  Ttriggersform = class(TForm)
    cmd_cancel: TBitBtn;
    cmd_OK: TBitBtn;
    grp_main: TGroupBox;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  triggersform: Ttriggersform;

implementation
Uses umainform;
{$R *.lfm}

{ Ttriggersform }

procedure Ttriggersform.FormCreate(Sender: TObject);
begin
  Icon:=Application.Icon;
  Caption:=Application.Title + ' - Set Triggers';
end;

end.

