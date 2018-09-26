unit JvNetReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvnet.res}

uses
  Classes, Controls,
  JvDsgnConsts, JvHtmlParser, JvFormToHtml, JvStringListToHtml, JvStrToHtml;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvHtmlParser, TJvFormToHtml, TJvStringListToHtml, TJvStrToHtml
  ]);
end;

end.
