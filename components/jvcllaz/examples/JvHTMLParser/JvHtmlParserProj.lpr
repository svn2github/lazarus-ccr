program JvHtmlParserProj;

uses
  Interfaces, Forms,
  JvHTMLParserMainFormU in 'JvHTMLParserMainFormU.pas' {JvHTMLParserMainForm};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.Title:='JvHtmlParserProj';
  Application.CreateForm(TJvHTMLParserMainForm, JvHTMLParserMainForm);
  Application.Run;
end.

