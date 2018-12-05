unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, rxlogin;

type

  { TForm1 }

  TForm1 = class(TForm)
    RxLoginDialog1: TRxLoginDialog;
    procedure RxLoginDialog1AfterLogin(Sender: TObject);
    procedure RxLoginDialog1BeforeLogin(Sender: TObject);
    procedure RxLoginDialog1UnlockApp(Sender: TObject; const UserName,
      Password: string; var AllowUnlock: Boolean);
  private

  public

  end;

var
  Form1: TForm1;

procedure LocalizeApp;
implementation
uses gettext, translations, rxFileUtils;

{$R *.lfm}

procedure LocalizeApp;
var
  Lang, FallbackLang: String;
begin
  GetLanguageIDs(Lang{%H-},FallbackLang{%H-}); // in unit gettext
  TranslateUnitResourceStrings('rxconst', NormalizeDirectoryName('../../languages/rxconst.%s.po'), Lang, FallbackLang);
  TranslateUnitResourceStrings('rxdconst', NormalizeDirectoryName('../../languages/rxdconst.%s.po'), Lang, FallbackLang);
end;

{ TForm1 }

procedure TForm1.RxLoginDialog1UnlockApp(Sender: TObject; const UserName,
  Password: string; var AllowUnlock: Boolean);
begin
  ShowMessage('Password = '+ Password);
  AllowUnlock:=true
end;

procedure TForm1.RxLoginDialog1BeforeLogin(Sender: TObject);
begin
  //
end;

procedure TForm1.RxLoginDialog1AfterLogin(Sender: TObject);
begin
  //
end;

end.

