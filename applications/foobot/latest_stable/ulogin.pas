unit ulogin;

{ Foobot Interrogator

  Copyright (C)2016 Gordon Bamber minsadorada@charcodelvalle.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { Tloginform }

  Tloginform = class(TForm)
    cmd_OK: TBitBtn;
    edt_emailaddress: TLabeledEdit;
    edt_password: TLabeledEdit;
    GroupBox1: TGroupBox;
    procedure cmd_OKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    function ValidEmail(sEmail: string): boolean;
  public

  end;

var
  loginform: Tloginform;

implementation

{$R *.lfm}
uses umainform;

{ Tloginform }

procedure Tloginform.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  Caption := Application.Title + ' Login';
  edt_emailaddress.Text := mainform.INI.ReadString('Foobot', 'Foobot User',
    'myname@myserver.com');
  edt_password.Text := mainform.INI.ReadString('Foobot', 'Foobot Password', 'password');
end;

procedure Tloginform.cmd_OKClick(Sender: TObject);
begin
  Close;
end;

procedure Tloginform.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not ValidEmail(edt_emailaddress.Text) then
  begin
    MessageDlg(Application.Title, edt_emailaddress.Text + LineEnding +
      ' is not a valid email address', mtError, [mbOK], 0);
    CanClose := False;
  end
  else
    CanClose := True;
  mainform.INI.WriteString('Foobot', 'Foobot User', edt_emailaddress.Text);
  mainform.INI.WriteString('Foobot', 'Foobot Password', edt_password.Text);
end;

function Tloginform.ValidEmail(sEmail: string): boolean;
var
  at, dot, i: integer;
  bOkay: boolean;
begin
  at := Pos('@', sEmail);
  dot := LastDelimiter('.', sEmail);
  bOkay := (at > 0) and (dot > at);
  if bOkay then
  begin
    for i := 1 to Length(sEmail) do
    begin
      if not (sEmail[i] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '@']) then
      begin
        bOkay := False;
        break;
      end;
    end;
  end;
  Result := bOkay;
end;

end.
