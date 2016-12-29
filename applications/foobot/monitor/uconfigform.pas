unit uconfigform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { Tconfigform }

  Tconfigform = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    grp_main: TGroupBox;
    edt_username: TLabeledEdit;
    Label1: TLabel;
    Memo1: TMemo;
    procedure edt_usernameEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure Memo1EditingDone(Sender: TObject);
  private
    bDoneUsername,bDoneSecretKey:Boolean;
    function ValidEmail(sEmail: string): boolean;
  public
     FoobotUsername,FoobotSecretKey:String;
     bValid:Boolean;
  end;

var
  configform: Tconfigform;

implementation
Uses umainform;
{$R *.lfm}

{ Tconfigform }
function Tconfigform.ValidEmail(sEmail: string): boolean;
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

procedure Tconfigform.FormCreate(Sender: TObject);
begin
  Caption:='Configure ' + Application.Title;
  Icon:=Application.Icon;
  bDoneUsername:=FALSE;
  bDoneSecretKey:=FALSE;
  bValid:=False;
  FoobotUsername:= mainform.INI.ReadString('Foobot', 'Foobot User', 'unknown');
  FoobotSecretKey:= mainform.INI.ReadString('Foobot', 'Secret Key', 'unknown');


end;

procedure Tconfigform.Memo1EditingDone(Sender: TObject);
begin
   If (Memo1.Text='Copy + Paste here') then
   begin
      MessageDlg(Application.Title,
      edt_username.Text + ' is not a valid API key. Try again',
      mtWarning,[MBOK],0);
      Exit;
   end
   else
   FoobotSecretKey:=Memo1.Text;
   bDoneSecretKey:=TRUE;
end;

procedure Tconfigform.edt_usernameEditingDone(Sender: TObject);
begin
  If NOT ValidEmail(edt_username.Text) then
  begin
     MessageDlg(Application.Title,
     edt_username.Text + ' is not a valid email address. Try again',
     mtWarning,[MBOK],0);
     Exit;
  end
  else
  begin
     FoobotUsername:=edt_username.Text;
     bDoneUsername:=TRUE;
  end;
end;

procedure Tconfigform.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  If ( bDoneUsername=FALSE) OR (bDoneSecretKey=FALSE) then
  begin
   CanClose:=FALSE;
   If MessageDlg('You haven''t completed all the fields.  Are you sure you want to quit?',
   mtConfirmation,[MBYES,MBNO],0,MBNO) = mrYes then CanClose:=TRUE;
  end
  else
  begin
   bValid:=TRUE;
   CanClose:=TRUE;
  end;

end;

end.

