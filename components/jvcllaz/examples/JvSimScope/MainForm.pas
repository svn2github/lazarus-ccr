{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MainForm.pas, released on 2007-02-06.

The Initial Developer of the Original Code is Olivier Sannier [obones att altern dott org]
Portions created by Olivier Sannier are Copyright (C) 2007 Olivier Sannier.
All Rights Reserved.

Contributor(s): None to date.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Demonstrates the usage of TJvSimScope

Known Issues:
-----------------------------------------------------------------------------}
unit MainForm;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvSimScope, StdCtrls;

type
  TfrmMain = class(TForm)
    jssRandom: TJvSimScope;
    lblRandomDetails1: TLabel;
    btnActivateDeactivateRandom: TButton;
    btnAdjustMax: TButton;
    Label1: TLabel;
    lblWelcome: TLabel;
    procedure jssRandomUpdate(Sender: TObject);
    procedure btnActivateDeactivateRandomClick(Sender: TObject);
    procedure btnAdjustMaxClick(Sender: TObject);
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnActivateDeactivateRandomClick(Sender: TObject);
begin
  jssRandom.Active := not jssRandom.Active;
  if jssRandom.Active then
    btnActivateDeactivateRandom.Caption := 'Deactivate'
  else
    btnActivateDeactivateRandom.Caption := 'Activate';
end;

procedure TfrmMain.btnAdjustMaxClick(Sender: TObject);
var
  I: Integer;
  LineMax: Integer;
begin
  // We check all values of line number 1 to see if there is one that is greater
  // than the current max value of the scope. If so, we change the Maximum value
  // to demonstrate how redrawing is done and how past values were kept to allow
  // redrawing with a new scale.
  LineMax := jssRandom.Minimum;
  for I := 0 to jssRandom.Lines[1].Values.Count - 1 do
  begin
    if jssRandom.Lines[1].Values[I] > LineMax then
      LineMax := jssRandom.Lines[1].Values[I];
  end;
  
  if LineMax > jssRandom.Maximum then
    jssRandom.Maximum := LineMax;
end;

procedure TfrmMain.jssRandomUpdate(Sender: TObject);
begin
  jssRandom.Lines[0].Position := Random(200) - 100;
  jssRandom.Lines[1].Position := Random(200);  // this one will eventually go out of scope
end;

end.
