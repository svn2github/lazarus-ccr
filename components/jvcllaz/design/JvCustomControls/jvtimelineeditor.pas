{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeLineEdit.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Property editor(s) for the @link(TJvTimeLine) component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTimeLineEditor;

//{$I jvcl.inc}

interface

uses
  PropEdits, ComponentEditors;

type
  { a component editor that by default opens the editor for the Items property
    in TTimeline }
  TJvTimeLineEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    procedure EditProperty(const Prop: TPropertyEditor; var {%H-}Continue: Boolean); override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  SysUtils,
  JvTimeLine, JvDsgnConsts;

procedure TJvTimeLineEditor.Edit;
var
  timeline: TJvTimeLine;
begin
  timeline := TJvTimeLine(GetComponent); //
  TCollectionPropertyEditor.ShowCollectionEditor(timeline.Items, timeline, 'Items');
{
  tree := TVirtualTreeCast(GetComponent);
  TCollectionPropertyEditor.ShowCollectionEditor(Tree.Header.Columns, Tree, 'Columns');
}
end;

procedure TJvTimeLineEditor.EditProperty(const Prop: TPropertyEditor; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if SameText(PropName, 'Items') then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

procedure TJvTimeLineEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

function TJvTimeLineEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RsItemsEditorEllipsis
  else
    Result := inherited GetVerb(Index);
end;

function TJvTimeLineEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
