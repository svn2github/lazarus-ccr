{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvHTMLParserMainFormU;

{$mode objfpc}{$H+}

interface

uses
  //Windows, Messages,
  Forms, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Dialogs,
  JvHtmlParser;

type
  TJvHTMLParserMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TreeView1: TTreeView;
    StatusBar1: TStatusBar;
    TabSheet4: TTabSheet;
    JvSplitter1: TSplitter;
    DisplayMemo1: TMemo;
    Panel1: TPanel;
    btnProcessTable: TButton;
    JvHtmlParser1: TJvHtmlParser;
    DisplayMemo2: TMemo;
    DisplayMemo3: TMemo;
    DisplayMemo4: TMemo;
    Panel2: TPanel;
    btnProcessHTML2Text: TButton;
    Panel3: TPanel;
    btnProcessURL: TButton;
    Panel4: TPanel;
    btnProcessTags: TButton;
    btnOpen: TButton;
    OpenDialog1: TOpenDialog;
    procedure btnProcessTableClick(Sender: TObject);
    procedure TableKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure TableKeyFoundEx(Sender: TObject; Key, Results,
      OriginalLine: String; TagInfo: TTagInfo; Attributes: TStrings);
    procedure HTML2TextKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure URLDetectKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure TagsKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure btnProcessHTML2TextClick(Sender: TObject);
    procedure btnProcessURLClick(Sender: TObject);
    procedure btnProcessTagsClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CurNode: TTreeNode;
    FText: string;
    FStartTime: TTime;
  public
    function ReplaceSpecials(Str: string): string;
    procedure ShowStatus(t: TTime);
    procedure DoKeyFoundEx(Sender: TObject; Key, Results, OriginalLine: string; TagInfo:TTagInfo; Attributes:TStrings);
  end;

var
  JvHTMLParserMainForm: TJvHTMLParserMainForm;

implementation

uses
  StrUtils;

  {
uses
  JclStrings;
   }
{$R *.lfm}

procedure TJvHTMLParserMainForm.btnProcessTableClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('TD', '<TD>', '</TD>');
    AddCondition('TH', '<TH>', '</TH>');
    AddCondition('TR', '<TR>', '</TR>');
//    OnKeyFound := TableKeyFound;
    OnKeyFoundEx := @TableKeyFoundEx;
  end;
  TreeView1.Items.BeginUpdate;
  DisplayMemo1.Lines.BeginUpdate;
  try
    DisplayMemo1.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    TreeView1.Items.EndUpdate;
    DisplayMemo1.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.btnProcessHTML2TextClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('Text', '>', '<');
    OnKeyFound := @HTML2TextKeyFound;
  end;
  DisplayMemo2.Lines.BeginUpdate;
  try
    DisplayMemo2.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
    DisplayMemo2.Text := ReplaceSpecials(FText);
  finally
    FText := '';
    DisplayMemo2.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.btnProcessURLClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('URL', 'href=http://', '>');
    AddCondition('URL', 'href="http://', '">');
    OnKeyFound := @URLDetectKeyFound;
  end;
  DisplayMemo3.Lines.BeginUpdate;
  try
    DisplayMemo3.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    DisplayMemo3.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.btnProcessTagsClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('URL', '<', '>');
    OnKeyFound := nil;
    OnKeyFoundEx := @DoKeyFoundEx;
  end;
  DisplayMemo4.Lines.BeginUpdate;
  try
    DisplayMemo4.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    DisplayMemo4.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.TableKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  DisplayMemo1.Lines.Add(Key + #13#10 + Results);
  if UpperCase(Key) = 'TR' then
    CurNode := TreeView1.Items.AddChild(nil, 'TR')
  else
    TreeView1.Items.AddChild(CurNode, Results);
end;

procedure TJvHTMLParserMainForm.TableKeyFoundEx(Sender: TObject;
  Key, Results, OriginalLine: String; TagInfo: TTagInfo;
  Attributes: TStrings);
var
  i : integer;
begin
  Self.Tag := Self.Tag + 1;
  DisplayMemo1.Lines.Add(Key + #13#10 + Results);
  for i:=0 to Attributes.Count-1 do
    DisplayMemo1.Lines.Add('Attributes=' + Attributes[i]);
  if UpperCase(Key) = 'TR' then
    CurNode := TreeView1.Items.AddChild(nil, 'TR')
  else
    TreeView1.Items.AddChild(CurNode, Results);
end;

procedure TJvHTMLParserMainForm.HTML2TextKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  //this is only for sample!
  Self.Tag := Self.Tag + 1;
  if (FText <> '') and (FText[Length(FText)] <> ' ') then
    FText := FText + ' ';
  FText := FText + Results;
end;

procedure TJvHTMLParserMainForm.URLDetectKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  DisplayMemo3.Lines.Add('http://' + Results);
end;

procedure TJvHTMLParserMainForm.TagsKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  DisplayMemo4.Lines.Add('<' + Results + '>');
end;

procedure TJvHTMLParserMainForm.ShowStatus(t: TTime);
var
  h, m, s, ms: Word;
begin
  DecodeTime(t, h, m, s, ms);
  StatusBar1.SimpleText :=
    Format('%d tags are processed for period: %0.2d:%0.2d:%0.2d.%0.3d',
     [Self.Tag, h, m, s, ms]);
end;

function TJvHTMLParserMainForm.ReplaceSpecials(Str: string): string;
begin
  Result := Str;
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&copy;', #169, [rfReplaceAll, rfIgnoreCase]);
  // add more here...
end;

procedure TJvHTMLParserMainForm.btnOpenClick(Sender: TObject);
var
  I: Integer;
begin
  if OpenDialog1.Execute then
  begin
    JvHtmlParser1.FileName := OpenDialog1.FileName;
    for I := 0 to ComponentCount - 1 do
      if (Components[I] <> btnOpen) and (Components[I] is TButton) then
        TButton(Components[I]).OnClick(nil);
  end;
end;

procedure TJvHTMLParserMainForm.FormCreate(Sender: TObject);
begin
  JvHtmlParser1.OnKeyFoundEx := @DoKeyFoundEx;
end;

procedure TJvHTMLParserMainForm.DoKeyFoundEx(Sender: TObject; Key, Results,
  OriginalLine: string; TagInfo: TTagInfo; Attributes: TStrings);
begin
  Self.Tag := Self.Tag + 1;
  DisplayMemo4.Lines.Add('<' + Results + '>');
  if Attributes.Count > 0 then
  begin
    DisplayMemo4.Lines.Add('Attributes:');
    DisplayMemo4.Lines.AddStrings(Attributes);
  end;
end;

end.
