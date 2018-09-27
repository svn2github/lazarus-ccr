{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNotebookTabBar.pas, released on 2018-05-01.

This unit was created as a workaround for the Lazarus issues with TJvCustomPage
and TJvCustomPageList and their descendants.
-----------------------------------------------------------------------------}

unit JvNotebookPageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  JvPageList;

type
  TJvNotebookPageList = class(TNotebook, IUnknown, IPageList)
  private
    FOnChanging: TJvPageChangingEvent;

  protected
    procedure AddPage(const ACaption: string);
    function CanChange(AIndex: Integer): Boolean;
    procedure DeletePage(AIndex: Integer);
    function GetPageCaption(AIndex: Integer): string;
    function GetPageCount: Integer;
    procedure MovePage(CurIndex, NewIndex: Integer);
    procedure PageCaptionChanged(AIndex: Integer; const NewCaption: string);
    procedure SetActivePageIndex(AIndex: Integer);

  public

  published
    property OnChanging: TJvPageChangingEvent read FOnChanging write FOnChanging;
  end;

implementation

function GetUniqueName(AOwner: TComponent; const AClassName: string): string;
var
  i: Integer;
begin
  i := 0;
  if AOwner = nil then
  begin
    repeat
      Inc(i);
      Result := AClassName + IntToStr(i);
    until FindGlobalComponent(Result) = nil;
  end
  else
    repeat
      Inc(i);
      Result := AClassName + IntToStr(i);
    until AOwner.FindComponent(Result) = nil;
end;


{-------------------------------------------------------------------------------
                           TJvNotebookPageList
-------------------------------------------------------------------------------}

procedure TJvNotebookPageList.AddPage(const ACaption: String);
var
  idx: Integer;
  lPage: TPage;
begin
  idx := Pages.Add(ACaption);
  lPage := Page[idx];
  lPage.Name := GetUniqueName(Self, 'TPage');
end;

function TJvNotebookPageList.CanChange(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < GetPageCount);
  if Result and Assigned(FOnChanging) then
    FOnChanging(Self, AIndex, Result);
end;

procedure TJvNotebookPageList.DeletePage(AIndex: Integer);
begin
  Pages.Delete(AIndex);
end;

function TJvNotebookPageList.GetPageCaption(AIndex: Integer): string;
begin
  Result := Pages[AIndex];
end;

function TJvNotebookPageList.GetPageCount: Integer;
begin
  Result := inherited PageCount;
end;

procedure TJvNotebookPageList.MovePage(CurIndex, NewIndex: Integer);
begin
  Pages.Exchange(CurIndex, NewIndex);
end;

procedure TJvNotebookPagelist.PageCaptionChanged(AIndex: Integer;
  const NewCaption: string);
begin
  Pages[AIndex] := NewCaption;
end;

procedure TJvNotebookPageList.SetActivePageIndex(AIndex: Integer);
begin
  PageIndex := AIndex;
end;


end.

