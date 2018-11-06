unit JvStrHolderEditor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls,
  ComponentEditors;

type
  TJvStrHolderEditor = class(TDefaultComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(AIndex: integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation

uses
  Forms, JvStringHolder, JvStringsForm;

procedure TJvStrHolderEditor.Edit;
var
  Temp: string;
  Comp: TPersistent;
begin
  with TJvStrEditDlg.Create(Application) do
  try
    Comp := Self.GetComponent;
    (*
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else
      Caption := GetName;
    Temp := GetStrValue;
    Memo.Lines.Text := Temp;
    *)
    Memo.Lines.Assign((Comp as TJvStrHolder).Strings);
    UpdateStatus(nil);
    if ShowModal = mrOk then
    begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      (Comp as TJvStrHolder).Strings.Text := Temp;
//      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

procedure TJvStrHolderEditor.ExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then Edit;
end;

function TJvStrHolderEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Strings Editor ----';
    else Result := '';
  end;
end;

function TJvStrHolderEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

