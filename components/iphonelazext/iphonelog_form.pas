unit iphonelog_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { Tiphonelogform }

  Tiphonelogform = class(TForm)
    chkStayOnTop: TCheckBox;
    LogSheet1: TTabSheet;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    procedure chkStayOnTopChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    fLogMemo: TMemo;
  public
    { public declarations }
    function AddNewSheet: TTabSheet;
    procedure CaptionLog(sh: TTabSheet);
    property LogMemo: TMemo read fLogMemo;
  end;

var
  iphonelogform: Tiphonelogform = nil;

implementation

{$R *.lfm}

{ Tiphonelogform }

procedure Tiphonelogform.chkStayOnTopChange(Sender: TObject);
begin
  if chkStayOnTop.Checked
    then Self.FormStyle:=fsStayOnTop
    else Self.FormStyle:=fsNormal;
end;

procedure Tiphonelogform.FormCreate(Sender: TObject);
begin
  fLogMemo:=Memo1;
  CaptionLog(LogSheet1);
end;

procedure Tiphonelogform.FormShow(Sender: TObject);
begin
end;

function Tiphonelogform.AddNewSheet: TTabSheet;
var
  m : TMemo;
begin
  Result:=PageControl1.AddTabSheet;
  CaptionLog(Result);
  m:=TMemo.Create(Result);
  m.Parent:=Result;
  m.Align:=Memo1.Align;
  m.ScrollBars:=Memo1.ScrollBars;
  m.Font.Assign(Memo1.Font);
  PageControl1.ActivePage:=Result;
  fLogMemo:=m;
end;

procedure Tiphonelogform.CaptionLog(sh: TTabSheet);
begin
  sh.Caption:='Log '+FormatDateTime('hh-nn', now);
end;

end.

