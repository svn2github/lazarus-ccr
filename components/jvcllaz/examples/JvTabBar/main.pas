unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Menus, ActnList, JvTabBar, JvTabBarXPPainter;

type

  { TForm1 }

  TForm1 = class(TForm)
    AcFileOpen: TAction;
    AcFileQuit: TAction;
    AcModernPainter: TAction;
    AcXPPainter: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    JvModernTabBarPainter1: TJvModernTabBarPainter;
    JvTabBar1: TJvTabBar;
    JvTabBarXPPainter1: TJvTabBarXPPainter;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    ToolBar: TToolBar;
    TbOpen: TToolButton;
    TbQuit: TToolButton;
    TbStyle: TToolButton;
    ToolButton2: TToolButton;
    procedure AcFileOpenExecute(Sender: TObject);
    procedure AcFileQuitExecute(Sender: TObject);
    procedure AcModernPainterExecute(Sender: TObject);
    procedure AcXPPainterExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvTabBar1TabClosed(Sender: TObject; Item: TJvTabBarItem);
    procedure JvTabBar1TabClosing(Sender: TObject; Item: TJvTabBarItem;
      var AllowClose: Boolean);
    procedure JvTabBar1TabSelected(Sender: TObject; Item: TJvTabBarItem);
  private
    FLoading: integer;
    procedure LoadFile(AFileName: String);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
  TTabInfo = class
    Filename: string;
  end;

{ TForm1 }

procedure TForm1.AcFileOpenExecute(Sender: TObject);
var
  fn: String;
begin
  if OpenDialog1.Execute then
    for fn in OpenDialog1.Files do LoadFile(fn);
end;

procedure TForm1.AcFileQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.AcModernPainterExecute(Sender: TObject);
begin
  JvTabBar1.Painter := JvModernTabBarPainter1;
end;

procedure TForm1.AcXPPainterExecute(Sender: TObject);
begin
  JvTabBar1.Painter := JvTabBarXPPainter1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenDialog1.InitialDir := '..\examples\JvTabBar';
  LoadFile('..\examples\JvTabBar\main.pas');
  LoadFile('..\examples\JvTabBar\main.lfm');
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
  info: TTabInfo;
begin
  for i := JvTabBar1.Tabs.Count-1 downto 0 do begin
    info := TTabInfo((JvTabBar1.Tabs[i]).Tag);
    FreeAndNil(info);
  end;
end;

procedure TForm1.JvTabBar1TabClosed(Sender: TObject; Item: TJvTabBarItem);
begin
  if JvTabBar1.Tabs.Count = 1 then begin
    JvTabBar1.Visible := false;
    Memo1.Clear;
  end;
end;

procedure TForm1.JvTabBar1TabClosing(Sender: TObject; Item: TJvTabBarItem;
  var AllowClose: Boolean);
var
  info: TTabInfo;
begin
  info := TTabInfo(Item.Tag);
  FreeAndNil(info);
  AllowClose := true;
end;

procedure TForm1.JvTabBar1TabSelected(Sender: TObject; Item: TJvTabBarItem);
var
  tab: TJvTabBarItem;
  info: TTabInfo;
  fn: String;
begin
  if FLoading <> 0 then
    exit;
  tab := JvTabBar1.SelectedTab;
  if tab = nil then exit;
  info := TTabInfo(tab.Tag);
  Memo1.Lines.LoadfromFile(info.FileName);
end;

procedure TForm1.Loadfile(AFileName: String);
var
  tab: TJvTabBarItem;
  info: TTabInfo;
begin
  if not FileExists(ExpandFileName(AFileName)) then begin
    ShowMessage('File "' + AFileName + '" does not exist.');
    exit;
  end;

  Memo1.Lines.LoadfromFile(AFileName);
  inc(FLoading);
  tab := TJvTabBarItem(JvTabBar1.Tabs.Add);
  tab.Caption := ExtractFileName(AFileName);
  info := TTabInfo.Create;
  info.FileName := AFileName;
  tab.Tag := PtrInt(info);
  tab.ImageIndex := 2;
  JvTabBar1.Visible := JvTabBar1.Tabs.Count > 0;
  tab.Selected := true;
  dec(FLoading);
end;

end.

