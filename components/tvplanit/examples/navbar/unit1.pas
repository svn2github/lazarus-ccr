unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, VpNavBar;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    BtnAddFolder: TButton;
    BtnAddItem: TButton;
    IconsLbl: TLabel;
    IconsLink: TLabel;
    RadioGroup2: TRadioGroup;
    Label1: TLabel;
    Images: TImageList;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    VpNavBar1: TVpNavBar;
    procedure BtnAddFolderClick(Sender: TObject);
    procedure BtnAddItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IconsLinkClick(Sender: TObject);
    procedure IconsLinkMouseEnter(Sender: TObject);
    procedure IconsLinkMouseLeave(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure VpNavBar1FolderChanged(Sender: TObject; Index: Integer);
    procedure VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; Index: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf;

{ TForm1 }

procedure TForm1.BtnAddFolderClick(Sender: TObject);
var
  s: String;
begin
  s := InputBox('Add folder', 'Folder name:', '');
  if s <> '' then
    VpNavBar1.AddFolder(s);
end;

procedure TForm1.BtnAddItemClick(Sender: TObject);
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
  s: String;
  idx: Integer;
begin
  if VpNavBar1.ActiveFolder = -1 then
    exit;
  s := InputBox('Add item', 'Item name:', '');
  if s <> '' then begin
    folder := VpNavBar1.Folders[VpNavBar1.ActiveFolder];
    idx := folder.ItemCount;
    VpNavBar1.AddItem(s, VpNavBar1.ActiveFolder, idx);
    item := folder.Items[idx];
    item.IconIndex := Random(VpNavBar1.Images.Count);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RandSeed := 1;
  IconsLink.Left := IconsLbl.Left + IconsLbl.Width;
  RadioGroup1.ItemIndex := ord(VpNavBar1.DrawingStyle);
end;

procedure TForm1.IconsLinkClick(Sender: TObject);
begin
  OpenDocument(IconsLink.Caption);
end;

procedure TForm1.IconsLinkMouseEnter(Sender: TObject);
begin
  IconsLink.Font.style := IconsLink.Font.Style + [fsUnderline];
end;

procedure TForm1.IconsLinkMouseLeave(Sender: TObject);
begin
  IconsLink.Font.style := IconsLink.Font.Style - [fsUnderline];
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  VpNavBar1.DrawingStyle := TVpFolderDrawingStyle(Radiogroup1.ItemIndex);
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
var
  folder: TVpNavFolder;
begin
  folder := VpNavBar1.Folders[VpNavBar1.ActiveFolder];
  folder.IconSize := TVpIconSize(RadioGroup1.ItemIndex);
end;

procedure TForm1.VpNavBar1FolderChanged(Sender: TObject; Index: Integer);
var
  folder: TVpNavFolder;
begin
  RadioGroup2.OnClick := nil;
  folder := VpNavBar1.Folders[Index];
  RadioGroup2.ItemIndex := ord(folder.IconSize);
  RadioGroup2.OnClick := @RadioGroup2Click;
end;

procedure TForm1.VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; Index: Integer);
var
  folder: TVpNavFolder;
  item: TVpNavBtnItem;
begin
  folder := VpNavBar1.Folders[VpNavBar1.ActiveFolder];
  item := folder.Items[Index];
  Label1.Caption := Format('Item "%s" clicked', [item.Caption]);
end;

end.

