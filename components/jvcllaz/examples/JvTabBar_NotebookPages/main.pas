unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  ActnList, StdActns, Menus, SynEdit, SynHighlighterPas, SynHighlighterXML,
  JvTabBar, JvNotebookPageList, JvTabBarXPPainter;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    JvModernTabBarPainter1: TJvModernTabBarPainter;
    JvNotebookPageList1: TJvNotebookPageList;
    JvTabBar1: TJvTabBar;
    JvTabBarXPPainter1: TJvTabBarXPPainter;
    lprPage: TPage;
    icoPage: TPage;
    lpiPage: TPage;
    PasSynEdit: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    XMLSynEdit: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure AcModernPainterExecute(Sender: TObject);
    procedure AcXPStylePainterExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvTabBar1TabCloseQuery(Sender: TObject; Item: TJvTabBarItem;
      var CanClose: Boolean);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  dir: String;
begin
  dir := ExpandFileName(Application.Location + '../examples/JvTabBar_NotebookPages/');
  PasSynEdit.Lines.LoadfromFile(dir + 'main.pas');
  XMLSynEdit.Lines.LoadFromFile(dir + 'JvTabBarDemo_NotebookPages.lpi');
  Image1.Picture.LoadFromFile(dir + 'JvTabBarDemo_NotebookPages.ico');
end;

procedure TForm1.JvTabBar1TabCloseQuery(Sender: TObject; Item: TJvTabBarItem;
  var CanClose: Boolean);
begin
  CanClose := MessageDlg('Do you really want to close this tab?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes;
end;

procedure TForm1.AcModernPainterExecute(Sender: TObject);
begin
  JvTabBar1.Painter := JvModernTabBarPainter1;
end;

procedure TForm1.AcXPStylePainterExecute(Sender: TObject);
begin
  JvTabBar1.Painter := JvTabBarXPPainter1;
end;

end.

