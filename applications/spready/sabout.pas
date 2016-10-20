unit sAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, IpHtml, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    BtnClose: TButton;
    IconImage: TImage;
    Image1: TImage;
    LblVersion: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure HTMLViewerHotClick(Sender: TObject);
  private
    { private declarations }
    FHTMLViewer: TIpHtmlPanel;
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, Types, resource, versiontypes, versionresource;

const
  LE = LineEnding;

  HTMLStr =
    '<!DOCTYPE html ' +
      'PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" '+
      '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"> '+ LE +
    '<html xmlns="http://www.w3.org/1999/xhtml">'+LE+
    '<head>' + LE +
    '  <meta http-equiv="content-type" content="text/html; charset=UTF-8">' + LE +
    '  <style type="text/css">' + LE +
    '    body {background-color:ffffff;}' + LE +
    '    h3{color:003366;}' + LE +
    '    li{font-size:9pt}' + LE +
    '  </style>' + LE +
    '<body>' + LE +
    '  <h3>Compiler and libaries:</h3>' + LE +
    '  <ul>'+ LE +
    '    <li><a href="www.freepascal.org">Free Pascal</a></li>' + LE +
    '    <li><a href="www.lazarus.freepascal.org">Lazarus</a></li>' + LE +
    '    <li><a href="http://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/fpspreadsheet/">fpspreadsheet</a></li>' + LE +
    '  </ul>' + LE +
    '  <h3>Icons:</h3>' + LE +
    '  <ul>' + LE +
    '    <li><a href="p.yusukekamiyamane.com">Fugue Icons</a></li>' + LE +
    '    <li><a href="www.famfamfam.com/lab/icons/silk/">famfamfam silk icons</a></li>' + LE +
    '    <li><a href="http://tango.freedesktop.org/Tango_Icon_Library">Tango icon library</a></li>' + LE +
//    '    <li><a href="https://github.com/pasnox/oxygen-icons-png">Oxygen cons</a></li>' + LE +
    '  </ul>' + LE +
    '</body>' + LE +
    '</html>';


function ResourceVersionInfo: String;
var
  Stream: TResourceStream;
  vr: TVersionResource;
  fi: TVersionFixedInfo;
begin
  Result := '';
  try
    { This raises an exception if version info has not been incorporated into the
      binary (Lazarus Project -> Project Options -> Version Info -> Version numbering). }
    Stream:= TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      vr := TVersionResource.Create;
      try
        vr.SetCustomRawDataStream(Stream);
        fi := vr.FixedInfo;
        Result := Format('%d.%d', [
          fi.FileVersion[0], fi.FileVersion[1]
        ]);
        vr.SetCustomRawDataStream(nil)
      finally
        vr.Free
      end;
    finally
      Stream.Free
    end;
  except
  end;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  sz: TSize;
begin
  sz.cx := 64; //128;
  sz.cy := 64; //128;

  IconImage.Picture.Icon := Application.Icon;
  IconImage.Picture.Icon.Current := Application.Icon.GetBestIndexForSize(sz); //4;
  LblVersion.Caption := 'Version ' + ResourceVersionInfo;

  FHTMLViewer := TIpHtmlPanel.Create(self);
  FHTMLViewer.Parent := self;
  FHTMLViewer.Align := alClient;
  FHTMLViewer.DefaultFontSize := 9;
  FHTMLViewer.OnHotClick := @HTMLViewerHotClick;
  FHTMLViewer.SetHtmlFromStr(HTMLStr);
end;

procedure TAboutForm.HTMLViewerHotClick(Sender: TObject);
begin
  OpenURL((Sender as TIpHtmlPanel).HotURL);
end;


end.

