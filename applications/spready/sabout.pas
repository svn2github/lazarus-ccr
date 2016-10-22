unit sAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    BtnClose: TButton;
    IconImage: TImage;
    Image1: TImage;
    LblBulletFPSpreadsheet1: TLabel;
    lblBulletFreePascal1: TLabel;
    LblBulletLazarus1: TLabel;
    LblCompilerAndLibs: TLabel;
    LblIcons: TLabel;
    lblTango: TLabel;
    LblFreePascal: TLabel;
    LblFugueIcons: TLabel;
    LblLazarus: TLabel;
    LblFPSpreadsheet: TLabel;
    lblBulletFreePascal: TLabel;
    LblBulletLazarus: TLabel;
    LblBulletFPSpreadsheet: TLabel;
    LblSilkIcons: TLabel;
    LblVersion: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, Types, resource, versiontypes, versionresource;

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
end;

procedure TAboutForm.LabelClick(Sender: TObject);
var
  url: String;
begin
  url := TLabel(Sender).Hint;
  OpenURL(url);
end;

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
var
  lbl: TLabel;
begin
  lbl := TLabel(Sender);
  lbl.Font.Style := [fsUnderline];
end;

procedure TAboutForm.LabelMouseLeave(Sender: TObject);
var
  lbl: TLabel;
begin
  lbl := TLabel(Sender);
  lbl.Font.Style := [];
end;


end.

