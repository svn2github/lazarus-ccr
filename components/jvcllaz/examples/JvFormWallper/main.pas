unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  ExtDlgs, Spin, StdCtrls, JvFormWallpaper;

type

  { TForm1 }

  TForm1 = class(TForm)
    CbOffsetMode: TComboBox;
    ImageList1: TImageList;
    JvFormWallpaper1: TJvFormWallpaper;
    Label1: TLabel;
    Label2: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    EdOffset: TSpinEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure CbOffsetModeChange(Sender: TObject);
    procedure EdOffsetChange(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    FImageFileName: String;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
var
  pic: TPicture;
begin
  with OpenPictureDialog1 do begin
    InitialDir := ExtractFileDir(FImageFileName);
    FileName := '';
    if Execute then begin
      FImageFileName := FileName;
      pic := TPicture.Create;
      try
        pic.LoadfromFile(FImageFileName);
        JvFormWallpaper1.Image.Assign(pic);
      finally
        pic.Free;
      end;
    end;
  end;
end;

procedure TForm1.EdOffsetChange(Sender: TObject);
begin
  JvFormWallPaper1.Offset := EdOffset.Value;
end;

procedure TForm1.CbOffsetModeChange(Sender: TObject);
begin
  JvFormWallPaper1.OffsetMode := TJvOffsetMode(CbOffsetMode.ItemIndex);
end;

end.

