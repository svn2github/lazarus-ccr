unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, SpkToolbar,
  SpkGUITools, SpkMath, SpkGraphTools, spkt_Tab, spkt_Pane, spkt_Types,
  spkt_Tools, ImgList, ComCtrls, Menus, Grids, ExtCtrls, StdCtrls,
  spkt_BaseItem, spkt_Buttons, spkt_Checkboxes;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList: TImageList;
    LargeImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MnuOffice2007Blue: TMenuItem;
    MnuOffice2007Silver1: TMenuItem;
    MnuOffice2007Silver2: TMenuItem;
    MnuMetroLight: TMenuItem;
    MnuMetroDark: TMenuItem;
    Panel1: TPanel;
    SpkLargeButton1: TSpkLargeButton;
    SpkLargeButton2: TSpkLargeButton;
    SpkPane2: TSpkPane;
    StylePopupMenu: TPopupMenu;
    RecentFilesPopupMenu: TPopupMenu;
    SpkPane1: TSpkPane;
    SpkSmallButton1: TSpkSmallButton;
    SpkSmallButton2: TSpkSmallButton;
    SpkSmallButton3: TSpkSmallButton;
    SpkTab1: TSpkTab;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StyleChangeHandler(Sender: TObject);
  private
    { private declarations }
    SpkToolbar : TSpkToolbar;
    CbHorizGrid : TSpkCheckbox;
    CbVertGrid: TSpkCheckbox;
    CbRowSelect: TSpkCheckbox;
    procedure AboutHandler(Sender: TObject);
    procedure FileOpenHandler(Sender: TObject);
    procedure FileSaveHandler(Sender: TObject);
    procedure FileQuitHandler(Sender: TObject);
    procedure HorizontalGridLinesHandler(Sender: TObject);
    procedure VerticalGridLinesHandler(Sender: TObject);
    procedure RowSelectHandler(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf, spkt_Appearance;

{ TAboutForm }
type
  TAboutForm = class(TForm)
  private
    FIconLink: TLabel;
    procedure LinkClickHandler(Sender: TObject);
    procedure LinkMouseEnterHandler(Sender: TObject);
    procedure LinkMouseLeaveHandler(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

constructor TAboutForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited;
  Width := 300;
  Height := 180;
  Caption := 'About';
  Position := poMainFormCenter;
  with TLabel.Create(self) do begin
    Caption := 'SpkToolbar demo';
    Parent := self;
    Align := alTop;
    BorderSpacing.Top := 16;
    Font.Size := 16;
    Alignment := taCenter;
  end;
  with TLabel.Create(self) do begin
    Caption := 'Icons kindly provided by';
    Parent := self;
    Align := alTop;
    Alignment := taCenter;
    BorderSpacing.Top := 16;
    Left := 16;
    Top := 999;
  end;
  FIconLink := TLabel.Create(self);
  with FIconLink do begin
    Caption := 'http://www.fatcow.com/free-icons';
    Font.Color := clBlue;
    //Font.Style := [fsUnderline];
    Parent := self;
    Align := alTop;
    Alignment := taCenter;
    Top := 9999;
    OnClick := @LinkClickHandler;
    OnMouseEnter := @LinkMouseEnterHandler;
    OnMouseLeave := @LinkMouseLeaveHandler;
  end;
  with TButton.Create(self) do begin
    Caption := 'Close';
    Parent := Self;
    Left := (Self.Width - Width) div 2;
    Top := Self.Height - 16 - Height;
    ModalResult := mrOK;
    Default := true;
    Cancel := true;
  end;
end;

procedure TAboutForm.LinkClickHandler(Sender: TObject);
begin
  OpenURL((Sender as TLabel).Caption);
end;

procedure TAboutForm.LinkMouseEnterHandler(Sender: TObject);
begin
  FIconLink.Font.Style := [fsUnderline];
end;

procedure TAboutForm.LinkMouseLeaveHandler(Sender: TObject);
begin
  FIconLink.Font.Style := [];
end;


{ TForm1 }

procedure TForm1.AboutHandler(Sender: TObject);
var
  F: TForm;
begin
  F := TAboutForm.CreateNew(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TForm1.FileOpenHandler(Sender: TObject);
begin
  Statusbar1.SimpleText := '"File" / "Open" clicked';
end;

procedure TForm1.FileSaveHandler(Sender: TObject);
begin
  Statusbar1.SimpleText := '"File" / "Save" clicked';
end;

procedure TForm1.FileQuitHandler(Sender: TObject);
begin
  Close;
end;

procedure TForm1.HorizontalGridLinesHandler(Sender: TObject);
begin
  if CbHorizGrid.Checked then
    StringGrid1.Options := StringGrid1.Options + [goHorzLine]
  else
    StringGrid1.Options := StringGrid1.Options - [goHorzLine];
end;

procedure TForm1.VerticalGridLinesHandler(Sender: TObject);
begin
  if CbVertGrid.Checked then
    StringGrid1.Options := StringGrid1.Options + [goVertLine]
  else
    StringGrid1.Options := StringGrid1.Options - [goVertLine];
end;

procedure TForm1.RowSelectHandler(Sender: TObject);
begin
  if CbRowSelect.Checked then
    StringGrid1.Options := StringGrid1.Options + [goRowSelect]
  else
    StringGrid1.Options := StringGrid1.Options - [goRowSelect];
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SpkToolbar := TSpkToolbar.Create(self);

  with SpkToolbar do begin
    Parent := self;
    Appearance.Pane.CaptionFont.Style := [fsBold, fsItalic];
    Color := clSkyBlue;
    Images := ImageList;
    LargeImages := LargeImageList;
    ShowHint := true;

    with Tabs.Add do begin
      Caption := 'File';
      with Panes.Add do begin
        Caption := 'File commands';
        with Items.AddLargeButton do begin
          Caption := 'Open file';
          ButtonKind := bkButtonDropdown;
          DropdownMenu := RecentFilesPopupMenu;
          LargeImageIndex := 1;
          Hint := 'Open a file';
          OnClick := @FileOpenHandler;
        end;
        with Items.AddLargeButton do begin
          Caption := 'Save file';
          LargeImageIndex := 2;
          Hint := 'Save file';
          OnClick := @FileSaveHandler;
        end;
        with Items.AddLargeButton do begin
          Caption := 'Quit program';
          LargeImageIndex := 0;
          Hint := 'Close application';
          OnClick := @FileQuitHandler;
        end;
      end;
    end;

    with Tabs.Add do begin
      Caption := 'Edit';
      with Panes.Add do begin
        Caption := 'Edit commands';
        with Items.AddSmallButton do begin
          Caption := 'Cut';
          HideFrameWhenIdle := true;
          TableBehaviour := tbBeginsRow;
          ImageIndex := 3;
          Hint := 'Cut to clipboard';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Copy';
          HideFrameWhenIdle := true;
          TableBehaviour := tbBeginsRow;
          ImageIndex := 4;
          Hint := 'Copy to clipboard';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Paste';
          HideFrameWhenIdle := true;
          TableBehaviour := tbBeginsColumn;
          ImageIndex := 5;
          Hint := 'Paste from clipboard';
        end;
      end;
    end;

    with Tabs.Add do begin
      Caption := 'Format';
      with Panes.Add do begin
        Caption := 'Format Settings';
        with Items.AddSmallButton do begin
          Caption := 'Bold';
          ButtonKind := bkToggle;
          GroupBehaviour := gbBeginsGroup;
          TableBehaviour := tbBeginsRow;
          ImageIndex := 6;
          ShowCaption := false;
          AllowAllUp := true;
          Hint := 'Bold';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Italic';
          ButtonKind := bkToggle;
          TableBehaviour := tbContinuesRow;
          GroupBehaviour := gbContinuesGroup;
          ImageIndex := 7;
          ShowCaption := false;
          AllowAllUp := true;
          Hint := 'Italic';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Underline';
          ButtonKind := bkToggle;
          TableBehaviour := tbContinuesRow;
          GroupBehaviour := gbEndsGroup;
          ImageIndex := 8;
          ShowCaption := false;
          AllowAllUp := true;
          Hint := 'Underlined';
        end;

        with Items.AddSmallButton do begin
          Caption := 'Left-aligned';
          ButtonKind := bkToggle;
          GroupBehaviour := gbBeginsGroup;
          TableBehaviour := tbBeginsRow;
          ImageIndex := 11;
          ShowCaption := false;
          GroupIndex := 2;
          Checked := true;
          Hint := 'Left-aligned';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Centered';
          ButtonKind := bkToggle;
          TableBehaviour := tbContinuesRow;
          GroupBehaviour := gbContinuesGroup;
          ImageIndex := 12;
          ShowCaption := false;
          GroupIndex := 2;
          Checked := false;
          Hint := 'Centered';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Right-aligned';
          ButtonKind := bkToggle;
          TableBehaviour := tbContinuesRow;
          GroupBehaviour := gbContinuesGroup;
          ImageIndex := 13;
          ShowCaption := false;
          GroupIndex := 2;
          Hint := 'Right-aligned';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Block';
          ButtonKind := bkToggle;
          TableBehaviour := tbContinuesRow;
          GroupBehaviour := gbEndsGroup;
          ImageIndex := 14;
          ShowCaption := false;
          GroupIndex := 2;
          Hint := 'Block';
        end;

        with Items.AddSmallButton do begin
          Caption := 'Subscript';
          ButtonKind := bkToggle;
          TableBehaviour := tbBeginsColumn;
          GroupBehaviour := gbBeginsGroup;
          ImageIndex := 9;
          ShowCaption := false;
          AllowAllUp := true;
          GroupIndex := 1;
          Hint := 'Subscript';
        end;
        with Items.AddSmallButton do begin
          Caption := 'Superscript';
          ButtonKind := bkToggle;
          TableBehaviour := tbContinuesRow;
          GroupBehaviour := gbEndsGroup;
          ImageIndex := 10;
          ShowCaption := false;
          AllowAllUp := true;
          GroupIndex := 1;
          Hint := 'Superscript';
        end;

        With Items.AddSmallButton do begin
          Enabled := false;
          TableBehaviour := tbBeginsRow;
          HideFrameWhenIdle := true;
          Caption := '';
        end;
      end;
    end;

    with Tabs.Add do begin
      Caption := 'Options';
      with Panes.Add do begin
        Caption := 'Grid settings';
        CbHorizGrid := Items.AddCheckbox;
        with CbHorizGrid do begin
          Caption := 'Horizontal grid lines';
          TableBehaviour := tbBeginsRow;
          Checked := true;
          Hint := 'Show/hide horizontal grid lines';
          OnClick := @HorizontalGridLinesHandler;
        end;
        CbVertGrid := Items.AddCheckbox;
        with CbVertGrid do begin
          Caption := 'Vertical grid lines';
          Hint := 'Show/hide vertical grid lines';
          TableBehaviour := tbBeginsRow;
          Checked := true;
          OnClick := @VerticalGridLinesHandler;
        end;
        CbRowSelect := Items.AddCheckbox;
        with CbRowSelect do begin
          Caption := 'Row select';
          TableBehaviour := tbBeginsRow;
          Checked := false;
          Hint := 'Select entire row';
          OnClick := @RowSelectHandler;
        end;
      end;
      with Panes.Add do begin
        Caption := 'Themes';
        with Items.AddLargeButton do begin
          Caption := 'Change style';
          Hint := 'Change theme';
          ButtonKind := bkDropdown;
          DropdownMenu := StylePopupMenu;
          LargeImageIndex := 7;
        end;
      end;
      with Panes.Add do begin
        Caption := 'Save settings';
        with Items.AddSmallButton do begin
          Caption := 'Save now';
          Hint := 'Save settings now';
          ImageIndex := 2;
        end;
        with Items.AddCheckbox do begin
          Caption := 'Auto-save settings';
          Checked := true;
          Hint := 'Automatically save settings when program closes';
        end;
      end;
    end;

    with Tabs.Add do begin
      Caption := 'Help';
      with Panes.Add do begin
        Caption := 'Help commands';
        with Items.AddLargeButton do begin
          Caption := 'About...';
          LargeImageIndex := 6;
          Hint := 'About this program';
          OnClick := @AboutHandler;
        end;
      end;
    end;
  end;
end;

procedure TForm1.StyleChangeHandler(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to StylePopupMenu.Items.Count-1 do
    StylePopupMenu.Items[i].Checked := StylePopupMenu.Items[i] = TMenuItem(Sender);
  SpkToolbar.Style := TSpkStyle((Sender as TMenuItem).Tag);
  case SpkToolbar.Style of
    spkOffice2007Blue            : SpkToolbar.Color := clSkyBlue;
    spkOffice2007Silver          : SpkToolbar.Color := clWhite;
    spkOffice2007SilverTurquoise : SpkToolbar.Color := clWhite;
    spkMetroLight                : SpkToolbar.Color := clSilver;
    spkMetroDark                 : SpkToolbar.Color := $080808;
  end;
end;


end.

