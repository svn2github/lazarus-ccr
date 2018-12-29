unit mdvMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ExtCtrls, ComCtrls, StdCtrls,
  fpeMetadata;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnChangeDate: TButton;
    CbDecodeMakerNotes: TCheckBox;
    EdChangeDate: TEdit;
    FilenameInfo: TLabel;
    Image: TImage;
    LblChangeDate: TLabel;
    Messages: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    DateTimePanel: TPanel;
    PreviewImage: TImage;
    ImageList: TImageList;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    PgMetadata: TTabSheet;
    PgImage: TTabSheet;
    TagListView: TListView;
    ShellPanel: TPanel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BtnChangeDateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewSelectionChanged(Sender: TObject);
    procedure TagListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure TagListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FImgInfo: TImgInfo;
    FImageLoaded: Boolean;
    procedure LoadFile(const AFileName: String);
    procedure LoadFromIni;
    procedure SaveToIni;
    procedure UpdateCaption;

  public
    procedure BeforeRun;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IniFiles, Math, StrUtils, DateUtils,
  fpeGlobal, fpeTags, fpeExifData, fpeIptcData;


function CalcIniName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;


{ TMainForm }

procedure TMainForm.BeforeRun;
begin
  LoadFromIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //ShellListView.Parent.DoubleBuffered := true;
end;

procedure TMainForm.BtnChangeDateClick(Sender: TObject);
var
  lTag: TTag;
  dt: TDateTime;
  fn: String;
begin
  if (FImgInfo = nil) or (FImgInfo.ExifData = nil) then
    exit;

  if not TryStrToDateTime(EdChangeDate.Text, dt) then begin
    MessageDlg('No valid date/time. Use your locale settings.', mtError, [mbOK], 0);
    exit;
  end;

  lTag := FImgInfo.ExifData.TagByName['DateTimeOriginal'];
  if lTag <> nil then
    TDateTimeTag(lTag).AsDateTime := dt;

  lTag := FImgInfo.ExifData.TagByName['DateTimeDigitized'];
  if lTag <> nil then
    TDateTimeTag(lTag).AsDateTime := dt;

  lTag := FImgInfo.ExifData.TagByName['DateTime'];
  if lTag <> nil then
    TDateTimeTag(lTag).AsDateTime := dt;

  fn := FImgInfo.FileName;
  fn := ChangeFileExt(fn, '') + '_modified' + ExtractFileExt(fn);
  FImgInfo.SaveToFile(fn);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  try
    SaveToIni;
  except
  end;
  FImgInfo.Free;
end;

procedure TMainForm.LoadFile(const AFileName: String);
var
  lTag: TTag;
  item: TListItem;
  i: Integer;
  ms: TMemoryStream;
begin
  FImageLoaded := false;
  Image.Picture.Clear;

  TagListView.Items.BeginUpdate;
  try
    TagListView.Clear;
    FImgInfo.Free;
    FImgInfo := TImgInfo.Create;
    try
      try
        if CbDecodeMakerNotes.Checked then
          FImgInfo.MetadataKinds := FImgInfo.MetadataKinds + [mdkExif] - [mdkExifNoMakerNotes]
        else
          FImgInfo.MetadataKinds := FImgInfo.MetadataKinds - [mdkExif] + [mdkExifNoMakerNotes];
        FImgInfo.LoadFromFile(AFileName);
        Messages.Hide;
      except
        on E:EFpExif do begin
          Messages.Lines.Text := E.Message;
          Messages.Show;
        end;
      end;
      if FImgInfo.HasExif then begin
        FImgInfo.ExifData.ExportOptions := FImgInfo.ExifData.ExportOptions + [eoTruncateBinary];
        for i := 0 to FImgInfo.ExifData.TagCount-1 do begin
          lTag := FImgInfo.ExifData.TagByIndex[i];
          item := TagListView.Items.Add;
          item.Data := lTag;
          item.Caption := 'EXIF.' + NiceGroupNames[lTag.Group];
          item.SubItems.Add(lTag.Description);
          item.SubItems.Add(lTag.AsString);
        end;

        lTag := FImgInfo.ExifData.TagByName['DateTimeOriginal'];
        if lTag <> nil then
          EdChangeDate.Text := DateTimeToStr(TDateTimeTag(lTag).AsDateTime)
        else
          EdChangeDate.Text := '';
        DateTimePanel.Show;
      end else
        DateTimePanel.Hide;

      if FImgInfo.HasIptc then begin
        for i := 0 to FImgInfo.IptcData.TagCount-1 do begin
          lTag := FImgInfo.IptcData.TagByIndex[i];
          item := TagListView.Items.Add;
          item.Data := lTag;
          item.Caption := 'IPTC';
          item.SubItems.Add(lTag.Description);
          item.SubItems.Add(lTag.AsString);
        end;
      end;
      if FImgInfo.HasThumbnail then begin
        ms := TMemoryStream.Create;
        try
          FImgInfo.ExifData.SaveThumbnailToStream(ms);
          ms.Position := 0;
          PreviewImage.Picture.LoadFromStream(ms);
        finally
          ms.Free;
        end;
      end else
        PreviewImage.Picture.Clear;

      if FImgInfo.HasWarnings then begin
        Messages.Lines.Text := FImgInfo.Warnings;
        Messages.Show;
      end;

      if PageControl1.ActivePage = PgImage then begin
        Image.Picture.LoadfromFile(AFileName);
        FImageLoaded := true;
      end;
    except
      on E:Exception do begin
        FreeAndNil(FImgInfo);
        Messages.Lines.Text := E.Message;
        Messages.Show;
      end;
    end;
    UpdateCaption;
  finally
    TagListView.Items.EndUpdate;
    TagListView.Sort;
  end;
end;

procedure TMainForm.LoadFromIni;
var
  ini: TCustomIniFile;
  i, W, H, L, T: Integer;
  rct: TRect;
  s: String;
begin
  ini := TIniFile.Create(CalcIniName);
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    rct := Screen.DesktopRect;
    if W > rct.Right - rct.Left then W := rct.Right - rct.Left;
    if H > rct.Bottom - rct.Top then H := rct.Bottom - rct.Top;
    if L < rct.Left then L := rct.Left;
    if T < rct.Top then T := rct.Top;
    if L+W > rct.Right then L := rct.Right - W;
    if T+H > rct.Bottom then T := rct.Bottom - H;
    SetBounds(L, T, W, H);

    s := ini.ReadString('MainForm', 'Path', '');
    if s <> '' then ShellTreeView.Path := s;

    w := ini.ReadInteger('MainForm', 'LeftPanelWidth', 0);
    if w <> 0 then ShellPanel.Width := w;

    h := ini.ReadInteger('MainForm', 'TreeHeight', 0);
    if h <> 0 then ShellTreeView.Height := h;

    for i:=0 to TagListView.Columns.Count-1 do begin
      w := ini.ReadInteger('TagList', 'ColWidth'+IntToStr(i), 0);
      if w <> 0 then
        TagListView.Columns[i].Width := w;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  if FImgInfo = nil then
    exit;

  if not FImageLoaded then begin
    Image.Picture.LoadfromFile(FImgInfo.FileName);
    FImageLoaded := true;
  end;
end;

procedure TMainForm.SaveToIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := TIniFile.Create(CalcIniName);
  try
    if WindowState = wsNormal then begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;
    ini.WriteString('MainForm', 'Path', ShellTreeView.Path);
    ini.WriteInteger('MainForm', 'LeftPanelWidth', ShellPanel.Width);
    ini.WriteInteger('MainForm', 'TreeHeight', ShellTreeView.Height);
    for i:=0 to TagListView.Columns.Count-1 do
      ini.WriteInteger('TagList', 'ColWidth'+IntToStr(i), TagListView.Columns[i].Width);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.ShellListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  dir, fn: String;
begin
  if Selected then
  begin
    dir := ShellTreeView.GetPathFromNode(ShellTreeView.Selected);
    fn := Item.Caption;
    LoadFile(dir + fn);
  end;
end;

procedure TMainForm.ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node = nil then
    exit;
  if Node.Level = 0 then
    Node.ImageIndex := 0
  else
  if Node.Expanded then
    Node.ImageIndex := 2
  else
    Node.ImageIndex := 1;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TMainForm.ShellTreeViewSelectionChanged(Sender: TObject);
begin
  TagListView.Items.Clear;
  PreviewImage.Picture.Assign(nil);
  ShellTreeViewGetImageIndex(nil, ShellTreeView.Selected);
  FreeAndNil(FImgInfo);
  UpdateCaption;
end;

procedure TMainForm.TagListViewCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  tag1, tag2: TTag;
begin
  tag1 := TTag(Item1.Data);
  tag2 := TTag(Item2.Data);
  Compare := CompareValue(ord(tag1.Group), ord(tag2.Group));
  if Compare = 0 then
    Compare := CompareText(Item1.SubItems[0], Item2.SubItems[0]);
end;

procedure TMainForm.TagListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
const
  { TTagType:
  ttUInt8 = 1, ttString, ttUInt16, ttUInt32, ttURational,
  ttSInt8, ttBinary, ttSInt16, ttSInt32, ttSRational,
  ttSingle, ttDouble,
  ttIFD   // rarely used, in Olympus maker notes
  }
  TAGTYPE_NAMES: array[TTagType] of string = (
    'BYTE',  'ASCII', 'UINT16', 'UINT32', 'URATIONAL',
    'SBYTE', 'BINARY', 'SINT16', 'SINT32', 'SRATIONAL',
    'SINGLE', 'DOUBLE',
    'IFD'
  );
var
  lTag: TTag;
  s: String;
  tagID: TTagIDRec;
begin
  if Selected then begin
    lTag := TTag(Item.Data);
    if lTag <> nil then begin
      tagID := TTagIDRec(lTag.TagID);
      Statusbar1.Panels[0].Text := Format('ID %d [$%.4x]', [tagID.Tag, tagID.Tag]);
      Statusbar1.Panels[1].Text := Format('Parent %d [$%.4x]', [tagID.Parent, tagID.Parent]);
      Statusbar1.Panels[2].Text := 'Name: ' + lTag.Name;
      Statusbar1.Panels[3].Text := 'Type: ' + TAGTYPE_NAMES[lTag.TagType];
      Statusbar1.Panels[4].Text := 'Elements: ' + IntToStr(lTag.Count);
      exit;
    end;
  end;
  Statusbar1.Panels[0].Text := '';
  Statusbar1.Panels[1].Text := '';
  Statusbar1.Panels[2].Text := '';
  Statusbar1.Panels[3].Text := '';
  Statusbar1.Panels[4].Text := '';
end;

procedure TMainForm.UpdateCaption;
var
  fn: String;
begin
  if FImgInfo <> nil then
    FileNameInfo.Caption := Format(
      'File: %s' + LineEnding +
      'Size: %d kB' + LineEnding +
      'Date: %s', [
      FImgInfo.Filename, FImgInfo.FileSize div 1024, DateTimeToStr(FImgInfo.FileDate)])
  else
    FilenameInfo.caption := '< no file >';
end;

end.

