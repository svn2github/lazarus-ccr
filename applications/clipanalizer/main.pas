{ ClipAnalizer: A tool for analizing the clipboard

  Copyright (C) 2018 Jesus Reyes Aguilar <jesusrmx@gmail.com>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, LConvEncoding,
  Clipbrd, Forms, Controls, Graphics, Dialogs,
  LCLProc, StdCtrls, ExtCtrls, Menus, OwnClip;

type

  THexDumpOpts = set of (dopOffsetHex, dopOffsetDec);


  { TfrmClipboardAnalizer }

  TfrmClipboardAnalizer = class(TForm)
    btnReOpen: TButton;
    btnUpdate: TButton;
    btnSave: TButton;
    btnIsolate: TButton;
    btnUpdateIsolate: TButton;
    btnOpen: TButton;
    chkBinText: TCheckBox;
    oDlg: TOpenDialog;
    sDlg: TSaveDialog;
    txtFormat: TEdit;
    lblSize: TLabel;
    lblEncoding: TLabel;
    lstTypes: TListBox;
    memoDump: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    radAsText: TRadioButton;
    radHex: TRadioButton;
    radStream: TRadioButton;
    radEncoding: TRadioButton;
    Splitter1: TSplitter;
    procedure btnOpenClick(Sender: TObject);
    procedure btnUpdateIsolateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnIsolateClick(Sender: TObject);
    procedure btnReOpenClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure chkBinTextClick(Sender: TObject);
    procedure lstTypesSelectionChange(Sender: TObject; User: boolean);
  private
    fFilename: string;
    procedure DumpClipboard;
    procedure UpdateFormatList;
  public

  end;

var
  frmClipboardAnalizer: TfrmClipboardAnalizer;

implementation

{$R *.lfm}

// ref: https://stackoverflow.com/questions/6304896/hex-view-of-a-file
procedure HexDump(data: pointer; size: Integer; Lines: TStrings; opt:THexDumpOpts=[]);
const
  SEPHEX=' ';
  SEPASC=' ';
  NONASC='.';
var
  i : Integer;
  hexDat, ascDat : string;
  buff : PByte;
  L: TStringList;

  procedure OutputStr(s:string);
  begin
    if Lines<>nil then begin
      L.Add(s);
      //s := ConvertEncoding(s, EncodingAnsi, EncodingUTF8);
      //Lines.Add(s)
    end
    else
      WriteLn(s);
  end;

  function OffsetStr(offset: Integer): string;
  begin
    result := '';
    if [dopOffsetHex, dopOffsetDec]*Opt=[] then
      exit;
    if dopOffsetHex in Opt then
      Result:=Result + format('%.8x: ',[Offset]);
    if dopOffsetDec in Opt then
      Result:=Result + format('%.8d: ',[Offset]);
  end;

begin
  L := TStringList.Create;
  hexDat:=OffsetStr(0);
  ascDat:='';
  buff := data;
  for i:=0 to size-1 do begin
    hexDat := hexDat + IntToHex(buff[i], 2);
    if ((buff[i]>31) and (buff[i]<128)) then
      ascDat := ascDat + Char(buff[i])
    else
      ascDat := ascDat + NONASC;

    if (((i+1) mod 16)<>0) and (((i+1) mod 8)=0) then
      hexDat:=hexDat + SEPHEX;

    if ((i+1) mod 16)=0 then begin
      OutputStr(hexdat+SEPASC+ascdat);
      hexdat:=OffsetStr(i+1);
      ascdat:='';
    end;
  end;

  if (size mod 16)<>0 then
  begin
    if (size mod 16)<8 then
      hexDat := hexDat+StringOfChar(' ',(8-(size mod 8))*2)
               +SEPHEX+StringOfChar(' ',16)
    else
      hexDat := hexDat+StringOfChar(' ',(16-(size mod 16))*2);
    OutputStr(hexDat + SEPASC  + ascDat);
  end;
  if Lines<>nil then
    Lines.Assign(L);
  L.Free;
end;

// ref: https://www.codeproject.com/Reference/1091137/Windows-Clipboard-Formatss
function FormatIDToStr(formatID: TClipboardFormat):string;
begin
  case formatID of
    1: result := 'CF_TEXT'; // (1)	ANSI text	Text.
    2: result := 'CF_BITMAP'; // (2)	HBITMAP	Handle to a bitmap (GDI object).
    3: result := 'CF_METAFILEPICT'; // (3)	METAFILEPICT	Windows-Format Metafiles picture.
    4: result := 'CF_SYLK'; // (4)	ANSI text	Microsoft Symbolic Link [Wikipedia].
    5: result := 'CF_DIF'; // (5)	ASCII text	Software Arts Data Interchange Format [Wikipedia].
    6: result := 'CF_TIFF'; // (6)	TIFF [Wikipedia]	TIFF image.
    7: result := 'CF_OEMTEXT'; // (7)	8-Bit DOS text	Text.
    8: result := 'CF_DIB'; // (8)	BITMAPINFO	Structure followed by bitmap bits.
    9: result := 'CF_PALETTE'; // (9)	HPALETTE	Handle to a color palette (GDI object).
    10: result := 'CF_PENDATA'; // (10)	-	Windows 3.1 pen extension data.
    11: result := 'CF_RIFF'; // (11)	RIFF	Resource Interchange File Format (RIFF) audio.
    12: result := 'CF_WAVE'; // (12)	WAVE	WAVE audio.
    13: result := 'CF_UNICODETEXT'; // (13)	Unicode text	Text.
    14: result := 'CF_ENHMETAFILE'; // (14)	HENHMETAFILE	Enhanced-Format Metafiles handle.
    15: result := 'CF_HDROP'; // (15)	DROPFILES	List of file names.
    16: result := 'CF_LOCALE'; // (16)	DWORD (LCID)	LCID for CF_TEXT to CF_UNICODE conversion.
    17: result := 'CF_DIBV5'; // (17)	BITMAPV5HEADER	Structure followed by bitmap bits
    $0081: result := 'CF_DSPTEXT'; // (0x0081)	ANSI text	Text.
    $0082: result := 'CF_DSPBITMAP'; // (0x0082)	HBITMAP	Handle to a bitmap (GDI object)
    $0083: result := 'CF_DSPMETAFILEPICT'; // (0x0083)	METAFILEPICT	Windows-Format Metafiles picture.
    $0084: result := 'CF_DSPENHMETAFILE'; // (0x008E)	HENHMETAFILE	Enhanced-Format Metafiles handle.
    else begin
      result := '';
    end;
  end;
end;

function FormatIDIsText(formatID:TClipboardFormat): boolean;
begin
  case formatID of
    1,4,5,7,
    49291:
      result := true;
    else
      result := false;
  end;
end;

{ TfrmClipboardAnalizer }

procedure TfrmClipboardAnalizer.btnUpdateClick(Sender: TObject);
begin
  UpdateFormatList;
end;

procedure TfrmClipboardAnalizer.btnSaveClick(Sender: TObject);
var
  aIndex: Integer;
  formatID: TClipboardFormat;
  stream: TMemoryStream;
begin
  aIndex := lstTypes.ItemIndex;
  if aIndex<0 then
    exit;

  if not sDlg.Execute then
    exit;

  formatID := TClipboardFormat(lstTypes.Items.Objects[aIndex]);

  stream := TMemoryStream.Create;
  try
    ClipboardGetFormat(formatID, stream);
    stream.position := 0;

    stream.SaveToFile(sDlg.FileName);

  finally
    stream.free;
  end;
end;

procedure TfrmClipboardAnalizer.btnUpdateIsolateClick(Sender: TObject);
var
  aIndex: Integer;
  formatID: TClipboardFormat;
  stream: TMemoryStream;
begin
  aIndex := lstTypes.ItemIndex;
  if aIndex<0 then
    exit;

  formatID := TClipboardFormat(lstTypes.Items.Objects[aIndex]);

  stream := TMemoryStream.Create;
  try
    memoDump.Lines.SaveToStream(stream);
    stream.position := 0;

    Clipboard.Open;
    Clipboard.Clear;
    Clipboard.AddFormat(formatID, stream);
    Clipboard.Close;

    UpdateFormatList;
  finally
    stream.free;
  end;
end;

procedure TfrmClipboardAnalizer.btnOpenClick(Sender: TObject);
begin
  if oDlg.Execute then begin
    fFilename := oDlg.Filename;
    memoDump.Lines.LoadFromFile(oDlg.FileName);
  end;
end;

procedure TfrmClipboardAnalizer.btnIsolateClick(Sender: TObject);
var
  aIndex: Integer;
  formatID: TClipboardFormat;
  stream: TMemoryStream;
begin
  aIndex := lstTypes.ItemIndex;
  if aIndex<0 then
    exit;

  formatID := TClipboardFormat(lstTypes.Items.Objects[aIndex]);

  stream := TMemoryStream.Create;
  try
    ClipboardGetFormat(formatID, stream);
    stream.position := 0;

    Clipboard.Open;
    Clipboard.Clear;
    Clipboard.AddFormat(formatID, stream);
    Clipboard.Close;

    UpdateFormatList;
  finally
    stream.free;
  end;
end;

procedure TfrmClipboardAnalizer.btnReOpenClick(Sender: TObject);
begin
  if fFilename='' then
    btnOpenClick(Self)
  else
    memoDump.Lines.LoadFromFile(fFilename);
end;

procedure TfrmClipboardAnalizer.chkBinTextClick(Sender: TObject);
begin
  if lstTypes.ItemIndex>=0 then
    DumpClipboard;
end;

procedure TfrmClipboardAnalizer.lstTypesSelectionChange(Sender: TObject; User: boolean);
begin
  DumpClipboard;
end;

procedure TfrmClipboardAnalizer.DumpClipboard;
var
  formatID: TClipboardFormat;
  stream: TMemoryStream;
  isText: Boolean;
  s: RawByteString;
  encodingStr: string;
  aIndex: Integer;

  procedure ProcessBin;
  var
    i: Integer;
  begin
    if chkBinText.Checked then
      for i:=0 to memoDump.Lines.Count-1 do begin
        memoDump.Lines[i] := DbgStr(memoDump.Lines[i]);
      end;
  end;

begin
  aIndex := lstTypes.ItemIndex;
  if aIndex<0 then begin
    memoDump.Clear;
    exit;
  end;
  formatID := TClipboardFormat(lstTypes.Items.Objects[aIndex]);
  isText := FormatIDIsText(formatID);

  stream := TMemoryStream.Create;
  try
    memoDump.Lines.BeginUpdate;

    ClipboardGetFormat(formatID, stream);
    stream.position := 0;
    SetString(s, stream.Memory, stream.Size);
    encodingStr := GuessEncoding(s);
    if formatID=13{CF_UNICODETEXT} then
      encodingStr := EncodingUCS2LE;

    txtFormat.Text := lstTypes.Items[aIndex];
    lblEncoding.Caption := encodingStr;
    lblSize.Caption := format('%d bytes',[stream.size]);

    if radHex.Checked then begin
      HexDump(stream.Memory, stream.Size, memoDump.Lines, [dopOffsetHex]);
      exit;
    end;

    if radAsText.checked then
      memoDump.Text := Clipboard.AsText
    else
    if radStream.Checked then
      memoDump.Lines.LoadFromStream(stream{, True})
    else begin
      if encodingStr=EncodingUTF8 then
        memoDump.Text := s
      else
        memoDump.Text := ConvertEncoding(s, encodingStr, EncodingUTF8);
    end;

    ProcessBin;

  finally
    stream.Free;
    memoDump.Lines.EndUpdate;
  end;
end;

procedure TfrmClipboardAnalizer.UpdateFormatList;
var
  L: TStringList;
  formatID: TClipboardFormat;
  i: Integer;
begin
  L := TStringList.Create;

  for i := 0 to Clipboard.FormatCount - 1 do begin
    formatID := Clipboard.Formats[i];
    if formatID = 0 then
      continue;
    L.AddObject(
      format('%d:%s:%s', [
        FormatID,
        FormatIDToStr(FormatID),
        ClipboardFormatToMimeType(FormatID)]),
      TObject(PtrUInt(formatID)));
  end;
  lstTypes.Items.Assign(L);

  L.Free;
end;

end.

