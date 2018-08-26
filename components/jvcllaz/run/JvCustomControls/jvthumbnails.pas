{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThumbNail.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer att Excite dott com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Thumbimage, ThumbNail components
  Thumbimage is a TImage descentant wich passes the control of the mouse events
  to the ThumbNail and have the ability to change an images look by changing
  the rgb values with the changergb,changergbcurve procedures.
  You can have precise control over the images look.
  The changergb procedure just adds the values you pass to its rgb variables to
  the actual values of the image.
  The Changergbcurves procedure just replaces the value of the rgb values
  accordingly with the values that passed in the the arrays.
  e.g.
  the r array in the position 15 has a value of 35 this meens that wherever in
  the Picture there is a pixels which has a red value equall to 15 it will be ]
  replaced with the value 35.

  ThumbNail is what the name says a component to simply shrink an image
  proportionally to fit in a portion of the screen with some extra mouse handling
  to Create a Button like effect. Just give it a FileName and it will do the work
  for you.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThumbnails;

{$MODE objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF}
  LCLIntf, LCLType, LMessages, Types,
  Classes, Controls, ExtCtrls, SysUtils, Graphics, Forms,
  JvThumbImage, JvBaseThumbnail, Dialogs;

const
  TH_IMAGESIZECHANGED = WM_USER + 1;

type
  // (rom) elements renamed
  TTitlePos = (tpUp, tpDown, tpNone);

  TTitleNotify = procedure(Sender: TObject; FileName: string;
    var ThumbnailTitle: string) of object;

  TJvThumbnail = class(TJvBaseThumbnail)
  private
    FTitle: string;
    FTitlePanel: TJvThumbTitle;
    FTitleColor: TColor;
    FTitleFont: TFont;
    FStreamFileKind: TGRFKind;
    FDFileCreated: TDateTime;
    FDFileChanged: TDateTime;
    FDFileAccessed: TDateTime;
    FShowTitle: Boolean;
    FDFileSize: Longint;
    FStream: TStream;
    FImageWidth: Longint;
    FImageHeight: Longint;
    FThumbHeight: Word;
    FThumbWidth: Word;
    FShadowObj: TShape;
    FUpdated: Boolean;
    FImageReady: Boolean;
    FTitlePlacement: TTitlePos;
    FPhotoName: TJvFileName;
    FPhoto: TJvThumbImage;
    FOnGetTitle: TTitleNotify;
    FMousePressed: Boolean;
    FDestroying: Boolean;
    FAsButton: Boolean;
    FMinimizeMemory: Boolean;
    FAutoLoad: Boolean; // if True then load the image either from a thumb file or Create it from the FileName
    FShadowColor: TColor;
    FShowShadow: Boolean;
    FHShadowOffset: Word;
    FVShadowOffset: Word;
    FMargin: Integer;
    (************** NOT CONVERTED ***
    procedure PhotoOnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean;
      const R: TRect; const Msg: string);
    *******************************)
    procedure GetFileInfo(AName: string);
    function GetFileName: string;
    function GetTitleBevelInner: TPanelBevel;
    function GetTitleBevelOuter: TPanelBevel;
    function GetTitleBorderStyle: TBorderStyle;
    function IsTitleFontStored: Boolean;
    procedure RefreshFont(Sender: TObject);
    procedure SetDummyCard({%H-}AInt: Longint);
    procedure SetDummyStr({%H-}AStr: string);
    procedure SetFileName(const AFile: string);
    procedure SetMargin(AValue: Integer);
    procedure SetMinimizeMemory(Min: Boolean);
    //procedure SetShadowColor(aColor: TColor);
    procedure SetShowShadow(AShow: Boolean);
    procedure SetStream(const AStream: TStream);
    procedure SetShowTitle(const AState: Boolean);
    procedure SetTitle(const Value: string);
    procedure SetTitleBevelInner(const Value: TPanelBevel);
    procedure SetTitleBevelOuter(const Value: TPanelBevel);
    procedure SetTitleBorderStyle(const Value: TBorderStyle);
    procedure SetTitleColor(const Value: TColor);
    procedure SetTitleFont(const Value: TFont);
    procedure SetTitlePlacement(const AState: TTitlePos);

  protected
    procedure BoundsChanged; override;
    procedure CalculateImageSize; virtual;
    procedure CreateHandle; override;
    procedure THSizeChanged(var {%H-}Msg: TLMessage); message TH_IMAGESIZECHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function LoadFile(AFile: string): string;
    procedure UpdateThumbHeight;
    procedure UpdateThumbWidth;
    procedure UpdateTitlePanelHeight;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetTitlePanel(ATitle: string; AFont: TFont; AColor: TColor);
    procedure Refresh;
    property ImageHeight: Longint read FImageHeight default 0;
    property ImageReady: Boolean read FImageReady;
    property ImageWidth: Longint read FImageWidth default 0;
    property Stream: TStream read FStream write SetStream;
    property Photo: TJvThumbImage read FPhoto write FPhoto;
  published
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default true;
    property AsButton: Boolean read FAsButton write FAsButton default false;
    property FileName: string read GetFileName write SetFileName;
    property Margin: Integer read FMargin write SetMargin default 8;
    property MinimizeMemory: Boolean read FMinimizeMemory write SetMinimizeMemory default true;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow default false;
    property ShowTitle: Boolean read FShowTitle write SetShowTitle default true;
    property StreamFileType: TGRFKind read FStreamFileKind write FStreamFileKind default grBMP;
    property Title: string read FTitle write SetTitle;
    property TitleBevelInner: TPanelBevel read GetTitleBevelInner write SetTitleBevelInner default bvNone;
    property TitleBevelOuter: TPanelBevel read GetTitleBevelOuter write SetTitleBevelOuter default bvLowered;
    property TitleBorderStyle: TBorderStyle read GetTitleBorderStyle write SetTitleBorderStyle default bsNone;
    property TitleColor: TColor read FTitleColor write SetTitleColor default clDefault;
    property TitleFont: TFont read FTitleFont write SetTitleFont stored IsTitleFontStored;
    property TitlePlacement: TTitlePos read FTitlePlacement write SetTitlePlacement default tpUp;
    property OnGetTitle: TTitleNotify read FOnGetTitle write FOnGetTitle;
    { Do not store dummies }
    property FileSize: Longint read FDFileSize stored false; // write SetDummyCard stored False;
    property FileAccessed: TDateTime read FDFileAccessed stored false; // write SetDummyStr stored False;
    property FileCreated: TDateTime read FDFileCreated stored false; // write SetDummyStr stored False;
    property FileChanged: TDateTime read FDFileChanged stored false; // write SetDummyStr stored False;
  end;


implementation

uses
  FileUtil, DateUtils,
  JvThumbViews; //, JvResources;

constructor TJvThumbnail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowTitle := True;
  FMargin := 8;
  FHShadowOffset := 3;
  FVShadowOffset := 3;
  FShowShadow := False;
  FShadowColor := clSilver;
  FTitleColor := clDefault; //clBtnFace;
  FTitlePlacement := tpUp;
  FTitle := '';
  FUpdated := False;

  FPhotoName := TJvFileName.Create;

  Photo := TJvThumbImage.Create(Self);
  Photo.AutoSize := False;
  Photo.Align := alNone;
  Photo.Stretch := True;
  (************** NOT CONVERTED)
  Photo.OnProgress := PhotoOnProgress;
  **************)

  FShadowObj := TShape.Create(Self);
  FShadowObj.Visible := FShowShadow;
  FShadowObj.Brush.Color := FShadowColor;
  FShadowObj.Parent := Self;
  FShadowObj.Pen.Style := psClear;
  FShadowObj.Width := Photo.Width;
  FShadowObj.Height := Photo.Height;
  FShadowObj.Left := Photo.Left + FHShadowOffset;
  FShadowObj.Top := Photo.Top + FVShadowOffset;

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'default';
  FTitleFont.Size := 0;
  FTitleFont.OnChange := @RefreshFont;

  FTitlePanel := TJvThumbTitle.Create(Self);
  FTitlePanel.Align := alTop;
  FTitlePanel.Height := 15;
  FTitlePanel.Alignment := taCenter;
  FTitlePanel.Color := FTitleColor;
  FTitlePanel.BevelOuter := bvLowered;
  FTitlePanel.ParentColor := True;
//  FTitlePanel.Color := Self.Color;
  FTitlePanel.Visible := (FTitlePlacement <> tpNone) and FShowTitle;

  InsertControl(Photo);
  InsertControl(FTitlePanel);
  Align := alNone;
  if AOwner is TJvThumbView then
  begin
    Width := TJvThumbView(Owner).MaxWidth;
    Height := TJvThumbView(Owner).MaxHeight;
  end else
  begin
    Width := 120;
    Height := 120;
  end;
  FMinimizeMemory := True;
  AsButton := False;
  Left := 10;
  Top := 10;
  Visible := True;
  BevelOuter := bvRaised;
  StreamFileType := grBMP;
  FAutoLoad := True;
end;

destructor TJvThumbnail.Destroy;
begin
  FDestroying := True;
  (************* NOT CONVERTED ***
  Photo.OnProgress := nil;
  **********)
  FPhotoName.Free;
  FTitleFont.OnChange := nil;
  FTitleFont.Free;
  inherited Destroy;
end;

procedure TJvThumbnail.BoundsChanged;
begin
  CalculateImageSize;
  inherited BoundsChanged;
end;

procedure TJvThumbnail.CalculateImageSize;
var
  Percent: Byte;
  TempX, TempY: Single;
begin
  if (Photo = nil) or (Photo.Picture = nil) then
    exit;
  UpdateThumbHeight;
  UpdateThumbWidth;
  if (Photo.Picture.Width > FThumbWidth) or (Photo.Picture.Height > FThumbHeight) then
  begin
    TempX := (FThumbWidth / Photo.Picture.Width) * 100;
    TempY := (FThumbHeight / Photo.Picture.Height) * 100;
  end
  else
  begin
    TempX := 100;
    TempY := 100;
  end;
  if TempX <= TempY then
    Percent := Trunc(TempX)
  else
    Percent := Trunc(TempY);
  Photo.Width := Trunc((Photo.Picture.Width / 100) * Percent);
  Photo.Height := Trunc((Photo.Picture.Height / 100) * Percent);
  Photo.Left := Trunc(Width / 2 - Photo.Width / 2);
  Photo.Top := (Height div 2) - (Photo.Height div 2);
  case FTitlePlacement of
    tpUp:
      Photo.Top := Photo.Top + (FTitlePanel.Height div 2);
    tpDown:
      Photo.Top := Photo.Top - (FTitlePanel.Height div 2);
  end;
  FShadowObj.SetBounds(Photo.Left + FHShadowOffset, Photo.Top + FVShadowOffset,
    Photo.Width, Photo.Height);
end;

procedure TJvThumbnail.CreateHandle;
begin
  inherited;
  if not (csDesigning in ComponentState) and (FTitleColor = clDefault) then
    FTitleColor := Color;
  UpdateTitlePanelHeight;
end;

procedure TJvThumbnail.GetFileInfo(AName: String);
var
  {$IFDEF WINDOWS}
  info: TWin32FindDataW;
  dft: DWORD = 0;
  lft: TFileTime;
  H: THandle;
  ws: WideString;
  {$ENDIF}
  {$IFDEF UNIX}
  info: stat;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  ws := UTF8Decode(AName);
  H := Windows.FindFirstFileW(PWideChar(ws), info{%H-});
  if H <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(H);
    //fdFileAccessed
    FileTimeToLocalFileTime(info.ftLastAccessTime, lft{%H-});
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    try
      FDFileAccessed := FileDateToDateTime(dft);
    except
      FDFileAccessed := 0;
    end;
    //fdFilechanged
    FileTimeToLocalFileTime(info.ftLastwriteTime, lft);
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    try
      FDFileChanged := FileDateToDateTime(dft);
    except
      FDFileChanged := 0;
    end;
    //fdFileCreated
    FileTimeToLocalFileTime(info.ftCreationTime, lft);
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    try
      FDFileCreated := FileDateToDateTime(dft);
    except
      FDFileCreated := 0;
    end;
    FDFileSize := info.nFileSizeHigh * MAXDWORD + info.nFileSizeLow;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  if fpstat(AName, info) = 0 then begin
    FDFileAccessed := UnixToDateTime(info.st_atime);
    FDFileChanged := UnixToDateTime(info.st_mtime);
    FDFileCreated := UnixToDateTime(info.st_ctime);
    FDFileSize := info.Size;
  end;
  {$ENDIF}
end;

function TJvThumbnail.GetFileName: string;
begin
  Result := FPhotoName.FileName;
end;

function TJvThumbnail.GetTitleBevelInner: TPanelBevel;
begin
  Result := FTitlePanel.BevelInner;
end;

function TJvThumbnail.GetTitleBevelOuter: TPanelBevel;
begin
  Result := FTitlePanel.BevelOuter;
end;

function TJvThumbnail.GetTitleBorderStyle: TBorderStyle;
begin
  Result := FTitlePanel.BorderStyle;
end;

function TJvThumbnail.IsTitleFontStored: Boolean;
begin
  Result := not FTitleFont.IsDefault;
end;

function TJvThumbnail.LoadFile(AFile: string): string;
var
  FName: string;
begin
  try
    FName := AFile;
    Photo.LoadFromFile(AFile);
    FImageWidth := Photo.Picture.Width;
    FImageHeight := Photo.Picture.Height;
    FUpdated := False;
    CalculateImageSize;
    Photo.Visible := True;
  except
    // (rom) ShowMessage removed
    FName := '';
  end;
  if MinimizeMemory and (FPhotoName.FileName <> '') then
  begin
    if Owner is TJvThumbView then
      Photo.ScaleDown(TJvThumbView(Owner).MaxWidth, TJvThumbView(Owner).MaxHeight)
    else
      Photo.ScaleDown(FThumbWidth, FThumbHeight);
  end;
  Result := FName;
end;

procedure TJvThumbnail.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if AsButton then
    if Button = mbLeft then
    begin
      FMousePressed := True;
      BevelOuter := bvLowered;
      FTitlePanel.BevelOuter := bvRaised;
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvThumbnail.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if AsButton then
    if FMousePressed then
    begin
      if (X < 0) or (X > Width) or (Y < 0) or (Y > Height) then
      begin
        BevelOuter := bvRaised;
        FTitlePanel.BevelOuter := bvLowered
      end
      else
      begin
        BevelOuter := bvLowered;
        FTitlePanel.BevelOuter := bvRaised;
      end;
    end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvThumbnail.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if AsButton then
    if Button = mbLeft then
    begin
      FMousePressed := False;
      BevelOuter := bvRaised;
      FTitlePanel.BevelOuter := bvLowered;
    end;
  inherited MouseUp(Button, Shift, X, Y);
end;

(********** NOT CONVERTED ***
procedure TJvThumbnail.PhotoOnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  FImageReady := (Stage = psEnding);
end;
***************************)

procedure TJvThumbnail.Refresh;
begin
  CalculateImageSize;
  inherited Refresh;
end;

procedure TJvThumbnail.RefreshFont(Sender: TObject);
begin
  FTitlePanel.Font.Assign(FTitleFont);
  UpdateTitlePanelHeight;
end;

// dummy property functions to allow the object inspector to
// show the properties and their values
procedure TJvThumbnail.SetDummyStr(AStr: string);
begin
end;

procedure TJvThumbnail.SetDummyCard(AInt: Longint);
begin
end;

procedure TJvThumbnail.SetFileName(const AFile: string);
var
  FName: string;
//  Pos: Longint;
//  tmp: TJvThumbImage;
//  D1, D2: TdateTime;
begin
  if AFile <> '' then
  begin
    GetFileInfo(AFile);
    if FAutoLoad then
      FName := LoadFile(AFile);
  end
  else
    FName := ''; {}
  if FName = AFile then
    if (Title = ExtractFileName(FPhotoName.FileName)) or (Title = '') then
      Title := ExtractFileName(FName);
  FPhotoName.FileName := FName;
end;

procedure TJvThumbnail.SetMargin(AValue: Integer);
begin
  if AValue <> FMargin then begin
    FMargin := AValue;
    CalculateImageSize;
//    UpdateThumbWidth;
//    UpdateThumbHeight;
    Invalidate;
  end;
end;

procedure TJvThumbnail.SetMinimizeMemory(Min: Boolean);
begin
  if Assigned(Photo.Picture.Graphic) then
  begin
    if FMinimizeMemory <> Min then
    begin
      if Min then
      begin
        if Owner is TJvThumbView then
          Photo.ScaleDown(TJvThumbView(Owner).MaxWidth, TJvThumbView(Owner).MaxHeight)
        else
          Photo.ScaleDown(Width, Height);
      end
      else
      if FMinimizeMemory then
        Photo.Picture.LoadFromFile(FileName);
      FMinimizeMemory := Min;
    end;
  end
  else
    FMinimizeMemory := Min;
end;

{procedure TJvThumbnail.SetShadowColor(aColor: TColor);
begin
  FShadowObj.Brush.Color := aColor;
  FShadowColor := aColor;
end;}

procedure TJvThumbnail.SetShowShadow(AShow: Boolean);
begin
  FShadowObj.Visible := AShow;
  FShowShadow := AShow;
end;

procedure TJvThumbnail.SetShowTitle(const AState: Boolean);
begin
  if AState <> FShowTitle then
  begin
    FShowTitle := AState;
    FTitlePanel.Visible := AState;
  end
end;

procedure TJvThumbnail.SetStream(const AStream: TStream);
var
  Bmp: Graphics.TBitmap;
  Size: TPoint;
  Img2: TJPEGImage;
begin
  case StreamFileType of
    grBMP:
      Photo.Picture.Bitmap.LoadFromStream(AStream);
    (********* NOT CONVERTED ***
    grEMF, grWMF:
      Photo.Picture.Metafile.LoadFromStream(AStream);
      *************************)
    grJPG:
      begin
        Img2 := TJPEGImage.Create;
        Img2.LoadFromStream(AStream);
        Photo.Picture.Assign(Img2);
        FreeAndNil(Img2);
      end;
  end;

  if FMinimizeMemory then
  begin
    Bmp := Graphics.TBitmap.Create;
    if Parent is TJvThumbView then
      Size := ProportionalSize(Point(Photo.Picture.Width, Photo.Picture.Height),
        Point(TJvThumbView(Parent).MaxWidth, TJvThumbView(Parent).MaxHeight))
    else
      Size := ProportionalSize(Point(Photo.Picture.Width, Photo.Picture.Height),
        Point(Width, Height));
    Bmp.Width := Size.X;
    Bmp.Height := Size.Y;
    Bmp.handletype := bmDIB;
    Bmp.pixelformat := pf24bit;
    Bmp.Canvas.StretchDraw(rect(0, 0, Bmp.Width, Bmp.Height),
      Photo.Picture.Graphic);
    //Photo.Picture.Graphic.Free; // (rom) not needed
    //Photo.Picture.Graphic := nil;
    Photo.Picture.Assign(Bmp);
    Bmp.Free;
  end;
end;

procedure TJvThumbnail.THSizeChanged(var Msg: TLMessage);
begin
  CalculateImageSize;
end;

procedure TJvThumbnail.SetTitle(const Value: string);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    FTitlePanel.Caption := Value;
  end;
end;

procedure TJvThumbnail.SetTitleBevelInner(const Value: TPanelBevel);
begin
  FTitlePanel.BevelInner := Value;
end;

procedure TJvThumbnail.SetTitleBevelOuter(const Value: TPanelBevel);
begin
  FTitlePanel.BevelOuter := Value;
end;

procedure TJvThumbnail.SetTitleBorderStyle(const Value: TBorderStyle);
begin
  FTitlePanel.BorderStyle := Value;
end;

procedure TJvThumbnail.SetTitleColor(const Value: TColor);
begin
  if Value <> FTitleColor then
  begin
    FTitleColor := Value;
    FTitlePanel.Color := Value;
  end;
end;

procedure TJvThumbnail.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TJvThumbnail.SetTitlePanel(ATitle: string; AFont: TFont;
  AColor: TColor);
begin
  SetTitleFont(AFont);
  SetTitleColor(AColor);
  SetTitle(ATitle);
  FUpdated := True;
end;

procedure TJvThumbnail.SetTitlePlacement(const AState: TTitlePos);
begin
  if AState <> FTitlePlacement then
    case AState of
      tpUp:
        FTitlePanel.Align := alTop;
      tpDown:
        FTitlePanel.Align := alBottom;
      tpNone:
        FTitlePanel.Visible := False;
    end;
  if FTitlePlacement <> tpNone then
    FTitlePanel.Visible := True;
  FTitlePlacement := AState;
  CalculateImageSize;
end;

procedure TJvThumbnail.UpdateThumbHeight;
begin
  if Assigned(FTitlePanel) and FTitlePanel.Visible then
    FThumbHeight := ClientHeight - FTitlePanel.Height - FMargin
  else
    FThumbHeight := ClientHeight - FMargin;
end;

procedure TJvThumbnail.UpdateThumbWidth;
begin
  FThumbWidth := ClientWidth - 2 * FMargin;
end;

procedure TJvThumbnail.UpdateTitlePanelHeight;
var
  fd: TFontData;
begin
  fd := GetFontData(FTitleFont.Handle);
  Canvas.Font.Name := fd.Name;
  Canvas.Font.Style := fd.Style;
  Canvas.Font.Height := fd.Height;
  FTitlePanel.Height := Canvas.TextHeight('Tg') + 4;
  CalculateImageSize;
end;

procedure TJvThumbnail.WMPaint(var Msg: TLMPaint);
var
  ThumbnailTitle: string;
begin
  if not FUpdated then
  begin
    ThumbnailTitle := Title;
    if Assigned(FOnGetTitle) then
    begin
      FOnGetTitle(Self, FileName, ThumbnailTitle);
      SetTitle(ThumbnailTitle);
    end
    else
    begin
      if ThumbnailTitle = '' then
        SetTitle(ExtractFileName(FileName))
      else
        SetTitle(ThumbnailTitle);
    end;
    FUpdated := True;
  end;
  inherited;
end;

end.

