{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImagesViewer.PAS, released on 2003-12-01.

The Initial Developer of the Original Code is: Peter Thrnqvist
All Rights Reserved.
Lazarus port: Michał Gawrycki

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvImagesViewer;

interface

{.$I jvcl.inc}
{$MODE OBJFPC}{$H+}

uses
  SysUtils, Classes, Controls, Graphics, StdCtrls, ComCtrls,
  JvCustomItemViewer, FPImage;

type

  { TJvPictureItem }

  TJvPictureItem = class(TJvViewerItem)
  private
    FFileName: String;
    FPicture: TPicture;
    FCaption: String;
    procedure SetFileName(const Value: String);
    procedure SetCaption(const Value: String);
    procedure SetPicture(const Value: TPicture);
    function GetPicture: TPicture;
    procedure CreatePicture;
  protected
    procedure DoPictureChange(Sender: TObject); virtual;
    procedure DoLoadProgress(Sender: TObject; Stage: TFPImgProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: AnsiString; var Continue : Boolean); virtual;
    procedure ReduceMemoryUsage; override;
  public
    destructor Destroy; override;
    procedure Refresh;
  public
    property FileName: String read FFileName write SetFileName;
    property Picture: TPicture read GetPicture write SetPicture;
    property Caption: String read FCaption write SetCaption;
  end;

  TJvImageViewerOptions = class(TJvCustomItemViewerOptions)
  private
    FImagePadding: Integer;
    FFrameColor: TColor;
    FHotFrameSize: Integer;
    FHotColor: TColor;
    FTransparent: Boolean;
    procedure SetImagePadding(const Value: Integer);
    procedure SetFrameColor(const Value: TColor);
    procedure SetHotColor(const Value: TColor);
    procedure SetHotFrameSize(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
  public
    constructor Create(AOwner: TJvCustomItemViewer); override;
  published
    property AutoCenter;
    property Alignment;
    property BrushPattern;
    property DragAutoScroll;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clGray;
    property Height;
    property HorzSpacing;
    property HotColor: TColor read FHotColor write SetHotColor default clHighlight;
    property HotFrameSize: Integer read FHotFrameSize write SetHotFrameSize default 2;
    property HotTrack;
    property ImagePadding: Integer read FImagePadding write SetImagePadding default 8;
    property Layout;
    property LazyRead;
    property MultiSelect;
    property ReduceMemoryUsage;
    property RightClickSelect;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property ScrollBar;
    property ShowCaptions default True;
    property Tracking;
    property VertSpacing;
    property Width;
  end;

  TJvImageLoadEvent = procedure(Sender: TObject; Item: TJvPictureItem) of object;
  TJvImageLoadErrorEvent = procedure(Sender: TObject; E: Exception;
    const FileName: String; var Handled: Boolean) of object;
  TJvImageViewerLoadProgress = procedure(Sender: TObject; Item: TJvPictureItem; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string) of object;

  TJvImagesViewer = class(TJvCustomItemViewer)
  private
    FFileMask: String;
    FDirectory: String;
    FOnLoadError: TJvImageLoadErrorEvent;
    FOnLoadProgress: TJvImageViewerLoadProgress;
    FOnLoadBegin: TNotifyEvent;
    FOnLoadEnd: TNotifyEvent;
    procedure SetDirectory(const Value: String);
    procedure SetFileMask(const Value: String);
    function GetItems(Index: Integer): TJvPictureItem;
    procedure ExpandFileMask(const Mask: String; Strings: TStrings);
    function ScaleRect(ARect, RefRect: TRect): TRect;
    function GetOptions: TJvImageViewerOptions;
    procedure SetOptions(const Value: TJvImageViewerOptions);
  protected
    function GetItemClass: TJvViewerItemClass; override;
    function GetOptionsClass: TJvItemViewerOptionsClass; override;
    function LoadErrorHandled(E: Exception; const FileName: String): Boolean;
    procedure DoLoadBegin; virtual;
    procedure DoLoadProgress(Item: TJvPictureItem; Stage: TProgressStage; PercentDone: Byte;
      RedrawNow: Boolean; const R: TRect; const Msg: String);
    procedure DoLoadEnd; virtual;
    procedure DrawItem(Index: Integer; State: TCustomDrawState; ACanvas: TCanvas;
      AItemRect, TextRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    function LoadImages: Boolean;virtual;
    procedure CustomSort(Compare: TListSortCompare); override;

    property Items[Index: Integer]: TJvPictureItem read GetItems;
    property Count;
  published
    property Directory: String read FDirectory write SetDirectory;
    property FileMask: String read FFileMask write SetFileMask;
    property Options: TJvImageViewerOptions read GetOptions write SetOptions;
    property SelectedIndex;
    property OnScroll;
    property OnLoadBegin: TNotifyEvent read FOnLoadBegin write FOnLoadBegin;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TJvImageLoadErrorEvent read FOnLoadError write FOnLoadError;
    property OnLoadProgress: TJvImageViewerLoadProgress read FOnLoadProgress write FOnLoadProgress;
    property OnDrawItem;
    property OnOptionsChanged;
    property OnItemChanging;
    property OnItemChanged;
    property OnItemHint;
    property OnInsertion;
    property OnDeletion;

    property Align;
    property Anchors;
    //    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    //    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

implementation

uses
  JvJCLUtils, LCLIntf, LCLType;

//=== { TJvImageViewerOptions } ==============================================

constructor TJvImageViewerOptions.Create(AOwner: TJvCustomItemViewer);
begin
  inherited Create(AOwner);
  FImagePadding := 20;
  FFrameColor := clGray;
  FHotColor := clHighlight;
  FHotFrameSize := 2;
  ShowCaptions := True;
end;

procedure TJvImageViewerOptions.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Change;
  end;
end;

procedure TJvImageViewerOptions.SetHotColor(const Value: TColor);
begin
  FHotColor := Value;
end;

procedure TJvImageViewerOptions.SetHotFrameSize(const Value: Integer);
begin
  FHotFrameSize := Value;
end;

procedure TJvImageViewerOptions.SetImagePadding(const Value: Integer);
begin
  if FImagePadding <> Value then
  begin
    FImagePadding := Value;
    Change;
  end;
end;

procedure TJvImageViewerOptions.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Change;
  end;
end;

//=== { TJvPictureItem } =====================================================

destructor TJvPictureItem.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TJvPictureItem.CreatePicture;
var
  S: String;
begin
  if FPicture = nil then
  begin
    FPicture := TPicture.Create;
    FPicture.OnChange := @DoPictureChange;
    FPicture.OnProgress := @DoLoadProgress;
    S := ExpandUNCFileName(FileName);
    if (S <> '') and FileExists(S) then
    try
      FPicture.LoadFromFile(S);
      if FPicture.Graphic <> nil then
        FPicture.Graphic.Transparent := TJvImagesViewer(Owner).Options.Transparent;
    except
      on E: Exception do
        if not TJvImagesViewer(Owner).LoadErrorHandled(E, FileName) then
          raise
        else
        begin
          Delete;
          FreeAndNil(FPicture);
        end;
    end;
  end;
end;

procedure TJvPictureItem.DoPictureChange(Sender: TObject);
begin
  Changed;
end;

procedure TJvPictureItem.DoLoadProgress(Sender: TObject;
  Stage: TFPImgProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: AnsiString; var Continue: Boolean);
begin
  if Owner is TJvImagesViewer then
    TJvImagesViewer(Owner).DoLoadProgress(Self, Stage, PercentDone, RedrawNow, R, Msg);
end;

function TJvPictureItem.GetPicture: TPicture;
begin
  CreatePicture;
  Result := FPicture;
end;

procedure TJvPictureItem.SetFileName(const Value: String);
begin
  if (AnsiCompareFileName(FFileName, Value) <> 0) and Changing then
  begin
    FFileName := Value;
    // don't load image until .Picture is used
    FreeAndNil(FPicture);
  end;
end;

procedure TJvPictureItem.SetPicture(const Value: TPicture);
begin
  if Changing then
  begin
    if Value <> nil then
      GetPicture.Assign(Value)
    else
    if Assigned(FPicture) then
    begin
      FreeAndNil(FPicture);
      Changed;
    end;
  end;
end;

procedure TJvPictureItem.SetCaption(const Value: String);
begin
  if (FCaption <> Value) and Changing then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TJvPictureItem.ReduceMemoryUsage;
begin
  inherited ReduceMemoryUsage;
  if FileName <> '' then // release image if we can recreate it from it's filename
    Picture := nil;
end;

procedure TJvPictureItem.Refresh;
begin
  FreeAndNil(FPicture);
end;

//=== { TJvImagesViewer } ====================================================

constructor TJvImagesViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  FDirectory := GetCurrentDir;
  FFileMask := Graphics.GraphicFileMask(TGraphic);
  Color := clWindow;
end;

function TJvImagesViewer.ScaleRect(ARect, RefRect: TRect): TRect;
var
  w, h, cw, ch: Integer;
  XYAspect: Double;
begin
  w := ARect.Right - ARect.Left;
  h := ARect.Bottom - ARect.Top;
  cw := RefRect.Right - RefRect.Left;
  ch := RefRect.Bottom - RefRect.Top;

  if (w > cw) or (h > ch) then
  begin
    if (w > 0) and (h > 0) then
    begin
      XYAspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / XYAspect);
        if h > ch then
        begin
          h := ch;
          w := Trunc(ch * XYAspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * XYAspect);
        if w > cw then
        begin
          w := cw;
          h := Trunc(cw / XYAspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;
end;

procedure TJvImagesViewer.DrawItem(Index: Integer; State: TCustomDrawState;
  ACanvas: TCanvas; AItemRect, TextRect: TRect);
var
  ImageRect: TRect;
  TotalPadding, BottomRightShift: Integer;
  AItem: TJvPictureItem;
  S: String;

  procedure ModifyRect(var R: TRect; ALeft, ATop, ARight, ABottom: Integer);
  begin
    Inc(R.Left, ALeft);
    Inc(R.Top, ATop);
    Inc(R.Right, ARight);
    Inc(R.Bottom, ABottom);
  end;

begin
  inherited DrawItem(Index, State, ACanvas, AItemRect, TextRect);
  //{$IFDEF MSWINDOWS}
  //if Win32Platform = VER_PLATFORM_WIN32_NT then
  //  BottomRightShift := 1
  //else
  //{$ENDIF MSWINDOWS}
    BottomRightShift := 0;
  AItem := Items[Index];
  ACanvas.Font := Font;
  ACanvas.Brush.Color := Color;
  ACanvas.Pen.Color := Font.Color;
  TotalPadding := Options.ImagePadding;
  if Options.ShowCaptions then
  begin
    Dec(AItemRect.Bottom, 2);
    Inc(TextRect.Top, 2);
    S := AItem.Caption;
    if S = '' then
      S := ExtractFileName(AItem.FileName);
  end;

  if cdsHot in State then
  begin
    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
    ACanvas.Font.Color := clHighlight;
    ACanvas.Pen.Color := Options.HotColor;
    ACanvas.Pen.Width := Options.HotFrameSize;
    ACanvas.Brush.Style := bsClear;
    ModifyRect(AItemRect,Options.HotFrameSize div 2,Options.HotFrameSize div 2,
      -Options.HotFrameSize div 2 + BottomRightShift,-Options.HotFrameSize div 2 + BottomRightShift);
    ACanvas.Rectangle(AItemRect);
    ModifyRect(AItemRect,-Options.HotFrameSize div 2,-Options.HotFrameSize div 2,
      Options.HotFrameSize div 2 - BottomRightShift,Options.HotFrameSize div 2 - BottomRightShift);
    ACanvas.Brush.Style := bsSolid;
    SetBkMode(ACanvas.Handle, {Windows.}TRANSPARENT);
    ACanvas.Pen.Width := 1;
  end;
  if cdsSelected in State then
  begin
    ACanvas.Pen.Color := clBtnFace;
    ACanvas.Brush.Color := clHighlight;
    if Options.BrushPattern.Active then
      ACanvas.Brush.Bitmap := Options.BrushPattern.GetBitmap
    else
      ACanvas.Brush.Color := Options.BrushPattern.OddColor;
    ACanvas.Rectangle(AItemRect);
    ACanvas.Brush.Bitmap := nil;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Color := Options.HotColor;
    ACanvas.Pen.Width := Options.HotFrameSize;
    ModifyRect(AItemRect,Options.HotFrameSize div 2, Options.HotFrameSize div 2,
      -Options.HotFrameSize div 2 + BottomRightShift, -Options.HotFrameSize div 2 + BottomRightShift);
    ACanvas.Rectangle(AItemRect);
    ModifyRect(AItemRect,-Options.HotFrameSize div 2, -Options.HotFrameSize div 2,
      Options.HotFrameSize div 2 - BottomRightShift, Options.HotFrameSize div 2 - BottomRightShift);
    ACanvas.Font.Color := clHighlightText;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := clHighlight;
    ACanvas.Pen.Width := 1;
  end
  else
  if (Options.FrameColor <> clNone) and not (cdsHot in State) then
  begin
    ACanvas.Brush.Color := Options.FrameColor;
    ACanvas.FrameRect(AItemRect);
    SetBkMode(ACanvas.Handle, {Windows.}TRANSPARENT);
  end;
  // make space around image
  InflateRect(AItemRect, -TotalPadding, -TotalPadding);
  if AItem.Picture <> nil then // access Picture to load image
  begin
    ImageRect := Rect(0, 0, AItem.Picture.Width, AItem.Picture.Height);
    ImageRect := CenterRect(ScaleRect(ImageRect, AItemRect), AItemRect);
    if (RectWidth(ImageRect) > 0) and (RectHeight(ImageRect) > 0) then
      {if AItem.Picture.Graphic is TIcon then
        //        and (RectWidth(ImageRect) < RectWidth(R)) and (RectHeight(ImageRect) < RectHeight(R))  then
        // TIcon doesn't scale it's content
        DrawIconEx(ACanvas.Handle, ImageRect.Left, ImageRect.Top, AItem.Picture.Icon.Handle,
          ImageRect.Right - ImageRect.Left, ImageRect.Bottom - ImageRect.Top, 0, 0, DI_NORMAL)
      else}
        ACanvas.StretchDraw(ImageRect, AItem.Picture.Graphic);
  end;

  if Options.ShowCaptions and (S <> '') then
  begin
    if Options.Layout = tlCenter then
      S := ' ' + S + ' ';
    ViewerDrawText(ACanvas, S, Length(S),
      TextRect, DT_END_ELLIPSIS or DT_EDITCONTROL, Options.Alignment, tlCenter, False);
  end;
end;

function TJvImagesViewer.GetItems(Index: Integer): TJvPictureItem;
begin
  Result := TJvPictureItem(inherited Items[Index]);
end;

function TJvImagesViewer.GetItemClass: TJvViewerItemClass;
begin
  Result := TJvPictureItem;
end;

function TJvImagesViewer.LoadImages: Boolean;
var
  I, J: Integer;
  F: TSearchRec;
  Files, FileMasks: TStringList;
  TmpDir: String;
begin
  BeginUpdate;
  try
    Count := 0;
    TmpDir := ExpandUNCFileName(Directory);
    FileMasks := TStringList.Create;
    try
      FileMasks.Sorted := True; // make sure no duplicates are added
      ExpandFileMask(FileMask, FileMasks);
      if TmpDir <> '' then
        TmpDir := IncludeTrailingPathDelimiter(TmpDir);
      DoLoadBegin;
      Files := TStringList.Create;
      try
        Files.Sorted := True;
        for I := 0 to FileMasks.Count - 1 do
        begin
          if SysUtils.FindFirst(TmpDir + FileMasks[I], faAnyFile, F) = 0 then
          try
            repeat
              if F.Attr and faDirectory = 0 then
                Files.Add(TmpDir + F.Name);
            until SysUtils.FindNext(F) <> 0;
            Count := Files.Count;
            J := 0;
            while J < Files.Count do
            begin
              Items[J].FileName := Files[J];
              Inc(J);
            end;
          finally
            SysUtils.FindClose(F);
          end;
        end;
      finally
        Files.Free;
      end;
      DoLoadEnd;
    finally
      FileMasks.Free;
    end;
    Result := Count > 0;
  finally
    EndUpdate;
  end;
end;

procedure TJvImagesViewer.SetDirectory(const Value: String);
begin
  if FDirectory <> Value then
  begin
    FDirectory := Value;
    LoadImages;
  end;
end;

procedure TJvImagesViewer.SetFileMask(const Value: String);
begin
  if FFileMask <> Value then
  begin
    FFileMask := Value;
    LoadImages;
  end;
end;

procedure TJvImagesViewer.ExpandFileMask(const Mask: String;
  Strings: TStrings);
var
  Start, Current: PChar;
  TmpChar: Char;
begin
  Current := PChar(string(Mask));
  Start := Current;
  while (Current <> nil) and (Current^ <> #0) do
  begin
    if CharInSet(Current^, [',', ';']) then
    begin
      TmpChar := Current^;
      Current^ := #0;
      if Start <> '' then
        Strings.Add(Start);
      Current^ := TmpChar;
      Start := Current + 1;
    end;
    Inc(Current);
  end;
  if Start <> '' then
    Strings.Add(Start);
end;

function TJvImagesViewer.LoadErrorHandled(E: Exception; const FileName: String): Boolean;
begin
  Result := False;
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, E, FileName, Result);
end;

procedure TJvImagesViewer.DoLoadBegin;
begin
  if Assigned(FOnLoadBegin) then
    FOnLoadBegin(Self);
end;

procedure TJvImagesViewer.DoLoadProgress(Item: TJvPictureItem;
  Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: String);
begin
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, Item, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TJvImagesViewer.DoLoadEnd;
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self);
end;

function TJvImagesViewer.GetOptionsClass: TJvItemViewerOptionsClass;
begin
  Result := TJvImageViewerOptions;
end;

function TJvImagesViewer.GetOptions: TJvImageViewerOptions;
begin
  Result := TJvImageViewerOptions(inherited Options);
end;

procedure TJvImagesViewer.SetOptions(const Value: TJvImageViewerOptions);
begin
  inherited Options := Value;
end;

function SortByFilename(Item1, Item2:Pointer):integer;
begin
  Result := AnsiCompareFileName(TJvPictureItem(Item1).Filename, TJvPictureItem(Item2).Filename);
end;

procedure TJvImagesViewer.CustomSort(Compare: TListSortCompare);
begin
  if Assigned(Compare) then
    inherited CustomSort(Compare)
  else
    inherited CustomSort(@SortByFilename);
  Invalidate;
end;

end.
