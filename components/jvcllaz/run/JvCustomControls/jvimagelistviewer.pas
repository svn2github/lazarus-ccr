{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageListViewer.PAS, released on 2003-12-01.

The Initial Developer of the Original Code is: Peter Th�rnqvist
All Rights Reserved.
Lazarus port: Micha� Gawrycki

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvImageListViewer;

interface

{.$I jvcl.inc}
{$MODE OBJFPC}{$H+}

uses
  SysUtils, Classes, Controls, Graphics, StdCtrls, ComCtrls, ImgList,
  JvCustomItemViewer;

type
  TJvImageListViewerOptions = class(TJvCustomItemViewerOptions)
  private
    FDrawingStyle: TDrawingStyle;
    FSelectedStyle: TDrawingStyle;
    FFillCaption: Boolean;
    FFrameSize: Word;
    procedure SetDrawingStyle(const Value: TDrawingStyle);
    procedure SetSelectedStyle(const Value: TDrawingStyle);
    procedure SetFillCaption(const Value: Boolean);
    procedure SetFrameSize(const Value: Word);
  public
    constructor Create(AOwner: TJvCustomItemViewer); override;
  published
    property AutoCenter;
    property BrushPattern;
    property DragAutoScroll;
    property DrawingStyle: TDrawingStyle read FDrawingStyle write SetDrawingStyle default dsTransparent;
    property FillCaption: Boolean read FFillCaption write SetFillCaption default True;
    property SelectedStyle: TDrawingStyle read FSelectedStyle write SetSelectedStyle default dsSelected;
    property FrameSize: Word read FFrameSize write SetFrameSize default 1;
    property Height;
    property Layout;
    property RightClickSelect;
    property ScrollBar;
    property ShowCaptions;
    property Tracking;
    property Width;
  end;

  TJvImageListViewerCaptionEvent = procedure(Sender: TObject;
    ImageIndex: Integer; var ACaption: WideString) of object;

  TJvImageListViewer = class(TJvCustomItemViewer)
  private
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FOnGetCaption: TJvImageListViewerCaptionEvent;
    procedure SetImages(const Value: TCustomImageList);
    function GetOptions: TJvImageListViewerOptions;
    procedure SetOptions(const Value: TJvImageListViewerOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoImageChange(Sender: TObject);
    procedure DrawItem(Index: Integer; State: TCustomDrawState; ACanvas: TCanvas;
      AItemRect, TextRect: TRect); override;
    function GetOptionsClass: TJvItemViewerOptionsClass; override;
    function GetCaption(ImageIndex: Integer): WideString; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TJvImageListViewerOptions read GetOptions write SetOptions;
    property SelectedIndex;
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
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCaption: TJvImageListViewerCaptionEvent read FOnGetCaption write FOnGetCaption;
    property OnDrawItem;
    property OnOptionsChanged;
    property OnItemChanging;
    property OnItemChanged;
    property OnItemHint;
    property OnGetSiteInfo;
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
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnUTF8KeyPress;
  end;

implementation

uses
  LCLType, LCLProc, LCLIntf,
  Math,
  JvJCLUtils, JvJVCLUtils;

//=== { TJvImageListViewerOptions } ==========================================

constructor TJvImageListViewerOptions.Create(AOwner: TJvCustomItemViewer);
begin
  inherited Create(AOwner);
  FDrawingStyle := dsTransparent;
  FSelectedStyle := dsSelected;
  FFillCaption := True;
  FFrameSize := 1;
end;

procedure TJvImageListViewerOptions.SetDrawingStyle(const Value: TDrawingStyle);
begin
  if FDrawingStyle <> Value then
  begin
    FDrawingStyle := Value;
    Change;
  end;
end;

procedure TJvImageListViewerOptions.SetFillCaption(const Value: Boolean);
begin
  if FFillCaption <> Value then
  begin
    FFillCaption := Value;
    Change;
  end;
end;

procedure TJvImageListViewerOptions.SetFrameSize(const Value: Word);
begin
  if FFrameSize <> Value then
  begin
    FFrameSize := Value;
    Change;
  end;
end;

procedure TJvImageListViewerOptions.SetSelectedStyle(const Value: TDrawingStyle);
begin
  if FSelectedStyle <> Value then
  begin
    FSelectedStyle := Value;
    Change;
  end;
end;

//=== { TJvImageListViewer } =================================================

constructor TJvImageListViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := @DoImageChange;
  Color := clWindow;
end;

destructor TJvImageListViewer.Destroy;
begin
  Images := nil;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvImageListViewer.DoImageChange(Sender: TObject);
begin
  if Images <> nil then
    Count := Images.Count
  else
    Count := 0;
  Repaint;
end;

procedure TJvImageListViewer.DrawItem(Index: Integer; State: TCustomDrawState;
  ACanvas: TCanvas; AItemRect, TextRect: TRect);
//const
//  DrawingStyles: array [TDrawingStyle] of Cardinal =
//    (ILD_FOCUS, ILD_SELECTED, ILD_NORMAL, ILD_TRANSPARENT);
//  DrawMask: array [Boolean] of Cardinal =
//    (ILD_MASK, ILD_NORMAL);
var
  X, Y: Integer;
  S: WideString;
  DrawStyle: TDrawingStyle;
  Flags: Cardinal;
begin
  ACanvas.Brush.Color := Color;
  ACanvas.Font := Self.Font;
  if Images <> nil then
  begin
    Flags := DT_END_ELLIPSIS or DT_EDITCONTROL;
    S := GetCaption(Index);
    // determine where to draw image
    X := Max(AItemRect.Left, AItemRect.Left + (RectWidth(AItemRect) - Images.Width) div 2);
    Y := AItemRect.Top + (RectHeight(AItemRect) - Images.Height) div 2;
    if not Options.FillCaption then
      OffsetRect(TextRect,0,2);
    if cdsSelected in State then
    begin
      if Options.BrushPattern.Active then
      begin
        ACanvas.Pen.Color := Options.BrushPattern.OddColor;
        ACanvas.Brush.Bitmap := Options.BrushPattern.GetBitmap;
      end
      else
      begin
        ACanvas.Pen.Color := Options.BrushPattern.OddColor;
        ACanvas.Brush.Color := Options.BrushPattern.OddColor;
      end;
      if Options.FrameSize > 0 then
      begin
        ACanvas.Pen.Width := Options.FrameSize;
        ACanvas.Rectangle(AItemRect);
      end
      else
        ACanvas.FillRect(AItemRect);
    end
    else
    begin
      ACanvas.Brush.Color := Color;
      ACanvas.FillRect(AItemRect);
    end;
    if cdsSelected in Items[Index].State then
      DrawStyle := {DrawingStyles[}Options.SelectedStyle{]}
    else
      DrawStyle := {DrawingStyles[}Options.DrawingStyle{]};
    //ImageList_Draw(Images.Handle, Index, ACanvas.Handle, X, Y,
    //  DrawStyle or DrawMask[Images.ImageType = itImage]);
    Images.Draw(ACanvas, X, Y, Index, DrawStyle, itImage);
    if S <> '' then
    begin
      if cdsSelected in State then
      begin
        ACanvas.Brush.Color := clHighlight; // Options.BrushPattern.OddColor;
        ACanvas.Font.Color := clHighlightText; // Options.BrushPattern.EvenColor;
      end
      else
        SetBkMode(ACanvas.Handle, {Windows.}TRANSPARENT);
      if (Options.Layout <> tlCenter) and Options.FillCaption then
        ACanvas.FillRect(TextRect)
      else
        S := ' ' + S + ' ';
      ViewerDrawText(ACanvas, PWideChar(S), Length(S), TextRect, Flags, taCenter, tlCenter, True);
    end;
//    if not Options.BrushPattern.Active and (cdsSelected in State) then
//    begin
//      ACanvas.DrawFocusRect(AItemRect);
//    end;
  end;
end;

function TJvImageListViewer.GetCaption(ImageIndex: Integer): WideString;
begin
  Result := '';
  if Assigned(FOnGetCaption) then
    FOnGetCaption(Self, ImageIndex, Result);
end;

function TJvImageListViewer.GetOptions: TJvImageListViewerOptions;
begin
  Result := TJvImageListViewerOptions(inherited Options);
end;

function TJvImageListViewer.GetOptionsClass: TJvItemViewerOptionsClass;
begin
  Result := TJvImageListViewerOptions;
end;

procedure TJvImageListViewer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TJvImageListViewer.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    ReplaceImageListReference(Self, Value, FImages, FChangeLink);
    Count := 0;
    if FImages <> nil then
    begin
      Options.Width := Max(Options.Width, FImages.Width);
      Options.Height := Max(Options.Height, FImages.Height);
    end;
    DoImageChange(Value);
  end;
end;

procedure TJvImageListViewer.SetOptions(const Value: TJvImageListViewerOptions);
begin
  inherited Options := Value;
end;

end.

