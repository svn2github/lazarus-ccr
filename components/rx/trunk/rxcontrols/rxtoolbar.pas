{ rxtoolbar unit

  Copyright (C) 2005-2018 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

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
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxtoolbar;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, Buttons, Controls, ExtCtrls, ActnList,
  PropertyStorage, Menus, Forms, types, Graphics, ImgList;

const
  DefButtonWidth        = 24;
  DefButtonHeight       = 23;
  DefSeparatorWidth     = 7;
  DropDownExtraBtnWidth = 15;
  DefSpacing            = 4;
  
type
  TToolPanel = class;
  TToolbarItem = class;
  TToolbarItemsEnumerator = class;

  TToolbarButtonStyle = (tbrButton, tbrCheck, tbrDropDown, tbrSeparator,
     tbrDivider, tbrDropDownExtra);
  TToolBarStyle = (tbsStandart, tbsWindowsXP, tbsNative);
  TToolButtonAllign = (tbaNone, tbaLeft, tbaRignt);

  TToolPanelOption = (tpFlatBtns, tpTransparentBtns, tpStretchBitmap,
       tpCustomizable, tpGlyphPopup, tpCaptionPopup);
  TToolPanelOptions = set of TToolPanelOption;

  { TToolbarButtonActionLink }

  TToolbarButtonActionLink = class(TSpeedButtonActionLink)
  protected
    procedure SetImageIndex(Value: Integer); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
  public
    function IsImageIndexLinked: Boolean; override;
  end;
  
  TToolbarButtonActionLinkClass = class of TToolbarButtonActionLink;
  
  { TToolbarButton }
  TToolbarButton = class(TCustomSpeedButton)
  private
    FDesign:boolean;
    FDesignX,
    FDesignY:integer;
    FDrag:boolean;
    FDropDownMenu:TPopupMenu;
    FToolbarButtonStyle:TToolbarButtonStyle;
    FLastDrawFlagsA:integer;
    FOwnerItem:TToolbarItem;
    FFullPush:boolean;
    function IsDesignMode:boolean;
    procedure PaintSeparator;
    function ToolPanel:TToolPanel; inline;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure Paint; override;
    procedure Click; override;
    procedure UpdateState(InvalidateOnChange: boolean); override;
    procedure SetDesign(AValue:boolean; AToolbarItem:TToolbarItem);
    procedure SetEnabled(NewEnabled: boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetDrawFlagsA: integer;
    procedure CalculatePreferredSize( var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    destructor Destroy; override;
  end;

  { TToolbarItem }

  TToolbarItem = class(TCollectionItem)
  private
    FTextWidth:integer;
    FTextHeight:integer;

    FIntWidth:integer;
    FIntHeight:integer;

    FButton: TToolbarButton;
    function GetAction: TBasicAction;
    function GetButtonStyle: TToolbarButtonStyle;
    function GetDropDownMenu: TPopupMenu;
    function GetGroupIndex: Integer;
    function GetLayout: TButtonLayout;
    function GetShowCaption: boolean;
    function GetTag: Longint;
    function GetVisible: boolean;
    procedure SetAction(const AValue: TBasicAction);
    procedure SetButtonStyle(const AValue: TToolbarButtonStyle);
    procedure SetDropDownMenu(const AValue: TPopupMenu);
    procedure SetGroupIndex(const AValue: Integer);
    procedure SetLayout(const AValue: TButtonLayout);
    procedure SetShowCaption(const AValue: boolean);
    procedure SetTag(const AValue: Longint);
    procedure SetVisible(const AValue: boolean);

    procedure InternalCalcSize;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Action:TBasicAction read GetAction write SetAction;
    property Visible:boolean read GetVisible write SetVisible;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property ShowCaption:boolean read GetShowCaption write SetShowCaption;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    property Layout: TButtonLayout read GetLayout write SetLayout default blGlyphLeft;
    property ButtonStyle:TToolbarButtonStyle read GetButtonStyle write SetButtonStyle default tbrButton;
    property Tag: Longint read GetTag write SetTag default 0;
  end;

  { TToolbarItems }

  TToolbarItems = class(TOwnedCollection)
  private
    FToolPanel:TToolPanel;
    function GetByActionName(ActionName: string): TToolbarItem;
    function GetToolbarItem(Index: Integer): TToolbarItem;
    procedure SetToolbarItem(Index: Integer; const AValue: TToolbarItem);
  protected
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); override;
  public
    constructor Create(ToolPanel: TToolPanel);
    function GetEnumerator: TToolbarItemsEnumerator;
    function Add(AAction:TBasicAction): TToolbarItem;
    function IndexOf(AItem:TToolbarItem):Integer;
    property Items[Index: Integer]: TToolbarItem read GetToolbarItem write SetToolbarItem; default;
    property ByActionName[ActionName:string]:TToolbarItem read GetByActionName;
  end;

  { TToolbarItemsEnumerator }

  TToolbarItemsEnumerator = class
  private
    FList: TToolbarItems;
    FPosition: Integer;
  public
    constructor Create(AList: TToolbarItems);
    function GetCurrent: TToolbarItem;
    function MoveNext: Boolean;
    property Current: TToolbarItem read GetCurrent;
  end;


  { TToolPanel }

  TToolPanel = class(TCustomPanel)
  private
    //
    FInternalDefButtonWidth:integer;
    FInternalDefButtonHeight:integer;
    FInternalDefSeparatorWidth:integer;
    FInternalDropDownExtraBtnWidth:integer;
    FInternalSpacing:integer;

    //
    FButtonAllign: TToolButtonAllign;
    FCustomizeShortCut: boolean;
    FImageList: TImageList;
    FImageListSelected: TImageList;
    FOptions: TToolPanelOptions;
    FPropertyStorageLink:TPropertyStorageLink;
    FSpacing: Integer;
    FToolbarItems:TToolbarItems;

    FToolBarStyle: TToolBarStyle;
    FVersion: Integer;
    FArrowBmp:TBitmap;
    function GetBtnHeight: Integer;
    function GetBtnWidth: Integer;
    function GetPropertyStorage: TCustomPropertyStorage;
    procedure SetButtonAllign(const AValue: TToolButtonAllign);
    procedure SetImageList(const AValue: TImageList);
    procedure SetImageListSelected(const AValue: TImageList);
    procedure SetItems(const AValue: TToolbarItems);
    procedure SetOptions(const AValue: TToolPanelOptions);
    procedure SetPropertyStorage(const AValue: TCustomPropertyStorage);
    procedure OnIniSave(Sender: TObject);
    procedure OnIniLoad(Sender: TObject);
    procedure SetToolBarStyle(const AValue: TToolBarStyle);
    procedure ReAlignToolBtn;

    procedure InternalCalcImgSize;
    procedure InternalCalcButtonsSize(out MaxHeight:Integer);
  protected
    FDefImgWidth:integer;
    FDefImgHeight:integer;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetCustomizing(AValue:boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure CalculatePreferredSize( var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    function DoAlignChildControls(TheAlign: TAlign; AControl: TControl; AControlList: TFPList; var ARect: TRect): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Customize(HelpCtx: Longint);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    property DefImgWidth:integer read FDefImgWidth;
    property DefImgHeight:integer read FDefImgHeight;
  published
    property Items:TToolbarItems read FToolbarItems write SetItems;
    property ImageList:TImageList read FImageList write SetImageList;
    property ImageListSelected:TImageList read FImageListSelected write SetImageListSelected;
    property PropertyStorage:TCustomPropertyStorage read GetPropertyStorage write SetPropertyStorage;
    property ToolBarStyle:TToolBarStyle read FToolBarStyle write SetToolBarStyle default tbsStandart;
    property Options:TToolPanelOptions read FOptions write SetOptions;
    property Version: Integer read FVersion write FVersion default 0;
    property ButtonAllign:TToolButtonAllign read FButtonAllign write SetButtonAllign default tbaLeft;
    property CustomizeShortCut:boolean read FCustomizeShortCut write FCustomizeShortCut;

    property Align;
    property Alignment;
    property Anchors;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Spacing:Integer read FSpacing write FSpacing default DefSpacing;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation
uses Math, RxTBRSetup, LCLProc, rxlclutils, Dialogs, typinfo, rxdconst, GraphType,
  LResources, LazVersion, LCLVersion;

{ TToolbarItemsEnumerator }

constructor TToolbarItemsEnumerator.Create(AList: TToolbarItems);
begin
  FList := AList;
  FPosition := -1;
end;

function TToolbarItemsEnumerator.GetCurrent: TToolbarItem;
begin
  Result := FList[FPosition];
end;

function TToolbarItemsEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TToolbarButton }

function TToolbarButton.IsDesignMode: boolean;
begin
  Result:=(Assigned(Parent) and (csDesigning in Parent.ComponentState)) or (FDesign);
end;

procedure TToolbarButton.PaintSeparator;
var
  PaintRect: TRect;
  X, H:integer;
begin
  PaintRect:=ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(PaintRect);
  if FToolbarButtonStyle = tbrSeparator then
  begin
    X:=Width div 2 - 1;
    H:=TToolPanel(Parent).Height;
    if X>0 then
    begin
      Canvas.Pen.Color:=clBtnShadow;
      Canvas.Line(X, 1, X, H);
      Canvas.Pen.Color:=clWindow;
      Canvas.Line(X+1, 1, X+1, H);
    end;
  end;
end;

function TToolbarButton.ToolPanel: TToolPanel;
begin
  Result:=TToolbarItems(FOwnerItem.Collection).FToolPanel;
end;

procedure TToolbarButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsDesignMode  then
  begin
    FDrag:=true;
    FDesignX:=Max(X-1, 1);
    FDesignY:=Max(Y-1, 1);
  end
  else
  begin
    FFullPush:=X < (Width - DropDownExtraBtnWidth - 5);
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TToolbarButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if IsDesignMode and FDrag then
  begin
    Top:=Max(0, Min(Y+Top-FDesignY, Parent.Height - Height));
    Left:=Max(0, Min(X+Left-FDesignX, Parent.Width - Width));
  end
  else
  begin
    inherited MouseMove(Shift, X, Y);
  end
end;

procedure TToolbarButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsDesignMode  then
  begin
    FDrag:=false;
    Top:=4;
  end
  else
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TToolbarButton.MouseLeave;
begin
  inherited MouseLeave;
  FFullPush:=true;
end;

procedure TToolbarButton.Paint;
var
  PaintRect, PaintRect1: TRect;
  GlyphWidth, GlyphHeight: Integer;
  Offset, OffsetCap: TPoint;
  ClientSize, TotalSize, TextSize: TSize;
  M, S , FExternalImageWidth: integer;
  TXTStyle : TTextStyle;
  SIndex : Longint;
  TMP : String;
  AImageResolution: TScaledImageListResolution;
  FImgN, FImgS: TImageList;
begin
  if FToolbarButtonStyle in [tbrSeparator, tbrDivider] then
  begin
    PaintSeparator;
    exit;
  end;

  UpdateState(false);
  if (not Assigned(Action)) or (ToolPanel.FToolBarStyle = tbsNative) then
  begin
    inherited Paint;
    exit;
  end;

  FImgN:=ToolPanel.FImageList;
  FImgS:=ToolPanel.FImageListSelected;

  PaintRect:=ClientRect;

  if (Action is TCustomAction) and Assigned(FImgN) and (TCustomAction(Action).ImageIndex>-1) and (TCustomAction(Action).ImageIndex < FImgN.Count) then
  begin

    FLastDrawFlagsA:=GetDrawFlagsA;

    if not Transparent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(PaintRect);
    end;

    if FLastDrawFlagsA <> 0 then
    begin
      if ToolPanel.FToolBarStyle = tbsWindowsXP then
      begin

        if FToolbarButtonStyle = tbrDropDownExtra then
        begin
          PaintRect1:=PaintRect;
          Dec(PaintRect1.Right, DropDownExtraBtnWidth);
          if FFullPush then
          begin
            DrawButtonFrameXP(Canvas, PaintRect1, (FLastDrawFlagsA and DFCS_PUSHED) <> 0,
                                          (FLastDrawFlagsA and DFCS_FLAT) <> 0);
          end
          else
            DrawButtonFrameXP(Canvas, PaintRect1, false,
                                          (FLastDrawFlagsA and DFCS_FLAT) <> 0);
          ;

          PaintRect1:=PaintRect;
          PaintRect1.Left:=PaintRect1.Right -  DropDownExtraBtnWidth;
          DrawButtonFrameXP(Canvas, PaintRect1, (FLastDrawFlagsA and DFCS_PUSHED) <> 0,
                                           (FLastDrawFlagsA and DFCS_FLAT) <> 0);
        end
        else
          DrawButtonFrameXP(Canvas, PaintRect, (FLastDrawFlagsA and DFCS_PUSHED) <> 0,
                                         (FLastDrawFlagsA and DFCS_FLAT) <> 0)
      end
      else
      begin
        if FToolbarButtonStyle = tbrDropDownExtra then
        begin
          PaintRect1:=PaintRect;
          Dec(PaintRect1.Right, DropDownExtraBtnWidth);

          if FFullPush then
          begin
            DrawButtonFrame(Canvas, PaintRect1, (FLastDrawFlagsA and DFCS_PUSHED) <> 0,
                                         (FLastDrawFlagsA and DFCS_FLAT) <> 0);
          end
          else
          begin
            DrawButtonFrame(Canvas, PaintRect1, false,
                                         (FLastDrawFlagsA and DFCS_FLAT) <> 0);
          end;
          
          PaintRect1:=PaintRect;
          PaintRect1.Left:=PaintRect1.Right - DropDownExtraBtnWidth;
          DrawButtonFrame(Canvas, PaintRect1, (FLastDrawFlagsA and DFCS_PUSHED) <> 0,
                                           (FLastDrawFlagsA and DFCS_FLAT) <> 0);
        end
        else
          DrawButtonFrame(Canvas, PaintRect, (FLastDrawFlagsA and DFCS_PUSHED) <> 0,
                                         (FLastDrawFlagsA and DFCS_FLAT) <> 0);
      end;
    end;

    if FToolbarButtonStyle = tbrDropDownExtra then
    begin
      Canvas.Draw(PaintRect.Right - 10, Height div 2, ToolPanel.FArrowBmp);
      Dec(PaintRect.Right, DropDownExtraBtnWidth);
    end;
      
    GlyphWidth:=ToolPanel.FDefImgWidth;
    GlyphHeight:=ToolPanel.FDefImgHeight;

    ClientSize.cx:= PaintRect.Right - PaintRect.Left;
    ClientSize.cy:= PaintRect.Bottom - PaintRect.Top;

    if ShowCaption then
    begin
      TextSize.cx:= FOwnerItem.FTextWidth;
      TextSize.cy:= FOwnerItem.FTextHeight;
    end
    else
    begin
      TextSize.cx:= 0;
      TextSize.cy:= 0;
    end;

    if (GlyphWidth = 0) or (GlyphHeight = 0) or (TextSize.cx = 0) or (TextSize.cy = 0)  then
      S:= 0
    else
      S:= ToolPanel.FInternalSpacing;

    // Calculate caption and glyph layout

    if S = -1 then
    begin
        TotalSize.cx:= TextSize.cx + GlyphWidth;
        TotalSize.cy:= TextSize.cy + GlyphHeight;
        if Layout in [blGlyphLeft, blGlyphRight] then
          M:= (ClientSize.cx - TotalSize.cx) div 3
        else
          M:= (ClientSize.cy - TotalSize.cy) div 3;
        S:= M;
    end
    else
    begin
        TotalSize.cx:= GlyphWidth + S + TextSize.cx;
        TotalSize.cy:= GlyphHeight + S + TextSize.cy;
        if Layout in [blGlyphLeft, blGlyphRight] then
          M:= (ClientSize.cx - TotalSize.cx + 1) div 2
        else
          M:= (ClientSize.cy - TotalSize.cy + 1) div 2
    end;

    case Layout of
      blGlyphLeft :
        begin
          Offset.X:= M;
          Offset.Y:= (ClientSize.cy - GlyphHeight + 1) div 2;
          OffsetCap.X:= Offset.X + GlyphWidth + S;
          OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
        end;
      blGlyphRight : begin
        Offset.X:= ClientSize.cx - M - GlyphWidth;
        Offset.Y:= (ClientSize.cy - GlyphHeight + 1) div 2;
        OffsetCap.X:= Offset.X - S - TextSize.cx;
        OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
      end;
      blGlyphTop : begin
        Offset.X:= (ClientSize.cx - GlyphWidth + 1) div 2;
        Offset.Y:= M;
        OffsetCap.X:= (ClientSize.cx - TextSize.cx + 1) div 2;
        OffsetCap.Y:= Offset.Y + GlyphHeight + S;
      end;
      blGlyphBottom : begin
        Offset.X:= (ClientSize.cx - GlyphWidth + 1) div 2;
        Offset.Y:= ClientSize.cy - M - GlyphHeight;
        OffsetCap.X:= (ClientSize.cx - TextSize.cx + 1) div 2;
        OffsetCap.Y:= Offset.Y - S - TextSize.cy;
      end;
    end;

    if ((FLastDrawFlagsA and DFCS_FLAT) <> 0) and ((FLastDrawFlagsA and DFCS_PUSHED) = 0) and (not Assigned(FImgS)) and (tpGlyphPopup in ToolPanel.Options) and FFullPush then
    begin
      {$IF LCL_FullVersion >= 1080000}
        //FImageList.DrawForPPI(Canvas, Offset.X, Offset.Y,
        FImgN.DrawForPPI(Canvas, Offset.X, Offset.Y,
           TCustomAction(Action).ImageIndex,
           FImgN.Width,
           Font.PixelsPerInch,
           GetCanvasScaleFactor,
           gdeShadowed);
      {$ELSE}
      FImageList.Draw(Canvas, Offset.X, Offset.Y, TCustomAction(Action).ImageIndex, gde1Bit);
      {$ENDIF}
      Dec(Offset.X, 2);
      Dec(Offset.Y, 2);
    end;

    if Assigned(FImgS) and (FImgS.Count>TCustomAction(Action).ImageIndex) and ((FLastDrawFlagsA and DFCS_FLAT) <> 0) and ((FLastDrawFlagsA and DFCS_PUSHED) = 0) then
    begin
      {$IF LCL_FullVersion >= 1080000}
      FImgS.DrawForPPI(Canvas, Offset.X, Offset.Y,
         TCustomAction(Action).ImageIndex,
         FImgS.Width,
         Font.PixelsPerInch,
         GetCanvasScaleFactor,
         TCustomAction(Action).Enabled);
      {$ELSE}
      FImgS.Draw(Canvas, Offset.X, Offset.Y, TCustomAction(Action).ImageIndex, TCustomAction(Action).Enabled)
      {$ENDIF}
    end
    else
    {$IF LCL_FullVersion >= 1080000}
      //FImageList.DrawForPPI(Canvas, Offset.X, Offset.Y,
      FImgN.DrawForPPI(Canvas, Offset.X, Offset.Y,
         TCustomAction(Action).ImageIndex,
         FImgN.Width,
         Font.PixelsPerInch,
         GetCanvasScaleFactor,
         TCustomAction(Action).Enabled);
    {$ELSE}
      FImageList.Draw(Canvas, Offset.X, Offset.Y, TCustomAction(Action).ImageIndex, TCustomAction(Action).Enabled);
    {$ENDIF}
  end;

  if (Caption <> '') and ShowCaption then
  begin
    TXTStyle := Canvas.TextStyle;
    TXTStyle.Opaque := False;
    TXTStyle.Clipping := True;
    TXTStyle.ShowPrefix := True;
    TXTStyle.Alignment := taLeftJustify;
    TXTStyle.Layout := tlTop;
    TXTStyle.SystemFont := Canvas.Font.IsDefault;//Match System Default Style
    With PaintRect, OffsetCap do
    begin
      Left := Left + X;
      Top := Top + Y;
    end;
    If not Enabled then
    begin
      Canvas.Font.Color := clBtnHighlight;
      OffsetRect(PaintRect, 1, 1);
      Canvas.TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
      Canvas.Font.Color := clBtnShadow;
      OffsetRect(PaintRect, -1, -1);
    end
    else
    begin
      Canvas.Font.Color := clWindowText;
      if ((FLastDrawFlagsA and DFCS_FLAT) <> 0) and ((FLastDrawFlagsA and DFCS_PUSHED) = 0) and (ToolPanel.FToolBarStyle <> tbsWindowsXP) and (tpCaptionPopup in ToolPanel.Options) then
        OffsetRect(PaintRect, -2, -2);
    end;
    Canvas.TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
  end;
end;

procedure TToolbarButton.Click;
var
  P:TPoint;
begin
  if (csDesigning in ComponentState) or FDesign then exit;
  if FToolbarButtonStyle = tbrDropDown then
  begin
    if Assigned(FDropDownMenu) then
    begin
      P.X:=0;
      P.Y:=Height;
      P:=ClientToScreen(P);
      FDropDownMenu.PopUp(P.X, P.Y);
    end;
  end
  else
  if (FToolbarButtonStyle = tbrDropDownExtra) and (not FFullPush) then
  begin
    if Assigned(FDropDownMenu) then
    begin
      P.X:=Width - DropDownExtraBtnWidth;
      P.Y:=Height;
      P:=ClientToScreen(P);
      FDropDownMenu.PopUp(P.X, P.Y);
    end;
  end
  else
    inherited Click;
end;

procedure TToolbarButton.UpdateState(InvalidateOnChange: boolean);
var
  OldState: TButtonState;
begin
  OldState:=FState;
  inherited UpdateState(InvalidateOnChange);
  if InvalidateOnChange and ((FState<>OldState) or (FLastDrawFlagsA<>GetDrawFlagsA)) then
    Invalidate;
end;

procedure TToolbarButton.SetDesign(AValue:boolean; AToolbarItem:TToolbarItem);
begin
  FDesign:=AValue;
  if FDesign then
  begin
    Enabled:=true;
    Flat:=false;
  end
  else
  begin
    Flat:=tpFlatBtns in ToolPanel.Options;
    ActionChange(Action, true);
  end;
end;

procedure TToolbarButton.SetEnabled(NewEnabled: boolean);
begin
  if FToolbarButtonStyle = tbrDropDown then
    NewEnabled :=true;
  if (not NewEnabled) and Enabled then
  begin
    FState := bsDisabled;
    MouseLeave;
  end;
  inherited SetEnabled(NewEnabled);
end;

function TToolbarButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result:=TToolbarButtonActionLink;
end;

function TToolbarButton.GetDrawFlagsA: integer;
begin
  if (Flat and not MouseInControl and not (FState in [bsDown, bsExclusive])) or (not Enabled) then
  begin
    Result := 0;
  end
  else
  begin
    Result:=DFCS_BUTTONPUSH;
    if FState in [bsDown, bsExclusive] then
      inc(Result,DFCS_PUSHED);
    if not Enabled then
      inc(Result,DFCS_INACTIVE);
    if Flat then
      inc(Result,DFCS_FLAT);
  end;
end;

procedure TToolbarButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  if PreferredWidth <  FOwnerItem.FIntWidth then
    PreferredWidth:=FOwnerItem.FIntWidth;
  if PreferredHeight < FOwnerItem.FIntHeight then
    PreferredHeight:=FOwnerItem.FIntHeight;
end;

destructor TToolbarButton.Destroy;
begin
  if Assigned(FOwnerItem) then
  begin
    FOwnerItem.FButton:=nil;
    FOwnerItem.Free;
  end;
  inherited Destroy;
end;


{ TToolbarItems }

function TToolbarItems.GetToolbarItem(Index: Integer): TToolbarItem;
begin
  result := TToolbarItem( inherited Items[Index] );
end;

function TToolbarItems.GetByActionName(ActionName: string): TToolbarItem;
var
  i:integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
    if Assigned(Items[i].Action) and (Items[i].Action.Name = ActionName) then
    begin
      Result:=Items[i];
    end;
end;

procedure TToolbarItems.SetToolbarItem(Index: Integer;
  const AValue: TToolbarItem);
begin
  Items[Index].Assign( AValue );
end;

procedure TToolbarItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  if Action = cnDeleting then
    FToolPanel.DisableAlign;
  inherited Notify(Item, Action);
  if Action = cnDeleting then
    FToolPanel.EnableAlign;
end;

constructor TToolbarItems.Create(ToolPanel: TToolPanel);
begin
  inherited Create(ToolPanel, TToolbarItem);
  FToolPanel:=ToolPanel;
end;

function TToolbarItems.GetEnumerator: TToolbarItemsEnumerator;
begin
  Result:=TToolbarItemsEnumerator.Create(Self);
end;

function TToolbarItems.Add(AAction: TBasicAction): TToolbarItem;
begin
  Result:=inherited Add as TToolbarItem;
  Result.Action:=AAction;
end;

function TToolbarItems.IndexOf(AItem: TToolbarItem): Integer;
var
  i: Integer;
begin
  Result:=-1;
  if AItem.Collection <> Self then Exit;
  for i:=0 to Count-1 do
    if Items[i] = AItem then
      Exit(i);
end;

{ TToolPanel }

function TToolPanel.GetBtnHeight: Integer;
var
  AImageResolution: TScaledImageListResolution;
  FExternalImageWidth: Integer;
begin
  {$IF LCL_FullVersion >= 1080000}
  FExternalImageWidth:=0;
  AImageResolution := FImageList.ResolutionForPPI[FExternalImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
  Result:=AImageResolution.Height;
  {$ELSE}
  Result:=FDefButtonHeight;
  {$ENDIF}
end;

function TToolPanel.GetBtnWidth: Integer;
var
  AImageResolution: TScaledImageListResolution;
  FExternalImageWidth: Integer;
begin
  {$IF LCL_FullVersion >= 1080000}
  FExternalImageWidth:=0;
  AImageResolution := FImageList.ResolutionForPPI[FExternalImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
  Result:=AImageResolution.Width;
  {$ELSE}
  Result:=FDefButtonWidth;
  {$ENDIF}
end;

function TToolPanel.GetPropertyStorage: TCustomPropertyStorage;
begin
  Result:=FPropertyStorageLink.Storage;
end;

procedure TToolPanel.SetButtonAllign(const AValue: TToolButtonAllign);
var
  i:integer;
  ARect: TRect;
begin
  if FButtonAllign=AValue then exit;
  FButtonAllign:=AValue;
  if not (csLoading in ComponentState) then
    ReAlign;
end;

procedure TToolPanel.SetImageList(const AValue: TImageList);
var
  i:integer;
begin
  if FImageList=AValue then exit;
  FImageList:=AValue;
  if not (csLoading in ComponentState) then
    ReAlignToolBtn;
end;

procedure TToolPanel.SetImageListSelected(const AValue: TImageList);
var
  i:integer;
begin
  if FImageListSelected=AValue then exit;
  FImageListSelected:=AValue;
end;

procedure TToolPanel.SetItems(const AValue: TToolbarItems);
begin
  FToolbarItems.Assign(AValue);
end;

procedure TToolPanel.SetOptions(const AValue: TToolPanelOptions);
var
  i:integer;
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;

  for i:=0 to FToolbarItems.Count - 1 do
  begin
    FToolbarItems[i].FButton.Transparent:=tpTransparentBtns in FOptions;
    FToolbarItems[i].FButton.Flat:=tpFlatBtns in FOptions;
  end;
  
  Invalidate;
end;

procedure TToolPanel.SetPropertyStorage(const AValue: TCustomPropertyStorage);
begin
  FPropertyStorageLink.Storage:=AValue;
end;


procedure TToolPanel.OnIniSave(Sender: TObject);
var
  S, S1:string;
  tpo:TToolPanelOptions;
  tpo1:integer absolute tpo;
  IT: TToolbarItem;
  I:integer;
begin
  S:=Owner.Name+'.'+Name;
  FPropertyStorageLink.Storage.WriteInteger(S+sVersion2, FVersion);
  FPropertyStorageLink.Storage.WriteInteger(S+sShowHint, ord(ShowHint));
  tpo:=FOptions;
  FPropertyStorageLink.Storage.WriteString(S+sOptions, SetToString(GetPropInfo(Self, 'Options'), tpo1));
  FPropertyStorageLink.Storage.WriteString(S+sToolBarStyle, GetEnumProp(Self, 'ToolBarStyle'));
  FPropertyStorageLink.Storage.WriteString(S+sButtonAllign, GetEnumProp(Self, 'ButtonAllign'));

  I:=0;
  for IT in Items do
    if IT.Visible then
    begin
      S1:=S + sItem + IntToStr(i);
      FPropertyStorageLink.Storage.WriteString(S1+sOptions, GetEnumProp(IT, 'ButtonStyle'));
      if Assigned(IT.Action) then
      begin
        FPropertyStorageLink.Storage.WriteString(S1+sAction, IT.Action.Name);
        FPropertyStorageLink.Storage.WriteInteger(S1+sShowCaption, ord(IT.ShowCaption));
        if FCustomizeShortCut then
          FPropertyStorageLink.Storage.WriteString(S1+sShortCut, ShortCutToText(TCustomAction(IT.Action).ShortCut));
      end;
      Inc(i);
    end;
  FPropertyStorageLink.Storage.WriteInteger(S+sCount, i);
(*  for i:=0 to FToolbarItems.Count-1 do
  begin
    IT:=FToolbarItems[i];
    if IT.Visible then ;
    if Assigned(IT.Action) then
    begin
      FPropertyStorageLink.Storage.WriteString(S1+sAction, IT.Action.Name);
      FPropertyStorageLink.Storage.WriteInteger(S1+sVisible, ord(IT.Visible));
      FPropertyStorageLink.Storage.WriteInteger(S1+sShowCaption, ord(IT.ShowCaption));
      if FCustomizeShortCut and Assigned(IT.Action) then
        FPropertyStorageLink.Storage.WriteString(S1+sShortCut, ShortCutToText(TCustomAction(IT.Action).ShortCut));
    end
    else
    if IT
    begin
      FPropertyStorageLink.Storage.WriteString(S1+sAction, '');
      FPropertyStorageLink.Storage.WriteString(S1+sAction, '');
    end;
  end; *)
end;

procedure TToolPanel.OnIniLoad(Sender: TObject);
function OldReadBtnPosition(ASection:string):boolean;
var
  ACount: LongInt;
  i: Integer;
  S, S1, AActionName, S2: String;
  St: TStringList;
  P, P1: TToolbarItem;
begin
  ACount:=FPropertyStorageLink.Storage.ReadInteger(ASection+sCount, -1);
  if ACount < 0 then Exit(false);
  Result:=true;
  S:=ASection+sItem;
  St:=TStringList.Create;
  St.Sorted:=true;
  for P in Items do P.Visible:=false;
  for i:=0 to ACount-1 do
  begin
    S1:=S+IntToStr(i);
    AActionName:=FPropertyStorageLink.Storage.ReadString(S1+sAction, '');
    P:=FToolbarItems.ByActionName[AActionName];
    if Assigned(P) then
    begin
      St.AddObject('%0.5d-%s', [FPropertyStorageLink.Storage.ReadInteger(S1+sLeft, -1), AActionName], P);
      P.Visible:=true;

      if FCustomizeShortCut and Assigned(P.Action) then
      begin
        S2:=FPropertyStorageLink.Storage.ReadString(S1+sShortCut, ShortCutToText(TCustomAction(P.Action).ShortCut));
        TCustomAction(P.Action).ShortCut:=TextToShortCut(S2);
      end;
    end;
  end;

  for i:=0 to St.Count-1 do
  begin
    P:=TToolbarItem(St.Objects[i]);
    P1:=Items[i];
    Items.Exchange(P.ID, P1.ID);
  end;
  St.Free;
end;

function GetFreeSeparator:TToolbarItem;
var
  TI: TToolbarItem;
begin
  Result:=nil;
  for TI in Items do
    if (TI.ButtonStyle in [tbrSeparator, tbrDivider]) and (not TI.Visible) then
      Exit(TI);

  Result:=Items.Add(nil);
  Result.ButtonStyle:=tbrSeparator;
  Result.Visible:=true;
end;

function ReadBtnPosition(ASection:string):boolean;
var
  St: TStringList;
  P, P1: TToolbarItem;
  FCnt: LongInt;
  S, S1, SType, FActionName, S2: string;
  i: Integer;
begin
  Result:=false;
  S:=ASection;
  FCnt:=FPropertyStorageLink.Storage.ReadInteger(S+sVersion2, FVersion);
  if FCnt < FVersion then Exit;
  for P in Items do P.Visible:=false;
  St:=TStringList.Create;
  FCnt:=FPropertyStorageLink.Storage.ReadInteger(S+sCount, 0);
  S:=S+sItem;
  for i:=0 to FCnt-1 do
  begin
    S1:=S+IntToStr(i);
    SType:=FPropertyStorageLink.Storage.ReadString(S1+sOptions, '');
    if (SType = 'tbrSeparator') or (SType = 'tbrDivider') then
    begin
      P:=GetFreeSeparator;
      if Assigned(P) then
      begin
        P.Visible:=true;
        St.AddObject('S', P);
      end;
    end
    else
    if (SType <>'') then
    begin
      FActionName:=FPropertyStorageLink.Storage.ReadString(S1+sAction, '');
      P:=FToolbarItems.ByActionName[FActionName];
      if Assigned(P) then
      begin
        P.Visible:=true;
        P.ShowCaption:=FPropertyStorageLink.Storage.ReadInteger(S1+sShowCaption, ord(P.ShowCaption))<>0;
        if FCustomizeShortCut and Assigned(P.Action) then
        begin
          S2:=FPropertyStorageLink.Storage.ReadString(S1+sShortCut, ShortCutToText(TCustomAction(P.Action).ShortCut));
          TCustomAction(P.Action).ShortCut:=TextToShortCut(S2);
        end;

        St.AddObject('S', P);
      end;
    end;
  end;
  for i:=0 to St.Count-1 do
  begin
    P:=TToolbarItem(St.Objects[i]);
    P1:=Items[i];
    Items.Exchange(P.ID, P1.ID);
  end;

  St.Free;

  Result:=true;
end;

var
  i, ACount:integer;
  S1, AActionName, S2:string;
  AItem:TToolbarItem;
  tpo:TToolPanelOptions;
  tpo1:integer absolute tpo;
  S: TComponentName;
  FEnableRead: Boolean;
begin
  S:=Owner.Name+'.'+Name;
  ACount:=FPropertyStorageLink.Storage.ReadInteger(S+sVersion2, -9999); //Check cfg version

  Items.BeginUpdate;
  if ACount = -9999 then
    FEnableRead:=OldReadBtnPosition(S)
  else
    FEnableRead:=ReadBtnPosition(S);

  if FEnableRead then
  begin
    ShowHint:=FPropertyStorageLink.Storage.ReadInteger(S+sShowHint, ord(ShowHint))<>0;

    tpo:=FOptions;
    tpo1:=StringToSet(GetPropInfo(Self, 'Options'), FPropertyStorageLink.Storage.ReadString(S+sOptions, SetToString(GetPropInfo(Self, 'Options'), tpo1)));
    SetOptions(tpo);

    SetEnumProp(Self, 'ToolBarStyle', FPropertyStorageLink.Storage.ReadString(S+sToolBarStyle, GetEnumProp(Self, 'ToolBarStyle')));
    SetEnumProp(Self, 'ButtonAllign', FPropertyStorageLink.Storage.ReadString(S+sButtonAllign, GetEnumProp(Self, 'ButtonAllign')));
  end;
  Items.EndUpdate;
  ReAlignToolBtn;
  Invalidate;
end;

procedure TToolPanel.SetToolBarStyle(const AValue: TToolBarStyle);
begin
  if FToolBarStyle=AValue then exit;
  FToolBarStyle:=AValue;

  if FToolBarStyle = tbsWindowsXP then
  begin
//    BorderWidth:=4;
    SetOptions(FOptions + [tpFlatBtns]);
  end
  else
  begin
//    BorderWidth:=1
  end;
  Invalidate;
end;


procedure TToolPanel.ReAlignToolBtn;
var
  H: Integer;
begin
  if Assigned(Owner) and not (csLoading in ComponentState) then
  begin
    InternalCalcButtonsSize(H);
    ReAlign;
  end;
end;

procedure TToolPanel.InternalCalcImgSize;
var
  AImageResolution: TScaledImageListResolution;
begin
  FInternalDefButtonWidth:=ScaleDesignToForm(DefButtonWidth);
  FInternalDefButtonHeight:=ScaleDesignToForm(DefButtonHeight);
  FInternalDefSeparatorWidth:=ScaleDesignToForm(DefSeparatorWidth);
  FInternalDropDownExtraBtnWidth:=ScaleDesignToForm(DropDownExtraBtnWidth);
  FInternalSpacing:=ScaleDesignToForm(Spacing);

  if Assigned(FImageList) then
  begin
    {$IF LCL_FullVersion >= 1080000}
    AImageResolution := FImageList.ResolutionForPPI[0, Font.PixelsPerInch, GetCanvasScaleFactor];
    FDefImgWidth:=AImageResolution.Width;
    FDefImgHeight:=AImageResolution.Height;
    {$ELSE}
    FDefImgWidth:=FImageList.Width;
    FDefImgHeight:=FImageList.Height;
    {$ENDIF}
  end;
end;

procedure TToolPanel.InternalCalcButtonsSize(out MaxHeight: Integer);
var
  DC: HDC;
  Flags: Cardinal;
  TI: TToolbarItem;
  R: TRect;
  OldFont: HGDIOBJ;
  S: TTranslateString;
begin
  MaxHeight:=0;
  InternalCalcImgSize;
  DC := GetDC(Handle);
  try
    OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
    Flags := DT_CALCRECT or DT_SINGLELINE or DT_NOPREFIX;

    for TI in FToolbarItems do
    begin
      R := Rect(0, 0, 10000, 10000);

      if Assigned(TI.FButton) and Assigned(TI.Action) and (TI.Action is TCustomAction) then
      begin
        //S:=TCustomAction(TI.Action).Caption;
        S:=TI.FButton.Caption;
        DrawText(DC, PChar(S), Length(S), R, Flags);
        TI.FTextWidth := R.Right - R.Left;
        TI.FTextHeight := R.Bottom - R.Top;
        TI.InternalCalcSize;
      end;
    end;
    SelectObject(DC, OldFont);
  finally
    ReleaseDC(Parent.Handle, DC);
  end;

  for TI in FToolbarItems do
    if Assigned(TI.FButton) then
    begin
      TI.InternalCalcSize;
      MaxHeight:=Max(MaxHeight, TI.FIntHeight);
    end;


  for TI in FToolbarItems do
    if Assigned(TI.FButton) then
      TI.FIntHeight:=MaxHeight;
end;

procedure TToolPanel.Notification(AComponent: TComponent; Operation: TOperation);
var
  i:integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImageList then
      SetImageList(nil)
    else
    if AComponent = FImageListSelected then
      SetImageListSelected(nil)
    else
    if AComponent is TPopupMenu then
    begin
      for i:=0 to FToolbarItems.Count - 1 do
        if FToolbarItems[i].DropDownMenu = AComponent then
          FToolbarItems[i].DropDownMenu:=nil;
    end
    else
    if AComponent is TBasicAction then
    begin
      for i:=0 to FToolbarItems.Count - 1 do
        if FToolbarItems[i].Action = AComponent then
          FToolbarItems[i].Action:=nil;
    end;
  end;
end;

procedure TToolPanel.SetCustomizing(AValue: boolean);
var
  i:integer;
begin
  for i:=0 to FToolbarItems.Count - 1 do
    FToolbarItems[i].FButton.SetDesign(AValue, FToolbarItems[i]);
end;

procedure TToolPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbRight) and (ssCtrl in Shift) and (tpCustomizable in FOptions) then
    Customize(HelpContext);
end;

procedure TToolPanel.Loaded;
begin
  inherited Loaded;
  SetCustomizing(false);
end;

procedure TToolPanel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=Parent.ClientWidth;
  InternalCalcButtonsSize(PreferredHeight);
  inherited CalculatePreferredSize( PreferredWidth, PreferredHeight, WithThemeSpace);
end;

function TToolPanel.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
  AControlList: TFPList; var ARect: TRect): Boolean;
var
  R: TRect;
  TI: TToolbarItem;
  I, L: Integer;
begin
  if TheAlign = alCustom then
  begin
    Result:=true;

    if FButtonAllign = tbaLeft then
    begin
      L:=BorderWidth;
      for i:=0 to FToolbarItems.Count-1 do
      begin
        TI:=FToolbarItems[i];
        if TI.Visible and Assigned(TI.FButton) then
        begin
          TI.FButton.SetBounds(L, FInternalSpacing, TI.FIntWidth, TI.FIntHeight);
          L:=L + TI.FIntWidth;
        end;
      end;
    end
    else
    begin
      L:=ClientWidth - BorderWidth;
      for i:=FToolbarItems.Count-1 downto 0 do
      begin
        TI:=FToolbarItems[i];
        if TI.Visible and Assigned(TI.FButton) then
        begin
          L:=L - TI.FIntWidth;
          TI.FButton.SetBounds(L, FInternalSpacing, TI.FIntWidth, TI.FIntHeight);
        end;
      end;
    end;
  end
  else
    Result:=inherited DoAlignChildControls(TheAlign, AControl, AControlList, ARect);
end;

constructor TToolPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArrowBmp:=CreateArrowBitmap;
  FCustomizeShortCut:=false;
  AutoSize:=true;
  FButtonAllign:=tbaLeft;
  FToolbarItems:=TToolbarItems.Create(Self);
  if Assigned(AOwner) and not (csLoading in AOwner.ComponentState) then
    Align:=alTop;
  Height:=DefButtonHeight;
  FPropertyStorageLink:=TPropertyStorageLink.Create;
  FPropertyStorageLink.OnSave:=@OnIniSave;
  FPropertyStorageLink.OnLoad:=@OnIniLoad;
  FToolBarStyle:=tbsStandart;
  BorderWidth:=4;
  ControlStyle:=ControlStyle - [csSetCaption] + [csAcceptsControls];
  FSpacing:=DefSpacing;
  Caption:='';
end;

destructor TToolPanel.Destroy;
begin
  FreeAndNil(FToolbarItems);
  FreeAndNil(FPropertyStorageLink);
  FreeAndNil(FArrowBmp);
  inherited Destroy;
end;

procedure TToolPanel.Customize(HelpCtx: Longint);
var
  FCustomizer: TToolPanelSetupForm;
begin
  FCustomizer:=TToolPanelSetupForm.CreateSetupForm(Self);
  try
    FCustomizer.HelpContext:=HelpCtx;
    SetCustomizing(true);
    FCustomizer.ShowModal;
  finally
    SetCustomizing(false);
    FCustomizer.Free;
  end;
end;

procedure TToolPanel.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
var
  FExternalImageWidth: Integer;
  AImageResolution: TScaledImageListResolution;
begin
  if not (csLoading in ComponentState) then
  begin
    if Assigned(FImageList) then
    begin
      {$IF LCL_FullVersion >= 1080000}
      FExternalImageWidth:=0;
      AImageResolution := FImageList.ResolutionForPPI[FExternalImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
      aHeight:=AImageResolution.Height+8  + Max(BorderWidth, 4) * 2
      {$ELSE}
      aHeight:=FImageList.Height+8  + Max(BorderWidth, 4) * 2
      {$ENDIF}
    end
    else
      aHeight:=FInternalDefButtonHeight + BorderWidth * 2;
  end;
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;

{ TToolbarItem }

procedure TToolbarItem.SetAction(const AValue: TBasicAction);
begin
  if FButton.Action<>AValue then
  begin
    FButton.Action:=AValue;
    if not (csLoading in TToolbarItems(Collection).FToolPanel.ComponentState) then
      TToolbarItems(Collection).FToolPanel.ReAlignToolBtn;
  end;
end;


procedure TToolbarItem.SetButtonStyle(const AValue: TToolbarButtonStyle);
begin
  if FButton.FToolbarButtonStyle<>AValue then
  begin
    FButton.FToolbarButtonStyle:=AValue;
    TToolbarItems(Collection).FToolPanel.ReAlignToolBtn;
  end;
end;

procedure TToolbarItem.SetDropDownMenu(const AValue: TPopupMenu);
begin
  if FButton.FDropDownMenu<>AValue then
  begin
    FButton.FDropDownMenu:=AValue;
    FButton.Invalidate;
  end;
end;

procedure TToolbarItem.SetGroupIndex(const AValue: Integer);
begin
  FButton.GroupIndex:=AValue;
end;
(*
procedure TToolbarItem.SetHeight(const AValue: Integer);
begin
  FButton.Height:=AValue;
end;
*)
procedure TToolbarItem.SetLayout(const AValue: TButtonLayout);
begin
  FButton.Layout:=AValue;
  TToolbarItems(Collection).FToolPanel.ReAlignToolBtn;
end;
(*
procedure TToolbarItem.SetLeft(const AValue: Integer);
begin
  if csLoading in TToolbarItems(Collection).FToolPanel.ComponentState then
    FSaveLeft:=AValue
  else
    FButton.Left:=AValue;
end;
*)
procedure TToolbarItem.SetShowCaption(const AValue: boolean);
begin
  if FButton.ShowCaption<>AValue then
  begin
    FButton.ShowCaption:=AValue;
    if not (csLoading in TToolbarItems(Collection).FToolPanel.ComponentState) then
      TToolbarItems(Collection).FToolPanel.ReAlignToolBtn;
  end;
end;

procedure TToolbarItem.SetTag(const AValue: Longint);
begin
  FButton.Tag:=AValue;
end;
(*
procedure TToolbarItem.SetTop(const AValue: Integer);
begin
  FButton.Top:=AValue;
end;
*)
function TToolbarItem.GetAction: TBasicAction;
begin
  Result:=FButton.Action;
end;

{
function TToolbarItem.GetAutoSize: boolean;
begin
  Result:=FButton.FAutoSize;
end;
}
function TToolbarItem.GetButtonStyle: TToolbarButtonStyle;
begin
  Result:=FButton.FToolbarButtonStyle;
end;

function TToolbarItem.GetDropDownMenu: TPopupMenu;
begin
  Result:=FButton.FDropDownMenu;
end;

function TToolbarItem.GetGroupIndex: Integer;
begin
  Result:=FButton.GroupIndex;
end;

function TToolbarItem.GetLayout: TButtonLayout;
begin
  Result:=FButton.Layout;
end;

function TToolbarItem.GetShowCaption: boolean;
begin
  Result:=FButton.ShowCaption;
end;

function TToolbarItem.GetTag: Longint;
begin
  Result:=FButton.Tag;
end;
(*
function TToolbarItem.GetTop: Integer;
begin
  Result:=FButton.Top;
end;
*)
function TToolbarItem.GetVisible: boolean;
begin
  Result:=FButton.Visible;
end;
(*
function TToolbarItem.GetWidth: Integer;
begin
  Result:=FButton.Width;
end;
*)
procedure TToolbarItem.SetVisible(const AValue: boolean);
begin
  if FButton.Visible<>AValue then
  begin
    FButton.Visible:=AValue;
    FButton.Invalidate;
  end;
end;
(*
procedure TToolbarItem.SetWidth(const AValue: Integer);
begin
  FButton.Width:=AValue;
end;

procedure TToolbarItem.UpdateLeftAfterLoad;
begin
  FButton.Left:=FSaveLeft;
end;
*)

procedure TToolbarItem.InternalCalcSize;
begin
  FIntHeight:=0;
  if ButtonStyle in [tbrSeparator, tbrDivider] then
    FIntWidth:=DefSeparatorWidth
  else
  begin
    FIntWidth:=TToolbarItems(Collection).FToolPanel.FDefImgWidth;

    if ButtonStyle = tbrDropDownExtra then
      FIntWidth:=FIntWidth +  TToolbarItems(Collection).FToolPanel.FInternalDropDownExtraBtnWidth + TToolbarItems(Collection).FToolPanel.FInternalSpacing * 2;

    if ShowCaption then
    begin
      if (Layout in [blGlyphLeft, blGlyphRight])  then
      begin
        FIntWidth:=FIntWidth + TToolbarItems(Collection).FToolPanel.FInternalSpacing * 2 + FTextWidth;
        FIntHeight:=Max(FTextHeight + TToolbarItems(Collection).FToolPanel.FInternalSpacing * 2, TToolbarItems(Collection).FToolPanel.FDefImgHeight);
      end
      else
      begin
        FIntWidth:=Max(FTextWidth + TToolbarItems(Collection).FToolPanel.FInternalSpacing * 2, TToolbarItems(Collection).FToolPanel.FDefImgWidth);
        FIntHeight:=FIntHeight + TToolbarItems(Collection).FToolPanel.FInternalSpacing * 2 + FTextHeight;
      end;
    end
    else
    begin
       FIntWidth:=FIntWidth + (*TToolbarItems(Collection).FToolPanel.FDefImgWidth + *) TToolbarItems(Collection).FToolPanel.FInternalSpacing * 2;
       FIntHeight:=FIntHeight + (*TToolbarItems(Collection).FToolPanel.FDefImgHeight + *) TToolbarItems(Collection).FToolPanel.FInternalSpacing * 2;
    end;
  end;
end;

function TToolbarItem.GetDisplayName: string;
begin
  if ButtonStyle in [tbrSeparator, tbrDivider] then
  begin
    Result:='Separator'
  end
  else
  if Assigned(Action) then
  begin
    if (Action is TCustomAction) then
      Result:=TCustomAction(Action).Name + ' - ' +TCustomAction(Action).Caption
    else
      Result:=TCustomAction(Action).Name;
  end
  else
    Result:=inherited GetDisplayName;
end;

constructor TToolbarItem.Create(ACollection: TCollection);
var
  i, W:integer;
  TB:TToolPanel;
  TI: TToolbarItem;
begin
  inherited Create(ACollection);
  TB:=TToolbarItems(ACollection).FToolPanel;

  TB.DisableAlign;
  FButton:=TToolbarButton.Create(TB);
  FButton.Parent:=TB;
  FButton.Flat:=tpFlatBtns in TToolbarItems(ACollection).FToolPanel.Options;
  FButton.Transparent:=tpTransparentBtns in TToolbarItems(ACollection).FToolPanel.Options;
  FButton.ShowCaption:=false;
  FButton.AutoSize:=true;
  FButton.Align:=alCustom;
  FButton.FOwnerItem:=Self;
  FButton.FFullPush:=true;
  FButton.ParentColor:=true;
  TB.EnableAlign;
end;

destructor TToolbarItem.Destroy;
begin
  if Assigned(FButton) then
  begin
    FButton.FOwnerItem:=nil;
    FreeAndNil(FButton);
  end;
  inherited Destroy;
end;

{ TToolbarButtonActionLink }

procedure TToolbarButtonActionLink.SetImageIndex(Value: Integer);
begin
  inherited;
  FClient.Invalidate;
end;

function TToolbarButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result:=true;
end;

procedure TToolbarButtonActionLink.SetEnabled(Value: Boolean);
begin
  if (FClient as TToolbarButton).FToolbarButtonStyle = tbrDropDown then
    FClient.Enabled:=true
  else
    inherited SetEnabled(Value);
end;

procedure TToolbarButtonActionLink.SetCaption(const Value: string);
begin
  inherited SetCaption(Value);
  TToolbarButton(FClient).ToolPanel.ReAlignToolBtn
end;

initialization
  RegisterPropertyToSkip(TToolbarButton, 'AutoSize', 'Old stile AutoSize in button', '');
  RegisterPropertyToSkip(TToolbarItem, 'AutoSize', 'Old stile AutoSize in button', '');
  RegisterPropertyToSkip(TToolPanel, 'AutoSize', 'Old stile AutoSize in button', '');
  RegisterPropertyToSkip(TToolPanel, 'BtnWidth', 'Use scaling', '');
  RegisterPropertyToSkip(TToolPanel, 'BtnHeight', 'Use scaling', '');
  RegisterPropertyToSkip(TToolbarItem, 'Left', 'Use scaling', '');
  RegisterPropertyToSkip(TToolbarItem, 'Height', 'Use scaling', '');
  RegisterPropertyToSkip(TToolbarItem, 'Top', 'Use scaling', '');
  RegisterPropertyToSkip(TToolbarItem, 'Width', 'Use scaling', '');
end.

