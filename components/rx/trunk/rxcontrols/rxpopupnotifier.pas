{ rxPopupNotifier unit

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

unit RxPopupNotifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms, Graphics, Controls, StdCtrls, Buttons, LCLType;

type
  TRxPopupNotifierItem = class;
  TRxPopupNotifier = class;
  TRxPopupNotifierState = (rpsInactive, rpsMaximazed, rpsShowing, rpsMinimized);
  TRxPopupNotifierCorner = (rpcTopLeft, rpcTopRight, rpcBootomLeft, rpcBottomRight);

  TRxPopupNotifierEvent = procedure(Sender:TRxPopupNotifier; AItem:TRxPopupNotifierItem) of object;

  { TRxNotifierForm }

  TRxNotifierForm = class(TCustomForm)
  private
    FCloseButton:TSpeedButton;
    FCaptionLabel:TLabel;
    FMessageLabel:TLabel;
    FTimerLabel:TLabel;
    FOwnerItem:TRxPopupNotifierItem;
    FMessageIcon:TImage;
    procedure CreateCloseButton;
    procedure CreateCaption(ACaption:string);
    procedure CreateMessage(AMessage:string);
    procedure CreateTimerLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure DoUpdateControls;
  protected
  public
    constructor CreateNotifierForm(AOwnerItem:TRxPopupNotifierItem);
  end;

  { TCloseButtonItem }

  TCloseButtonItem = class(TPersistent)
  private
    FOwner: TRxPopupNotifierItem;
    FFlat: Boolean;
    FHint: TTranslateString;
    FVisible: boolean;
    procedure SetFlat(AValue: Boolean);
    procedure SetHint(AValue: TTranslateString);
    procedure SetVisible(AValue: boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner:TRxPopupNotifierItem);
  published
    property Hint:TTranslateString read FHint write SetHint;
    property Flat:Boolean read FFlat write SetFlat;
    property Visible:boolean read FVisible write SetVisible default true;
  end;


  { TRxPopupNotifierItem }

  TRxPopupNotifierItem = class(TCollectionItem)
  private
    FActive: boolean;
    FAlphaBlend: boolean;
    FAlphaBlendValue: Byte;
    FAutoClose: boolean;
    FCaption: string;
    FCloseButton: TCloseButtonItem;
    FColor: TColor;
    FMessage: string;
    FNotifyForm:TRxNotifierForm;
    FShowCloseTimer: boolean;
    FCloseTime:TDateTime;
    FState: TRxPopupNotifierState;
    function GetShowCloseButton: boolean;
    procedure OnNotifyFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetActive(AValue: boolean);
    procedure SetAlphaBlend(AValue: boolean);
    procedure SetAlphaBlendValue(AValue: Byte);
    procedure SetCloseButton(AValue: TCloseButtonItem);
    procedure SetColor(AValue: TColor);
    procedure SetShowCloseButton(AValue: boolean);
    procedure SetShowCloseTimer(AValue: boolean);
    procedure UpdateCloseLabel;
    procedure CreateNotifierForm;
    procedure UpdateFormSizes(var ATop:integer);
    procedure UpdateFormPosition(var ATop:integer);
    procedure NotifierClick(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property State:TRxPopupNotifierState read FState;
  published
    property AlphaBlend:boolean read FAlphaBlend write SetAlphaBlend default false;
    property AlphaBlendValue:Byte read FAlphaBlendValue write SetAlphaBlendValue default 255;
    property Active:boolean read FActive write SetActive;
    property Color:TColor read FColor write SetColor default clYellow;
    property AutoClose:boolean read FAutoClose write FAutoClose default true;
    property ShowCloseTimer:boolean read FShowCloseTimer write SetShowCloseTimer default true;
    property ShowCloseButton:boolean read GetShowCloseButton write SetShowCloseButton default true;
    property Caption:string read FCaption write FCaption;
    property Message:string read FMessage write FMessage;
    property CloseButton:TCloseButtonItem read FCloseButton write SetCloseButton;
  end;

  { TNotifierCollection }

  TNotifierCollection = class(TOwnedCollection)
  private
    function GetItems(AIndex: Integer): TRxPopupNotifierItem;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    property Items[AIndex:Integer]:TRxPopupNotifierItem read GetItems; default;
  end;

  { TRxPopupNotifier }

  TRxPopupNotifier = class(TComponent)
  private
    FActive: boolean;
    FCloseInterval: Cardinal;
    FDefaultColor: TColor;
    FDefNotifierFormHeight: Cardinal;
    FDefNotifierFormWidth: Cardinal;
    FItems: TNotifierCollection;
    FMessageCorner: TRxPopupNotifierCorner;
    FOnNotifiClick: TRxPopupNotifierEvent;
    FTimer:TTimer;
    procedure SetActive(AValue: boolean);
    procedure SetItems(AValue: TNotifierCollection);
    procedure UpdateNotifyFormsPositoon;
    procedure UpdateTimeState;
    procedure UpdateClosed;
    procedure NotifyTimerEvent(Sender: TObject);
    procedure DoNotifiClick(AItem:TRxPopupNotifierItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddNotifyItem(ACaption, AMessage:string):TRxPopupNotifierItem;
    procedure Clear;
    function NotifierFormWidth:Cardinal;
    function NotifierFormHeight:Cardinal;
  published
    property Active:boolean read FActive write SetActive default True;
    property Items:TNotifierCollection read FItems write SetItems;
    property MessageCorner:TRxPopupNotifierCorner read FMessageCorner write FMessageCorner default rpcBottomRight;
    property DefaultColor:TColor read FDefaultColor write FDefaultColor default clYellow;
    property DefNotifierFormWidth:Cardinal read FDefNotifierFormWidth write FDefNotifierFormWidth default 0;
    property DefNotifierFormHeight:Cardinal read FDefNotifierFormHeight write FDefNotifierFormHeight default 0;
    property CloseInterval:Cardinal read FCloseInterval write FCloseInterval default 5;
    property OnNotifiClick:TRxPopupNotifierEvent read FOnNotifiClick write FOnNotifiClick;
  end;

implementation
uses rxconst, rxlclutils;

{ TCloseButtonItem }

procedure TCloseButtonItem.SetFlat(AValue: Boolean);
begin
  if FFlat=AValue then Exit;
  FFlat:=AValue;
  FOwner.Changed(false);
end;

procedure TCloseButtonItem.SetHint(AValue: TTranslateString);
begin
  if FHint=AValue then Exit;
  FHint:=AValue;
  FOwner.Changed(false);
end;

procedure TCloseButtonItem.SetVisible(AValue: boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  FOwner.Changed(false);
end;

procedure TCloseButtonItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TCloseButtonItem then
  begin
    TCloseButtonItem(Dest).FFlat:=FFlat;
    TCloseButtonItem(Dest).FHint:=FHint;
    TCloseButtonItem(Dest).FVisible:=FVisible;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TCloseButtonItem.Create(AOwner: TRxPopupNotifierItem);
begin
  inherited Create;
  FOwner:=AOwner;
  FFlat:=true;
  FHint:=sCloseMessageHint;
  FVisible:=true;
end;

{ TRxNotifierForm }

procedure TRxNotifierForm.CreateCloseButton;
var
  D: TBitmap;
begin
  FCloseButton:=TSpeedButton.Create(Self);
  FCloseButton.Parent:=Self;
  FCloseButton.BorderSpacing.Around:=6;

  FCloseButton.Width:=26;
  FCloseButton.Height:=26;
  D:=LoadLazResBitmapImage('RxMDICloseIcon');
  FCloseButton.Glyph:=D;
  D.Free;
  FCloseButton.Hint:=FOwnerItem.CloseButton.Hint;
  FCloseButton.Flat:=FOwnerItem.CloseButton.Flat;

  FCloseButton.Anchors:=[akTop, akRight];
  FCloseButton.AnchorSideRight.Control:=Self;
  FCloseButton.AnchorSideRight.Side:=asrRight;
  FCloseButton.AnchorSideTop.Control:=Self;

  FCloseButton.OnClick:=@ButtonCloseClick;
end;

procedure TRxNotifierForm.CreateCaption(ACaption: string);
begin
  FCaptionLabel:=TLabel.Create(Self);
  FCaptionLabel.Parent:=Self;
  FCaptionLabel.AutoSize:=true;

  FCaptionLabel.BorderSpacing.Around:=6;
  FCaptionLabel.Anchors:=[akTop, akLeft, akRight];
  FCaptionLabel.AnchorSideTop.Control:=Self;
  FCaptionLabel.AnchorSideLeft.Control:=Self;
  if Assigned(FCloseButton) then
  begin
    FCaptionLabel.AnchorSideRight.Control:=FCloseButton;
    FCaptionLabel.AnchorSideRight.Side:=asrLeft;
  end
  else
  begin
    FCaptionLabel.AnchorSideRight.Control:=Self;
    FCaptionLabel.AnchorSideRight.Side:=asrRight;
  end;

  FCaptionLabel.Caption:=ACaption;
  FCaptionLabel.Font.Style:=FCaptionLabel.Font.Style + [fsBold];
  FCaptionLabel.OnClick:=@FOwnerItem.NotifierClick;
end;

procedure TRxNotifierForm.CreateMessage(AMessage: string);
begin
  FMessageLabel:=TLabel.Create(Self);
  FMessageLabel.Parent:=Self;
  FMessageLabel.WordWrap:=true;
  FMessageLabel.BorderSpacing.Around:=6;

  FMessageLabel.Anchors:=[akTop, akLeft, akRight, akBottom];

  FMessageLabel.AnchorSideTop.Side:=asrBottom;

  FMessageLabel.AnchorSideLeft.Control:=Self;
  FMessageLabel.AnchorSideRight.Control:=Self;
  FMessageLabel.AnchorSideRight.Side:=asrRight;
  FMessageLabel.AnchorSideBottom.Control:=Self;
  FMessageLabel.AnchorSideBottom.Side:=asrBottom;


  FMessageLabel.Caption:=AMessage;
  FMessageLabel.OnClick:=@FOwnerItem.NotifierClick;

  DoUpdateControls;
end;

procedure TRxNotifierForm.CreateTimerLabel;
begin
  FTimerLabel:=TLabel.Create(Self);
  FTimerLabel.Parent:=Self;
  FTimerLabel.AutoSize:=true;

  FTimerLabel.BorderSpacing.Around:=6;
  FTimerLabel.Anchors:=[akTop, akLeft, akRight];

  FTimerLabel.AnchorSideTop.Control:=FCaptionLabel;
  FTimerLabel.AnchorSideTop.Side:=asrBottom;

  FTimerLabel.AnchorSideLeft.Control:=Self;
  FTimerLabel.AnchorSideLeft.Side:=asrLeft;

  if Assigned(FCloseButton) then
  begin
    FTimerLabel.AnchorSideRight.Control:=FCloseButton;
    FTimerLabel.AnchorSideRight.Side:=asrLeft;
  end
  else
  begin
    FTimerLabel.AnchorSideRight.Control:=Self;
    FTimerLabel.AnchorSideRight.Side:=asrRight;
  end;

  FTimerLabel.Visible:=FOwnerItem.ShowCloseTimer;
  FTimerLabel.Font.Style:=FTimerLabel.Font.Style + [fsItalic];
  FTimerLabel.Caption:=' ';
  FTimerLabel.OnClick:=@FOwnerItem.NotifierClick;
end;

procedure TRxNotifierForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRxNotifierForm.DoUpdateControls;
begin
  if FTimerLabel.Visible then
    FMessageLabel.AnchorSideTop.Control:=FTimerLabel
  else
    FMessageLabel.AnchorSideTop.Control:=FCaptionLabel;
end;

constructor TRxNotifierForm.CreateNotifierForm(AOwnerItem: TRxPopupNotifierItem
  );
begin
  inherited CreateNew(Application);
  FOwnerItem:=AOwnerItem;
  ShowHint:=true;
end;

{ TNotifierCollection }

function TNotifierCollection.GetItems(AIndex: Integer): TRxPopupNotifierItem;
begin
  Result:=TRxPopupNotifierItem(GetItem(AIndex));
end;

procedure TNotifierCollection.Update(Item: TCollectionItem);
var
  FActive: Boolean;
  i: Integer;
begin
  inherited Update(Item);

  FActive:=false;
  for i:=0 to Count-1 do
    if TRxPopupNotifierItem(Items[i]).Active then
    begin
      FActive:=true;
      Break;
    end;

  TRxPopupNotifier(Owner).FTimer.Enabled:=TRxPopupNotifier(Owner).FActive and FActive;
end;

constructor TNotifierCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRxPopupNotifierItem);
end;

{ TRxPopupNotifierItem }

procedure TRxPopupNotifierItem.OnNotifyFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  FNotifyForm:=nil;
  FState:=rpsInactive;
end;

function TRxPopupNotifierItem.GetShowCloseButton: boolean;
begin
  Result:=FCloseButton.Visible;
end;

procedure TRxPopupNotifierItem.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;

  if not AValue then
  begin
    FState:=rpsInactive;
    if Assigned(FNotifyForm) then
      FNotifyForm.Close;
  end
  else
  begin
    CreateNotifierForm;
    FState:=rpsMaximazed;
  end;

  Changed(false);
end;

procedure TRxPopupNotifierItem.SetAlphaBlend(AValue: boolean);
begin
  if FAlphaBlend=AValue then Exit;
  FAlphaBlend:=AValue;
  if Assigned(FNotifyForm) then
    FNotifyForm.AlphaBlend:=FAlphaBlend;
end;

procedure TRxPopupNotifierItem.SetAlphaBlendValue(AValue: Byte);
begin
  if FAlphaBlendValue=AValue then Exit;
  FAlphaBlendValue:=AValue;
  if Assigned(FNotifyForm) then
    FNotifyForm.AlphaBlendValue:=FAlphaBlendValue;
end;

procedure TRxPopupNotifierItem.SetCloseButton(AValue: TCloseButtonItem);
begin
  FCloseButton.Assign(AValue);
end;

procedure TRxPopupNotifierItem.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  if Assigned(FNotifyForm) then
    FNotifyForm.Color:=FColor;
end;

procedure TRxPopupNotifierItem.SetShowCloseButton(AValue: boolean);
begin
  FCloseButton.Visible:=AValue;
end;

procedure TRxPopupNotifierItem.SetShowCloseTimer(AValue: boolean);
begin
  if FShowCloseTimer=AValue then Exit;
  FShowCloseTimer:=AValue;
  if Assigned(FNotifyForm) then
  begin
    FNotifyForm.FTimerLabel.Visible:=AValue;
    FNotifyForm.DoUpdateControls;
  end;
end;

procedure TRxPopupNotifierItem.UpdateCloseLabel;
var
  D: TDateTime;
  N: Int64;
begin
  if Assigned(FNotifyForm) and FAutoClose then
  begin
    D:=Now;
    if FCloseTime < D then
      FState:=rpsMinimized
    else
    if FShowCloseTimer then
    begin
      N:=Trunc((FCloseTime - D) * SecsPerDay);
      FNotifyForm.FTimerLabel.Caption:=Format( sCloseAfterSec, [N]);
    end;
  end;
end;

procedure TRxPopupNotifierItem.CreateNotifierForm;
var
  FSaveActiveForm: TForm;
begin
  if Assigned(FNotifyForm) then exit;
  FSaveActiveForm:=Screen.ActiveForm;
  FNotifyForm:=TRxNotifierForm.CreateNotifierForm(Self);
  FNotifyForm.Width:=TRxPopupNotifier(Collection.Owner).NotifierFormWidth;
  FNotifyForm.Height:=1;

  case TRxPopupNotifier(Collection.Owner).FMessageCorner of
    rpcTopLeft:
      begin
        //TODO : доделать
        FNotifyForm.Left := 2;
        FNotifyForm.Top := 2;
      end;
    rpcTopRight:
      begin
        //TODO : доделать
        FNotifyForm.Left := Screen.Width - FNotifyForm.Width - 2;
        FNotifyForm.Top := 2;
      end;
    rpcBootomLeft:
      begin
        //TODO : доделать
        FNotifyForm.Left := 2;
        FNotifyForm.Top := Screen.Height - FNotifyForm.Height - 2;
      end;
    rpcBottomRight:
      begin
        FNotifyForm.Left:=Screen.Width - FNotifyForm.Width - 2;
        FNotifyForm.Top:=Screen.Height - FNotifyForm.Height - 2;
      end;
  end;

  FNotifyForm.BorderStyle:=bsNone;
  FNotifyForm.FormStyle:=fsStayOnTop;
  FNotifyForm.ShowInTaskBar:=stNever;
  FNotifyForm.Color:=FColor;
  FNotifyForm.AlphaBlend:=FAlphaBlend;
  FNotifyForm.AlphaBlendValue:=FAlphaBlendValue;

  if FCloseButton.Visible then
    FNotifyForm.CreateCloseButton;

  FNotifyForm.CreateCaption(FCaption);
  FNotifyForm.CreateTimerLabel;
  FNotifyForm.CreateMessage(FMessage);

  FNotifyForm.OnClose:=@OnNotifyFormClose;
  FNotifyForm.Show;



  if Assigned(FSaveActiveForm) then
    FSaveActiveForm.BringToFront;
end;

procedure TRxPopupNotifierItem.UpdateFormSizes(var ATop: integer);
begin
  if Assigned(FNotifyForm) then
  begin
    if (FState = rpsMaximazed) then
    begin
      if (TRxPopupNotifier(Collection.Owner).NotifierFormHeight > FNotifyForm.Height) then
      begin
        FNotifyForm.Height:=FNotifyForm.Height + 1;
        case TRxPopupNotifier(Collection.Owner).FMessageCorner of
          //rpcTopLeft:;
          //rpcTopRight:;
          rpcBootomLeft:FNotifyForm.Top:=ATop - FNotifyForm.Height;
          rpcBottomRight:FNotifyForm.Top:=ATop - FNotifyForm.Height;
        end;
      end
      else
      begin
        FState:=rpsShowing;
        FCloseTime:=Now + TRxPopupNotifier(Collection.Owner).FCloseInterval / SecsPerDay;
      end;
    end
    else
    if (FState = rpsMinimized) then
    begin
      if (FNotifyForm.Height > 1) then
      begin
        FNotifyForm.Height:=FNotifyForm.Height - 1;
        case TRxPopupNotifier(Collection.Owner).FMessageCorner of
          //rpcTopLeft:;
          //rpcTopRight:;
          rpcBootomLeft:FNotifyForm.Top:=ATop - FNotifyForm.Height;
          rpcBottomRight:FNotifyForm.Top:=ATop - FNotifyForm.Height;
        end;
      end
      else
        FState:=rpsInactive;
    end;

    if TRxPopupNotifier(Collection.Owner).FMessageCorner in [rpcTopLeft, rpcTopRight] then
      ATop:=ATop + FNotifyForm.Height + 2
    else
      ATop:=ATop - FNotifyForm.Height - 2;
  end;
end;

procedure TRxPopupNotifierItem.UpdateFormPosition(var ATop: integer);
begin
  if Assigned(FNotifyForm) then
  begin
    case TRxPopupNotifier(Collection.Owner).FMessageCorner of
      rpcTopLeft,
      rpcTopRight:
        begin
          FNotifyForm.Top:=ATop;
          ATop:=ATop + FNotifyForm.Height + 2;
        end;
      rpcBootomLeft,
      rpcBottomRight:
        begin
          FNotifyForm.Top:=ATop - FNotifyForm.Height;
          ATop:=ATop - FNotifyForm.Height - 2;
        end;
    end;
  end;
end;

procedure TRxPopupNotifierItem.NotifierClick(Sender: TObject);
begin
  TRxPopupNotifier(Collection.Owner).DoNotifiClick(Self);
end;

procedure TRxPopupNotifierItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TRxPopupNotifierItem then
  begin
    TRxPopupNotifierItem(Dest).FColor:=FColor;
    TRxPopupNotifierItem(Dest).FAutoClose:=FAutoClose;
    TRxPopupNotifierItem(Dest).FShowCloseTimer:=FShowCloseTimer;
    TRxPopupNotifierItem(Dest).FCaption:=FCaption;
    TRxPopupNotifierItem(Dest).FMessage:=FMessage;
    TRxPopupNotifierItem(Dest).CloseButton:=CloseButton;
  end
  else
  inherited AssignTo(Dest);
end;

constructor TRxPopupNotifierItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCloseButton:=TCloseButtonItem.Create(Self);
  FColor:=TRxPopupNotifier(ACollection.Owner).FDefaultColor;
  FShowCloseTimer:=true;
  FAutoClose:=true;
  FAlphaBlendValue:=255;
  FAlphaBlend:=false;
end;

destructor TRxPopupNotifierItem.Destroy;
begin
  FreeAndNil(FCloseButton);
  inherited Destroy;
end;

{ TRxPopupNotifier }

procedure TRxPopupNotifier.UpdateNotifyFormsPositoon;
var
  F: TRxPopupNotifierItem;
  Y, i: Integer;
  FReposition: Boolean;
begin
  if FMessageCorner in [rpcTopLeft, rpcTopRight] then
    Y:=2
  else
    Y:=Screen.Height - 2;

  FReposition:=false;
  for i:=FItems.Count - 1 downto 0 do
  begin
    F:=FItems.Items[i] as TRxPopupNotifierItem;
    if F.Active then
    begin
      if F.FState in [rpsMaximazed, rpsMinimized] then
      begin
        F.UpdateFormSizes(Y);
        FReposition:=true;
      end
      else
      if F.FState = rpsInactive then
        FReposition:=true
      else
      if FReposition then
        F.UpdateFormPosition(Y)
      else
      begin
        if FMessageCorner in [rpcTopLeft, rpcTopRight] then
          Y:=Y + F.FNotifyForm.Height + 2
        else
          Y:=Y - F.FNotifyForm.Height - 2;
      end;

      if Y<0 then
        Y:=2
      else
      if Y>Screen.Height then
        Y:=Screen.Height - 2;
    end;
  end;
end;

procedure TRxPopupNotifier.UpdateTimeState;
var
  i: Integer;
  F: TRxPopupNotifierItem;
begin
  for i:=FItems.Count - 1 downto 0 do
  begin
    F:=FItems.Items[i] as TRxPopupNotifierItem;
    if F.Active and (F.State = rpsShowing) and F.AutoClose then
      F.UpdateCloseLabel;
  end;
end;

procedure TRxPopupNotifier.UpdateClosed;
var
  F: TRxPopupNotifierItem;
  i: Integer;
begin
  for i:=FItems.Count - 1 downto 0 do
  begin
    F:=FItems.Items[i] as TRxPopupNotifierItem;
    if F.FState = rpsInactive then
      F.Active:=false;
  end;
end;

procedure TRxPopupNotifier.SetItems(AValue: TNotifierCollection);
begin
  FItems.Assign(AValue);
end;

procedure TRxPopupNotifier.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  FTimer.Enabled:=false;
  if not FActive then
    Clear;
end;

procedure TRxPopupNotifier.NotifyTimerEvent(Sender: TObject);
begin
  UpdateNotifyFormsPositoon;
  UpdateClosed;
  UpdateTimeState;
end;

procedure TRxPopupNotifier.DoNotifiClick(AItem: TRxPopupNotifierItem);
begin
  if Assigned(FOnNotifiClick) then
    FOnNotifiClick(Self, AItem)
end;

constructor TRxPopupNotifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultColor:=clYellow;
  FCloseInterval:=5;
  FMessageCorner:=rpcBottomRight;
  FActive:=true;
  FItems:=TNotifierCollection.Create(Self);
  FTimer:=TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.Interval:=10;
  FTimer.OnTimer:=@NotifyTimerEvent;
end;

destructor TRxPopupNotifier.Destroy;
begin
  FTimer.Enabled:=false;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TRxPopupNotifier.AddNotifyItem(ACaption, AMessage: string
  ): TRxPopupNotifierItem;
begin
  Result:=FItems.Add as TRxPopupNotifierItem;
  Result.Caption:=ACaption;
  Result.Message:=AMessage;
  Result.FState:=rpsMaximazed;
  Result.FColor:=FDefaultColor;
  Result.Active:=true;
end;

procedure TRxPopupNotifier.Clear;
begin

end;

function TRxPopupNotifier.NotifierFormWidth: Cardinal;
begin
  if FDefNotifierFormWidth > 0 then
    Result:=FDefNotifierFormWidth
  else
    Result:=Screen.Width div 4;
end;

function TRxPopupNotifier.NotifierFormHeight: Cardinal;
begin
  if FDefNotifierFormHeight > 0 then
    Result:=FDefNotifierFormHeight
  else
    Result:=Screen.Height div 8;
end;

end.

