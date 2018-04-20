unit rxPopupNotifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms, Graphics, Controls, StdCtrls;

type
  TRxPopupNotifierItem = class;
  TRxPopupNotifier = class;
  TRxPopupNotifierState = (rpsInactive, rpsMaximazed, rpsShowing, rpsMinimized);
  TRxPopupNotifierEvent = procedure(Sender:TRxPopupNotifier; AItem:TRxPopupNotifierItem) of object;

  { TRxNotifierForm }

  TRxNotifierForm = class(TForm)
  private
    FCloseButton:TButton;
    FCaptionLabel:TLabel;
    FMessageLabel:TLabel;
    FTimerLabel:TLabel;
    FOwnerItem:TRxPopupNotifierItem;
    procedure CreateCloseButton;
    procedure CreateCaption(ACaption:string);
    procedure CreateMessage(AMessage:string);
    procedure CreateTimerLabel;
    procedure ButtonCloseClick(Sender: TObject);
  public
    constructor CreateNotifierForm(AOwnerItem:TRxPopupNotifierItem);
  end;

  { TRxPopupNotifierItem }

  TRxPopupNotifierItem = class(TCollectionItem)
  private
    FActive: boolean;
    FAutoClose: boolean;
    FCaption: string;
    FColor: TColor;
    FMessage: string;
    FNotifyForm:TRxNotifierForm;
    FShowCloseButton: boolean;
    FShowCloseTimer: boolean;
    FCloseTime:TDateTime;
    FState: TRxPopupNotifierState;
    procedure OnNotifyFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetActive(AValue: boolean);
    procedure SetColor(AValue: TColor);
    procedure UpdateCloseLabel;
    procedure CreateNotifierForm;
    procedure UpdateFormSizes(var ATop:integer);
    procedure UpdateFormPosition(var ATop:integer);
    procedure NotifierClick(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ACollection: TCollection); override;
    property State:TRxPopupNotifierState read FState;
  published
    property Active:boolean read FActive write SetActive;
    property Color:TColor read FColor write SetColor default clYellow;
    property AutoClose:boolean read FAutoClose write FAutoClose default true;
    property ShowCloseTimer:boolean read FShowCloseTimer write FShowCloseTimer default true;
    property ShowCloseButton:boolean read FShowCloseButton write FShowCloseButton default true;
    property Caption:string read FCaption write FCaption;
    property Message:string read FMessage write FMessage;
  end;

  { TNotifierCollection }

  TNotifierCollection = class(TOwnedCollection)
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
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
    property DefaultColor:TColor read FDefaultColor write FDefaultColor default clYellow;
    property DefNotifierFormWidth:Cardinal read FDefNotifierFormWidth write FDefNotifierFormWidth default 0;
    property DefNotifierFormHeight:Cardinal read FDefNotifierFormHeight write FDefNotifierFormHeight default 0;
    property CloseInterval:Cardinal read FCloseInterval write FCloseInterval default 5;
    property OnNotifiClick:TRxPopupNotifierEvent read FOnNotifiClick write FOnNotifiClick;
  end;

implementation
uses rxconst;

{ TRxNotifierForm }

procedure TRxNotifierForm.CreateCloseButton;
begin
  begin
    FCloseButton:=TButton.Create(Self);
    FCloseButton.Parent:=Self;
    FCloseButton.Caption:=sClose;
    FCloseButton.AutoSize:=true;
    FCloseButton.BorderSpacing.Around:=6;
    FCloseButton.Left:=Width - FCloseButton.Width;
    FCloseButton.AnchorSideLeft.Control:=nil;
    FCloseButton.AnchorSideRight.Control:=Self;
    FCloseButton.AnchorSideRight.Side:=asrRight;
    FCloseButton.AnchorSideTop.Control:=Self;

    FCloseButton.OnClick:=@ButtonCloseClick;
  end;
end;

procedure TRxNotifierForm.CreateCaption(ACaption: string);
begin
  FCaptionLabel:=TLabel.Create(Self);
  FCaptionLabel.Parent:=Self;
  FCaptionLabel.BorderSpacing.Around:=6;
  FCaptionLabel.Align:=alTop;
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
  FMessageLabel.Align:=alClient;
  FMessageLabel.Caption:=AMessage;
  FMessageLabel.OnClick:=@FOwnerItem.NotifierClick;
end;

procedure TRxNotifierForm.CreateTimerLabel;
begin
  FTimerLabel:=TLabel.Create(Self);
  FTimerLabel.Parent:=Self;
  FTimerLabel.Top:=FCaptionLabel.Height+1;
  FTimerLabel.Align:=alTop;
  FTimerLabel.BorderSpacing.Around:=6;
  FTimerLabel.Font.Style:=FTimerLabel.Font.Style + [fsItalic];
  FTimerLabel.Caption:=' ';
  FTimerLabel.OnClick:=@FOwnerItem.NotifierClick;
end;

procedure TRxNotifierForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TRxNotifierForm.CreateNotifierForm(AOwnerItem: TRxPopupNotifierItem
  );
begin
  inherited CreateNew(Application);
  FOwnerItem:=AOwnerItem;
end;

{ TNotifierCollection }

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

procedure TRxPopupNotifierItem.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  if Assigned(FNotifyForm) then
    FNotifyForm.Color:=FColor;
end;

procedure TRxPopupNotifierItem.UpdateCloseLabel;
var
  D, D1: TDateTime;
  N: Int64;
begin
  if Assigned(FNotifyForm) and FShowCloseTimer then
  begin
    D:=Now;
    if FCloseTime < D then
      FState:=rpsMinimized
    else
    begin
//      D1:=;
      N:=Trunc((FCloseTime - D) * SecsPerDay);
      FNotifyForm.FTimerLabel.Caption:=Format( sCloseAfterSec, [N]);
    end;
  end;
end;

procedure TRxPopupNotifierItem.CreateNotifierForm;
begin
  if Assigned(FNotifyForm) then exit;
  FNotifyForm:=TRxNotifierForm.CreateNotifierForm(Self);
  FNotifyForm.Width:=TRxPopupNotifier(Collection.Owner).NotifierFormWidth;
  FNotifyForm.Height:=1;
  FNotifyForm.Left:=Screen.Width - FNotifyForm.Width - 2;
  FNotifyForm.Top:=Screen.Height - FNotifyForm.Height - 2;
  FNotifyForm.BorderStyle:=bsNone;
  FNotifyForm.FormStyle:=fsStayOnTop;
  FNotifyForm.ShowInTaskBar:=stNever;
  FNotifyForm.Color:=FColor;

  if FShowCloseButton then
    FNotifyForm.CreateCloseButton;

  FNotifyForm.CreateCaption(FCaption);
  if FShowCloseTimer then
    FNotifyForm.CreateTimerLabel;
  FNotifyForm.CreateMessage(FMessage);

  FNotifyForm.OnClose:=@OnNotifyFormClose;
  FNotifyForm.Show;
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
        FNotifyForm.Top:=ATop - FNotifyForm.Height;
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
        FNotifyForm.Top:=ATop - FNotifyForm.Height;
      end
      else
        FState:=rpsInactive;
    end;
    ATop:=ATop - FNotifyForm.Height;
    ATop:=ATop - 2;
  end;
end;

procedure TRxPopupNotifierItem.UpdateFormPosition(var ATop: integer);
begin
  if Assigned(FNotifyForm) then
  begin
    FNotifyForm.Top:=ATop - FNotifyForm.Height;
    ATop:=ATop - FNotifyForm.Height - 2;
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
    TRxPopupNotifierItem(Dest).FShowCloseButton:=FShowCloseButton;
  end
  else
  inherited AssignTo(Dest);
end;

constructor TRxPopupNotifierItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FColor:=TRxPopupNotifier(ACollection.Owner).FDefaultColor;
  FShowCloseButton:=true;
  FShowCloseTimer:=true;
end;

{ TRxPopupNotifier }

procedure TRxPopupNotifier.UpdateNotifyFormsPositoon;
var
  F: TRxPopupNotifierItem;
  Y, i: Integer;
  FReposition: Boolean;
begin
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
        Y:=Y - F.FNotifyForm.Height - 2;
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
    if F.Active and (F.State = rpsShowing) and F.ShowCloseTimer then
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

