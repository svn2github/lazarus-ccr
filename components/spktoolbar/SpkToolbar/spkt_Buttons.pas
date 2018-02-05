unit spkt_Buttons;

{$mode delphi}
{.$Define EnhancedRecordSupport}

(*******************************************************************************
*                                                                              *
*  File:        spkt_Buttons.pas                                               *
*  Description: A module containing button components for the toolbar.         *
*  Copyright:   (c) 2009 by Spook.                                             *
*  License:     Modified LGPL (with linking exception, like Lazarus LCL)       *
'               See "license.txt" in this installation                         *
*                                                                              *
*******************************************************************************)

interface

uses
  Graphics, Classes, Types, Controls, Menus, ActnList, Math,
  Dialogs, ImgList, Forms,
  SpkGUITools, SpkGraphTools, SpkMath,
  spkt_Const, spkt_BaseItem, spkt_Exceptions, spkt_Tools;

type
  TSpkMouseButtonElement = (beNone, beButton, beDropdown);

  TSpkButtonKind = (bkButton, bkButtonDropdown, bkDropdown, bkToggle);

  TSpkBaseButton = class;

  TSpkButtonActionLink = class(TActionLink)
  protected
    FClient: TSpkBaseButton;
    procedure AssignClient(AClient: TObject); override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetImageIndex(Value: integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  public
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
  end;


  { TSpkBaseButton }

  TSpkBaseButton = class abstract(TSpkBaseItem)
  private
    FMouseHoverElement: TSpkMouseButtonElement;
    FMouseActiveElement: TSpkMouseButtonElement;

    // Getters and Setters
    function GetAction: TBasicAction;
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetButtonKind(const Value: TSpkButtonKind);
    procedure SetCaption(const Value: string);
    procedure SetDropdownMenu(const Value: TPopupMenu);
    procedure SetGroupIndex(const Value: Integer);

  protected
    FCaption: string;
    FOnClick: TNotifyEvent;
    FActionLink: TSpkButtonActionLink;
    FButtonState: TSpkButtonState;
    FButtonRect: T2DIntRect;
    FDropdownRect: T2DIntRect;
    FButtonKind: TSpkButtonKind;
    FChecked: Boolean;
    FGroupIndex: Integer;
    FAllowAllUp: Boolean;
    FDropdownMenu: TPopupMenu;

    // *** Drawing support ***
    // The task of the method in inherited classes is to calculate the
    // button's rectangle and the dropdown menu depending on FButtonState
    procedure CalcRects; virtual; abstract;
    function GetDropdownPoint: T2DIntPoint; virtual; abstract;

    // *** Action support ***
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); virtual;
    procedure Click; virtual;
    procedure DoActionChange(Sender: TObject);
    function GetDefaultCaption: String; virtual;

    function SiblingsChecked: Boolean; virtual;
    procedure UncheckSiblings; virtual;

    procedure DrawDropdownArrow(ABuffer: TBitmap; ARect: TRect; AColor: TColor);

    // Getters and Setters
    function GetChecked: Boolean; virtual;
    procedure SetAction(const Value: TBasicAction); virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    procedure SetEnabled(const Value: boolean); override;
    procedure SetRect(const Value: T2DIntRect); override;

    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
    property ButtonKind: TSpkButtonKind read FButtonKind write SetButtonKind;
    property Checked: Boolean read GetChecked write SetChecked default false;
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    function GetRootComponent: TComponent;

  published
    property Action: TBasicAction read GetAction write SetAction;
    property Caption: string read FCaption write SetCaption;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;


  { TSpkLargeButton }

  TSpkLargeButton = class(TSpkBaseButton)
  private
    FLargeImageIndex: TImageIndex;
    procedure FindBreakPlace(s: string; out Position: integer; out Width: integer);
    procedure SetLargeImageIndex(const Value: TImageIndex);
  protected
    procedure CalcRects; override;
    function GetDropdownPoint : T2DIntPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Draw(ABuffer: TBitmap; ClipRect: T2DIntRect); override;
    function GetGroupBehaviour: TSpkItemGroupBehaviour; override;
    function GetSize: TSpkItemSize; override;
    function GetTableBehaviour: TSpkItemTableBehaviour; override;
    function GetWidth: integer; override;
  published
    property LargeImageIndex: TImageIndex read FLargeImageIndex write SetLargeImageIndex default -1;
    property AllowAllUp;
    property ButtonKind;
    property Checked;
    property DropdownMenu;
    property GroupIndex;
  end;


  { TSpkSmallButton }

  TSpkSmallButton = class(TSpkBaseButton)
  private
    FImageIndex: TImageIndex;
    FTableBehaviour: TSpkItemTableBehaviour;
    FGroupBehaviour: TSPkItemGroupBehaviour;
    FHideFrameWhenIdle: boolean;
    FShowCaption: boolean;
    procedure ConstructRects(out BtnRect, DropRect: T2DIntRect);
    procedure SetGroupBehaviour(const Value: TSpkItemGroupBehaviour);
    procedure SetHideFrameWhenIdle(const Value: boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetShowCaption(const Value: boolean);
    procedure SetTableBehaviour(const Value: TSpkItemTableBehaviour);
  protected
    procedure CalcRects; override;
    function GetDropdownPoint: T2DIntPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Draw(ABuffer: TBitmap; ClipRect: T2DIntRect); override;
    function GetGroupBehaviour: TSpkItemGroupBehaviour; override;
    function GetSize: TSpkItemSize; override;
    function GetTableBehaviour: TSpkItemTableBehaviour; override;
    function GetWidth: integer; override;
  published
    property GroupBehaviour: TSpkItemGroupBehaviour read FGroupBehaviour write SetGroupBehaviour;
    property HideFrameWhenIdle: boolean read FHideFrameWhenIdle write SetHideFrameWhenIdle;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ShowCaption: boolean read FShowCaption write SetShowCaption;
    property TableBehaviour: TSpkItemTableBehaviour read FTableBehaviour write SetTableBehaviour;
    property AllowAllUp;
    property ButtonKind;
    property Checked;
    property DropdownMenu;
    property GroupIndex;
  end;


implementation

uses
  LCLType, LCLIntf, LCLProc, SysUtils,
  spkt_Pane, spkt_Appearance;


{ TSpkButtonActionLink }

procedure TSpkButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := TSpkBaseButton(AClient);
end;

function TSpkButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and Assigned(FClient) and
            (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TSpkButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and Assigned(FClient) and
            (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TSpkButtonActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and Assigned(FClient) and
            (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TSpkButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := inherited IsGroupIndexLinked and Assigned(FClient) and
            (FClient.GroupIndex = (Action as TCustomAction).GroupIndex);
end;

function TSpkButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked;
  if (FClient is TSpkSmallButton) then
    Result := Result and (TSpkSmallButton(FClient).ImageIndex = (Action as TCustomAction).ImageIndex)
  else
  if (FClient is TSpkLargeButton) then
    Result := Result and (TSpkLargeButton(FClient).LargeImageIndex = (Action as TCustomAction).ImageIndex)
  else
    Result := false;
end;

function TSpkButtonActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked;
  //and
  //          (@TSpkBaseButton(FClient).OnClick = @Action.OnExecute);
end;

function TSpkButtonActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and Assigned(FClient) and
            (FClient.Visible = (Action as TCustomAction).Visible);
end;

procedure TSpkButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;

procedure TSpkButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    FClient.Checked := Value;
end;

procedure TSpkButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TSpkButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    FClient.GroupIndex := Value;
end;

procedure TSpkButtonActionLink.SetImageIndex(Value: integer);
begin
  if IsImageIndexLinked then begin
    if (FClient is TSpkSmallButton) then
      (TSpkSmallButton(FClient)).ImageIndex := Value
    else
    if (FClient is TSpkLargeButton) then
      (TSpkLargeButton(FClient)).LargeImageIndex := Value;
  end;
end;

procedure TSpkButtonActionLink.SetOnExecute(Value: TNotifyEvent);
begin
// Note: formerly this changed FClient.OnClick, but that is unneeded, because
//       TControl.Click executes Action
end;

procedure TSpkButtonActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;


{ TSpkBaseButton }

constructor TSpkBaseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := GetDefaultCaption;
  FButtonState := bsIdle;
  FButtonKind := bkButton;
  {$IFDEF EnhancedRecordSupport}
  FButtonRect := T2DIntRect.Create(0, 0, 1, 1);
  FDropdownRect := T2DIntRect.Create(0, 0, 1, 1);
  {$ELSE}
  FButtonRect.Create(0, 0, 1, 1);
  FDropdownRect.Create(0, 0, 1, 1);
  {$ENDIF}
  FMouseHoverElement := beNone;
  FMouseActiveElement := beNone;
end;

destructor TSpkBaseButton.Destroy;
begin
  FreeAndNil(FActionLink);
  inherited Destroy;
end;

procedure TSpkBaseButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var
  newAction: TCustomAction;
begin
  if Sender is TCustomAction then begin
    newAction := TCustomAction(Sender);
    if (not CheckDefaults) or (Caption = '') or (Caption = Name) then
      Caption := newAction.Caption;
    if not CheckDefaults or Enabled then
      Enabled := newAction.Enabled;

    {  wp: !!! Hints not yet supported !!!

    if not CheckDefaults or (Hint = '') then
      Hint := newAction.Hint;
    }

    if not CheckDefaults or Visible then
      Visible := newAction.Visible;
    if not CheckDefaults or Checked then
      Checked := newAction.Checked;
    if not CheckDefaults or (GroupIndex > 0) then
      GroupIndex := newAction.GroupIndex;

    {   !!! wp: Actions don't have an AllowAllUp property !!!

    if not CheckDefaults or not AllowAllUp then
      AllowAllUp := newAction.AllowAllUp;
    }

    if self is TSpkSmallButton then begin
      if not CheckDefaults or (TSpkSmallButton(self).ImageIndex < 0) then
        TSpkSmallButton(self).ImageIndex := newAction.ImageIndex;
    end;
    if self is TSpkLargeButton then begin
      if not CheckDefaults or (TSpkLargeButton(self).LargeImageIndex < 0) then
        TSpkLargeButton(self).LargeImageIndex := newAction.ImageIndex;
    end;

    { wp: !!! Helpcontext not yet supported !!!

    if not CheckDefaults or (Self.HelpContext = 0) then
      Self.HelpContext := HelpContext;
    if not CheckDefaults or (Self.HelpKeyword = '') then
      Self.HelpKeyword := HelpKeyword;
    // HelpType is set implicitly when assigning HelpContext or HelpKeyword
    }
  end;
end;

(*   wp: Thid is the old part (before avoiding OnExecute = OnClick) - just for reference.

    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') or (Self.Caption = GetDefaultCaption) then
         Self.Caption := Caption;
      if not CheckDefaults or Self.Enabled then
         Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Visible = True) then
         Self.Visible := Visible;
      if not CheckDefaults or Self.Checked then
        Self.Checked := Checked;
      if not CheckDefaults or (Self.GroupIndex > 0) then
        Self.GroupIndex := GroupIndex;
      if not CheckDefaults or not Self.AllowAllUp then
        Self.AllowAllUp := AllowAllUp;
{
      if not CheckDefaults or not Assigned(Self.OnClick) then
         Self.OnClick := OnExecute;
}
      if self is TSpkSmallButton then begin
        if not CheckDefaults or (TSpkSmallButton(self).ImageIndex < 0) then
          TSpkSmallButton(self).ImageIndex := ImageIndex;
      end;
      if self is TSpkLargeButton then begin
        if not CheckDefaults or (TSpkLargeButton(self).LargeImageIndex < 0) then
          TSpkLargeButton(Self).LargeImageIndex := ImageIndex;
      end;
    end;
end;
*)

procedure TSpkBaseButton.Click;
begin
  // first call our own OnClick
  if Assigned(FOnClick) then
    FOnClick(Self);
  // then trigger the Action
  if (not (csDesigning in ComponentState)) and (FActionLink <> nil) then
    FActionLink.Execute(Self);
end;

procedure TSpkBaseButton.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChange(Sender, False);
end;

procedure TSpkBaseButton.DrawDropdownArrow(ABuffer: TBitmap; ARect: TRect;
  AColor: TColor);
const
  w = 8;
  h = 8;
var
  P: array[0..3] of TPoint;
  wsc, hsc: Integer;
begin
  wsc := ScaleX(w, DesignDPI);                 // 0   1
  hsc := ScaleY(h, DesignDPI);                 //   2
  P[2].x := ARect.Left + (ARect.Right - ARect.Left) div 2;
  P[2].y := ARect.Top + (ARect.Bottom - ARect.Top + hsc) div 2 - 1;
  P[0] := Point(P[2].x - wsc div 2, P[2].y - hsc div 2);
  P[1] := Point(P[2].x + wsc div 2, P[0].y);
  P[3] := P[0];
  ABuffer.Canvas.Brush.Color := AColor;
  ABuffer.Canvas.Pen.Style := psClear;
  ABuffer.Canvas.Polygon(P);
end;

function TSpkBaseButton.GetAction: TBasicAction;
begin
  if Assigned(FActionLink) then
    Result := FActionLink.Action
  else
    Result := nil;
end;

function TSpkBaseButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TSpkBaseButton.GetDefaultCaption: String;
begin
  Result := 'Button';
end;

function TSpkBaseButton.GetRootComponent: TComponent;
var
  pane: TSpkBaseItem;
  tab: TSpkBaseItem;
begin
  result := nil;
  if Collection <> nil then
    pane := TSpkBaseItem(Collection.RootComponent)
  else
    exit;
  if (pane <> nil) and (pane.Collection <> nil) then
    tab := TSpkBaseItem(pane.Collection.RootComponent)
  else
    exit;
  if (tab <> nil) and (tab.Collection <> nil) then
    result := tab.Collection.RootComponent;
end;

procedure TSpkBaseButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FEnabled then
  begin
    // The buttons react only to the left mouse button
    if Button <> mbLeft then
      exit;

    if (FButtonKind = bkToggle) and ((Action = nil) or
       ((Action is TCustomAction) and not TCustomAction(Action).AutoCheck))
    then
      Checked := not Checked;

    if FMouseActiveElement = beButton then
    begin
      if FButtonState <> bsBtnPressed then
      begin
        FButtonState := bsBtnPressed;
        if Assigned(FToolbarDispatch) then
          FToolbarDispatch.NotifyVisualsChanged;
      end;
    end else
    if FMouseActiveElement = beDropdown then
    begin
      if FButtonState <> bsDropdownPressed then
      begin
        FButtonState := bsDropdownPressed;
        if Assigned(FToolbarDispatch) then
          FToolbarDispatch.NotifyVisualsChanged;
      end;
    end else
    if FMouseActiveElement = beNone then
    begin
      if FMouseHoverElement = beButton then
      begin
        FMouseActiveElement := beButton;
        if FButtonState <> bsBtnPressed then
        begin
          FButtonState := bsBtnPressed;
          if FToolbarDispatch <> nil then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end else
      if FMouseHoverElement = beDropdown then
      begin
        FMouseActiveElement := beDropdown;
        if FButtonState <> bsDropdownPressed then
        begin
          FButtonState := bsDropdownPressed;
          if FToolbarDispatch <> nil then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end;
    end;
  end    // if FEnabled
  else
  begin
    FMouseHoverElement := beNone;
    FMouseActiveElement := beNone;
    if FButtonState <> bsIdle then
    begin
      FButtonState := bsIdle;
      if Assigned(FToolbarDispatch) then
        FToolbarDispatch.NotifyVisualsChanged;
    end;
  end;
end;

procedure TSpkBaseButton.MouseLeave;
begin
  if FEnabled then
  begin
    if FMouseActiveElement = beNone then
    begin
      if FMouseHoverElement = beButton then
      begin
        // Placeholder, if there is a need to handle this event
      end else
      if FMouseHoverElement = beDropdown then
      begin
        // Placeholder, if there is a need to handle this event
      end;
    end;
    if FButtonState <> bsIdle then
    begin
      FButtonState := bsIdle;
      if Assigned(FToolbarDispatch) then
        FToolbarDispatch.NotifyVisualsChanged;
    end;
  end  // if FEnabled
  else
  begin
    FMouseHoverElement := beNone;
    FMouseActiveElement := beNone;
    if FButtonState <> bsIdle then
    begin
      FButtonState := bsIdle;
      if Assigned(FToolbarDispatch) then
        FToolbarDispatch.NotifyVisualsChanged;
    end;
  end;
end;

procedure TSpkBaseButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewMouseHoverElement: TSpkMouseButtonElement;
begin
  if FEnabled then
  begin
    {$IFDEF EnhancedRecordSupport}
    if FButtonRect.Contains(T2DIntPoint.Create(X,Y)) then
    {$ELSE}
    if FButtonRect.Contains(X,Y)
    {$ENDIF}
    then
      NewMouseHoverElement := beButton
    else
    if (FButtonKind = bkButtonDropdown) and
      {$IFDEF EnhancedRecordSupport}
      (FDropdownRect.Contains(T2DIntPoint.Create(X,Y))) then
      {$ELSE}
      (FDropdownRect.Contains(X,Y))
      {$ENDIF}
    then
      NewMouseHoverElement := beDropdown
    else
      NewMouseHoverElement := beNone;

    if FMouseActiveElement = beButton then
    begin
      if (NewMouseHoverElement = beNone) and (FButtonState <> bsIdle) then
      begin
        FButtonState := bsIdle;
        if FToolbarDispatch <> nil then
          FToolbarDispatch.NotifyVisualsChanged;
      end else
      if (NewMouseHoverElement = beButton) and (FButtonState <> bsBtnPressed) then
      begin
        FButtonState := bsBtnPressed;
        if FToolbarDispatch <> nil then
          FToolbarDispatch.NotifyVisualsChanged;
      end;
    end else
    if FMouseActiveElement = beDropdown then
    begin
      if (NewMouseHoverElement = beNone) and (FButtonState <> bsIdle) then
      begin
        FButtonState := bsIdle;
        if FToolbarDispatch <> nil then
          FToolbarDispatch.NotifyVisualsChanged;
      end else
      if (NewMouseHoverElement = beDropdown) and (FButtonState <> bsDropdownPressed) then
      begin
        FButtonState := bsDropdownPressed;
        if FToolbarDispatch <> nil then
          FToolbarDispatch.NotifyVisualsChanged;
      end;
    end else
    if FMouseActiveElement = beNone then
    begin
      // Due to the simplified mouse support in the button, there is no need to
      // inform the previous element that the mouse has left its area.
      if NewMouseHoverElement = beButton then
      begin
        if FButtonState <> bsBtnHottrack then
        begin
          FButtonState := bsBtnHottrack;
          if FToolbarDispatch <> nil then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end else
      if NewMouseHoverElement = beDropdown then
      begin
        if FButtonState <> bsDropdownHottrack then
        begin
          FButtonState := bsDropdownHottrack;
          if FToolbarDispatch <> nil then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end;
    end;
    FMouseHoverElement := NewMouseHoverElement;
  end    // if FEnabled
  else
  begin
    FMouseHoverElement := beNone;
    FMouseActiveElement := beNone;
    if FButtonState <> bsIdle then
    begin
      FButtonState := bsIdle;
      if Assigned(FToolbarDispatch) then
        FToolbarDispatch.NotifyVisualsChanged;
    end;
  end;
end;

procedure TSpkBaseButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ClearActive: boolean;
  DropPoint: T2DIntPoint;
begin
  if FEnabled then
  begin
    // The buttons react only to the left mouse button
    if Button <> mbLeft then
      exit;

    ClearActive := not (ssLeft in Shift);

    if FMouseActiveElement = beButton then
    begin
      // The event only works when the mouse button is released above the button
      if FMouseHoverElement = beButton then
      begin
        if FButtonKind in [bkButton, bkButtonDropdown, bkToggle] then
        begin
          Click;
          FButtonState := bsBtnHottrack;
          if Assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
        end else
        if FButtonKind = bkDropdown then
        begin
          if Assigned(FDropdownMenu) then
          begin
            DropPoint := FToolbarDispatch.ClientToScreen(GetDropdownPoint);
            FDropdownMenu.Popup(DropPoint.x, DropPoint.y);
            FButtonState := bsBtnHottrack;
            if Assigned(FToolbarDispatch) then
              FToolbarDispatch.NotifyVisualsChanged;
          end;
        end;
      end;
    end else
    if FMouseActiveElement = beDropDown then
    begin
      // The event only works if the mouse button has been released above the
      // DropDown button
      if FMouseHoverElement = beDropDown then
      begin
        if Assigned(FDropdownMenu) then
        begin
          DropPoint := FToolbarDispatch.ClientToScreen(GetDropdownPoint);
          FDropdownMenu.Popup(DropPoint.x, DropPoint.y);
          FButtonState := bsBtnHottrack;
          if Assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end;
    end;

    if ClearActive and (FMouseActiveElement <> FMouseHoverElement) then
    begin
      // Due to the simplified handling, there is no need to inform the
      // previous element that the mouse has left its area.
      if FMouseHoverElement = beButton then
      begin
        if FButtonState <> bsBtnHottrack then
        begin
          FButtonState := bsBtnHottrack;
          if Assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end else
      if FMouseHoverElement = beDropdown then
      begin
        if FButtonState <> bsDropdownHottrack then
        begin
          FButtonState := bsDropdownHottrack;
          if Assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end else
      if FMouseHoverElement = beNone then
      begin
        if FButtonState <> bsIdle then
        begin
          FButtonState := bsIdle;
          if Assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
        end;
      end;
    end;

    if ClearActive then
    begin
      FMouseActiveElement := beNone;
    end;
  end   // if FEnabled
  else
  begin
    FMouseHoverElement := beNone;
    FMouseActiveElement := beNone;
    if FButtonState <> bsIdle then
    begin
      FButtonState := bsIdle;
      if Assigned(FToolbarDispatch) then
        FToolbarDispatch.NotifyVisualsChanged;
    end;
  end;
end;

procedure TSpkBaseButton.SetAction(const Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end else
  begin
    if FActionLink = nil then
      FActionLink := TSpkButtonActionLink.Create(self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
  end;
end;

procedure TSpkBaseButton.SetAllowAllUp(const Value: Boolean);
begin
  FAllowAllUp := Value;
end;

procedure TSpkBaseButton.SetButtonKind(const Value: TSpkButtonKind);
begin
  FButtonKind := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkBaseButton.SetCaption(const Value: string);
begin
  FCaption := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkBaseButton.SetChecked(const Value: Boolean);
begin
  if FChecked = Value then
    exit;

  if FGroupIndex > 0 then
  begin
    if FAllowAllUp or ((not FAllowAllUp) and Value) then
      UncheckSiblings;
    if not FAllowAllUp and (not Value) and not SiblingsChecked then
      exit;
  end;

  FChecked := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyVisualsChanged;

  if not (csDesigning in ComponentState) and (Action <> nil) then
    (Action as TCustomAction).Checked := Value;
end;

procedure TSpkBaseButton.SetDropdownMenu(const Value: TPopupMenu);
begin
  FDropdownMenu := Value;
  if Assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkBaseButton.SetEnabled(const Value: boolean);
begin
  inherited;
  if not FEnabled then
  begin
    // If the button has been switched off, it is immediately switched into
    // the Idle state and the active and under the mouse are reset.
    // If it has been enabled, its status will change during the first
    // mouse action.

    // Original comment:
    // Jeœli przycisk zosta³ wy³¹czony, zostaje natychmiast prze³¹czony
    // w stan Idle i zerowane s¹ elementy aktywne i pod mysz¹. Jeœli zosta³
    // w³¹czony, jego stan zmieni siê podczas pierwszej akcji myszy.

    FMouseHoverElement := beNone;
    FMouseActiveElement := beNone;

    if FButtonState <> bsIdle then
    begin
      FButtonState := bsIdle;
      if Assigned(FToolbarDispatch) then
        FToolbarDispatch.NotifyVisualsChanged;
    end;
  end;
end;

procedure TSpkBaseButton.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex = Value then
    exit;

  FGroupIndex := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyVisualsChanged;
end;

procedure TSpkBaseButton.SetRect(const Value: T2DIntRect);
begin
  inherited;
  CalcRects;
end;

function TSpkBaseButton.SiblingsChecked: Boolean;
var
  i: Integer;
  pane: TSpkPane;
  btn: TSpkBaseButton;
begin
  if (Parent is TSpkPane) then
  begin
    pane := TSpkPane(Parent);
    for i:=0 to pane.Items.Count-1 do
      if pane.Items[i] is TSpkBaseButton then
      begin
        btn := TSpkBaseButton(pane.Items[i]);
        if (btn <> self) and (btn.ButtonKind = bkToggle) and
           (btn.GroupIndex = FGroupIndex) and btn.Checked then
        begin
          Result := true;
          exit;
        end;
      end;
  end;
  Result := false;
end;

procedure TSpkBaseButton.UncheckSiblings;
var
  i: Integer;
  pane: TSpkPane;
  btn: TSpkBaseButton;
begin
  if (Parent is TSpkPane) then begin
    pane := TSpkPane(Parent);
    for i:=0 to pane.Items.Count-1 do
      if pane.Items[i] is TSpkBasebutton then
      begin
        btn := TSpkBaseButton(pane.Items[i]);
        if (btn <> self) and (btn.ButtonKind = bkToggle) and (btn.GroupIndex = FGroupIndex) then
          btn.FChecked := false;
      end;
  end;
end;


{ TSpkLargeButton }

procedure TSpkLargeButton.CalcRects;
begin
 {$IFDEF EnhancedRecordSupport}
  if FButtonKind = bkButtonDropdown then
  begin
    FButtonRect := T2DIntRect.Create(FRect.Left, FRect.Top, FRect.Right, FRect.Bottom - LargeButtonDropdownFieldSize);
    FDropdownRect := T2DIntRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize, FRect.Right, FRect.Bottom);
    //FDropdownRect := T2DIntRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize + 1, FRect.Right, FRect.Bottom);
  end else
  begin
    FButtonRect := FRect;
    FDropdownRect := T2DIntRect.Create(0, 0, 0, 0);
  end;
 {$ELSE}
  if FButtonKind = bkButtonDropdown then
  begin
    FButtonRect.Create(FRect.Left, FRect.Top, FRect.Right, FRect.Bottom - LargeButtonDropdownFieldSize);
    FDropdownRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize, FRect.Right, FRect.Bottom);
//    FDropdownRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize + 1, FRect.Right, FRect.Bottom);
  end else
  begin
    FButtonRect := FRect;
    FDropdownRect.Create(0, 0, 0, 0);
  end;
 {$ENDIF}
end;

constructor TSpkLargeButton.Create(AOwner: TComponent);
begin
  inherited;
  FLargeImageIndex := -1;
end;

procedure TSpkLargeButton.Draw(ABuffer: TBitmap; ClipRect: T2DIntRect);
var
  fontColor, frameColor: TColor;
  gradientFromColor, gradientToColor: TColor;
  innerLightColor, innerDarkColor: TColor;
  gradientKind: TBackgroundKind;
  x: Integer;
  y: Integer;
  delta: Integer;
  cornerRadius: Integer;
  imgList: TImageList;
  txtHeight: Integer;
  breakPos, breakWidth: Integer;
  s: String;
  P: T2DIntPoint;
  drawBtn: Boolean;
  R: TRect;
begin
  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;

  if (FRect.Width < 2*LargeButtonRadius) or (FRect.Height < 2*LargeButtonRadius) then
    exit;

  delta := FAppearance.Element.HotTrackBrightnessChange;
  case FAppearance.Element.Style of
    esRounded:
      cornerRadius := LargeButtonRadius;
    esRectangle:
      cornerRadius := 0;
  end;

  // Prepare text color
  fontColor := clNone;
  case FButtonState of
    bsIdle:
      fontColor := FAppearance.Element.IdleCaptionColor;
    bsBtnHottrack,
    bsDropdownHottrack:
      fontColor := FAppearance.Element.HotTrackCaptionColor;
    bsBtnPressed,
    bsDropdownPressed:
      fontColor := FAppearance.ELement.ActiveCaptionColor;
  end;
  if not FEnabled then
    fontColor := TColorTools.ColorToGrayscale(fontColor);

  // Dropdown button
  // Draw full rect, otherwise the DropDownRect will contain the full gradient
  if FButtonKind = bkButtonDropdown then
  begin
    drawBtn := true;
    if (FButtonState in [bsBtnHotTrack, bsBtnPressed]) then
    begin
      FAppearance.Element.GetHotTrackColors(Checked,
        frameColor, innerLightColor, innerDarkColor,
        gradientFromColor, gradientToColor, gradientKind,
        delta);
    end else
    if (FButtonState = bsDropdownHottrack) then
    begin
      FAppearance.Element.GetHotTrackColors(Checked,
        frameColor, innerLightColor, innerDarkColor,
        gradientFromColor, gradientToColor, gradientKind);
    end else
    if (FButtonState = bsDropdownPressed) then
    begin
      FAppearance.Element.GetActiveColors(Checked,
        frameColor, innerLightColor, innerDarkColor,
        gradientFromColor, gradientToColor, gradientKind);
    end else
      drawBtn := false;

    if drawBtn then begin
      TButtonTools.DrawButton(
        ABuffer,
        FRect,
        frameColor,
        innerLightColor,
        innerDarkColor,
        gradientFromColor,
        gradientToColor,
        gradientKind,
        false,
        false,
        false,
        false,
        cornerRadius,
        ClipRect
      );
    end;
  end;

  // Button (Background and frame)
  drawBtn := true;
  if FButtonState = bsBtnHottrack then
  begin
    FAppearance.Element.GetHotTrackColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientKind);
  end else
  if FButtonState = bsBtnPressed then
  begin
    FAppearance.Element.GetActiveColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientkind);
  end else
  if (FButtonState in [bsDropdownHotTrack, bsDropdownPressed]) then
  begin
    FAppearance.Element.GetHotTrackColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientKind,
      delta);
  end else
  if (FButtonState = bsIdle) and Checked then
  begin
    FAppearance.Element.GetActiveColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientKind
    );
  end else
    drawBtn := false;

  if drawBtn then
  begin
    TButtonTools.DrawButton(
      ABuffer,
      FButtonRect,       // draw button part only
      frameColor,
      innerLightColor,
      innerDarkColor,
      gradientFromColor,
      gradientToColor,
      gradientKind,
      false,
      false,
      false,
      FButtonKind = bkButtonDropdown,
      cornerRadius,
      ClipRect
    );
  end;

  // Dropdown button - draw horizontal dividing line
  if FButtonKind = bkButtonDropdown then
  begin
    drawBtn := true;
    if (FButtonState in [bsDropdownHotTrack, bsBtnHotTrack]) then
      frameColor := FAppearance.element.HotTrackFrameColor
    else
    if (FButtonState in [bsDropDownPressed, bsBtnPressed]) then
      frameColor := FAppearance.Element.ActiveFrameColor
    else
      drawBtn := false;
    if drawBtn then
      TGuiTools.DrawHLine(
        ABuffer,
        FDropDownRect.Left,
        FDropDownRect.Right,
        FDropDownRect.Top,
        frameColor,
        ClipRect
     );
  end;

  // Icon
  if not FEnabled and (FDisabledLargeImages <> nil) then
    imgList := FDisabledLargeImages
  else
    imgList := FLargeImages;

  if (imgList <> nil) and (FLargeImageIndex >= 0) and (FLargeImageIndex < imgList.Count) then
  begin
    P := {$IFDEF EnhancedRecordSupport}T2DIntPoint.Create{$ELSE}Create2DIntPoint{$ENDIF}(
      FButtonRect.Left + (FButtonRect.Width - imgList.Width) div 2,
      FButtonRect.Top + LargeButtonBorderSize + LargeButtonGlyphMargin
    );
    TGUITools.DrawImage(
      ABuffer.Canvas,
      imgList,
      FLargeImageIndex,
      P,
      ClipRect
    );
  end;

  // Text
  ABuffer.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
  ABuffer.Canvas.Font.Color := fontColor;

  if FButtonKind in [bkButton, bkToggle] then
    FindBreakPlace(FCaption, breakPos, breakWidth)
  else
    breakPos := 0;
  txtHeight := ABuffer.Canvas.TextHeight('Wy');

  if breakPos > 0 then
  begin
    s := Copy(FCaption, 1, breakPos - 1);
    x := FRect.Left + (FRect.Width - ABuffer.Canvas.Textwidth(s)) div 2;
    y := FRect.Top + LargeButtonCaptionTopRail - txtHeight div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, s, fontColor, ClipRect);

    s := Copy(FCaption, breakPos+1, Length(FCaption) - breakPos);
    x := FRect.Left + (FRect.Width - ABuffer.Canvas.Textwidth(s)) div 2;
    y := FRect.Top + LargeButtonCaptionButtomRail - txtHeight div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, s, fontColor, ClipRect);
  end else
  begin
    // The text is not broken
    x := FButtonRect.Left + (FButtonRect.Width - ABuffer.Canvas.Textwidth(FCaption)) div 2;
    y := FRect.Top + LargeButtonCaptionTopRail - txtHeight div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, FCaption, FontColor, ClipRect);
  end;

  // Dropdown arrow
  if FButtonKind = bkDropdown then
  begin
    y := FButtonRect.Bottom - ABuffer.Canvas.TextHeight('Tg') - 1;
    R := Classes.Rect(FButtonRect.Left, y, FButtonRect.Right, FButtonRect.Bottom);
    DrawDropdownArrow(ABuffer, R, fontcolor);
  end else
  if FButtonKind = bkButtonDropdown then
  begin
    y := FDropdownRect.Bottom - ABuffer.Canvas.TextHeight('Tg') - 1;
    R := Classes.Rect(FDropdownRect.Left, y, FDropDownRect.Right, FDropdownRect.Bottom);
    DrawDropdownArrow(ABuffer, R, fontcolor);
  end;
end;

procedure TSpkLargeButton.FindBreakPlace(s: string; out Position: integer; out Width: integer);
var
  i: integer;
  Bitmap: TBitmap;
  BeforeWidth, AfterWidth: integer;
begin
  Position := -1;
  Width := -1;

  if FToolbarDispatch=nil then
     exit;
  if FAppearance=nil then
     exit;

  Bitmap := FToolbarDispatch.GetTempBitmap;
  if Bitmap=nil then
    exit;

  Bitmap.Canvas.Font.Assign(FAppearance.Element.CaptionFont);

  Width := Bitmap.Canvas.TextWidth(FCaption);

  for i := 1 to Length(s) do
    if s[i] = ' ' then
    begin
      if i > 1 then
        BeforeWidth := Bitmap.Canvas.TextWidth(Copy(s, 1, i-1))
      else
        BeforeWidth := 0;

      if i < Length(s) then
        AfterWidth := Bitmap.Canvas.TextWidth(Copy(s, i+1, Length(s)-i))
      else
        AfterWidth := 0;

      if (Position = -1) or (Max(BeforeWidth, AfterWidth) < Width) then
      begin
        Width := Max(BeforeWidth, AfterWidth);
        Position := i;
      end;
    end;
end;

function TSpkLargeButton.GetDropdownPoint: T2DIntPoint;
begin
  {$IFDEF EnhancedRecordSupport}
  case FButtonKind of
    bkDropdown       : Result := T2DIntPoint.Create(FButtonRect.left, FButtonRect.Bottom+1);
    bkButtonDropdown : Result := T2DIntPoint.Create(FDropdownRect.left, FDropdownRect.Bottom+1);
  else
    Result := T2DIntPoint.Create(0,0);
  end;
  {$ELSE}
  case FButtonKind of
    bkDropdown       : Result.Create(FButtonRect.left, FButtonRect.Bottom+1);
    bkButtonDropdown : Result.Create(FDropdownRect.left, FDropdownRect.Bottom+1);
  else
    Result.Create(0,0);
  end;
  {$ENDIF}
end;

function TSpkLargeButton.GetGroupBehaviour: TSpkItemGroupBehaviour;
begin
  Result := gbSingleItem;
end;

function TSpkLargeButton.GetSize: TSpkItemSize;
begin
  Result := isLarge;
end;

function TSpkLargeButton.GetTableBehaviour: TSpkItemTableBehaviour;
begin
  Result := tbBeginsColumn;
end;

function TSpkLargeButton.GetWidth: integer;
var
  GlyphWidth: integer;
  TextWidth: integer;
  Bitmap: TBitmap;
  BreakPos, RowWidth: integer;
begin
  Result := -1;

  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;

  Bitmap := FToolbarDispatch.GetTempBitmap;
  if Bitmap = nil then
    exit;

  // Glyph
  if FLargeImages <> nil then
    GlyphWidth := 2 * LargeButtonGlyphMargin + FLargeImages.Width
  else
    GlyphWidth := 0;

  // Text
  if FButtonKind in [bkButton, bkToggle] then
  begin
    // Label
    FindBreakPlace(FCaption,BreakPos,RowWidth);
    TextWidth := 2 * LargeButtonCaptionHMargin + RowWidth;
  end else
  begin
    // do not break the label
    Bitmap.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
    TextWidth := 2 * LargeButtonCaptionHMargin + Bitmap.Canvas.TextWidth(FCaption);
  end;

  Result := Max(LargeButtonMinWidth, Max(GlyphWidth, TextWidth));
end;

procedure TSpkLargeButton.SetLargeImageIndex(const Value: TImageIndex);
begin
  FLargeImageIndex := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

{ TSpkSmallButton }

constructor TSpkSmallButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FTableBehaviour := tbContinuesRow;
  FGroupBehaviour := gbSingleItem;
  FHideFrameWhenIdle := false;
  FShowCaption := true;
end;

procedure TSpkSmallButton.CalcRects;
var
  RectVector: T2DIntVector;
begin
  ConstructRects(FButtonRect, FDropdownRect);
 {$IFDEF EnhancedRecordSupport}
  RectVector := T2DIntVector.Create(FRect.Left, FRect.Top);
 {$ELSE}
  RectVector.Create(FRect.Left, FRect.Top);
 {$ENDIF}
  FButtonRect := FButtonRect + RectVector;
  FDropdownRect := FDropdownRect + RectVector;
end;

procedure TSpkSmallButton.ConstructRects(out BtnRect, DropRect: T2DIntRect);
var
  BtnWidth: integer;
  DropdownWidth: Integer;
  Bitmap: TBitmap;
  TextWidth: Integer;
  AdditionalPadding: Boolean;
begin
  {$IFDEF EnhancedRecordSupport}
  BtnRect := T2DIntRect.Create(0, 0, 0, 0);
  DropRect := T2DIntRect.Create(0, 0, 0, 0);
  {$ELSE}
  BtnRect.Create(0, 0, 0, 0);
  DropRect.Create(0, 0, 0, 0);
  {$ENDIF}

  if not Assigned(FToolbarDispatch) then
    exit;
  if not Assigned(FAppearance) then
    exit;

  Bitmap := FToolbarDispatch.GetTempBitmap;
  if not Assigned(Bitmap) then
    exit;

  // *** Regardless of the type, there must be room for the icon and / or text ***

  BtnWidth := 0;
  AdditionalPadding := false;

  // Icon
  if FImageIndex <> -1 then
  begin
    BtnWidth := BtnWidth + SmallButtonPadding + SmallButtonGlyphWidth;
    AdditionalPadding := true;
  end;

  // Text
  if FShowCaption then
  begin
    Bitmap.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
    TextWidth := Bitmap.Canvas.TextWidth(FCaption);

    BtnWidth := BtnWidth + SmallButtonPadding + TextWidth;
    AdditionalPadding := true;
  end;

  // Padding behind the text or icon
  if AdditionalPadding then
    BtnWidth := BtnWidth + SmallButtonPadding;

  // The width of the button content must be at least SMALLBUTTON_MIN_WIDTH
  BtnWidth := Max(SmallButtonMinWidth, BtnWidth);

  // *** Dropdown ***
  case FButtonKind of
    bkButton, bkToggle:
      begin
        // Left edge of the button
        if FGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
          BtnWidth := BtnWidth + SmallButtonHalfBorderWidth
        else
          BtnWidth := BtnWidth + SmallButtonBorderWidth;

        // Right edge of the button
        if (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) then
          BtnWidth := BtnWidth + SmallButtonHalfBorderWidth
        else
          BtnWidth := BtnWidth + SmallButtonBorderWidth;

        {$IFDEF EnhancedRecordSupport}
        BtnRect := T2DIntRect.Create(0, 0, BtnWidth - 1, SpkLayoutSizes.PANE_ROW_HEIGHT - 1);
        DropRect := T2DIntRect.Create(0, 0, 0, 0);
        {$ELSE}
        BtnRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
        DropRect.Create(0, 0, 0, 0);
        {$ENDIF}
      end;

    bkButtonDropdown:
      begin
        // Left edge of the button
        if FGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
          BtnWidth := BtnWidth + SmallButtonHalfBorderWidth
        else
          BtnWidth := BtnWidth + SmallButtonBorderWidth;

        // Right edge of the button
        BtnWidth := BtnWidth + SmallButtonHalfBorderWidth;

        // Left edge and dropdown field content
        DropdownWidth := SmallButtonHalfBorderWidth + SmallButtonDropdownWidth;

        // Right edge of the dropdown field
        if (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) then
          DropdownWidth := DropdownWidth + SmallButtonHalfBorderWidth
        else
          DropdownWidth := DropdownWidth + SmallButtonBorderWidth;

        {$IFDEF EnhancedRecordSupport}
        BtnRect := T2DIntRect.Create(0, 0, BtnWidth - 1, PaneRowHeightT - 1);
        DropRect := T2DIntRect.Create(BtnRect.Right+1, 0, BtnRect.Right+DropdownWidth, PaneRowHeight - 1);
        {$ELSE}
        BtnRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
        DropRect.Create(BtnRect.Right+1,  0, BtnRect.Right+DropdownWidth, PaneRowHeight - 1);
        {$ENDIF}
      end;

    bkDropdown:
      begin
        // Left edge of the button
        if FGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
          BtnWidth := BtnWidth + SmallButtonHalfBorderWidth
        else
          BtnWidth := BtnWidth + SmallButtonBorderWidth;

        // Right edge of the button
        if (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) then
          BtnWidth := BtnWidth + SmallButtonHalfBorderWidth
        else
          BtnWidth := BtnWidth + SmallButtonBorderWidth;

        // Additional area for dropdown + place for the central edge,
        // for dimensional compatibility with dkButtonDropdown
        BtnWidth := BtnWidth + SmallButtonBorderWidth + SmallButtonDropdownWidth;

        {$IFDEF EnhancedRecordSupport}
        BtnRect := T2DIntRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
        DropRect := T2DIntRect.Create(0, 0, 0, 0);
        {$ELSE}
        BtnRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
        DropRect.Create(0, 0, 0, 0);
        {$ENDIF}
      end;
  end;
end;

procedure TSpkSmallButton.Draw(ABuffer: TBitmap; ClipRect: T2DIntRect);
var
  fontColor: TColor;
  frameColor, innerLightColor, innerDarkColor: TColor;
  gradientFromColor, gradientToColor: TColor;
  gradientKind: TBackgroundKind;
  P: T2DIntPoint;
  x, y: Integer;
  delta: Integer;
  cornerRadius: Integer;
  imgList: TImageList;
  drawBtn: Boolean;
  R: TRect;
  dx: Integer;
begin
  if (FToolbarDispatch = nil) or (FAppearance = nil) then
    exit;

  if (FRect.Width < 2*SmallButtonRadius) or (FRect.Height < 2*SmallButtonRadius) then
    exit;

  delta := FAppearance.Element.HotTrackBrightnessChange;
  case FAppearance.Element.Style of
    esRounded:
      cornerRadius := SmallButtonRadius;
    esRectangle:
      cornerRadius := 0;
  end;

  // Button (Background and frame)
  drawBtn := true;
  if (FButtonState = bsIdle) and (not FHideFrameWhenIdle) then
  begin
    FAppearance.Element.GetIdleColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientKind
    );
  end else
  if FButtonState = bsBtnHottrack then
  begin
    FAppearance.Element.GetHotTrackColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientKind
    );
  end else
  if FButtonState = bsBtnPressed then
  begin
    FAppearance.Element.GetActiveColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientKind
    );
  end else
  if (FButtonState in [bsDropdownHotTrack, bsDropdownPressed]) then
  begin
    FAppearance.Element.GetHotTrackColors(Checked,
      frameColor, innerLightColor, innerDarkColor,
      gradientFromColor, gradientToColor, gradientKind,
      delta
    );
  end else
    drawBtn := false;

  if drawBtn then
  begin
    TButtonTools.DrawButton(
      ABuffer,
      FButtonRect,       // draw button part only
      frameColor,
      innerLightColor,
      innerDarkColor,
      gradientFromColor,
      gradientToColor,
      gradientKind,
      (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]),
      (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) or (FButtonKind = bkButtonDropdown),
      false,
      false,
      cornerRadius,
      ClipRect
    );
  end;

  // Icon
  if not FEnabled and (FDisabledImages <> nil) then
    imgList := FDisabledImages
  else
    imgList := FImages;

  if (imgList <> nil) and (FImageIndex >= 0) and (FImageIndex < imgList.Count) then
  begin
    if (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]) then
      x := FButtonRect.Left + SmallButtonHalfBorderWidth + SmallButtonPadding
    else
      x := FButtonRect.Left + SmallButtonBorderWidth + SmallButtonPadding;
    y := FButtonRect.top + (FButtonRect.height - imgList.Height) div 2;
    P := {$IFDEF EnhancedRecordSupport}T2DIntPoint.Create{$ELSE}Create2DIntPoint{$ENDIF}(x, y);
    TGUITools.DrawImage(
      ABuffer.Canvas,
      imgList,
      FImageIndex,
      P,
      ClipRect
    );
  end;

  // Prepare font and chevron color
  fontColor := clNone;
  case FButtonState of
    bsIdle:
      fontColor := FAppearance.Element.IdleCaptionColor;
    bsBtnHottrack,
    bsDropdownHottrack:
      fontColor := FAppearance.Element.HotTrackCaptionColor;
    bsBtnPressed,
    bsDropdownPressed:
      fontColor := FAppearance.ELement.ActiveCaptionColor;
  end;
  if not FEnabled then
    fontColor := TColorTools.ColorToGrayscale(fontColor);

  // Text
  if FShowCaption then
  begin
    ABuffer.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
    ABuffer.Canvas.Font.Color := fontColor;

    if (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]) then
      x := FButtonRect.Left + SmallButtonHalfBorderWidth
    else
      x := FButtonRect.Left + SmallButtonBorderWidth;

    if FImageIndex <> -1 then
      x := x + 2 * SmallButtonPadding + SmallButtonGlyphWidth
    else
      x := x + SmallButtonPadding;
    y := FButtonRect.Top + (FButtonRect.Height - ABuffer.Canvas.TextHeight('Wy')) div 2;

    TGUITools.DrawText(ABuffer.Canvas, x, y, FCaption, fontColor, ClipRect);
  end;

  // Dropdown button
  if FButtonKind = bkButtonDropdown then
  begin
    drawBtn := true;
    if (FButtonState = bsIdle) and (not FHideFrameWhenIdle) then
    begin
      FAppearance.Element.GetIdleColors(Checked,
        frameColor, innerLightColor, innerDarkColor,
        gradientFromColor, gradientToColor, gradientkind
      );
    end else
    if (FButtonState in [bsBtnHottrack, bsBtnPressed]) then
    begin
      FAppearance.Element.GetHotTrackColors(Checked,
        frameColor, innerLightColor, innerDarkColor,
        gradientFromColor, gradientToColor, gradientKind,
        delta
      );
    end else
    if (FButtonState = bsDropdownHottrack) then
    begin
      FAppearance.Element.GetHotTrackColors(Checked,
        frameColor, innerLightColor, innerDarkColor,
        gradientFromColor, gradientToColor, gradientkind
      );
    end else
    if (FButtonState = bsDropdownPressed) then
    begin
      FAppearance.Element.GetActiveColors(Checked,
        frameColor, innerLightColor, innerDarkColor,
        gradientFromColor, gradientToColor, gradientKind
      );
    end else
      drawBtn := false;

    if drawBtn then begin
      TButtonTools.DrawButton(
        ABuffer,
        FDropdownRect,
        frameColor,
        innerLightColor,
        innerDarkColor,
        gradientFromColor,
        gradientToColor,
        gradientKind,
        true,
        (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]),
        false,
        false,
        cornerRadius,
        ClipRect
      );
    end;
  end;

  // Dropdown arrow
  if FButtonKind in [bkDropdown, bkButtonDropdown] then begin
    dx := SmallButtonDropdownWidth;
    if FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup] then
      inc(dx, SmallButtonHalfBorderWidth)
    else
      inc(dx, SmallButtonBorderWidth);
    if FButtonKind = bkDropdown then
      R := Classes.Rect(FButtonRect.Right-dx, FButtonRect.Top, FButtonRect.Right, FButtonRect.Bottom)
    else
      R := Classes.Rect(FDropdownRect.Right-dx, FDropdownRect.Top, FDropdownRect.Right, FDropdownRect.Bottom);
    DrawdropdownArrow(ABuffer, R, fontcolor);
  end;
end;

function TSpkSmallButton.GetDropdownPoint: T2DIntPoint;
begin
 {$IFDEF EnhancedRecordSupport}
  if FButtonKind in [bkButtonDropdown, bkDropdown] then
    Result := T2DIntPoint.Create(FButtonRect.Left, FButtonRect.Bottom+1)
  else
    Result := T2DIntPoint.Create(0,0);
 {$ELSE}
  if FButtonKind in [bkButtonDropdown, bkDropdown] then
    Result.Create(FButtonRect.Left, FButtonRect.Bottom+1)
  else
    Result.Create(0,0);
 {$ENDIF}
end;

function TSpkSmallButton.GetGroupBehaviour: TSpkItemGroupBehaviour;
begin
  Result := FGroupBehaviour;
end;

function TSpkSmallButton.GetSize: TSpkItemSize;
begin
  Result := isNormal;
end;

function TSpkSmallButton.GetTableBehaviour: TSpkItemTableBehaviour;
begin
  Result := FTableBehaviour;
end;

function TSpkSmallButton.GetWidth: integer;
var
  BtnRect, DropRect: T2DIntRect;
begin
  Result := -1;

  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;

  ConstructRects(BtnRect, DropRect);

  if FButtonKind = bkButtonDropdown then
    Result := DropRect.Right+1
  else
    Result := BtnRect.Right+1;
end;

procedure TSpkSmallButton.SetGroupBehaviour(const Value: TSpkItemGroupBehaviour);
begin
  FGroupBehaviour := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkSmallButton.SetHideFrameWhenIdle(const Value: boolean);
begin
  FHideFrameWhenIdle := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyVisualsChanged;
end;

procedure TSpkSmallButton.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkSmallButton.SetShowCaption(const Value: boolean);
begin
  FShowCaption := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkSmallButton.SetTableBehaviour(const Value: TSpkItemTableBehaviour);
begin
  FTableBehaviour := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;


end.
