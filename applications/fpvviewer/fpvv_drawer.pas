unit fpvv_drawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType;

type

  { TFPVVDrawer }

  TFPVVDrawer = class(TCustomControl)
  private
    DragDropStarted: Boolean;
    DragStartPos: TPoint;
  public
    PosX, PosY: Integer;
    Drawing: TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleClick(Sender: TObject);
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Clear;
  end;

implementation

{ TFPVVDrawer }

constructor TFPVVDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Drawing := TBitmap.Create;

  OnKeyDown := @HandleKeyDown;
  OnClick := @HandleClick;
  OnMouseDown := @HandleMouseDown;
  OnMouseUp := @HandleMouseUp;
end;

destructor TFPVVDrawer.Destroy;
begin
  Drawing.Free;
  inherited Destroy;
end;

procedure TFPVVDrawer.EraseBackground(DC: HDC);
begin

end;

procedure TFPVVDrawer.Paint;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Self.ClientRect);
  Canvas.Draw(PosX, PosY, Drawing);
//  inherited Paint;
end;

procedure TFPVVDrawer.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_UP: Inc(PosY, 5);
  VK_DOWN: Dec(PosY, 5);
  VK_LEFT: Inc(PosX, 5);
  VK_RIGHT: Dec(PosX, 5);
  else
    Exit;
  end;
  Invalidate();
end;

procedure TFPVVDrawer.HandleClick(Sender: TObject);
begin
  Self.SetFocus();
end;

procedure TFPVVDrawer.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not DragDropStarted then
  begin
    DragDropStarted := True;
    DragStartPos := Point(X, Y);
  end;
end;

procedure TFPVVDrawer.HandleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if DragDropStarted then
  begin
    DragDropStarted := False;
    PosX := PosX + (X - DragStartPos.X);
    PosY := PosY + (Y - DragStartPos.Y);
    Invalidate();
  end;
end;

procedure TFPVVDrawer.Clear;
begin
  PosX := 0;
  PosY := 0;
end;

end.

