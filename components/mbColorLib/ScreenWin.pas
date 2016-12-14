unit ScreenWin;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls,
  PalUtils;

const
 crPickerCursor = 13;

type
  TScreenForm = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EndSelection(x, y: integer; ok: boolean);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  private
   FOnSelColorChange: TNotifyEvent;
   FOnKeyDown: TKeyEvent;

  protected
   procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;

  public
    FHintFormat: string;
    SelectedColor: TColor;
    property OnSelColorChange: TNotifyEvent read FOnSelColorChange write FOnSelColorChange;
    property OnScreenKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
  end;

var
  ScreenForm: TScreenForm;

implementation

{$IFDEF DELPHI}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{$R PickCursor.res}

function ColorToHex(Color: TColor): string;
begin
 Result := IntToHex(GetRValue(Color), 2) + IntToHex(GetGValue(Color), 2) + IntToHex(GetBValue(Color), 2);
end;

function GetDesktopColor(const X, Y: Integer): TColor;
var
  c: TCanvas;
  screenDC: HDC;
begin
  c := TCanvas.Create;
  try
    screenDC := GetDC(0);
    c.Handle := screenDC;
    Result := c.Pixels[X, Y];
  finally
    c.Free;
  end;
end;
(*
{$ELSE}
var
  bmp: TBitmap;
  screenDC: HDC;
begin
  bmp := TBitmap.Create;
  screenDC := GetDC(0);
  bmp.LoadFromDevice(screenDC);
  Result := bmp.Canvas.Pixels[X, Y];
  ReleaseDC(0, screenDC);
  bmp.Free;
end;
{$ENDIF}
*)

procedure TScreenForm.FormShow(Sender: TObject);
begin
 Width := Screen.Width;
 Height := Screen.Height;
 Left := 0;
 Top := 0;
end;

procedure TScreenForm.FormCreate(Sender: TObject);
begin
 Brush.Style := bsClear;
 Screen.Cursors[crPickerCursor] := LoadCursor(HInstance, 'PickerCursor');
 Cursor := crPickerCursor;
 SelectedColor := clNone;
 FHintFormat := 'RGB(%r, %g, %b)'#13'Hex: %h';
end;

procedure TScreenForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = VK_ESCAPE) or (ssAlt in Shift) or (ssCtrl in Shift) then
    EndSelection(0, 0, false);
  if (key = VK_RETURN) then
    EndSelection(Mouse.CursorPos.X, Mouse.CursorPos.Y, true);
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TScreenForm.EndSelection(x, y: integer; ok: boolean);
begin
  if ok then
    SelectedColor := GetDesktopColor(x, y)
  else
    SelectedColor := clNone;
  Close;
  if Assigned(FOnSelColorChange) then
    FOnSelColorChange(Self);
end;

procedure TScreenForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 EndSelection(x, y, true);
end;

procedure TScreenForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  SelectedColor := GetDesktopColor(x, y);
  if Assigned(FOnSelColorChange) then FOnSelColorChange(Self);
  Application.ProcessMessages;
end;

procedure TScreenForm.CMHintShow(var Message: TCMHintShow);
begin
  with TCMHintShow(Message) do
    if not ShowHint then
      Message.Result := 1
   else
     with HintInfo^ do
     begin
       Result := 0;
       ReshowTimeout := 1;
       HideTimeout := 5000;
       HintPos := Point(HintPos.X + 16, HintPos.y - 16);
       HintStr := FormatHint(FHintFormat, SelectedColor);
     end;
  inherited;
end;

end.
