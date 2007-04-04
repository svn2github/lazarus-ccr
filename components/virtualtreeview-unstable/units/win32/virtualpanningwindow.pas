unit virtualpanningwindow;

{$mode objfpc}{$H+}

interface

uses
  Windows, Graphics, Classes, SysUtils;
  
type

  { TVirtualPanningWindow }

  TVirtualPanningWindow = class
  private
    FHandle: THandle;
    FOwnerHandle: THandle;
    FImage: TBitmap;
    procedure HandlePaintMessage;
  public
    procedure Start(OwnerHandle: THandle; const Position: TPoint);
    procedure Stop;
    procedure Show(ClipRegion: HRGN);
    property Image: TBitmap read FImage;
    property Handle: THandle read FHandle;
  end;

implementation

uses
  vtlogger;
  
function PanningWindowProc(Window: HWnd; Msg: UInt;WPara: WParam; LPara: LParam): LResult; stdcall;
var
  PanningObject: TVirtualPanningWindow;
begin
  if Msg = WM_PAINT then
  begin
    PanningObject:=TVirtualPanningWindow(GetWindowLong(Window,GWL_USERDATA));
    if Assigned(PanningObject) then
      PanningObject.HandlePaintMessage;
  end
  else
    DefWindowProc(Window,Msg,WPara,LPara);
end;

var
  PanningWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @PanningWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'VTPanningWindow'
  );

{ TVirtualPanningWindow }

procedure TVirtualPanningWindow.HandlePaintMessage;
var
  PS: PaintStruct;
  Canvas: TCanvas;
begin
  BeginPaint(FHandle, PS);
  Canvas := TCanvas.Create;
  Canvas.Handle := PS.hdc;
  try
    Canvas.Draw(0, 0, FImage);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    EndPaint(FHandle, PS);
  end;
end;


procedure TVirtualPanningWindow.Start(OwnerHandle: THandle; const Position: TPoint);
var
  TempClass: TWndClass;
begin
  // Register the helper window class.
  PanningWindowClass.hInstance := HInstance;
  if not GetClassInfo(HInstance, PanningWindowClass.lpszClassName, TempClass) then
    Windows.RegisterClass(PanningWindowClass);

  // Create the helper window and show it at the given position without activating it.
  with Position do
    FHandle := CreateWindowEx(WS_EX_TOOLWINDOW, PanningWindowClass.lpszClassName, nil, WS_POPUP, X - 16, Y - 16,
      32, 32, OwnerHandle, 0, HInstance, nil);
  //todo use SetWindowLongPtr later
  SetWindowLong(FHandle,GWL_USERDATA,PtrInt(Self));
  
  FImage := TBitmap.Create;
end;

procedure TVirtualPanningWindow.Stop;
begin
  // Destroy the helper window.
  DestroyWindow(FHandle);
  FHandle := 0;
  FImage.Free;
  FImage := nil;
end;

procedure TVirtualPanningWindow.Show(ClipRegion: HRGN);
begin
  Logger.SendBitmap([lcPanning],'Panning Image',FImage);
  SetWindowRgn(FHandle, ClipRegion, False);
  ShowWindow(FHandle, SW_SHOWNOACTIVATE);
end;

end.

