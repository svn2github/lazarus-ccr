{ LCL control for playing videos using mplayer under gtk2

  Copyright (C) 2009 Mattias Gaertner mattias@freepascal.org

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

Changes:
  2014-03-24  Changes for Microsoft Windows Compatibility and added Events
              for Mouse Actions/ Michael Koecher aka six1
}
unit MPlayerCtrl;

{$mode objfpc}{$H+}

{$ifdef Linux}
 {$ifndef LCLgtk2}
 {$error this unit only supports LCL under gtk2}
 {$endif}
{$endif}

interface

uses
  Classes, SysUtils, Controls, WSLCLClasses, LCLProc, LCLType, InterfaceBase,
  LResources, LMessages, Graphics, ExtCtrls, FileUtil, Process, UTF8Process,
  LazFileUtils
  {$ifdef Linux}
  , gtk2int, gtk2, glib2, gdk2x, Gtk2WSControls, GTK2Proc, Gtk2Def
  {$endif}
  ;

type
TPlayerStatusEvent = procedure(var Msg: String) of Object;
type
TPlayerErrorEvent = procedure(var Msg: String) of Object;


type

  { TCustomMPlayerControl }

  TCustomMPlayerControl = class(TWinControl)
  private
    FFilename: string;
    FStartParam:string;
    FLoop: integer;
    FMPlayerPath: string;
    FPaused: boolean;
    FPlayerInfos:boolean;
    fPlayerProcess: TProcessUTF8;
    fTimer: TTimer;
    FVolume: integer;
    FCanvas: TCanvas;
    FPlayerStatusEvent : TPlayerStatusEvent;
    FPlayerErrorEvent : TPlayerErrorEvent;

    procedure SetFilename(const AValue: string);
    procedure SetLoop(const AValue: integer);
    procedure SetMPlayerPath(const AValue: string);
    procedure SetPaused(const AValue: boolean);
    procedure SetVolume(const AValue: integer);
    procedure SetStartParam(const AValue: string);
    procedure TimerEvent(Sender: TObject);
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMPlayerCommand(Cmd: string); // see: mplayer -input cmdlist and http://www.mplayerhq.hu/DOCS/tech/slave.txt
    function Running: boolean;
    procedure Play;
    procedure Stop;
    function Playing: boolean;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
  public
    property Filename: string read FFilename write SetFilename;
    property StartParam: string read FStartParam write SetStartParam;
    property MPlayerPath: string read FMPlayerPath write SetMPlayerPath;
    property PlayerProcess: TProcessUTF8 read fPlayerProcess;
    property Paused: boolean read FPaused write SetPaused;
    property PlayerInfos: boolean read FPlayerInfos write FPlayerInfos;
    property Loop: integer read FLoop write SetLoop; // -1 no, 0 forever, 1 once, 2 twice, ...
    property Volume: integer read FVolume write SetVolume;
  published
    property OnPlayerStatusEvent: TPlayerStatusEvent read FPlayerStatusEvent write FPlayerStatusEvent;
    property OnPlayerErrorEvent: TPlayerErrorEvent read FPlayerErrorEvent write FPlayerErrorEvent;
  end;

  TMPlayerControl = class(TCustomMPlayerControl)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property Filename;
    property Loop;
    property OnChangeBounds;
    property OnConstrainedResize;
    property OnResize;
    property OnClick;
    property OnMouseUp;
    property OnMouseDown;
    property Visible;
    property Volume;
  end;

  { TWSMPlayerControl }

  {$ifdef Linux}
  TWSMPlayerControl = class(TGtk2WSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;
  {$endif}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Multimedia',[TMPlayerControl]);
end;


{ TCustomMPlayerControl }


procedure TCustomMPlayerControl.TimerEvent(Sender: TObject);
var
  OutList, ErrList:TStringlist;
  x:integer;
  msg:string;
begin
  if Running then begin
    if (fPlayerProcess <> nil) and (fPlayerProcess.Output.NumBytesAvailable > 0) then begin
      OutList:=TStringlist.create;
      try
        OutList.LoadFromStream(fPlayerProcess.Output);
        if Assigned(fPlayerStatusEvent) then begin
          for x := 0 to OutList.Count -1 do begin
            msg:=OutList[x];
            fPlayerStatusEvent(msg);
          end;
        end;
      finally
        OutList.free;
      end;
    end;
    if (fPlayerProcess <> nil) and (fPlayerProcess.StdErr.NumBytesAvailable > 0) then begin
      ErrList:=TStringlist.create;
      try
        ErrList.LoadFromStream(fPlayerProcess.Stderr);
        if Assigned(fPlayerErrorEvent) then begin
          for x := 0 to ErrList.Count -1 do begin
            msg:=ErrList[x];
            fPlayerErrorEvent(msg);
          end;
        end;
      finally
        ErrList.free;
      end;
    end;
  end else begin
    Stop;
  end;
end;

procedure TCustomMPlayerControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  if (csDesigning in ComponentState) and (FCanvas<>nil) then begin
    with FCanvas do begin
      if Message.DC <> 0 then
        Handle := Message.DC;
      Brush.Color:=clLtGray;
      Pen.Color:=clRed;
      Rectangle(0,0,Self.Width-1,Self.Height-1);
      MoveTo(0,0);
      LineTo(Self.Width,Self.Height);
      MoveTo(0,Self.Height);
      LineTo(Self.Width,0);
      if Message.DC <> 0 then
        Handle := 0;
    end;
  end;
  Exclude(FControlState, csCustomPaint);
end;

procedure TCustomMPlayerControl.WMSize(var Message: TLMSize);
begin
  if (Message.SizeType and Size_SourceIsInterface)>0 then
    DoOnResize;
end;


procedure TCustomMPlayerControl.SetStartParam(const AValue: string);
begin
  if FStartParam=AValue then exit;
  FStartParam:=AValue;
end;

procedure TCustomMPlayerControl.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  if Running then
    SendMPlayerCommand('loadfile '+StrToCmdLineParam(Filename));
end;

procedure TCustomMPlayerControl.SetLoop(const AValue: integer);
begin
  if FLoop=AValue then exit;
  FLoop:=AValue;
  if Running then
    SendMPlayerCommand('loop '+IntToStr(FLoop));
end;

procedure TCustomMPlayerControl.SetMPlayerPath(const AValue: string);
begin
  if FMPlayerPath=AValue then exit;
  FMPlayerPath:=AValue;
end;

procedure TCustomMPlayerControl.SetPaused(const AValue: boolean);
begin
  if FPaused=AValue then exit;
  if Running then begin
    FPaused:=AValue;
    SendMPlayerCommand('pause');
  end;
end;

procedure TCustomMPlayerControl.SetVolume(const AValue: integer);
begin
  if FVolume=AValue then exit;
  FVolume:=AValue;
  if Running then
    SendMPlayerCommand('volume '+IntToStr(FVolume)+' 1');
end;

constructor TCustomMPlayerControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  if (csDesigning in ComponentState) then begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end else
    FCompStyle:=csNonLCL;
  SetInitialBounds(0, 0, 160, 90);

  fMPlayerPath:='mplayer'+GetExeExt;
  fTimer:=TTimer.Create(Self);
  fTimer.Interval:=10;
  fTimer.OnTimer:=@TimerEvent;
end;

destructor TCustomMPlayerControl.Destroy;
begin
  Stop;
  FreeAndNil(FCanvas);
  FreeAndNil(fTimer);
  inherited Destroy;
end;

procedure TCustomMPlayerControl.SendMPlayerCommand(Cmd: string);
begin
  if Cmd='' then exit;
  if not Running then exit;
  if Cmd[length(Cmd)]<>LineEnding then Cmd:=Cmd+LineEnding;
  fPlayerProcess.Input.Write(Cmd[1],length(Cmd));
end;

function TCustomMPlayerControl.Running: boolean;
begin
  Result:=(fPlayerProcess<>nil) and fPlayerProcess.Running;
end;

procedure TCustomMPlayerControl.Play;
var
  ExePath: String;
  CurWindowID: PtrUInt;
begin
  if (csDesigning in ComponentState) then exit;

  if Running and Paused then begin
    Paused:=false;
    exit;
  end;

  if Playing then exit;

  {$IFDEF Linux}
  if (not HandleAllocated) then exit;
  DebugLn(['TCustomMPlayerControl.Play ']);
  {$endif}

  if fPlayerProcess<>nil then
    FreeAndNil(fPlayerProcess);
//    raise Exception.Create('TCustomMPlayerControl.Play fPlayerProcess still exists');

  if MPlayerPath='' then
    MPlayerPath:='mplayer'+GetExeExt;
  ExePath:=MPlayerPath;
  if not FilenameIsAbsolute(ExePath) then
    ExePath:=FindDefaultExecutablePath(ExePath);
  if not FileExistsUTF8(ExePath) then
    raise Exception.Create(MPlayerPath+' not found');

  {$IFDEF Linux}
    CurWindowID := GDK_WINDOW_XWINDOW({%H-}PGtkWidget(PtrUInt(Handle))^.window);
  {$else}
    CurWindowID := Handle;
  {$ENDIF}

  fPlayerProcess:=TProcessUTF8.Create(Self);
  fPlayerProcess.Options:=fPlayerProcess.Options+[poUsePipes,poNoConsole];
  if FPlayerInfos then
    fPlayerProcess.CommandLine:=ExePath+' -slave -noquiet -wid '+IntToStr(CurWindowID)+' '+StartParam+' '+StrToCmdLineParam(Filename)
  else
    fPlayerProcess.CommandLine:=ExePath+' -slave -quiet -wid '+IntToStr(CurWindowID)+' '+StartParam+' '+StrToCmdLineParam(Filename);
  DebugLn(['TCustomMPlayerControl.Play ',fPlayerProcess.CommandLine]);

  fPlayerProcess.Execute;
  fTimer.Enabled:=true;
end;

procedure TCustomMPlayerControl.Stop;
begin
  if fPlayerProcess=nil then exit;
  FPaused:=false;
  fTimer.Enabled:=false;
  SendMPlayerCommand('quit');
  FreeAndNil(fPlayerProcess);
end;

function TCustomMPlayerControl.Playing: boolean;
begin
  Result:=(fPlayerProcess<>nil) and fPlayerProcess.Running and (not Paused);
end;

procedure TCustomMPlayerControl.Invalidate;
begin
  if csCustomPaint in FControlState then exit;
  inherited Invalidate;
end;

procedure TCustomMPlayerControl.EraseBackground(DC: HDC);
begin
  if DC=0 then ;
  // everything is painted, so erasing the background is not needed
end;

{$ifdef Linux}
function MPLayerWidgetDestroyCB(Widget: PGtkWidget; {%H-}data: gPointer): GBoolean; cdecl;
begin
  FreeWidgetInfo(Widget); // created in TWSMPlayerControl.CreateHandle
  Result:=false;
end;

{ TWSMPlayerControl }

class function TWSMPlayerControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  NewWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  if csDesigning in AWinControl.ComponentState then
    Result:=inherited CreateHandle(AWinControl,AParams)
  else begin
    NewWidget:=gtk_event_box_new;

    WidgetInfo := GetWidgetInfo(NewWidget,true); // destroyed in MPLayerWidgetDestroyCB
    WidgetInfo^.LCLObject := AWinControl;
    WidgetInfo^.Style := AParams.Style;
    WidgetInfo^.ExStyle := AParams.ExStyle;
    WidgetInfo^.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);

    // set allocation
    Allocation.X := AParams.X;
    Allocation.Y := AParams.Y;
    Allocation.Width := AParams.Width;
    Allocation.Height := AParams.Height;
    gtk_widget_size_allocate(NewWidget, @Allocation);

    if csDesigning in AWinControl.ComponentState then begin
      // at designtime setup normal handlers
      TGtk2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    end else begin
      // at runtime
      g_signal_connect(GPointer(NewWidget), 'destroy',
                       TGTKSignalFunc(@MPLayerWidgetDestroyCB), WidgetInfo);
    end;
    Result:=HWND({%H-}PtrUInt(Pointer(NewWidget)));
    DebugLn(['TWSMPlayerControl.CreateHandle ',dbgs(NewWidget)]);
  end;
end;

class procedure TWSMPlayerControl.DestroyHandle(const AWinControl: TWinControl
  );
begin
  inherited DestroyHandle(AWinControl);
end;

initialization
  RegisterWSComponent(TCustomMPlayerControl,TWSMPlayerControl);
  {$I mplayerctrl.lrs}

{$else ifwindows}

initialization
  {$I mplayerctrl.lrs}

{$endif}
end.

