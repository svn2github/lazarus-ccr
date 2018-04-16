{ (c) 2014 ti_dic MapViewer component for lazarus
  Parts of this component are based on :
    Map Viewer Copyright (C) 2011 Maciej Kaczkowski / keit.co

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit mvmapviewer;

{$MODE objfpc}{$H+}

// Activate one of the following defines
{$DEFINE USE_LAZINTFIMAGE}
{.$DEFINE USE_RGBGRAPHICS}    // NOTE: This needs package "rgb_graphics" in requirements

// Make sure that one of the USE_XXXX defines is active. Default is USE_LAZINTFIMAGE
{$IFNDEF USE_RGBGRAPHICS}{$IFNDEF USE_LAZINTFIMAGE}{$DEFINE USE_LAZINTFIMAGES}{$ENDIF}{$ENDIF}
{$IFDEF USE_RGBGRAPHICS}{$IFDEF USE_LAZINTFIMAGE}{$UNDEF USE_RGBGRAPHICS}{$ENDIF}{$ENDIF}

interface

uses
  Classes, SysUtils, Controls, Graphics, IntfGraphics,
  {$IFDEF USE_RGBGRAPHICS}RGBGraphics,{$ENDIF}
  {$IFDEF USE_LAZINTFIMAGE}FPCanvas,{$ENDIF}
  MvTypes, MvGPSObj, MvEngine, MvMapProvider, MvDownloadEngine;

Type

  { TMapView }

  TMapView = class(TCustomControl)
    private
      FDownloadEngine: TMvCustomDownloadEngine;
      FEngine: TMapViewerEngine;
     {$IFDEF USE_RGBGRAPHICS}
      Buffer: TRGB32Bitmap;
     {$ENDIF}
     {$IFDEF USE_LAZINTFIMAGE}
      Buffer: TLazIntfImage;
      BufferCanvas: TFPCustomCanvas;
     {$ENDIF}
      FActive: boolean;
      FGPSItems: TGPSObjectList;
      FInactiveColor: TColor;
      FPOIImage: TBitmap;
      procedure CallAsyncInvalidate;
      procedure DoAsyncInvalidate(Data: PtrInt);
      procedure DrawObjects(const TileId: TTileId; aLeft, aTop, aRight,aBottom: integer);
      procedure DrawPt(const Area: TRealArea;aPOI: TGPSPoint);
      procedure DrawTrk(const Area: TRealArea;trk: TGPSTrack);
      function GetCacheOnDisk: boolean;
      function GetCachePath: String;
      function GetCenter: TRealPoint;
      function GetMapProvider: String;
      function GetOnCenterMove: TNotifyEvent;
      function GetOnChange: TNotifyEvent;
      function GetOnZoomChange: TNotifyEvent;
      function GetUseThreads: boolean;
      function GetZoom: integer;
      procedure SetActive(AValue: boolean);
      procedure SetCacheOnDisk(AValue: boolean);
      procedure SetCachePath(AValue: String);
      procedure SetCenter(AValue: TRealPoint);
      procedure SetInactiveColor(AValue: TColor);
      procedure SetMapProvider(AValue: String);
      procedure SetOnCenterMove(AValue: TNotifyEvent);
      procedure SetOnChange(AValue: TNotifyEvent);
      procedure SetOnZoomChange(AValue: TNotifyEvent);
      procedure SetUseThreads(AValue: boolean);
      procedure SetZoom(AValue: integer);

    protected
      AsyncInvalidate : boolean;
      procedure ActivateEngine;
     {$IFDEF USE_LAZINTFIMAGE}
      procedure CreateLazIntfImageAndCanvas(out ABuffer: TLazIntfImage;
        out ACanvas: TFPCustomCanvas; AWidth, AHeight: Integer);
     {$ENDIF}
      procedure DblClick; override;
      Procedure DoDrawTile(const TileId: TTileId; X,Y: integer; TileImg: TLazIntfImage);
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      procedure DoOnResize; override;
      Function IsActive : Boolean;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
      procedure Paint; override;
      procedure OnGPSItemsModified(Sender: TObject; objs: TGPSObjList;Adding : boolean);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure ClearBuffer;
      procedure GetMapProviders(lstProviders : TStrings);
      function GetVisibleArea: TRealArea;
      function LonLatToScreen(aPt: TRealPoint): TPoint;
      function ScreenToLonLat(aPt: TPoint): TRealPoint;
      procedure CenterOnObj(obj: TGPSObj);
      procedure ZoomOnArea(const aArea: TRealArea);
      procedure ZoomOnObj(obj: TGPSObj);
      procedure WaitEndOfRendering;
      property Center: TRealPoint read GetCenter write SetCenter;
      property DownloadEngine: TMvCustomDownloadEngine read FDownloadEngine;
      property Engine: TMapViewerEngine read FEngine;
      property GPSItems: TGPSObjectList read FGPSItems;
    published
      property Active: boolean read FActive write SetActive;
      property Align;
      property CacheOnDisk: boolean read GetCacheOnDisk write SetCacheOnDisk;
      property CachePath: String read GetCachePath write SetCachePath;
      property Height default 150;
      property InactiveColor: TColor read FInactiveColor write SetInactiveColor;
      property MapProvider: String read GetMapProvider write SetMapProvider;
      property POIImage: TBitmap read FPOIImage write FPOIImage;
      property PopupMenu;
      property UseThreads: boolean read GetUseThreads write SetUseThreads;
      property Width default 150;
      property Zoom: integer read GetZoom write SetZoom;
      property OnCenterMove: TNotifyEvent  read GetOnCenterMove write SetOnCenterMove;
      property OnZoomChange: TNotifyEvent  read GetOnZoomChange write SetOnZoomChange;
      property OnChange: TNotifyEvent Read GetOnChange write SetOnChange;
      property OnMouseDown;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseMove;
      property OnMouseUp;
  end;

implementation

uses
  {$IFDEF USE_LAZINTFIMAGE}
  Math, FPImgCanv, FPImage, LCLVersion,
  {$ENDIF}
  GraphType, mvJobQueue, mvExtraData, mvDLEFpc;


{$IFDEF USE_LAZINTFIMAGE}
// Workaround for http://mantis.freepascal.org/view.php?id=27144
procedure CopyPixels(ASource, ADest: TLazIntfImage;
  XDst: Integer = 0; YDst: Integer = 0;
  AlphaMask: Boolean = False; AlphaTreshold: Word = 0);
var
  SrcHasMask, DstHasMask: Boolean;
  x, y, xStart, yStart, xStop, yStop: Integer;
  c: TFPColor;
  SrcRawImage, DestRawImage: TRawImage;
begin
  ASource.GetRawImage(SrcRawImage);
  ADest.GetRawImage(DestRawImage);

  if DestRawImage.Description.IsEqual(SrcRawImage.Description) and (XDst =  0) and (YDst = 0) then
  begin
    // same description -> copy
    if DestRawImage.Data <> nil then
      System.Move(SrcRawImage.Data^, DestRawImage.Data^, DestRawImage.DataSize);
    if DestRawImage.Mask <> nil then
      System.Move(SrcRawImage.Mask^, DestRawImage.Mask^, DestRawImage.MaskSize);
    Exit;
  end;

  // copy pixels
  XStart := IfThen(XDst < 0, -XDst, 0);
  YStart := IfThen(YDst < 0, -YDst, 0);
  XStop := IfThen(ADest.Width - XDst < ASource.Width, ADest.Width - XDst, ASource.Width) - 1;
  YStop := IfTHen(ADest.Height - YDst < ASource.Height, ADest.Height - YDst, ASource.Height) - 1;

  SrcHasMask := SrcRawImage.Description.MaskBitsPerPixel > 0;
  DstHasMask := DestRawImage.Description.MaskBitsPerPixel > 0;

  if DstHasMask then begin
    for y:= yStart to yStop do
      for x:=xStart to xStop do
        ADest.Masked[x+XDst,y+YDst] := SrcHasMask and ASource.Masked[x,y];
  end;

  for y:=yStart to yStop do
    for x:=xStart to xStop do
    begin
      c := ASource.Colors[x,y];
      if not DstHasMask and SrcHasMask and (c.alpha = $FFFF) then // copy mask to alpha channel
        if ASource.Masked[x,y] then
          c.alpha := 0;

      ADest.Colors[x+XDst,y+YDst] := c;
      if AlphaMask and (c.alpha < AlphaTreshold) then
        ADest.Masked[x+XDst,y+YDst] := True;
    end;
end;
{$ENDIF}

Type

  { TDrawObjJob }

  TDrawObjJob = Class(TJob)
  private
    AllRun : boolean;
    Viewer : TMapView;
    FRunning : boolean;
    FLst : TGPSObjList;
    FStates : Array of integer;
    FArea : TRealArea;
  protected
    function pGetTask : integer;override;
    procedure pTaskStarted(aTask: integer);override;
    procedure pTaskEnded(aTask : integer;aExcept : Exception);override;
  public
    procedure ExecuteTask(aTask : integer;FromWaiting : boolean);override;
    function Running : boolean;override;
  public
    Constructor Create(aViewer : TMapView;aLst : TGPSObjList;const aArea : TRealArea);
    destructor Destroy;override;
  end;

{ TDrawObjJob }

function TDrawObjJob.pGetTask: integer;
var i : integer;
begin
  if not(AllRun) and not(Cancelled) then
  Begin
    For i:=low(FStates) to high(FStates) do
        if FStates[i]=0 then
        Begin
          result:=i+1;
          Exit;
        end;
    AllRun:=True;
  end;
  Result:=ALL_TASK_COMPLETED;
  For i:=low(FStates) to high(FStates) do
      if FStates[i]=1 then
      Begin
          Result:=NO_MORE_TASK;
          Exit;
      end;
end;

procedure TDrawObjJob.pTaskStarted(aTask: integer);
begin
  FRunning:=True;
  FStates[aTask-1]:=1;
end;

procedure TDrawObjJob.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  if Assigned(aExcept) then
    FStates[aTask-1]:=3
  Else
    FStates[aTask-1]:=2;
end;

procedure TDrawObjJob.ExecuteTask(aTask: integer; FromWaiting: boolean);
var iObj : integer;
    Obj : TGpsObj;
begin
    iObj:=aTask-1;
    Obj:=FLst[iObj];
    if Obj.InheritsFrom(TGPSTrack) then
    Begin
      Viewer.DrawTrk(FArea,TGPSTrack(Obj));
    End;
    if Obj.InheritsFrom(TGPSPoint) then
    Begin
      Viewer.DrawPt(FArea,TGPSPoint(Obj));
    end;
end;

function TDrawObjJob.Running: boolean;
begin
  Result := FRunning;
end;

constructor TDrawObjJob.Create(aViewer: TMapView; aLst: TGPSObjList;
  const aArea: TRealArea);
begin
  FArea := aArea;
  FLst := aLst;
  SetLEngth(FStates,FLst.Count);
  Viewer := aViewer;
  AllRun := false;
  Name := 'DrawObj';
end;

destructor TDrawObjJob.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLst);
  if not(Cancelled) then
    Viewer.CallAsyncInvalidate;
end;


{ TMapView }

procedure TMapView.SetActive(AValue: boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if FActive then
    ActivateEngine
  else
    Engine.Active := false;
end;

function TMapView.GetCacheOnDisk: boolean;
begin
  Result := Engine.CacheOnDisk;
end;

function TMapView.GetCachePath: String;
begin
  Result := Engine.CachePath;
end;

function TMapView.GetCenter: TRealPoint;
begin
  Result := Engine.Center;
end;

function TMapView.GetMapProvider: String;
begin
  result := Engine.MapProvider;
end;

function TMapView.GetOnCenterMove: TNotifyEvent;
begin
  result := Engine.OnCenterMove;
end;

function TMapView.GetOnChange: TNotifyEvent;
begin
  Result := Engine.OnChange;
end;

function TMapView.GetOnZoomChange: TNotifyEvent;
begin
  Result := Engine.OnZoomChange;
end;

function TMapView.GetUseThreads: boolean;
begin
  Result := Engine.UseThreads;
end;

function TMapView.GetZoom: integer;
begin
  result := Engine.Zoom;
end;

procedure TMapView.SetCacheOnDisk(AValue: boolean);
begin
  Engine.CacheOnDisk := AValue;
end;

procedure TMapView.SetCachePath(AValue: String);
begin
  Engine.CachePath := CachePath;
end;

procedure TMapView.SetCenter(AValue: TRealPoint);
begin
  Engine.Center := AValue;
end;

procedure TMapView.SetInactiveColor(AValue: TColor);
begin
  if FInactiveColor = AValue then
    exit;
  FInactiveColor := AValue;
  if not IsActive then
    Invalidate;
end;

procedure TMapView.ActivateEngine;
begin
  Engine.SetSize(ClientWidth,ClientHeight);
  Engine.Active := IsActive;
end;

procedure TMapView.SetMapProvider(AValue: String);
begin
  Engine.MapProvider := AValue;
end;

procedure TMapView.SetOnCenterMove(AValue: TNotifyEvent);
begin
  Engine.OnCenterMove := AValue;
end;

procedure TMapView.SetOnChange(AValue: TNotifyEvent);
begin
  Engine.OnChange := AValue;
end;

procedure TMapView.SetOnZoomChange(AValue: TNotifyEvent);
begin
  Engine.OnZoomChange := AValue;
end;

procedure TMapView.SetUseThreads(AValue: boolean);
begin
  Engine.UseThreads := aValue;
end;

procedure TMapView.SetZoom(AValue: integer);
begin
  Engine.Zoom := AValue;
end;

function TMapView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if IsActive then
    Engine.MouseWheel(self,Shift,WheelDelta,MousePos,Result);
end;

procedure TMapView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if IsActive then
    Engine.MouseDown(self,Button,Shift,X,Y);
end;

procedure TMapView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if IsActive then
    Engine.MouseUp(self,Button,Shift,X,Y);
end;

procedure TMapView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aPt: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if IsActive then
    Engine.MouseMove(self,Shift,X,Y);
end;

procedure TMapView.DblClick;
begin
  inherited DblClick;
  if IsActive then
    Engine.DblClick(self);
end;

procedure TMapView.DoOnResize;
begin
  inherited DoOnResize;
  //cancel all rendering threads
  Engine.CancelCurrentDrawing;
  FreeAndNil(Buffer);
  {$IFDEF USE_RGBGRAPHICS}
  Buffer := TRGB32Bitmap.Create(ClientWidth,ClientHeight);
  {$ENDIF}
  {$IFDEF USE_LAZINTFIMAGE}
  BufferCanvas.Free;
  CreateLazIntfImageAndCanvas(Buffer, BufferCanvas, ClientWidth, ClientHeight);
  {$ENDIF}
  if IsActive then
    Engine.SetSize(ClientWidth, ClientHeight);
end;

procedure TMapView.Paint;
var
  bmp: TBitmap;
begin
  inherited Paint;
  if IsActive and Assigned(Buffer) then
  begin
    {$IFDEF USE_RGBGRAPHICS}
    Buffer.Canvas.DrawTo(Canvas,0,0);
    {$ENDIF}
    {$IFDEF USE_LAZINTFIMAGE}
    bmp := TBitmap.Create;
    try
      bmp.SetSize(Buffer.Width, Buffer.Height);
      bmp.LoadFromIntfImage(Buffer);
      Canvas.Draw(0, 0, bmp);
    finally
      bmp.Free;
    end;
    {$ENDIF}
  end
  else
  begin
    Canvas.Brush.Color:=InactiveColor;
    Canvas.Brush.Style:=bsSolid;
    Canvas.FillRect(0,0,ClientWidth,ClientHeight);
  end;
end;

procedure TMapView.OnGPSItemsModified(Sender: TObject; objs: TGPSObjList;
  Adding: boolean);
var
  Area,ObjArea,vArea: TRealArea;
begin
  if Adding and assigned(Objs) then
  begin
    ObjArea := GetAreaOf(Objs);
    vArea := GetVisibleArea;
    if hasIntersectArea(ObjArea,vArea) then
    begin
      Area:=IntersectArea(ObjArea,vArea);
      Engine.Jobqueue.AddJob(TDrawObjJob.Create(self,Objs,Area),Engine);
    end
    else
      objs.Free;
  end
  else
  begin
    Engine.Redraw;
    Objs.free;
  end;
end;

procedure TMapView.DrawTrk(const Area : TRealArea;trk : TGPSTrack);
var Old,New : TPoint;
    i : integer;
    aPt : TRealPoint;
    LastInside,IsInside : boolean;
    trkColor : TColor;
Begin
     if trk.Points.Count>0 then
     Begin
       trkColor:=clRed;
       if trk.ExtraData<>nil then
       Begin
         if trk.ExtraData.inheritsFrom(TDrawingExtraData) then
           trkColor:=TDrawingExtraData(trk.ExtraData).Color;
       end;
       LastInside:=false;
       For i:=0 to pred(trk.Points.Count) do
       Begin
           aPt:=trk.Points[i].RealPoint;
           IsInside:=PtInsideArea(aPt,Area);
           if IsInside or LastInside then
           Begin
             New:=Engine.LonLatToScreen(aPt);
             if i>0 then
             Begin
               if not(LastInside) then
                 Old:=Engine.LonLatToScreen(trk.Points[pred(i)].RealPoint);
               {$IFDEF USE_RGBGRAPHICS}
               Buffer.Canvas.OutlineColor := trkColor;
               Buffer.Canvas.Line(Old.X, Old.y, New.X, New.Y);
               {$ENDIF}
               {$IFDEF USE_LAZINTFIMAGE}
               BufferCanvas.Pen.FPColor := TColorToFPColor(trkColor);
               BufferCanvas.Line(Old.X, Old.Y, New.X, New.Y);
               {$ENDIF}
             end;
             Old := New;
             LastInside := IsInside;
           end;
       end;
     end;
end;

procedure TMapView.DrawPt(const Area: TRealArea; aPOI: TGPSPoint);
var
  PT : TPoint;
  PtColor : TColor;
begin
  Pt:=Engine.LonLatToScreen(aPOI.RealPoint);
  PtColor:=clRed;
  if aPOI.ExtraData<>nil then
  Begin
     if aPOI.ExtraData.inheritsFrom(TDrawingExtraData) then
       PtColor:=TDrawingExtraData(aPOI.ExtraData).Color;
  end;
  {$IFDEF USE_RGBGRAPHICS}
  Buffer.canvas.OutlineColor:=ptColor;
  Buffer.canvas.Line(Pt.X,Pt.y-5,Pt.X,Pt.Y+5);
  Buffer.canvas.Line(Pt.X-5,Pt.y,Pt.X+5,Pt.Y);
  {$ENDIF}
  {$IFDEF USE_LAZINTFIMAGE}
  BufferCanvas.Pen.FPColor := TColorToFPColor(ptColor);
  BufferCanvas.Line(Pt.X, Pt.Y-5, Pt.X, Pt.Y+5);
  BufferCanvas.Line(Pt.X-5, Pt.Y, Pt.X+5, Pt.Y);
  {$ENDIF}

//  Buffer.Draw();
end;

procedure TMapView.CallAsyncInvalidate;
Begin
  if not(AsyncInvalidate) then
  Begin
    AsyncInvalidate:=true;
    Engine.Jobqueue.QueueAsyncCall(@DoAsyncInvalidate,0);
  end;
end;

procedure TMapView.DrawObjects(const TileId: TTileId; aLeft, aTop,aRight,aBottom: integer);
var aPt : TPoint;
    Area : TRealArea;
    lst  : TGPSObjList;
    i : integer;
    trk : TGPSTrack;
begin
  aPt.X:=aLeft;
  aPt.Y:=aTop;
  Area.TopLeft:=Engine.ScreenToLonLat(aPt);
  aPt.X:=aRight;
  aPt.Y:=aBottom;
  Area.BottomRight:=Engine.ScreenToLonLat(aPt);
  if GPSItems.count>0 then
  begin
    lst:=GPSItems.GetObjectsInArea(Area);
    if lst.Count>0 then
      Engine.Jobqueue.AddJob(TDrawObjJob.Create(self,lst,Area),Engine)
    else
    begin
        freeAndNil(Lst);
        CallAsyncInvalidate;
    end;
  end
  Else
    CallAsyncInvalidate;
end;

procedure TMapView.DoAsyncInvalidate(Data: PtrInt);
Begin
  Invalidate;
  AsyncInvalidate:=false;
end;

procedure TMapView.DoDrawTile(const TileId: TTileId; X, Y: integer;
  TileImg: TLazIntfImage);
var
  {$IFDEF USE_RGBGRAPHICS}
  temp : TRGB32Bitmap;
  ri : TRawImage;
  BuffLaz : TLazIntfImage;
  {$ENDIF}
  {$IFDEF USE_LAZINTFIMAGE}
  temp: TBitmap;
  {$ENDIF}
begin
  if Assigned(Buffer) then
  begin
    if Assigned(TileImg) then
    Begin
     {$IFDEF USE_RGBGRAPHICS}
      if (X>=0) and (Y>=0) then //http://mantis.freepascal.org/view.php?id=27144
      begin
        ri.Init;
        ri.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Buffer.Width,Buffer.Height);
        ri.Data:=Buffer.Pixels;
        BuffLaz := TLazIntfImage.Create(ri,false);
        try
          BuffLaz.CopyPixels(TileImg,X,y);
          ri.Init;
        finally
          FreeandNil(BuffLaz);
        end;
      end
      else
      begin
        //i think it take more memory then the previous method but work in all case
        temp:=TRGB32Bitmap.CreateFromLazIntfImage(TileImg);
        try
          Buffer.Draw(X,Y,temp);
        finally
          FreeAndNil(Temp);
        end;
      end;
     {$ENDIF}
     {$IFDEF USE_LAZINTFIMAGE}
      {$IF LCL_FULLVERSION < 1090000}
      { Workaround for //http://mantis.freepascal.org/view.php?id=27144 }
      CopyPixels(TileImg, Buffer, X, Y);
      {$ELSE}
      Buffer.CopyPixels(TileImg, X, Y);
      {$IFEND}
     {$ENDIF}
    end
    else
    {$IFDEF USE_RGBGRAPHICS}
      Buffer.Canvas.FillRect(X,Y,X+TILE_SIZE,Y+TILE_SIZE);
    {$ENDIF}
    {$IFDEF USE_LAZINTFIMAGE}
     begin
       BufferCanvas.Brush.FPColor := ColWhite;
       BufferCanvas.FillRect(X, Y, X + TILE_SIZE, Y + TILE_SIZE);
     end;
    {$ENDIF}
  end;
  DrawObjects(TileId,X,Y,X+TILE_SIZE,Y+TILE_SIZE);
end;

function TMapView.IsActive: Boolean;
begin
  if not(csDesigning in ComponentState) then
    Result:=FActive
  else
    Result:=false;
end;

constructor TMapView.Create(AOwner: TComponent);
begin
  Active := false;
  FGPSItems := TGPSObjectList.Create;
  FGPSItems.OnModified := @OnGPSItemsModified;
  FInactiveColor := clWhite;
  FEngine := TMapViewerEngine.Create(self);
  FdownloadEngine := TMvDEFpc.Create(self);
  {$IFDEF USE_RGBGRAPHICS}
  Buffer := TRGB32Bitmap.Create(Width,Height);
  {$ENDIF}
  {$IFDEF USE_LAZINTFIMAGE}
  CreateLazIntfImageAndCanvas(Buffer, BufferCanvas, Width, Height);
  {$ENDIF}
  Engine.CachePath := 'cache/';
  Engine.CacheOnDisk := true;
  Engine.OnDrawTile := @DoDrawTile;
  Engine.DrawTitleInGuiThread := false;
  Engine.DownloadEngine := FDownloadengine;
  inherited Create(AOwner);
  Width := 150;
  Height := 150;
end;

destructor TMapView.Destroy;
begin
  {$IFDEF USE_LAZINTFIMAGE}
  BufferCanvas.Free;
  {$ENDIF}
  Buffer.Free;
  inherited Destroy;
  FreeAndNil(FGPSItems);
end;

{$IFDEF USE_LAZINTFIMAGE}
procedure TMapView.CreateLazIntfImageAndCanvas(out ABuffer: TLazIntfImage;
  out ACanvas: TFPCustomCanvas; AWidth, AHeight: Integer);
var
  rawImg: TRawImage;
begin
  rawImg.Init;
  rawImg.Description.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight);
  rawImg.CreateData(True);
  ABuffer := TLazIntfImage.Create(rawImg, true);
  ACanvas := TFPImageCanvas.Create(ABuffer);
  ACanvas.Brush.FPColor := colWhite;
  ACanvas.FillRect(0, 0, AWidth, AHeight);
end;
{$ENDIF}

function TMapView.ScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result:=Engine.ScreenToLonLat(aPt);
end;

function TMapView.LonLatToScreen(aPt: TRealPoint): TPoint;
begin
  Result:=LonLatToScreen(aPt);
end;

procedure TMapView.GetMapProviders(lstProviders: TStrings);
begin
  Engine.GetMapProviders(lstProviders);
end;

procedure TMapView.WaitEndOfRendering;
begin
  Engine.Jobqueue.WaitAllJobTerminated(Engine);
end;

procedure TMapView.CenterOnObj(obj: TGPSObj);
var Area : TRealArea;
    Pt : TRealPoint;
begin
  obj.GetArea(Area);
  Pt.Lon:=(Area.TopLeft.Lon+Area.BottomRight.Lon) /2;
  Pt.Lat:=(Area.TopLeft.Lat+Area.BottomRight.Lat) /2;
  Center:=Pt;
end;

procedure TMapView.ZoomOnObj(obj: TGPSObj);
var Area : TRealArea;
begin
  obj.GetArea(Area);
  Engine.ZoomOnArea(Area);
end;

procedure TMapView.ZoomOnArea(const aArea: TRealArea);
begin
  Engine.ZoomOnArea(aArea);
end;

function TMapView.GetVisibleArea: TRealArea;
var aPt : TPoint;
begin
  aPt.X:=0;
  aPt.Y:=0;
  Result.TopLeft:=Engine.ScreenToLonLat(aPt);
  aPt.X:=Width;
  aPt.Y:=Height;
  Result.BottomRight:=Engine.ScreenToLonLat(aPt);;
end;

procedure TMapView.ClearBuffer;
begin
  {$IFDEF USE_LAZINTFIMAGE}
  CreateLazIntfImageAndCanvas(Buffer, BufferCanvas, ClientWidth, ClientHeight);
  {$ENDIF}
end;

end.

