unit mbBasicPicker;

{$mode objfpc}{$H+}

interface

uses
 {$IFDEF FPC}
  LMessages,
 {$ELSE}
  Messages,
 {$ENDIF}
  Classes, SysUtils, Graphics, Controls;

type
  TmbBasicPicker = class(TCustomControl)
  protected
    procedure PaintParentBack; virtual; overload;
    procedure PaintParentBack(ACanvas: TCanvas); overload;
    procedure PaintParentBack(ABitmap: TBitmap); overload;
    {$IFDEF DELPHI}
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ELSE}
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultColor(const DefaultColorType: TDefaultColorType): TColor; override;
  published
    property ParentColor default true;
  end;

implementation

constructor TmbBasicPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  ParentColor := true;
end;

procedure TmbBasicPicker.CMParentColorChanged(var Message: TLMessage);
begin
  if ParentColor then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  inherited;
end;

function TmbBasicPicker.GetDefaultColor(const DefaultColorType: TDefaultColorType): TColor;
begin
  result := inherited GetDefaultColor(DefaultColorType);
end;

procedure TmbBasicPicker.PaintParentBack;
begin
  PaintParentBack(Canvas);
end;

procedure TmbBasicPicker.PaintParentBack(ABitmap: TBitmap);
begin
  {$IFNDEF DELPHI}
  if Color = clDefault then
    ABitmap.Canvas.Brush.Color := GetDefaultColor(dctBrush)
  else
  {$ENDIF}
    ABitmap.Canvas.Brush.Color := Color;
  ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
  {$IFDEF DELPHI_7_UP}{$IFDEF DELPHI}
  if ParentBackground then
   with ThemeServices do
    if ThemesEnabled then
     begin
      MemDC := CreateCompatibleDC(0);
      OldBMP := SelectObject(MemDC, ABitmap.Handle);
      DrawParentBackground(Handle, MemDC, nil, False);
      if OldBMP <> 0 then SelectObject(MemDC, OldBMP);
      if MemDC <> 0 then DeleteDC(MemDC);
     end;
  {$ENDIF}{$ENDIF}
end;

procedure TmbBasicPicker.PaintParentBack(ACanvas: TCanvas);
var
  OffScreen: TBitmap;
begin
  Offscreen := TBitmap.Create;
  try
    Offscreen.PixelFormat := pf32bit;
    Offscreen.Width := Width;
    Offscreen.Height := Height;
    PaintParentBack(Offscreen);
    ACanvas.Draw(0, 0, Offscreen);
  finally
    Offscreen.Free;
  end;
end;

procedure TmbBasicPicker.WMEraseBkgnd(
  var Message: {$IFDEF DELPHI}TWMEraseBkgnd{$ELSE}TLMEraseBkgnd{$ENDIF} );
begin
  inherited;
//  Message.Result := 1;
end;

end.

