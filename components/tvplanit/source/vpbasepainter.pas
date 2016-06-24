unit VpBasePainter;

interface

uses
  Classes, Graphics, LCLType,
  VPBase;

type

  { TVpBasePainter }

  TVpBasePainter = class
  protected
    // Buffered input parameters
    RenderCanvas: TCanvas;
    Angle: TVpRotationAngle;
    Scale: Extended;
    RenderDate: TDateTime;
    RenderIn: TRect;
    StartLine: Integer;
    StopLine: Integer;
    UseGran: TVpGranularity;
    DisplayOnly: Boolean;
  protected
    // Shared local variables of all the old painting routines
    Rgn: HRGN;
    RealWidth: Integer;
    RealHeight: Integer;
    RealLeft: Integer;
    RealRight: Integer;
    RealTop: Integer;
    RealBottom: Integer;
    SaveBrushColor: TColor;
    SavePenStyle: TPenStyle;
    SavePenColor: TColor;
    procedure InitPenBrush; virtual;
    procedure SavePenBrush; virtual;
    procedure RestorePenBrush; virtual;
    procedure SetMeasurements; virtual;
  public
    constructor Create(ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AGranularity: TVpGranularity; ADisplayOnly: Boolean); virtual;
  end;

implementation

uses
  VpCanvasUtils;

{ TBasePainter }

constructor TVpBasePainter.Create(ARenderCanvas: TCanvas);
begin
  RenderCanvas := ARenderCanvas;
end;

procedure TVpBasePainter.InitPenBrush;
begin
  RenderCanvas.Pen.Style := psSolid;
  RenderCanvas.Pen.Width := 1;
  RenderCanvas.Pen.Mode := pmCopy;
  RenderCanvas.Brush.Style := bsSolid;
end;

procedure TVpBasePainter.RenderToCanvas(ARenderIn: TRect;
  AAngle: TVpRotationAngle; AScale: Extended; ARenderDate: TDateTime;
  AStartLine, AStopLine: Integer; AGranularity: TVpGranularity;
  ADisplayOnly: Boolean);
begin
  // Buffer parameters
  RenderIn := ARenderIn;
  Angle := AAngle;
  Scale := AScale;
  RenderDate := ARenderDate;
  StartLine := AStartLine;
  StopLine := AStopLine;
  UseGran := AGranularity;
  DisplayOnly := ADisplayOnly;

  // call the old RenderToCanvas method here...
end;

procedure TVpBasePainter.RestorePenBrush;
begin
  RenderCanvas.Pen.Style := SavePenStyle;
  RenderCanvas.Brush.Color := SaveBrushColor;
  RenderCanvas.Pen.Color := SavePenColor;
end;

procedure TVpBasePainter.SavePenBrush;
begin
  SavePenStyle := RenderCanvas.Pen.Style;
  SaveBrushColor := RenderCanvas.Brush.Color;
  SavePenColor := RenderCanvas.Pen.Color;
end;

procedure TVpBasePainter.SetMeasurements;
begin
  RealWidth := TPSViewportWidth(Angle, RenderIn);
  RealHeight := TPSViewportHeight(Angle, RenderIn);
  RealLeft := TPSViewportLeft(Angle, RenderIn);
  RealRight := TPSViewportRight(Angle, RenderIn);
  RealTop := TPSViewportTop(Angle, RenderIn);
  RealBottom := TPSViewportBottom(Angle, RenderIn);
end;

end.
