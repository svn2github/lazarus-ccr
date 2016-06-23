unit VpBasePainter;

interface

uses
  Classes, Graphics,
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
  public
    constructor Create(ARenderCanvas: TCanvas);
    procedure RenderToCanvas(ARenderIn: TRect; AAngle: TVpRotationAngle;
      AScale: Extended; ARenderDate: TDateTime; AStartLine, AStopLine: Integer;
      AGranularity: TVpGranularity; ADisplayOnly: Boolean); virtual;
  end;

implementation

{ TBasePainter }

constructor TVpBasePainter.Create(ARenderCanvas: TCanvas);
begin
  RenderCanvas := ARenderCanvas;
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

end.
