unit spkt_Dispatch;

{$mode delphi}

(*******************************************************************************
*                                                                              *
*  Plik: spkt_Dispatch.pas                                                     *
*  Opis: Bazowe klasy dyspozytorów poœrednicz¹cych pomiêdzy elementami         *
*        toolbara.                                                             *
*  Copyright: (c) 2009 by Spook.                                               *
*  License:   Modified LGPL (with linking exception, like Lazarus LCL)         *
'             See "license.txt" in this installation                           *
*                                                                              *
*******************************************************************************)

interface

uses
  Classes, Controls, Graphics,
  SpkMath;

type
  TSpkBaseDispatch = class abstract(TObject)
  private
  protected
  public
  end;

  TSpkBaseAppearanceDispatch = class abstract(TSpkBaseDispatch)
  public
    procedure NotifyAppearanceChanged; virtual; abstract;
  end;

  TSpkBaseToolbarDispatch = class abstract(TSpkBaseAppearanceDispatch)
  public
    procedure NotifyItemsChanged; virtual; abstract;
    procedure NotifyMetricsChanged; virtual; abstract;
    procedure NotifyVisualsChanged; virtual; abstract;
    function GetTempBitmap: TBitmap; virtual; abstract;
    function ClientToScreen(Point: T2DIntPoint): T2DIntPoint; virtual; abstract;
  end;

implementation

end.
