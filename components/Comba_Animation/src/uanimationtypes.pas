unit uanimationtypes;

(*
  Comba - Animation controls helper
  ---------------------------------
  @Licence: (c) 2017 José Mejuto // joshyfun at gmail.com
  @Licence: LGPL when compiled with FPC (Free Pascal), GNU GPL V3 in other cases.
  @Links:
     GPL:  https://www.gnu.org/licenses/gpl-3.0.en.html
     LGPL: https://www.gnu.org/licenses/lgpl-3.0.en.html

  @Description:
     Helper unit.

*)

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

type

  { TAnimationRect }

  TAnimationRect=record
  private
    function GetBottom: integer;
    function GetRight: integer;
    procedure SetBottom(aValue: integer);
    procedure SetRight(aValue: integer);
  public
    Left: integer;
    Top: integer;
    Width: integer;
    Height: integer;
    property  Right: integer read GetRight write SetRight;
    property  Bottom: integer read GetBottom write SetBottom;
    function  GetAsRect: TRect;
    function  GetCenter: TPoint;
    procedure SetFromRect(const aRect: TRect);
    procedure MoveTo(const aNewLeftTop: TPoint);
  end;

implementation

{ TAnimationRect }

function TAnimationRect.GetBottom: integer;
begin
  Result:=Top+Height;
end;

function TAnimationRect.GetRight: integer;
begin
  Result:=Left+Width;
end;

procedure TAnimationRect.SetBottom(aValue: integer);
begin
  Height:=aValue-Top;
end;

procedure TAnimationRect.SetRight(aValue: integer);
begin
  Width:=aValue-Left;
end;

function TAnimationRect.GetAsRect: TRect;
begin
  Result.Left:=Left;
  Result.Right:=Left+Width;
  Result.Top:=Top;
  Result.Bottom:=Top+Height;
end;

function TAnimationRect.GetCenter: TPoint;
begin
  Result.x:=Left+(Width div 2);
  Result.y:=Top+(Height div 2);
end;

procedure TAnimationRect.SetFromRect(const aRect: TRect);
begin
  Left:=aRect.Left;
  Top:=aRect.Top;
  Width:=aRect.Right-aRect.Left;
  Height:=aRect.Bottom-aRect.Top;
end;

procedure TAnimationRect.MoveTo(const aNewLeftTop: TPoint);
begin
  Left:=aNewLeftTop.x;
  Top:=aNewLeftTop.y;
end;

end.

