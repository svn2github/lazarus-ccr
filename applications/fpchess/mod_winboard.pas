unit mod_winboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, Forms, Controls,
  chessmodules, chessgame, chessdrawer;

type

  { TWinboardChessModule }

  TWinboardChessModule = class(TChessModule)
  private
    textEnginePatch : TStaticText;
    editEnginePatch : TEdit;
  public
    constructor Create; override;
    procedure CreateUserInterface(); override;
    procedure ShowUserInterface(AParent: TWinControl); override;
    procedure HideUserInterface(); override;
    procedure FreeUserInterface(); override;
    procedure PrepareForGame(); override;
    function  GetSecondPlayerName(): ansistring; override;
    procedure HandleOnMove(AFrom, ATo: TPoint); override;
    procedure HandleOnTimer(); override;
  end;

implementation
uses winboardConn;

{ TSinglePlayerChessModule }

constructor TWinboardChessModule.Create;
begin
  inherited Create;

  SelectionDescription := 'Play against a winboard engine';
  PlayingDescription := 'Play against a winboard engine';
  Kind := cmkAgainstComputer;
end;

procedure TWinboardChessModule.CreateUserInterface;
begin
  textEnginePatch := TStaticText.Create(nil);
  textEnginePatch.SetBounds(20, 20, 180, 50);
  textEnginePatch.Caption := 'Full patch to the engine';

  editEnginePatch := TEdit.Create(nil);
  editEnginePatch.SetBounds(200, 20, 150, 50);
  editEnginePatch.Text := 'gnuchess';
end;

procedure TWinboardChessModule.ShowUserInterface(AParent: TWinControl);
begin
  textEnginePatch.Parent := AParent;
  editEnginePatch.Parent := AParent;
end;

procedure TWinboardChessModule.HideUserInterface();
begin
  textEnginePatch.Parent := nil;
  editEnginePatch.Parent := nil;
end;

procedure TWinboardChessModule.FreeUserInterface();
begin
  textEnginePatch.Free;
  editEnginePatch.Free;
end;

function TWinboardChessModule.GetSecondPlayerName(): ansistring;
begin
  Result := editEnginePatch.text;
end;

procedure TWinboardChessModule.HandleOnMove(AFrom, ATo: TPoint);
var
  moveStr : String;
begin
  moveStr:=vwinboardConn.coordToString(AFrom,ATo,vChessGame.PreviousMove.PieceMoved,vChessGame.PreviousMove.PieceCaptured);
  vwinboardConn.tellMove(moveStr);
end;

procedure TWinboardChessModule.HandleOnTimer;
var
  CompMove: moveInCoord;
  lAnimation: TChessMoveAnimation;
begin
  CompMove:=vwinboardConn.engineMove;

  if (CompMove <> nilCoord) then
  begin
    lAnimation := TChessMoveAnimation.Create;
    lAnimation.AFrom := CompMove[1];
    lAnimation.ATo := CompMove[2];
    vChessDrawer.AddAnimation(lAnimation);
  end;
end;

procedure TWinboardChessModule.PrepareForGame;
begin
  vwinboardConn.startEngine(editEnginePatch.text);
  if not vChessGame.FirstPlayerIsWhite then
  begin
    vwinboardConn.tellMove('go');
  end;
end;

initialization
  RegisterChessModule(TWinboardChessModule.Create);
finalization
  vwinboardConn.stopEngine(True);
end.
