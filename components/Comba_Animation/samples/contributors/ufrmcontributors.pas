unit ufrmcontributors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls
  , uanimationbasic, uanimationcontributors;

type

  { TfrmContributors }

  TfrmContributors = class(TForm)
    imgLazarus: TImage;
    tmrAnimationCadence: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrAnimationCadenceTimer(Sender: TObject);
  private
  protected
    AnimationQueue: TAboutAnimation;
  public

  end;

var
  frmContributors: TfrmContributors;

implementation

{$R *.lfm}

{ TfrmContributors }

procedure TfrmContributors.FormCreate(Sender: TObject);
begin
  imgLazarus.Canvas.Clear;
  AnimationQueue:=TAboutAnimation.Create(imgLazarus.Picture.Bitmap);

  AnimationQueue.Start(true);
end;

procedure TfrmContributors.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AnimationQueue.Pause;
  CloseAction:=caFree;
end;

procedure TfrmContributors.FormDestroy(Sender: TObject);
begin
  FreeAndNil(AnimationQueue);
end;

procedure TfrmContributors.tmrAnimationCadenceTimer(Sender: TObject);
begin
  AnimationQueue.MousePosition:=imgLazarus.ScreenToClient(Mouse.CursorPos);
  AnimationQueue.Animate;
//  Self.Repaint;
//  Self.Caption:=format('FPS: %.2f',[AnimationQueue.AverageFPS]);
end;

end.

