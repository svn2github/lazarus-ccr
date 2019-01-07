unit uMySubComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TMySubComponent = class(TComponent)
  private
    { Private declarations }
    fSubproperty1,fSubproperty2,fSubproperty3,fSubproperty4:String;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override; // Constructor must be public
    destructor Destroy; override; // Destructor must be public
    procedure DoSomething;
  published
    { Published declarations }
    property Subproperty1:String read fSubproperty1 write fSubproperty1;
    property Subproperty2:String read fSubproperty2 write fSubproperty2;
    property Subproperty3:String read fSubproperty3 write fSubproperty3;
    property Subproperty4:String read fSubproperty4 write fSubproperty4;
  end;

procedure Register;

implementation
procedure TMySubComponent.DoSomething;
begin
    ShowMessageFmt('TMySubComponent.DoSomething called: Subproperty1=%s',[fSubproperty1]);
end;

Constructor TMySubComponent.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 fSubproperty1:='One';
 fSubproperty2:='Two';
end;
destructor TMySubComponent.Destroy;
begin
  // Clean-up code goes here
  // FreeandNil any user-created objects here
  inherited Destroy;
end;
procedure Register;
begin
  {$I umysubcomponent_icon.lrs}
  RegisterComponents('Additional',[TMySubComponent]);
end;

end.
