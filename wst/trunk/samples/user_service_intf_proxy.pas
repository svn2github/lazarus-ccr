{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf_proxy".
  Date            : "02/05/2007 20:07".
}
Unit user_service_intf_proxy;
{$mode objfpc}{$H+}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, user_service_intf;

Type


  TUserService_Proxy=class(TBaseProxy,UserService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function GetList():TUserArray;
    procedure Add(
      Const AUser : TUser
    );
    procedure Update(
      Const AUser : TUser
    );
    function Find(
      Const AName : string
    ):TUser;
    function Delete(
      Const AName : string
    ):boolean;
  End;

  Function wst_CreateInstance_UserService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'):UserService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_UserService(const AFormat : string; const ATransport : string):UserService;
Begin
  Result := TUserService_Proxy.Create('UserService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(UserService)),ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(UserService)));
End;

{ TUserService_Proxy implementation }

class function TUserService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(UserService);
end;

function TUserService_Proxy.GetList():TUserArray;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetList', GetTarget(),(Self as ICallContext));
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(TUserArray), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TUserService_Proxy.Add(
  Const AUser : TUser
);
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Add', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AUser', TypeInfo(TUser), AUser);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));

  Finally
    locSerializer.Clear();
  End;
End;

procedure TUserService_Proxy.Update(
  Const AUser : TUser
);
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Update', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AUser', TypeInfo(TUser), AUser);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));

  Finally
    locSerializer.Clear();
  End;
End;

function TUserService_Proxy.Find(
  Const AName : string
):TUser;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Find', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AName', TypeInfo(string), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(TUser), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TUserService_Proxy.Delete(
  Const AName : string
):boolean;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Delete', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AName', TypeInfo(string), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(boolean), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i user_service_intf.wst}

  {$IF DECLARED(Register_user_service_intf_ServiceMetadata)}
  Register_user_service_intf_ServiceMetadata();
  {$ENDIF}
End.
