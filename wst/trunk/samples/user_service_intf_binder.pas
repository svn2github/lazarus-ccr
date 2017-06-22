{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf_binder".
  Date            : "22/06/2017 15:01:59".
}
unit user_service_intf_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, user_service_intf;


type
  TUserService_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure GetListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure UpdateHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure FindHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DeleteHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

type
  TUserService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterUserServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TUserService_ServiceBinder implementation }
procedure TUserService_ServiceBinder.GetListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : UserService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  returnVal : TUserArray;
begin
  callCtx := AContext;
  returnVal := nil;
  
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetList();
    if (returnVal <> nil) then
      callCtx.AddObjectToFree(returnVal);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(TUserArray),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TUserService_ServiceBinder.AddHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : UserService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  AUser : TUser;
begin
  callCtx := AContext;
  AUser := nil;
  
  locStrPrmName := 'AUser';  AFormatter.Get(TypeInfo(TUser),locStrPrmName,AUser);
  if (AUser <> nil) then
    callCtx.AddObjectToFree(AUser);
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.Add(AUser);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TUserService_ServiceBinder.UpdateHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : UserService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  AUser : TUser;
begin
  callCtx := AContext;
  AUser := nil;
  
  locStrPrmName := 'AUser';  AFormatter.Get(TypeInfo(TUser),locStrPrmName,AUser);
  if (AUser <> nil) then
    callCtx.AddObjectToFree(AUser);
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.Update(AUser);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TUserService_ServiceBinder.FindHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : UserService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  AName : UnicodeString;
  returnVal : TUser;
begin
  callCtx := AContext;
  returnVal := nil;
  
  locStrPrmName := 'AName';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,AName);
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.Find(AName);
    if (returnVal <> nil) then
      callCtx.AddObjectToFree(returnVal);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(TUser),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TUserService_ServiceBinder.DeleteHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : UserService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  AName : UnicodeString;
  returnVal : boolean;
begin
  callCtx := AContext;
  
  locStrPrmName := 'AName';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,AName);
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.Delete(AName);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(boolean),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TUserService_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('UserService'));
  RegisterVerbHandler('GetList',{$IFDEF FPC}@{$ENDIF}GetListHandler);
  RegisterVerbHandler('Add',{$IFDEF FPC}@{$ENDIF}AddHandler);
  RegisterVerbHandler('Update',{$IFDEF FPC}@{$ENDIF}UpdateHandler);
  RegisterVerbHandler('Find',{$IFDEF FPC}@{$ENDIF}FindHandler);
  RegisterVerbHandler('Delete',{$IFDEF FPC}@{$ENDIF}DeleteHandler);
end;


{ TUserService_ServiceBinderFactory }

function TUserService_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TUserService_ServiceBinderFactory.Create();
begin
  FInstance := TUserService_ServiceBinder.Create() as IInterface;
end;

destructor TUserService_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterUserServiceService();
Begin
  GetServerServiceRegistry().Register('UserService',TUserService_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$i user_service_intf.wst}

  {$IF DECLARED(Register_user_service_intf_ServiceMetadata)}
  Register_user_service_intf_ServiceMetadata();
  {$IFEND}

End.
