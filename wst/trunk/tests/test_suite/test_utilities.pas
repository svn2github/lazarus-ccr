{$INCLUDE wst_global.inc}
unit test_utilities;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  TypInfo,
  base_service_intf, server_service_intf;

type

  ITest = interface
    ['{61442DCF-0F6B-490F-AA33-FF856C07A757}']
    procedure SayHello();
    procedure DontPool();
  end;

  { TTestClass }

  TTestClass = class(TActivableServiceImplementation,IObjectControl,ITest)
  private
    FPooled : Boolean;
  protected
    procedure SayHello();
    function CanBePooled() : Boolean;
    procedure DontPool();
  public
    constructor Create();override;
  end;

  ISimple_A = interface
     ['{D015AD95-6062-4650-9B00-CF3004E9CA1A}']//['{4793180A-DAA4-4E50-9194-5EEEE851EBE3}']
  end;

  ISimple_B = interface
     ['{4793180A-DAA4-4E50-9194-5EEEE851EBE3}']
  end;

  TSimpleFactoryItem_A = class(TSimpleFactoryItem,IInterface,ISimple_A)
  end;

  TSimpleFactoryItem_B = class(TSimpleFactoryItem,IInterface,ISimple_B)
  end;

  { TTest_TIntfPoolItem }

  TTest_TIntfPoolItem = class(TTestCase)
  published
    procedure All();
  end;

  { TTest_TSimpleItemFactory }

  TTest_TSimpleItemFactory = class(TTestCase)
  published
    procedure CreateProc();
    procedure CreateInstance();
  end;
  
  { TTest_TIntfPool }

  TTest_TIntfPool= class(TTestCase)
  published
    procedure Create_ZEROS();
    procedure Create_NON_ZERO_MIN();
    procedure Release();
    procedure Release_NON();
    procedure Discard();
  end;
  
  { TTest_TSimpleItemFactoryEx }

  TTest_TSimpleItemFactoryEx = class(TTestCase)
  published
    procedure NOT_Pooled();
    procedure POOLED_Create_ZEROS();
    procedure POOLED_Release();
    procedure POOLED_Release_NON();
    procedure POOLED_Discard();
  end;

  { TTest_TImplementationFactory }

  TTest_TImplementationFactory = class(TTestCase)
  published
    procedure POOLED_Discard();
  end;

implementation

{ TTestClass }

procedure TTestClass.SayHello();
begin

end;

function TTestClass.CanBePooled() : Boolean;
begin
  Result := FPooled;
end;

procedure TTestClass.DontPool();
begin
  FPooled := False;
end;

constructor TTestClass.Create();
begin
  inherited Create();
  FPooled := True;
  _AddRef(); // not to allow the rtl to reuse the same memory for another instance of the same class!!
end;

{ TTest_TIntfPool }

procedure TTest_TIntfPool.Create_ZEROS();
var
  ok : Boolean;
  obj : TIntfPool;
begin
  ok := False;
  try
    obj := TIntfPool.Create(0,0,TSimpleItemFactory.Create(TTestClass));
  except
    ok := True;
  end;
  Check(ok);
end;

procedure TTest_TIntfPool.Create_NON_ZERO_MIN();
const MIN_A = Integer(1); MAX_A = Integer(5);
var
  obj : TIntfPool;
begin
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  CheckEquals(MIN_A,obj.Min);
  CheckEquals(MAX_A,obj.Max);
  CheckEquals(MIN_A,obj.GetInstancesCount());
end;

procedure TTest_TIntfPool.Release();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : TIntfPool;
  elt : ITest;
  i : Integer;
begin
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 0 to 300 do begin
    elt := obj.Get(0) as ITest;
    elt.SayHello();
    obj.Release(elt);
  end;
  
  FreeAndNil(obj);
  obj := TIntfPool.Create(MIN_B,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 0 to 300 do begin
    elt := obj.Get(0) as ITest;
    elt.SayHello();
    obj.Release(elt);
  end;
  
  FreeAndNil(obj);
  obj := TIntfPool.Create(MAX_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 0 to 300 do begin
    elt := obj.Get(0) as ITest;
    elt.SayHello();
    obj.Release(elt);
  end;
end;

procedure TTest_TIntfPool.Release_NON();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : TIntfPool;
  elt : ITest;
  i : Integer;
  ok : Boolean;
  il : IInterfaceList;
begin
  il := TInterfaceList.Create();
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  for i := 1 to MAX_A do begin
    elt := obj.Get(100) as ITest;
    elt.SayHello();
    il.Add(elt);
    //obj.Release(elt); do not release
  end;
  ok := False;
  try
    elt := obj.Get(100) as ITest;
  except
    ok := True;
  end;
  Check(ok);
  CheckEquals(MAX_A,obj.GetInstancesCount());
  for i := 0 to Pred(MAX_A) do begin
    obj.Release(il[0]);
    il.Delete(0);
  end;

  for i := 1 to 100 do begin
    elt := obj.Get(100) as ITest;
    elt.SayHello();
    il.Add(elt);
    obj.Release(elt);
  end;
end;

procedure TTest_TIntfPool.Discard();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : TIntfPool;
  oldElt, elt : ITest;
begin
  obj := TIntfPool.Create(MIN_A,MIN_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );
  
  FreeAndNil(obj);oldElt := nil; elt := nil;
  obj := TIntfPool.Create(MIN_A,MAX_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );
  
  FreeAndNil(obj);oldElt := nil; elt := nil;
  obj := TIntfPool.Create(MIN_B,MIN_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );

  FreeAndNil(obj);oldElt := nil; elt := nil;
  obj := TIntfPool.Create(MIN_B,MAX_A,TSimpleItemFactory.Create(TTestClass));
  elt := obj.Get(10) as ITest;
  oldElt := elt;
  obj.Release(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt = elt);
  obj.Discard(elt);
  elt := obj.Get(10) as ITest;
  Check(oldElt <> elt );
end;

{ TTest_TSimpleItemFactoryEx }

procedure TTest_TSimpleItemFactoryEx.NOT_Pooled();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  elt : ITest;
  i : Integer;
begin
  obj := TSimpleItemFactoryEx.Create(TTestClass);
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
  end;
  
  obj := TSimpleItemFactoryEx.Create(TTestClass,'');
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
  end;
end;

procedure TTest_TSimpleItemFactoryEx.POOLED_Create_ZEROS();
var
  ok : Boolean;
  obj : IItemFactoryEx;
begin
  ok := False;
  try
    obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[0,0]));
  except
    ok := True;
  end;
  Check(ok);
end;

procedure TTest_TSimpleItemFactoryEx.POOLED_Release();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  elt : ITest;
  i : Integer;
begin
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[MIN_A,MAX_A]));
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    obj.ReleaseInstance(elt);
  end;

  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[MIN_B,MAX_A]));
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    obj.ReleaseInstance(elt);
  end;

  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;Pooled=True',[MAX_A,MAX_A]));
  for i := 0 to 300 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    obj.ReleaseInstance(elt);
  end;
end;

procedure TTest_TSimpleItemFactoryEx.POOLED_Release_NON();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  elt : ITest;
  i : Integer;
  ok : Boolean;
  il : IInterfaceList;
begin
  il := TInterfaceList.Create();
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MAX_A]));
  for i := 1 to MAX_A do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    il.Add(elt);
    //obj.Release(elt); do not release
  end;
  ok := False;
  try
    elt := obj.CreateInstance() as ITest;
  except
    ok := True;
  end;
  Check(ok);
  for i := 0 to Pred(MAX_A) do begin
    obj.ReleaseInstance(il[0]);
    il.Delete(0);
  end;

  for i := 1 to 100 do begin
    elt := obj.CreateInstance() as ITest;
    elt.SayHello();
    il.Add(elt);
    obj.ReleaseInstance(elt);
  end;
end;

procedure TTest_TSimpleItemFactoryEx.POOLED_Discard();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  oldElt, elt : ITest;
begin
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'1.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt, '1.2' );

  oldElt := nil; elt := nil;
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'2.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'2.2');

  oldElt := nil; elt := nil;
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'3.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'3.2');

  oldElt := nil; elt := nil;
  obj := TSimpleItemFactoryEx.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'4.1');
  obj.DiscardInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt,'4.2');
end;

{ TTest_TImplementationFactory }

procedure TTest_TImplementationFactory.POOLED_Discard();
const MIN_A = Integer(1); MAX_A = Integer(5); MIN_B = Integer(0);
var
  obj : IItemFactoryEx;
  oldElt, elt : ITest;
begin
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'1.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt, '1.2' );

  oldElt := nil; elt := nil;
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_A,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'2.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'2.2');

  oldElt := nil; elt := nil;
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MIN_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'3.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt ,'3.2');

  oldElt := nil; elt := nil;
  obj := TImplementationFactory.Create(TTestClass,Format('PoolMin=%d;PoolMax=%d;TimeOut=100;Pooled=True',[MIN_B,MAX_A]));
  elt := obj.CreateInstance() as ITest;
  oldElt := elt;
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt = elt,'4.1');
  elt.DontPool();
  obj.ReleaseInstance(elt);
  elt := obj.CreateInstance() as ITest;
  Check(oldElt <> elt,'4.2');
end;

{ TTest_TIntfPoolItem }

procedure TTest_TIntfPoolItem.All();
var
  i : IInterface;
  b : Boolean;
  a : TIntfPoolItem;
begin
  i := nil;
  b := False;
  a := TIntfPoolItem.Create(i,b);
  try
    Check(( i = a.Intf ),'Create() > Intf');
    CheckEquals(b,a.Used,'Create() > Used');
    b := not b;
    a.Used := b;
    CheckEquals(b,a.Used,'Used');
  finally
    FreeAndNil(a);
  end;
  a := nil;
  
  i := nil;
  b := True;
  a := TIntfPoolItem.Create(i,b);
  try
    Check(( i = a.Intf ),'Create() > Intf');
    CheckEquals(b,a.Used,'Create() > Used');
    b := not b;
    a.Used := b;
    CheckEquals(b,a.Used,'Used');
  finally
    FreeAndNil(a);
  end;
end;

{ TTest_TSimpleItemFactory }

procedure TTest_TSimpleItemFactory.CreateInstance();
var
  b, a : IItemFactory;
  itm : IInterface;
begin
  a := TSimpleItemFactory.Create(TSimpleFactoryItem_A);
    itm := a.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_A));

    itm := a.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_A));

  b := TSimpleItemFactory.Create(TSimpleFactoryItem_B);
    itm := b.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_B));
    
    itm := b.CreateInstance();
    CheckEquals(True,Assigned(itm));
    CheckEquals(True,Supports(itm,ISimple_B));
end;

type

  { TSimpleItemFactoryCrack }

  TSimpleItemFactoryCrack = class(TSimpleItemFactory)
  public
    function GetItemClass() : TSimpleFactoryItemClass;
  end;

{ TSimpleItemFactoryCrack }

function TSimpleItemFactoryCrack.GetItemClass() : TSimpleFactoryItemClass;
begin
  Result := inherited GetItemClass();
end;

procedure TTest_TSimpleItemFactory.CreateProc();
var
  a : IItemFactory;
  b : TSimpleItemFactoryCrack;
  ok : Boolean;
begin
  ok := False;
  try
    TSimpleItemFactory.Create(nil);
  except
    on e : EServiceConfigException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok,'Create(nil)');
  
  b := TSimpleItemFactoryCrack.Create(TSimpleFactoryItem_A);
  CheckEquals(TSimpleFactoryItem_A,b.GetItemClass());
  FreeAndNil(b);
  
  b := TSimpleItemFactoryCrack.Create(TSimpleFactoryItem_B);
  CheckEquals(TSimpleFactoryItem_B,b.GetItemClass());
end;

initialization
{$IFDEF FPC}
  RegisterTest(TTest_TIntfPool);
  RegisterTest(TTest_TSimpleItemFactoryEx);
  RegisterTest(TTest_TImplementationFactory);
  RegisterTest(TTest_TIntfPoolItem);
  RegisterTest(TTest_TImplementationFactory);
{$ELSE}
  RegisterTest(TTest_TIntfPool.Suite);
  RegisterTest(TTest_TSimpleItemFactoryEx.Suite);
  RegisterTest(TTest_TImplementationFactory.Suite);
  RegisterTest(TTest_TIntfPoolItem.Suite);
  RegisterTest(TTest_TImplementationFactory.Suite);
{$ENDIF}

end.
