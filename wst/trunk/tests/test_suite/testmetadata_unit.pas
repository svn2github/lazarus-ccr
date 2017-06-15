{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}
unit testmetadata_unit;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XMLWrite, wst_fpc_xml,
{$ELSE}
  TestFrameWork, xmldom, wst_delphi_xml,
{$ENDIF}
  test_suite_utils, metadata_generator, binary_streamer, metadata_repository,
  pastree, PScanner, pascal_parser_intf, metadata_wsdl;

type

  { TTestMetadata }

  TTestMetadata= class(TWstBaseTest)
  protected
    function CreateSymbolTable():TwstPasTreeContainer;
  published
    procedure test_Metadata();
  end;


implementation

{ TTestMetadata }

function TTestMetadata.CreateSymbolTable(): TwstPasTreeContainer;

  function CreateProc(
    const AName : string;
          AClass : TPasClassType;
          AContainer : TwstPasTreeContainer
  ) : TPasProcedure ;
  begin
    Result := TPasProcedure(AContainer.CreateElement(TPasProcedure,AName,AContainer.CurrentModule.InterfaceSection,visDefault,'',0));
    Result.ProcType := TPasProcedureType(AContainer.CreateElement(TPasProcedureType,'',Result,visDefault,'',0));
    AClass.Members.Add(Result);
  end;

  function CreateFunc(
    const AName, AResultTypeName : string;
          AClass : TPasClassType;
          AContainer : TwstPasTreeContainer
  ) : TPasFunction ;
  begin            
    Result := TPasFunction(AContainer.CreateElement(TPasFunction,AName,AContainer.CurrentModule.InterfaceSection,visDefault,'',0));
  {$IFDEF WST_TPASSOURCEPOS}                                               
    Result.ProcType := AContainer.CreateFunctionType('','result',Result,True,Default(TPasSourcePos));
  {$ELSE WST_TPASSOURCEPOS}                                                   
    Result.ProcType := AContainer.CreateFunctionType('','result',Result,True,'',0);
  {$ENDIF WST_TPASSOURCEPOS}
    AClass.Members.Add(Result);
    TPasFunctionType(Result.ProcType).ResultEl.ResultType := AContainer.FindElement(AResultTypeName) as TPasType;
    TPasFunctionType(Result.ProcType).ResultEl.ResultType.AddRef();
  end;

  function CreateParam(
    const AName, ATypeName : string;
    const AAccess : TArgumentAccess;
          AProc : TPasProcedure;
          AContainer : TwstPasTreeContainer
  ) : TPasArgument ;
  begin
    Result := TPasArgument(AContainer.CreateElement(TPasArgument,AName,AProc.ProcType,visDefault,'',0));
    Result.ArgType := AContainer.FindElement(ATypeName) as TPasType;
    Result.ArgType.AddRef();
    Result.Access := AAccess;
    AProc.ProcType.Args.Add(Result);
  end;
  
var
  inft : TPasClassType;
  sct : TInterfaceSection;
  locProc : TPasProcedure;
begin
  Result := TwstPasTreeContainer.Create();
  CreateWstInterfaceSymbolTable(Result);
  Result.Package.Modules.Add(Result.CreateElement(TPasModule,'test_unit_name',Result.Package,visDefault,'',0));
  sct := TInterfaceSection(Result.CreateElement(TInterfaceSection,'',Result.CurrentModule,visDefault,'',0));
  Result.CurrentModule.InterfaceSection := sct;

  inft := TPasClassType(Result.CreateElement(TPasClassType,'service_1',sct,visDefault,'',0));
  inft.ObjKind := okInterface;
  sct.Declarations.Add(inft);
  sct.Types.Add(inft);
    CreateProc('void_operation_proc',inft,Result);
    CreateFunc('void_operation_func','integer',inft,Result);

  inft := TPasClassType(Result.CreateElement(TPasClassType,'service_2',sct,visDefault,'',0));
  inft.ObjKind := okInterface;
  sct.Declarations.Add(inft);
  sct.Types.Add(inft);
    locProc := CreateProc('dis_proc',inft,Result);
      CreateParam('d','double',argDefault,locProc,Result);
      CreateParam('i','integer',argConst,locProc,Result);
      CreateParam('s','string',argOut,locProc,Result);
    locProc := CreateFunc('sid_func','double',inft,Result);
      CreateParam('s','string',argConst,locProc,Result);
      CreateParam('i','integer',argVar,locProc,Result);
end;

procedure PrintWSDL(ARep : PServiceRepository);
var
  locDoc : TXMLDocument;
  strm : TMemoryStream;
  s : string;
begin
  strm := nil;;
  locDoc := CreateDoc();// TXMLDocument.Create();
  try
    GenerateWSDL(ARep,locDoc);
    strm := TMemoryStream.Create();
    WriteXMLFile(locDoc,strm);
    SetLength(s,strm.Size);
    Move(strm.Memory^,s[1],strm.Size);
    WriteLn('*******************************************************');
    WriteLn(s);
    WriteLn('*******************************************************');
  finally
    ReleaseDomNode(locDoc);
    strm.Free();
  end;
end;

procedure TTestMetadata.test_Metadata();
var
  st : TwstPasTreeContainer;
  mg : TMetadataGenerator;
  wtr : IDataStore;
  strm : TMemoryStream;

  rp : PServiceRepository;
  ps : PService;
  po : PServiceOperation;
  pop : POperationParam;
begin
  strm := nil;
  mg := nil;
  rp := nil;
  st := CreateSymbolTable();
  try
    strm := TMemoryStream.Create();
    wtr := CreateBinaryWriter(strm);
    mg := TMetadataGenerator.Create(st,wtr);
    mg.Execute();
    wtr := nil;
    strm.Position := 0;
    
    Check(strm.Size>10);
    CheckEquals(2,LoadRepositoryData(strm,rp),'symbol count');
    CheckEquals('test_unit_name',rp^.Name,'unit name');
    CheckEquals(2,rp^.ServicesCount,'services count');
    Check( rp^.Services <> nil , 'services pointer');
    
    ps := rp^.Services;
    CheckEquals('service_1',ps^.Name,'service name');
    CheckEquals(2,ps^.OperationsCount,'operations count');
    Check(ps^.Operations <> nil, 'operations pointer');
      po := ps^.Operations;
      CheckEquals('void_operation_proc',po^.Name, 'operation name');
      CheckEquals(0,po^.ParamsCount,'params count');
      Check( po^.Params = nil ,'params pointer');
      Inc(po);
      CheckEquals('void_operation_func',po^.Name, 'operation name');
      CheckEquals(1,po^.ParamsCount, 'params count');
      Check( po^.Params <> nil, 'params pointer');
        pop := po^.Params;
        CheckEquals('result',pop^.Name,'param name');
        CheckEquals('integer',pop^.TypeName,'param type name');
        CheckEquals(ord(argOut),ord(pop^.Modifier),'param modifier');

     rp^.NameSpace := 'http://test_name_space/';
     //PrintWSDL(rp);
  finally
    mg.Free();
    st.Free();
    strm.Free();
    ClearRepositoryData(rp);
  end;
end;

initialization
  RegisterTest('Metadata', TTestMetadata.Suite);
  
end.
