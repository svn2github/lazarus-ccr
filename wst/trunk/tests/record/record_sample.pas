{
This unit has been produced by ws_helper.
  Input unit name : "record_sample".
  This unit name  : "record_sample".
  Date            : "02/01/2015 23:42:08".
}
unit record_sample;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$DEFINE WST_RECORD_RTTI}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'urn:record_sample';
  sUNIT_NAME = 'record_sample';

type


  RecordA = record
    fieldA : integer;
    fieldB : Single;
    comment : UnicodeString;
  end;

  RecordB = record
    singleField : Single;
    intField : integer;
    comment : UnicodeString;
    RecordField : RecordA;
  end;

  RecordC = record
    intField : integer;
    RecordField : RecordB;
  end;

  RecordService = interface(IInvokable)
    ['{4CB555BD-B523-49BD-8861-496497B081DD}']
    function Add(
      const  AValue : RecordA
    ):RecordB;
    function AddRec(
      const  AA : RecordA; 
      const  AB : RecordB; 
      const  AC : RecordC
    ):RecordC;
  end;

  procedure Register_record_sample_ServiceMetadata();

Implementation
uses metadata_repository, record_rtti, wst_types;


procedure Register_record_sample_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'RecordService',
    'TRANSPORT_Address',
    'http://127.0.0.1:20000'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'RecordService',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'Add',
    '_E_N_',
    'Add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'Add',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'Add',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'AddRec',
    '_E_N_',
    'AddRec'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'AddRec',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'RecordService',
    'AddRec',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;



{$IFDEF WST_RECORD_RTTI}
function __RecordA_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^RecordA;
  r : RecordA;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'RecordA',
    SizeOf(RecordA),
    [ PtrUInt(@(p^.fieldA)) - PtrUInt(p), PtrUInt(@(p^.fieldB)) - PtrUInt(p), PtrUInt(@(p^.comment)) - PtrUInt(p) ],
    [ TypeInfo(integer), TypeInfo(Single), TypeInfo(UnicodeString) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}

{$IFDEF WST_RECORD_RTTI}
function __RecordB_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^RecordB;
  r : RecordB;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'RecordB',
    SizeOf(RecordB),
    [ PtrUInt(@(p^.singleField)) - PtrUInt(p), PtrUInt(@(p^.intField)) - PtrUInt(p), PtrUInt(@(p^.comment)) - PtrUInt(p), PtrUInt(@(p^.RecordField)) - PtrUInt(p) ],
    [ TypeInfo(Single), TypeInfo(integer), TypeInfo(UnicodeString), TypeInfo(RecordA) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}

{$IFDEF WST_RECORD_RTTI}
function __RecordC_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^RecordC;
  r : RecordC;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'RecordC',
    SizeOf(RecordC),
    [ PtrUInt(@(p^.intField)) - PtrUInt(p), PtrUInt(@(p^.RecordField)) - PtrUInt(p) ],
    [ TypeInfo(integer), TypeInfo(RecordB) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}
var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();



  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(RecordA),'RecordA').RegisterExternalPropertyName('__FIELDS__','fieldA;fieldB;comment');
{$IFNDEF WST_RECORD_RTTI}
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(RecordA)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(RecordA)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordA)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(RecordA)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__RecordA_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordA)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(RecordB),'RecordB').RegisterExternalPropertyName('__FIELDS__','singleField;intField;comment;RecordField');
{$IFNDEF WST_RECORD_RTTI}
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(RecordB)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(RecordB)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordB)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(RecordB)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__RecordB_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordB)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(RecordC),'RecordC').RegisterExternalPropertyName('__FIELDS__','intField;RecordField');
{$IFNDEF WST_RECORD_RTTI}
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(RecordC)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(RecordC)),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordC)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(RecordC)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__RecordC_TYPEINFO_FUNC__()),GetTypeRegistry().ItemByTypeInfo[TypeInfo(RecordC)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}


End.
