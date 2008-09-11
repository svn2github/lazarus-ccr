{
    This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit object_serializer;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs, SyncObjs,
  base_service_intf, wst_types;

type

  TObjectSerializerOption = ( osoDontDoBeginRead, osoDontDoBeginWrite );
  TObjectSerializerOptions = set of TObjectSerializerOption;
  
  ESerializerException = class(EServiceException)
  end;
  
  TPropSerializationInfo = class;
  
  TPropertyReadProc = procedure(
    AObject : TObject;
    APropInfo : TPropSerializationInfo;
    AStore : IFormatterBase
  );
  TPropertyWriteProc = TPropertyReadProc;
  
  { TPropSerializationInfo }

  TPropSerializationInfo = class
  private
    FExternalName : string;
    FName : string;
    FNameSpace : string;
    FPersisteType : TPropStoreType;
    FPropInfo : PPropInfo;
    FQualifiedName : Boolean;
    FReaderProc : TPropertyReadProc;
    FStyle : TSerializationStyle;
    FWriterProc : TPropertyWriteProc;
  public
    property Name : string read FName;
    property ExternalName : string read FExternalName;
    // NameSpace apply only if ( QualifiedName = True )
    property NameSpace : string read FNameSpace;
    property Style : TSerializationStyle read FStyle;
    property PersisteType : TPropStoreType read FPersisteType;
    property PropInfo : PPropInfo read FPropInfo;
    property QualifiedName : Boolean read FQualifiedName;
    property ReaderProc : TPropertyReadProc read FReaderProc;
    property WriterProc : TPropertyWriteProc read FWriterProc;
  end;
  
  { TObjectSerializer }

  TObjectSerializer = class
  private
    FSerializationInfos : TObjectList;
    FTarget : TBaseComplexRemotableClass;
    FRawPropList : PPropList;
    FOptions : TObjectSerializerOptions;
  private
    procedure Prepare(ATypeRegistry : TTypeRegistry);
  public
    constructor Create(
      ATargetClass : TBaseComplexRemotableClass;
      ATypeRegistry : TTypeRegistry
    );
    destructor Destroy();override;
    procedure Read(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );
    procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );
    property Target : TBaseComplexRemotableClass read FTarget;
    property Options : TObjectSerializerOptions read FOptions write FOptions;
  end;
  
  TGetSerializerFunction = function() : TObjectSerializer of object;
  
  { TBaseComplexTypeRegistryItem }

  TBaseComplexTypeRegistryItem = class(TTypeRegistryItem)
  private
    FGetterLock : TCriticalSection;
    FSerializer : TObjectSerializer;
    FGetFunction : TGetSerializerFunction;
    FFuncIsNotReady : Boolean;
  private
    function FirstGetter() : TObjectSerializer;
    function StaticGetter() : TObjectSerializer;
  public
    constructor Create(
            AOwner        : TTypeRegistry;
            ANameSpace    : string;
            ADataType     : PTypeInfo;
      Const ADeclaredName : string = ''
    );override;
    destructor Destroy();override;
    function GetSerializer() : TObjectSerializer;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;
  
  { TBaseComplexRemotableInitializer }

  TBaseComplexRemotableInitializer = class(TRemotableTypeInitializer)
  public
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean;override;
    class function GetItemClass(const ATypeInfo : PTypeInfo) : TTypeRegistryItemClass;override;
{$IFDEF TRemotableTypeInitializer_Initialize}
    class function Initialize(
      ATypeInfo : PTypeInfo;
      ARegistryItem : TTypeRegistryItem
    ) : Boolean;override;
{$ENDIF TRemotableTypeInitializer_Initialize}
  end;
  
resourcestring
  SERR_NoReaderProc = 'No reader proc for that type, Prop : "(%s : %s)".';
  SERR_NoSerializerFoThisType = 'No serializer for this type : %s.';
  SERR_SerializerInitializationException = 'Unable to initialize the serializer of that type : "%s".';
  
implementation

procedure ErrorProc(
    AObject : TObject;
    APropInfo : TPropSerializationInfo;
    AStore : IFormatterBase
);
begin
  raise Exception.CreateFmt(SERR_NoReaderProc,[APropInfo.Name,APropInfo.FPropInfo^.Name]);
end;

type
  TEnumBuffer = record
    case TOrdType of
      otSByte : (ShortIntData : ShortInt);
      otUByte : (ByteData : Byte);
      otSWord : (SmallIntData : SmallInt);
      otUWord : (WordData : Word);
      otSLong : (SLongIntData : LongInt);
      otULong : (ULongIntData : LongWord);
  end;
  TFloatBuffer = record
    case TFloatType of
      ftSingle : (SingleData : Single);
      ftDouble : (DoubleData : Double);
      ftExtended : (ExtendedData : Extended);
      ftCurr : (CurrencyData : Currency);
      ftComp : (CompData : Comp);
  end;
  TFloatExtendedType = Extended;
  
//   Simple readers
{$IFDEF HAS_TKBOOL}
procedure BoolReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Boolean;
begin
  locData := False;
  locName := APropInfo.ExternalName;
  AStore.Get(APropInfo.PropInfo^.PropType,locName,locData);
  SetOrdProp(AObject,APropInfo.PropInfo,Ord(locData));
end;
{$ENDIF HAS_TKBOOL}

procedure ClassReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  objData : TObject;
  objDataCreateHere : Boolean;
begin
  locName := APropInfo.ExternalName;
  objData := GetObjectProp(AObject,APropInfo.PropInfo);
  objDataCreateHere := not Assigned(objData);
  try
    AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,objData);
    if objDataCreateHere then
      SetObjectProp(AObject,APropInfo.PropInfo,objData);
  finally
    if objDataCreateHere and ( objData <> GetObjectProp(AObject,APropInfo.PropInfo) ) then
      FreeAndNil(objData);
  end;
end;

procedure FloatReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  propName : string;
  floatBuffer : TFloatBuffer;
  floatDt : TFloatExtendedType;
  pt : PTypeInfo;
begin
  floatBuffer.ExtendedData := 0;
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        AStore.Get(pt,propName,floatBuffer.SingleData);
        floatDt := floatBuffer.SingleData;
      end;
    ftDouble :
      begin
        AStore.Get(pt,propName,floatBuffer.DoubleData);
        floatDt := floatBuffer.DoubleData;
      end;
    ftExtended :
      begin
        AStore.Get(pt,propName,floatBuffer.ExtendedData);
        floatDt := floatBuffer.ExtendedData;
      end;
    ftCurr :
      begin
        AStore.Get(pt,propName,floatBuffer.CurrencyData);
        floatDt := floatBuffer.CurrencyData;
      end;
    ftComp :
      begin
        AStore.Get(pt,propName,floatBuffer.CompData);
        floatDt := floatBuffer.CompData;
      end;
  end;
  SetFloatProp(AObject,APropInfo.PropInfo,floatDt);
end;

procedure IntEnumReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  propName : string;
  int64Data : Int64;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    AStore.Get(pt,propName,boolData);
    SetPropValue(AObject,propName,boolData);
  end else begin
{$ENDIF}
    enumData.ULongIntData := 0;
    Case GetTypeData(pt)^.OrdType Of
      otSByte :
        Begin
          AStore.Get(pt,propName,enumData.ShortIntData);
          int64Data := enumData.ShortIntData;
        End;
      otUByte :
        Begin
          AStore.Get(pt,propName,enumData.ByteData);
          int64Data := enumData.ByteData;
        End;
      otSWord :
        Begin
          AStore.Get(pt,propName,enumData.SmallIntData);
          int64Data := enumData.SmallIntData;
        End;
      otUWord :
        Begin
          AStore.Get(pt,propName,enumData.WordData);
          int64Data := enumData.WordData;
        End;
      otSLong:
        Begin
          AStore.Get(pt,propName,enumData.SLongIntData);
          int64Data := enumData.SLongIntData;
        End;
      otULong :
        Begin
          AStore.Get(pt,propName,enumData.ULongIntData);
          int64Data := enumData.ULongIntData;
        End;
    End;
    SetOrdProp(AObject,APropInfo.PropInfo,int64Data);
{$IFDEF WST_DELPHI}
  end;
{$ENDIF}
end;

procedure Int64Reader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Int64;
begin
  locData := 0;
  locName := APropInfo.ExternalName;
  AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,locData);
  SetInt64Prop(AObject,APropInfo.PropInfo,locData);
end;

procedure StringReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : string;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,locData);
  SetStrProp(AObject,APropInfo.PropInfo,locData);
end;

// Qualified readers
{$IFDEF HAS_TKBOOL}
procedure BoolReaderQualifier(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Boolean;
begin
  locData := False;
  locName := APropInfo.ExternalName;
  AStore.Get(APropInfo.PropInfo^.PropType,APropInfo.NameSpace,locName,locData);
  SetOrdProp(AObject,APropInfo.PropInfo,Ord(locData));
end;
{$ENDIF HAS_TKBOOL}

procedure ClassReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  objData : TObject;
  objDataCreateHere : Boolean;
begin
  locName := APropInfo.ExternalName;
  objData := GetObjectProp(AObject,APropInfo.PropInfo);
  objDataCreateHere := not Assigned(objData);
  try
    AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,objData);
    if objDataCreateHere then
      SetObjectProp(AObject,APropInfo.PropInfo,objData);
  finally
    if objDataCreateHere and ( objData <> GetObjectProp(AObject,APropInfo.PropInfo) ) then
      FreeAndNil(objData);
  end;
end;

procedure FloatReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  propName : string;
  floatBuffer : TFloatBuffer;
  floatDt : TFloatExtendedType;
  pt : PTypeInfo;
begin
  floatBuffer.ExtendedData := 0;
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.SingleData);
        floatDt := floatBuffer.SingleData;
      end;
    ftDouble :
      begin
        AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.DoubleData);
        floatDt := floatBuffer.DoubleData;
      end;
    ftExtended :
      begin
        AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.ExtendedData);
        floatDt := floatBuffer.ExtendedData;
      end;
    ftCurr :
      begin
        AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.CurrencyData);
        floatDt := floatBuffer.CurrencyData;
      end;
    ftComp :
      begin
        AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.CompData);
        floatDt := floatBuffer.CompData;
      end;
  end;
  SetFloatProp(AObject,APropInfo.PropInfo,floatDt);
end;

procedure Int64ReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Int64;
begin
  locData := 0;
  locName := APropInfo.ExternalName;
  AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,locData);
  SetInt64Prop(AObject,APropInfo.PropInfo,locData);
end;

procedure IntEnumReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  propName : string;
  int64Data : Int64;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    AStore.Get(pt,APropInfo.NameSpace,propName,boolData);
    SetPropValue(AObject,propName,boolData);
  end else begin
{$ENDIF}
    enumData.ULongIntData := 0;
    Case GetTypeData(pt)^.OrdType Of
      otSByte :
        Begin
          AStore.Get(pt,APropInfo.NameSpace,propName,enumData.ShortIntData);
          int64Data := enumData.ShortIntData;
        End;
      otUByte :
        Begin
          AStore.Get(pt,APropInfo.NameSpace,propName,enumData.ByteData);
          int64Data := enumData.ByteData;
        End;
      otSWord :
        Begin
          AStore.Get(pt,APropInfo.NameSpace,propName,enumData.SmallIntData);
          int64Data := enumData.SmallIntData;
        End;
      otUWord :
        Begin
          AStore.Get(pt,APropInfo.NameSpace,propName,enumData.WordData);
          int64Data := enumData.WordData;
        End;
      otSLong:
        Begin
          AStore.Get(pt,APropInfo.NameSpace,propName,enumData.SLongIntData);
          int64Data := enumData.SLongIntData;
        End;
      otULong :
        Begin
          AStore.Get(pt,APropInfo.NameSpace,propName,enumData.ULongIntData);
          int64Data := enumData.ULongIntData;
        End;
    End;
    SetOrdProp(AObject,APropInfo.PropInfo,int64Data);
{$IFDEF WST_DELPHI}
  end;
{$ENDIF}
end;

procedure StringReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : string;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,locData);
  SetStrProp(AObject,APropInfo.PropInfo,locData);
end;

//   Simple Writers
{$IFDEF HAS_TKBOOL}
procedure BoolWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Boolean;
begin
  locName := APropInfo.ExternalName;
  locData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
  AStore.Put(locName,APropInfo.PropInfo^.PropType,locData);
end;
{$ENDIF HAS_TKBOOL}

procedure ClassWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Tobject;
begin
  locName := APropInfo.ExternalName;
  locData := GetObjectProp(AObject,APropInfo.PropInfo);
  AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

procedure FloatWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  floatDt : TFloatBuffer;
  pt : PTypeInfo;
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        floatDt.SingleData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(prpName,pt,floatDt.SingleData);
      end;
    ftDouble :
      begin
        floatDt.DoubleData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(prpName,pt,floatDt.DoubleData);
      end;
    ftExtended :
      begin
        floatDt.ExtendedData := Extended(GetFloatProp(AObject,APropInfo.PropInfo));
        AStore.Put(prpName,pt,floatDt.ExtendedData);
      end;
    ftCurr :
      begin
        floatDt.CurrencyData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(prpName,pt,floatDt.CurrencyData);
      end;
{$IFDEF HAS_COMP}
    ftComp :
      begin
        floatDt.CompData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(prpName,pt,floatDt.CompData);
      end;
{$ENDIF}
  end;
end;

procedure IntEnumWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    boolData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
    AStore.Put(prpName,pt,boolData);
  end else begin
{$ENDIF WST_DELPHI}
    FillChar(enumData,SizeOf(enumData),#0);
    case GetTypeData(pt)^.OrdType of
      otSByte :
        begin
          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(prpName,pt,enumData.ShortIntData);
        end;
      otUByte :
        begin
          enumData.ByteData := Byte(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(prpName,pt,enumData.ByteData);
        end;
      otSWord :
        begin
          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(prpName,pt,enumData.SmallIntData);
        end;
      otUWord :
        begin
          enumData.WordData := Word(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(prpName,pt,enumData.WordData);
        end;
      otSLong :
        begin
          enumData.SLongIntData := LongInt(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(prpName,pt,enumData.SLongIntData);
        end;
      otULong :
        begin
          enumData.ULongIntData := LongWord(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(prpName,pt,enumData.ULongIntData);
        end;
    end;
{$IFDEF WST_DELPHI}
  end;
{$ENDIF WST_DELPHI}
end;

procedure Int64Writer(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Int64;
begin
  locName := APropInfo.ExternalName;
  locData := GetInt64Prop(AObject,APropInfo.PropInfo);
  AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

procedure StringWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : string;
begin
  locName := APropInfo.ExternalName;
  locData := GetStrProp(AObject,APropInfo.PropInfo);
  AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;


// Qualified writers
{$IFDEF HAS_TKBOOL}
procedure BoolWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Boolean;
begin
  locName := APropInfo.ExternalName;
  locData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
  AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType,locData);
end;
{$ENDIF HAS_TKBOOL}

procedure ClassWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Tobject;
begin
  locName := APropInfo.ExternalName;
  locData := GetObjectProp(AObject,APropInfo.PropInfo);
  AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

procedure FloatWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  floatDt : TFloatBuffer;
  pt : PTypeInfo;
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        floatDt.SingleData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.SingleData);
      end;
    ftDouble :
      begin
        floatDt.DoubleData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.DoubleData);
      end;
    ftExtended :
      begin
        floatDt.ExtendedData := Extended(GetFloatProp(AObject,APropInfo.PropInfo));
        AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.ExtendedData);
      end;
    ftCurr :
      begin
        floatDt.CurrencyData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.CurrencyData);
      end;
{$IFDEF HAS_COMP}
    ftComp :
      begin
        floatDt.CompData := GetFloatProp(AObject,APropInfo.PropInfo);
        AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.CompData);
      end;
{$ENDIF}
  end;
end;

procedure IntEnumWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    boolData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
    AStore.Put(APropInfo.NameSpace,prpName,pt,boolData);
  end else begin
{$ENDIF WST_DELPHI}
    FillChar(enumData,SizeOf(enumData),#0);
    case GetTypeData(pt)^.OrdType of
      otSByte :
        begin
          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.ShortIntData);
        end;
      otUByte :
        begin
          enumData.ByteData := Byte(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.ByteData);
        end;
      otSWord :
        begin
          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.SmallIntData);
        end;
      otUWord :
        begin
          enumData.WordData := Word(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.WordData);
        end;
      otSLong :
        begin
          enumData.SLongIntData := LongInt(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.SLongIntData);
        end;
      otULong :
        begin
          enumData.ULongIntData := LongWord(GetOrdProp(AObject,APropInfo.PropInfo));
          AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.ULongIntData);
        end;
    end;
{$IFDEF WST_DELPHI}
  end;
{$ENDIF WST_DELPHI}
end;

procedure Int64WriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Int64;
begin
  locName := APropInfo.ExternalName;
  locData := GetInt64Prop(AObject,APropInfo.PropInfo);
  AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

procedure StringWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : string;
begin
  locName := APropInfo.ExternalName;
  locData := GetStrProp(AObject,APropInfo.PropInfo);
  AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;



type
  TReaderWriterInfo = record
    Simple : TPropertyReadProc;
    Qualified : TPropertyReadProc;
  end;

var
{$IFDEF FPC}
  ReaderWriterInfoMap : array[0..1] of array[TTypeKind] of TReaderWriterInfo = (
    ( // Readers
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkUnknown
      ( Simple : {$IFDEF FPC}@{$ENDIF}IntEnumReader; Qualified : {$IFDEF FPC}@{$ENDIF}IntEnumReaderQualified ;) , //tkInteger
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkChar
      ( Simple : {$IFDEF FPC}@{$ENDIF}IntEnumReader; Qualified : {$IFDEF FPC}@{$ENDIF}IntEnumReaderQualified ;) , //tkEnumeration
      ( Simple : {$IFDEF FPC}@{$ENDIF}FloatReader; Qualified : {$IFDEF FPC}@{$ENDIF}FloatReaderQualified ;) , //tkFloat
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkSet
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkMethod
      ( Simple : {$IFDEF FPC}@{$ENDIF}StringReader; Qualified : {$IFDEF FPC}@{$ENDIF}StringReaderQualified ;) , //tkSString
      ( Simple : {$IFDEF FPC}@{$ENDIF}StringReader; Qualified : {$IFDEF FPC}@{$ENDIF}StringReaderQualified ;) , //tkLString
      ( Simple : {$IFDEF FPC}@{$ENDIF}StringReader; Qualified : {$IFDEF FPC}@{$ENDIF}StringReaderQualified ;) , //tkAString
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkWString
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkVariant
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkArray
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkRecord
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkInterface
      ( Simple : {$IFDEF FPC}@{$ENDIF}ClassReader; Qualified : {$IFDEF FPC}@{$ENDIF}ClassReaderQualified ;) , //tkClass
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkObject
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkWChar
      ( Simple : {$IFDEF FPC}@{$ENDIF}BoolReader; Qualified : {$IFDEF FPC}@{$ENDIF}BoolReaderQualifier ;) , //tkBool
      ( Simple : {$IFDEF FPC}@{$ENDIF}Int64Reader; Qualified : {$IFDEF FPC}@{$ENDIF}Int64ReaderQualified ;) , //tkInt64
      ( Simple : {$IFDEF FPC}@{$ENDIF}Int64Reader; Qualified : {$IFDEF FPC}@{$ENDIF}Int64ReaderQualified ;) , //tkQWord
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkDynArray
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;)   //tkInterfaceRaw
    ),
    ( // Writers
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkUnknown
      ( Simple : {$IFDEF FPC}@{$ENDIF}IntEnumWriter; Qualified : {$IFDEF FPC}@{$ENDIF}IntEnumWriterQualified ;) , //tkInteger
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkChar
      ( Simple : {$IFDEF FPC}@{$ENDIF}IntEnumWriter; Qualified : {$IFDEF FPC}@{$ENDIF}IntEnumWriterQualified ;) , //tkEnumeration
      ( Simple : {$IFDEF FPC}@{$ENDIF}FloatWriter; Qualified : {$IFDEF FPC}@{$ENDIF}FloatWriterQualified ;) , //tkFloat
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkSet
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkMethod
      ( Simple : {$IFDEF FPC}@{$ENDIF}StringWriter; Qualified : {$IFDEF FPC}@{$ENDIF}StringWriterQualified ;) , //tkSString
      ( Simple : {$IFDEF FPC}@{$ENDIF}StringWriter; Qualified : {$IFDEF FPC}@{$ENDIF}StringWriterQualified ;) , //tkLString
      ( Simple : {$IFDEF FPC}@{$ENDIF}StringWriter; Qualified : {$IFDEF FPC}@{$ENDIF}StringWriterQualified ;) , //tkAString
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkWString
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkVariant
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkArray
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkRecord
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkInterface
      ( Simple : {$IFDEF FPC}@{$ENDIF}ClassWriter; Qualified : {$IFDEF FPC}@{$ENDIF}ClassWriterQualified ;) , //tkClass
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkObject
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkWChar
      ( Simple : {$IFDEF FPC}@{$ENDIF}BoolWriter; Qualified : {$IFDEF FPC}@{$ENDIF}BoolWriterQualified ;) , //tkBool
      ( Simple : {$IFDEF FPC}@{$ENDIF}Int64Writer; Qualified : {$IFDEF FPC}@{$ENDIF}Int64WriterQualified ;) , //tkInt64
      ( Simple : {$IFDEF FPC}@{$ENDIF}Int64Writer; Qualified : {$IFDEF FPC}@{$ENDIF}Int64WriterQualified ;) , //tkQWord
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;) , //tkDynArray
      ( Simple : {$IFDEF FPC}@{$ENDIF}ErrorProc; Qualified : {$IFDEF FPC}@{$ENDIF}ErrorProc ;)   //tkInterfaceRaw
    )
  );
{$ENDIF FPC}

{$IFDEF WST_DELPHI}
  ReaderWriterInfoMap : array[0..1] of array[TTypeKind] of TReaderWriterInfo = (
    ( // Readers
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkUnknown
      ( Simple : IntEnumReader; Qualified : IntEnumReaderQualified ;) , //tkInteger
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkChar
      ( Simple : IntEnumReader; Qualified : IntEnumReaderQualified ;) , //tkEnumeration
      ( Simple : FloatReader; Qualified : FloatReaderQualified ;) , //tkFloat
      ( Simple : StringReader; Qualified : StringReaderQualified ;) , //tkString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkSet
      ( Simple : ClassReader; Qualified : ClassReaderQualified ;) , //tkClass
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkMethod
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkWChar
      ( Simple : StringReader; Qualified : StringReaderQualified ;) , //tkLString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkWString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkVariant
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkArray
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkRecord
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkInterface
      ( Simple : Int64Reader; Qualified : Int64ReaderQualified ;) , //tkInt64
      ( Simple : ErrorProc; Qualified : ErrorProc ;) //tkDynArray
    ),
    ( // Writers
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkUnknown
      ( Simple : IntEnumWriter; Qualified : IntEnumWriterQualified ;) , //tkInteger
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkChar
      ( Simple : IntEnumWriter; Qualified : IntEnumWriterQualified ;) , //tkEnumeration
      ( Simple : FloatWriter; Qualified : FloatWriterQualified ;) , //tkFloat
      ( Simple : StringWriter; Qualified : StringWriterQualified ;) , //tkSString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkSet
      ( Simple : ClassWriter; Qualified : ClassWriterQualified ;) , //tkClass
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkMethod
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkWChar
      ( Simple : StringWriter; Qualified : StringWriterQualified ;) , //tkLString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkWString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkVariant
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkArray
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkRecord
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkInterface
      ( Simple : Int64Writer; Qualified : Int64WriterQualified ;) , //tkInt64
      ( Simple : ErrorProc; Qualified : ErrorProc ;) //tkDynArray
    )
  );
{$ENDIF WST_DELPHI}

{ TObjectSerializer }

procedure TObjectSerializer.Prepare(ATypeRegistry : TTypeRegistry);
var
  locObjTypeData : PTypeData;
  locTypeInfo : PTypeInfo;
  c, i : PtrInt;
  ppi : PPropInfo;
  cl : TClass;
  serArray : array of TPropSerializationInfo;
  serInfo : TPropSerializationInfo;
  regItem, thisRegItem : TTypeRegistryItem;
  st : TPropStoreType;
  clPL : PPropList;
begin
  FSerializationInfos.Clear();
  locTypeInfo := PTypeInfo(Target.ClassInfo);
  locObjTypeData := GetTypeData(locTypeInfo);
  c := locObjTypeData^.PropCount;
  if ( c > 0 ) then begin
    clPL := nil;
    SetLength(serArray,c);
    try
      FillChar(Pointer(serArray)^,SizeOf(TPropSerializationInfo)*c,#0);
      cl := Target;
      thisRegItem := ATypeRegistry.ItemByTypeInfo[locTypeInfo];
      regItem := thisRegItem;
      GetPropList(locTypeInfo,FRawPropList);
      try
        for i := 0 to Pred(c) do begin
          ppi := FRawPropList^[i];
          st := IsStoredPropClass(cl,ppi);
          if ( st in [pstAlways,pstOptional] ) then begin
            serInfo := TPropSerializationInfo.Create();
            serArray[ppi^.NameIndex] := serInfo;
            serInfo.FExternalName := regItem.GetExternalPropertyName(ppi^.Name);
            serInfo.FName := ppi^.Name;
            serInfo.FPersisteType := st;
            serInfo.FPropInfo := ppi;
            serInfo.FReaderProc := ReaderWriterInfoMap[0][ppi^.PropType^.Kind].Simple;
            serInfo.FWriterProc := ReaderWriterInfoMap[1][ppi^.PropType^.Kind].Simple;
            if Target.IsAttributeProperty(ppi^.Name) then
              serInfo.FStyle := ssAttibuteSerialization
            else
              serInfo.FStyle := ssNodeSerialization;
          end;
        end;
        //Check for inherited properties declared in other namespace
        GetMem(clPL,c*SizeOf(Pointer));
        cl := cl.ClassParent;
        while ( cl <> nil ) and ( cl <> TBaseComplexRemotable ) do begin
          c := GetTypeData(PTypeInfo(cl.ClassInfo))^.PropCount;
          if ( c > 0 ) then begin
            GetPropInfos(PTypeInfo(cl.ClassInfo),clPL);
            regItem := ATypeRegistry.Find(PTypeInfo(cl.ClassInfo),True);
            if ( regItem <> nil ) then begin
              for i := 0 to Pred(c) do begin
                ppi := clPL^[i];
                serInfo := serArray[ppi^.NameIndex];
                if ( serInfo <> nil ) then begin
                  if ( thisRegItem.NameSpace <> regItem.NameSpace ) then begin
                    serInfo.FNameSpace := regItem.NameSpace;
                    serInfo.FQualifiedName := True;
                    serInfo.FReaderProc := ReaderWriterInfoMap[0][ppi^.PropType^.Kind].Qualified;
                    serInfo.FWriterProc := ReaderWriterInfoMap[1][ppi^.PropType^.Kind].Qualified;
                  end;
                end;
              end;
            end;
          end;
          cl := cl.ClassParent;
        end;
        // Fill the list now
        for i := 0 to Pred(Length(serArray)) do begin
          if ( serArray[i] <> nil ) then begin
            FSerializationInfos.Add(serArray[i]);
            serArray[i] := nil;
          end;
        end;
      except
        for i := 0 to Pred(locObjTypeData^.PropCount) do
          serArray[i].Free();
        raise;
      end;
    finally
      if ( clPL <> nil ) then
        FreeMem(clPL,locObjTypeData^.PropCount*SizeOf(Pointer));
      SetLength(serArray,0);
    end;
  end;
end;

constructor TObjectSerializer.Create(
  ATargetClass : TBaseComplexRemotableClass;
  ATypeRegistry : TTypeRegistry
);
begin
  Assert(ATargetClass <> nil);
  Assert(ATypeRegistry <> nil);
  FTarget := ATargetClass;
  FSerializationInfos := TObjectList.Create(True);
  Prepare(ATypeRegistry);
end;

destructor TObjectSerializer.Destroy();
begin
  if ( FRawPropList <> nil ) then
    FreeMem(FRawPropList,GetTypeData(PTypeInfo(Target.ClassInfo))^.PropCount*SizeOf(Pointer));
  FSerializationInfos.Free();
  inherited Destroy();
end;

procedure TObjectSerializer.Read(
  var AObject : TObject;
      AStore : IFormatterBase;
  var AName : string;
  const ATypeInfo : PTypeInfo
);
var
  oldSS : TSerializationStyle;
  i, c : PtrInt;
  locSerInfo : TPropSerializationInfo;
begin
  oldSS := AStore.GetSerializationStyle();
  if ( osoDontDoBeginRead in Options ) or ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit; // ???? FreeAndNil(AObject);
      if not Assigned(AObject) then
        AObject := Target.Create();
      c := FSerializationInfos.Count;
      if ( c > 0 ) then begin
        for i := 0 to Pred(c) do begin
          locSerInfo := TPropSerializationInfo(FSerializationInfos[i]);
          if ( locSerInfo.Style <> AStore.GetSerializationStyle() ) then
            AStore.SetSerializationStyle(locSerInfo.Style);
          try
            locSerInfo.ReaderProc(AObject,locSerInfo,AStore);
          except
            on e : EBaseRemoteException do begin
              if ( locSerInfo.PersisteType = pstAlways ) then
                raise;
            end;
          end;
        end;
      end;
    finally
      if not ( osoDontDoBeginRead in Options ) then
        AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;

procedure TObjectSerializer.Save(
        AObject : TBaseRemotable;
        AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  oldSS : TSerializationStyle;
  i, c : PtrInt;
  locSerInfo : TPropSerializationInfo;
begin
  oldSS := AStore.GetSerializationStyle();
  if not ( osoDontDoBeginWrite in Options ) then
    AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(AObject) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    c := FSerializationInfos.Count;
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        locSerInfo := TPropSerializationInfo(FSerializationInfos[i]);
        if ( locSerInfo.Style <> AStore.GetSerializationStyle() ) then
          AStore.SetSerializationStyle(locSerInfo.Style);
        locSerInfo.WriterProc(AObject,locSerInfo,AStore);
      end;
    end;
  finally
    if not ( osoDontDoBeginWrite in Options ) then
      AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;

{ TBaseComplexRemotableInitializer }

class function TBaseComplexRemotableInitializer.CanHandle(ATypeInfo : PTypeInfo) : Boolean;
begin
  Result := ( ATypeInfo <> nil ) and
            ( ATypeInfo^.Kind = tkClass ) and
            GetTypeData(ATypeInfo)^.ClassType.InheritsFrom(TBaseComplexRemotable);
end;

class function TBaseComplexRemotableInitializer.GetItemClass(
  const ATypeInfo : PTypeInfo
) : TTypeRegistryItemClass;
begin
  Result := TBaseComplexTypeRegistryItem;
end;

{$IFDEF TRemotableTypeInitializer_Initialize}
class function TBaseComplexRemotableInitializer.Initialize(
  ATypeInfo : PTypeInfo;
  ARegistryItem : TTypeRegistryItem
) : Boolean;
begin
end;
{$ENDIF TRemotableTypeInitializer_Initialize}

{ TBaseComplexTypeRegistryItem }

function TBaseComplexTypeRegistryItem.FirstGetter() : TObjectSerializer;
begin
  FGetterLock.Acquire();
  try
    if ( FSerializer = nil ) then begin
      FSerializer := TObjectSerializer.Create(TBaseComplexRemotableClass(GetTypeData(DataType)^.ClassType),Owner);
      FFuncIsNotReady := True;
        FGetFunction := {$IFDEF FPC}@{$ENDIF}StaticGetter;
      FFuncIsNotReady := False;
    end;
  finally
    FGetterLock.Release();
  end;
  Result := FSerializer;
end;

function TBaseComplexTypeRegistryItem.StaticGetter() : TObjectSerializer;
begin
  Result := FSerializer;
end;

constructor TBaseComplexTypeRegistryItem.Create(
        AOwner : TTypeRegistry;
        ANameSpace : string;
        ADataType : PTypeInfo;
  const ADeclaredName : string
);
begin
  inherited Create(AOwner, ANameSpace, ADataType, ADeclaredName);
  FGetFunction := {$IFDEF FPC}@{$ENDIF}FirstGetter;
  FGetterLock := TCriticalSection.Create();
end;

destructor TBaseComplexTypeRegistryItem.Destroy();
begin
  FGetterLock.Free();
  FSerializer.Free();
  inherited Destroy();
end;

function TBaseComplexTypeRegistryItem.GetSerializer() : TObjectSerializer;
begin
  while FFuncIsNotReady do begin
    //busy wait
  end;
  Result := FGetFunction();
end;

end.
