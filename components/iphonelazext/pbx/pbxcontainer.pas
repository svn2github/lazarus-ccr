unit pbxcontainer;
(*-------------------------------------------------------------------------------
* by Dmitry Boyarintsev - Oct 2014                                              *
*                                                                               *
* license: free for use, but please leave a note to the origin of the library   *
*                                                                               *
* PBXcontainer unit is a library to read/write the pbx formatter file as a      *
* whole The file structure is made to keep the reference in a complex objects   *
* struture.  Hierarchial trees, lists, cycles and so on.                        *
*                                                                               *
* It's achieved by giving a full list of objects. With each object having an    *
* "id" assigned. Later in the description, the reference is specifeid by the    *
* object's id.                                                                  *
*                                                                               *
* Currently PBXContainer would read and try to build a structure based of       *
*  Object Pascal RTTI. (Not sure if Delphi compatible)                          *
* Following rules are used                                                      *
*   read/write objects are going to join the list of objects (for the futher    *
*             reference)                                                        *
*   read-only objects should be allocated in the constructor of the parent      *
*             they're "inlined" in the file                                     *
*   for array of objects TPBXObjectsList must be used                           *
*   for array of strings TPBXStringArray must be used                           *
*   for key-value set use TPBXKeyValue class                                    *
*   string and integer properties are supported... anything else?               *
*   booleans are (always) written as 0 or 1 to the file                         *
*                                                                               *
*                                                                               *
* todo: add more documentions                                                   *
*                                                                               *
* todo: memoty allocation and release. ObjC is using ref-counted structure.     *
*       do the same? similar?                                                   *
*                                                                               *
* Class naming convention.                                                      *
* Any "utility" class should follow Object Pascal naming convention and start   *
* with "T" (i.e. TPBXValue)                                                     *
*  Any class that's expected to map an actual class (objective-c?), stored      *
* in .pbx file, should go without "T" prefix. The name of the class             *
* should match the one loaded/saved to the PBX file.                            *
* This is necessary for serializing purposes                                    *
*  Any mapped class must inherit from the base PBXObject.                       *
*                                                                               *
* TPBXUnkObject inherites from PBXObject, but since it has an utility role      *
* it falls under Object Pascal naming convention. It also stores ObjC           *
* class name as a property (which is serialized as a classname)                 *
*                                                                               *
-------------------------------------------------------------------------------*)

interface

{$ifdef fpc}{$mode delphi}{$endif}

uses
  Classes, SysUtils, typinfo, pbxfile, contnrs;

type
  TPBXValue = class;
  { TPBXUnkProperty }

  TPBXUnkProperty = class(TObject)
  private
    fname: string;
  public
    value : TPBXValue;
    constructor Create;
    destructor Destroy; override;
    property name  : string read fname;
  end;

  { PBXObject }

  PBXObject = class(TObject)
  private
    _id   : string;
    _fheaderComment : string;
    _funkProp : TFPHashObjectList;
    function GetHasUnkProp: Boolean;
  protected
    // collects the name of string properties that should be written out
    // even if their values is an empty string.
    // if property value is an empty string, it would not be written to the file
    class procedure _WriteEmpty(propnames: TStrings); virtual;
    function GetUnkProp: TFPHashObjectList;
  public
    property __id: string read _id;
    property _headerComment: string read _fheaderComment write _fheaderComment;
    constructor Create; virtual;
    destructor Destroy; override;
    function GetIsaName: string; virtual;
    //todo: unknown properties are read as "strings"
    //      however, some strings are actually object references
    //      these references must be resolved (as actual objects!)
    function _addUnkProp(const aname: string): TPBXUnkProperty;
    property _unkProp: TFPHashObjectList read GetUnkProp;
    property _hasUnkProp: Boolean read GetHasUnkProp;
  end;
  PBXObjectClass = class of PBXObject;

  TPBXObjectsList = class(TObjectList);
  TPBXStringArray = class(TStringList);

  TPBXValueType = (vtString, vtArrayOfStr, vtKeyVal);

  TPBXKeyValue = class;

  { TPBXValue }

  TPBXValue = class(TObject)
  public
    valType : TPBXValueType;
    str     : string;
    arr     : TPBXStringArray;
    keyval  : TPBXKeyValue;
    destructor Destroy; override;
  end;

  { TPBXKeyValue }

  TPBXKeyValue = class(TFPHashObjectList)
  protected
    function AddVal(const name: string; atype: TPBXValueType): TPBXValue;
    function GetValStr(const name: string): string;
    procedure AddValStr(const name, avalue: string);
  public
    function AddStr(const name: string; const avalue: string = ''): TPBXValue;
    function AddStrArray(const name: string): TPBXValue;
    function AddKeyVal(const name: string): TPBXValue;
    // it must be "public, not published"
    property Str[const name: string]: string read GetValStr write AddValStr;
  end;

  TPBXFileInfo = record
    archiveVersion : string;
    objectVersion  : string;
    rootObject     : PBXObject;
  end;

  { TPBXReref }

  TPBXReref = class(TObject)
    instance : TObject;
    propname : string;
    _id      : string;
    constructor Create(ainstance: TObject; const apropname, aref: string);
  end;

  { TPBXContainer }

  TObjHashList = TFPHashObjectList;

  { TPBXUnkClass }

  TPBXUnkClass = class(PBXObject)
  private
    fisa : string;
  public
    constructor CreateWithName(const AISA: string);
    constructor Create; override;
    function GetIsaName: string; override;
    property _isa: string read fisa write fisa;
  end;

  TPBXContainer = class(TObject)
  protected
    procedure ReadObjects(p: TPBXParser; objs: TObjHashList);
    function AllocObject(const nm: string): PBXObject;
  public
    function ReadFile(s: TStream; var AFileInfo: TPBXFileInfo): Boolean;
  end;

procedure TestContainer(const buf: string);

procedure PBXRegisterClass(aclass: PBXObjectClass);
function PBXFindClass(const aclassname: string): PBXObjectClass;

function PBXReadObjectsListRef(p: TPBXParser; obj: PBXObject; propName: string; refs: TList): Boolean;
function PBXReadStringArray(p: TPBXParser; arr: TPBXStringArray): Boolean;
function PBXReadKeyValue(p: TPBXParser; kv: TPBXKeyValue): Boolean;
function PBXReadClass(p: TPBXParser; obj: PBXObject; refs: TList): Boolean;
procedure PBXReref(objs: TObjHashList; refs: TList);

function PBXWriteContainer(const FileInfo: TPBXFileInfo; AssignRef: Boolean = true): string;
procedure PBXWriteObjArray( w: TPBXWriter; list: TPBXObjectsList );
procedure PBXWriteStrArray( w: TPBXWriter; list: TPBXStringArray );
procedure PBXWriteKeyValue( w: TPBXWriter; kv: TPBXKeyValue );
procedure PBXWriteObj(pbx: PBXObject; w: TPBXWriter; WriteEmpty: TStrings);

{ assigns reference IDs to each object in the list. "ID" is 24 charactewr long hex-string }
procedure PBXAssignRef(list: TList);

{ Returns the list of objects that should populate the "objects" section of pbx file }
procedure PBXGatherObjects(obj: TObject; srz: TList);

procedure PBXKeyValsCopy(src, dst: TPBXKeyValue);
procedure PBXValueCopy(src, dst: TPBXValue);
procedure PBXStringArrayCopy(src, dst: TPBXStringArray);

implementation

var
  pbxClassList : TStringList;

function PBXReadKeyValue(p: TPBXParser; kv: TPBXKeyValue): Boolean;
var
  et : TPBXEntity;
  v  : TPBXValue;
begin
  et:=p.FetchNextEntity;
  while et<>etCloseObject do begin
    case et of
      etValue: kv.AddStr(p.Name, p.Value);
      etOpenArray: begin
        v:=kv.AddStrArray(p.Name);
        PBXReadStringArray(p, v.arr);
      end;
      etOpenObject: begin
        v:=kv.AddKeyVal(p.Name);
        PBXReadKeyValue(p, v.keyval);
      end;
    else
      Result:=false;
      Exit;
    end;
    et:=p.FetchNextEntity;
  end;
  Result:=True;
end;

procedure PBXStringArrayCopy(src, dst: TPBXStringArray);
begin
  if not Assigned(src) or not Assigned(dst) then Exit;
  src.Assign(dst);
end;

procedure PBXValueCopy(src, dst: TPBXValue);
begin
  if not Assigned(src) or not Assigned(dst) then Exit;
  dst.valType:=src.valType;
  case dst.valType of
    vtString: dst.str:=src.str;
    vtArrayOfStr: begin
      if not Assigned(dst.arr) then dst.arr:=TPBXStringArray.Create;
      PBXStringArrayCopy(src.arr, dst.arr);
    end;
    vtKeyVal: begin
      if not Assigned(dst.keyval) then dst.keyval:=TPBXKeyValue.Create(true);
      PBXKeyValsCopy(src.keyval, dst.keyval);
    end;
  end;
end;

procedure PBXKeyValsCopy(src, dst: TPBXKeyValue);
var
  svl : TPBXValue;
  nm  : string;
  i   : Integer;
  dvl : TPBXValue;
begin
  if not Assigned(src) or not Assigned(dst) then Exit;
  for i:=0 to src.Count-1 do begin
    nm:=src.NameOfIndex(i);
    svl:=TPBXValue(src.Items[i]);
    dvl:=dst.AddVal(nm, svl.valType);
    PBXValueCopy(svl, dvl);
  end;
end;


procedure TestContainer(const buf: string);
var
  c : TPBXContainer;
  st : TStringStream;
  info : TPBXFileInfo;
begin
  c:= TPBXContainer.Create;
  st := TStringStream.Create(buf);
  try
    c.ReadFile(st, info);
    writeln('arch ver: ',info.archiveVersion);
    writeln(' obj ver: ',info.objectVersion);
    writeln('root obj: ', PtrUInt( info.rootObject ));
  finally
    st.Free;
    c.Free;
  end;
end;

procedure PBXRegisterClass(aclass: PBXObjectClass);
begin
  pbxClassList.AddObject(aclass.ClassName, TObject(aclass));
end;

function PBXFindClass(const aclassname: string): PBXObjectClass;
var
  i : integer;
begin
  i:=pbxClassList.IndexOf(aclassname);
  if i<0 then Result:=nil
  else Result:=PBXObjectClass(pbxClassList.Objects[i]);

end;

{ TPBXUnkProperty }

constructor TPBXUnkProperty.Create;
begin
  inherited Create;
  value:=TPBXValue.Create;
end;

destructor TPBXUnkProperty.Destroy;
begin
  value.Free;
  inherited Destroy;
end;

{ TPBXUnkClass }

constructor TPBXUnkClass.CreateWithName(const AISA: string);
begin
  fisa:=AISA;
  Create;
end;

constructor TPBXUnkClass.Create;
begin
  inherited Create;
end;

function TPBXUnkClass.GetIsaName: string;
begin
  Result:=fisa;
end;

{ TPBXValue }

destructor TPBXValue.Destroy;
begin
  arr.Free;
  keyval.Free;
  inherited Destroy;
end;

{ TPBXKeyValue }

procedure TPBXKeyValue.AddValStr(const name, AValue: string);
begin
  AddStr(name, Avalue);
end;

function TPBXKeyValue.AddVal(const name: string; atype: TPBXValueType): TPBXValue;
begin
  Result:=TPBXValue.Create;
  Result.valType:=atype;
  case atype of
    vtKeyVal: Result.keyval:=TPBXKeyValue.Create(true);
    vtArrayOfStr: Result.arr:=TPBXStringArray.Create;
  end;
  Add(name, Result);
end;

function TPBXKeyValue.GetValStr(const name: string): string;
var
  vl : TPBXValue;
begin
  vl:=TPBXValue(Self.Find(name));
  if not Assigned(vl)
    then Result:=''
    else Result:=vl.str;
end;

function TPBXKeyValue.AddStr(const name: string; const avalue: string): TPBXValue;
begin
  Result:=AddVal(name, vtString);
  Result.str:=avalue;
end;

function TPBXKeyValue.AddStrArray(const name: string): TPBXValue;
begin
  Result:=AddVal(name, vtArrayOfStr);
end;

function TPBXKeyValue.AddKeyVal(const name: string): TPBXValue;
begin
  Result:=AddVal(name, vtKeyVal);
end;


{ TPBXReref }

constructor TPBXReref.Create(ainstance: TObject; const apropname, aref: string);
begin
  inherited Create;
  instance := ainstance;
  propname := apropname;
  _id     := aref;
end;

{ TPBXObject }

function PBXObject.GetHasUnkProp: Boolean;
begin
  Result:=Assigned(_funkProp) and (_funkProp.Count>0);
end;

class procedure PBXObject._WriteEmpty(propnames: TStrings);
begin

end;

function PBXObject.GetUnkProp: TFPHashObjectList;
begin
  if not Assigned(_funkProp) then _funkProp:=TFPHashObjectList.Create(true);
  Result:=_funkProp;
end;

constructor PBXObject.Create;
begin
end;

destructor PBXObject.Destroy;
begin
  _funkProp.Free;
  inherited Destroy;
end;

function PBXObject.GetIsaName: string;
begin
  Result:=ClassName;
end;

function PBXObject._addUnkProp(const aname: string): TPBXUnkProperty;
begin
  Result:=TPBXUnkProperty.Create;
  Result.fname:=aname;
  _unkProp.Add(aname, Result);
end;

{ TPBXContainer }

procedure PBXReref(objs: TObjHashList; refs: TList);
var
  i       : integer;
  refobj  : TObject;
  r       : TPBXReref;
  prp     : PPropInfo;
  pcls    : TObject;
begin
  for i:=0 to refs.Count-1 do begin
    r := TPBXReref(refs[i]);
    refobj:=objs.Find(r._id);
    if Assigned(refobj) then begin
      prp:=GetPropInfo(r.instance, r.propname);
      if prp^.PropType^.Kind=tkClass then begin
        pcls:=GetObjectProp(r.instance, r.propname);
        if pcls is TPBXObjectsList then begin
          TPBXObjectsList(pcls).Add(refobj);
        end else begin
          //writeln('setting prop: ', r.propname,' ');
          SetObjectProp(r.instance, r.propname, refobj);
        end;
      end;
    end;
    //else writeln('no object found! ', r._id);
  end;
end;

procedure TPBXContainer.ReadObjects(p: TPBXParser; objs: TObjHashList);
var
  tk    : TPBXEntity;
  id    : string;
  cls   : string;
  obj   : PBXObject;
  i     : Integer;
  refs  : TList;
  cmt   : string;
begin
  tk:=p.FetchNextEntity;
  refs:=TList.Create;
  try
    while not (tk in [etError, etCloseObject]) do begin
      if tk=etOpenObject then begin
        id:=p.Name;
        cmt:=p.LastComment;
        cls:='';
        p.FetchNextEntity;
        if (p.CurEntity = etValue) and (p.Name = 'isa') then begin
          cls:=p.Value;
          obj:=AllocObject(cls);
          if not Assigned(obj) then
            obj:=TPBXUnkClass.CreateWithName(cls);
          if Assigned(obj) then begin
            obj._headerComment:=cmt;
            obj._id:=id;
            PBXReadClass(p, obj, refs);
            objs.Add(id, obj);
          end else begin
            PBXParserSkipLevel(p);
          end;

        end else
          PBXParserSkipLevel(p);
      end;
      tk:=p.FetchNextEntity;
    end;

    PBXReref(objs, refs);

  finally
    for i:=0 to refs.Count-1 do TObject(refs[i]).Free;
    refs.Free;
  end;
end;

function TPBXContainer.AllocObject(const nm: string): PBXObject;
var
  cls : PBXObjectClass;
begin
  cls:=PBXFindClass(nm);
  if not Assigned(cls) then Result:=nil
  else Result:=cls.Create;
end;

function TPBXContainer.ReadFile(s: TStream; var AFileInfo: TPBXFileInfo): Boolean;
var
  p     : TPBXParser;
  buf   : string;
  tk    : TPBXEntity;
  root  : string;
  objs  : TObjHashList;
  rt    : TObject;
begin
  Result:=false;
  AFileInfo.archiveVersion:='';
  AFileInfo.objectVersion:='';
  AFileInfo.rootObject:=nil;

  if not Assigned(s) then Exit;
  SetLength(buf, s.Size);
  s.Read(buf[1], length(buf));

  objs:=TObjHashList.Create(False);
  p:=TPBXParser.Create;
  try
    p.scanner.SetBuf(buf);
    if p.FetchNextEntity <> etOpenObject then Exit;


    tk:=p.FetchNextEntity;
    while not (tk in [etEOF,etError]) do begin
      if tk = etValue then begin
        if p.Name='archiveVersion' then AFileInfo.archiveVersion:=p.Value
        else if p.Name='objectVersion' then AFileInfo.objectVersion:=p.Value
        else if p.Name='rootObject' then root:=p.Value;
      end else if (tk=etOpenObject) and (p.Name = 'objects') then begin
        ReadObjects(p, objs);
      end;
      tk:=p.FetchNextEntity;
    end;

    if tk=etError then begin
      {for i:=0 to objs.Count-1 do
        TObject(objs[i]).Free;}
      Result:=false;
      Exit;
    end;

    rt:=objs.Find(root);

    if Assigned(rt) and (rt is PBXObject) then
      AFileInfo.rootObject:=PBXObject(rt);
    Result:=true;
  finally
    objs.Free;
    p.Free;
  end;
end;

function PBXReadObjectsListRef(p: TPBXParser; obj: PBXObject; propName: string; refs: TList): Boolean;
begin
  Result:=true;
  p.FetchNextEntity;
  while not (p.CurEntity in [etCloseArray, etEOF, etError]) do begin
    if p.CurEntity <> etValue then begin
      Result:=false;
      Exit;
    end;
    if p.Value<>'' then
      refs.Add ( TPBXReref.Create( obj, propName, p.Value ));
    p.FetchNextEntity;
  end;
end;

function PBXReadStringArray(p: TPBXParser; arr: TPBXStringArray): Boolean;
begin
  Result:=true;
  p.FetchNextEntity;
  while not (p.CurEntity in [etCloseArray, etEOF, etError]) do begin
    if p.CurEntity <> etValue then begin
      Result:=false;
      Exit;
    end;
    arr.Add(p.Value);
    p.FetchNextEntity;
  end;
end;

function GuessProperty(p: TPBXParser; uprop: TPBXUnkProperty): Boolean;
begin
  case p.CurEntity of
    etValue: begin
      // assuming string. Object Ref will be resolved later!
      uprop.value.str:=p.Value;
      uprop.value.valType:=vtString;
      Result:=true;
    end;
    etOpenArray: begin
      // assuming array of strings. Array of Objects will be resolved later!
      uprop.value.arr:=TPBXStringArray.Create;
      uprop.value.valType:=vtArrayOfStr;
      Result:=PBXReadStringArray(p, uprop.value.arr);
    end;
    etOpenObject: begin
      uprop.value.keyval:=TPBXKeyValue.Create;
      uprop.value.valType:=vtKeyVal;
      Result:=PBXReadKeyValue(p, uprop.value.keyval);
    end;
  else
    Result:=false;
  end;
end;

function PBXReadClass(p: TPBXParser; obj: PBXObject; refs: TList): Boolean;
var
  tk  : TPBXEntity;
  lvl : Integer;
  prp  : PPropInfo;
  pobj  : TObject;
  pk    : TTypeKind;
  uprop : TPBXUnkProperty;
begin
  lvl:=p.Level;
  tk:=p.FetchNextEntity;
  while p.Level>=lvl {tk<>tkCurlyBraceClose} do begin
    prp:=GetPropInfo(obj, p.Name);
    if Assigned(prp) then begin
      pk:=prp^.PropType^.Kind;
      if pk=tkClass then
        pobj:=GetObjectProp(obj, prp)
      else
        pobj:=nil;

      if tk=etValue then begin

        case pk of
          tkClass: begin
            //writeln('ref for: ',p.Name,' to ', p.Value);
            refs.Add( TPBXReref.Create(obj, p.Name, p.Value))
          end;
          tkInteger, tkInt64, tkQWord: begin
            SetInt64Prop(obj, p.Name, StrToIntDef(p.Value, GetInt64Prop(obj, p.Name)) );
          end;
          tkBool: begin
            SetOrdProp(obj, p.Name, StrToIntDef(p.Value, GetInt64Prop(obj, p.Name)) );
          end;
        else
          SetStrProp(obj, p.Name, p.Value);
        end;
      end else begin
        {write( p.CurEntity,' ',p.Name,' ',PtrUInt(pobj));
        if Assigned(pobj) then write(' ', pobj.ClassName);
        writeln;}
        if (pobj is TPBXObjectsList) and (tk = etOpenArray) then begin
          Result:=PBXReadObjectsListRef(p, obj, p.Name, refs);
          if not Result then Exit;
        end else if (pobj is TPBXStringArray) and (tk = etOpenArray) then begin
          Result:=PBXReadStringArray(p, TPBXStringArray(pobj) );
          if not Result then Exit;
        end else if (pobj is TPBXKeyValue) and (tk = etOpenObject) then begin
          Result:=PBXReadKeyValue(p, TPBXKeyValue(pobj) );
          if not Result then Exit;
        end else
          // array of object
          PBXParserSkipLevel(p);
      end;
    end else begin
      //writeln(obj.ClassName, ': unkown property: ', p.Name);
      uprop:=obj._addUnkProp(p.Name);
      GuessProperty(p,uprop);
      {if tk <> etValue then
        PBXParserSkipLevel(p);}
    end;

    tk:=p.FetchNextEntity;
  end;
  Result:=true;
end;

procedure PBXGatherObjects(obj: TObject; srz: TList);
var
  plist : PPropList;
  cnt   : Integer;
  i     : Integer;
  j     : Integer;
  k     : Integer;
  arr   : TPBXObjectsList;
  ch    : TObject;
  ach   : TObject;
  kind  : TTypeKind;
const
  FlagGet = 3;  // 1 + 2                  //ptField = 0;
  FlagSet = 12; // 4 + 8 , 16 + 32        //ptStatic = 1;
  FlagSP  = 16 + 32;                      //ptVirtual = 2;
  FlagIdx = 64;                           //ptConst = 3;
begin
  if (not Assigned(obj)) or (not Assigned(srz)) then Exit;

  srz.Add(obj);
  j:=0;
  while j<srz.Count do begin
    obj:=TObject(srz[j]);

    plist:=nil;
    cnt:=GetPropList(obj, plist);
    if Assigned(plist) then begin
      for i:=0 to cnt-1 do begin
        kind := plist^[i]^.PropType^.Kind;
        if (kind<>tkClass) then Continue;

        ch:=GetObjectProp(obj, plist^[i] );
        if not Assigned(ch) then Continue;

        if (plist^[i]^.PropProcs and FlagSet <> FlagSet) then begin
          if srz.IndexOf(ch)<0 then
            srz.Add ( ch );
        end else if ch is TPBXObjectsList then begin

          arr:=TPBXObjectsList(ch);
          for k:=0 to arr.Count-1 do begin
            ach:=arr[k];
            if srz.IndexOf(ach)<0 then srz.Add(ach);
          end;
        end;
      end;
      Freemem(plist);
    end;
    inc(j);
  end;
end;

function GetNextID(hash: TFPHashObjectList; var id: Int64): string;
begin
  repeat
    Result:=IntToHex(id, 24);
    inc(id);
  until not Assigned(hash.Find(Result));
end;


procedure PBXAssignRef(list: TList);
var
  i : Integer;
  p : PBXObject;
  id: Int64;
  usedid: TFPHashObjectList;
begin
  if not Assigned(list) then Exit;
  usedid:=TFPHashObjectList.Create(false);
  try
    for i:=0 to list.Count-1 do begin
      p:=PBXObject(list[i]);
      if p._id<>'' then usedid.Add(p._id, p);;
    end;


    id:=2; // root! :)
    for i:=0 to list.Count-1 do begin
      p:=PBXObject(list[i]);
      if not Assigned(p) then Continue;
      if (p._id='') then begin
        p._id:=GetNextID(usedid, id);
      end;
    end;
  finally
    usedid.Free;
  end;
  // 0AFA6EA519F60EFD004C8FD9
  // 123456789012345678901234
end;

procedure PBXWriteStrArray( w: TPBXWriter; list: TPBXStringArray );
var
  i : Integer;
begin
  w.OpenBlock('(');
  for i:=0 to list.Count-1 do
    w.WriteArrValue(list.Strings[i]);
  w.CloseBlock(')');
end;


procedure PBXWriteObjArray( w: TPBXWriter; list: TPBXObjectsList );
var
  i   : Integer;
  pbx : PBXObject;
begin
  for i:=0 to list.Count-1 do begin
    pbx:=PBXObject(list[i]);
    w.WriteArrValue(pbx._id, pbx._headerComment);
  end;
end;

procedure PBXWriteKeyValue( w: TPBXWriter; kv: TPBXKeyValue );
var
  i   : Integer;
  v   : TPBXValue;
  nm  : string;
begin
  w.OpenBlock( '{' );
  for i:=0 to kv.Count-1 do begin
    v:=TPBXValue(kv.Items[i]);
    nm:=kv.NameOfIndex(i);
    w.WriteName(nm);
    case v.valType of
      vtString: w.WriteValue(v.str);
      vtArrayOfStr: PBXWriteStrArray(w, v.arr);
      vtKeyVal: PBXWriteKeyValue(w, v.keyval);
    end;
  end;
  w.CloseBlock( '}' );
end;

type

  { TWriteProp }

  TWriteProp = class(TObject)
    propIdx : integer;
    unk     : TPBXUnkProperty;
    constructor Create(aidx: Integer); overload;
    constructor Create(aunk: TPBXUnkProperty); overload;
  end;

procedure PBXWriteObj(pbx: PBXObject; w: TPBXWriter; WriteEmpty: TStrings);
var
  p     : PPropList;
  cnt   : Integer;
  i,j   : Integer;
  isMan : Boolean;
  vl    : string;
  sobj  : TObject;
  nm    : string;
  vcmt  : string;
  isstr : Boolean;

  names : TStringList; // used for sorting.
                       // todo: find a better way for sort by names
  wp    : TWriteProp;
  up    : TPBXUnkProperty;
begin

  w.WriteName(pbx._id, pbx._headerComment);

  isMan:=(pbx.GetIsaName='PBXFileReference') or (pbx.GetIsaName='PBXBuildFile');
  if isMan then w.ManualLineBreak:=true;

  w.OpenBlock('{');
  w.WriteNamedValue('isa', pbx.GetIsaName);

  p:=nil;
  cnt:=GetPropList(pbx, p);

  //todo: I don't like this sorting at all!
  //      but it appears to be the most common available
  names:=TStringList.Create;
  try
    names.OwnsObjects:=true;

    for i:=0 to cnt-1 do begin
      wp:=TWriteProp.Create(i);
      names.AddObject(p^[i].Name, wp);
      //names.AddObject(p^[i].Name, TObject(PtrInt(i)));
    end;
    //for i:=0 to
    if pbx._hasUnkProp then begin
      for i:=0 to pbx._unkProp.Count-1 do begin
        up:=TPBXUnkProperty(pbx._unkProp[i]);
        wp:=TWriteProp.Create(up);
        names.AddObject(up.name, wp);
      end;
    end;

    names.Sort;

    for j:=0 to names.Count-1 do begin
      //i:=Integer(PtrInt(names.Objects[j]));

      vl:='';
      vcmt:='';
      isstr:=false;
      sobj:=nil;

      wp:=TWriteProp(names.Objects[j]);
      if not Assigned(wp.unk) then begin
        i:=wp.propIdx;
        nm:=p^[i].Name;
        if p^[i].PropType.Kind=tkClass then begin
          sobj:=GetObjectProp(pbx, p^[i])
        end else begin
          if p^[i].PropType.Kind in [tkAString, tkString] then begin
            vl:=GetStrProp(pbx,p^[i]);
            isstr:=(vl<>'') or (WriteEmpty.indexOf(nm)>=0);
          end else if p^[i].PropType.Kind in [tkInteger, tkInt64, tkQWord] then begin
            vl:=IntToStr(GetInt64Prop(pbx, p^[i]));
            isstr:=(vl<>'') or (WriteEmpty.indexOf(nm)>=0);
          end else if p^[i].PropType.Kind = tkBool then begin
            isstr:=PtrUInt(p^[i].Default)<>PtrUInt(p^[i].GetProc);
            if isstr then vl:=IntToStr(GetOrdProp(pbx, p^[i]));
          end;
        end;
      end else begin
        nm:=wp.unk.name;
        case wp.unk.value.valType of
          vtArrayOfStr: sobj:=wp.unk.value.arr;
          vtKeyVal: sobj:=wp.unk.value.keyval;
        else
          isstr:=true;
          vl:=wp.unk.value.str;
        end;
      end;

      if Assigned(sobj) then begin
        if sobj is PBXObject then begin
          vl:=PBXObject(sobj)._id;
          vcmt:=PBXObject(sobj)._headerComment;
          isstr:=vl<>'';
        end else if sobj is TPBXObjectsList then begin
          w.WriteName(nm); w.OpenBlock('(');
          PBXWriteObjArray( w, TPBXObjectsList(sobj) );
          w.CloseBlock(')');
        end else if sobj is TPBXStringArray then begin
          w.WriteName(nm);
          PBXWriteStrArray( w, TPBXStringArray(sobj) );
        end else if sobj is TPBXKeyValue then begin
          w.WriteName(nm);
          PBXWriteKeyValue(w, TPBXKeyValue(sobj));
        end;
      end;

      if isstr then begin
        w.WriteName(nm);
        w.WriteValue(vl,vcmt);
      end;

    end;

    if isMan then w.ManualLineBreak:=false;
    w.CloseBlock('}');
  finally
    names.Free;
    if Assigned(p) then Freemem(p);
  end;
end;

function PBXWriteContainer(const FileInfo: TPBXFileInfo; AssignRef: Boolean = true): string;
var
  lst : TList;
  st  : TStringList;
  i   : Integer;
  w   : TPBXWriter;
  sc  : string;
  pbx : PBXObject;
  emp : TStringList;
begin
  lst:=TList.Create;
  st:=TStringList.Create;
  emp:=TStringList.Create;
  try
    PBXGatherObjects(fileInfo.rootObject, lst);
    if AssignRef then PBXAssignRef(lst);

    for i:=0 to lst.Count-1 do begin
      st.AddObject( PBXObject(lst[i]).GetIsaName+' '+PBXObject(lst[i])._id, PBXObject(lst[i]));
    end;
    st.Sort;

    w:=TPBXWriter.Create;
    try
      sc:='';
      w.WriteRaw('// !$*UTF8*$!');
      w.WriteLineBreak;
      w.OpenBlock('{');
      w.WriteNamedValue('archiveVersion', FileInfo.archiveVersion);
      w.WriteName('classes'); w.OpenBlock('{'); w.CloseBlock('}');
      w.WriteNamedValue('objectVersion', FileInfo.objectVersion);
      w.WriteName('objects'); w.OpenBlock('{');
      for i:=0 to st.Count-1 do begin
        pbx:=PBXObject(st.Objects[i]);
        if sc<>pbx.GetIsaName then begin
          if sc<>'' then begin
            w.WriteLineComment('End '+sc+' section');
          end;
          sc:=pbx.GetIsaName;
          w.WriteLineBreak();
          w.WriteLineComment('Begin '+sc+' section');
          emp.Clear;
          pbx._WriteEmpty(emp);
        end;
        PBXWriteObj(pbx, w, emp);
      end;

      if sc<>'' then w.WriteLineComment('End '+sc+' section');
      w.CloseBlock('}');

      w.WriteNamedValue('rootObject', FileInfo.rootObject._id, FileInfo.rootObject._headerComment);
      w.CloseBlock('}');
      Result:=w.Buffer;
    finally
      w.Free;
    end;

  finally
    st.Free;
    lst.Free;
    emp.Free;
  end;
end;

{ TWriteProp }

constructor TWriteProp.Create(aidx: Integer);
begin
  inherited Create;
  propIdx:=aidx;
end;

constructor TWriteProp.Create(aunk: TPBXUnkProperty);
begin
  inherited Create;
  unk:=aunk;
end;

initialization
  pbxClassList := TStringList.Create;

finalization
  pbxClassList.Free;



end.
