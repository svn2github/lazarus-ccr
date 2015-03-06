unit cparserexp;

interface

uses
  cparsertypes;

type
  TIdentType = (itIdent, itIndex, itFuncCall, itField, itSubSel);

  TExpDir = (    // Expression Direction describes what relative fields should be initialzed
                 // for the expression node.
      edValue    // "none" - this should be a leaf of the expression graph
    , edPrefix   // "right"
    , edPostfix  // "left" for the host. "inner" is used for arrays and function calls
                 //   "left" ( "inner" )
    , edInfix    // "left" and "right" are mandatory. used for all binary operators!
    , edTernary  // used for ? operator. "main", "left", "right" are used, "main" ? "left" : "right"
    , edSequence // used for , operator (and parameters). "left" and "right" are used
  );

  TExp = class(TObject)
    left    : TExp;
    right   : TExp;
    main    : TExp;
    inner   : TExp;
    pr      : Integer;
    op      : string;
    val     : string;
    dir     : TExpDir;
    casttype  : string;
    identtype : TIdentType;
    constructor Create(apr: Integer; const aop: string =''; adir: TExpDir = edInfix);
  end;

const
  CIdentClose : array [TIdentType] of string = ('', ']',')', '', '');
  CIdentOpen  : array [TIdentType] of string = ('', '[','(', '.', '->');

function ParseCExprEx(p: TTextParser): TExp;

function ValuateIntExp(exp: TExp; macros: TCMacroHandler): Integer; overload;
function ValuateIntExp(const exp: string; macros: TCMacroHandler): Integer; overload;

function isCTypeCast(exp: TExp; tinfo: TCTypeInfo): Boolean;

implementation

function isCTypeCast(exp: TExp; tinfo: TCTypeInfo): Boolean;
var
  hasType: Boolean;
begin
  Result:=false;
  while Assigned(exp) do begin
    if exp.dir = edPostfix then begin
      exp:=exp.left;
    end else if exp.dir = edPrefix then begin
      exp:=exp.right;
    end else if (exp.dir = edValue) then begin
      if isStdCType(exp.val) then
        hastype:=true
      else begin
        hasType:=Assigned(tinfo) and (tinfo.isType(exp.val));
        if not hasType then Exit // an identify that's not a type
      end;
      exp:=nil;
    end else begin
      // nothing else os allowed in typecast
      Exit;
    end;
  end;
  Result:=hasType;
end;

function Rotate(core: TExp): TExp;
begin
  if Assigned(core.right) and (core.right.dir<>edValue) and (core.right.pr>=core.pr) then
  begin
    Result:=core.right;
    core.right:=Result.left;
    Result.left:=core;

    // additional rotate
    Result.left:=Rotate(core);
  end else
    Result:=core;
end;

function isIdentSep(const t: string; var it: TIdentType): Boolean;
begin
  Result:=true;
  if t='(' then it:=itFuncCall
  else if t = '[' then it:=itIndex
  else if t = '.' then it:=itField
  else if t = '->' then it:=itSubSel
  else Result:=false;
end;

function level2(p: TTextParser; NameOnly: Boolean = false): TExp;
var
  exp : TExp;
  res : Boolean;
  it  : TIdentType;
  e   : TExp;
begin
  Result:=nil;
  if p.TokenType=tt_Numeric then begin
    exp := TExp.Create(2, '', edValue);
    exp.val:=p.Token;
    p.NextToken;
    Result:=exp;
  end else if p.TokenType=tt_Ident then begin
    exp := TExp.Create(2, '', edValue);
    exp.val:=p.Token;
    p.NextToken;
    res:=isIdentSep(p.Token, it);
    if res then begin
      e:=TExp.Create(2, p.Token, edPostfix);
      e.left:=exp;
      e.identtype:=it;
      exp:=e;
      p.NextToken;
      if it in [itField, itSubSel] then
        exp.right:=level2(p, true)
      else if it in [itFuncCall, itIndex] then begin
        exp.inner:=ParseCExprEx(p);
        if p.Token = CIdentClose[it] then
          p.NextToken
        else begin
          // error!
        end;
      end;
    end else if (p.Token='++') or (p.Token='--') then begin
      e:=TExp.Create(2, p.Token, edPostfix);
      e.left:=exp;
      exp:=e;
      p.NextToken;
    end;
    Result:=exp;
  end;
end;

function level3(p: TTextParser): TExp;
var
  exp : TExp;
  ct  : TExp;
begin
  exp:=level2(p);
  if not Assigned(exp) then begin
    Result:=nil;
    // typecast
    if (p.Tokentype=tt_Symbol) and (p.Token='(') then begin
      p.NextToken;
      ct:=ParseCExprEx(p);
      if (p.TokenType=tt_Symbol) and (p.Token = ')') then
        p.NextToken;
      if not isCTypeCast(ct, p.CTypeInfo) then begin
        // not a typecast!
        ct.pr:=1;
        Result:=ct;
      end else begin
        Result:=TExp.Create(3, 'typecast', edInfix);
        Result.inner:=ct;
        Result.right:=ParseCExprEx(p);
        Result:=Rotate(REsult);
      end;
    end else if (p.Token='sizeof') or (p.Token='++') or (p.Token='--')
      or ((length(p.Token) = 1) and (p.Token[1] in ['&','*','~','!','-','+']))
    then begin
      Result:=TExp.Create(3, p.Token, edPrefix);
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end
  end else
    Result:=exp;
end;

function level5(p: TTextParser): TExp;
var
  e : TExp;
begin
  e:=level3(p);
  if Assigned(e) then begin
    if (p.TokenType = tt_Symbol) and ( (length(p.Token)=1) and (p.Token[1] in ['*','/','%'])) then
    begin
      Result:=TExp.Create(5, p.Token, edInfix);
      p.NextToken;
      Result.left:=e;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level6(p: TTextParser): TExp;
var
  e : TExp;
begin
  e:=level5(p);
  if Assigned(e) then begin
    if (p.TokenType = tt_Symbol) and ( (length(p.Token)=1) and (p.Token[1] in ['+','-'])) then
    begin
      Result:=TExp.Create(6, p.Token, edInfix);
      p.NextToken;
      Result.left:=e;         // a * b + c
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level7(p: TTextParser): TExp;
var
  e : TExp;
begin
  e:=level6(p);
  if Assigned(e) then begin
    if (p.TokenType = tt_Symbol) and ( (p.Token = '<<') or (p.Token='>>')) then
    begin
      Result:=TExp.Create(7, p.Token, edInfix);
      p.NextToken;
      Result.left:=e;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level8(p: TTextParser): TExp;
var
  e : TExp;
  tk : string;
begin
  e:=level7(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (p.TokenType = tt_Symbol) and ((tk='<') or (tk='<=') or (tk='>=') or (tk='>')) then
    begin
      Result:=TExp.Create(8, p.Token, edInfix);
      p.NextToken;
      Result.left:=e;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level9(p: TTextParser): TExp;
var
  e : TExp;
  tk : string;
begin
  e:=level8(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (p.TokenType = tt_Symbol) and ((tk='==') or (tk='!=')) then
    begin
      Result:=TExp.Create(9, p.Token, edInfix);
      Result.left:=e;
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level10(p: TTextParser): TExp;
var
  e : TExp;
  tk : string;
begin
  e:=level9(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (p.TokenType = tt_Symbol) and (tk='&') then
    begin
      Result:=TExp.Create(10, p.Token, edInfix);
      Result.left:=e;
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level11(p: TTextParser): TExp;
var
  e : TExp;
  tk : string;
begin
  e:=level10(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (p.TokenType = tt_Symbol) and (tk='^') then
    begin
      Result:=TExp.Create(11, p.Token, edInfix);
      Result.left:=e;
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level12(p: TTextParser): TExp;
var
  e : TExp;
  tk : string;
begin
  e:=level11(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (p.TokenType = tt_Symbol) and (tk='|') then
    begin
      Result:=TExp.Create(12, p.Token, edInfix);
      Result.left:=e;
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level13(p: TTextParser): TExp;
var
  e : TExp;
  tk : string;
begin
  e:=level12(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (p.TokenType = tt_Symbol) and (tk='&&') then
    begin
      Result:=TExp.Create(13, p.Token, edInfix);
      Result.left:=e;
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level14(p: TTextParser): TExp;
var
  e : TExp;
  tk : string;
begin
  e:=level13(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (p.TokenType = tt_Symbol) and (tk='||') then
    begin
      Result:=TExp.Create(14, p.Token, edInfix);
      Result.left:=e;
      p.NextToken;
      Result.right:=ParseCExprEx(p);
      Result:=Rotate(Result);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level15(p: TTextParser): TExp;
var
  e   : TExp;
  tk  : string;
begin
  e:=level14(p);
  if Assigned(e) then begin
    if p.Token='?' then begin
      p.NextToken;
      Result:=TExp.Create(15, '?', edTernary);
      Result.main:=e;
      Result.left:=ParseCExprEx(p);
      if p.Token = ':' then p.NextToken;
      Result.right:=ParseCExprEx(p);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level16(p: TTextParser): TExp;
var
  tk  : string;
  e   : TExp;
begin
  e:=level15(p);
  if Assigned(e) then begin
    tk:=p.Token;
    if (tk='=')
       or (tk='+=') or (tk='-=') or (tk='*=') or (tk='/=')
       or (tk='%=') or (tk='<<=') or (tk='>>=') or (tk='&=')
       or (tk='^=') or (tk='|=') then
    begin
      Result:=TExp.Create(16, tk, edInfix);
      Result.right:=e;
      p.NextToken;
      Result.left:=ParseCExprEx(p);
    end else
      Result:=e;
  end else
    Result:=nil;
end;

function level17(p: TTextParser): TExp;
var
  tk  : string;
  e   : TExp;
begin
  Result:=level16(p);
  if Assigned(Result) and (p.TokenType=tt_Symbol) and (p.Token=',') then begin
    e:=Result;
    p.NextToken;
    Result:=TExp.Create(17, ',', edSequence);
    Result.left:=e;
    Result.right:=ParseCExprEx(p);
  end;
end;

function ParseCExprEx(p: TTextParser): TExp;
begin
  Result:=level17(p);
end;

{ TExp }

constructor TExp.Create(apr: Integer; const aop: string; adir: TExpDir = edInfix);
begin
  inherited Create;
  pr:=apr;
  op:=aop;
  dir:=adir;
end;


function IntVal(exp: TExp; m: TCMacroHandler): Integer;
var
  code  : Integer;
  l, r  : Integer;
  lt    : TExp;
  rt    : TExp;
  nm    : string;
  s     : string;
const
  IntRes : array [boolean] of integer = (0,1);
begin
  Result:=0;

  if (exp.identtype = itFuncCall) and (exp.dir=edPostfix) then begin
    if Assigned(exp.left)  and (exp.left.identtype=itIdent)  then nm:=exp.left.val
    else nm:='';
    if Assigned(exp.inner) and (exp.inner.identtype=itIdent) then s:=exp.inner.val
    else s:='';
    if (nm='defined') and Assigned(m) then Result:=IntRes[ m.isMacroDefined(s)]
    else Result:=0;
  end else if exp.dir = edPrefix then begin
    r:=IntVal(exp.right, m);
    //writeln('koko! ', PtrUInt(exp.right));
    if exp.op='!' then begin
      if r = 0 then Result:=1
      else Result:=0;
    end;
    // it should be
  end else if exp.dir = edInfix then begin
    l:=IntVal(exp.left,m);
    r:=IntVal(exp.right,m);
    if exp.op = '+' then Result:=l+r
    else if exp.op = '-' then Result:=l-r
    else if exp.op = '/' then Result:=l div r
    else if exp.op = '%' then Result:=l mod r
    else if exp.op = '*' then Result:=l * r
    else if exp.op = '&' then Result:=l and r
    else if exp.op = '|' then Result:=l or r
    else if exp.op = '<<' then Result:=l shr r
    else if exp.op = '>>' then Result:=l shl r
    else if exp.op = '|' then Result:=l or r
    else if exp.op = '&' then Result:=l and r
    else if exp.op = '||' then Result:=IntRes[(l or r) > 0]
    else if exp.op = '&&' then Result:=IntRes[(l and r) > 0]
    else if exp.op = '==' then Result:=IntRes[l = r]
    else if exp.op = '!=' then Result:=IntRes[l <> r]
    else if exp.op = '>=' then Result:=IntRes[l >= r]
    else if exp.op = '<=' then Result:=IntRes[l <= r]
    else if exp.op = '>' then Result:=IntRes[l > r]
    else if exp.op = '<' then Result:=IntRes[l < r];
  end else begin
    Val(exp.val, Result, code);
  end;
end;

function ValuateIntExp(exp: TExp; macros: TCMacroHandler): Integer;
begin
  Result:=IntVal(Exp, macros);
end;

function ValuateIntExp(const exp: string; macros: TCMacroHandler): Integer;
var
  prs : TTextParser;
  expObj : TExp;
begin
  prs := CreateCParser(exp, false);
  try
    //no macros are defined for pre-compiler, they would be used in evaluation instead!
    //prs.MacroHandler:=macros;
    if prs.NextToken then begin
      expObj:=ParseCExprEx(prs);
      if Assigned(expObj)
        then Result:=ValuateIntExp(expObj, macros)
        else Result:=0;
    end else
      Result:=0;
  finally
    prs.Free;
  end;
end;

end.
