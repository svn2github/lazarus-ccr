program fractest;

{$mode objfpc}{$H+}
//{$MODESWITCH  ADVANCEDRECORDS}
uses
  Classes, sysutils, fractions, math;

var
  F1, F2, Res: TFraction;
  D: Extended;
  Prec: Extended;
  I64: Int64;
  i: Integer;
  G: Int64;
  B: Boolean;
  S: String;
  I32: integer;
  //R1,R2, D1, D2: Int64;
  //StartMS, EndMS: Comp;
  //i: Integer;


(*
function float2rat(x) {
    var tolerance = 1.0E-6;
    var h1=1; var h2=0;
    var k1=0; var k2=1;
    var b = x;
    do {
        var a = Math.floor(b);
        var aux = h1; h1 = a*h1+h2; h2 = aux;
        aux = k1; k1 = a*k1+k2; k2 = aux;
        b = 1/(b-a);
    } while (Math.abs(x-h1/k1) > x*tolerance);  <<wrong must be "<"

    return h1+"/"+k1;
}
*)

//uses method of Continued fractions
function FloatToFrac(F: Double; Precision: Double): TFraction;
var
  H1, H2, K1, K2, A, tmp: Int64;
  B, diff, test: Double;
begin
  H1 := 1;
  H2 := 0;
  K1 := 0;
  K2 := 1;
  b := F;
  repeat
    A := Round(Floor(b));
    tmp := H1;
    H1 := (a * H1) + H2;
    H2 := tmp;
    tmp := K1;
    K1 := (a * K1) + K2;
    K2 := tmp;
    B := 1 / (B - A);
    test := H1 / K1;
    diff := Abs(test - F);
  until (diff < Precision) or (H1 > $B504F334) or (K1 > $B504F334); // $B504F334 = Sqrt(High(Int64))
  Result.Numerator := H1;
  Result.Denominator := K1;
end;

function SBT_FloatToFraction(AValue: Double; AMaxNumerator, AMaxDenominator: Int64): TFraction; overload;
// "Stern-Brocot-Tree"
// http://stackoverflow.com/questions/5124743/algorithm-for-simplifying-decimal-to-fractions
// Ported by "circular"
var
  n: Integer;
  lower_n, lower_d, upper_n, upper_d, middle_n, middle_d: Int64;
  isNeg: Boolean;
  backup, newResult: TFraction;
  EPS: Double;
begin
  EPS := 0.01/AMaxDenominator;

  isNeg := AValue < 0;
  if isNeg then
    AValue := -AValue;

  n := Trunc(AValue);
  newResult := Fraction(Round(Avalue),1);
  //newResult.Init(round(AValue),1);
  if isNeg then newResult.Numerator := -newResult.Numerator;
  backup := newResult;

  AValue := AValue - n;

  // Lower fraction is 0/1
  lower_n := 0;
  lower_d := 1;

  // Upper fraction is 1/1
  upper_n := 1;
  upper_d := 1;

  while true do
  begin
    if abs(newResult.Numerator/newResult.Denominator - n - AValue) <
       abs(backup.Numerator/backup.Denominator - n - AValue) then
      backup := newResult;

    // middle fraction is (lower_n + upper_n) / (lower_d + upper_d)
    middle_n := lower_n + upper_n;
    middle_d := lower_d + upper_d;
    newResult := fraction(n * middle_d + middle_n, middle_d);
    newResult.Normalize;
    if (newResult.Numerator > AMaxNumerator) or
     (newResult.Denominator > AMaxDenominator) then
    begin
      result := backup;
      exit;
    end;

    if isNeg then newResult.Numerator := -newResult.Numerator;

    // AValue + EPS < middle
    if middle_d * (AValue + EPS) < middle_n then
    begin
      // middle is our new upper
      upper_n := middle_n;
      upper_d := middle_d;
    end else
    // middle < AValue - EPS
    if middle_n < (AValue - EPS) * middle_d then
    begin
      // moddle is our new lower
      lower_n := middle_n;
      lower_d := middle_d;
    end else
    // middle is our best fraction
    begin
      result := newResult;
      exit;
    end;
  end;
end;

(*

typedef
  struct{int num,denom} Fraction; //????

  Fail:
  D         = 0.499938637949526
  F.ToFloat = 0.500000000000000
  Prec      = 0.000000000000001
  Diff      = 0.000061362050474
  Magnitude = 61362050474
  F         = 1/2

------
Fraction Fraction::toFract(double val)
{    // find nearest fraction
 int intPart = (int)val;
 val -= (double)intPart;
 Fraction low(0, 1);           // "A" = 0/1
 Fraction high(1, 1);          // "B" = 1/1
 for (int i = 0; i < 100; ++i)
 {
  double testLow = low.denom * val - low.num;
  double testHigh = high.num - high.denom * val;
  if (testHigh < Precision * high.denom)
   break; // high is answer
  if (testLow < Precision * low.denom)
  {  // low is answer
   high = low;
   break;
  }
  if (i & 1)
  {  // odd step: add multiple of low to high
   double test = testHigh / testLow;
   int count = (int)test;    // "N"
   int num = (count + 1) * low.num + high.num;
   int denom = (count + 1) * low.denom + high.denom;
   if ((num > 0x8000) ||
       (denom > 0x10000))
    break;
   high.num = num - low.num;  // new "A"
   high.denom = denom - low.denom;
   low.num = num;             // new "B"
   low.denom = denom;
  }
  else
  {  // even step: add multiple of high to low
   double test = testLow / testHigh;
   int count = (int)test;     // "N"
   int num = low.num + (count + 1) * high.num;
   int denom = low.denom + (count + 1) * high.denom;
   if ((num > 0x10000) ||
       (denom > 0x10000))
    break;
   low.num = num - high.num;  // new "A"
   low.denom = denom - high.denom;
   high.num = num;            // new "B"
   high.denom = denom;
  }
 }
 return Fraction(intPart, 1) + high;
}


*)


Type
  TFracFunc = function(D, Prec: Double): TFraction;

procedure FTF_Test(Func1, Func2: TFracFunc; Name: String);
var
  F1,F2: TFraction;
  D1, D2, Prec, Diff1, Diff2: Double;
  i, Count: Integer;
  T: TextFile;
  procedure OpenT;
  begin
    if FileExists(Name+'_Test.txt') then
      Append(T)
    else
      Rewrite(T);
  end;
begin
  AssignFile(T,Name+'_Test.txt');
  OpenT;
  writeln(T,'Testing: ',Name);
  CloseFile(T);
//  Randomize;
  Count := 0;
  repeat
    D1 := Random;
    Prec := 0.1;
    for i := 1 to 15 do
    begin
      write('.');
      try
        F1 := Func1(D1, Prec);
        Diff1 := Abs(F1.ToFloat - D1);
        F2 := Func2(D1, Prec);
        Diff2 := Abs(F2.ToFloat - D1);
        if (Diff1 > Prec) or (Diff2 > Prec) then
        begin
          Inc(Count);
          OpenT;
          writeln(T,'Fail: ');
          writeln(T,'D          = ',D1:24:24);
          writeln(T,'F1.ToFloat = ',F1.ToFloat:24:24);
          writeln(T,'F2.ToFloat = ',F2.ToFloat:24:24);
          writeln(T,'Prec       = ',Prec:24:24);
          writeln(T,'Diff1      = ',Diff1:24:24);
          writeln(T,'Magnitude  = ',Round(Diff1/Prec));
          writeln(T,'Diff2      = ',Diff2:24:24);
          writeln(T,'Magnitude  = ',Round(Diff2/Prec));
          writeln(T,'F1         = ',F1.ToString);
          writeln(T,'F2         = ',F2.ToString);
          CloseFile(T);
          writeln;
          writeln('Fail: ');
          writeln('Fail: ');
          writeln('D          = ',D1:24:24);
          writeln('F1.ToFloat = ',F1.ToFloat:24:24);
          writeln('F2.ToFloat = ',F2.ToFloat:24:24);
          writeln('Prec       = ',Prec:24:24);
          writeln('Diff1      = ',Diff1:24:24);
          writeln('Magnitude  = ',Round(Diff2/Prec));
          writeln('Diff2      = ',Diff2:24:24);
          writeln('Magnitude  = ',Round(Diff2/Prec));
          writeln('F1         = ',F1.ToString);
          writeln('F2         = ',F2.ToString);
        end;
      except
        on E: Exception do
        begin
          Inc(Count);
          OpenT;
          writeln(T,'Exception: ',E.Classname,', Message: ',E.Message);
          writeln(T,'  D=',D1:16:16,' Prec=',Prec:16:16);
          CloseFile(T);
          writeln;
          writeln('Exception: ',E.Classname,', Message: ',E.Message);
          writeln('  D=',D1:16:16,' Prec=',Prec:16:16);
        end;
      end;
      Prec := Prec/10;
    end;
  until Count >= 15;
  OpenT;
  writeln(T,'Stopped after ',Count,' failures.');
  CloseFile(T);
  writeln('Stopped after ',Count,' failures.');
end;



procedure FTF_Test2(Func1, Func2: TFracFunc; Name: String);
var
  F,F1,F2: TFraction;
  D1, D2, Prec, Diff1, Diff2: Double;
  A,B: Int64;
  i, Count: Integer;
  T: TextFile;
  procedure OpenT;
  begin
    if FileExists(Name+'_Test2.txt') then
      Append(T)
    else
      Rewrite(T);
  end;
begin
  AssignFile(T,Name+'_Test2.txt');
  OpenT;
  writeln(T,'Testing: ',Name);
  CloseFile(T);
//  Randomize;
  Count := 0;
  repeat

    A := Random($8000000) + 1;
    B := Random($8000000) + 1;
    F.Numerator := Min(A,B);
    F.Denominator := Max(A,B);
    F.Normalize;


    D1 := F.ToFloat;
    Prec := 0.000000000000001;
    for i := 1 to 1 do
    begin
      write('.');
      try
        F1 := Func1(D1, Prec);
        F1.Normalize;
        F2 := Func2(D1, Prec);
        F2.Normalize;
        if (F <> F1) or (F <> F2) then
        begin
          Inc(Count);
          OpenT;
          writeln(T,'Fail: ');
          writeln(T,'D          = ',D1:24:24);
          writeln(T,'F1.ToFloat = ',F1.ToFloat:24:24);
          writeln(T,'F2.ToFloat = ',F1.ToFloat:24:24);
          writeln(T,'Prec       = ',Prec:24:24);
          writeln(T,'F          = ',F.ToString);
          writeln(T,'F1         = ',F1.ToString);
          writeln(T,'F2         = ',F2.ToString);
          writeln(T,'F<>F1      = ',F<>F1);
          writeln(T,'F<>F2      = ',F<>F2);
          CloseFile(T);
          writeln;
          writeln('Fail: ');
          writeln('D          = ',D1:24:24);
          writeln('F1.ToFloat = ',F1.ToFloat:24:24);
          writeln('F2.ToFloat = ',F1.ToFloat:24:24);
          writeln('Prec       = ',Prec:24:24);
          writeln('F          = ',F.ToString);
          writeln('F1         = ',F1.ToString);
          writeln('F2         = ',F2.ToString);
          writeln('F<>F1      = ',F<>F1);
          writeln('F<>F2      = ',F<>F2);
        end;
      except
        on E: Exception do
        begin
          Inc(Count);
          OpenT;
          writeln(T,'Exception: ',E.Classname,', Message: ',E.Message);
          writeln(T,'  D=',D1:16:16,' Prec=',Prec:16:16);
          CloseFile(T);
          writeln;
          writeln('Exception: ',E.Classname,', Message: ',E.Message);
          writeln('  D=',D1:16:16,' Prec=',Prec:16:16);
        end;
      end;
      Prec := Prec/10;
    end;
  until Count >= 15;
  OpenT;
  writeln(T,'Stopped after ',Count,' failures.');
  CloseFile(T);
  writeln('Stopped after ',Count,' failures.');
end;

(*
tofrac(dec)     [dec is the (decimal) number to be converted]
{
    num1 = 0    [these are integers]
    den1 = 1
    num2 = 1
    den2 = 0
    q = dec     [q is a float, the current value being worked on]
    n = 0
    loop
    {
        n = n + 1
        if (q > max_int)                    [prevent overflow]
            exit loop
        a = int(q)                          [a is an integer]
        num = num1 + a * num2
        den = den1 + a * den2
        if (q - a < epsilon)                [prevent divide by zero]
            exit loop
        q = 1 / (q - a)
        num1 = num2
        den1 = den2
        num2 = num
        den2 = den
    }
    until((abs(num/den - dec) < epsilon)    [stop when close enough]
          or
          (n > max_steps)                   [avoid infinite loops]
          or
          (num > max_numerator)             [stop if too big]
          or
          (den > max_denominator))

    return num, den
}

*)


var
  MaxDen,MaxNum: Int64;
  Diff: Double;

begin
{

D         = 0.478370306547731
F.ToFloat = 0.478370306564808
Prec      = 0.000000000000001
Diff      = 0.000000000017077
Magnitude = 17077
F         = 135507/283268

}



  F1 := Fraction(1,3);
  F2 := Fraction(4,5);
  Res := F1 * F2;
  writeln(F1.ToString,' * ',F2.ToString,' = ',Res.ToString);
  F1 := '1/15';
  F2 := Res;
  Res := F2 - F1;
  writeln(F2.ToString,' - ',F1.ToString,' = ',Res.ToString);
  F1 := Res;
  Res := F1 + 2;
  writeln(F1.ToString,' + 2 = ',Res.ToString,' which resolves to: ',Res.Resolve);
  Res := FloatToFraction(0.5, 0.001);
  writeln('FloatToFraction(0.5, 0.001) = ',Res.ToString);
  F1 := Fraction(1,1,2);
  writeln('Fraction(1,1,2) = ',F1.ToString,' = ',F1.Resolve);
  F1 := Fraction(-1,1,2);
  writeln('Fraction(-1,1,2) = ',F1.ToString,' = ',F1.Resolve);
  F1 := Fraction(1,1,-2);
  writeln('Fraction(1,1,-2) = ',F1.ToString,' = ',F1.Resolve);
  F1 := Fraction(-1,1,-2);
  writeln('Fraction(-1,1,-2) = ',F1.ToString,' = ',F1.Resolve);
  try
    F1 := Fraction(1,-1,2);
    writeln('Fraction(1,-1,2) = ',F1.ToString,' = ',F1.Resolve,' SHOULD RAISE EXCEPTION!');
  except
    writeln('Fraction(1,-1,2): Exception raised, as expected');
  end;
  Exit;




  D := 1E25;
  Prec := 1;
  B := TryFloatToFraction(D, Prec, F1);
  writeln(D:16:16,', ',Prec:16:16,' : ',B,' Expected: FALSE');

  //FloatToFraction := @CF_FloatToFraction;
  D         := 0.478370306547731;
  Prec      := 1E-19;
  B := TryFloatToFraction(D, Prec, F1, True);
  writeln(D:24:24,', ',Prec:24:24,' : ',B,' Expected: TRUE');
  B := TryFloatToFraction(D, Prec, F1, False);
  writeln(D:24:24,', ',Prec:24:24,' : ',B,' Expected: FALSE');
  //if B then
  begin
    Diff := Abs(D-F1.ToFloat);
    writeln('D    = ',D:24:24);
    writeln('F1   = ',F1.ToFloat:24:24);
    writeln('Diff = ',Diff);
    if (Diff>Prec) then writeln('Fail') else writeln('Ok');
  end;
  F2 := FloatToFractionDef(D,Prec, Fraction(1,1), False);
  writeln('F2 = ',F2.ToString,', Expected: 1/1');

  exit;


  D := 0.478370306547731; //0.826824217801914;//0.235382874263451; //0.499938637949526;
  Prec := 0.000000000000001 ; //0.000000000000001; //0.000000000001000; //0.000000000000001;
  F1 := MF_FloatTofraction(D, Prec);
  Diff := Abs(D-F1.ToFloat);
  writeln('D    = ',D:16:16);
  writeln('F1   = ',F1.ToFloat:16:16);
  writeln('Diff = ',Diff:16:16);
  if (Diff>Prec) then writeln('Fail') else writeln('Ok');

  //EXIT;

  FTF_Test(@MF_FloatToFraction, {@}Fractions.FloatToFraction ,'MF-Def');

  EXIT;

  F1 := Fractions.FloatToFraction(10/3, 0.0000001);
  writeln('F1 = ',F1.ToString);
  exit;

  //AllowZeroPowerZero := True;
  //B := TryStrToint(paramstr(1), I32);
  //if not B then I32 := 2;
  //F1 := Fraction(0,2);
  //F2 := F1 ** I32;
  //writeln(F1.ToString,' ** ',I32,' = ',F2.ToString);
  //EXIT;

  //F1 := Fraction(5,13);
  //F2 := Fraction(10,26);
  //if not (F1 < F2) then write ('NOT '); writeln(F1.ToString ,' < ',F2.ToString);
  //if not (F2 > F1) then write ('NOT '); writeln(F2.ToString ,' > ',F1.ToString);
  //if not (F2 < F1) then write ('NOT '); writeln(F2.ToString ,' < ',F1.ToString);
  //if not (F1 = F2) then write ('NOT ');  writeln(F1.ToString ,' = ',F2.ToString);
  //EXIT;

  //FracSymbol := 'div';
  {
  S := ParamStr(1);
  if S='' then S := '1 1'+FracSymbol+'2';
  B := TryStrTofraction(S, F1);
  write('B = ',B);
  if B then writeln(' ',S, ' -> ',F1.ToString)
  else writeln;
  //if B then
  //begin
  //  F2 := S;
  //  writeln('F1 = ',F1.ToString);
  //end;
  //F1 := StrToFractionDef(S, Fraction(123,456));
  //writeln('F1 = ',F1.ToString);
  exit;

  }
  D := 1.0 + 307/1700;
  D := pi;
  Prec := 0.01;
  MaxDen := 10;
  MaxNum := 4 * MaxDen ;


  for i := 1 to 10 do
  begin
    writeln('Prec    = ',Prec:16:16);
    writeln('MaxNum  = ',MaxNum,' MaxDen = ',MaxDen);
    F1 := FloatTofraction(D, Prec);
    writeln('FloatToFraction:');
    writeln('Result  = ',F1.ToString);
    //writeln('D       = ',D:16:16);
    writeln('ToFloat = ',(F1.ToFloat):16:16);
    writeln('Diff    = ',Abs(D-(F1.ToFloat)):16:16);

    F1 := SBT_FloatToFraction(D, MaxNum, MaxDen);
    writeln('FloatToFraction2:');
    writeln('Result  = ',F1.ToString);
    //writeln('D       = ',D:16:16);
    writeln('ToFloat = ',(F1.ToFloat):16:16);
    writeln('Diff    = ',Abs(D-(F1.ToFloat)):16:16);
    Prec := Prec / 10;
    MaxNum := 10*MaxNum;
    MaxDen := 10*MaxDen;
    writeln;
  end;
  EXIT;
  F1 := Fraction(2,3);
  F2 := Fraction(1,5);
  Res := F1 + F2;
  writeln(F1.ToString,' + ',F2.ToString,' = ',Res.ToString);
  Res := F1 - F2;
  writeln(F1.ToString,' - ',F2.ToString,' = ',Res.ToString);
  Res := F1 * F2;
  writeln(F1.ToString,' * ',F2.ToString,' = ',Res.ToString);
  Res := F1 / F2;
  writeln(F1.ToString,' / ',F2.ToString,' = ',Res.ToString);
  Res := '123/456';
  writeln('123/456 -> ',Res.ToString);
  Res := '123';
  writeln('123 -> ',Res.ToString);
  Res := '456/123';
  writeln('456/123: ToString = ',Res.ToString,' Resolve = ',Res.Resolve);
  Res := '456/-123';
  writeln('456/-123: ToString = ',Res.ToString,' Resolve = ',Res.Resolve);
  Res := '25/5';
  writeln('25/5: ToString = ',Res.ToString,' Resolve = ',Res.Resolve);

  F1 := '10/20'; F2 := '2/4';
  writeln('(',F1.ToString,' = ',F2.ToString,') = ',F1=F2);
  F1 := '1/2'; F2 := '21/40';
  writeln('(',F1.ToString,' = ',F2.ToString,') = ',F1=F2);

  if not (F1 < F2) then write('NOT '); writeln(F1.ToString,' < ',F2.ToString);
  if not (F2 > F2) then write('NOT '); writeln(F1.ToString,' > ',F2.ToString);;
  if not (F1 <= F2) then write('NOT '); writeln(F1.ToString,' <= ',F2.ToString);;
  if not (F1 >= F2) then write('NOT '); writeln(F1.ToString,' >= ',F2.ToString);;
  F2 := -F1;
  writeln('-F1 = ',F2.ToString);
end.

