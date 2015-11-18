unit fractions;

{ A unit for doing arithmatics with fractions

  Copyright (C) 2015 by Lazarus and FreePascal community
  (http://www.lazarus.freepascal.org and http://www.freepascal.org)
  Original code by
    Bart Broersma (www.flyingsheep.nl)
    HappyLarry

  Portions copyright by David Peterson and The Math Forum @ Drexel,
  (redistributed with the consent of the copyright holder)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}
{$MODESWITCH  ADVANCEDRECORDS}

interface

uses
  SysUtils, Math;

type

  { TFraction }

  TFraction = record
  private
    FNumerator: Int64;
    FDenominator: Int64;
    procedure SetDominator(AValue: Int64);
  public
    procedure Normalize;
    function ToString: String;
    function Resolve: String;
    function ToFloat: Double;
    property Numerator: Int64 read FNumerator write FNumerator;
    property Denominator: Int64 read FDenominator write SetDominator;
  end;

  TFloatToFractionFunc = function(Value, Precision: Double): TFraction;
  TGreatestCommonDivisorFunc = function(a, b: Int64): Int64;

function Fraction(ANumerator, ADenominator: Int64): TFraction;
function Fraction(AIntPart, ANumerator, ADenominator: Int64): TFraction;

operator = (F1: TFraction; F2: TFraction) B: Boolean;
operator < (F1: TFraction; F2: TFraction) B: Boolean;
operator > (F1: TFraction; F2: TFraction) B: Boolean;
operator <= (F1: TFraction; F2: TFraction) B: Boolean;
operator >= (F1: TFraction; F2: TFraction) B: Boolean;
operator := (I: Int64) F: TFraction;
operator := (S: String) F: TFraction;
operator + (L: TFraction; R: TFraction) F: TFraction;
operator - (L: TFraction; R: TFraction) F: TFraction;
operator - (L: TFraction) F: TFraction;
operator * (L: TFraction; R: TFraction) F: TFraction;
operator * (L: TFraction; R: Int64) F: TFraction;
operator * (L: Int64; R: TFraction) F: TFraction;
operator / (L: TFraction; R: TFraction) F: TFraction;
operator / (L: TFraction; R: Int64) F: TFraction;
operator ** (L: TFraction; R: Integer) F: TFraction;

function GreatestCommonDivisor_DivisionBased(a, b: Int64): Int64;
function GreatestCommonDivisor_EuclidSubraction(a, b: Int64): Int64;
function CF_FloatToFraction(Value, Precision: Double): TFraction; //based upon Continue Fractios
function MF_FloatToFraction(Value, Precision: Double): TFraction; //Original code by David Peterson and The Math Forum @ Drexel

function StrToFraction(const S: String): TFraction;
function TryStrToFraction(const S: String; out F: TFraction): Boolean;
function StrToFractionDef(const S: String; Def: TFraction): TFraction;
function TryFloatToFraction(Value, Precision: Double; out F: TFraction; AcceptPrecisionError: Boolean = True): Boolean;
function FloatToFractionDef(Value, Precision: Double; Def: TFraction; AcceptPrecisionError: Boolean = True): TFraction;

function Min(a, b: TFraction): TFraction; inline; overload;
function Max(a, b: TFraction): TFraction; inline; overload;
function InRange(const AValue, AMin, AMax: TFraction): Boolean; inline; overload;
function EnsureRange(const AValue, AMin, AMax: TFraction): TFraction; inline;  overload;
function Sign(const AValue: TFraction): TValueSign; inline; overload;
function IsZero(const AValue: TFraction): Boolean; overload;
function Abs(const AValue: TFraction): TFraction; overload;

function Floor(D: Double): Int64; overload;
procedure AdjustPrecision(var Precision: Double; Value: Double);

var
  FloatToFraction: TFloatToFractionFunc = @MF_FloatTofraction;
  GreatestCommonDivisor: TGreatestCommonDivisorFunc = @GreatestCommonDivisor_DivisionBased;

ResourceString
  SDenominatorCannotBeZero = 'The denominator of a fraction cannot be 0';

const
  FracSymbol: string = '/';
  AllowZeroPowerZero: Boolean = False;
  SDivisionByZero = 'Division by zero';
  SZeroPowerZero = 'Raising 0 to the power 0 is undifined';
  SInvalidFraction = '"%s" is not a valid fraction';
  SRangeCheckError = 'Range check error';

const
  MaxInt64 = High(Int64);
  MinInt64 = Low(Int64);

implementation

//Math.Floor returns Integers and does NOT cause RangeCheck or OverFlow errors,
//So Math.Floor(1/9E-11) returns
//  -1773790777 [FFFFFFFF964619C7] instead of
//  11111111111 [00000002964619C7]  which is the correct Int64 answer
//this causes an infinite loop in CF_FloatToFraction function

function Floor(D: Double): Int64;
begin
  Result := Trunc(D);
  if Frac(D) < 0 then
    Result := Result - 1;
end;

function GreatestCommonDivisor_DivisionBased(a, b: Int64): Int64;
var
  temp: Int64;
begin
  while b <> 0 do
  begin
    temp := b;
    b := a mod b;
    a := temp
  end;
  result := a
end;

function GreatestCommonDivisor_EuclidSubraction(a, b: Int64): Int64;
//Euclids subtraction method (http://en.wikipedia.org/wiki/Euclidean_algorithm)
//only workswith positive integers
begin
  if (a = 0) then Exit(b);
  if (b = 0) then Exit(a);
  if (a < 0) then a := -a;
  if (b < 0) then b := -b;
  while not (a = b) do
  begin
    if (a > b) then
     a := a - b
    else
     b := b - a;
  end;
  Result := a;
end;


procedure AdjustPrecision(var Precision: Double; Value: Double);
const
  MaxPrec = Double(1.0)/MaxInt64;
begin
  Precision := Abs(Precision);
  if ((Abs(Value) / Precision) > 1E15) then
  begin
    //writeln('Value / Precision > 1E15');
    Precision := Abs(Value) / 1E16;
  end;
  if (Precision < MaxPrec) then
    Precision := MaxPrec;
end;

function InRange64(Value: Double): Boolean; inline;
begin
  Result := not ((Value > MaxInt64) or (Value < MinInt64));
end;

procedure CheckRange(Value: Double);
begin
  if not InRange64(Value) then
    raise ERangeError.Create(SRangeCheckError);
end;

function IsBorderlineValue(Value: Double; out F: TFraction): Boolean;
const
  MaxPrec = Double(1.0)/MaxInt64;
  ZeroBoundary = MaxPrec / 2;
begin
  if (Abs(Value) <= MaxPrec) then
  begin
    Result := True;
    //writeln('Abs(Value) < 1/MaxInt64 [',MaxPrec,']');
    if (Abs(Value) < ZeroBoundary) then
    begin
      //writeln('Abs(Value) < ZeroBoundary [',ZeroBoundary,']');
      F.Numerator := 0;
      F.Denominator := 1;
    end
    else
    begin
      if (Value < 0) then
      F.Numerator := -1
        else
      F.Numerator := 1;
      F.Denominator := MaxInt64;
    end;
  end
  else
    Result := False;
end;

//uses method of Continued fractions
function CF_FloatToFraction(Value, Precision: Double): TFraction;
var
  H1, H2, K1, K2, A, NewA, tmp: Int64;
  B, diff, test: Double;
  PendingOverFlow, Found: Boolean;
begin
  CheckRange(Value);
  AdjustPrecision(Precision, Value);
  //Borderline cases
  if IsBorderlineValue(Value, Result) then
    Exit;
  H1 := 1;
  H2 := 0;
  K1 := 0;
  K2 := 1;
  b := Value;
  NewA := Round(Floor(b));
  repeat
    A := NewA;
    tmp := H1;
    H1 := (a * H1) + H2;
    H2 := tmp;
    tmp := K1;
    K1 := (a * K1) + K2;
    K2 := tmp;
    //write('H1=',H1,' K1=',K1,' A=',A);
    test := H1 / K1;
    //write(' test=',test);
    diff := Abs(test - Value);
    //write(' diff=',diff);
    Found := (diff < Precision);
    if not Found then
    begin
      if (Abs(B-A) < 1E-30) then
        B := 1E30   //happens when H1/K2 exactly matches Value
      else
        B := 1 / (B - A);
      //write(' B=',B);
      PendingOverFlow := (((Double(B) * H1) + H2) > MaxInt64) or
                         (((Double(B) * K1) + K2) > MaxInt64) or
                         (B > MaxInt64);
      //writeln(' PendingOverFlow=',PendingOverFlow);
      if not PendingOverFlow then
        NewA := Round(Floor(B));

      {
      if PendingOverFlow then
      begin
        writeln('PendingOverFlow');
        writeln('New H1   = ',(Double(NewA) * H1) + H2);
        writeln('New K1   = ',(Double(NewA) * K1) + K2);
        writeln('B        = ',B);
        writeln('MaxInt64 = ',Double(MaxInt64));
      end;
      }
    end;
  until Found or PendingOverFlow;
  Result.Numerator := H1;
  Result.Denominator := K1;
end;

{
  This implementation of FloatToFraction was originally written by:
  David Peterson and The Math Forum @ Drexel
  Source: http://mathforum.org/library/drmath/view/51886.html
  It was ported to FreePascal by Bart Broersma
  Adjustments made:
    * Precision is bound by a magnitude of -15 to Value
    * Bordeline cases close to zero are handled
    * Handle negative values
    * Handle negative precision
    * Original code dealt with 32-bit integers, it was adjusted for 64-bit integers

  The original copyrigh holder has granted me permission to adjust and redistribute
  this code under modified LGPL license with kining exception (see COPYING.modifiedLGPL.txt).

  When redistributing this code, the comments above MUST also be redistributed with it!
}
function MF_FloatToFraction(Value, Precision: Double): TFraction;
var
  IntPart, Count, Num, Denom: Int64;
  i: Integer;
  TestLow, TestHigh, Test: Double;
  L,H: TFraction;
  IsNeg: Boolean;
begin // find nearest fraction
  //writeln('MF_FloatToFraction:');
  //writeln('  Value = ',Value);
  //writeln('  Prec  = ',Precision);
  CheckRange(Value);
  AdjustPrecision(Precision, Value);
  //Borderline cases
  if IsBorderlineValue(Value, Result) then
    Exit;
  IsNeg := (Value < 0);
  Value := Abs(Value);
  intPart := Round(Int(Value));
  Value := Frac(Value);
  L.Numerator := 0;
  L.Denominator := 1;
  H.Numerator := 1;
  H.denominator := 1;
  for i := 1 to 100 do   //was 100
  begin
    //writeln('  i = ',i);
    testLow := L.Denominator * Value - L.Numerator;
    testHigh := H.Numerator - H.Denominator * Value;
    //if (testHigh < Precision * H.Denominator) then
    if (Abs(H.ToFloat - Value) < Precision) then
    begin
      //writeln('  (testHigh < Precision * H.Denominator)');
      //writeln('  testHigh = ',testHigh);
      //writeln('  Precision * H.Denominator = ',Precision * H.Denominator);
      break; // high is answer
    end;
    //if (testLow < Precision * L.Denominator) then
    if (Abs(L.ToFloat - Value) < Precision) then
    begin // low is answer
      //writeln('  (testLow < Precision * L.Denominator)');
      //writeln('  testLow = ',testLow);
      //writeln('  Precision * L.Denominator = ',Precision * L.Denominator);
      H := L;
      break;
    end;
    if Odd(i) then
    begin // odd step: add multiple of low to high
      //writeln('  Odd step');
      test := testHigh / testLow;
      count := Round(Int(test)); // "N"
      num := (count + 1) * L.Numerator + H.Numerator;
      denom := (count + 1) * L.Denominator + H.Denominator;
      if ((num > High(Int64) - 1) or    // was 8000, 10000
      (denom > High(Int64) - 1)) then
      //if ((num > $8000) or    // was 8000, 10000
      //(denom > $10000)) then
      begin
        //writeln('  ((num > High(Int64) - 1) or (denom > High(Int64) - 1))');
        break;
      end;
      H.Numerator := num - L.Numerator; // new "A"
      H.Denominator := denom - L.Denominator;
      L.Numerator := num; // new "B"
      L.Denominator := denom;
    end
    else
    begin // even step: add multiple of high to low
      //writeln('  Even step');
      test := testLow / testHigh;
      count := Round(Int(test)); // "N"
      num := L.Numerator + (count + 1) * H.Numerator;
      denom := L.Denominator + (count + 1) * H.Denominator;
      if ((num > High(Int64) - 1) or   //10000. 10000
      (denom > High(Int64) - 1)) then
      //if ((num > $10000) or   //10000. 10000
      //(denom > $10000)) then
      begin
        //writeln('  ((num > High(Int64) - 1) or (denom > High(Int64) - 1))');
        break;
      end;
      L.Numerator := num - H.Numerator; // new "A"
      L.Denominator := denom - H.Denominator;
      H.Numerator := num; // new "B"
      H.Denominator := denom;
    end;
    //writeln('  End of loop iteration for i = ',i);
  end;
  //return Fraction(intPart, 1) + high;
  //writeln('MF_FloatToFraction: assigning Result');
  //writeln('H = ',H.ToString);
  //writeln('IntPart := ',IntPart);
  //Avoid call to TFraction.Normalize in Result := H + IntPart
  Result := H;
  Result.Numerator := Result.Numerator+ (Result.Denominator * IntPart);
  if IsNeg then
    Result.Numerator := -Result.Numerator;
  //writeln('MF_FloatToFraction End.');
end;


function TryFloatToFraction(Value, Precision: Double; out F: TFraction;
                            AcceptPrecisionError: Boolean): Boolean;
begin
  Result := False;
  if not InRange64(Value) then Exit;
  AdjustPrecision(Precision, Value);
  try
    F := FloatToFraction(Value, Precision);
    Result := AcceptPrecisionError or (Abs(Value - F.ToFloat) <= Precision)
  except
    Result := False;
  end
end;

function FloatToFractionDef(Value, Precision: Double; Def: TFraction;
                            AcceptPrecisionError: Boolean): TFraction;
begin
  if not TryFloatToFraction(Value, Precision, Result, AcceptPrecisionError) then
    Result := Def;
end;


function StrToFraction(const S: String): TFraction;
begin
  if not TryStrToFraction(S, Result) then raise EConvertError.CreateFmt(SInvalidFraction, [S]);
end;

{
  S is either:
  A single fraction e.g. 2/3
  A single integer e.g. 10
  An integer + a fraction, separated by a single space (#32) e.g. 2 1/2 (two and a half)
}
function TryStrToFraction(const S: String; out F: TFraction): Boolean;
type
  TFracType = (ftComplex, ftSingleFraction, ftInteger);
var
  FracType: TFracType;
  SInt, SNum, SDen: String;
  IntPart, NumPart, DenPart: Integer;
  PSpace, PFrac: SizeInt;
begin
  Result := False;
  SInt := '';
  SNum := '';
  SDen := '';
  IntPart := 0;
  NumPart := 0;
  DenPart := 0;
  PSpace := Pos(#32, S);
  PFrac := Pos(FracSymbol,S);
  if (PSpace > 0) and (PFrac > 0) then FracType := ftComplex
  else if (PSpace = 0) and (PFrac > 0) then FracType := ftSingleFraction
  else FracType := ftInteger;
  case FracType of
    ftComplex:
      begin
        //writeln('ftComplex');
        SInt := Copy(S, 1, PSpace - 1);
        SNum := Copy(S, PSpace + 1, PFrac - PSpace - 1);
        SDen := Copy(S, PFrac + Length(FracSymbol), MaxInt);
        //no spaces allowed in SNum or SDen
        //writeln('SInt = "',sint,'" SNum = "',snum,'" SDen = "',sden,'"');
        if (Pos(#32, SNum) > 0) or (Pos(#32, SDen) > 0) then Exit;
        if not (TryStrToInt(SInt, IntPart) and TryStrToInt(SNum, NumPart) and TryStrToInt(SDen, DenPart))
                or (DenPart = 0) or (NumPart < 0) then
                Exit;
        if (DenPart < 0) then
        begin
          DenPart := -DenPart;
          NumPart := -NumPart;
        end;
        if (IntPart = 0) then
        begin
          F.Numerator := NumPart;
          F.Denominator := DenPart;
        end
        else
        begin
          if (NumPart < 0) then
          begin
            NumPart := -NumPart;
            IntPart := -IntPart;
          end;
          if (IntPart > 0) then
            F.Numerator := NumPart + Int64(DenPart) * IntPart
          else
            F.Numerator := -(NumPart - Int64(DenPart) * IntPart);
          F.Denominator := DenPart;
        end;
      end;
    ftSingleFraction:
      begin
        //writeln('ftSingleFraction');
        SNum := Copy(S, 1, PFrac - 1);
        SDen := Copy(S, PFrac + Length(FracSymbol), MaxInt);
        //writeln('SNum = "',snum,'" SDen = "',sden,'"');
        //no spaces allowed in SNum or SDen
        if (Pos(#32, SNum) > 0) or (Pos(#32, SDen) > 0) then Exit;
        if not (TryStrToInt(SNum, NumPart) and TryStrToInt(SDen, DenPart))
                or (DenPart = 0) then
                Exit;
        F.Numerator := NumPart;
        F.Denominator := DenPart;
      end;
    ftInteger:
      begin
        //writeln('ftInteger');
        //no spaces allowed in SInt
        if (Pos(#32, SInt) > 0) then Exit;
        if not TryStrToInt(S, IntPart) then Exit;
        F.Numerator := IntPart;
        F.Denominator := 1;
      end;
  end;//case
  Result := True;
end;

function StrToFractionDef(const S: String; Def: TFraction): TFraction;
begin
  if not TryStrToFraction(S, Result) then Result := Def;
end;


function Min(a, b: TFraction): TFraction;
begin
  if (a < b) then
    Result := b
  else
    Result := b;
end;

function Max(a, b: TFraction): TFraction;
begin
  if (a > b) then
    Result := a
  else
    Result := b;
end;

function InRange(const AValue, AMin, AMax: TFraction): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function EnsureRange(const AValue, AMin, AMax: TFraction): TFraction;
begin
  If (AValue < AMin) then
    Result := AMin
  else if (AValue > AMax) then
    Result := AMax
  else
    Result := AValue;
end;

function Sign(const AValue: TFraction): TValueSign;
begin
  if (AValue.Denominator < 0) then
  begin
    AValue.Numerator := -AValue.Numerator;
    AValue.Denominator := -AValue.Denominator;
  end;
  if (AValue.Numerator < 0) then
    Result := NegativeValue
  else if (AValue.Numerator > 0) then
    Result := PositiveValue
  else
    Result := ZeroValue;
end;

function IsZero(const AValue: TFraction): Boolean;
begin
  Result := (AValue.Numerator = 0);
end;

function Abs(const AValue: TFraction): TFraction;
begin
  Result := AValue;
  Result.Normalize;
  if (Result.Numerator < 0) then Result.Numerator := -Result.Numerator;
end;


function Fraction(ANumerator, ADenominator: Int64): TFraction;
begin
  Result.Numerator := ANumerator;
  //will raise exception if ADenominator = 0
  Result.Denominator := ADenominator;
end;

function Fraction(AIntPart, ANumerator, ADenominator: Int64): TFraction;
var
  IsNeg: Boolean;
begin
  if (ANumerator < 0) then
    raise EMathError.CreateFmt(SInvalidFraction,[Format('%d %d/%d',[AIntPart,ANumerator,ADenominator])]);
  IsNeg := (AIntPart < 0) xor (ADenominator < 0);
  AIntPart := Abs(AIntPart);
  ADenominator := Abs(ADenominator);
  Result.Numerator := ANumerator + (ADenominator * AIntPart);
  if IsNeg then Result.Numerator := - Result.Numerator;
  //will raise exception if ADenominator = 0
  Result.Denominator := ADenominator;
end;



operator = (F1: TFraction; F2: TFraction) B: Boolean;
begin
  F1.Normalize;
  F2.Normalize;
  B := (F1.Numerator = F2.Numerator) and (F1.Denominator = F2.Denominator);
end;

operator < (F1: TFraction; F2: TFraction) B: Boolean;
begin
  F1.Normalize;
  F2.Normalize;
  B := (F1.Numerator * F2.Denominator) < (F2.Numerator * F1.Denominator);
end;

operator > (F1: TFraction; F2: TFraction) B: Boolean;
begin
  F1.Normalize;
  F2.Normalize;
  B := (F1.Numerator * F2.Denominator) > (F2.Numerator * F1.Denominator);
end;

operator <= (F1: TFraction; F2: TFraction) B: Boolean;
begin
  B := (F1 < F2) or (F1 = F2);
end;

operator >= (F1: TFraction; F2: TFraction) B: Boolean;
begin
  B := (F1 > F2) or (F1 = F2);
end;

operator := (I: Int64) F: TFraction;
begin
  F.Numerator := I;
  F.Denominator := 1;
end;

operator := (S: String) F: TFraction;
begin
  if not TryStrToFraction(S, F) then
    raise EConvertError.CreateFmt(SInvalidFraction, [S]);
end;


operator + (L: TFraction; R: TFraction) F: TFraction;
begin
  F.Numerator := L.Numerator * R.Denominator + R.Numerator * L.Denominator;
  F.Denominator := L.Denominator * R.Denominator;
  F.Normalize;
end;


operator - (L: TFraction; R: TFraction) F: TFraction;
begin
  R.Numerator := - R.Numerator;
  F := L + R;
end;


operator - (L: TFraction) F: TFraction;
begin
  F.Numerator := - L.Numerator;
  F.Denominator := L.Denominator;
  F.Normalize;
end;

operator * (L: TFraction; R: TFraction) F: TFraction;
begin
  L.Normalize;
  R.Normalize;
  F.Numerator := L.Numerator * R.Numerator;
  F.Denominator := L.Denominator * R.Denominator;
  F.Normalize;
end;

operator * (L: TFraction; R: Int64) F: TFraction;
begin
  F := L;
  F.Normalize;
  F.Numerator := L.Numerator * R;
  F.Normalize;
end;

operator * (L: Int64; R: TFraction) F: TFraction;
begin
  F := R * L;
end;

operator / (L: TFraction; R: TFraction) F: TFraction;
var
  Temp: TFraction;
begin
  Temp.Numerator := R.Denominator;
  //this will raise an exception if R = 0
  Temp.Denominator := R.Numerator;
  F := L * Temp;
end;

operator / (L: TFraction; R: Int64) F: TFraction;
begin
  if (R = 0) or (L.Denominator = 0) then raise EZeroDivide.Create(SDivisionByZero);
  F := L;
  F.Normalize;
  F.Denominator := F.Denominator * R;
  F.Normalize;
end;

operator ** (L: TFraction; R: Integer) F: TFraction;
var
  i: Integer;
begin
  L.Normalize;
  F := L;
  if (R = 0) then
  begin
    if (L.Numerator = 0) and not AllowZeroPowerZero then raise EMathError.Create(SZeroPowerZero);
    F.Numerator := 1;
    F.Denominator := 1;
  end
  else if (R > 0) then
  begin
    for i := 1 to R-1 do F := F * L
  end
  else
  begin
    F := 1 / F;
    L := F;
    for i := 1 to -(R)-1 do F := F * L ;
  end;
end;


procedure TFraction.SetDominator(AValue: Int64);
begin
  if (AValue = 0) then raise EZeroDivide.Create(SDivisionByZero);
  if (FDenominator = AValue) then Exit;
  FDenominator := AValue;
end;

procedure TFraction.Normalize;
var
  GCD: Int64;
begin
  if (Denominator < 0) then
  begin
    Numerator := - Numerator;
    Denominator := - Denominator;
  end;
  GCD := GreatestCommonDivisor(Numerator, Denominator);
  if (GCD <> 1) then
  begin
    Numerator := Numerator div GCD;
    Denominator := Denominator div GCD
  end;
end;

function TFraction.ToString: String;
begin
  if (Denominator < 0) then
  begin
    Numerator := - Numerator;
    Denominator := - Denominator;
  end;
  Result := IntToStr(Numerator) + FracSymbol + IntToStr(Denominator);
end;

function TFraction.Resolve: String;
var
  Num, IntPart: Int64;
begin
  Normalize;
  if (Abs(Numerator) > Abs(Denominator)) then
  begin
    IntPart := Numerator div Denominator;
    Num := Numerator mod Denominator;
    if (IntPart < 0) then Num := -Num;
    if (Num <> 0) then
      Result := IntToStr(IntPart) + #32 + IntToStr(Num) +  FracSymbol + IntToStr(Denominator)
    else
      Result := IntToStr(IntPart);
  end
  else
  begin
    Result := ToString;
  end;
end;

function TFraction.ToFloat: Double;
begin
  Result := Numerator / Denominator;
end;

end.

