program example;

{$mode objfpc}{$H+}
{$apptype console}

uses
  Classes, sysutils, fractions, math;

var
  F1, F2: TFraction;
  D, Prec: Double;
  i: Integer;
begin
  F1 := Fraction(1,1,3); // 1 1/3
  F2 := Fraction(4,3);   // 4/3
  writeln('F1.ToString = ',F1.ToString); // '4/3'
  writeln('F1.Resolve  = ',F1.Resolve);  // '1 1/3'
  writeln('F1.ToFloat  = ',F1.ToFloat:16:16); // 1.3333333333333333
  writeln('F2.ToString = ',F2.ToString); // '4/3'
  writeln('(F1 = F2)   = ',F1=F2);         //True

  F1 := Fraction(1,2);
  F2 := Fraction(1,3);
  writeln(F1.ToString,' * ',F2.ToString,'   = ',(F1*F2).Resolve);  // '1/6'
  writeln(F1.ToString,' / ',F2.ToString,'   = ',(F1/F2).Resolve);  // '1 1/2'
  writeln(F1.ToString,' + ',F2.ToString,'   = ',(F1+F2).Resolve);  // '5/6'
  writeln(F1.ToString,' - ',F2.ToString,'   = ',(F1-F2).Resolve);  // '1/6'
  writeln(F1.ToString,' ** 2    = ',(F1**2).Resolve);  // '1/6'

  D := 0.25;
  F1 := FloatToFraction(D, 0.000001);
  writeln('FloatTofraction(0.25) -> ',F1.ToString);   // '1/4'
  writeln;
  writeln('Approximations of Pi:');
  writeln('                                              [Pi = ',Pi:16:16,']');
  Prec := 1.0;
  for i := 1 to 10 do
  begin
    Prec := Prec / 10;
    F2 := FloatTofraction(Pi, Prec);
    writeln('FloatTofraction(Pi,',Prec:10:10,') = ',Format('%-13s',[F2.Resolve]),'   [',F2.ToFloat:16:16,']');
  end;
end.

