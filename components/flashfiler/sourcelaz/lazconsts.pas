{taked from Delphi DBConsts.pas
used in lazcommon.pas}
unit lazconsts;

interface

resourcestring
  // ALL FROM DBConsts.pas
  SDataSetOpen = 'Cannot perform this operation on an open dataset';
  SInvalidIntegerValue = '''%s'' is not a valid integer value for field ''%s''';
  SInvalidFloatValue = '''%s'' is not a valid floating point value for field ''%s''';
  SFieldRangeError = '%g is not a valid value for field ''%s''. The allowed range is %g to %g';

  SExprTermination = 'Filter expression incorrectly terminated';
  SExprNameError = 'Unterminated field name';
  SExprStringError = 'Unterminated string constant';
  SExprInvalidChar = 'Invalid filter expression character: ''%s''';
  SExprNoLParen = '''('' expected but %s found';
  SExprNoRParen = ''')'' expected but %s found';
  SExprNoRParenOrComma = ''')'' or '','' expected but %s found';
  SExprExpected = 'Expression expected but %s found';
  SExprBadField = 'Field ''%s'' cannot be used in a filter expression';
  SExprBadNullTest = 'NULL only allowed with ''='' and ''<>''';
  SExprRangeError = 'Constant out of range';
  SExprNotBoolean = 'Field ''%s'' is not of type Boolean';
  SExprIncorrect = 'Incorrectly formed filter expression';
  SExprNothing = 'nothing';
  SExprTypeMis = 'Type mismatch in expression';
  SExprBadScope = 'Operation cannot mix aggregate value with record-varying value';
  SExprNoArith = 'Arithmetic in filter expressions not supported';
  SExprNotAgg = 'Expression is not an aggregate expression';
  SExprBadConst = 'Constant is not correct type %s';
  SExprNoAggFilter = 'Aggregate expressions not allowed in filters';
  SExprEmptyInList = 'IN predicate list may not be empty';
  SExprNoAggOnCalcs = 'Field ''%s'' is not the correct type of calculated field to be used in an aggregate, use an internalcalc';

  SInvalidKeywordUse = 'Invalid use of keyword';
  STextFalse = 'False';
  STextTrue = 'True';
  SBadFieldType = 'Field ''%s'' is of an unsupported type';
  SInvalidVersion = 'Unable to load bind parameters';
  SBcdOverflow = 'BCD overflow';
  // END OF ALL FROM DBConsts.pas
implementation

end.
