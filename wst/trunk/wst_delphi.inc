{$IFDEF DELPHI}
  type
  {$IFDEF CompilerVersion}
    {$IF ( CompilerVersion > 16.0 )}
      QWord = UInt64;
    {$IFEND}
  {$ENDIF CompilerVersion}
  {$IF Not Declared(QWord) }
    QWord = type Int64;
  {$IFEND}
    DWORD = LongWord;
    PByteArray = ^ByteArray;
    ByteArray = array[0..$effffff] of Byte;
    {$IF Declared(NativeUInt)}
      PtrInt  = NativeInt;
      PtrUInt = NativeUInt;
    {$ELSE} // old delphi
      PtrInt = Integer;
      PtrUInt = Cardinal;
    {$IFEND}
    SizeInt = PtrInt;
    UnicodeChar = WideChar;
{$ENDIF DELPHI}
