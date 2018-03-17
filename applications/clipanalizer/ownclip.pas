unit ownclip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef MSWINDOWS}
  , Windows;
  {$else}
  , LCLType, LCLIntf;
  {$endif}

  function ClipboardGetFormat(formatID: UINT; DestStream: TStream): boolean;

implementation

function ClipboardGetFormat(formatID: UINT; DestStream: TStream): boolean;
{$ifdef MSWINDOWS}
var
  DataHandle: HANDLE;
  ASize: PtrUInt;
  Data: LPVOID;
begin

  // borrowed from Lazarus clipboard support ..

  result := false;

  if (FormatID=0) or (DestStream=nil) or
    not Windows.IsClipboardFormatAvailable(FormatID) then exit;

  if Windows.OpenClipboard(Windows.HWND(nil)) then
    try

      DataHandle := Windows.GetClipboardData(FormatID);
      if DataHandle<>HWND(0) then
      begin
        ASize := Windows.GlobalSize(DataHandle);
        if ASize>0 then
        begin
          Data := Windows.GlobalLock(DataHandle);
          try
            DestStream.Write(Data^, ASize);
          finally
            Windows.GlobalUnlock(DataHandle);
          end;
          Result := true;
        end;
      end;
    finally
      Windows.CloseClipboard;
    end;
end;
{$else}
begin
  ClipboardGetData(ctPrimarySelection, formatID, DestStream);
end;

{$endif}

end.

