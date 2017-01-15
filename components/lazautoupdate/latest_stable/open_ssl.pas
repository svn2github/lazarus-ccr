unit open_ssl;

{$mode objfpc}{$H+}

interface

uses
  // Built-in: fphttpclient
  Classes, SysUtils,lazautoupdate_httpclient,LazFileUtils,FileUtil,zipper;

function CheckForOpenSSL:Boolean;
function OpenSSLInstalled:Boolean;

implementation
const
{$ifdef win64}
 cOpenSSLURL = 'http://packages.lazarus-ide.org/openssl-1.0.2j-x64_86-win64.zip';
{$endif}
{$ifdef win32}
cOpenSSLURL = 'http://packages.lazarus-ide.org/openssl-1.0.2j-i386-win32.zip';
{$endif}
Var FHTTPClient:TFPHttpClient;

function OpenSSLInstalled:Boolean;
begin
  {$IFDEF MSWINDOWS}
   Result:= FileExistsUTF8(ExtractFilePath(ParamStr(0)) + 'libeay32.dll') and
   FileExistsUTF8(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll');
  // Look in Windows system dir?
  {$ELSE}
  Result:=True;
  {$ENDIF}
end;

Function CheckForOpenSSL:Boolean;
var
  ZipFile: String;
  UnZipper: TUnZipper;
begin
  {$IFDEF MSWINDOWS}
  Result:=FALSE;
   if not OpenSSLInstalled then
   begin
     ZipFile := ExtractFilePath(ParamStr(0)) + ExtractFileName(cOpenSSLURL);
     try
       FHTTPClient.Get(cOpenSSLURL, ZipFile);
     except
     end;

     if FileExistsUTF8(ZipFile) then
     begin
       UnZipper := TUnZipper.Create;
       try
         try
           UnZipper.FileName := ZipFile;
           UnZipper.Examine;
           UnZipper.UnZipAllFiles;
         except
         end;
       finally
         UnZipper.Free;
       end;
       DeleteFileUTF8(ZipFile);
       Result:=OpenSSLInstalled;
     end;
  end
  else
  Result:=True;
  {$ELSE}
  Result:=True;
  {$ENDIF}
end;
initialization
begin
     FHTTPClient:=TFPHttpClient.Create(nil);
end;
finalization
begin
     FreeAndNil(FHTTPClient);
end;
end.

