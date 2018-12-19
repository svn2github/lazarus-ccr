{ rxapputils unit

  Copyright (C) 2005-2018 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

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
unit rxlogging;

{$I rx.inc}

interface

uses
  Classes, SysUtils;

type
  TRxLoggerEvent = procedure( ALogType:TEventType; const ALogMessage:string);

var
  OnRxLoggerEvent:TRxLoggerEvent = nil;

procedure RxWriteLog(ALogType:TEventType; const ALogMessage:string);
procedure RxWriteLog(ALogType:TEventType; const ALogMessage:string; Params:array of const);
procedure RxDefaultWriteLog( ALogType:TEventType; const ALogMessage:string);
function RxDefaultLogFileName:string;
procedure InitRxLogs;

implementation
uses LazFileUtils;

procedure RxWriteLog(ALogType:TEventType; const ALogMessage:string);
begin
  if Assigned(OnRxLoggerEvent) then
    OnRxLoggerEvent(ALogType, ALogMessage)
end;

procedure RxWriteLog(ALogType: TEventType; const ALogMessage: string;
  Params: array of const);
begin
  RxWriteLog(ALogType, Format(ALogMessage, Params));
end;

procedure RxDefaultWriteLog(ALogType: TEventType; const ALogMessage: string);
var
  F: Text;
  S: String;
const
  sEventNames : array [TEventType] of string =
    ('CUSTOM','INFO','WARNING','ERROR','DEBUG');
begin
  S:=RxDefaultLogFileName;
  if S<>'' then
  begin
    Assign(F, S);
    if FileExists(S) then
      Append(F)
    else
      Rewrite(F);

    Writeln(F,Format( '|%s| %20s |%s', [sEventNames[ALogType], DateTimeToStr(Now), ALogMessage]));
    CloseFile(F);
  end;
end;

function RxDefaultLogFileName: string;
begin
  Result:=AppendPathDelim(GetTempDir) + ExtractFileNameOnly(ParamStr(0)) + '.log';
end;

procedure InitRxLogs;
begin
  OnRxLoggerEvent:=@RxDefaultWriteLog;
end;
end.

