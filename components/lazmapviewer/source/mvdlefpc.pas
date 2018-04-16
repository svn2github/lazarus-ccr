{ Map Viewer Download Engine Free Pascal HTTP Client

  Copyright (C) 2011 Maciej Kaczkowski / keit.co

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  Taken from:
  https://forum.lazarus.freepascal.org/index.php/topic,12674.msg160255.html#msg160255

}

unit mvDLEFpc;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  mvDownloadEngine;

type

  { TMVDEFPC }

  TMVDEFPC = class(TMvCustomDownloadEngine)
  protected
    procedure DownloadFile(const Url: string; str: TStream); override;
  {$IF FPC_FullVersion >= 30101}
  published
    property UseProxy;
    property ProxyHost;
    property ProxyPort;
    property ProxyUsername;
    property ProxyPassword;
  {$ENDIF}
  end;


implementation

uses
  fphttpclient;

{ TMVDEFPC }

procedure TMVDEFPC.DownloadFile(const Url: string; str: TStream);
var
  http: TFpHttpClient;
begin
  inherited;
  http := TFpHttpClient.Create(nil);
  try
    http.AllowRedirect := true;
    http.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
   {$IF FPC_FullVersion >= 30101}
    if UseProxy then begin
      http.Proxy.Host := ProxyHost;
      http.Proxy.Port := ProxyPort;
      http.Proxy.UserName := ProxyUserName;
      http.Proxy.Password := ProxyPassword;
    end;
   {$ENDIF}
    http.Get(Url, str);
    str.Position := 0;
  finally
    http.Free;
  end;
end;

end.
