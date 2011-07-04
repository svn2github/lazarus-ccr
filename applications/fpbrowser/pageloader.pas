unit pageloader;

{$mode delphi}

interface

uses
  Classes, SysUtils; 

type

  { TPageLoader }

  TPageLoader = class
  public
    Contents: string;
    LastPageURL: string;
    ContentsList: TStringList;
    constructor Create;
    procedure LoadFromURL(AURL: string);
    procedure LoadBinaryResource(AURL: string; var ADest: TMemoryStream);
  end;

var
  MyPageLoader: TPageLoader;

implementation

uses httpsend;

{ TPageLoader }

constructor TPageLoader.Create;
begin
  ContentsList := TStringList.Create;
end;

procedure TPageLoader.LoadFromURL(AURL: string);
var
  Client: THttpSend;
  J: Integer;
begin
  // If there is no protocol, add http
  J := Pos(':', AURL);
  if (J = 0) then LastPageURL := 'http://' + AURL
  else LastPageURL := AURL;

  Client := THttpSend.Create;
  try
//  if checkGZip.Checked then
//  begin
    Client.Headers.Add('Accept:	text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
    Client.Headers.Add('Accept-Language:	en-gb,en;q=0.5');
//    Client.Headers.Add('Accept-Encoding:	gzip,deflate');
    Client.Headers.Add('Accept-Charset:	utf-8;q=0.7,*;q=0.7'); // ISO-8859-1,
//  end;

//  Client.UserAgent := AUserAgent;
    Client.HttpMethod('GET', LastPageURL);
    Client.Document.Position := 0;

    ContentsList.LoadFromStream(Client.Document);

    Contents := ContentsList.Text;
  finally
    Client.Free;
  end;
end;

procedure TPageLoader.LoadBinaryResource(AURL: string; var ADest: TMemoryStream);
var
  Client: THttpSend;
  i: Integer;
begin
  Client := THttpSend.Create;
  try
    Client.Headers.Add('Accept:	image/png, image/jpeg, image/gif');
    Client.Headers.Add('Accept-Language:	en-gb,en;q=0.5');
//    Client.Headers.Add('Accept-Encoding:	gzip,deflate');
    Client.Headers.Add('Accept-Charset:	utf-8;q=0.7,*;q=0.7'); // ISO-8859-1,

//  Client.UserAgent := AUserAgent;
    Client.HttpMethod('GET', AURL);

    Client.Document.Position := 0;
    ADest := TMemoryStream.Create;
    ADest.CopyFrom(Client.Document, Client.Document.Size);
//    WriteLn(AURL, ' Size: ', ADest.Size);
  finally
    Client.Free;
  end;
end;

initialization

  MyPageLoader := TPageLoader.Create;

finalization

  MyPageLoader.Free;

end.

