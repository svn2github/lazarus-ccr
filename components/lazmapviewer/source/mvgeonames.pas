{ Map Viewer Geolocation Engine for geonames.org

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
}
unit mvGeoNames;

interface

uses
  SysUtils, Classes, StrUtils,
  mvTypes, mvDownloadEngine;

type
  TNameFoundEvent = procedure (const AName: string; const ADescr: String;
    const ALoc: TRealPoint) of object;

  TStringArray = array of string;

  { TMVGeoNames }

  TMVGeoNames = class(TComponent)
  private
    FLocationName: string;
    FOnNameFound: TNameFoundEvent;
    function RemoveTag(const str: String): TStringArray;
  public
    function Search(ALocationName: String;
      ADownloadEngine: TMvCustomDownloadEngine): TRealPoint;
  published
    property LocationName: string read FLocationName;
    property OnNameFound : TNameFoundEvent read FOnNameFound write FOnNameFound;
  end;

procedure Register;


implementation

function CleanLocationName(x: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(x) do
  begin
    if x[i] in ['A'..'Z', 'a'..'z', '0'..'9'] then
      Result := Result + x[i]
    else
      Result := Result + '+'
  end;
end;

{ TMVGeoNames }

Type
  TResRec = record
       Name : String;
       Descr : String;
       Loc : TRealPoint;
  End;

procedure Register;
begin
    RegisterComponents('Maps',[TMVGeoNames]);
end;

function TMVGeoNames.RemoveTag(Const str : String) : TStringArray;
var iStart,iEnd,i : Integer;
    tmp : String;
    lst : TStringList;
Begin
  SetLength(Result,0);
  tmp:=StringReplace(str,'<br>',#13,[rfReplaceall]);
  tmp:=StringReplace(tmp,'&nbsp;',' ',[rfReplaceall]);
  tmp:=StringReplace(tmp,'  ',' ',[rfReplaceall]);
  repeat
    iEnd:=-1;
    iStart:=pos('<',tmp);
    if iStart>0 then
    Begin
      iEnd:=posEx('>',tmp,iStart);
      if iEnd>0 then
      Begin
        tmp:=copy(tmp,1,iStart-1)+copy(tmp,iEnd+1,length(tmp));
      end;
    end;
  until iEnd<=0;
  lst:=TStringList.Create;
  try
    lst.Text:=tmp;
    SetLEngth(Result,lst.Count);
    For i:=0 to pred(lst.Count) do
      Result[i]:=trim(lst[i]);
  finally
    freeAndNil(lst);
  end;

end;

function TMVGeoNames.Search(ALocationName: String;
  ADownloadEngine: TMvCustomDownloadEngine): TRealPoint;
const
  LAT_ID = '<span class="latitude">';
  LONG_ID = '<span class="longitude">';
var
  s: string;

  function gs(id: string;Start : integer): string;
  var
    i: Integer;
    ln: Integer;
  begin
    Result := '';
    ln := Length(s);
    i := PosEx(id, s,start) + Length(id);
    while (s[i] <> '<') and (i < ln) do
    begin
      if s[i] = '.' then
        Result := Result + FormatSettings.DecimalSeparator
      else
        Result := Result + s[i];
      Inc(i);
    end;
  end;

var
  m: TMemoryStream;
  iRes,i : integer;
  lstRes : Array  of TResRec;
  iStartDescr : integer;
  lst : TStringArray;
begin
  FLocationName := ALocationName;
  m := TMemoryStream.Create;
  try
    ADownloadEngine.DownloadFile('http://www.geonames.org/search.html?q='+
      CleanLocationName(FLocationName), m);
    m.Position := 0;
    SetLength(s, m.Size);
    m.Read(s[1], m.Size);
  finally
    m.Free;
  end;

  Result.Lon := 0;
  Result.Lat := 0;
  SetLength(lstRes, 0);
  iRes := Pos('<span class="geo"',s);
  while (iRes>0) do
  begin
    SetLength(lstRes,length(lstRes)+1);
    lstRes[high(lstRes)].Loc.Lon := StrToFloat(gs(LONG_ID,iRes));
    lstRes[high(lstRes)].Loc.Lat := StrToFloat(gs(LAT_ID,iRes));
    iStartDescr := RPosex('<td>',s,iRes);
    if iStartDescr>0 then
    begin
      lst:=RemoveTag(Copy(s,iStartDescr,iRes-iStartDescr));
      if length(lst)>0 then
        lstRes[high(lstRes)].Name:=lst[0];
      lstRes[high(lstRes)].Descr:='';
      for i:=1 to high(lst) do
        lstRes[high(lstRes)].Descr+=lst[i];
    end;

    Result.Lon += lstRes[high(lstRes)].Loc.Lon;
    Result.Lat += lstRes[high(lstRes)].Loc.Lat;
    iRes := PosEx('<span class="geo"',s,iRes+17);
  end;

  if length(lstRes)>0 then
  begin
    if length(lstRes)>1 then
    begin
      Result.Lon := Result.Lon/length(lstRes);
      Result.Lat := Result.Lat/length(lstRes);
    end;
    if Assigned(FOnNameFound) then
      for iRes:=low(lstRes) to high(lstRes) do
      begin
        FOnNameFound(lstRes[iRes].Name,lstRes[iRes].Descr,lstRes[iRes].Loc);
      end;
  end;
end;

end.
