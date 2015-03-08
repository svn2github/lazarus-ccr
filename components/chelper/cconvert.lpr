{ C-to-Pas converter command-line utility part of Lazarus Chelper package

  Copyright (C) 2010 Dmitry Boyarintsev skalogryz dot lists at gmail.com

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
program cconvert;

{$mode delphi}{$H+}
{.$define leaks}

uses
  {$ifdef leaks}
  heaptrc,
  {$endif}
  SysUtils,Classes,
  ctopasconvert, cparsertypes, cparserutils, cconvconfig, objcparsing
  , cconvlog;

var
  ConfigFile    : AnsiString = '';
  OutputFile    : AnsiString = '';
  ConfigFileRO  : Boolean = false;
  ParseAll      : Boolean = true;
  ShowCodeSize  : Boolean = False; // show the size of code processed
  isPascalUnit  : Boolean = False; // convert to pascal unit
  isPrintHelp   : Boolean = False;
  isVerbose     : Boolean = false;
  DoIncludes    : Boolean = true;

  IncludePath   : TStringList = nil; // adjustement for include paths

procedure NormalizePath(var pth: string);
var
  i : integer;
begin
  for i:=1 to length(pth) do
    if pth[i] in ['/','\'] then
      pth[i]:=DirectorySeparator;
end;

function isIncludePath(const include: string; var pth: string): Boolean;
var
  i  : integer;
  p  : string;
  v  : string;
begin
  pth:=include;
  NormalizePath(pth);

  p:=ExtractFileDir(pth);
  if (p='') then p:='.'; // special case
  i:=IncludePath.IndexOfName(p);

  Result:=i>=0;
  if not Result then Exit;

  v:=IncludePath.ValueFromIndex[i];
  if p<>'' then p:=IncludeTrailingPathDelimiter(p);
  pth:=StringReplace(pth, p, v, [rfIgnoreCase]);
  //pth:=StringReplace(include, '\', DirectorySeparator, [rfReplaceAll]);
end;

function StringFromFile(const FileName: AnsiString): AnsiString;
var
  fs  : TFileStream;
begin
  Result:='';
  if not FileExists(FileName) then Exit;
  try
    fs:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, fs.Size);
      fs.Read(Result[1], fs.Size);
    finally
      fs.Free;
    end;
  except
  end;
end;

function SafeParamStr(i: integer): string;
begin
  if (i>=0) and (i<=ParamCount) then Result:=ParamStr(i)
  else Result:='';
end;

procedure PrintHelp;
begin
  writeln('cconvert 1.1 - c to pascal convert utility by Dmitry Boyarintsev');
  writeln('usage:');
  writeln(' cconvert [options] %header_filename%');
  writeln('possible options:');
  writeln(' -first            - stops on the first first entity');
  writeln(' -o %filename%     - specify the output file. if not specified, outputs to stdout');
  writeln(' -ro               - prevent the configuration file from modifications (adding new types, etc)');
  writeln(' -cfg %filename%   - specifies the configuration file');
  writeln(' -defines %filename%');
  writeln('                   - macros definition file. should be in C-preprocessor format');
  writeln(' -showunparsed     - writes out unprased entities by their classname (for debugging only)');
  writeln(' -codesize         - show two numbers of the code processed (used by Chelper)');
  writeln(' -pasunit          - generates a pascal unit');
  writeln(' -noinclude        - prevent processing of #include-ed or @import-ed files');
  writeln(' -ip %include_path%[:%phys_path%]');
  writeln('                   - specify paths to be included (imported) during the conversion');
  writeln('                     allowing mulitple files to be converted in one call.');
  writeln('                     physical path is used to specify the actual physical path on the hard drive.');
  writeln('                     if not specified, the current directory is assumed as a location for the header');
  writeln(' -verbose          - verbose output');
end;

procedure ConsumeIncludePath(const s: string);
var
  k : integer;
  ip : string;
  pp : string;
begin
  if s ='' then Exit;
  k:=Pos(':', s);
  if k=0 then begin
    pp:='';
    ip:=s;
  end else begin
    pp:=Copy(s, k+1, length(s));
    ip:=Copy(s, 1, k-1);
  end;
  if ip='' then ip:='.';
  if ip<>'' then begin
    NormalizePath(ip);
    IncludePath.Values[ip]:=pp;
  end;
end;

procedure ReadParams(files: TStrings; cfg: TConvertSettings);
var
  i : integer;
  s : string;
  ss : string;
  fn : AnsiString;
  k  : integer;
begin
  if ParamCount=0 then
    isPrintHelp:=true
  else begin
    i:=1;
    while i<= ParamCount do begin
      ss:=SafeParamStr(i);
      s:=LowerCase(ss);
      if (s='-h') or (s='-help') or (s='-?') then begin
        isPrintHelp:=true;
        Break;
      end else if s='-showunparsed' then begin
        DoDebugEntities:=True;
      end else if s='-cfg' then begin
        inc(i);
        fn:=Trim(SafeParamStr(i));
        ConfigFile:=fn;
        if FileExists(fn) then cconvconfig.LoadFromFile(fn, cfg);
      end else if s='-ro' then
        ConfigFileRO:=True
      else if s='-defines' then begin
        inc(i);
        cfg.CustomDefines:=cfg.CustomDefines+' ' + StringFromFile(SafeParamStr(i));
      end else if s='-o' then begin
        inc(i);
        OutputFile:=SafeParamStr(i);
      end else if s='-first' then begin
        ParseAll:=false
      end else if s='-pasunit' then begin
        isPascalUnit:=True;
      end else if s='-noinclude' then begin
        DoIncludes:=false;
      end else if s='-verbose' then begin
        // do not assign log function now, wait until all params are done
        isVerbose:=true;
      end else if s='-ip' then begin
        inc(i);
        ConsumeIncludePath( Trim(SafeParamStr(i)) );
      end else
        files.Add(ss);
      inc(i);
    end;
    //InputFileName:=SafeParamStr(ParamCount);
  end;
  if isVerbose then _log:=_stdOutLog;
end;


function GetPascalUnitName(const UnitName: String): String;
begin
  Result:=ChangeFileExt(UnitName, '');
end;

procedure AddPascalUnit(outs: TStrings; const UnitName: String);
begin
  if not Assigned(outs) then Exit;
  outs.Insert(0, 'unit '+UnitName+';');
  outs.Insert(1, '');
  outs.Insert(2, 'interface');
  outs.Add(      'implementation');
  outs.Add(      'end.');
end;

function GetIncludeFN(const fn: string): string;
var
 i : integer;
begin
  i:=length(fn);
  while (i>0) and not (fn[i] in ['/','\']) do dec(i);
  Result:=Copy(fn, i+1, length(fn));
end;

function SortByUsage(p1, p2: Pointer): integer;
var
  f1, f2: THeaderFile;
begin
  f1:=THeaderFile(p1);
  f2:=THeaderFile(p2);
  if (f1.usedBy=f2.usedBy) then begin
    if f1.inclOrder=f2.inclOrder then Result:=0
    else if f1.inclOrder<f2.inclOrder then Result:=-1
    else Result:=1;
  end else if (f1.usedBy<>0) then begin
    if f1.usedBy<=f2.inclOrder then Result:=-1
    else Result:=1;
  end else if (f2.usedBy<>0) then begin
    if f2.usedBy<=f1.inclOrder then Result:=1
    else Result:=-1;
  end;
end;

procedure ResortByUsage(files: TStrings);
var
  fl : TList;
  i  : integer;
begin
  fl:=TList.Create;
  try
    for i:=0 to files.Count-1 do
      fl.Add(files.Objects[i]);
    fl.Sort(SortByUsage);
    files.Clear;
    for i:=0 to fl.Count-1 do
      files.AddObject( THeaderFile(fl[i]).fn, fl[i] );
  finally
    fl.Free;
  end;
end;
                   (*
procedure NewerMode(files: TStrings; cfg: TConvertSettings);
var
  inp   : TParseInput;
  ot    : TParseOutput;
  txt   : TSTringList;
  res   : string;
  fn    : string;
  i     : integer;
  j     : integer;
  fi    : integer;
  ic    : TCPrepInclude;
  hdr   : THeaderFile;
  hh    : THeaderFile;

begin
  InitCParserInput(inp, true);
  try
    LoadDefines(inp, cfg.CustomDefines);

    i:=0;
    while i<files.Count do begin
      fn:=files[i];

      hdr:=THeaderFile(files.Objects[i]);
      if not Assigned(hdr) then begin
        hdr:=THeaderFile.Create;
        hdr.fn:=ExtractFileName(fn);
        files.Objects[i]:=hdr;
      end;
      hdr.inclOrder:=i;

      txt:=TStringList.Create;
      try
        txt.LoadFromFile(fn);
        ResetText(inp, txt.Text);
      finally
        txt.Free;
      end;

      //writeln('parsing entities');
      if not ParseCEntities(inp, hdr.ents, ot) then begin
        //writeln('error:');
        writeln('Parsing error at ', fn,' (',ot.error.ErrorPos.y,':',ot.error.ErrorPos.X,')');
        writeln(ot.error.ErrorMsg);

        ReleaseList(hdr.Ents);
        inc(i);

        Continue;
      end;
      hdr.text:=inp.parser.Buf;

      // assing internal comments
      //DebugEnList(ents);
      AssignIntComments(hdr.ents);
      //DebugEnList(ents);

      //writeln('c to pas');
      //res:=CEntitiesToPas(inp.parser.Buf, hdr.ents, cfg);
      //writeln('done!');
      //writeln(res);

      if DoIncludes then
        for j:=0 to hdr.ents.Count-1 do
          if TObject(hdr.ents[j]) is TCPrepInclude then begin
            ic:=TCPrepInclude(hdr.ents[j]);
            if isIncludePath(ic.Included, fn) then begin
              //fn:='C:\fpc_laz\chelper\uikit\Headers\'+GetIncludeFN(ic.Included);
              fi:=Files.IndexOf(fn);
              if fi<0 then
                // GetIncludeFN(ic.Included) is a hack not to add UIKit.h twice
                fi:=Files.IndexOf( GetIncludeFN(ic.Included) );
              if fi<0 then begin
                log('adding: ', fn);
                hh:=THeaderFile.Create;
                hh.fn:=ExtractFileName(fn);
                hh.usedBy:=hdr.inclOrder;
                fi:=Files.AddObject(fn, hh);
              end else begin
                hh:=THeaderFile(Files.Objects[fi]);
                // fi<>0 is a hack not to add reassing UIKit.h twice
                if (hh.usedBy=0) and (fi<>0) then hh.usedBy:=i;
              end;
              inc(THeaderFile(Files.Objects[fi]).useCount);

            end;
          end;

      inc(i);
    end;

    if isVerbose then begin
      log('files count = ', files.Count);
      log('original order');
      DebugHeaders(files);
    end;

    log('files order after usage resolving');
    ResortByUsage(files);
    if isVerbose then DebugHeaders(files);

    for i:=0 to files.Count-1 do begin
      hdr:=THeaderFile(files.Objects[i]);
      log('// '+files[i]+' ', hdr.ents.Count);
      res:=CEntitiesToPas(hdr.text, hdr.ents, cfg);

      writeln(res);
    end;

    {writeln('alphabet!');
    TSTringList(files).Sort;
    DebugHeaders(files);}


  finally
    FreeCParserInput(inp);
  end;
end;

procedure OldMode(fns: TStrings; cfg: TConvertSettings);
var
  inps, outs: TStringList;
  err : TErrorInfo;
  p   : TPoint;
  i   : Integer;
begin
  inps := TStringList.Create;
  outs := TStringList.Create;

  try
    inps.LoadFromFile(ParamStr(ParamCount));

    outs.Text:=ConvertCode(inps.Text, p, ParseAll, err, cfg);;

    if ShowCodeSize then outs.Insert(0, Format('%d %d', [p.Y,p.X]));
    if err.isError then outs.Insert(0, Format('error %d %d %s',[err.ErrorPos.Y, err.ErrorPos. X, err.ErrorMsg]) );

    if isPascalUnit then begin
      AddPascalUnit(outs, GetPascalUnitName(fns[0]));
    end;


    if OutputFile<>'' then
      outs.SaveToFile(OutputFile)
    else
      for i:=0 to outs.Count-1 do
        writeln(outs[i]);
  finally
    if not ConfigFileRO and (ConfigFile<>'') then begin
      ForceDirectories(ExtractFilePath(ConfigFile));
      try
        cconvconfig.SaveToFile(ConfigFile, cfg);
      except
      end;
    end;
    inps.Free;
    outs.Free;
  end;
end;
*)
procedure SaveDebugToFile(const buf, filename: string; const comment: string = '');
var
  fn : string;
  st : text;
begin
  fn:='__'+ExtractFileName(filename);
  Assign(st, fn); Rewrite(st);
  if comment='' then
    write(st, buf)
  else begin
    writeln(st,buf);
    writeln(st,'-----');
    write(st,comment);
  end;
  CloseFile(st);
end;

procedure NewestMode(files: TStrings; cfg: TConvertSettings);
var
  inp   : TParseInput;
  ot    : TParseOutput;
  txt   : TStringList;
  buf   : string;
  res   : string;
  fn    : string;

  i     : integer;
  j     : integer;
  fi    : integer;
  ic    : TCPrepInclude;
  hdr   : THeaderFile;
  hh    : THeaderFile;
  applied : TList;

begin
  applied := TList.Create;
  InitCParserInput(inp, true);
  try
    LoadDefines(inp, cfg.CustomDefines);

    i:=0;

    while i<files.Count do begin
      fn:=files[i];
      //writeln('i = ',i,' ',fn);

      hdr:=THeaderFile(files.Objects[i]);
      if not Assigned(hdr) then begin
        hdr:=THeaderFile.Create;
        hdr.fn:=ExtractFileName(fn);
        files.Objects[i]:=hdr;
      end;
      hdr.inclOrder:=i;

      txt:=TStringList.Create;
      try
        txt.LoadFromFile(fn);
        buf:=txt.Text;
      finally
        txt.Free;
      end;
      buf:=PreprocGlobal(buf, hdr.fileOfs, hdr.cmts);
      //writeln('?parseri!');
      ParseDirectives(buf, hdr.pres);

      applied.Clear;
      //writeln('?preproc!');
      buf:=PreprocessHeader(buf, hdr.pres, inp.parser.MacroHandler, hdr.fileOfs, applied);

      ResetText(inp, buf);

      //writeln('?handler = ', PtrUInt(inp.parser.MacroHandler));
      //DebugMacros(inp.parser.MacroHandler);
      //writeln('parsing entities');

      if not ParseCEntities(inp, hdr.ents, ot) then begin
        //writeln('error:');
        writeln('Parsing error at ', fn,' (',ot.error.ErrorPos.y,':',ot.error.ErrorPos.X,')');
        writeln(ot.error.ErrorMsg);

        ReleaseList(hdr.Ents);
        inc(i);

        SaveDebugToFile(inp.parser.Buf
          +LineEnding+'----'+LineEnding+
          buf
          , fn,
           Format('Parsing error at "%s" (%d:%d)'#13#10'%s',
                 [fn, ot.error.ErrorPos.y, ot.error.ErrorPos.X,ot.error.ErrorMsg]));

        Continue;
      end;
      hdr.text:=inp.parser.Buf;

      AssignIntComments(hdr.ents);

      if DoIncludes then
        //writeln('kokoke: applied');
        for j:=0 to applied.Count-1 do
          if TObject(applied[j]) is TCPrepInclude then begin
            ic:=TCPrepInclude(applied[j]);
            if isIncludePath(ic.Included, fn) then begin
              fi:=Files.IndexOf(fn);
              if fi<0 then
                // GetIncludeFN(ic.Included) is a hack not to add UIKit.h twice
                fi:=Files.IndexOf( GetIncludeFN(ic.Included) );
              if fi<0 then begin
                log('adding: ', fn);
                hh:=THeaderFile.Create;
                hh.fn:=ExtractFileName(fn);
                hh.usedBy:=hdr.inclOrder;
                fi:=Files.AddObject(fn, hh);
              end else begin
                hh:=THeaderFile(Files.Objects[fi]);
                // fi<>0 is a hack not to add reassing UIKit.h twice
                if (hh.usedBy=0) and (fi<>0) then hh.usedBy:=i;
              end;
              inc(THeaderFile(Files.Objects[fi]).useCount);

            end;
          end;

      inc(i);
    end;
    //writeln('done!');

    if isVerbose then begin
      log('files count = ', files.Count);
      log('original order');
      DebugHeaders(files);
    end;

    log('files order after usage resolving');
    ResortByUsage(files);
    if isVerbose then DebugHeaders(files);

    //writeln('cout = ', files.Count);
    for i:=0 to files.Count-1 do begin
      hdr:=THeaderFile(files.Objects[i]);
      log('// '+files[i]+' ', hdr.ents.Count);
      res:=CEntitiesToPas(hdr.text, hdr.ents, cfg);

      writeln(res);
    end;

    {writeln('alphabet!');
    TSTringList(files).Sort;
    DebugHeaders(files);}


  finally
    FreeCParserInput(inp);
    applied.Free;
  end;
end;

var
  cfg : TConvertSettings;
  fns : TStringList;
begin
  {$ifdef leaks}
  DeleteFile('leaks.txt');
  SetHeapTraceOutput('leaks.txt');
  {$endif}

  cfg:=TConvertSettings.Create;
  fns:=TStringList.Create;
  IncludePath:=TStringList.Create;
  try
    ReadParams(fns, cfg);
    if isPrintHelp then begin
      PrintHelp;
      Exit;
    end;

    if fns.Count=0 then begin
      writeln('no input header files were specified');
      Exit;
    end;

    NewestMode(fns, cfg);

  finally
    cfg.Free;
    fns.Free;
    IncludePath.Free;
  end;
end.

