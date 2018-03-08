{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterRC.pas, released 2004-06-12.
The initial author of this file is Yiannis Mandravellos.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterRC.pas,v 1.9 2005/12/31 07:34:36 skyweb Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERRC}
unit SynHighlighterRC;
{$ENDIF}

{$I synedit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics, 
  QSynEditTypes, 
  QSynEditHighlighter, 
{$ELSE}
  Controls,
  Graphics, 
  SynEditTypes, 
  SynEditHighlighter, 
{$ENDIF}
  SysUtils,
  Classes;

type
 TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
                 tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

 TRangeState = (rsUnknown, rsDirective, rsComment);

 TProcTableProc = procedure of object;

 PIdentFuncTableFunc = ^TIdentFuncTableFunc;
 TIdentFuncTableFunc = function: TtkTokenKind of object;

 TSynRCSyn = class(TSynCustomHighlighter)
  private
   fRange: TRangeState;
   fLine: PChar;
   fProcTable: array[#0..#255] of TProcTableProc;
   Run: LongInt;
   fStringLen: integer;
   fToIdent: PChar;
   fTokenPos: integer;
   fTokenID: TtkTokenKind;
   fLineNumber: integer;
   fIdentFuncTable: array[0..199] of TIdentFuncTableFunc;
   fCommentAttri: TSynHighlighterAttributes;
   fDirecAttri: TSynHighlighterAttributes;
   fIdentifierAttri: TSynHighlighterAttributes;
   fKeyAttri: TSynHighlighterAttributes;
   fNumberAttri: TSynHighlighterAttributes;
   fSpaceAttri: TSynHighlighterAttributes;
   fStringAttri: TSynHighlighterAttributes;
   fSymbolAttri: TSynHighlighterAttributes;
   function KeyHash(ToHash: PChar): integer;
   function KeyComp(const aKey: string): boolean;

   // number id functions
   function Func33: TtkTokenKind;
   function Func39: TtkTokenKind;
   function Func41: TtkTokenKind;
   function Func43: TtkTokenKind;
   function Func47: TtkTokenKind;
   function Func48: TtkTokenKind;
   function Func53: TtkTokenKind;
   function Func54: TtkTokenKind;
   function Func55: TtkTokenKind;
   function Func60: TtkTokenKind;
   function Func61: TtkTokenKind;
   function Func62: TtkTokenKind;
   function Func66: TtkTokenKind;
   function Func68: TtkTokenKind;
   function Func71: TtkTokenKind;
   function Func72: TtkTokenKind;
   function Func75: TtkTokenKind;
   function Func77: TtkTokenKind;
   function Func78: TtkTokenKind;
   function Func81: TtkTokenKind;
   function Func82: TtkTokenKind;
   function Func83: TtkTokenKind;
   function Func84: TtkTokenKind;
   function Func87: TtkTokenKind;
   function Func89: TtkTokenKind;
   function Func90: TtkTokenKind;
   function Func94: TtkTokenKind;
   function Func97: TtkTokenKind;
   function Func98: TtkTokenKind;
   function Func100: TtkTokenKind;
   function Func101: TtkTokenKind;
   function Func102: TtkTokenKind;
   function Func105: TtkTokenKind;
   function Func107: TtkTokenKind;
   function Func109: TtkTokenKind;
   function Func110: TtkTokenKind;
   function Func111: TtkTokenKind;
   function Func113: TtkTokenKind;
   function Func117: TtkTokenKind;
   function Func118: TtkTokenKind;
   function Func120: TtkTokenKind;
   function Func121: TtkTokenKind;
   function Func125: TtkTokenKind;
   function Func126: TtkTokenKind;
   function Func127: TtkTokenKind;
   function Func128: TtkTokenKind;
   function Func130: TtkTokenKind;
   function Func134: TtkTokenKind;
   function Func139: TtkTokenKind;
   function Func140: TtkTokenKind;
   function Func146: TtkTokenKind;
   function Func147: TtkTokenKind;
   function Func150: TtkTokenKind;
   function Func156: TtkTokenKind;
   function Func158: TtkTokenKind;
   function Func164: TtkTokenKind;
   function Func171: TtkTokenKind;
   function Func196: TtkTokenKind;
   function Func199: TtkTokenKind;

   // symbol functions
   procedure CommentProc;
   procedure CRProc;
   procedure DirectiveProc;
   procedure IdentProc;
   procedure LFProc;
   procedure NullProc;
   procedure NumberProc;
   procedure QuoteProc;
   procedure SlashProc;
   procedure SpaceProc;
   procedure SymbolProc;
   procedure UnknownProc;

   function AltFunc: TtkTokenKind;
   procedure InitIdent;
   function IdentKind(Maybe: PChar): TtkTokenKind;
   procedure MakeMethodTables;
  protected
   function GetIdentChars: TSynIdentChars; override;
   function IsFilterStored: Boolean; override;
  public
   class function GetCapabilities: TSynHighlighterCapabilities; override;
   class function GetLanguageName: string; override;
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   function GetDefaultAttribute(index: integer): TSynHighlighterAttributes; override;
   function GetEol: boolean; override;
   function GetRange: pointer; override;
   function GetTokenID: TtkTokenKind;
   {$IFDEF SYN_LAZARUS}
   procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
   {$ENDIF}
   procedure SetLine(const NewValue: string; LineNumber: integer); override;
   function GetToken: string; override;
   function GetTokenAttribute: TSynHighlighterAttributes; override;
   function GetTokenKind: integer; override;
   function GetTokenPos: integer; override;
   procedure Next; override;
   procedure SetRange(value: pointer); override;
   procedure ResetRange; override;
   function UseUserSettings(SettingIndex: integer): boolean; override;
   procedure EnumUserSettings(Settings: TStrings); override;
   function GetSampleSource: string; override;
  published
   property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
   property DirecAttri: TSynHighlighterAttributes read fDirecAttri write fDirecAttri;
   property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
   property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
   property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
   property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
   property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
   property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
 end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst, SynEditStrConstExtra;
{$ENDIF}

const
 MAXNumFunc = 199; // ** don't forget to change

var
 Identifiers: array[#0..#255] of bytebool;
 mHashTable: array[#0..#255] of integer;

procedure MakeIdentTable;
var
 i, j: char;
begin
  for i:= #0 to #255 do
   begin
     case i of
      '_', '0'..'9', 'a'..'z','A','Z': identifiers[i]:= TRUE;
     else
      identifiers[i]:= FALSE;
     end;
     j:= UpCase(i);
     case i of
      '_', 'a'..'z', 'A'..'Z': mHashTable[i]:= ord(j) -64;
     else
      mHashTable[i]:= 0;
     end;
   end;
end;

{ TSynRCSyn }

procedure TSynRCSyn.InitIdent;
var
 i: integer;
 pF: PIdentFuncTableFunc;
begin
  pF:= PIdentFuncTableFunc(@fIdentFuncTable);
  for i:= low(fIdentFuncTable) to high(fIdentFuncTable) do
   begin
     pF^:= {$ifdef FPC} @ {$endif}AltFunc;
     inc(pF);
   end;
  // number func set
  fIdentFuncTable[33] :=  {$IFDEF FPC}@{$ENDIF}  Func33;
  fIdentFuncTable[39] :=  {$IFDEF FPC}@{$ENDIF}  Func39;
  fIdentFuncTable[41] :=  {$IFDEF FPC}@{$ENDIF}  Func41;
  fIdentFuncTable[43] :=  {$IFDEF FPC}@{$ENDIF}  Func43;
  fIdentFuncTable[47] :=  {$IFDEF FPC}@{$ENDIF}  Func47;
  fIdentFuncTable[48] :=  {$IFDEF FPC}@{$ENDIF}  Func48;
  fIdentFuncTable[53] :=  {$IFDEF FPC}@{$ENDIF}  Func53;
  fIdentFuncTable[54] :=  {$IFDEF FPC}@{$ENDIF}  Func54;
  fIdentFuncTable[55] :=  {$IFDEF FPC}@{$ENDIF}  Func55;
  fIdentFuncTable[60] :=  {$IFDEF FPC}@{$ENDIF}  Func60;
  fIdentFuncTable[61] :=  {$IFDEF FPC}@{$ENDIF}  Func61;
  fIdentFuncTable[62] :=  {$IFDEF FPC}@{$ENDIF}  Func62;
  fIdentFuncTable[66] :=  {$IFDEF FPC}@{$ENDIF}  Func66;
  fIdentFuncTable[68] :=  {$IFDEF FPC}@{$ENDIF}  Func68;
  fIdentFuncTable[71] :=  {$IFDEF FPC}@{$ENDIF}  Func71;
  fIdentFuncTable[72] :=  {$IFDEF FPC}@{$ENDIF}  Func72;
  fIdentFuncTable[75] :=  {$IFDEF FPC}@{$ENDIF}  Func75;
  fIdentFuncTable[77] :=  {$IFDEF FPC}@{$ENDIF}  Func77;
  fIdentFuncTable[78] :=  {$IFDEF FPC}@{$ENDIF}  Func78;
  fIdentFuncTable[81] :=  {$IFDEF FPC}@{$ENDIF}  Func81;
  fIdentFuncTable[82] :=  {$IFDEF FPC}@{$ENDIF}  Func82;
  fIdentFuncTable[83] :=  {$IFDEF FPC}@{$ENDIF}  Func83;
  fIdentFuncTable[84] :=  {$IFDEF FPC}@{$ENDIF}  Func84;
  fIdentFuncTable[87] :=  {$IFDEF FPC}@{$ENDIF}  Func87;
  fIdentFuncTable[89] :=  {$IFDEF FPC}@{$ENDIF}  Func89;
  fIdentFuncTable[90] :=  {$IFDEF FPC}@{$ENDIF}  Func90;
  fIdentFuncTable[94] :=  {$IFDEF FPC}@{$ENDIF}  Func94;
  fIdentFuncTable[97] :=  {$IFDEF FPC}@{$ENDIF}  Func97;
  fIdentFuncTable[98] :=  {$IFDEF FPC}@{$ENDIF}  Func98;
  fIdentFuncTable[100] :=  {$IFDEF FPC}@{$ENDIF}  Func100;
  fIdentFuncTable[101] :=  {$IFDEF FPC}@{$ENDIF}  Func101;
  fIdentFuncTable[102] :=  {$IFDEF FPC}@{$ENDIF}  Func102;
  fIdentFuncTable[105] :=  {$IFDEF FPC}@{$ENDIF}  Func105;
  fIdentFuncTable[107] :=  {$IFDEF FPC}@{$ENDIF}  Func107;
  fIdentFuncTable[109] :=  {$IFDEF FPC}@{$ENDIF}  Func109;
  fIdentFuncTable[110] :=  {$IFDEF FPC}@{$ENDIF}  Func110;
  fIdentFuncTable[111] :=  {$IFDEF FPC}@{$ENDIF}  Func111;
  fIdentFuncTable[113] :=  {$IFDEF FPC}@{$ENDIF}  Func113;
  fIdentFuncTable[117] :=  {$IFDEF FPC}@{$ENDIF}  Func117;
  fIdentFuncTable[118] :=  {$IFDEF FPC}@{$ENDIF}  Func118;
  fIdentFuncTable[120] :=  {$IFDEF FPC}@{$ENDIF}  Func120;
  fIdentFuncTable[121] :=  {$IFDEF FPC}@{$ENDIF}  Func121;
  fIdentFuncTable[125] :=  {$IFDEF FPC}@{$ENDIF}  Func125;
  fIdentFuncTable[126] :=  {$IFDEF FPC}@{$ENDIF}  Func126;
  fIdentFuncTable[127] :=  {$IFDEF FPC}@{$ENDIF}  Func127;
  fIdentFuncTable[128] :=  {$IFDEF FPC}@{$ENDIF}  Func128;
  fIdentFuncTable[130] :=  {$IFDEF FPC}@{$ENDIF}  Func130;
  fIdentFuncTable[134] :=  {$IFDEF FPC}@{$ENDIF}  Func134;
  fIdentFuncTable[139] :=  {$IFDEF FPC}@{$ENDIF}  Func139;
  fIdentFuncTable[140] :=  {$IFDEF FPC}@{$ENDIF}  Func140;
  fIdentFuncTable[146] :=  {$IFDEF FPC}@{$ENDIF}  Func146;
  fIdentFuncTable[147] :=  {$IFDEF FPC}@{$ENDIF}  Func147;
  fIdentFuncTable[150] :=  {$IFDEF FPC}@{$ENDIF}  Func150;
  fIdentFuncTable[156] :=  {$IFDEF FPC}@{$ENDIF}  Func156;
  fIdentFuncTable[158] :=  {$IFDEF FPC}@{$ENDIF}  Func158;
  fIdentFuncTable[164] :=  {$IFDEF FPC}@{$ENDIF}  Func164;
  fIdentFuncTable[171] :=  {$IFDEF FPC}@{$ENDIF}  Func171;
  fIdentFuncTable[196] :=  {$IFDEF FPC}@{$ENDIF}  Func196;
  fIdentFuncTable[199] :=  {$IFDEF FPC}@{$ENDIF}  Func199;
end;

function TSynRCSyn.KeyComp(const aKey: string): boolean;
var
 i: integer;
 tmp: pchar;
begin
  tmp:= fToIdent;
  if length(aKey) = fStringLen then
   begin
     result:= TRUE;
     for i:= 1 to fStringLen do
      begin
        if tmp^ <> akey[i] then
         begin
           result:= FALSE;
           break;
         end;
        inc(tmp);
      end;
   end
  else
   result:= FALSE;
end;

function TSynRCSyn.KeyHash(ToHash: PChar): integer;
begin
  result:= 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
   begin
     inc(Result, mHashTable[ToHash^]);
     inc(ToHash);
   end;
   fStringLen:= ToHash -fToIdent;
end;

function TSynRCSyn.AltFunc: TtkTokenKind;
begin
  result:= tkIdentifier;
end;

function TSynRCSyn.IdentKind(Maybe: PChar): TtkTokenKind;
var
 HashKey: integer;
begin
  fToIdent:= Maybe;
  HashKey:= KeyHash(maybe);
  if HashKey <= MAXNumFunc then
   result:= fIdentFuncTable[HashKey] {$ifdef FPC} () {$endif}
  else
   result:= tkIdentifier;
end;

constructor TSynRCSyn.Create(aOwner: TComponent);
begin
  inherited;
  fCommentAttri:= TSynHighlighterAttributes.Create(SYNS_AttrComment);
  AddAttribute(fCommentAttri);

  fDirecAttri:= TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor);
  AddAttribute(fDirecAttri);

  fIdentifierAttri:= TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri:= TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri:= TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);

  fSpaceAttri:= TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri:= TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);

  fSymbolAttri:= TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange({$ifdef FPC} @ {$endif}DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fRange:= rsUnknown;
  fDefaultFilter:= SYNS_FilterRC;
end;

destructor TSynRCSyn.Destroy;
begin
  inherited;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynRCSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;
{$ENDIF}

procedure TSynRCSyn.SetLine(const NewValue: string; LineNumber: integer);
begin
  fLine:= PChar(NewValue);
  Run:= 0;
  fLineNumber:= LineNumber;
  Next;
end;

// ** insert symbol procs/num funcs

function TSynRCSyn.Func33: TtkTokenKind;
begin
  if KeyComp('ALT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func39: TtkTokenKind;
begin
  if KeyComp('CHECKED') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func41: TtkTokenKind;
begin
  if KeyComp('ICON') then result:= tkKey
  else
   if KeyComp('ASCII') then result:= tkKey
   else
    if KeyComp('HELP') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func43: TtkTokenKind;
begin
  if KeyComp('BLOCK') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func47: TtkTokenKind;
begin
  if KeyComp('RCDATA') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func48: TtkTokenKind;
begin
  if KeyComp('DIALOG') then result:= tkKey
  else
   if KeyComp('FIXED') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func53: TtkTokenKind;
begin
  if KeyComp('MENU') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func54: TtkTokenKind;
begin
  if KeyComp('CLASS') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func55: TtkTokenKind;
begin
  if KeyComp('FONT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func60: TtkTokenKind;
begin
  if KeyComp('PURE') then result:= tkKey
  else
   if KeyComp('GRAYED') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func61: TtkTokenKind;
begin
  if KeyComp('BITMAP') then result:= tkKey
  else
   if KeyComp('VALUE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func62: TtkTokenKind;
begin
  if KeyComp('SHIFT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func66: TtkTokenKind;
begin
  if KeyComp('FILEOS') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func68: TtkTokenKind;
begin
  if KeyComp('LANGUAGE') then result:= tkKey
  else
   if KeyComp('STATE3') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func71: TtkTokenKind;
begin
  if KeyComp('CHECKBOX') then result:= tkKey
  else
   if KeyComp('PRELOAD') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func72: TtkTokenKind;
begin
  if KeyComp('CTEXT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func75: TtkTokenKind;
begin
  if KeyComp('MOVEABLE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func77: TtkTokenKind;
begin
  if KeyComp('DIALOGEX') then result:= tkKey
  else
   if KeyComp('FILEFLAGS') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func78: TtkTokenKind;
begin
  if KeyComp('CAPTION') then result:= tkKey
  else
   if KeyComp('DISCARDABLE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func81: TtkTokenKind;
begin
  if KeyComp('LTEXT') then result:= tkKey
  else
   if KeyComp('STYLE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func82: TtkTokenKind;
begin
  if KeyComp('MENUEX') then result:= tkKey
  else
   if KeyComp('IMPURE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func83: TtkTokenKind;
begin
  if KeyComp('INACTIVE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func84: TtkTokenKind;
begin
  if KeyComp('POPUP') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func87: TtkTokenKind;
begin
  if KeyComp('RTEXT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func89: TtkTokenKind;
begin
  if KeyComp('COMBOBOX') then result:= tkKey
  else
   if KeyComp('LOADONCALL') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func90: TtkTokenKind;
begin
  if KeyComp('MENUBREAK') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func94: TtkTokenKind;
begin
  if KeyComp('CURSOR') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func97: TtkTokenKind;
begin
  if KeyComp('CONTROL') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func98: TtkTokenKind;
begin
  if KeyComp('FILETYPE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func100: TtkTokenKind;
begin
  if KeyComp('MENUITEM') then result:= tkKey
  else
   if KeyComp('SCROLLBAR') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func101: TtkTokenKind;
begin
  if KeyComp('LISTBOX') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func102: TtkTokenKind;
begin
  if KeyComp('VERSION') then result:= tkKey
  else
   if KeyComp('COMMENTS') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func105: TtkTokenKind;
begin
  if KeyComp('PUSHBOX') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func107: TtkTokenKind;
begin
  if KeyComp('EDITTEXT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func109: TtkTokenKind;
begin
  if KeyComp('MESSAGETABLE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func110: TtkTokenKind;
begin
  if KeyComp('EXSTYLE') then result:= tkKey
  else
   if KeyComp('VIRTKEY') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func111: TtkTokenKind;
begin
  if KeyComp('MENUBARBREAK') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func113: TtkTokenKind;
begin
  if KeyComp('SEPARATOR') then result:= tkKey
  else
   if KeyComp('SPECIALBUILD') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func117: TtkTokenKind;
begin
  if KeyComp('NOINVERT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func118: TtkTokenKind;
begin
  if KeyComp('GROUPBOX') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func120: TtkTokenKind;
begin
  if KeyComp('ACCELERATORS') then result:= tkKey
  else
   if KeyComp('COMPANYNAME') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func121: TtkTokenKind;
begin
  if KeyComp('FILEFLAGSMASK') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func125: TtkTokenKind;
begin
  if KeyComp('AUTO3STATE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func126: TtkTokenKind;
begin
  if KeyComp('INTERNALNAME') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func127: TtkTokenKind;
begin
  if KeyComp('STRINGTABLE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func128: TtkTokenKind;
begin
  if KeyComp('AUTOCHECKBOX') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func130: TtkTokenKind;
begin
  if KeyComp('PRODUCTNAME') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func134: TtkTokenKind;
begin
  if KeyComp('FILEVERSION') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func139: TtkTokenKind;
begin
  if KeyComp('RADIOBUTTON') then result:= tkKey
  else
   if KeyComp('PRIVATEBUILD') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func140: TtkTokenKind;
begin
  if KeyComp('FILESUBTYPE') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func146: TtkTokenKind;
begin
  if KeyComp('VERSIONINFO') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func147: TtkTokenKind;
begin
  if KeyComp('LEGALTRADEMARKS') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func150: TtkTokenKind;
begin
  if KeyComp('ORIGINALFILENAME') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func156: TtkTokenKind;
begin
  if KeyComp('CHARACTERISTICS') then result:= tkKey
  else
   if KeyComp('PUSHBUTTON') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func158: TtkTokenKind;
begin
  if KeyComp('LEGALCOPYRIGHT') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func164: TtkTokenKind;
begin
  if KeyComp('FILEDESCRIPTION') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func171: TtkTokenKind;
begin
  if KeyComp('DEFPUSHBUTTON') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func196: TtkTokenKind;
begin
  if KeyComp('AUTORADIOBUTTON') then result:= tkKey
  else
   result:= tkIdentifier;
end;

function TSynRCSyn.Func199: TtkTokenKind;
begin
  if KeyComp('PRODUCTVERSION') then result:= tkKey
  else
   result:= tkIdentifier;
end;

procedure TSynRCSyn.MakeMethodTables;
var
 i: char;
begin
  for i:= #0 to #255 do
   case i of
    #0:                         fProcTable[i]:= {$ifdef FPC} @ {$endif}NullProc;
    #13:                        fProcTable[i]:= {$ifdef FPC} @ {$endif}CRProc;
    #10:                        fProcTable[i]:= {$ifdef FPC} @ {$endif}LFProc;
    '/':                        fProcTable[i]:= {$ifdef FPC} @ {$endif}SlashProc;
    '"':                        fProcTable[i]:= {$ifdef FPC} @ {$endif}QuoteProc;
    '#':                        fProcTable[i]:= {$ifdef FPC} @ {$endif}DirectiveProc;
    'A'..'Z', 'a'..'z', '_':    fProcTable[i]:= {$ifdef FPC} @ {$endif}IdentProc;
    '0'..'9':                   fProcTable[i]:= {$ifdef FPC} @ {$endif}NumberProc;
    #1..#9, #11, #12, #14..#32: fPRocTable[i]:= {$ifdef FPC} @ {$endif}SpaceProc;
    '|', ',', '{', '}':         fProcTable[i]:= {$ifdef FPC} @ {$endif}SymbolProc;
   else
    fProcTable[i]:= {$ifdef FPC} @ {$endif}UnknownProc;
   end;
end;

procedure TSynRCSyn.QuoteProc;
begin
  fTokenId:= tkString;
  repeat
   inc(Run);
  until (fLine[Run] in [#0, #10, #13, #34]);
  if fLine[Run] = #34 then
    inc(Run);
end;

procedure TSynRCSyn.SlashProc;
begin
  case fLine[Run +1] of
   #13: CRPRoc;
   #10: LFProc;
   '/':
    begin
      fTokenId:= tkComment;
      inc(Run, 2);
      while not (fLine[Run] in [#0, #13, #10]) do inc(Run);
    end;
   '*':
    begin
      fTokenID:= tkComment;
      fRange:= rsComment;
      inc(Run, 2);
      while fLine[Run] <> #0 do
       case fLine[Run] of
        '*':
         if fLine[Run +1] = '/' then
          begin
            inc(Run, 2);
            fRange:= rsUnknown;
            break;
          end
         else inc(Run);
        #10, #13: break;
       else
        inc(Run);
       end;
    end;
  else
   fTokenId:= tkSymbol;
   inc(Run);  
  end
end;

procedure TSynRCSyn.CommentProc;
begin
  fTokenId:= tkComment;
  case fLine[Run] of
   #0: NullProc;
  #13: CRProc;
  #10: LFProc;
  else
   fTokenId:= tkComment;
   repeat
    if (fLine[Run] = '*') and (fLine[Run +1] = '/') then
     begin
       inc(Run, 2);
       fRange:= rsUnknown;
       break;
     end
    else
     inc(Run);
   until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynRCSyn.DirectiveProc;
begin
  fTokenId:= tkDirective;
  repeat
   if (fLine[Run] = '/') then
    begin
      if fLine[Run +1] = '/' then
       begin
         fRange:= rsUnknown;
         exit;
       end
      else
       if fLine[Run +1] = '*' then
        begin
          fRange:= rsComment;
          exit;
        end
    end;
   inc(Run);
  until (fLine[Run] in [#0, #13, #10]);
end;

procedure TSynRCSyn.IdentProc;
begin
  fTokenId:= IdentKind((fLine +Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynRCSyn.CRProc;
begin
  fTokenID:= tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
   inc(Run);
end;

procedure TSynRCSyn.LFProc;
begin
  inc(Run);
  fTokenID:= tkSpace;
end;

procedure TSynRCSyn.SpaceProc;
begin
  inc(Run);
  fTokenId:= tkSpace;
  while fLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynRCSyn.NullProc;
begin
  fTokenId:= tkNull;
end;

procedure TSynRCSyn.NumberProc;
begin
  inc(Run);
  fTokenID:= tkNumber;
  while (fLine[Run] in ['0'..'9', '.', 'u', 'U', 'e', 'E', 'x', 'X']) or
  (fLine[Run] in ['A'..'F', 'a'..'f','L', 'l', '-', '+']) do
   begin
     case fLine[Run] of
      '.': if fLine[Run +1] = '.' then break;
     end;
     inc(Run);
   end;
end;

procedure TSynRCSyn.SymbolProc;
begin
  inc(Run);
  fTokenID:= tkSymbol;
end;

procedure TSynRCSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynRCSyn.Next;
begin
  fTokenPos:= Run;
  case fRange of
   rsDirective: DirectiveProc;
   rsComment: CommentProc;
  else
   fProcTable[fLine[Run]];
  end;
end;

function TSynRCSyn.GetDefaultAttribute(
  index: integer): TSynHighlighterAttributes;
begin
  case index of
   SYN_ATTR_COMMENT: result:= fCommentAttri;
   SYN_ATTR_IDENTIFIER: result:= fIdentifierAttri;
   SYN_ATTR_KEYWORD: result:= fKeyAttri;
   SYN_ATTR_STRING: result:= fStringAttri;
   SYN_ATTR_WHITESPACE: result:= fSpaceAttri;
   SYN_ATTR_SYMBOL: result:= fSymbolAttri;
  else
   result:= nil;
  end;
end;

function TSynRCSyn.GetEOL: boolean;
begin
  result:= fTokenID = tkNull;
end;

function TSynRCSyn.GetRange: pointer;
begin
  result:= pointer(PtrInt(fRange));
end;

function TSynRCSyn.GetToken: string;
var
 len: longint;
begin
  len:= Run - fTokenPos;
  SetString(result, (fLine +fTokenPos), len);
end;

function TSynRCSyn.GetTokenID: TtkTokenKind;
begin
  result:= fTokenID;
end;

function TSynRCSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
   tkComment: result:= fCommentAttri;
   tkDirective: result:= fDirecAttri;
   tkIdentifier: result:= fIdentifierAttri;
   tkKey: result:= fKeyAttri;
   tkNumber: result:= fNumberAttri;
   tkSpace: result:= fSpaceAttri;
   tkString: result:= fStringAttri;
   tkSymbol: result:= fSymbolAttri;
   tkUnknown: result:= fSymbolAttri;
  else
   result:= nil;
  end;
end;

function TSynRCSyn.GetTokenKind: integer;
begin
  result:= ord(GetTokenID);
end;

function TSynRCSyn.GetTokenPos: integer;
begin
  result:= fTokenPos;
end;

procedure TSynRCSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynRCSyn.SetRange(value: pointer);
begin
  fRange:= TRangeState(PtrUInt(value));
end;

procedure TSynRCSyn.EnumUserSettings(Settings: TStrings);
begin
  // ** ??
end;

function TSynRCSyn.UseUserSettings(SettingIndex: integer): boolean;
begin
  result:= FALSE;
end;

function TSynRCSyn.GetIdentChars: TSynIdentChars;
begin
  result:= TSynValidStringChars; // ** check
end;

class function TSynRCSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  result:= inherited GetCapabilities;
end;

function TSynRCSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterRC;
end;

class function TSynRCSyn.GetLanguageName: string;
begin
  result:= SYNS_LangRC;
end;

function TSynRCSyn.GetSampleSource: string;
begin
  result:= '// Overlay icon for Hardlinks'+LineEnding+
           'HARDLINK ICON DISCARDABLE IMPURE "..\..\graphics\hardlink.ico"'+LineEnding+LineEnding+
           '// Overlay icon for Junctions'+LineEnding+
           'JUNCTION ICON DISCARDABLE IMPURE "..\..\graphics\junction.ico"'+LineEnding+LineEnding+
           '// Icons used for various of menu items of NTFS Link'+LineEnding+
           'MENU_GLYPH_STD BITMAP DISCARDABLE IMPURE "..\..\graphics\menu_glyph_ntfslinkext.bmp"'+LineEnding+
           'MENU_GLYPH_JUNCTION BITMAP DISCARDABLE IMPURE "..\..\graphics\menu_glyph_junction.bmp"'+LineEnding+
           'MENU_GLYPH_HARDLINK BITMAP DISCARDABLE IMPURE "..\..\graphics\menu_glyph_hardlink.bmp"'+LineEnding+
           'MENU_GLYPH_LINKDEL BITMAP DISCARDABLE IMPURE "..\..\graphics\menu_glyph_linkdel.bmp"'+LineEnding+
           'MENU_GLYPH_EXPLORER BITMAP DISCARDABLE IMPURE "..\..\graphics\menu_glyph_explorer.bmp"'+LineEnding+LineEnding+LineEnding+
           '#define IDC_TFLINKSEXISTINGDIALOG 1000'+LineEnding+
           '#define IDC_CAPTION 100'+LineEnding+
           '#define IDC_MLINKS 102'+LineEnding+
           '#define IDC_BNO 103'+LineEnding+
           '#define IDC_BYES 104'+LineEnding+
           '#define IDC_ICON 105'+LineEnding+
           '#define IDC_BYESDELETE 106'+LineEnding+LineEnding+
           'IDC_TFLINKSEXISTINGDIALOG DIALOGEX 120 82 266 144'+LineEnding+
           'EXSTYLE 0x00 | WS_EX_DLGMODALFRAME | WS_EX_WINDOWEDGE'+LineEnding+
           'STYLE 0x00 |  WS_POPUP |  WS_CAPTION | WS_DLGFRAME | DS_MODALFRAME | DS_CENTER | WS_VISIBLE | DS_3DLOOK | DS_FIXEDSYS | WS_SYSMENU'+LineEnding+
           'CAPTION "NTFS Link"'+LineEnding+
           'FONT 8, "MS Shell Dlg 2", 400, 0'+LineEnding+
           '{'+LineEnding+
           '  CONTROL "There are junctions pointing to this folder or a subfolder (see list below). If you continue the operation, these links will no longer work. Are you sure?", IDC_CAPTION, "static", WS_CHILD | SS_NOTIFY | SS_LEFT | WS_VISIBLE, 40, 6, 214, 25, 0x00'+LineEnding+
           '  CONTROL "", IDC_MLINKS, "edit", WS_CHILD | ES_MULTILINE | WS_VISIBLE | ES_READONLY, 40, 40, 214, 79, 0x00 | WS_EX_CLIENTEDGE'+LineEnding+
           '  CONTROL "", IDC_BYESDELETE, "button", WS_CHILD | WS_GROUP | BS_PUSHBUTTON | WS_VISIBLE | WS_TABSTOP, 67, 124, 79, 15, 0x00'+LineEnding+
           '  CONTROL "Yes", IDC_BYES, "button", WS_CHILD | BS_PUSHBUTTON | WS_VISIBLE | WS_TABSTOP, 150, 124, 50, 15, 0x00'+LineEnding+
           '  CONTROL "No", IDC_BNO, "button", WS_CHILD | BS_DEFPUSHBUTTON | WS_VISIBLE | WS_TABSTOP, 204, 124, 50, 15, 0x00'+LineEnding+
           '  CONTROL "", IDC_ICON, "static", SS_ICON | WS_CHILD | WS_VISIBLE | WS_GROUP, 8, 10, 32, 32'+LineEnding+
           '}'+LineEnding+LineEnding+
           '// ActivationContext.pas expects this at resource ID 2.'+LineEnding+
           '2 24 "NTFSLink.manifest"'+LineEnding+LineEnding+LineEnding+
           '1 VERSIONINFO'+LineEnding+
           '  FILEVERSION 8,3,0,0'+LineEnding+
           '  FILEOS VOS__WINDOWS32'+LineEnding+
           '  FILETYPE VFT_APP'+LineEnding+
           '  BEGIN'+LineEnding+
           '    BLOCK "StringFileInfo"'+LineEnding+
           '    BEGIN'+LineEnding+
           '      BLOCK "040904E4"'+LineEnding+
           '      BEGIN'+LineEnding+
           '        VALUE "FileDescription", "%APPNAME% %APPVER%\000"'+LineEnding+
           '        VALUE "ProductName", "%APPNAME%\000"'+LineEnding+
           '        VALUE "LegalCopyright", "See GPL license\000"'+LineEnding+
           '      END'+LineEnding+
           '    END'+LineEnding+
           '    BLOCK "VarFileInfo"'+LineEnding+
           '    BEGIN'+LineEnding+
           '      VALUE "Translation", 0x0409 0x04E4'+LineEnding+
           '    END'+LineEnding+
           '  END'+LineEnding
           ;

end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynRCSyn);
{$ENDIF}
end.