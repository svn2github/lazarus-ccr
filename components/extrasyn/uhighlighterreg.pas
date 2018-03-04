unit uHighlighterReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  SynHighlighterADSP21xx, SynHighlighterFortran, SynHighlighterFoxpro,  SynHighlighterGalaxy, SynHighlighterBaan,
  SynHighlighterHaskell,  SynHighlighterCache,   SynHighlighterCS,      SynHighlighterDml,    SynHighlighterCAC,
  SynHighlighterModelica, SynHighlighterCobol,   SynHighlighterTclTk,   SynHighlighterHP48,   SynHighlighterAWK,
  SynHighlighterProgress, SynHighlighterEiffel,  SynHighlighterGWS,     SynHighlighterDOT,    SynHighlighterLDraw,
  SynHighlighterVBScript, SynHighlighterUnreal,  SynHighlighterVrml97,  SynHighlighterSml,    SynHighlighterIDL,
  SynHighlighterRuby,     SynHighlighterInno,    SynHighlighterAsm,     SynHighlighter8051,   SynHighlighterLua,
  SynHighlighterKix,      SynHighlighterSDD,     SynHighlighterProlog,  SynHighlighterRC,     SynHighlighterM3;

{.$R ImagesPNG.Res}
{.$R ImagesPNG.Res}
{.$R ImagesPNG.Res}

{.$R *.rc}
{.$R Images.rc}
//failed miserably to use rc files they are not linked in lazarus for some reason although if I delete the res file
//from the disk the compiler complains about the missing file.
procedure Register;

implementation
uses LResources;



procedure Register;
begin
  RegisterComponents('SynEdit Highlighters',[TSynADSP21xxSyn, TSynTclTkSyn,  TSynRubySyn, TSynDOTSyn, TSynCSSyn,
                                             TSynHaskellSyn,  TSynFoxproSyn, TSynInnoSyn, TSynDmlSyn, TSynCACSyn,
                                             TSynModelicaSyn, TSynVrml97Syn, TSynHP48Syn, TSynKixSyn, TSynAWKSyn,
                                             TSynProgressSyn, TSynEiffelSyn, TSynBaanSyn, TSynM3Syn,  TSynLDRSyn,
                                             TSynVBScriptSyn, TSynUnrealSyn, TSynSMLSyn,  TSynIdlSyn, TSynCobolSyn,
                                             TSynGWScriptSyn, TSynGalaxySyn, TSyn8051Syn, TSynAsmSyn, TSynLuaSyn,
                                             TSynFortranSyn,  TSynPrologSyn, TSynSDDSyn,  TSynRCSyn,  TSynCacheSyn]);
end;
initialization
  {$I Images.lrs}
end.

