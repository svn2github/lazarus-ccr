unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  SynEdit, Menus, StdCtrls, SynEditHighlighter,

  SynHighlighterADSP21xx, SynHighlighterFortran, SynHighlighterFoxpro,  SynHighlighterGalaxy,   SynHighlighterBaan,
  SynHighlighterHaskell,  SynHighlighterCache,  {SynHighlighterDfm,}    SynHighlighterModelica, SynHighlighterCobol,
  {SynHighlighterCPM,}    SynHighlighterCS,      SynHighlighterDml,     SynHighlighterProgress, SynHighlighterEiffel,
  SynHighlighterGWS,     {SynHighlighterHC11,}   SynHighlighterHP48,    SynHighlighterVBScript, SynHighlighterUnreal,
  SynHighlighterVrml97,   SynHighlighterTclTk,   SynHighlighterLDraw,   SynHighlighterRuby,     SynHighlighterInno,
  SynHighlighterAsm,      SynHighlighterDOT,     SynHighlighterIDL,     SynHighlighterKix,     {SynHighlighterMsg,}
  SynHighlighterSDD,      SynHighlighterSml,     SynHighlighterURI,     SynHighlighterM3,       SynHighlighterRC,
  SynHighlighterST,       SynHighlighter8051,    SynHighlighterLua,     SynHighlighterProlog,   SynHighlighterCAC,
  SynHighlighterAWK;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    SynEdit1: TSynEdit;
    procedure MenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FHighlighters: Array of TSynCustomHighlighter;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SynUniHighlighter;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);

  procedure SetDefaultColors(const vHighLighter:TSynCustomHighlighter);
  begin
    if Assigned(vHighLighter.CommentAttribute)    then vHighLighter.CommentAttribute.Foreground    := clSilver;
    if Assigned(vHighLighter.IdentifierAttribute) then vHighLighter.IdentifierAttribute.Foreground := clNone;
    if Assigned(vHighLighter.CommentAttribute)    then vHighLighter.CommentAttribute.Foreground    := $00A2A2A2;
    if Assigned(vHighLighter.KeywordAttribute)    then vHighLighter.KeywordAttribute.Foreground    := clNavy;
    //vHighLighter.NumberAttri.ForeGround         := $004080FF;
    if Assigned(vHighLighter.StringAttribute)     then vHighLighter.StringAttribute.ForeGround     := $003FB306;
    if Assigned(vHighLighter.SymbolAttribute)     then vHighLighter.SymbolAttribute.ForeGround     := $00A25151;
  end;

var
  i, c :Integer;
  item :TMenuItem;
begin
  SetLength(FHighlighters, 35);

  for i:= low(FHighlighters) to high(FHighlighters) do
    FHighlighters[i] := Nil;

  FHighlighters[00] := TSynADSP21xxSyn.Create(self);
  FHighlighters[01] := TSynFortranSyn.Create(self);
  FHighlighters[02] := TSynFoxproSyn.Create(self);
  FHighlighters[03] := TSynGalaxySyn.Create(self);
  FHighlighters[04] := TSynBaanSyn.Create(self);
  FHighlighters[05] := TSynAWKSyn.Create(self);
  FHighlighters[06] := TSynHaskellSyn.Create(self);
  FHighlighters[07] := TSynCacheSyn.Create(self);
  FHighlighters[08] := TSynModelicaSyn.Create(self);
  FHighlighters[09] := TSynCobolSyn.Create(self);
  FHighlighters[10] := TSynCSSyn.Create(self);
  FHighlighters[11] := TSynDmlSyn.Create(self);
  FHighlighters[12] := TSynProgressSyn.Create(self);
  FHighlighters[13] := TSynEiffelSyn.Create(self);
  FHighlighters[14] := TSynGWScriptSyn.Create(self);
  FHighlighters[15] := TSynHP48Syn.Create(self);
  FHighlighters[16] := TSynVBScriptSyn.Create(self);
  FHighlighters[17] := TSynUnrealSyn.Create(self);
  FHighlighters[18] := TSynVrml97Syn.Create(self);
  FHighlighters[19] := TSynTclTkSyn.Create(self);
  FHighlighters[20] := TSynLDRSyn.Create(self);
  FHighlighters[21] := TSynRubySyn.Create(self);
  FHighlighters[22] := TSynInnoSyn.Create(self);
  FHighlighters[23] := TSynAsmSyn.Create(self);
  FHighlighters[24] := TSynDOTSyn.Create(self);
  FHighlighters[25] := TSynIdlSyn.Create(self);
  FHighlighters[26] := TSynKixSyn.Create(self);
  FHighlighters[27] := TSynSDDSyn.Create(self);
  FHighlighters[28] := TSynSMLSyn.Create(self);
  FHighlighters[29] := TSynURISyn.Create(self);
  FHighlighters[30] := TSynM3Syn.Create(self);
  FHighlighters[31] := TSynRCSyn.Create(self);
  FHighlighters[32] := TSynPrologSyn.Create(Self);
  FHighlighters[33] := TSynLuaSyn.Create(Self);
  FHighlighters[34] := TSyn8051Syn.Create(Self);
  FHighlighters[35] := TSynCACSyn.Create(Self);

  // FHighlighters[06] := TSynGeneralSyn.Create(self);
  // FHighlighters[09] := TSynDfmSyn.Create(self);
  // FHighlighters[12] := TSynUniSyn.Create(self);
  // FHighlighters[12] := TSynCPMSyn.Create(self);
  // FHighlighters[30] := TSynMsgSyn.Create(self);
  // FHighlighters[36] := TSynSTSyn.Create(self);

  for i:= low(FHighlighters) to high(FHighlighters) do
    if Assigned(FHighlighters[i]) then begin
      SetDefaultColors(FHighlighters[i]);
      item         := TMenuItem.Create(self);
      item.Tag     := i+1; //0 = unknown highlighter
      try
        if (FHighlighters[i] is TSynUniSyn) and (TSynUniSyn(FHighlighters[i]).Info.General.Name <> '') then
          item.Caption := IntToStr(i) + ' - ' + TSynUniSyn(FHighlighters[i]).Info.General.Name
        else
          item.Caption := IntToStr(i) + ' - ' + TSynCustomHighlighter(FHighlighters[i]).LanguageName;
      except
        on E : Exception  do
          ShowMessage(E.Message+LineEnding+' at index '+inttostr(i));
      end;
      item.Name := FHighlighters[i].ClassName;
      item.OnClick := @MenuClick;
      if i > 27 then
        WriteLn(I, ' : ',Item.Name,' : ', item.Caption);
      MenuItem1.Add(item);
    end;

  SynEdit1.Highlighter := FHighlighters[12];
  Label1.Caption := FHighlighters[12].ClassName;
end;

procedure TForm1.MenuClick(Sender: TObject);
var i,c:Integer;
begin
  c:= ComponentCount-1;
  if (Sender is TMenuItem) and (TMenuItem(Sender).Tag > 0) then begin
    SynEdit1.Highlighter := FHighlighters[TMenuItem(Sender).Tag-1];
    Caption := SynEdit1.Highlighter.LanguageName;
    SynEdit1.Text := SynEdit1.Highlighter.SampleSource;
    Label1.Caption := Caption;
  end;
end;

end.




