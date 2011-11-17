unit gameconfigform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // LCL
  ExtCtrls,
  // TappyTux
  tappymodules, gameplayform;

type

  { TformConfig }

  TformConfig = class(TForm)
    buttonLoad: TButton;
    btnWordlist: TButton;
    comboLanguage: TComboBox;
    comboGameType: TComboBox;
    comboSound: TComboBox;
    comboMusic: TComboBox;
    comboLevel: TComboBox;
    labelGameType: TLabel;
    labelWordlist: TLabel;
    lblLevel1: TLabel;
    labelSettings: TLabel;
    lblSound: TLabel;
    lblMusic: TLabel;
    lblLevel: TLabel;
    lblCredits: TLabel;
    listWordlist: TListBox;
    memoGameType: TMemo;
    memoCredits: TMemo;
    procedure buttonLoadClick(Sender: TObject);
    procedure btnWordlistClick(Sender: TObject);
    procedure comboGameTypeChange(Sender: TObject);
    procedure comboLanguageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memoCreditsChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure TranslateUI();
  end;

var
  formConfig: TformConfig;

implementation


{$R *.lfm}

{ TformConfig }

procedure TformConfig.comboGameTypeChange(Sender: TObject);
var
  lModule: TTappyModule;
begin
  lModule := GetModule(comboGameType.itemIndex);
  memoGameType.Text := lModule.LongDescription;
  labelWordlist.Caption := lModule.ConfigCaption;
  listWordlist.Items.Text := lModule.ConfigItems;
  if listWordlist.Items.Count >= comboLanguage.ItemIndex then
  begin
    if comboLanguage.ItemIndex < listWordlist.Items.Count then
      listWordlist.ItemIndex := comboLanguage.ItemIndex;
  end;
end;

procedure TformConfig.comboLanguageChange(Sender: TObject);
begin
  case comboLanguage.ItemIndex of
  0: // english
  begin
    labelGameType.Caption := 'Game type:';
    labelSettings.Caption := 'Settings';
    buttonLoad.Caption := 'Play';
  end;
  1: // portuguese
  begin
    labelGameType.Caption := 'Tipo do jogo:';
    labelSettings.Caption := 'Configurações';
    buttonLoad.Caption := 'Iniciar o Jogo';
  end;
  end;
end;

procedure TformConfig.buttonLoadClick(Sender: TObject);
begin
  SetCurrentModule(comboGameType.ItemIndex);
  formTappyTuxGame.Show;
  GetCurrentModule().StartNewGame(comboSound.ItemIndex, comboMusic.ItemIndex,
                                  comboLevel.ItemIndex, listWordlist.ItemIndex);

  Hide;
end;

procedure TformConfig.btnWordlistClick(Sender: TObject);
begin

end;

procedure TformConfig.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  TranslateUI();

  // Initialize modules
  for i := 0 to GetModuleCount() -1 do
    GetModule(i).InitModule();
end;

procedure TformConfig.FormShow(Sender: TObject);
begin
  comboLanguageChange(Self);
  comboGameTypeChange(Self);
end;

procedure TformConfig.memoCreditsChange(Sender: TObject);
begin

end;

procedure TformConfig.TranslateUI;
var
  i: Integer;
  lModule: TTappyModule;
begin
  comboGameType.Items.Clear;
  for i := 0 to GetModuleCount() - 1 do
  begin
    lModule := GetModule(i);
    comboGameType.Items.Add(lModule.ShortDescription);
  end;
  comboGameType.ItemIndex := 0;
end;

end.

