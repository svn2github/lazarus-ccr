unit demoMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, LCLTranslator,
  VpBaseDS, VpDayView, VpWeekView, VpTaskList,
  VpContactGrid, VpMonthView, VpResEditDlg, VpContactButtons, VpBufDS, VpNavBar;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnNewRes: TButton;
    BtnEditRes: TButton;
    CbLanguages: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RbAllTasks: TRadioButton;
    RbHideCompletedTasks: TRadioButton;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabEvents: TTabSheet;
    TabContacts: TTabSheet;
    TabInfo: TTabSheet;
    TabTasks: TTabSheet;
    VpBufDSDataStore1: TVpBufDSDataStore;
    VpContactButtonBar1: TVpContactButtonBar;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpNavBar1: TVpNavBar;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskList1: TVpTaskList;
    VpWeekView1: TVpWeekView;
    procedure BtnNewResClick(Sender: TObject);
    procedure BtnEditResClick(Sender: TObject);
    procedure CbLanguagesChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure RbAllTasksChange(Sender: TObject);
    procedure RbHideCompletedTasksChange(Sender: TObject);
    procedure VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; Index: Integer);
  private
    { private declarations }
    FLang: String;
    procedure PopulateLanguages;
    procedure ReadIni;
    procedure SetLanguage(ALang: String); overload;
    procedure SetLanguage(AIndex: Integer); overload;
    procedure WriteIni;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
 {$IFDEF WINDOWS}
  Windows,
 {$ENDIF}
  LResources, LazUTF8, LazFileUtils, StrUtils, Translations, IniFiles,
  VpMisc, VpBase, VpData;

const
  LANGUAGE_DIR = '..\..\languages\';

{$IFDEF WINDOWS}
{ This function determines the LCID from the language code.
  Works only for Windows. }
function LangToLCID(ALang: String): Integer;
begin
 case lowercase(ALang) of
   ''     : Result := $0409;    // Default = englisch
   'ar'   : Result := $0401;    // Arabic
   'bg'   : Result := $0403;    // Bulgarian
   'ca'   : Result := $0403;    // Catalan
   'cs'   : Result := $0405;    // Czech
   'de'   : Result := $0407;    // German
   'en'   : Result := $0409;    // English  (US)
   'es'   : Result := $040A;    // Spanisch
   'fi'   : Result := $040B;    // Finnish
   'fr'   : Result := $040C;    // French
   'he'   : Result := $040D;    // Hebrew
   'hu'   : Result := $040E;    // Hungarian
   'it'   : Result := $0410;    // Italian
   'jp'   : Result := $0411;    // Japanese
   'pl'   : Result := $0415;    // Polish
   'pt'   : Result := $0816;    // Portuguese (Portugal)
   'ru'   : Result := $0419;    // Russian
   'tr'   : Result := $041F;    // Turkish
   'zh_cn', 'zh-cn': Result := $0804;    // Chinese (China)
   'zh_tw', 'zh-tw': Result := $0404;    // Chinese (Taiwan)
   // please complete if necessary. Language code and LCIDs can be found at
   // http://www.science.co.il/Language/Locale-codes.asp
   else  raise Exception.CreateFmt('Language "%s" not supported. Please add to GetLCIDFromLangCode.',[ALang]);
 end;
end;
{$ENDIF}

procedure UpdateFormatSettings(ALang: String);
{$IFDEF WINDOWS}
var
  LCID: Integer;
{$ENDIF}
begin
 {$IFDEF WINDOWS}
  // Determine the LCID for the requested language
  LCID := LangToLCID(ALang);

  // Now we update the format settings to the new language
  GetLocaleFormatSettingsUTF8(LCID, DefaultFormatSettings);
 {$ENDIF}
end;

function GetFirstDayOfWeek(ALang: String): TVpDayType;
// Don't know how to determine this from the OS
begin
   Result := dtSunday;
end;

{ TMainForm }

// Adds a new resource
procedure TMainForm.BtnNewResClick(Sender: TObject);
begin
  VpResourceEditDialog1.AddNewResource;
end;

procedure TMainForm.CbLanguagesChange(Sender: TObject);
begin
  SetLanguage(CbLanguages.ItemIndex);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if CanClose then
    try
      WriteIni;
    except
    end;
end;

// Edits the currently selected resource
procedure TMainForm.BtnEditResClick(Sender: TObject);
begin
  // Open the resource editor dialog, everything is done here.
  VpResourceEditDialog1.Execute;
end;

// Load the last resource.
procedure TMainForm.FormCreate(Sender: TObject);
var
  lastRes: TVpResource;
begin
  PopulateLanguages;
  ReadIni;

  if VpBufDSDatastore1.Resources.Count > 0 then
  begin
    lastRes := VpBufDSDatastore1.Resources.Items[VpBufDSDatastore1.Resources.Count-1];
    VpBufDSDatastore1.ResourceID := lastRes.ResourceID;
  end;
end;

procedure TMainForm.PopulateLanguages;

  function ExtractLanguage(s: String): String;
  var
    p: Integer;
  begin
    s := ChangeFileExt(s, '');
    p := RPos('.', s);
    if p > 0 then
      Result := Copy(s, p+1, Length(s))
    else
      Result := '';
  end;

var
  L: TStrings;
  po: TStringList;
  lang: String;
  i: Integer;
  langdir: String;
begin
  L := TStringList.Create;
  po := TStringList.Create;
  try
    langdir := ExpandFileName(AppendPathDelim(Application.Location) + LANGUAGE_DIR);
    FindAllFiles(L, langdir, '*.po');
    po.Sorted := true;
    po.Duplicates := dupIgnore;
    for i := 0 to L.Count-1 do begin
      lang := ExtractLanguage(L[i]);
      case lang of
        'de': po.Add('de - Deutsch');
        '',
        'en': po.Add('en - English');
        'es': po.Add('es - Español');
        'fr': po.Add('fr - Français');
        'he': po.Add('he - Hebrew');
        'hu': po.Add('hu - magyar');
        'it': po.Add('it - Italian');
        'ru': po.Add('ru - русский');
      end;
    end;

    CbLanguages.Items.Assign(po);
    SetLanguage(GetDefaultLang);

  finally
    po.Free;
    L.Free;
  end;
end;

procedure TMainForm.RbAllTasksChange(Sender: TObject);
begin
  VpTaskList1.DisplayOptions.ShowAll := RbAllTasks.Checked;
end;

procedure TMainForm.RbHideCompletedTasksChange(Sender: TObject);
begin
  VpTaskList1.DisplayOptions.ShowAll := not RbHideCompletedTasks.Checked;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  lang: String;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    lang := ini.ReadString('Settings', 'Language', GetDefaultLang);
    SetLanguage(lang);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    ini.WriteString('Settings', 'Language', FLang);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.SetLanguage(AIndex: Integer);
var
  p: Integer;
  lang: String;
begin
  p := pos(' - ', CbLanguages.Items[AIndex]);
  if p > 0 then
    lang := Copy(CbLanguages.Items[AIndex], 1, p-1)
  else
    raise Exception.Create('Incorrect structure of language combobox.');
  SetLanguage(lang);
end;

procedure TMainForm.SetLanguage(ALang: String);

  function GetTimeFormat: TVpTimeFormat;
  var
    s: String;
  begin
    s := lowercase(FormatDateTime('hh:nn ampm', 0.25));
    if pos(lowercase(FormatSettings.TimeAMString), s) = Length(s) - Length(FormatSettings.TimeAMString) then
      Result := tf12Hour else
      Result := tf24Hour;
  end;

var
  i: Integer;
  langdir: String;
  found: Boolean;
  tfmt: TVpTimeFormat;
  firstWeekDay: TVpDayType;
  translator: TUpdateTranslator;
begin
  langdir := ExpandFileName(AppendPathDelim(Application.Location) + LANGUAGE_DIR);

  // Select new language
  if ALang = 'en' then
    FLang := '' else
    FLang := ALang;

  // Translate VisualPlanIt strings.
  if FLang = '' then begin
    TranslateUnitResourceStrings('vpsr', langdir + 'vpsr.po');

    { NOTE: Translation of app strings back to english not working }

    TranslateUnitResourceStrings('demoMain', langDir + 'demo.po');
    TranslateUnitResourceStrings('lclstrconsts', langDir + 'lclstrconsts.po');
    translator := TPOTranslator.Create(langdir + 'demo.po');
    if Assigned(LRSTranslator) then
      LRSTranslator.Free;
    LRSTranslator := translator;
    for i := 0 to Screen.CustomFormCount-1 do
      translator.UpdateTranslation(Screen.CustomForms[i]);
  end else
  begin
    SetDefaultLang(FLang, langdir);
    TranslateUnitResourceStrings('vpsr', langdir + 'vpsr.' + FLang + '.po');
  end;
  VpDayView1.LoadLanguage;
  VpWeekView1.LoadLanguage;
  VpMonthView1.LoadLanguage;

  // Select language in language combobox.
  if ALang = '' then ALang := 'en';
  found := false;
  for i:=0 to CbLanguages.Items.Count-1 do
    if pos(ALang + ' ', CbLanguages.Items[i]) = 1 then begin
      CbLanguages.ItemIndex := i;
      found := true;
      break;
    end;
  if not found then
    CbLanguages.ItemIndex := 0;

  // Next settings work correctly only for Windows.
 {$IFDEF WINDOWS}
  UpdateFormatSettings(ALang);
  VpDayView1.DateLabelFormat := FormatSettings.LongDateFormat;
  VpWeekView1.DayHeadAttributes.DateFormat := FormatSettings.LongDateFormat;
  VpWeekView1.DateLabelFormat := FormatSettings.LongDateFormat;
  VpMonthView1.DateLabelFormat := 'mmmm yyyy';
  VpTaskList1.DisplayOptions.DueDateFormat := FormatSettings.ShortDateFormat;
  tfmt := GetTimeFormat;
  VpDayView1.TimeFormat := tfmt;
  VpWeekView1.TimeFormat := tfmt;
  VpMonthView1.TimeFormat := tfmt;
  firstWeekDay := GetFirstDayofWeek(ALang);   // not correct at the moment
  VpMonthView1.WeekStartsOn := firstWeekDay;
  VpWeekView1.WeekStartsOn := firstWeekDay;
 {$ENDIF}

  Invalidate;
end;

procedure TMainForm.VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; Index: Integer);

  procedure ShowAllEvents;
  begin
    PageControl1.ActivePage := TabEvents;
    VpDayView1.Parent := Panel2;
    VpMonthView1.Parent := Panel2;
    VpMonthView1.Align := alBottom;
    VpDayview1.Show;
    VpMonthView1.Show;
    Splitter2.Top := 0;
    Panel2.Show;
    Splitter3.Show;
    Splitter3.Left := Width;
    VpWeekView1.Show;
  end;

  procedure ShowEventsPerMonth;
  begin
    PageControl1.ActivePage := TabEvents;
    Panel2.Hide;
    Splitter3.Hide;
    VpWeekView1.Hide;
    VpMonthView1.Parent := TabEvents;
    VpMonthView1.Align := alClient;
    VpMonthView1.Show;
  end;

  procedure ShowEventsPerWeek;
  begin
    PageControl1.ActivePage := TabEvents;
    Panel2.Hide;
    Splitter3.Hide;
    VpMonthView1.Hide;
    VpDayView1.Hide;
    VpWeekView1.Show;
  end;

  procedure ShowEventsPerDay;
  begin
    PageControl1.ActivePage := TabEvents;
    Panel2.Hide;
    Splitter3.Hide;
    VpWeekView1.Hide;
    VpDayView1.Parent := TabEvents;
    VpDayView1.Align := alClient;
    VpDayView1.Show;
  end;

  procedure ShowTasks;
  begin
    Pagecontrol1.ActivePage := TabTasks;
  end;

  procedure ShowContacts;
  begin
    PageControl1.ActivePage := TabContacts;
  end;

begin
 case VpNavBar1.ActiveFolder of
   0: case Index of  // All planner items
        0: ShowAllEvents;        // show all
        1: ShowEventsPerMonth;   // Month view only
        2: ShowEventsPerWeek;    // Week view only
        3: ShowEventsPerDay;     // Day view only
        4: ShowTasks;            // Tasks
        5: ShowContacts;         // Contacts
      end;
   1: case Index of           // Events only
        0: ShowAllEvents;       // show all
        1: ShowEventsPerMonth;  // Month view only
        2: ShowEventsPerWeek;   // Week view only
        3: ShowEventsPerDay;    // Day view only
      end;
   2: ShowTasks;
   3: ShowContacts;
 end;
end;

end.

