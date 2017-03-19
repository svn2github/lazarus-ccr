unit sWorksheetProtection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, CheckLst, fpsTypes;

type

  { TWorksheetProtectionForm }

  TWorksheetProtectionForm = class(TForm)
    TopBevel: TBevel;
    ButtonPanel: TButtonPanel;
    CbProtect: TCheckBox;
    CbSelectLockedCells: TCheckBox;
    CbSelectUnlockedCells: TCheckBox;
    LblProtectionItems: TLabel;
    ItemsPanel: TPanel;
    procedure CbProtectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetProtected: Boolean;
    function GetProtections: TsWorksheetProtections;
    procedure SetProtected(AValue: Boolean);
    procedure SetProtections(AValue: TsWorksheetProtections);
  public
    property IsProtected: Boolean read GetProtected write SetProtected;
    property Protection: TsWorksheetProtections read GetProtections write SetProtections;

  end;

var
  WorksheetProtectionForm: TWorksheetProtectionForm;

implementation

{$R *.lfm}

procedure TWorksheetProtectionForm.CbProtectChange(Sender: TObject);
var
  unlocked: Boolean;
begin
  unlocked := not IsProtected;
  LblProtectionItems.Enabled := unlocked;
  CbSelectLockedCells.Enabled := unlocked;
  CbSelectUnlockedCells.Enabled := unlocked;
end;

procedure TWorksheetProtectionForm.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := ItemsPanel.Height + CbProtect.Height +
    TopBevel.Height + ButtonPanel.Height;
end;

function TWorksheetProtectionForm.GetProtected: Boolean;
begin
  Result := CbProtect.Checked;
end;

function TWorksheetProtectionForm.GetProtections: TsWorksheetProtections;
begin
  Result := DEFAULT_SHEET_PROTECTION;
  if CbSelectLockedCells.Checked then
    Include(Result, spSelectLockedCells) else
    Exclude(Result, spSelectLockedCells);
  if CbSelectUnlockedCells.Checked then
    Include(Result, spSelectUnlockedCells) else
    Exclude(Result, spSelectUnlockedCells);
end;

procedure TWorksheetProtectionForm.SetProtected(AValue: Boolean);
begin
  CbProtect.Checked := AValue;
end;

procedure TWorksheetProtectionForm.SetProtections(AValue: TsWorksheetProtections);
begin
  CbSelectLockedCells.Checked := spSelectLockedCells in AValue;
  CbSelectUnlockedCells.Checked := spSelectUnlockedCells in AValue;
end;

end.

