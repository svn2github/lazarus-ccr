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
  // NOTE: There's negative logic - if an option is checked it is ALLOWED, but
  // the set of protections contains the items which are FORBIDDEN.
  if CbSelectLockedCells.Checked then
    Exclude(Result, spSelectLockedCells) else
    Include(Result, spSelectLockedCells);
  if CbSelectUnlockedCells.Checked then
    Exclude(Result, spSelectUnlockedCells) else
    Include(Result, spSelectUnlockedCells);
end;

procedure TWorksheetProtectionForm.SetProtected(AValue: Boolean);
begin
  CbProtect.Checked := AValue;
end;

procedure TWorksheetProtectionForm.SetProtections(AValue: TsWorksheetProtections);
begin
  CbSelectLockedCells.Checked := not (spSelectLockedCells in AValue);
  CbSelectUnlockedCells.Checked := not (spSelectUnlockedCells in AValue);
end;

end.

