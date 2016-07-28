unit environment_buildscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, synhighlighterunixshellscript, Forms,
  Controls, StdCtrls, EditBtn, IDEOptionsIntf, iPhoneExtOptions;

type

  { TTiPhoneBuildScriptEditor }

  TTiPhoneBuildScriptEditor = class(TAbstractIDEOptionsEditor)
    lblScriptNote: TLabel;
    scriptFileName: TFileNameEdit;
    lblFileName: TLabel;
    scriptEdit: TSynEdit;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
  private
    { private declarations }
  public
    { public declarations }
    function GetTitle: String; override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

implementation

{$R *.lfm}

{ TTiPhoneBuildScriptEditor }

function TTiPhoneBuildScriptEditor.GetTitle: String;
begin
  Result:='Xcode Build Script';
end;

class function TTiPhoneBuildScriptEditor.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TiPhoneEnvironmentOptions;
end;

procedure TTiPhoneBuildScriptEditor.Setup(ADialog: TAbstractOptionsEditorDialog
  );
begin

end;

procedure TTiPhoneBuildScriptEditor.ReadSettings(AOptions: TAbstractIDEOptions);
var
  opt: TiPhoneEnvironmentOptions;
begin
  if not (AOptions is TiPhoneEnvironmentOptions) then Exit;
  opt:=TiPhoneEnvironmentOptions(AOptions);

  scriptFileName.FileName:=opt.ScriptTemplate;
  if FileExists(opt.ScriptTemplate) then begin
    scriptEdit.Lines.LoadFromFile(opt.ScriptTemplate)
  end else
    scriptEdit.Text:='';
end;

procedure TTiPhoneBuildScriptEditor.WriteSettings(AOptions: TAbstractIDEOptions
  );
begin

end;

end.

