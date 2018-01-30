(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StReg.pas 4.04                              *}
{*********************************************************}
{* SysTools: Component Registration Unit                 *}
{*********************************************************}

//{$I StDefine.inc}

{$R stregwin.res}

unit StRegWin;

interface

uses
  Classes,
{$IFDEF FPC}
  PropEdits
{$ELSE}
 {$IFDEF VERSION6}
  DesignIntf,
  DesignEditorsM
 {$ELSE}
  DsgnIntfM
 {$ENDIF}
{$ENDIF}
  ;

procedure Register;

implementation

uses
  StPropEd,

//  StAbout0,

  { components }
  (*,
  StNetCon,
  StNetMsg,
  StNetPfm,
  *)
  StSpawn,
  StVInfo,
  StWMDCpy,

  {forces these units to be compiled when components are installed}
  {vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv}
  (*
  StFirst,
  StMime,
  StNet,
  StNetApi,
  StNVCont,
  StOStr,
  *)
  StRegIni,
  StSort,
  (*
  StStrW,
  StStrZ,
  *)
  StText,
  { new units in ver 4: }
  StSystem,
  StNTLog,
  { !!! StExpEng unit designed to handle problem with initialization }
  { section in C++Builder; should NOT be included in Registration unit }
  { nor in Run-time package !!! }
  {StExpEng,}
//  StExpLog,
  StGenLog;
  {^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}

procedure Register;
begin
  (*
  RegisterPropertyEditor(TypeInfo(string), TStComponent, 'Version',
                         TStVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStBaseEdit, 'Version',
                         TStVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStBarCode, 'Version',
                         TStVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStPNBarCode, 'Version',
                         TStVersionProperty);
                         *)
  RegisterPropertyEditor(TypeInfo(string), TStVersionInfo, 'FileName',
                         TStFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStSpawnApplication, 'FileName',
                         TStGenericFileNameProperty);

  RegisterComponents('SysTools', [
    {
     TStNetConnection,
     TStNetPerformance,
     TStNetMessage,
     }
     TStVersionInfo,
     TStWMDataCopy,
     TStSpawnApplication,
     TStGeneralLog,
{.$IFNDEF BCB} {!!! problem with initialization section in BCB }
//     TStExceptionLog,
{.$ENDIF}
     TStNTEventLog
  ]);
end;

end.
