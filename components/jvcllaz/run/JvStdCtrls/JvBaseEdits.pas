{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCurrEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  Andreas Hausladen

Lazarus port: Michal Gawrycki

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  (rb) Compare property names with those of TJvSpinEdit, JvValidateEdit, for
       example DecimalPlaces/Decimal, CheckMinValue (name indicates action?
       maybe better: TJvValidateEdit's HasMinValue) etc.
-----------------------------------------------------------------------------}

unit JvBaseEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EditBtn, LMessages, CalcForm, Forms, GroupedEdit;

type

  { TJvEbEdit }

  TJvEbEdit = class(TEbEdit)
  protected
    procedure WMPaste(var Msg: TLMessage); message LM_PASTE;
  end;

  { TJvCustomNumEdit }

  TJvCustomNumEdit = class(TCustomEditButton)
  private
    FFocused: Boolean;
    FValue: Double;
    FMinValue: Double;
    FMaxValue: Double;
    FDecimalPlaces: Cardinal;
    FDecimalPlacesAlwaysShown: Boolean; // WAP Added. True means Use 0 instead of # in FormatFloat picture (ie 0.000 versus 0.####). NEW.
    FCheckOnExit: Boolean;
    FZeroEmpty: Boolean;
    FFormatOnEditing: Boolean;
    FFormatting: Boolean;
    FDisplayFormat: string;
    FDecimalPlaceRound: Boolean;
    function GetEditFormat: string; // WAP added.
    procedure SetDecimalPlaceRound(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure SetDisplayFormat(const Value: string);
    function GetDisplayFormat: string;
    procedure SetDecimalPlaces(Value: Cardinal);
    procedure SetDecimalPlacesAlwaysShown( Value:Boolean );
    function GetValue: Double;
    procedure SetValue(AValue: Double);
    function GetAsInteger: Longint;
    procedure SetAsInteger(AValue: Longint);
    procedure SetMaxValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetZeroEmpty(Value: Boolean);
    procedure SetFormatOnEditing(Value: Boolean);
    function GetText: string;
    function IsFormatStored: Boolean;
  protected
    function GetEditorClassType: TGEEditClass; override;
    procedure SetText(const AValue: string); virtual;
    procedure EnabledChanged; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure FontChanged(Sender: TObject); override;
    //Polaris up to protected
    function CheckValue(NewValue: Double; RaiseOnError: Boolean): Double;
    procedure EditChange; override;
    procedure ReformatEditText; dynamic;
    procedure DataChanged; virtual;
    function DefaultDisplayFormat: string; virtual;
    procedure EditKeyPress(var Key: Char); override;
    function IsValidChar(Key: Char): Boolean; virtual;
    function FormatDisplayText(Value: Double): string;
    function GetDisplayText: string; virtual;
    procedure Reset; override;
    procedure CheckRange;
    procedure UpdateData;
    property Formatting: Boolean read FFormatting;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces default 2;
    // WAP Added. True means Use 0 instead of # in FormatFloat picture (ie 0.000 versus 0.####). NEW.
    property DecimalPlacesAlwaysShown: Boolean read FDecimalPlacesAlwaysShown write SetDecimalPlacesAlwaysShown;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat stored IsFormatStored;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property FormatOnEditing: Boolean read FFormatOnEditing write SetFormatOnEditing default False;
    property Text: string read GetText write SetText stored False;
    property MaxLength default 0;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty default True;
    //Polaris
    property DecimalPlaceRound: Boolean read FDecimalPlaceRound write SetDecimalPlaceRound default False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property DisplayText: string read GetDisplayText;
    property Value: Double read GetValue write SetValue;
  end;

  TJvxCurrencyEdit = class(TJvCustomNumEdit)
  protected
    function DefaultDisplayFormat: string; override;
  published
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property Flat;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property Align; //Polaris
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CheckOnExit;
    property Color;
    property Constraints;
    property DecimalPlaceRound; //Polaris
    property DecimalPlaces;
    property DisplayFormat;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property HideSelection;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property ButtonCaption;
    property ButtonCursor;
    property ButtonHint;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property Glyph;
    property NumGlyphs;
    property Images;
    property ImageIndex;
    property ImageWidth;
    property Action;
    property DirectInput;
    property FocusOnButtonClick;
    property BorderSpacing;
    property Layout;
    property Spacing;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnButtonClick;
    property OnEditingDone;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUTF8KeyPress;
    property TextHint;
  end;

  { TJvCustomCalcEdit }

  TJvCustomCalcEdit = class(TJvCustomNumEdit)
  private
    FDialogTitle: String;
    FCalculatorLayout: TCalculatorLayout;
    FDialogPosition: TPosition;
    FDialogLeft: Integer;
    FDialogTop: Integer;
    FOnAcceptValue: TAcceptValueEvent;
    function TitleStored: boolean;
  protected
    procedure AcceptValue(ANewValue: Double); virtual;
    function GetDefaultGlyphName: string; override;
    procedure ButtonClick; override;
    property CalculatorLayout : TCalculatorLayout read FCalculatorLayout write FCalculatorLayout;
    property DialogTitle : String read FDialogTitle write FDialogTitle stored TitleStored;
    property DialogPosition: TPosition read FDialogPosition write FDialogPosition default poScreenCenter;
    property DialogTop: Integer read FDialogTop write FDialogTop;
    property DialogLeft: Integer read FDialogLeft write FDialogLeft;
    property OnAcceptValue: TAcceptValueEvent read FOnAcceptValue write FOnAcceptValue;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RunDialog; virtual;
  end;

  TJvCalcEdit = class(TJvCustomCalcEdit)
  published
    property CalculatorLayout;
    property OnAcceptValue;
    property DialogTitle;
    property DialogPosition;
    property DialogTop;
    property DialogLeft;
    property ButtonCaption;
    property ButtonCursor;
    property ButtonHint;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property Constraints;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property Glyph;
    property NumGlyphs;
    property Images;
    property ImageIndex;
    property ImageWidth;
    property Flat;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property Action;
    property Align; //Polaris
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CheckOnExit;
    property Color;
    property DecimalPlaceRound; //Polaris
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property FocusOnButtonClick;
    property HideSelection;
    property BorderSpacing;
    property Layout;
    property Spacing;
    property Anchors;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property DecimalPlacesAlwaysShown;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnContextPopup;
    property OnStartDrag;
    property OnEditingDone;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUTF8KeyPress;
    property TextHint;
  end;

implementation

uses
  LCLIntf, LCLStrConsts, Math, StrUtils, Controls,
  JvConsts, JvResources, JvJCLUtils;

function IsValidFloat(const Value: string; var RetValue: Double): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not CharInSet(Value[I], [FormatSettings.DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(PChar(Value), RetValue, fvDouble);
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and CharInSet(S[1], SignSymbols);
  if IsSign then
    MinSym := 2
  else
    MinSym := 1;
  I := Pos(FormatSettings.DecimalSeparator, S);
  if I > 0 then
    MaxSym := I - 1;
  I := Pos('E', AnsiUpperCase(S));
  if I > 0 then
    MaxSym := Min(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do
  begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then
    begin
      Group := 0;
      Result := FormatSettings.ThousandSeparator + Result;
    end;
  end;
  if IsSign then
    Result := S[1] + Result;
end;

{ TJvEbEdit }

procedure TJvEbEdit.WMPaste(var Msg: TLMessage);
var
  S: string;
  WasModified: Boolean;
begin
  if Owner is TJvCustomNumEdit then
    with Owner as TJvCustomNumEdit do
    begin
      WasModified := Modified;
      S := EditText;
      try
        inherited;
        UpdateData;
      except
        { Changing EditText sets Modified to false }
        EditText := S;
        Modified := WasModified;
        SelectAll;
        if Edit.CanFocus then
          Edit.SetFocus;
        //DoBeepOnError;
      end;
    end
  else
    inherited;
end;

//=== { TJvCustomNumEdit } ===================================================

constructor TJvCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDecimalPlaceRound := False; // Polaris
  MaxLength := 0;
  Alignment := taRightJustify;
  FDisplayFormat := DefaultDisplayFormat;
  FDecimalPlaces := 2;
  FZeroEmpty := True;
  inherited Text := '';
  { forces update }
  DataChanged;
end;

//Polaris

procedure TJvCustomNumEdit.SetDecimalPlaceRound(Value: Boolean);
begin
  if FDecimalPlaceRound <> Value then
  begin
    FDecimalPlaceRound := Value;
    SetValue(CheckValue(FValue, False));
    Invalidate;
    ReformatEditText;
  end;
end;

function TJvCustomNumEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

function TJvCustomNumEdit.IsFormatStored: Boolean;
begin
  Result := (DisplayFormat <> DefaultDisplayFormat);
end;

function TJvCustomNumEdit.GetEditorClassType: TGEEditClass;
begin
  Result := TJvEbEdit;
end;


{ (rb) This function works NOT the same as JvJCLUtils.TextToValText; for example
       it does NOT remove 'a'..'z' chars.
       Couldn't come up with a good name, so feel free to change it
}
function xTextToValText(const AValue: string): string;
var
  fs: TFormatSettings absolute DefaultFormatSettings;  // less typing...
begin
  Result := Trim(AValue);
  if AnsiChar(fs.DecimalSeparator) <> AnsiChar(fs.ThousandSeparator) then
    Result := DelChars(Result, fs.ThousandSeparator);
  if (fs.DecimalSeparator <> '.') and (fs.ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', fs.DecimalSeparator);
  if (fs.DecimalSeparator <> ',') and (fs.ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', fs.DecimalSeparator);
  if Result = '' then
    Result := '0'
  else
  if Result = '-' then
    Result := '-0';
end;

function TJvCustomNumEdit.IsValidChar(Key: Char): Boolean;
var
  S: string;
  FSelStart, SelStop, DecPos: Integer;
  RetValue: Double;
begin
  Result := False;
  S := EditText;
  GetSel(FSelStart, SelStop);
  Delete(S, FSelStart + 1, SelStop - FSelStart);
  Insert(Key, S, FSelStart + 1);
  S := xTextToValText(S);
  DecPos := Pos(FormatSettings.DecimalSeparator, S);
  if DecPos > 0 then
  begin
    FSelStart := Pos('E', UpperCase(S));
    if FSelStart > DecPos then
      DecPos := FSelStart - DecPos
    else
      DecPos := Length(S) - DecPos;
    if DecPos > Integer(FDecimalPlaces) then
      Exit;
  end;
  Result := IsValidFloat(S, RetValue);
  if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
    Result := False;
end;

procedure TJvCustomNumEdit.EditKeyPress(var Key: Char);
begin
  if CharInSet(Key, ['.', ','] - [FormatSettings.ThousandSeparator]) then
    Key := FormatSettings.DecimalSeparator;
  inherited KeyPress(Key);
  if (Key >= #32) and not IsValidChar(Key) then
  begin
    //DoBeepOnError;
    Key := #0;
  end
  else
  if Key = Esc then
  begin
    Reset;
    Key := #0;
  end;
end;

procedure TJvCustomNumEdit.Reset;
begin
  DataChanged;
  SelectAll;
end;

procedure TJvCustomNumEdit.SetZeroEmpty(Value: Boolean);
begin
  if FZeroEmpty <> Value then
  begin
    FZeroEmpty := Value;
    DataChanged;
  end;
end;

procedure TJvCustomNumEdit.SetDisplayFormat(const Value: string);
begin
  if DisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Invalidate;
    DataChanged;
  end;
end;

function TJvCustomNumEdit.GetDisplayFormat: string;
begin
  Result := FDisplayFormat;
end;

procedure TJvCustomNumEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FFormatting := True;
    try
      DataChanged;
    finally
      FFormatting := False;
    end;
  end;
end;

procedure TJvCustomNumEdit.SetFormatOnEditing(Value: Boolean);
begin
  if FFormatOnEditing <> Value then
  begin
    FFormatOnEditing := Value;
    if FFormatOnEditing then
      inherited Alignment := Alignment
    else
      inherited Alignment := taLeftJustify;
    if FFormatOnEditing and FFocused then
      ReformatEditText
    else
    if FFocused then
    begin
      UpdateData;
      DataChanged;
    end;
  end;
end;

procedure TJvCustomNumEdit.SetDecimalPlaces(Value: Cardinal);
begin
  if FDecimalPlaces <> Value then
  begin
    FDecimalPlaces := Value;
    // WAP Added. Changes to decimal places formerly did not EditChange
    // FDisplayFormat, which causes both designtime and runtime problems!
    SetDisplayFormat(GetEditFormat);
    SetValue(CheckValue(FValue, False)); // Polaris (?)
    DataChanged;
    Invalidate;
  end;
end;

{WAP added this new property: Switches between using 0.000
     and 0.### as a FormatFloat picture. }
procedure TJvCustomNumEdit.SetDecimalPlacesAlwaysShown( Value:Boolean );
begin
  if FDecimalPlacesAlwaysShown <> Value then
  begin
    FDecimalPlacesAlwaysShown := Value;
    SetDisplayFormat(GetEditFormat); // Redo format picture
    SetValue(CheckValue(FValue, False)); // Polaris (?)
    DataChanged;
    Invalidate;
  end;
end;

function TJvCustomNumEdit.FormatDisplayText(Value: Double): string;
begin
  if DisplayFormat <> '' then
    Result := FormatFloat(DisplayFormat, Value)
  else
    Result := FloatToStr(Value);
end;

function TJvCustomNumEdit.GetDisplayText: string;
begin
  Result := FormatDisplayText(FValue);
end;

procedure TJvCustomNumEdit.Clear;
begin
  Text := '';
end;

{WAP added GetEditFormat, this code used to be ininline inside DataChanged.}
function TJvCustomNumEdit.GetEditFormat: string;
begin
  Result := ',0';  // must put the thousands separator by default to allow direct edit of value (paste for example)
  if FDecimalPlaces > 0 then
    if FDecimalPlacesAlwaysShown then
       Result  := Result + '.' + StringOfChar('0', FDecimalPlaces)
    else
       Result  := Result + '.' + StringOfChar('#', FDecimalPlaces);
end;

procedure TJvCustomNumEdit.DataChanged;
var
  EditFormat: string;
  WasModified: Boolean;
begin
  EditFormat := GetEditFormat;
  { Changing EditText sets Modified to false }
  WasModified := Modified;
  try
    if (FValue = 0.0) and FZeroEmpty then
      EditText := ''
    else
      if FFocused then
        EditText := FormatFloat(EditFormat, CheckValue(FValue, False))
      else
        EditText := GetDisplayText;
  finally
    Modified := WasModified;
  end;
end;

function TJvCustomNumEdit.CheckValue(NewValue: Double;
  RaiseOnError: Boolean): Double;
var
  DP: Integer;
begin
  if FDecimalPlaceRound then
  begin //Polaris
    DP := FDecimalPlaces;
    { (rb) Probably: Round to the nearest, and if two are equally near, away from zero
           Ln, Exp are slow; make more generic (why only this one?), see
           http://www.merlyn.demon.co.uk/pas-chop.htm
    }
    NewValue := Int(NewValue * Exp(DP * Ln(10)) + Sign(NewValue) * 0.50000001) * Exp(-DP * Ln(10));
  end;
  Result := NewValue;
  if FMaxValue <> FMinValue then
  begin
    if FMaxValue > FMinValue then
    begin
      if NewValue < FMinValue then
        Result := FMinValue
      else
      if NewValue > FMaxValue then
        Result := FMaxValue;
    end
    else
    begin
      if FMaxValue = 0 then
      begin
        if NewValue < FMinValue then
          Result := FMinValue;
      end
      else
      if FMinValue = 0 then
      begin
        if NewValue > FMaxValue then
          Result := FMaxValue;
      end;
    end;
    if RaiseOnError and (Result <> NewValue) then
      raise ERangeError.CreateResFmt(@RsEOutOfRangeXFloat,
        [DecimalPlaces, FMinValue, DecimalPlaces, FMaxValue]);
  end;
end;

procedure TJvCustomNumEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValue(StrToFloat(TextToValText(EditText)), True);
end;

procedure TJvCustomNumEdit.UpdateData;
begin
  ValidateEdit;
  FValue := CheckValue(StrToFloat(TextToValText(EditText)), False);
end;

function TJvCustomNumEdit.GetValue: Double;
begin
  if not (csDesigning in ComponentState) then
  try
    UpdateData;
  except
    FValue := FMinValue;
  end;
  Result := FValue;
end;

procedure TJvCustomNumEdit.SetValue(AValue: Double);
begin
  FValue := CheckValue(AValue, False);
  DataChanged;
  Invalidate;
end;

function TJvCustomNumEdit.GetAsInteger: Longint;
begin
  Result := trunc(Value);
end;

procedure TJvCustomNumEdit.SetAsInteger(AValue: Longint);
begin
  SetValue(AValue);
end;

procedure TJvCustomNumEdit.SetMinValue(AValue: Double);
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    Value := FValue;
  end;
end;

procedure TJvCustomNumEdit.SetMaxValue(AValue: Double);
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    Value := FValue;
  end;
end;

function TJvCustomNumEdit.GetText: string;
begin
  Result := inherited Text;
end;

procedure TJvCustomNumEdit.SetText(const AValue: string);
begin
  if not (csReading in ComponentState) then
  begin
    FValue := CheckValue(StrToFloat(TextToValText(AValue)), False);
    DataChanged;
    Invalidate;
  end;
end;

procedure TJvCustomNumEdit.ReformatEditText;
var
  S: string;
  IsEmpty: Boolean;
  OldLen, ASelStart, SelStop: Integer;
  WasModified: Boolean;
begin
  FFormatting := True;
  { Changing Text sets Modified to false }
  WasModified := Modified;
  try
    S := inherited Text;
    OldLen := Length(S);
    IsEmpty := (OldLen = 0) or (S = '-');
    if Edit.HandleAllocated then
      GetSel(ASelStart, SelStop);
    if not IsEmpty then
      S := TextToValText(S);
    S := FormatFloatStr(S, Pos(',', DisplayFormat) > 0);
    inherited Text := S;
    if Edit.HandleAllocated and (GetFocus = Edit.Handle) and
      not (csDesigning in ComponentState) then
    begin
      Inc(ASelStart, Length(S) - OldLen);
      SetSel(ASelStart, ASelStart);
    end;
  finally
    FFormatting := False;
    Modified := WasModified;
  end;
end;

procedure TJvCustomNumEdit.EditChange;
begin
  if not FFormatting then
  begin
    if FFormatOnEditing and FFocused then
      ReformatEditText;
    inherited EditChange;
  end;
end;

procedure TJvCustomNumEdit.DoEnter;
begin
  SetFocused(True);
  if FFormatOnEditing then
    ReformatEditText;
  inherited DoEnter;
end;

procedure TJvCustomNumEdit.DoExit;
begin
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if Edit.CanFocus then
      Edit.SetFocus;
    raise;
  end;
  SetFocused(False);
  SetSel(0, 0);
  inherited DoExit;
end;

procedure TJvCustomNumEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  if not FFocused then
    Invalidate;
end;

procedure TJvCustomNumEdit.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  Invalidate;
end;

//=== { TJvxCurrencyEdit } ===================================================

function TJvxCurrencyEdit.DefaultDisplayFormat: string;
var
  CurrStr: string;
  I: Integer;
  C: Char;
begin
  Result := ',0.' + StringOfChar('0', FormatSettings.CurrencyDecimals);
  CurrStr := '';
  for I := 1 to Length(FormatSettings.CurrencyString) do
  begin
    C := FormatSettings.CurrencyString[I];
    if CharInSet(C, [',', '.']) then
      CurrStr := CurrStr + '''' + C + ''''
    else
      CurrStr := CurrStr + C;
  end;
  if Length(CurrStr) > 0 then
    case FormatSettings.CurrencyFormat of
      0:
        Result := CurrStr + Result; { '$1' }
      1:
        Result := Result + CurrStr; { '1$' }
      2:
        Result := CurrStr + ' ' + Result; { '$ 1' }
      3:
        Result := Result + ' ' + CurrStr; { '1 $' }
    end;
  Result := Format('%s;-%s', [Result, Result]);
end;

//=== { TJvCustomCalcEdit } ==================================================

function TJvCustomCalcEdit.TitleStored: boolean;
begin
  Result := FDialogTitle <> rsCalculator;
end;

procedure TJvCustomCalcEdit.AcceptValue(ANewValue: Double);
begin
  Value := ANewValue;
end;

function TJvCustomCalcEdit.GetDefaultGlyphName: string;
begin
  Result := ResBtnCalculator;
end;

procedure TJvCustomCalcEdit.ButtonClick;
begin
  inherited ButtonClick;
  RunDialog;
  //Do this after the dialog, otherwise it just looks silly
  if FocusOnButtonClick then FocusAndMaybeSelectAll;
end;

constructor TJvCustomCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDialogTitle := rsCalculator;
  FDialogPosition := poScreenCenter;
end;

procedure TJvCustomCalcEdit.RunDialog;
var
  D: Double;
  B: Boolean;
  Dlg: TCalculatorForm;
begin
  D := Value;
  Dlg := CreateCalculatorForm(Self, FCalculatorLayout, 0);
  with Dlg do
    try
      Caption := DialogTitle;
      Value := D;
      Dlg.Top := FDialogTop;
      Dlg.Left := FDialogLeft;
      Dlg.Position := FDialogPosition;
      if (ShowModal = mrOK) then
      begin
        D := Value;
        B := True;
        if Assigned(FOnAcceptValue) then
          FOnAcceptValue(Self, D, B);
        if B then
        begin
          AcceptValue(D);
          Edit.SelectAll;
        end;
      end;
    finally
      Free;
    end;
end;

end.

