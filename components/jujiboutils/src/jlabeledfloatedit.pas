{ JLabeledFloatEdit

  Copyright (C) 2011 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit JLabeledFloatEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics,
  Dialogs, jinputconsts;

type

  { TJLabeledFloatEdit }

  TJLabeledFloatEdit = class(TCustomLabeledEdit)
  private
    fNColor: TColor;
    fPColor: TColor;
    theValue: double;
    fFormat: string;
    fEFormat: string;
    fDecimals: integer;
    function getDecimals: integer;
    function getFormat: string;
    function getValue: double;
    function getCurrentValue: double;
    procedure formatInput;
    procedure setDecimals(const AValue: integer);
    procedure setFormat(const AValue: string);
    function scaleTo(const AValue: double; const NDecimals: integer): double;
    function IsValidFloat(const Value: string): boolean;
    procedure setNegativeColor(AValue: TColor);
    procedure setValue(const AValue: double);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentValue: double read getCurrentValue;
  published
    property DisplayFormat: string read getFormat write setFormat;
    property EditFormat: string read fEFormat write fEFormat;
    property Decimals: integer read getDecimals write setDecimals;
    property Value: double read getValue write setValue;
    property NegativeColor: TColor read fNColor write setNegativeColor;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property EditLabel;
    property Enabled;
    property Font;
    property LabelPosition;
    property LabelSpacing;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
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
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  {$I jlabeledfloatedit_icon.lrs}
  RegisterComponents('Jujibo', [TJLabeledFloatEdit]);
end;

function TJLabeledFloatEdit.getDecimals: integer;
begin
  Result := fDecimals;
end;

function TJLabeledFloatEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJLabeledFloatEdit.getValue: double;
begin
  Result := theValue;
end;

function TJLabeledFloatEdit.getCurrentValue: double;
begin
  Result := StrToFloatDef(Text, Value);
end;

procedure TJLabeledFloatEdit.formatInput;
begin
  if Font.Color <> fNColor then
    fPColor := Font.Color;      // store original font color
  Caption := FormatFloat(DisplayFormat, theValue);
  if theValue < 0 then
    font.Color := fNColor
  else
    font.Color := fPColor;
end;

procedure TJLabeledFloatEdit.setDecimals(const AValue: integer);
begin
  if (AValue >= 0) and (AValue < 12) then
    fDecimals := AValue;
end;

procedure TJLabeledFloatEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

function TJLabeledFloatEdit.scaleTo(const AValue: double;
  const NDecimals: integer): double;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

function TJLabeledFloatEdit.IsValidFloat(const Value: string): boolean;
begin
  if StrToFloatDef(Value, MaxDouble) = MaxDouble then
    Result := False
  else
    Result := True;
end;

procedure TJLabeledFloatEdit.setNegativeColor(AValue: TColor);
begin
  if fNColor = AValue then
    Exit;
  fNColor := AValue;
  formatInput;
end;

procedure TJLabeledFloatEdit.setValue(const AValue: double);
begin
  if fDecimals > 0 then
    theValue := scaleTo(AValue, fDecimals)
  else
    theValue := AValue;
  formatInput;
end;

procedure TJLabeledFloatEdit.DoEnter;
begin
  inherited DoEnter;
  if ReadOnly then
    exit;
  if EditFormat <> '' then
    Text := FormatFloat(EditFormat, theValue)
  else
    Text := FloatToStr(theValue);
  SelectAll;
end;

procedure TJLabeledFloatEdit.DoExit;
begin
  inherited DoExit;
  if ReadOnly then
    exit;
  if IsValidFloat(Text) then
    theValue := StrToFloat(Text)
  else
  begin
    ShowMessage(Format(SInvalidNumber, [Text]));
    SetFocus;
  end;
  if fDecimals > 0 then
    theValue := scaleTo(theValue, fDecimals);
  formatInput;
end;

procedure TJLabeledFloatEdit.KeyPress(var Key: char);
begin
  if (Key in ['.', ',']) then
    Key := DefaultFormatSettings.Decimalseparator;
  if (key = DefaultFormatSettings.DecimalSeparator) and (Pos(key, Text) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator,
    '+', '-', #8, #9]) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJLabeledFloatEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  fEFormat := '';
  fFormat := '#,0.00';
  fDecimals := 2;
  fPColor := Font.Color;
  fNColor := Font.Color;
  formatInput;
end;

destructor TJLabeledFloatEdit.Destroy;
begin
  inherited Destroy;
end;

end.
