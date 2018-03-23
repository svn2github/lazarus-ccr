{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThemes.PAS, released on 2003-09-25

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
All Rights Reserved.

Contributors:

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThemes;

{$mode objfpc}{$H+}
{$DEFINE JVCLThemesEnabled}

{$DEFINE COMPILER7_UP}

(*
{$I jvcl.inc}
{$IFDEF JVCLThemesEnabled}
{$I windowsonly.inc}
{$ENDIF JVCLThemesEnabled}
*)

interface

uses
{
  Windows, Messages, CommCtrl,
  }
  Types, SysUtils, Classes, Contnrs,
  {$IFDEF JVCLThemesEnabled}
  Themes, UxTheme,
  {$ENDIF JVCLThemesEnabled}
  Controls, Forms, Graphics, Buttons;

(************************************** NOT CONVERTED ***
const
 // Add a message handler to a component that is themed by the ThemeManager but
 // should not be themed.
  CM_DENYSUBCLASSING = CM_BASE + 2000; // from ThemeMgr.pas

type
  TCMDenySubClassing = TMessage;

{$IFDEF JVCLThemesEnabled}

{$IFNDEF COMPILER16_UP}
type
  TElementSize = (esMinimum, esActual, esStretch);
{$ELSE}
  esMinimum = TElementSize.esStretch;
  esActual = TElementSize.esActual;
  esStretch = TElementSize.esStretch;
{$ENDIF ~COMPILER16_UP}
************)
type
  TThemeServicesEx = class(TThemeServices)
    (*************** NOT CONVERTED ***
  {$IFNDEF COMPILER16_UP}
  private
    function DoGetElementSize(DC: HDC; Details: TThemedElementDetails; Rect: PRect;
      ElementSize: TElementSize; out Size: TSize): Boolean;
  {$ENDIF ~COMPILER16_UP}
  public
    {$IFNDEF COMPILER7_UP}
    procedure ApplyThemeChange;
    {$ENDIF ~COMPILER7_UP}
    {$IFNDEF COMPILER16_UP}
    function GetElementContentRect(DC: HDC; Details: TThemedElementDetails;
      const BoundingRect: TRect; out AContentRect: TRect): Boolean;
    function GetElementSize(DC: HDC; Details: TThemedElementDetails; ElementSize: TElementSize;
      out Size: TSize): Boolean; overload;
    function GetElementSize(DC: HDC; Details: TThemedElementDetails; const Rect: TRect;
      ElementSize: TElementSize; out Size: TSize): Boolean; overload;
    function IsSystemStyle: Boolean;
    function Enabled: Boolean;
    function Available: Boolean;
    function GetSystemColor(Color: TColor): TColor;
    {$ENDIF ~COMPILER16_UP}
    ****************)
  end;

function ThemeServices: TThemeServicesEx;
function StyleServices: TThemeServicesEx;

(******************* NOT CONVERTED
{ PaintControlBorder paints the themed border for WinControls only when they
  have the WS_EX_CLIENTEDGE. }
procedure PaintControlBorder(Control: TWinControl);

{ DrawThemedBorder draws a teEditTextNormal element (border) to the DC. It uses
  the Control's BoundsRect. DrawThemedBorder forces border painting. }
procedure DrawThemedBorder(Control: TControl);

{$ENDIF JVCLThemesEnabled}
**********************)

type
  TJvThemeStyle = TControlStyle;
{
  Instead of the ControlStyle property you should use the following functions:

    ControlStyle := ControlStyle + [csXxx]; -> IncludeThemeStyle(Self, [csXxx]);
    ControlStyle := ControlStyle - [csXxx]; -> ExcludeThemeStyle(Self, [csXxx]);
    if csXxx in ControlStyle then           -> if csXxx in GetThemeStyle(Self) then

}
procedure IncludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
procedure ExcludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
function GetThemeStyle(Control: TControl): TJvThemeStyle;

(***************************** NOT CONVERTED ***
{ DrawThemedBackground fills R with Canvas.Brush.Color/Color. If the control uses
  csParentBackground and the color is that of it's parent the Rect is not filled
  because then it is done by the JvThemes/VCL7. }
procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; NeedsParentBackground: Boolean = True); overload;
procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; Color: TColor; NeedsParentBackground: Boolean = True); overload;
procedure DrawThemedBackground(Control: TControl; DC: HDC; const R: TRect;
  Brush: HBRUSH; NeedsParentBackground: Boolean = True); overload;

{ DrawThemesFrameControl draws a themed frame control when theming is enabled. }
function DrawThemedFrameControl(DC: HDC; const Rect: TRect; uType, uState: UINT): BOOL;


{ PerformEraseBackground sends a WM_ERASEBKGND message to the Control's parent. }
procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint); overload;
procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint; const R: TRect); overload;
procedure PerformEraseBackground(Control: TControl; DC: HDC); overload;
procedure PerformEraseBackground(Control: TControl; DC: HDC; const R: TRect); overload;


{ DrawThemedButtonFace draws a themed button when theming is enabled. }
function DrawThemedButtonFace(Control: TControl; Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; Style: TButtonStyle; IsRounded, IsDown,
  IsFocused, IsHot: Boolean): TRect;

{ IsMouseOver returns True if the mouse is over the control. }
function IsMouseOver(Control: TControl): Boolean;

// ~COMPILER7_UP: These functions are helpers for Delphi 6 that doesn't have the csParentPackground flag.
{ GetParentBackground returns True if the Control has the csParentPackground
  ControlStyle }
function GetParentBackground(Control: TWinControl): Boolean;
{ SetParentBackground sets the Control's csParentPackground ControlStyle }
procedure SetParentBackground(Control: TWinControl; Value: Boolean);

{ GetGlassPaintFlag returns True if csGlassPaint in ControlState }
function GetGlassPaintFlag(AControl: TControl): Boolean;
{ ControlInGlassPaint returns True if the Control is painted on a glass area }
function ControlInGlassPaint(AControl: TControl): Boolean;
{ DrawGlassableText paints text to a device context with support of PaintOnGlass }
procedure DrawGlassableText(DC: HDC; const Text: string; var TextRect: TRect; TextFlags: Cardinal;
  PaintOnGlass: Boolean = False);
{ DrawGlassableImageList paint a transparent imagelist image to the canvas with
  support of PaintOnGlass }
procedure DrawGlassableImageList(ImageList: HIMAGELIST; Index: Integer; Dest: HDC; X, Y: Integer;
  Style: UINT; PaintOnGlass: Boolean = False);

******************)

implementation
(*
uses
{$IFNDEF COMPILER10_UP}
  JclSysUtils,
{$ENDIF ~COMPILER10_UP}
  JclSysInfo;
  *)

(************************ NOT CONVERTED ***
type
  TWinControlThemeInfo = class(TWinControl)
  public
    property Color;
  end;

procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; NeedsParentBackground: Boolean = True);
begin
  DrawThemedBackground(Control, Canvas, R, Canvas.Brush.Color, NeedsParentBackground);
end;

procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; Color: TColor; NeedsParentBackground: Boolean = True);
var
  Cl: TColor;
begin
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled and
     (Control.Parent <> nil) and
     ((Color = TWinControlThemeInfo(Control.Parent).Color) or
      (ColorToRGB(Color) = ColorToRGB(TWinControlThemeInfo(Control.Parent).Color))) and
     (not NeedsParentBackground or (csParentBackground in GetThemeStyle(Control))) then
  begin
    if Control is TWinControl then
    begin
      if TWinControl(Control).DoubleBuffered then
        PerformEraseBackground(Control, Canvas.Handle, R)
      else
        StyleServices.DrawParentBackground(TWinControl(Control).Handle, Canvas.Handle, nil,
          False, @R);
    end
    else
      PerformEraseBackground(Control, Canvas.Handle, R)
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    {$IFDEF JVCLStylesEnabled}
    if StyleServices.Enabled and TStyleManager.IsCustomStyleActive then
      Color := StyleServices.GetSystemColor(Color);
    {$ENDIF JVCLStylesEnabled}
    Cl := Canvas.Brush.Color;
    if Cl <> Color then
      Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
    if Cl <> Canvas.Brush.Color then
      Canvas.Brush.Color := Cl;
  end;
end;

procedure DrawThemedBackground(Control: TControl; DC: HDC; const R: TRect;
  Brush: HBRUSH; NeedsParentBackground: Boolean = True);
{$IFDEF JVCLThemesEnabled}
var
  LogBrush: TLogBrush;
{$ENDIF JVCLThemesEnabled}
begin
  {$IFDEF JVCLThemesEnabled}
  GetObject(Brush, SizeOf(LogBrush), @LogBrush);
  if StyleServices.Enabled and
     (Control.Parent <> nil) and
     (LogBrush.lbColor = Cardinal(ColorToRGB(TWinControlThemeInfo(Control.Parent).Color))) and
     (not NeedsParentBackground or (csParentBackground in GetThemeStyle(Control))) then
  begin
    if Control is TWinControl then
    begin
      if TWinControl(Control).DoubleBuffered then
        PerformEraseBackground(Control, DC, R)
      else
        StyleServices.DrawParentBackground(TWinControl(Control).Handle, DC, nil, False, @R);
    end
    else
      PerformEraseBackground(Control, DC, R)
  end
  else
  {$ENDIF JVCLThemesEnabled}
    FillRect(DC, R, Brush);
end;

function DrawThemedFrameControl(DC: HDC; const Rect: TRect; uType, uState: UINT): BOOL;
{$IFDEF JVCLThemesEnabled}
const
  Mask = $00FF;
var
  Btn: TThemedButton;
  ComboBox: TThemedComboBox;
  ScrollBar: TThemedScrollBar;
  R: TRect;
  Details: TThemedElementDetails;
{$ENDIF JVCLThemesEnabled}
begin
  Result := False;
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    R := Rect;
    case uType of
      DFC_BUTTON:
        case uState and Mask of
          DFCS_BUTTONPUSH:
            begin
              if uState and (DFCS_TRANSPARENT or DFCS_FLAT) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbPushButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbPushButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbPushButtonHot
                else
                if uState and DFCS_MONO <> 0 then
                  Btn := tbPushButtonDefaulted
                else
                  Btn := tbPushButtonNormal;

                Details := StyleServices.GetElementDetails(Btn);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            end;
          DFCS_BUTTONCHECK:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbCheckBoxCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbCheckBoxCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbCheckBoxCheckedHot
                else
                  Btn := tbCheckBoxCheckedNormal;
              end
              else
              if uState and DFCS_MONO <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbCheckBoxMixedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbCheckBoxMixedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbCheckBoxMixedHot
                else
                  Btn := tbCheckBoxMixedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbCheckBoxUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbCheckBoxUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbCheckBoxUncheckedHot
                else
                  Btn := tbCheckBoxUncheckedNormal;
              end;
              Details := StyleServices.GetElementDetails(Btn);
              StyleServices.DrawElement(DC, Details, R);
              Result := True;
            end;
          DFCS_BUTTONRADIO:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbRadioButtonCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbRadioButtonCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbRadioButtonCheckedHot
                else
                  Btn := tbRadioButtonCheckedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbRadioButtonUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbRadioButtonUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbRadioButtonUncheckedHot
                else
                  Btn := tbRadioButtonUncheckedNormal;
              end;
              Details := StyleServices.GetElementDetails(Btn);
              StyleServices.DrawElement(DC, Details, R);
              Result := True;
            end;
        end;
      DFC_SCROLL:
        begin
          case uState and Mask of
            DFCS_SCROLLCOMBOBOX:
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ComboBox := tcDropDownButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ComboBox := tcDropDownButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ComboBox := tcDropDownButtonHot
                else
                  ComboBox := tcDropDownButtonNormal;

                Details := StyleServices.GetElementDetails(ComboBox);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLUP:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnUpDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnUpPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnUpHot
                else
                  ScrollBar := tsArrowBtnUpNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLDOWN:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnDownDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnDownPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnDownHot
                else
                  ScrollBar := tsArrowBtnDownNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLLEFT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnLeftDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnLeftPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnLeftHot
                else
                  ScrollBar := tsArrowBtnLeftNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLRIGHT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnRightDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnRightPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnRightHot
                else
                  ScrollBar := tsArrowBtnRightNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
          end;
        end;
    end;
  end;
  {$ENDIF JVCLThemesEnabled}

  if not Result then
    Result := DrawFrameControl(DC, Rect, uType, uState);
end;

function IsInvalidRect(const R: TRect): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := (R.Left = MaxInt) and (R.Top = MaxInt) and (R.Right = MaxInt) and (R.Bottom = MaxInt);
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint);
begin
  PerformEraseBackground(Control, DC, Offset, Rect(MaxInt, MaxInt, MaxInt, MaxInt));
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint; const R: TRect);
var
  WindowOrg: TPoint;
  OrgRgn, Rgn: THandle;
  {$IFDEF COMPILER16_UP}
  OldPen: HPEN;
  OldBrush: HBRUSH;
  OldFont: HFONT;
  OldTextColor: TColorRef;
  OldBkMode: Integer;
  {$ENDIF COMPILER16_UP}
begin
  if Control.Parent <> nil then
  begin
    if (Offset.X <> 0) and (Offset.Y <> 0) then
    begin
      GetWindowOrgEx(DC, WindowOrg);
      if Control is TGraphicControl then
        SetWindowOrgEx(DC, -Offset.X, -Offset.Y, nil)
      else
        SetWindowOrgEx(DC, WindowOrg.X + Offset.X, WindowOrg.Y + Offset.Y, nil);
    end;

    OrgRgn := 0;
    if not IsInvalidRect(R) then
    begin
      OrgRgn := CreateRectRgn(0, 0, 1, 1);
      if GetClipRgn(DC, OrgRgn) = 0 then
      begin
        DeleteObject(OrgRgn);
        OrgRgn := 0;
      end;
      Rgn := CreateRectRgnIndirect(R);
      SelectClipRgn(DC, Rgn);
      DeleteObject(Rgn);
    end;

    try
      {$IFDEF COMPILER16_UP}
      // Delphi XE2's Style-Engine has a bug in the TStyleHook.WMEraseBkgnd that replaces the
      // selected GDI objects with the TCanvas default objects ("System" font, ...).
      // We need to repair the damage in order to have the same behavior of the native theming API.
      // General rule for WM_ERASEBKGND: Return the DC in the state in that it was when the function
      // was called.
      OldPen := 0;
      OldBrush := 0;
      OldFont := 0;
      OldTextColor := 0;
      OldBkMode := 0;
      if StyleServices.Enabled and not StyleServices.IsSystemStyle then
      begin
        OldPen := GetCurrentObject(DC, OBJ_PEN);
        OldBrush := GetCurrentObject(DC, OBJ_BRUSH);
        OldFont := GetCurrentObject(DC, OBJ_FONT);
        OldTextColor := GetTextColor(DC);
        OldBkMode := GetBkMode(DC);
      end;
      {$ENDIF COMPILER16_UP}
      Control.Parent.Perform(WM_ERASEBKGND, DC, DC); // force redraw
      {$IFDEF COMPILER16_UP}
      if StyleServices.Enabled and not StyleServices.IsSystemStyle then
      begin
        if GetCurrentObject(DC, OBJ_PEN) <> OldPen then
          SelectObject(DC, OldPen);
        if GetCurrentObject(DC, OBJ_BRUSH) <> OldBrush then
          SelectObject(DC, OldBrush);
        if GetCurrentObject(DC, OBJ_FONT) <> OldFont then
          SelectObject(DC, OldFont);
        if GetTextColor(DC) <> OldTextColor then
          SetTextColor(DC, OldTextColor);
        if GetBkMode(DC) <> OldBkMode then
          SetBkMode(DC, OldBkMode);
      end;
      {$ENDIF COMPILER16_UP}
    finally
      if (Offset.X <> 0) and (Offset.Y <> 0) then
        SetWindowOrgEx(DC, WindowOrg.X, WindowOrg.Y, nil);

      if OrgRgn <> 0 then
      begin
        SelectClipRgn(DC, OrgRgn);
        DeleteObject(OrgRgn);
      end;
    end;
  end;
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC);
begin
  PerformEraseBackground(Control, DC, Point(Control.Left, Control.Top));
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; const R: TRect);
begin
  PerformEraseBackground(Control, DC, Point(Control.Left, Control.Top), R);
end;

function DrawThemedButtonFace(Control: TControl; Canvas: TCanvas;
  const Client: TRect; BevelWidth: Integer; Style: TButtonStyle;
  IsRounded, IsDown, IsFocused, IsHot: Boolean): TRect;
{$IFDEF JVCLThemesEnabled}
var
  Btn: TThemedButton;
  Details: TThemedElementDetails;
{$ENDIF JVCLThemesEnabled}
begin
  {$IFDEF JVCLThemesEnabled}
  if (Style <> bsWin31) and StyleServices.Enabled then
  begin
    Result := Client;

    if IsDown then
      Btn := tbPushButtonPressed
    else
    if IsFocused then
      Btn := tbPushButtonDefaulted
    else
    if IsHot then
      Btn := tbPushButtonHot
    else
      Btn := tbPushButtonNormal;

    Details := StyleServices.GetElementDetails(Btn);
    StyleServices.DrawElement(Canvas.Handle, Details, Result);
    StyleServices.GetElementContentRect(Canvas.Handle, Details, Client, Result);

    if IsFocused then
      DrawFocusRect(Canvas.Handle, Result);

    InflateRect(Result, -BevelWidth, -BevelWidth);
  end
  else
  {$ENDIF JVCLThemesEnabled}
    Result := DrawButtonFace(Canvas, Client, BevelWidth, Style, IsRounded, IsDown, IsFocused);
end;

function IsMouseOver(Control: TControl): Boolean;
var
  Pt: TPoint;
begin
  Pt := Control.ScreenToClient(Mouse.CursorPos);
  Result := PtInRect(Control.ClientRect, Pt);
end;

function GetParentBackground(Control: TWinControl): Boolean;
begin
  Result := csParentBackground in GetThemeStyle(Control);
end;

procedure SetParentBackground(Control: TWinControl; Value: Boolean);
begin
  if Value <> GetParentBackground(Control) then
  begin
    if Value then
      IncludeThemeStyle(Control, [csParentBackground])
    else
      ExcludeThemeStyle(Control, [csParentBackground]);
    Control.Invalidate;
  end;
end;

function GetGlassPaintFlag(AControl: TControl): Boolean;
{$IFDEF COMPILER11}
var
  Form: TCustomForm;
{$ENDIF COMPILER11}
begin
  {$IFDEF COMPILER12_UP}
  Result := csGlassPaint in AControl.ControlState;
  {$ELSE}
  Result := False;
  {$IFDEF COMPILER11}
  Form := GetParentForm(AControl);
  if (Form <> nil) and Form.GlassFrame.Enabled then
    Result := Form.GlassFrame.IntersectsControl(AControl);
  {$ENDIF COMPILER11}
  {$ENDIF COMPILER12_UP}
end;

function ControlInGlassPaint(AControl: TControl): Boolean;
{$IFDEF COMPILER11_UP}
var
  Parent: TWinControl;
{$ENDIF COMPILER11_UP}
begin
  {$IFDEF COMPILER11_UP}
  Result := GetGlassPaintFlag(AControl);
  if Result then
  begin
    Parent := AControl.Parent;
    while (Parent <> nil) and not Parent.DoubleBuffered and not (Parent is TCustomForm) do
      Parent := Parent.Parent;
    Result := (Parent = nil) or not Parent.DoubleBuffered or (Parent is TCustomForm);
  end;
  {$ELSE}
  Result := False;
  {$ENDIF COMPILER11_UP}
end;

procedure DrawGlassableText(DC: HDC; const Text: string; var TextRect: TRect; TextFlags: Cardinal;
  PaintOnGlass: Boolean = False);
{$IFDEF COMPILER11_UP}
var
  Options: TDTTOpts;
  {$IFDEF COMPILER11}
  S: WideString;
  {$ENDIF COMPILER11}
{$ENDIF COMPILER11_UP}
begin
  {$IFDEF COMPILER11_UP}
  if StyleServices.Enabled and JclCheckWinVersion(6, 0) then
  begin
    FillChar(Options, SizeOf(Options), 0);
    Options.dwSize := SizeOf(Options);
    if TextFlags and DT_CALCRECT <> 0 then
      Options.dwFlags := Options.dwFlags or DTT_CALCRECT;
    if PaintOnGlass then
      Options.dwFlags := Options.dwFlags or DTT_COMPOSITED;
    Options.dwFlags := Options.dwFlags or DTT_TEXTCOLOR;
    Options.crText := GetTextColor(DC);

    {$IFDEF COMPILER16_UP}
    if not StyleServices.IsSystemStyle then
    begin
      // The Style engine doesn't have DrawThemeTextEx support
      {$WARNINGS OFF} // ignore "deprecated" warning
      StyleServices.DrawText(DC, StyleServices.GetElementDetails(tbPushButtonNormal), Text, TextRect, TextFlags, 0);
      {$WARNINGS ON}
      Exit;
    end
    else
    {$ENDIF}
    begin
      {$IFDEF COMPILER12_UP}
      with ThemeServices do
        if DrawThemeTextEx(Theme[teToolBar], DC, TP_BUTTON, TS_NORMAL, PWideChar(Text), Length(Text),
                           TextFlags, TextRect, Options) <> E_NOTIMPL then
          Exit;
      {$ELSE}
      S := Text;
      with ThemeServices do
        if DrawThemeTextEx(Theme[teToolBar], DC, TP_BUTTON, TS_NORMAL, PWideChar(S), Length(S),
                           TextFlags, @TextRect, Options) <> E_NOTIMPL then
          Exit;
      {$ENDIF COMPILER12_UP}
    end;
  end;
  {$ENDIF COMPILER11_UP}
  Windows.DrawText(DC, PChar(Text), Length(Text), TextRect, TextFlags);
end;

procedure DrawGlassableImageList(ImageList: HIMAGELIST; Index: Integer; Dest: HDC; X, Y: Integer;
  Style: UINT; PaintOnGlass: Boolean = False);
{$IFDEF COMPILER11_UP}
var
  PaintBuffer: HPAINTBUFFER;
  R: TRect;
  MemDC, MaskDC: HDC;
  CX, CY, XX, YY: Integer;
  MaskBmp: TBitmap;
{$ENDIF COMPILER11_UP}
begin
  {$IFDEF COMPILER11_UP}
  if PaintOnGlass and JclCheckWinVersion(6, 0) then
  begin
    { TODO : Not working correctly on a JvSpeedButton. But it works if used direcly on
             a sheet of glass. Some optimizations could be done. }

    ImageList_GetIconSize(ImageList, CX, CY);
    R := Rect(X, Y, X + CX, Y + CY);

    PaintBuffer := BeginBufferedPaint(Dest, R, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      ImageList_Draw(ImageList, Index, MemDC, X, Y, Style);
      BufferedPaintMakeOpaque(PaintBuffer, @R);

      MaskBmp := TBitmap.Create;
      try
        MaskBmp.Width := CX;
        MaskBmp.Height := CY;
        MaskDC := MaskBmp.Canvas.Handle;
        ImageList_Draw(ImageList, Index, MaskDC, 0, 0, ILD_MASK);
        for YY := 0 to CY - 1 do
          for XX := 0 to CX - 1 do
            if GetPixel(MaskDC, XX, YY) <> 0 then
            begin
              R := Rect(X + XX, Y + YY, X + XX + 1, Y + YY + 1);
              BufferedPaintSetAlpha(PaintBuffer, @R, 0);
              //SetPixel(MemDC, X + XX, Y + YY, GetPixel(MemDC, X + XX, Y + YY) and $00FFFFFF);
            end;
      finally
        MaskBmp.Free;
      end;
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end
  else
  {$ENDIF COMPILER11_UP}
    ImageList_Draw(ImageList, Index, Dest, X, Y, Style);
end;
*******************)

(************************ NOT CONVERTED ***
{$IFDEF JVCLThemesEnabled}

{$IFNDEF COMPILER7_UP}
procedure TThemeServicesEx.ApplyThemeChange;
begin
  StyleServices.UpdateThemes;
  StyleServices.DoOnThemeChange;
end;
{$ENDIF ~COMPILER7_UP}

{$IFNDEF COMPILER16_UP}
function TThemeServicesEx.GetElementContentRect(DC: HDC; Details: TThemedElementDetails;
  const BoundingRect: TRect; out AContentRect: TRect): Boolean;
begin
  AContentRect := ContentRect(DC, Details, BoundingRect);
  Result := True;
end;

function TThemeServicesEx.DoGetElementSize(DC: HDC; Details: TThemedElementDetails; Rect: PRect;
  ElementSize: TElementSize; out Size: TSize): Boolean;
const
  ElementSizes: array[TElementSize] of TThemeSize = (TS_MIN, TS_TRUE, TS_DRAW);
begin
  Result := GetThemePartSize(Theme[Details.Element], DC, Details.Part, Details.State, Rect,
    ElementSizes[ElementSize], Size) = S_OK;
end;

function TThemeServicesEx.GetElementSize(DC: HDC; Details: TThemedElementDetails; ElementSize: TElementSize;
  out Size: TSize): Boolean;
begin
  Result := DoGetElementSize(DC, Details, nil, ElementSize, Size);
end;

function TThemeServicesEx.GetElementSize(DC: HDC; Details: TThemedElementDetails; const Rect: TRect;
  ElementSize: TElementSize; out Size: TSize): Boolean;
begin
  Result := DoGetElementSize(DC, Details, @Rect, ElementSize, Size);
end;

function TThemeServicesEx.GetSystemColor(Color: TColor): TColor;
begin
  Result := Color;
end;

function TThemeServicesEx.IsSystemStyle: Boolean;
begin
  Result := True;
end;

function TThemeServicesEx.Enabled: Boolean;
begin
  Result := ThemesEnabled;
end;

function TThemeServicesEx.Available: Boolean;
begin
  Result := ThemesAvailable;
end;
{$ENDIF ~COMPILER16_UP}
*******************)

function ThemeServices: TThemeServicesEx;
begin
  Result := TThemeServicesEx(
    {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.{$IFDEF RTL230_UP}StyleServices{$ELSE}ThemeServices{$ENDIF RTL230_UP});
end;

function StyleServices: TThemeServicesEx;
begin
  Result := TThemeServicesEx(
    {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.{$IFDEF RTL230_UP}StyleServices{$ELSE}ThemeServices{$ENDIF RTL230_UP});
end;

(************************ NOT CONVERTED ***
procedure PaintControlBorder(Control: TWinControl);
begin
  StyleServices.PaintBorder(Control, False)
end;

procedure DrawThemedBorder(Control: TControl);
var
  Details: TThemedElementDetails;
  DrawRect: TRect;
  DC: HDC;
  Handle: THandle;
begin
  if Control is TWinControl then
  begin
    Handle := TWinControl(Control).Handle;
    DC := GetWindowDC(Handle);
    GetWindowRect(Handle, DrawRect);
    OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
  end
  else
  begin
    if Control.Parent = nil then
      Exit;
    Handle := Control.Parent.Handle;
    DC := GetDC(Handle);
    DrawRect := Control.BoundsRect;
  end;

  ExcludeClipRect(DC, DrawRect.Left + 2, DrawRect.Top + 2, DrawRect.Right - 2, DrawRect.Bottom - 2);
  Details := StyleServices.GetElementDetails(teEditTextNormal);
  StyleServices.DrawElement(DC, Details, DrawRect);

  ReleaseDC(Handle, DC);
end;
***************************)

type
  TControlAccessProtected = class(TControl);

procedure IncludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
  with TControlAccessProtected(Control) do
    ControlStyle := ControlStyle + (Style * [csNeedsBorderPaint, csParentBackground]);
end;

procedure ExcludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
  with TControlAccessProtected(Control) do
    ControlStyle := ControlStyle - (Style * [csNeedsBorderPaint, csParentBackground]);
end;

function GetThemeStyle(Control: TControl): TJvThemeStyle;
begin
  with TControlAccessProtected(Control) do
    Result := ControlStyle * [csNeedsBorderPaint, csParentBackground];
end;

(********************* NOT CONVERTED ***
{$IFDEF JVCLThemesEnabled}

{$IFNDEF COMPILER10_UP}
type
  PPointer = ^Pointer;

var
  OrgWinControlWMPrintClient: procedure(Instance: TObject; var Msg: TMessage);

procedure FixedWMPrintClient(Instance: TObject; var Msg: TMessage);
var
  IdSave: Integer;
begin
  if Msg.Msg = WM_PRINTCLIENT then
  begin
    IdSave := SaveDC(HDC(Msg.WParam));
    try
      OrgWinControlWMPrintClient(Instance, Msg);
    finally
      RestoreDC(HDC(Msg.WParam), IdSave);
    end;
  end
  else
    OrgWinControlWMPrintClient(Instance, Msg);
end;

function FindWMPrintClient: PPointer;
var
  IdxList: PDynamicIndexList;
  I: Integer;
begin
  IdxList := GetDynamicIndexList(TWinControl);
  for I := 0 to GetDynamicMethodCount(TWinControl) - 1 do
    if IdxList[I] = WM_PRINTCLIENT then
    begin
      Result := @(GetDynamicAddressList(TWinControl)[I]);
      Exit;
    end;
  Result := nil;
end;

procedure InitializeWMPrintClientFix;
var
  NewProc: Pointer;
  Proc: PPointer;
  OldProtect, Dummy: Cardinal;
begin
  Proc := FindWMPrintClient();
  if Proc <> nil then
  begin
    OrgWinControlWMPrintClient := Proc^;
    NewProc := @FixedWMPrintClient;

    if VirtualProtect(Proc, SizeOf(NewProc), PAGE_EXECUTE_READWRITE, OldProtect) then
    try
      Proc^ := NewProc;
    finally
      VirtualProtect(Proc, SizeOf(NewProc), OldProtect, Dummy);
    end;
  end;
end;

procedure FinalizeWMPrintClientFix;
var
  NewProc: Pointer;
  Proc: PPointer;
  OldProtect, Dummy: Cardinal;
begin
  Proc := FindWMPrintClient;
  if Proc <> nil then
  begin
    NewProc := @OrgWinControlWMPrintClient;

    if VirtualProtect(Proc, SizeOf(NewProc), PAGE_EXECUTE_READWRITE, OldProtect) then
    try
      Proc^ := NewProc;
    finally
      VirtualProtect(Proc, SizeOf(NewProc), OldProtect, Dummy);
    end;
  end;
end;
{$ENDIF ~COMPILER10_UP}

{$ENDIF JVCLThemesEnabled}
************)

initialization
  (************** NOT CONVERTED ***
  {$IFDEF JVCLThemesEnabled}
  {$IFNDEF COMPILER10_UP}
  InitializeWMPrintClientFix;
  {$ENDIF ~COMPILER10_UP}
  {$ENDIF JVCLThemesEnabled}
  **********)

finalization
  (*************** NOT CONVERTED ***
  {$IFDEF JVCLThemesEnabled}
  {$IFNDEF COMPILER10_UP}
  FinalizeWMPrintClientFix;
  {$ENDIF ~COMPILER10_UP}
  {$ENDIF JVCLThemesEnabled}
  *************)

end.
