{ TPoweredby Component

  Copyright (C)2014 Gordon Bamber minesadorada@charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit uPoweredby;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows, JwaWindows,{$ENDIF}Classes, Controls, Dialogs,
  Forms, Graphics, LResources, SysUtils,
  ExtCtrls, InterfaceBase, LCLType, LCLVersion, AboutPoweredbyunit;

const
  C_VERSIONSTRING = '1.0.4.0';
  C_WIDGETSET_GTK = 'GTK widget set';
  C_WIDGETSET_GTK2 = 'GTK 2 widget set';
  C_WIDGETSET_GTK3 = 'GTK 3 widget set';
  C_WIDGETSET_WIN = 'Win32/Win64 widget set';
  C_WIDGETSET_WINCE = 'WinCE widget set';
  C_WIDGETSET_CARBON = 'Carbon widget set';
  C_WIDGETSET_QT = 'QT widget set';
  C_WIDGETSET_fpGUI = 'fpGUI widget set';
  C_WIDGETSET_COCOA = 'Cocoa widget set';
  C_WIDGETSET_CUSTOM = 'Custom drawn widget set';
  C_WIDGETSET_OTHER = 'Other gui';

type
  TPoweredby = class(TAboutPoweredBy)
  private
    { Private declarations }
    fPoweredByForm: TForm;
    fVersionString: string;
    fDelayMilliseconds: integer;
    fFadeInMilliseconds: integer;
    fShowOnlyOnce, fAlreadyShown: boolean;
    // Used by Timer to close the PoweredBy form
    procedure ClosePoweredByForm(Sender: TObject);
    // Windows only!
    procedure FadeInPoweredBy(Sender: TObject);
    procedure SetDelayMilliSeconds(AValue: integer);
    function GetWidgetSetString: string;
    function GetFPCTargetInfoString: string;
    function GetInfoLCLVersion: string;
    function GetInfoFPCVersion: string;
    {$IFDEF WINDOWS}
    procedure MakeTransparentWindow(var AForm: TForm; var AImage: TImage);
    {$ENDIF}
  protected
    { Protected declarations }
  public
    { Public declarations }
    // Call the method 'ShowPoweredByForm' to show the shaped window
    procedure ShowPoweredByForm;
    // Called when component is dropped onto a form
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    // Minimum delay=1000msec; Maximum delay=10000msec.  Fade-in time is automatically adjusted
    property DelayMilliSecs: integer read fDelayMilliSeconds
      write SetDelayMilliSeconds default 1000;
    // Call the method 'ShowPoweredByForm' to show the shaped window
    property Version: string read fVersionString;
    // Reports the current WidgetSet
    property InfoWidgetSet: string read GetWidgetSetString;
    // Reports your current Environment
    property InfoFPCTarget: string read GetFPCTargetInfoString;
    // Reports your current Environment
    property InfoFPCVersion: string read GetInfoFPCVersion;
    // Reports your current Environment
    property InfoLCLVersion: string read GetInfoLCLVersion;
    // Useful if you have ShowPoweredByForm in your TForm.Activate() method
    property ShowOnlyOnce: boolean read fShowOnlyOnce write fShowOnlyOnce default False;
  end;

procedure Register;

implementation

uses {$IF (lcl_major > 0) and (lcl_minor > 6)}LCLPlatformDef {$ENDIF};

procedure Register;
begin
  {$I upoweredby_icon.lrs}
  RegisterComponents('Additional', [TPoweredby]);
end;

constructor TPoweredby.Create(AOwner: TComponent);
  // Initialise private vars
begin
  inherited Create(AOwner);
  fVersionString := C_VERSIONSTRING;
  fDelayMilliseconds := 1000;
  fFadeInMilliseconds := 20;
  fAlreadyShown := False;
  fShowOnlyOnce := False;
  // About dialog
  AboutBoxComponentName := 'PoweredBy component';
  AboutBoxWidth := 400;
  //  AboutBoxHeight (integer)
  AboutBoxDescription := 'Component that shows a Powered By graphic.' +
    LineEnding + 'Use method ShowPoweredByForm in your form.create()' +
    LineEnding + 'to use the component';
  AboutBoxBackgroundColor := clWindow;
  AboutBoxFontName := 'Arial';
  AboutBoxFontSize := 10;
  AboutBoxVersion := C_VERSIONSTRING;
  AboutBoxAuthorname := 'Gordon Bamber';
  AboutBoxOrganisation := 'Public Domain';
  AboutBoxAuthorEmail := 'minesadorada@charcodelvalle.com';
  AboutBoxLicenseType := 'MODIFIEDGPL';
end;

function TPoweredby.GetInfoLCLVersion: string;
begin
  Result := lcl_version;
end;

function TPoweredby.GetInfoFPCVersion: string;
begin
    Result:={$I %FPCVERSION%};
end;

function TPoweredby.GetFPCTargetInfoString: string;
begin
    Result := {$I %FPCTARGETCPU%}+' - '+{$I %FPCTARGETOS%};
end;

function priv_GetWidgetSetString: string;
  // This code cannot be a method of TPoweredBy
begin
  case WidgetSet.LCLPlatform of
    lpGtk: Result := C_WIDGETSET_GTK;
    lpGtk2: Result := C_WIDGETSET_GTK2;
    lpWin32: Result := C_WIDGETSET_WIN;
    lpWinCE: Result := C_WIDGETSET_WINCE;
    lpCarbon: Result := C_WIDGETSET_CARBON;
    lpCocoa: Result := C_WIDGETSET_COCOA;
    lpQT: Result := C_WIDGETSET_QT;
    lpfpGUI: Result := C_WIDGETSET_fpGUI;
    // When were these first included in InterfaceBase?
    {$IFDEF FPC_FULLVERSION>24200}
    lpGtk3: Result := C_WIDGETSET_GTK3;
    lpCustomDrawn: Result := C_WIDGETSET_CUSTOM;
    {$ENDIF}
    else
      Result := C_WIDGETSET_OTHER;
  end;
end;

function TPoweredby.GetWidgetSetString: string;
begin
  Result := priv_GetWidgetSetString;
end;

procedure TPoweredby.SetDelayMilliSeconds(AValue: integer);
begin
  if ((fDelayMilliSeconds <> AValue) and (AValue > 0) and (AValue < 11000)) then
  begin
    fDelayMilliseconds := AValue;
    fFadeInMilliseconds := (AValue div 1000) * 20;
  end;

end;

procedure TPoweredby.ClosePoweredByForm(Sender: TObject);
// Called by Timer event in ShowPoweredByForm to close Modal window
// Also the image OnClick event
begin
  fPoweredByForm.Close;
end;

procedure TPoweredby.FadeInPoweredBy(Sender: TObject);
// Use Alphablend property of TForm
begin
  if (fPoweredByForm.AlphaBlendValue < 245) then
    fPoweredByForm.AlphaBlendValue := fPoweredByForm.AlphaBlendValue + 10;
end;

function CanShowRoundedGraphic: boolean;
{
Check the current WidgetSet, and add to the list that can show the rounded graphic
Choices are:
  lpGtk,
  lpGtk2,
  lpGtk3,
  lpWin32,
  lpWinCE,
  lpCarbon,
  lpQT,
  lpfpGUI,
  lpNoGUI,
  lpCocoa,
  lpCustomDrawn
  }
begin
  Result := False;
  case WidgetSet.LCLPlatform of
    lpWin32, lpQT: Result := True;
    else
      Result := False;
  end;

end;

{$IFDEF WINDOWS}
procedure TPoweredby.MakeTransparentWindow(var AForm: TForm; var AImage: TImage);
var
  BlendFunction: TBlendFunction;
  Size: TSize;
  P: TPoint;
  ExStyle: DWORD;
begin
  with AForm do
  begin
    ExStyle := GetWindowLongA(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_LAYERED = 0) then
      SetWindowLong(Handle, GWL_EXSTYLE, ExStyle or WS_EX_LAYERED);
    ClientWidth := AImage.picture.Bitmap.Width;
    ClientHeight := AImage.picture.Bitmap.Height;
    P.x := 0;
    P.y := 0;
    Size.cx := AImage.picture.Bitmap.Width;
    Size.cy := AImage.picture.Bitmap.Height;
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 255;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;
    UpdateLayeredWindow(Handle, 0, nil, @Size, AImage.picture.Bitmap.Canvas.Handle,
      @P, 0, @BlendFunction, ULW_ALPHA);
  end;
end;

{$ENDIF}

procedure TPoweredby.ShowPoweredByForm;

// Graphics are in graphics.lrs
// 1 ) Constructs a new TForm with an image control
// 2 ) Sets a timer to fade it in using the Alphablend property
// 3 ) Sets another timer to close the form

// Note: Windows can fade in a shaped transparent screen
// But some widgetsets (GTK,Carbon) cannot
var
  img_Background: TImage;
  DelayTimer: TTimer;
  FadeInTimer: TTImer;
begin
  // Respect the ShowOnlyOnce property setting
  if ((fShowOnlyOnce = True) and (fAlreadyShown = True)) then
    Exit;

  // Try..Finally so we can be sure resources are Freed
  try
    try
      // Create controls
      fPoweredByForm := TForm.Create(nil);
      fPoweredByForm.AlphaBlend := True;
      fPoweredByForm.AlphaBlendValue := 0;
      img_background := TImage.Create(fPoweredByForm);
      // Delay Timer
      Delaytimer := TTimer.Create(fPoweredByForm);
      delaytimer.Interval := fDelayMilliseconds;
      delaytimer.OnTimer := @ClosePoweredByForm;

      FadeInTimer := TTimer.Create(fPoweredByForm);
      FadeInTimer.Interval := fFadeInMilliseconds;
      FadeInTimer.OnTimer := @FadeInPoweredBy;

      // BackGround image - load from resource
      with img_background do
      begin
        Align := alClient;
        Stretch := True;
        Parent := fPoweredByForm;
        if CanShowRoundedGraphic then
           Picture.LoadFromLazarusResource('powered_by_graphic')
        else
          Picture.LoadFromLazarusResource('linux_powered_by_graphic');
        OnClick := @ClosePoweredByForm;
        SendToBack;
      end;
      // Set form properties
      with fPoweredByForm do
      begin
        position := poScreenCenter;
        borderstyle := bsnone;
        bordericons:=[];
        formstyle := fsSystemStayOnTop;
        OnClick := @ClosePoweredByForm;
        color := clNone;
        Scaled:=True;
        if CanShowRoundedGraphic then
        begin
          MakeTransparentWindow(fPoweredByForm,img_background);
        end
        else
        begin
          // If square graphic, then adjust form size
          Height := img_background.Picture.Height;
          Width := img_background.picture.Width;
        end;
        // Now show the completed form
        delaytimer.Enabled := True;
        FadeInTimer.Enabled := True;
        Application.ProcessMessages;
        ShowModal; // Closed via the Timer event or a user click
        fAlreadyShown := True;
      end;
    except
      On E: Exception do
        raise Exception.CreateFmt('%s Error: %s', [Name, Exception.ClassName]);
    end;
  finally
    FreeAndNil(img_background);
    FreeAndNil(delayTimer);
    FreeAndNil(FadeInTimer);
    FreeAndNil(fPoweredByForm);
  end;
end;

initialization
  // Load graphics as lazarus resources into the component
{$I graphics.lrs}

end.
