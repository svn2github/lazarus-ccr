unit myexamplecontrol;

{
= Example of a generic non-graphical component for Lazarus 1.x
= - example code for various types of properties and events to show and act correctly in the Object Inspector
= - including a custom 'About' dialog property editor
=
= Although this unit will compile as written, the purpose is to provide template code
= for various property types and events that can be copy/pasted into your own component
=
= Example Property types:
=  1) Bitmap
=  2) Font
=  3) Icon
=  4) Stringlist
=  5) Options expandable true/false list
=  6) User type drop-down list
=  7) Simple String
=  8) String with pre and post-processing
=  9) String Array with indexed properties
= 10) String with default value
= 11) Integer with default value
= 12) Overridden (custom) Tag property
= 13) Inherited drop-down list of types
= 14) Custom dialog
= 15) Custom events
= 16) Property hidden from the Object Inspector
= 17) Filename property with custom properties for the OpenDialog
=
= Author: minesadorada@charcodelvalle.com
= Date: May 2014
= License: LGPL
=
}
{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, Controls, Forms, Graphics, SysUtils,
  LCLIntf, LCLType, LResources,
  Dialogs, ExtCtrls, PropEdits, StdCtrls, uMySubComponent;

const
  C_VERSION = '1.2'; // Remember to co-ordinate with the Package manager VersionInfo

  C_ERRORMESSAGE = '%s error:' + LineEnding + '%s';
// Used with Exception.CreateFmt

type
  // Drop-down list in Object Inspector
  tmcType = (mcType1, mcType2, mcType3);

  // Expandable list of Options in Object Inspector
  tOptions = (Opt1, Opt2, Opt3);
  tOptionsFlags = set of tOptions;

  tAboutString = string; // Unique string type used in 'About' property editor
  tFilenameString = string; // Unique string type used in MyFilename property editor

  TSampleEvent = procedure(MyText: string) of object;  // Custom Event type

  TExampleComponent = class(TComponent) // Non-graphical ancestor
  private
    { Private declarations }
    fAboutString: string; // Dummy string for 'About' property
    fFileNameString: tFilenameString; // Filename
    fmcType: tmcType; // Type defined above
    fSimpleString: string; // Direct read and write (no methods)
    fProcessedString: string; // Methods used to read and write the property
    fStringWithDefault: string; // Object Inspector shows default
    fIntegerWithDefault: integer; // Object Inspector shows default
    fTag: string; // Property overrides normal Tag property
    fOptions: tOptionsFlags; // Variable of Set type. * If Opt1 IN fOptions then...
    fIcon: TIcon; // Assigned to Application Icon by default
    fFont: TFont; // Assigned only if MyFont property set
    fBitMap: TBitMap; // Assigned only if MyBitmap property set
    fSizeConstraints: TSizeConstraintsOptions;
    // Built-in set (part of TSizeConstraints object)
    fStringArray: array[0..3] of string;
    // Stores values of String1,String2,String3 and String4 properties
    fStringList: TStrings; // Holds MyStringList property values
    fVersion: string; // Holds read-only version property
    FOnSample: TSampleEvent; // Custom event

    fHiddenString: string; // Hidden property
    fOnChangeHiddenString: TNotifyEvent; // Custom event

    fsubcomponent: TMySubComponent; // A Subcomponent as a property

    { Private methods }

    procedure SetProcessedString(AValue: string);
    function GetProcessedString: string;
    // Read and Write procedures for property ProcessedString

    procedure SetTag(AValue: string); // Write procedure for overridden property 'Tag'

    function GetStringValue(const AIndex: integer): string;
    // Indexed Get Method
    procedure SetStringValue(const AIndex: integer; AValue: string);
    // Indexed Set method

    procedure SetStrings(const AValue: TStrings);
    // Needed to use a TStrings property.  FStringList also needs to be created in the Constructor

    procedure SetFont(const AValue: TFont);
    // Needed to use a Font property. fFont also needs to be created in the Constructor

    procedure SetBitMap(const AValue: TBitmap);
    // Needed to use a BitMap property. fBitMap also needs to be created in the Constructor

    // Sets the HiddenString property
    procedure SetHiddenString(const AValue: string);
  protected
    { Protected declarations }
    // Can be used if you plan to subclass this component
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override; // Constructor must be public
    destructor Destroy; override; // Destructor must be public
    // This public method accesses the Subcomponent's properties
    procedure CallDoSomething; // Calls the subcomponent's DoSomething method
    { Public properties }
    // This section used for properties you don't want to display in the Object Inspector (or array properties)
    property HiddenString: string read fHiddenString write SetHiddenString;
    // Public property hidden from the Object Inspector. * SetHiddenString method can trigger custom event OnChangeHiddenString
  published
    {
    Published declarations (Displayed in the Object Inspector)
    Note: Comments immediately above the property declarations are displayed in the Lazarus Object Inspector lower pane
    }

    // The custom property editor TAboutPropertyEditor is used in the Object Inspector
    property About: tAboutString read fAboutString; // Example of custom 'About' dialog

    // Example of drop-down list in the Object Inspector
    // Default is displayed in different font and is set in Constructor
    property MyTypeList: tmcType read fmcType write fmcType default mcType2;

    // Direct access read + write
    property SimpleString: string read fSimpleString write fSimpleString;

    // Example of using Read and Write methods
    property ProcessedString: string read GetProcessedString write SetProcessedString;

    // Example of pseudo-default property of a string (value is set in Constructor)
    property StringWithDefault: string read fStringWithDefault write fStringWithDefault;

    // Example of an expandable list of booleans in the Object Inspector
    property Options: tOptionsFlags read fOptions write fOptions;

    // Browse for Icon dialog in the Object Inspector. * No need to Create fIcon in constructor
    property MyIcon: TIcon read fIcon write fIcon;

    // Browse for Font dialog in the Object Inspector. * fFont needs to be created in Constructor
    property MyFont: TFont read fFont write SetFont; // * Note the SetFont method

    // * Note the SetBitmap method
    // Browse for Bitmap dialog in the Object Inspector. * fBitmap needs to be created in Constructor
    property MyBitMap: TBitmap read fBitMap write SetBitmap;

    // Showing a built-in list of booleans in the Object Inspector
    property MySizeConstraints: TSizeConstraintsOptions
      read fSizeConstraints write fSizeConstraints;

    // Default is displayed in different font but has to be set to the value 2 in Constructor
    property IntegerWithDefault: integer read fIntegerWithDefault
      write fIntegerWithDefault default 2; // Example of integer default

    // The method is passed the index as a parameter
    // Example of using an indexed read and write method to store strings in an array
    property String1: string index 1 read GetStringValue write SetStringValue;
    // Example of using an indexed read and write method to store strings in an array
    property String2: string index 2 read GetStringValue write SetStringValue;
    // Example of using an indexed read and write method to store strings in an array
    property String3: string index 3 read GetStringValue write SetStringValue;
    // Example of using an indexed read and write method to store strings in an array
    property String4: string index 4 read GetStringValue write SetStringValue;

    // Dialog shows in the Object Inspector. * fStringlist must be Created in the constructor.  * Note the SetStrings override
    // String list example.
    property MyStringList: TStrings read fStringList write SetStrings;

    // Example of a file open dialog property
    property MyFilename: tFilenameString read fFilenameString write fFilenameString;

    // Tag property will now only accept a single alpha character
    // Example of overriding an ancestor (TComponent's) Tag property
    property Tag: string read fTag write SetTag;

    // No need to specify type, read method or write method
    // Example of publishing a public property from an ancestor (TComponent)
    property ComponentState;

    // ReadOnly property
    property Version: string read fVersion;

    // Example of Custom Event that does nothing
    property OnSample: TSampleEvent read FOnSample write FOnSample;

    // This custom event is always triggered by a change in the value of HiddenString property
    property OnChangeHiddenString: TNotifyEvent
      read fOnChangeHiddenString write fOnChangeHiddenString;

    // A subcomponent in the Dependencies
    property SubComponent: TMySubComponent read fsubcomponent write fsubcomponent;
    // Subcomponents need a RegisterPropertyEditor in the Register proc
    // You can Get/Set the values of SubComponent's properties and call public methods
    // 1) In this unit
    // fsubcomponent.Subproperty1:='foo';
    // fsubcomponent.DoSomething;
    // 2) In an app
    // MyCustomControl1.SubComponent.Subproperty1:='foo';
    // MyCustomControl1.SubComponent.DoSomething;
  end;
  // Declaration for the 'About' property editor
  TAboutPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;
  // Declaration for 'MyFilename' property editor
  // (See PropEdits unit for a list of property editors to modify)
  TMyFileNamePropertyEditor = class(TFileNamePropertyEditor)
  public
    // Override the Edit method for total control
    function GetFilter: string; override;
    function GetDialogOptions: TOpenOptions; override;
    function GetDialogTitle: string; override;
  end;

procedure Register; // Must be declared here just before implementation

implementation

procedure Register;
begin
  {$I examplecomponent_icon.lrs}
  // Register this component into the IDE on the 'Additional' component palette
  RegisterComponents('Additional', [TExampleComponent]);

  // Register the custom property editor for the 'About' property
  RegisterPropertyEditor(TypeInfo(tAboutString),
    TExampleComponent, 'About', TAboutPropertyEditor);
  // Register the custom property editor for the 'MyFilename' property
  RegisterPropertyEditor(TypeInfo(tFilenameString),
    TExampleComponent, 'MyFilename', TMyFileNamePropertyEditor);
  // Register a Class-type property editor for any subcomponents
  RegisterPropertyEditor(TypeInfo(TMySubComponent),
    TExampleComponent, 'Subcomponent', TClassPropertyEditor);
  // Note the TypeInfo parameter is made into a unique type
  // (defined earlier in this unit)
  // so that the regular property editors in other components don't get confused
  // You could simply use TypeInfo(String)
end;

constructor TExampleComponent.Create(AOwner: TComponent);
  // Called when form is loaded into the IDE
begin
  inherited Create(AOwner);
  // Initialisation goes here
  // Set any properties to their defaults and intialise objects
  fmcType := mcType2; // because this was the default value specified
  fStringWithDefault := 'Default String'; // Unspecified default
  fIntegerWithDefault := 2; // Default value specified
  fTag := 'A'; // Unspecified default
  fIcon := Application.Icon; // Unspecified default
  fFont := TFont.Create; // Needs to be created here for Object Inspector to show options
  fBitmap := TBitMap.Create;
  // Needs to be created here for Object Inspector to show the bitmap dialog
  fStringList := TStringList.Create;
  // Needs to be created here for Object Inspector to show dialog
  fVersion := C_VERSION;
  // Assign read-only property

  // Use Tsubcomponent as a subcomponent
  fsubcomponent := TMySubComponent.Create(Self);
  fsubcomponent.SetSubComponent(True);  // Tell the IDE to store the modified properties
  fsubcomponent.Name := 'SubComponent1';

end;

destructor TExampleComponent.Destroy;
begin
  // Clean-up code goes here
  // FreeandNil any user-created objects here
  FreeAndNil(fBitmap);
  FreeAndNil(fStringList);
  FreeAndNil(fFont);
  inherited Destroy;
end;


// == START PROPERTY EDITOR CODE ==
procedure TAboutPropertyEditor.Edit;
// Shows a dialog when About property is double-clicked
var
  tAboutForm: TForm;
  OKbutton: TBitBtn;
  lbl_Description: TLabel;
  sz: string;
begin
  // Make up message string
  sz := 'My component for Lazarus' + LineEnding + 'by email@domain.com' +
    LineEnding + LineEnding;
  sz += 'Methods:' + LineEnding;
  sz += 'MyVisualComponent.Method1' + LineEnding;
  sz += 'MyVisualComponent.Method2' + LineEnding;
  sz += LineEnding + 'Version: ' + C_VERSION + LineEnding;
  sz += 'License: LGPL';
  // Create a new dialog
  tAboutForm := TForm.CreateNew(nil);
  try  //.. finally FreeAndNil everything
    with tAboutForm do
    begin
      // Set Form properties
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := 'About My Component';
      formstyle := fsSystemStayOnTop;
      color := clSkyBlue;
      Height := 240;
      Width := 320;
      // Create a BitBtn button
      okbutton := TBitBtn.Create(tAboutForm);
      // Set BitBtn properties
      okbutton.Kind := bkClose;
      okbutton.left := (Width div 2) - okbutton.Width div 2;
      okbutton.top := Height - okbutton.Height - 10;
      okbutton.parent := tAboutForm;
      // Create a label control
      lbl_Description := Tlabel.Create(tAboutForm);
      // Set label properties
      lbl_Description.left := 8;
      lbl_Description.Top := 30;
      lbl_Description.Width := 304;
      lbl_Description.Height := 200;
      lbl_Description.Autosize := False;
      lbl_Description.Alignment := taCenter;
      lbl_Description.Caption := sz;
      lbl_Description.parent := tAboutForm;
      // Display the dialog modally
      ShowModal;
    end;
  finally
    // Free all resources
    FreeAndNil(lbl_Description);
    FreeAndNil(okbutton);
    FreeAndNil(tAboutForm);
  end;
end;

function TAboutPropertyEditor.GetAttributes: TPropertyAttributes;
  // Show the ellipsis
begin
  Result := [paDialog];
end;

function TAboutPropertyEditor.GetValue: string;
  // Override standard string read method
begin
  Result := '(Double-click)';
end;

function TMyFileNamePropertyEditor.GetFilter: string;
begin
  Result := 'All Files|*.*|Bitmaps|*.bmp|JPegs|*.jpg';
end;

function TMyFileNamePropertyEditor.GetDialogOptions: TOpenOptions;
begin
  // To see the full list, drop an OpenDialog onto a form and see the Options property
  Result := [ofFileMustExist, ofPathMustExist];
end;

function TMyFileNamePropertyEditor.GetDialogTitle: string;
begin
  Result := 'My Custom Title';
end;
// == END PROPERTY EDITOR CODE ==

// Public method
procedure TExampleComponent.CallDoSomething;
// You can access the subcomponent's methods and properties
// in this component code
// Note: Changing the subcomponent's property at designtime in the Object Inspector works fine
begin
  // Call the subcomponent's method
  fsubcomponent.DoSomething;
  // Access the Subcomponent's properties (note: not the private member fSubproperty2)
  ShowMessageFmt('Procedure TMyVisualComponent.CallDoSomething: Subproperty2 = %s',
    [fsubcomponent.Subproperty2]);
end;

// == PROPERTY GET/SETS ==
procedure TExampleComponent.SetProcessedString(AValue: string);
begin
  // Check to see if a change is necessary
  // Avalue can be amended before it is assigned to fProcessedString
  if (fProcessedString <> AValue) then
    fProcessedString := UpperCase(AValue);
end;

function TExampleComponent.GetProcessedString: string;
begin
  // Do any error-checking or processing of fProcessedString here.
  Result := fProcessedString;
end;

procedure TExampleComponent.SetTag(AValue: string);
// Overridden Tag property will only accept letters
// If changed via Object Inspector to a non-alpha string, then show custom error message
begin
  if (fTag <> AValue) and (Length(AValue) > 0) then
    if AValue[1] in ['A'..'Z'] + ['a'..'z'] then
      fTag := AValue[1]
    else
    if ComponentState = [csDesigning] then
      raise Exception.CreateFmt(C_ERRORMESSAGE,
        [Name, 'You can only set the tag property to an alpha value']);
  // 'Name' is the Name property of this TComponent instance
end;

function TExampleComponent.GetStringValue(const AIndex: integer): string;
begin
  // Example of an indexed property Get method
  // Retrieve from private string Array
  Result := fStringArray[AIndex - 1];
end;

procedure TExampleComponent.SetStringValue(const AIndex: integer; AValue: string);
// Example of an indexed property Set method
// Value is stored in private string array
begin
  if (fStringArray[AIndex - 1] <> AValue) then
    fStringArray[AIndex - 1] := AValue;
end;

procedure TExampleComponent.SetStrings(const AValue: TStrings);
begin
  // this is correct statement
  FStringList.Assign(AValue);
  // this is not correct
  // FStrings := AValue;
end;

procedure TExampleComponent.SetFont(const AValue: TFont);
begin
  // this is correct statement
  fFont.Assign(AValue);
  // this is not correct
  // fFont := AValue;
end;

procedure TExampleComponent.SetBitMap(const AValue: TBitmap);
begin
  // this is correct statement
  fBitmap.Assign(AValue);
  // this is not correct
  // fBitmap := AValue;
end;

procedure TExampleComponent.SetHiddenString(const AValue: string);
// If value is changed then triggers the custom events OnChangeHiddenString and OnSample
begin
  if (fHiddenString <> AValue) then
  begin
    fHiddenString := AValue;
    // Trigger custom events
    if Assigned(fOnChangeHiddenString) then
      OnChangeHiddenString(Self);
    if Assigned(fOnSample) then
      OnSample(AValue);
  end;
end;
// Sample application code that uses the Custom Events as coded
// Both are triggered by changing the HiddenString property in SetHiddenString
{
procedure TForm1.MyVisualComponent1ChangeHiddenString(Sender: TObject);
begin
  ShowMessage('Hidden String was changed to something new');
end;

procedure TForm1.MyVisualComponent1Sample(MyText: String);
begin
  ShowMessageFmt('Hidden String was changed to "%s"',[MyText]);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  MyVisualComponent1.HiddenString:='Hello World';
end;
}
end.
