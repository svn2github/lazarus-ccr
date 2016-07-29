program ihelloworld;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$linkframework UIKit}

uses
  // you can probably find a number of different iPhoneAll units available online.
  // The example is written, using iPhoneAll from https://github.com/genericptr/iOS_8_0
  // Once you get the headers don't forget to add to search path for the project
  iPhoneAll;

type
  TAppDelegate = objcclass(NSObject)
    procedure applicationDidFinishLaunching(application: UIApplication); message 'applicationDidFinishLaunching:';
  end;

  TMyView = objcclass(UIView)
  public
    procedure drawRect(c: CGRect); override;
  end;

const
  helloworld = 'Hello world';

// window paint method
procedure TMyView.drawRect(c: CGRect);
var
  cg : CGContextRef;
begin
  // getting current context
  cg:=UIGraphicsGetCurrentContext;
  // setting back ground color
  CGContextSetRGBFillColor(cg, 0, 0, 0.5, 1);
  CGContextFillRect(cg, c);

  // rotating up-side down context
  CGContextTranslateCTM(cg, 0, c.size.height);
  CGContextScaleCTM(cg, 1, -1);

  // setting text color
  CGContextSetRGBFillColor(cg, 1, 1, 0, 1);
  CGContextSetRGBStrokeColor(cg, 1, 1, 0, 1);
  // setting font  (must set any)
  CGContextSelectFont(cg, 'Helvetica', 30, kCGEncodingMacRoman);
  // rendering text
  CGContextShowTextAtPoint(cg, 0, c.size.height-50, helloworld, length(helloworld));
end;

var
  mainwindow : UIWindow;
  ctrl   : UIViewController;
  myview : TMyView;

{ TAppDelegate }

procedure TAppDelegate.applicationDidFinishLaunching(application: UIApplication);
begin
  // application has initialized, now we can create the main window
  // initialize window in Objective-C style
  mainwindow:=UIWindow.alloc.initWithFrame(UIScreen.mainScreen.bounds);

  // creating the root controller
  ctrl:=UIViewController.alloc.init;
  mainwindow.setRootViewController(ctrl);

  myview := TMyView.alloc.init;
  myview.setBounds( mainwindow.bounds );
  ctrl.setView(myview);

  // activate and show the window
  mainwindow.makeKeyAndVisible;

end;

function NSStr(const s: string): NSString;
begin
  // converting string to NSString (CFStringRef and NSString are interchangable)
  Result:=NSString( CFStr(PChar(s)));
end;

var
  pool    : NSAutoreleasePool;

{$R *.res}

begin
  // initialize foundation memory manger (aka autorelease pool)
  pool := NSAutoreleasePool.alloc.init;
  // launching main application loop
  ExitCode:=UIApplicationMain(argc, PChar(argv), nil, NSSTR('TAppDelegate'));
  // according to docs the UIApplicationMain never returns,
  // but still the code present in the Obj-C main.m files
  pool.release;
end.
