program inheritancedemo;

{$mode objfpc}{$H+}

uses
  Classes, Gtk3,GObject2,GLib2, Math, gtk3mybutton;

// these procedures are signal handlers. they are called when a signal is emitted
procedure Gtk_Window_Destroy(Sender: PGtkWidget; user_data: gpointer); cdecl;
begin
  if gtk_main_level > 0 then
    gtk_main_quit;
end;

procedure Button_Click(Sender: PGtkWidget; user_data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

procedure CreateWidgets;
var
  Win: PGtkWindow;
  Button: PMyButton;
begin
  // first create window
  Win := TGtkWindow.new(GTK_WINDOW_TOPLEVEL);
  Win^.set_title('Hello World!');
  Win^.set_size_request(300,200);

  //then create a button and add it to the window
  Button := TMyButton.new;
  Button^.label_:='Quit';
  Button^.tooltip_text:='Don''t click me unless you want to!';
  Win^.add(PGtkWidget(Button));

  // now connect signals
  g_signal_connect_data(Win, 'destroy', TGCallback(@Gtk_Window_Destroy), nil, nil, 0);
  //g_signal_connect_data(Button, 'clicked', TGCallback(@Button_Click), nil, nil, 0);
  Button^.OnClicked:=@Button^.QuitProgram;

  Win^.show_all;
end;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  gtk_init(@argc, @argv);
  CreateWidgets;
  gtk_main;
end.

