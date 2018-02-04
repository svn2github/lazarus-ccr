unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  spktoolbar, spkt_Tab, spkt_Pane, spkt_Buttons, spkt_Checkboxes;

type

  { TForm1 }

  TForm1 = class(TForm)
    LargeImages: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ScrollBox1: TScrollBox;
    SmallImages: TImageList;
    SpkCheckbox1: TSpkCheckbox;
    SpkCheckbox2: TSpkCheckbox;
    SpkCheckbox3: TSpkCheckbox;
    SpkCheckbox4: TSpkCheckbox;
    SpkCheckbox5: TSpkCheckbox;
    SpkLargeButton1: TSpkLargeButton;
    SpkLargeButton10: TSpkLargeButton;
    SpkLargeButton11: TSpkLargeButton;
    SpkLargeButton12: TSpkLargeButton;
    SpkLargeButton13: TSpkLargeButton;
    SpkLargeButton14: TSpkLargeButton;
    SpkLargeButton15: TSpkLargeButton;
    SpkLargeButton2: TSpkLargeButton;
    SpkLargeButton3: TSpkLargeButton;
    SpkLargeButton4: TSpkLargeButton;
    SpkLargeButton5: TSpkLargeButton;
    SpkLargeButton6: TSpkLargeButton;
    SpkLargeButton7: TSpkLargeButton;
    SpkLargeButton8: TSpkLargeButton;
    SpkLargeButton9: TSpkLargeButton;
    SpkPane1: TSpkPane;
    SpkPane10: TSpkPane;
    SpkPane11: TSpkPane;
    SpkPane12: TSpkPane;
    SpkPane13: TSpkPane;
    SpkPane14: TSpkPane;
    SpkPane15: TSpkPane;
    SpkPane16: TSpkPane;
    SpkPane17: TSpkPane;
    SpkPane18: TSpkPane;
    SpkPane19: TSpkPane;
    SpkPane2: TSpkPane;
    SpkPane20: TSpkPane;
    SpkPane3: TSpkPane;
    SpkPane4: TSpkPane;
    SpkPane5: TSpkPane;
    SpkPane6: TSpkPane;
    SpkPane7: TSpkPane;
    SpkPane8: TSpkPane;
    SpkPane9: TSpkPane;
    SpkSmallButton1: TSpkSmallButton;
    SpkSmallButton10: TSpkSmallButton;
    SpkSmallButton11: TSpkSmallButton;
    SpkSmallButton12: TSpkSmallButton;
    SpkSmallButton13: TSpkSmallButton;
    SpkSmallButton14: TSpkSmallButton;
    SpkSmallButton15: TSpkSmallButton;
    SpkSmallButton16: TSpkSmallButton;
    SpkSmallButton17: TSpkSmallButton;
    SpkSmallButton18: TSpkSmallButton;
    SpkSmallButton19: TSpkSmallButton;
    SpkSmallButton2: TSpkSmallButton;
    SpkSmallButton20: TSpkSmallButton;
    SpkSmallButton21: TSpkSmallButton;
    SpkSmallButton22: TSpkSmallButton;
    SpkSmallButton23: TSpkSmallButton;
    SpkSmallButton24: TSpkSmallButton;
    SpkSmallButton25: TSpkSmallButton;
    SpkSmallButton26: TSpkSmallButton;
    SpkSmallButton27: TSpkSmallButton;
    SpkSmallButton28: TSpkSmallButton;
    SpkSmallButton29: TSpkSmallButton;
    SpkSmallButton3: TSpkSmallButton;
    SpkSmallButton30: TSpkSmallButton;
    SpkSmallButton31: TSpkSmallButton;
    SpkSmallButton32: TSpkSmallButton;
    SpkSmallButton33: TSpkSmallButton;
    SpkSmallButton34: TSpkSmallButton;
    SpkSmallButton35: TSpkSmallButton;
    SpkSmallButton36: TSpkSmallButton;
    SpkSmallButton37: TSpkSmallButton;
    SpkSmallButton38: TSpkSmallButton;
    SpkSmallButton39: TSpkSmallButton;
    SpkSmallButton4: TSpkSmallButton;
    SpkSmallButton40: TSpkSmallButton;
    SpkSmallButton41: TSpkSmallButton;
    SpkSmallButton42: TSpkSmallButton;
    SpkSmallButton43: TSpkSmallButton;
    SpkSmallButton44: TSpkSmallButton;
    SpkSmallButton45: TSpkSmallButton;
    SpkSmallButton46: TSpkSmallButton;
    SpkSmallButton47: TSpkSmallButton;
    SpkSmallButton48: TSpkSmallButton;
    SpkSmallButton49: TSpkSmallButton;
    SpkSmallButton5: TSpkSmallButton;
    SpkSmallButton50: TSpkSmallButton;
    SpkSmallButton51: TSpkSmallButton;
    SpkSmallButton52: TSpkSmallButton;
    SpkSmallButton53: TSpkSmallButton;
    SpkSmallButton54: TSpkSmallButton;
    SpkSmallButton55: TSpkSmallButton;
    SpkSmallButton56: TSpkSmallButton;
    SpkSmallButton57: TSpkSmallButton;
    SpkSmallButton58: TSpkSmallButton;
    SpkSmallButton59: TSpkSmallButton;
    SpkSmallButton6: TSpkSmallButton;
    SpkSmallButton60: TSpkSmallButton;
    SpkSmallButton61: TSpkSmallButton;
    SpkSmallButton62: TSpkSmallButton;
    SpkSmallButton63: TSpkSmallButton;
    SpkSmallButton64: TSpkSmallButton;
    SpkSmallButton65: TSpkSmallButton;
    SpkSmallButton66: TSpkSmallButton;
    SpkSmallButton67: TSpkSmallButton;
    SpkSmallButton68: TSpkSmallButton;
    SpkSmallButton69: TSpkSmallButton;
    SpkSmallButton7: TSpkSmallButton;
    SpkSmallButton70: TSpkSmallButton;
    SpkSmallButton71: TSpkSmallButton;
    SpkSmallButton72: TSpkSmallButton;
    SpkSmallButton73: TSpkSmallButton;
    SpkSmallButton74: TSpkSmallButton;
    SpkSmallButton75: TSpkSmallButton;
    SpkSmallButton8: TSpkSmallButton;
    SpkSmallButton9: TSpkSmallButton;
    SpkTab1: TSpkTab;
    SpkTab2: TSpkTab;
    SpkTab4: TSpkTab;
    SpkTab5: TSpkTab;
    SpkTab6: TSpkTab;
    SpkToolbar1: TSpkToolbar;
    SpkToolbar2: TSpkToolbar;
    SpkToolbar3: TSpkToolbar;
    SpkToolbar4: TSpkToolbar;
    SpkToolbar5: TSpkToolbar;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

