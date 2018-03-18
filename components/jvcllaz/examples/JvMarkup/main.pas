unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SynEdit, SynHighlighterHTML, JvMarkupViewer, JvMarkupLabel;

type

  { TMainForm }

  TMainForm = class(TForm)
    JvMarkupLabel1: TJvMarkupLabel;
    JvMarkupViewer1: TJvMarkupViewer;
    Panel1: TPanel;
    Panel2: TPanel;
    RbMarkupViewer: TRadioButton;
    RbMarkupLabel: TRadioButton;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    procedure FormCreate(Sender: TObject);
    procedure RbMarkupViewerChange(Sender: TObject);
    procedure RbMarkupLabelChange(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JvMarkupViewer1.Text := SynEdit1.Lines.Text;
  JvMarkupLabel1.Text := SynEdit1.Lines.Text;
  JvMarkupViewer1.Align := alClient;
  JvMarkupLabel1.Align := alClient;
  JvMarkupViewer1.Visible := RbMarkupViewer.Checked;
  JvMarkupLabel1.Visible := RbMarkupLabel.Checked;
end;

procedure TMainForm.RbMarkupViewerChange(Sender: TObject);
begin
  JvMarkupViewer1.Visible := RbMarkupViewer.Checked;
end;

procedure TMainForm.RbMarkupLabelChange(Sender: TObject);
begin
  JvMarkupLabel1.Visible := RbMarkupLabel.Checked;
end;

procedure TMainForm.SynEdit1Change(Sender: TObject);
begin
  if JvMarkupViewer1.Visible then
    JvMarkupViewer1.Text := SynEdit1.Lines.Text;
  if JvMarkupLabel1.Visible then
    JvMarkupLabel1.Text := SynEdit1.Lines.Text;
end;

end.

