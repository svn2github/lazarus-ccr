{ RxDBGridExportPdf unit

  Copyright (C) 2005-2016 Lagunov Aleksey alexs@yandex.ru
  original conception from rx library for Delphi (c)

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

unit RxDBGridExportPdf;

{$mode objfpc}{$H+}

interface

{$IF (FPC_FULLVERSION >= 30101)}
uses
  Classes, SysUtils, DB, rxdbgrid, fpPDF;

type

  TRxDBGridExportPdfOption = (repExportTitle,
    repExportColors,
    repExportFooter,
    repOverwriteExisting
    );
  TRxDBGridExportPdfOptions = set of TRxDBGridExportPdfOption;

  { TPdfExportOptions }

  TPdfExportOptions = class(TPersistent)
  private
    FOwner: TPersistent;
    FOptions: TPDFOptions;
    FPaperOrientation: TPDFPaperOrientation;
    FPaperType: TPDFPaperType;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent);
  published
    property PaperType:TPDFPaperType read FPaperType write FPaperType default ptA4;
    property PaperOrientation:TPDFPaperOrientation read FPaperOrientation write FPaperOrientation default ppoPortrait;
    property Options:TPDFOptions read FOptions write FOptions;
  end;

type

  { TRxDBGridExportPDF }

  TRxDBGridExportPDF = class(TRxDBGridAbstractTools)
  private
    FAuthorPDF: string;
    FFileName: string;
    FOpenAfterExport: boolean;
    FOptions: TRxDBGridExportPdfOptions;
    FProducerPDF: string;
    FPdfOptions:TPdfExportOptions;
    FWorkPages:TFPList;
    FWorkPagesNeedCount:integer;
    function GetPdfOptions: TPdfExportOptions;
    procedure SetPdfOptions(AValue: TPdfExportOptions);
  protected
    FPDFDocument:TPDFDocument;
    FCurSection: TPDFSection;
    FDataSet:TDataSet;
    FPosY : integer;
    FPageHeight:integer;

    FHeaderFont:integer;
    FBodyFont:integer;
    FFooterFont:integer;

    procedure DoExportTitle;
    procedure DoExportBody;
    procedure DoSetupFonts;
    procedure DoExportFooter;

    procedure DoSetupDocHeader;
    procedure DoExportPage;
    function DoExecTools:boolean;override;
    function DoSetupTools:boolean; override;
    procedure DoSaveDocument;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName:string read FFileName write FFileName;
    property Options:TRxDBGridExportPdfOptions read FOptions write FOptions;
    property PdfOptions:TPdfExportOptions read GetPdfOptions write SetPdfOptions;
    property OpenAfterExport:boolean read FOpenAfterExport write FOpenAfterExport default false;
    property AuthorPdf:string read FAuthorPDF write FAuthorPDF;
    property ProducerPdf:string read FProducerPDF write FProducerPDF;
  end;

  {$ENDIF}
implementation

{$IF (FPC_FULLVERSION >= 30101)}
uses rxdconst, forms, LCLIntf;

{ TPdfExportOptions }

procedure TPdfExportOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TPdfExportOptions then
  begin
    TPdfExportOptions(Dest).FOptions := FOptions;
    TPdfExportOptions(Dest).FPaperOrientation:=FPaperOrientation;
    TPdfExportOptions(Dest).FPaperType:=FPaperType;
  end
  else
  inherited AssignTo(Dest);
end;

constructor TPdfExportOptions.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner:=AOwner;
  FPaperType:=ptA4;
  FPaperOrientation:=ppoPortrait;
end;

{ TRxDBGridExportSpreadSheet }

function TRxDBGridExportPDF.GetPdfOptions: TPdfExportOptions;
begin
  Result:=FPdfOptions;
end;

procedure TRxDBGridExportPDF.SetPdfOptions(AValue: TPdfExportOptions);
begin
  FPdfOptions.Assign(AValue);
end;

procedure TRxDBGridExportPDF.DoExportTitle;
var
  P: TPDFPage;
  Pt: TPDFCoord;
  i, X: Integer;
  C: TRxColumn;
  S: String;
begin
  X:=20;

  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    P:=TPDFPage(FWorkPages[0]);
    C:=FRxDBGrid.Columns[i];
    Pt.X := X;
    Pt.Y := FPosY;
    P.SetColor(C.Color);
    P.DrawRect(Pt.X, Pt.Y, C.Width, FRxDBGrid.DefaultRowHeight, 1, false, true);


    P.SetFont(FHeaderFont, 10);
    //P.SetColor(clBlue, false);
    P.WriteText(Pt.X+2, Pt.Y-10, C.Title.Caption);


    Inc(X, C.Width);
  end;

  Inc(FPosY, FRxDBGrid.DefaultRowHeight);
{
  S:='Russian: Привет мир!';

  P.SetFont(FBodyFont, 11);
  P.WriteText(40, 160, S);}
end;

procedure TRxDBGridExportPDF.DoExportBody;
begin

end;

procedure TRxDBGridExportPDF.DoSetupFonts;
begin
  //FPDFDocument.FontDirectory := '/usr/share/fonts/liberation';
  FPDFDocument.FontDirectory := '/usr/share/fonts/liberation';
  FHeaderFont := FPDFDocument.AddFont('LiberationSans-Regular.ttf', 'LiberationSans', clGreen);

  FPDFDocument.FontDirectory := 'fonts';
  FBodyFont := FPDFDocument.AddFont('FreeSans.ttf', 'FreeSans', clGreen); // TODO: this color value means nothing - not used at all
//  FHeaderFont := FPDFDocument.AddFont('Helvetica');
//  FBodyFont := D.AddFont('Helvetica');
//  FFooterFont := D.AddFont('Helvetica');
//  FBodyFont := FHeaderFont;
  FFooterFont := FHeaderFont;

  {FtTitle := D.AddFont('Helvetica', clRed);
  FtText1 := D.AddFont('FreeSans.ttf', 'FreeSans', clGreen); // TODO: this color value means nothing - not used at all
  FtText2 := D.AddFont('Times-BoldItalic', clBlack);}

end;

procedure TRxDBGridExportPDF.DoExportFooter;
begin

end;

procedure TRxDBGridExportPDF.DoSetupDocHeader;
var
  MaxW, W, i: Integer;
begin
  FPDFDocument.Infos.Title := Application.Title;
  FPDFDocument.Infos.Author := FAuthorPDF;
  FPDFDocument.Infos.Producer := FProducerPDF;
  FPDFDocument.Infos.ApplicationName := ApplicationName;
  FPDFDocument.Infos.CreationDate := Now;

  FPDFDocument.Options:=FPdfOptions.FOptions;
  FPDFDocument.DefaultOrientation:=FPdfOptions.PaperOrientation;

  //calc need count pages for all columns
  FWorkPagesNeedCount:=0;
  if FPdfOptions.FPaperType <> ptCustom then
  begin
    if FPdfOptions.PaperOrientation = ppoPortrait then
      MaxW:=PDFPaperSizes[FPdfOptions.FPaperType, 0]
    else
      MaxW:=PDFPaperSizes[FPdfOptions.FPaperType, 1];

    W:=0;
    for i:=0 to FRxDBGrid.Columns.Count-1 do
    begin
      W:=W + FRxDBGrid.Columns[i].Width;

      if W > MaxW then
      begin
        Inc(FWorkPagesNeedCount);
        W:=0;
      end;
    end;
  end;

  if FWorkPagesNeedCount = 0 then
    FWorkPagesNeedCount:=1;

end;

procedure TRxDBGridExportPDF.DoExportPage;
var
  P: TPDFPage;
  i: Integer;
begin
  FWorkPages.Clear;
  for i:=0 to FWorkPagesNeedCount - 1 do
  begin
    P := FPDFDocument.Pages.AddPage;
    P.PaperType := FPdfOptions.PaperType;
    P.UnitOfMeasure := uomPixels;
    FCurSection.AddPage(P);
    FWorkPages.Add(P);
  end;

  FPosY:=40;

  if repExportTitle in FOptions then
    DoExportTitle;

  DoExportBody;
end;

function TRxDBGridExportPDF.DoExecTools: boolean;
var
  P: TBookMark;
begin
  Result:=false;
  FDataSet:=FRxDBGrid.DataSource.DataSet;
  FDataSet.DisableControls;
  {$IFDEF NoAutomatedBookmark}
  P:=FDataSet.GetBookmark;
  {$ELSE}
  P:=FDataSet.Bookmark;
  {$ENDIF}

  FPDFDocument:=TPDFDocument.Create(nil);
  FWorkPages:=TFPList.Create;
  try
    DoSetupFonts;
    DoSetupDocHeader;
    FPDFDocument.StartDocument;
    FCurSection := FPDFDocument.Sections.AddSection; // we always need at least one section
    FDataSet.First;
    repeat
      DoExportPage;
      FDataSet.Next;
    until FDataSet.EOF;

    DoSaveDocument;
    Result:=true;
  finally
    {$IFDEF NoAutomatedBookmark}
    FDataSet.GotoBookmark(P);
    FDataSet.FreeBookmark(P);
    {$ELSE}
    FDataSet.Bookmark:=P;
    {$ENDIF}
    FDataSet.EnableControls;

    FreeAndNil(FWorkPages);
    FreeAndNil(FPDFDocument);
  end;

  if Result and FOpenAfterExport then
    OpenDocument(FileName);
end;

function TRxDBGridExportPDF.DoSetupTools: boolean;
begin
  Result:=inherited DoSetupTools;
end;

procedure TRxDBGridExportPDF.DoSaveDocument;
var
  F: TFileStream;
begin
  F := TFileStream.Create(FFileName,fmCreate);
  try
    FPDFDocument.SaveToStream(F);
  finally
    F.Free;
  end;
end;

constructor TRxDBGridExportPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPdfOptions:=TPdfExportOptions.Create(Self);

  FCaption:=sToolsExportPDF;
  FOpenAfterExport:=false;
end;

destructor TRxDBGridExportPDF.Destroy;
begin
  FreeAndNil(FPdfOptions);
  inherited Destroy;
end;


{$ENDIF}
end.

