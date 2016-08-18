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
  Classes, SysUtils, DB, rxdbgrid, LazFreeTypeFontCollection, vclutils, Graphics, fpPDF, EasyLazFreeType,
  contnrs;

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
  TRxDBGridExportPDF = class;
  TExportFonts = class;

  { TExportFontItem }

  TExportFontItem = class
  private
    FFontName: string;
    FOwner:TExportFonts;
    FBold: boolean;
    FDefaultFont: boolean;
    FFont: TFont;
    //
    FFreeTypeFont:TFreeTypeFont;
    FPdfFont:integer;
    function GetFontSize: Single;
    procedure SetFontSize(AValue: Single);

  public
    constructor Create(AOwner:TExportFonts; AFont:TFont; AFreeTypeFont:TFreeTypeFont);
    destructor Destroy; override;
    procedure Activate;
    property FontSize:Single read GetFontSize write SetFontSize;
    property Bold:boolean read FBold;
    property DefaultFont:boolean read FDefaultFont;
    property FontName:string read FFontName;
  end;

  { TExportFonts }

  TExportFonts = class
  private
    FDefaultFontBold: TExportFontItem;
    FDefaultFontNormal: TExportFontItem;
    FOwner:TRxDBGridExportPDF;
    FList:TFPList;
    function GetCount: integer;
    function GetItem(Index: integer): TExportFontItem;
  public
    constructor Create(AOwner:TRxDBGridExportPDF);
    destructor Destroy; override;
    procedure Clear;
    function AddItem(AFont: TFont; AFontCollectionItem:TCustomFontCollectionItem; ADefStyle:TFontStyles = []): TExportFontItem;
    function FindItem(AFont:TFont; ADefStyle:TFontStyles = []):TExportFontItem;
    property DefaultFontNormal:TExportFontItem read FDefaultFontNormal;
    property DefaultFontBold:TExportFontItem read FDefaultFontBold;
    property Count:integer read GetCount;
    property Item[Index:integer]:TExportFontItem read GetItem;
  end;


  { TRxDBGridExportPDF }

  TRxDBGridExportPDF = class(TRxDBGridAbstractTools)
  private
    FPageMargin: TRxPageMargin;
    FPageHeight:integer;
    FPageWidth:integer;

    FAuthorPDF: string;
    FFileName: string;
    FOpenAfterExport: boolean;
    FOptions: TRxDBGridExportPdfOptions;
    FProducerPDF: string;
    FPdfOptions:TPdfExportOptions;
    FCurPage: TPDFPage;
    FWorkPages:TFPList;
    FWorkPagesNeedCount:integer;

    FFontCollection:TFreeTypeFontCollection;
    FFontItems:TExportFonts;

    function GetPdfOptions: TPdfExportOptions;
    procedure SetPageMargin(AValue: TRxPageMargin);
    procedure SetPdfOptions(AValue: TPdfExportOptions);
    function SelectFont(AFont:TFont):TExportFontItem;
    function ActivateFont(AFont:TFont; AOwnerFont:TFont):TExportFontItem;
  protected
    FPDFDocument:TPDFDocument;
    FCurSection: TPDFSection;
    FDataSet:TDataSet;
    FPosY : integer;

    procedure DoSetupDocHeader;
    procedure DoSetupFonts;

    procedure WriteTextRect(AExportFont:TExportFontItem; X, Y, W, H:integer; AText:string; ATextAlign:TAlignment);
    procedure StartNewPage;

    procedure DoExportPage;
    procedure DoExportTitle;
    procedure DoExportBody;
    procedure DoExportFooter;
    procedure DoSaveDocument;

    function DoExecTools:boolean;override;
    function DoSetupTools:boolean; override;
    //
    procedure InitFonts(AFontCollection:TFreeTypeFontCollection);
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
    property PageMargin:TRxPageMargin read FPageMargin write SetPageMargin;
  end;

  {$ENDIF}
implementation

{$IF (FPC_FULLVERSION >= 30101)}
uses Grids, rxdconst, FileUtil, Forms, Controls, LCLIntf, LazFileUtils, RxDBGridExportPdfSetupUnit;

function ColorToDdfColor(C:TColor):TARGBColor;
var
  A:array [1..4] of byte absolute C;
begin
  if C = clWindow then
    Result:=clWhite
  else
    Result:={A[1] shl 24 +} A[1] shl 16 + A[2] shl 8 + A[3];
end;

{ TExportFonts }

function TExportFonts.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TExportFonts.GetItem(Index: integer): TExportFontItem;
begin
  Result:=TExportFontItem(FList[Index]);
end;

constructor TExportFonts.Create(AOwner: TRxDBGridExportPDF);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TFPList.Create;
end;

destructor TExportFonts.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TExportFonts.Clear;
var
  I: Integer;
begin
  for I:=0 to FList.Count-1 do
    TExportFontItem(FList[i]).Free;
  FList.Clear;
end;

function TExportFonts.AddItem(AFont: TFont;
  AFontCollectionItem: TCustomFontCollectionItem; ADefStyle: TFontStyles
  ): TExportFontItem;
var
  S1, S2, S3: String;
begin
  Result:=nil;
  if not Assigned(AFont) then exit;

  Result:=FindItem(AFont, ADefStyle);
  if not Assigned(AFont) then exit;

  Result:=TExportFontItem.Create(Self, AFont, AFontCollectionItem.CreateFont);

  S1:=ExtractFileDir(AFontCollectionItem.Filename);
  S2:=ExtractFileName(AFontCollectionItem.Filename);
  S3:=AFontCollectionItem.Information[ftiFullName];
  FOwner.FPDFDocument.FontDirectory:=S1;
  Result.FPdfFont:=FOwner.FPDFDocument.AddFont(S2, S3);
end;

function TExportFonts.FindItem(AFont: TFont; ADefStyle: TFontStyles
  ): TExportFontItem;
var
  K: TExportFontItem;
  i: Integer;
begin
  Result:=nil;
  if not Assigned(AFont) then exit;

  if ADefStyle = [] then
    ADefStyle:=AFont.Style;

  if AFont.Name = 'default' then
  begin
    for i:=0 to FList.Count - 1 do
    begin
      //Стили!
      K:=TExportFontItem(FList[i]);
      if K.DefaultFont then
      begin
        if (fsBold in ADefStyle) and K.Bold then
        begin
          Result:=K;
          exit;
        end
        else
        if (not (fsBold in ADefStyle)) and not K.Bold then
        begin
          Result:=K;
          exit;
        end;
      end;
    end;
  end
  else
  begin
    for i:=0 to FList.Count-1 do
    begin
      K:=TExportFontItem(FList[i]);
      if K.FontName = AFont.Name then
      begin
        if (fsBold in ADefStyle) and K.Bold then
        begin
          Result:=K;
          exit;
        end
        else
        if (not (fsBold in ADefStyle)) and not K.Bold then
        begin
          Result:=K;
          exit;
        end;
      end;
    end;
  end;
end;

{ TExportFontItem }

function TExportFontItem.GetFontSize: Single;
begin
  Result:=FFreeTypeFont.SizeInPixels;
end;

procedure TExportFontItem.SetFontSize(AValue: Single);
begin
  FFreeTypeFont.SizeInPixels:=AValue;
end;


constructor TExportFontItem.Create(AOwner: TExportFonts; AFont: TFont;
  AFreeTypeFont: TFreeTypeFont);
begin
  inherited Create;
  FOwner:=AOwner;
  FOwner.FList.Add(Self);
  FFont:=AFont;
  FFreeTypeFont:=AFreeTypeFont;
  FFontName:=AFont.Name;
end;

destructor TExportFontItem.Destroy;
begin
  FreeAndNil(FFreeTypeFont);
  inherited Destroy;
end;

procedure TExportFontItem.Activate;
var
  FS: Integer;
begin
  if FFont.Size = 0 then
    FS:=10
  else
    FS:=FFont.Size;

  FFreeTypeFont.SizeInPoints:=FS;
  FOwner.FOwner.FCurPage.SetFont(FPdfFont, FS);
  FOwner.FOwner.FCurPage.SetColor(ColorToDdfColor(FFont.Color), false);
end;

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

procedure TRxDBGridExportPDF.SetPageMargin(AValue: TRxPageMargin);
begin
  FPageMargin.Assign(AValue);
end;

procedure TRxDBGridExportPDF.SetPdfOptions(AValue: TPdfExportOptions);
begin
  FPdfOptions.Assign(AValue);
end;

function TRxDBGridExportPDF.SelectFont(AFont: TFont): TExportFontItem;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FFontItems.Count-1 do
    if FFontItems.Item[i].FFont = AFont then
    begin
      Result:=FFontItems.Item[i];
      Exit;
    end;
end;

function TRxDBGridExportPDF.ActivateFont(AFont: TFont; AOwnerFont: TFont
  ): TExportFontItem;
begin
  Result:=SelectFont(AFont);
  if not Assigned(Result) then
    Result:=SelectFont(AOwnerFont);
  if not Assigned(Result) then
    Result:=FFontItems.FDefaultFontNormal;

  if Assigned(Result) then
    Result.Activate
  else
    raise Exception.Create('Font not found');

end;

procedure TRxDBGridExportPDF.WriteTextRect(AExportFont: TExportFontItem; X, Y,
  W, H: integer; AText: string; ATextAlign: TAlignment);
var
  FTW, FTH: Single;
  X1: TPDFFloat;
  Y1: TPDFFloat;
begin
  FTW:=AExportFont.FFreeTypeFont.TextWidth(AText);
  FTH:=AExportFont.FFreeTypeFont.TextHeight(AText);
  case ATextAlign of
    taLeftJustify:
      begin
        Y1:=Y;
        X1:=X + constCellPadding;
      end;
    taRightJustify:
      begin
        Y1:=Y;
        X1:=X + W - FTW - 2;
        if X1 < X then
          X1:=X;
      end;
    taCenter:
      begin
        Y1:=Y;
        X1:=(X + W) / 2 - FTW / 2 - constCellPadding;
        if X1 < X then
          X1:=X;
      end;
  end;
  FCurPage.WriteText(X1, Y1 - FTH, AText);
end;

procedure TRxDBGridExportPDF.StartNewPage;
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

  FPosY:=FPageMargin.Top + 20;
end;

procedure TRxDBGridExportPDF.DoExportTitle;
var
  i, X, CP: Integer;
  C: TRxColumn;
  S: String;
  PU: TPDFUnitOfMeasure;
  WW: Single;
begin
  X:=FPageWidth + FPageMargin.Right;
  CP:=-1;
  FCurPage:=nil;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i];
    if C.Visible then
    begin
      if X + C.Width > FPageWidth - FPageMargin.Right then
      begin
        Inc(CP);
        FCurPage:=TPDFPage(FWorkPages[CP]);
        X:=FPageMargin.Left;
      end;

      FCurPage.SetColor(ColorToDdfColor(FRxDBGrid.BorderColor), true);
      FCurPage.DrawRect(X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, 1, false, true);


      WriteTextRect(ActivateFont(C.Title.Font, FRxDBGrid.TitleFont), X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, C.Title.Caption, C.Title.Alignment);

      X:=X + C.Width;
    end;
  end;

  Inc(FPosY, FRxDBGrid.DefaultRowHeight);
end;

procedure TRxDBGridExportPDF.DoExportBody;
procedure DoWriteRow;
var
  i, X, CP: Integer;
  C: TRxColumn;
  S: String;
begin
  X:=FPageWidth + FPageMargin.Right;
  CP:=-1;
  FCurPage:=nil;


  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i];
    if C.Visible then
    begin
      if X + C.Width > FPageWidth - FPageMargin.Right then
      begin
        Inc(CP);
        FCurPage:=TPDFPage(FWorkPages[CP]);
        X:=FPageMargin.Left;
      end;

      FCurPage.SetColor(ColorToDdfColor(FRxDBGrid.BorderColor), true); //Border
      FCurPage.SetColor(ColorToDdfColor(C.Color), false);              // Fill color
      FCurPage.DrawRect(X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, 1, true, true);

      if Assigned(C.Field) then
        WriteTextRect(ActivateFont(C.Font, FRxDBGrid.Font), X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, C.Field.DisplayText, C.Alignment);

      X:=X + C.Width;
    end;
  end;
end;

begin
  while not FDataSet.EOF do
  begin
    DoWriteRow;
    FDataSet.Next;
    Inc(FPosY, FRxDBGrid.DefaultRowHeight);
    if FPosY > FPageHeight - FPageMargin.Bottom then
{    begin
      FPosY:=FPageMargin.Top + 20;}
      exit;
//    end;
  end;
end;

procedure TRxDBGridExportPDF.DoSetupFonts;

procedure AddFonts(AFont:TFont);
var
  FM: TCustomFamilyCollectionItem;
  S: String;
  FIH: TCustomFontCollectionItem;
begin
  S:=AFont.Name;
  FM:=FFontCollection.Family[S];
  if Assigned(FM) then
  begin
    if fsBold in AFont.Style then
      FIH:=FM.GetFont('Bold')
    else
      FIH:=FM.GetFont('Regular');

    if Assigned(FIH) then
      FFontItems.AddItem(AFont, FIH);
  end;
end;

var
  FM: TCustomFamilyCollectionItem;
  FIH: TCustomFontCollectionItem;
  F: TExportFontItem;
  i: Integer;
begin
  InitFonts(FFontCollection);
  FM:=nil;

  if FRxDBGrid.Font.Name <> 'default' then
    FM:=FFontCollection.Family[FRxDBGrid.Font.Name];
  if not Assigned(FM) then //Fill default fonts
    FM:=FFontCollection.Family['Liberation Sans'];
  if not Assigned(FM) then
    FM:=FFontCollection.Family['Arial'];

  if Assigned(FM) then
  begin
    FIH:=FM.GetFont('Bold');
    if Assigned(FIH) then
    begin
      F:=FFontItems.AddItem(FRxDBGrid.TitleFont, FIH, [fsBold]);
      F.FBold:=true;
      F.FDefaultFont:=true;
      FFontItems.FDefaultFontBold:=F;
    end;

    FIH:=FM.GetFont('Regular');
    if Assigned(FIH) then
    begin
      F:=FFontItems.AddItem(FRxDBGrid.Font, FIH, []);
      F.FDefaultFont:=true;
      FFontItems.FDefaultFontNormal:=F;
    end;
  end;

  if not Assigned(FM) then
   raise Exception.Create('Not found Sans font');

  for i:=0 to FRxDBGrid.Columns.Count-1 do
  begin
    if FRxDBGrid.Columns[i].Font.Name <> 'default' then
      AddFonts(FRxDBGrid.Columns[i].Font);

    if FRxDBGrid.Columns[i].Footer.Font.Name <> 'default' then
      AddFonts(FRxDBGrid.Columns[i].Footer.Font);

    if FRxDBGrid.Columns[i].Title.Font.Name <> 'default' then
      AddFonts(FRxDBGrid.Columns[i].Title.Font);
  end;
end;

procedure TRxDBGridExportPDF.DoExportFooter;

procedure WriteFooterRow(AFooterRow:Integer);
var
  i, X, CP, FS: Integer;
  S: String;
  C: TRxColumn;
begin
  X:=FPageWidth + FPageMargin.Right;
  CP:=-1;
  FCurPage:=nil;

  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i];
    if C.Visible then
    begin
      if X + C.Width > FPageWidth - FPageMargin.Right then
      begin
        Inc(CP);
        FCurPage:=TPDFPage(FWorkPages[CP]);
        X:=FPageMargin.Left;
      end;

      FCurPage.SetColor(ColorToDdfColor(FRxDBGrid.BorderColor), true); //Border
      FCurPage.SetColor(ColorToDdfColor(FRxDBGrid.FooterOptions.Color), false);              // Fill color
      FCurPage.DrawRect(X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, 1, true, true);

      if FRxDBGrid.FooterOptions.RowCount = 1 then
        S:=C.Footer.DisplayText
      else
      begin
        if C.Footers.Count > AFooterRow then
          S:=C.Footers[AFooterRow].DisplayText
        else
          S:='';
      end;

      if (S<>'') then
        WriteTextRect(ActivateFont(C.Footer.Font, FRxDBGrid.Font), X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, S, C.Footer.Alignment);

      X:=X + C.Width;
    end;
  end;
  Inc(FPosY, FRxDBGrid.DefaultRowHeight);
end;

var
  j: Integer;
begin
  if FRxDBGrid.FooterRowCount = 1 then
    WriteFooterRow(1)
  else
  begin
    for j:=0 to FRxDBGrid.FooterRowCount-1 do
    begin
      if FPosY > FPageHeight - FPageMargin.Bottom then
        StartNewPage;
      WriteFooterRow(j);
    end;
  end;
end;

procedure TRxDBGridExportPDF.DoSetupDocHeader;
var
  W, i: Integer;
  C: TRxColumn;
begin
  FPDFDocument.Infos.Title := Application.Title;
  FPDFDocument.Infos.Author := FAuthorPDF;
  FPDFDocument.Infos.Producer := FProducerPDF;
  FPDFDocument.Infos.ApplicationName := ApplicationName;
  FPDFDocument.Infos.CreationDate := Now;

  FPDFDocument.Options:=FPdfOptions.FOptions;
  FPDFDocument.DefaultOrientation:=FPdfOptions.PaperOrientation;

  //calc need count pages for all columns
  FWorkPagesNeedCount:=1;
  if FPdfOptions.FPaperType <> ptCustom then
  begin
    if FPdfOptions.PaperOrientation = ppoPortrait then
    begin
      FPageWidth := PDFPaperSizes[FPdfOptions.FPaperType, 1];
      FPageHeight := PDFPaperSizes[FPdfOptions.FPaperType, 0];
    end
    else
    begin
      FPageWidth := PDFPaperSizes[FPdfOptions.FPaperType, 0];
      FPageHeight := PDFPaperSizes[FPdfOptions.FPaperType, 1];
    end;

    W:=FPageWidth + FPageMargin.Right;
    FWorkPagesNeedCount:=0;
    for i:=0 to FRxDBGrid.Columns.Count - 1 do
    begin
      C:=FRxDBGrid.Columns[i];
      if C.Visible then
      begin
        if W + C.Width > FPageWidth - FPageMargin.Right then
        begin
          Inc(FWorkPagesNeedCount);
          W:=FPageMargin.Left;
        end;
        W:=W + C.Width;
      end;
    end;
  end;
end;

procedure TRxDBGridExportPDF.DoExportPage;
var
  P: TPDFPage;
  i: Integer;
begin
  StartNewPage;

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

  FFontCollection:=TFreeTypeFontCollection.Create;
  FPDFDocument:=TPDFDocument.Create(nil);
  FFontItems:=TExportFonts.Create(Self);
  FWorkPages:=TFPList.Create;
  try
    DoSetupFonts;
    DoSetupDocHeader;
    FPDFDocument.StartDocument;
    FCurSection := FPDFDocument.Sections.AddSection; // we always need at least one section
    FDataSet.First;
    repeat
      DoExportPage;
    until FDataSet.EOF;

    if repExportTitle in FOptions then
    begin
      if FPosY > FPageHeight - FPageMargin.Bottom then
        StartNewPage;

      DoExportFooter;
    end;

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
    FreeAndNil(FFontItems);
    FreeAndNil(FFontCollection);
  end;

  if Result and FOpenAfterExport then
    OpenDocument(FileName);
end;

function TRxDBGridExportPDF.DoSetupTools: boolean;
begin
  RxDBGridExportPdfSetupForm:=TRxDBGridExportPdfSetupForm.Create(Application);
  RxDBGridExportPdfSetupForm.FileNameEdit1.FileName:=FileName;
  RxDBGridExportPdfSetupForm.cbOpenAfterExport.Checked:=FOpenAfterExport;
  RxDBGridExportPdfSetupForm.cbExportColumnHeader.Checked:=repExportTitle in FOptions;
  RxDBGridExportPdfSetupForm.cbExportColumnFooter.Checked:=repExportFooter in FOptions;

  Result:=RxDBGridExportPdfSetupForm.ShowModal = mrOk;
  if Result then
  begin
    FileName:=RxDBGridExportPdfSetupForm.FileNameEdit1.FileName;
    FOpenAfterExport:=RxDBGridExportPdfSetupForm.cbOpenAfterExport.Checked;
    if  RxDBGridExportPdfSetupForm.cbExportColumnHeader.Checked then
      FOptions:=FOptions + [repExportTitle]
    else
      FOptions:=FOptions - [repExportTitle];

    if RxDBGridExportPdfSetupForm.cbExportColumnFooter.Checked then
      FOptions:=FOptions + [repExportFooter]
    else
      FOptions:=FOptions - [repExportFooter];
  end;
  RxDBGridExportPdfSetupForm.Free;
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

procedure TRxDBGridExportPDF.InitFonts(AFontCollection: TFreeTypeFontCollection
  );
var
  FontDirList: TStringList;

procedure CreateFontDirList;
var
  s: String;
begin
 {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(20); // CSIDL_FONTS = 20
  if s <> '' then
    FontDirList.Add(s);
 {$ENDIF}
 {$IFDEF linux}
  //tested on Fedora 24
  FontDirList.Add('/usr/share/cups/fonts/');
  FontDirList.Add('/usr/share/fonts/');
  FontDirList.Add('/usr/local/lib/X11/fonts/');
  FontDirList.Add(GetUserDir + '.fonts/');
 {$ENDIF}
end;

procedure AddFolder(AFolder: string);
var
  Files: TStringList;
  i: integer;
begin
  AFolder := AppendPathDelim(ExpandFileName(AFolder));
  Files := TStringList.Create;
  AFontCollection.BeginUpdate;
  try
    FindAllFiles(Files, AFolder, '*.ttf', true);
    Files.Sort;
    for i := 0 to Files.Count-1 do
      try
        AFontCollection.AddFile(Files[i]);
      except
      end;
  finally
    AFontCollection.EndUpdate;
    Files.Free;
  end;
end;

var
  i: Integer;
begin
  FontDirList := TStringList.Create;
  CreateFontDirList;

  for i:=0 to FontDirList.Count-1 do
    AddFolder(FontDirList[i]);
  FreeAndNil(FontDirList);
end;

constructor TRxDBGridExportPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageMargin:=TRxPageMargin.Create;
  FPdfOptions:=TPdfExportOptions.Create(Self);

  FCaption:=sToolsExportPDF;
  FOpenAfterExport:=false;
end;

destructor TRxDBGridExportPDF.Destroy;
begin
  FreeAndNil(FPdfOptions);
  FreeAndNil(FPageMargin);
  inherited Destroy;
end;


{$ENDIF}
end.

