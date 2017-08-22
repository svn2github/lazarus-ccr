unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxdbgrid,
  Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  DBGrids, Menus, db, rxdbverticalgrid, rxmemds, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    dsData: TDataSource;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    rxDataCREATE_USER_DATE: TDateTimeField;
    rxDataCREATE_USER_NAME: TStringField;
    rxDataTB_CLEINT_CODE: TLongintField;
    rxDataTB_CLIENT_EMAIL: TStringField;
    rxDataTB_CLIENT_ID: TLongintField;
    rxDataTB_CLIENT_INN: TStringField;
    rxDataTB_CLIENT_NAME: TStringField;
    rxDataTB_CLIENT_PHONE: TStringField;
    RxDBGrid1: TRxDBGrid;
    RxDBVerticalGrid1: TRxDBVerticalGrid;
    rxData: TRxMemoryData;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure FillDataBase;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FillDataBase;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RxDBVerticalGrid1.DataSource:=nil;
  RxDBGrid1.DataSource:=nil;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RxDBVerticalGrid1.DataSource:=dsData;
  RxDBGrid1.DataSource:=dsData;
end;

procedure TForm1.FillDataBase;
begin
  rxData.Open;
  rxData.AppendRecord([1, 1, '01000100101', 'JSC "BOOT"', 'test@email.com', '5(555)-557-88-77', 'alexs', now]);
  rxData.AppendRecord([2, 2, '02000100101', 'Wikimedia Foundation, Inc.', 'test@email.com', '5(555)-557-88-77', '1', now]);
  rxData.AppendRecord([3, 3, '03000100101', 'LLC Pilot ', 'test@email.com', '5(555)-557-88-77', '2', now]);
  rxData.AppendRecord([4, 4, '04000100101', 'Pilot, OOO', 'test@email.com', '5(555)-557-88-77', '3', now]);
  rxData.AppendRecord([5, 5, '05000100101', 'JSC "MS"', 'test@email.com', '5(555)-557-88-77', '4', now]);
  rxData.AppendRecord([6, 11, '06000100101', 'JSC "AA"', 'test@email.com', '5(555)-557-88-77', '5', now]);
  rxData.AppendRecord([7, 12, '07000100101', 'JSC "BBBB"', 'test@email.com', '5(555)-557-88-77', '6', now]);
  rxData.AppendRecord([8, 13, '08000100101', 'JSC "CCCC"', 'test@email.com', '5(555)-557-88-77', '7', now]);
  rxData.AppendRecord([9, 14, '09000100101', 'JSC "DDD"', 'test@email.com', '5(555)-557-88-77', '8', now]);
  rxData.AppendRecord([10, 15, '101000200101', 'JSC "EEEE"', 'test@email.com', '5(555)-557-88-77', '9', now]);
end;

end.

