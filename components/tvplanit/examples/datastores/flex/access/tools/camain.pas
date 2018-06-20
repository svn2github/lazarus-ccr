unit caMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, odbcconn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, EditBtn, ComCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    BtnCreateDB: TButton;
    BtnClose: TButton;
    CbCreateVPFields: TCheckBox;
    FileNameEdit: TFileNameEdit;
    ODBCConnection1: TODBCConnection;
    Panel1: TPanel;
    Panel2: TPanel;
    RgFormat: TRadioGroup;
    SQLTransaction1: TSQLTransaction;
    StatusBar1: TStatusBar;
    procedure BtnCreateDBClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RgFormatClick(Sender: TObject);
  private
    function CreateAccessDatabase(ADatabaseFile: string;
      out AErrorMsg: String): boolean;
    procedure CreateContactsTable;
    procedure CreateEventsTable;
    procedure CreateResourceTable;
    procedure CreateTasksTable;
    procedure StatusMsg(const AText: String);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLType, LazFileUtils;

const
   DB_DRIVERS: array[0..1] of String = (
     'Microsoft Access Driver (*.mdb)',
     'Microsoft Access Driver (*.mdb, *.accdb)'
   );
   EXT: array[0..1] of String = (
     '.mdb',
     '.accdb'
   );

   ODBC_ADD_DSN = 1;
   ODBC_CONFIG_DSN = 2;
   ODBC_REMOVE_DSN = 3;
   ODBC_ADD_SYS_DSN = 4;
   ODBC_CONFIG_SYS_DSN = 5;
   ODBC_REMOVE_SYS_DSN = 6;
   ODBC_REMOVE_DEFAULT_DSN = 7;

function SQLConfigDataSource(hwndParent: Integer; fRequest: Integer;
  lpszDriverString: PChar; lpszAttributes: PChar): Integer; stdcall; external 'odbccp32.dll';

function SQLInstallerError(iError: integer; pfErrorCode: PInteger;
  lpszErrorMsg: string; cbErrorMsgMax: integer; pcbErrorMsg: PInteger): integer; stdcall; external 'odbccp32.dll';


{ TForm1 }

procedure TForm1.BtnCreateDBClick(Sender: TObject);
var
  fn: String;
  errMsg: String;
begin
  if FileNameEdit.FileName = '' then
    exit;

  fn := ChangeFileExt(FilenameEdit.FileName, EXT[RgFormat.ItemIndex]);
  fn := ExpandFileNameUTF8(fn);
  if FileExistsUTF8(fn) then
    DeleteFileUTF8(fn);

  // Create empty database file
  if CreateAccessDatabase(fn, errMsg) then
    StatusMsg('Database file created')
  else begin
    MessageDlg('Database file could not be created:' + LineEnding + errMsg,
      mtError, [mbOK], 0);
    exit;
  end;

  if CbCreateVPFields.Checked then begin
    //connection
    ODBCConnection1.Driver := DB_DRIVERS[RgFormat.ItemIndex];
    ODBCConnection1.Params.Add('DBQ=' + fn);
//    ODBCConnection1.Params.Add('Locale Identifier=1031');
//    ODBCConnection1.Params.Add('ExtendedAnsiSQL=1');
//    ODBCConnection1.Params.Add('CHARSET=ansi');
    ODBCConnection1.KeepConnection := True;
    ODBCConnection1.Connected := True;

    //transaction
    SQLTransaction1.DataBase := ODBCConnection1;
    SQLTransaction1.Action := caCommit;
    SQLTransaction1.Active := True;

    // Create tables
    CreateResourceTable;
    CreateContactsTable;
    CreateEventsTable;
    CreateTasksTable;

    SQLTransaction1.Active := false;
    ODBCConnection1.Connected := false;

    Statusbar1.SimpleText := 'All tables created.';
  end;
end;

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

function TForm1.CreateAccessDatabase(ADatabaseFile: string;
  out AErrorMsg: String): boolean;
var
  dbType: string;
  driver: string;
  ErrorCode, ResizeErrorMessage: integer;
  ErrorMessage: PChar;
  retCode: integer;
  L: TStrings;
begin
  Result := false;
  AErrorMsg := '';

  driver := DB_DRIVERS[rgFormat.ItemIndex];

  { With the new accdb driver,
  CREATE_DB/CREATE_DBV12 will create an .accdb format database;
  CREATE_DBV4 will create an mdb
  http://stackoverflow.com/questions/9205633/how-do-i-specify-the-odbc-access-driver-format-when-creating-the-database
  }

  case rgFormat.ItemIndex of
    0 : dbtype := 'CREATE_DB="' + ADatabaseFile + '"';
    1 : case Lowercase(ExtractFileExt(ADatabaseFile)) of
          '', '.', '.mdb': dbType := 'CREATE_DBV4="' + ADatabaseFile + '"';
          '.accdb'       : dbtype := 'CREATE_DBV12="' + ADatabaseFile + '"';
        else
          raise Exception.CreateFmt('File format "%s" not supported.', [ExtractFileExt(ADatabaseFile)]);
        end;
  end;

  retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, PChar(driver), PChar(dbType));
  // returns 1 in case of success, 0 in case of failure
  if retCode <> 0 then begin
    if not FileExists(ADatabaseFile) then
      AErrorMsg := 'Successful creation reported, but file not found.'
    else
      Result := true
  end else
  begin
    ErrorCode := 0;
    ResizeErrorMessage := 0;
    // todo: verify how the DLL is called - use pointers?; has not been tested.
    GetMem(ErrorMessage, 512);
    try
      SQLInstallerError(1, @ErrorCode, ErrorMessage, SizeOf(ErrorMessage), @ResizeErrorMessage);
      L := TStringList.Create;
      try
        L.Delimiter := ';';
        L.StrictDelimiter := true;
        L.DelimitedText := ErrorMessage;
        AErrorMsg := L.Text;
      finally
        L.Free;
      end;
    finally
      FreeMem(ErrorMessage);
    end;
  end;
end;

procedure TForm1.CreateContactsTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Contacts ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER,' +
      'FirstName VARCHAR(50) ,'+
      'LastName VARCHAR(50) , '+
      'Birthdate DATE, '+
      'Anniversary DATE, '+
      'Title VARCHAR(50), '+
      'Company VARCHAR(50), '+
      'Department VARCHAR(50), '+
      'Job_Position VARCHAR(30), '+
      'AddressType1 INTEGER, '+
      'Address1 VARCHAR(100), '+
      'City1 VARCHAR(50), '+
      'State1 VARCHAR(25), '+
      'Zip1 VARCHAR(10), '+
      'Country1 VARCHAR(25), '+
      'AddressType2 INTEGER, '+
      'Address2 VARCHAR(100), '+
      'City2 VARCHAR(50), '+
      'State2 VARCHAR(25), '+
      'Zip2 VARCHAR(10), '+
      'Country2 VARCHAR(25), '+
      'Notes VARCHAR, '+
      'EMail1 VARCHAR(100), '+
      'EMail2 VARCHAR(100), '+
      'EMail3 VARCHAR(100), '+
      'EMailType1 INTEGER, '+
      'EMailType2 INTEGER, '+
      'EMailType3 INTEGER, '+
      'Phone1 VARCHAR(25), '+
      'Phone2 VARCHAR(25), '+
      'Phone3 VARCHAR(25), '+
      'Phone4 VARCHAR(25), '+
      'Phone5 VARCHAR(25), '+
      'PhoneType1 INTEGER, '+
      'PhoneType2 INTEGER, '+
      'PhoneType3 INTEGER, '+
      'PhoneType4 INTEGER, '+
      'PhoneType5 INTEGER, '+
      'Website1 VARCHAR(100), '+
      'Website2 VARCHAR(100), '+
      'WebsiteType1 INTEGER, '+
      'WebsiteType2 INTEGER, '+
      'Category INTEGER, '+
      'Custom1 VARCHAR(100), '+
      'Custom2 VARCHAR(100),'+
      'Custom3 VARCHAR(100), '+
      'Custom4 VARCHAR(100) )'
    );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piCRecordID ON Contacts(RecordID) WITH PRIMARY');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCResourceID ON Contacts(ResourceID)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCName ON Contacts(LastName, FirstName)' );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siCCompany ON Contacts(Company)');
  StatusMsg('Table "Contacts" created.');
end;

procedure TForm1.CreateEventsTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Events ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER, '+
      'StartTime DATETIME, '+
      'EndTime DATETIME, '+
      'Description VARCHAR(255), '+
      'Location VARCHAR(255), '+
      'Notes VARCHAR, ' +
      'Category INTEGER, '+
      'AllDayEvent LOGICAL, '+
      'DingPath VARCHAR(255), '+
      'AlarmSet LOGICAL, '+
      'AlarmAdvance INTEGER, '+
      'AlarmAdvanceType INTEGER, '+
      'SnoozeTime DATETIME, '+
      'RepeatCode INTEGER, '+
      'RepeatRangeEnd DATETIME, '+
      'CustomInterval INTEGER)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piERecordID ON Events(RecordID) WITH PRIMARY');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EResourceID ON Events(ResourceID)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EStartTime ON Events(StartTime)');
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX EEndTime ON Events(EndTime)');

  StatusMsg('Table "Events" created.');
end;

procedure TForm1.CreateResourceTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Resources ( '+
       'ResourceID COUNTER, ' +
       'Description VARCHAR(255), '+
       'Notes VARCHAR, '+                           // 1024 --> -
       'ImageIndex INTEGER, '+
       'ResourceActive LOGICAL, '+                  // BOOL --> LOGICAL
       'UserField0 VARCHAR(100), '+
       'UserField1 VARCHAR(100), '+
       'UserField2 VARCHAR(100), '+
       'UserField3 VARCHAR(100), '+
       'UserField4 VARCHAR(100), '+
       'UserField5 VARCHAR(100), '+
       'UserField6 VARCHAR(100), '+
       'UserField7 VARCHAR(100), '+
       'UserField8 VARCHAR(100), '+
       'UserField9 VARCHAR(100) )'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piRResourceID ON Resources(ResourceID) WITH PRIMARY'
  );
  StatusMsg('Table "Resources" created.');
end;

procedure TForm1.CreateTasksTable;
begin
  ODBCConnection1.ExecuteDirect(
    'CREATE TABLE Tasks ('+
      'RecordID COUNTER, ' +
      'ResourceID INTEGER, '+
      'Complete LOGICAL, '+
      'Description VARCHAR(255), '+
      'Details VARCHAR, '+
      'CreatedOn DATETIME, '+
      'Priority INTEGER, '+
      'Category INTEGER, '+
      'CompletedOn DATETIME, '+
      'DueDate DATETIME)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE UNIQUE INDEX piTRecordID ON Tasks(RecordID) WITH PRIMARY'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siTDueDate ON Tasks(DueDate)'
  );
  ODBCConnection1.ExecuteDirect(
    'CREATE INDEX siTCompletedOn ON Tasks(CompletedOn)'
  );
  StatusMsg('Table "Tasks" created.');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FilenameEdit.ButtonWidth := FilenameEdit.Height;
end;

procedure TForm1.RgFormatClick(Sender: TObject);
begin
  if FilenameEdit.Filename <> '' then
    FilenameEdit.FileName := ChangeFileExt(FileNameEdit.FileName, EXT[RgFormat.ItemIndex]);
end;

procedure TForm1.StatusMsg(const AText: String);
begin
  Statusbar1.SimpleText := AText;
  Application.ProcessMessages;
end;

end.

