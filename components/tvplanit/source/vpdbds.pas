{*********************************************************}
{*                  VPDBDS.PAS 1.03                      *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}
unit VpDBDS;

interface

uses
  {$IFDEF LCL}
  LCLProc, LCLType, LCLIntf,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, Dialogs, SysUtils, Db,
  VpBase, VpData, VpBaseDS, VpConst, VpException;

type
  TVpCustomDBDataStore = class(TVpCustomDataStore)
  protected {private}
    FReadOnly : Boolean;
    FAfterPostEvents : TNotifyEvent;
    FAfterPostTasks : TNotifyEvent;
    FAfterPostContacts : TNotifyEvent;
    FAfterPostResources : TNotifyEvent;

    { property getters }
    function GetResourceTable : TDataset; virtual; abstract;
    function GetEventsTable : TDataset; virtual; abstract;
    function GetContactsTable : TDataset; virtual; abstract;
    function GetTasksTable : TDataset; virtual; abstract;

    { property setters }
    procedure SetReadOnly(const Value: boolean);

    { internal methods }
    procedure LoadContact(AContact: TVpContact); virtual;
    procedure LoadTask(ATask: TVpTask); virtual;
    procedure SetFilterCriteria(ATable: TDataset; AUseDateTime: Boolean;
      AResourceID: Integer; AStartDateTime, AEndDateTime: TDateTime); virtual;

  protected {properties that may be surfaced later}
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property ResourceTable: TDataset read GetResourceTable;
    property EventsTable: TDataset read GetEventsTable;
    property ContactsTable: TDataset read GetContactsTable;
    property TasksTable: TDataset read GetTasksTable;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load; override;
    procedure LoadEventsOfResource(AResID: integer); override;
    procedure LoadContacts; override;
    procedure LoadTasks; override;

    procedure RefreshEvents; override;
    procedure RefreshContacts; override;
    procedure RefreshTasks; override;
    procedure RefreshResource; override;

    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;
    procedure PostResources; override;

    procedure PurgeResource(Res: TVpResource); override;
    procedure PurgeEvents(Res: TVpResource); override;
    procedure PurgeContacts(Res: TVpResource); override;
    procedure PurgeTasks(Res: TVpResource); override;

    procedure SetResourceByName(Value: string); override;
    procedure CreateFieldDefs(const TableName: string; FieldDefs: TFieldDefs); virtual;
    procedure CreateIndexDefs(const TableName: string; IndexDefs: TIndexDefs); virtual;

  published
    {events}
    property AfterPostEvents : TNotifyEvent
      read FAfterPostEvents write FAfterPostEvents;
    property AfterPostContacts : TNotifyEvent
      read FAfterPostContacts write FAfterPostContacts;
    property AfterPostTasks : TNotifyEvent
      read FAfterPostTasks write FAfterPostTasks;
    property AfterPostResources : TNotifyEvent
      read FAfterPostResources write FAfterPostResources;
  end;

{ Constants for index names }
const
  VpcIndexNameResID = 'ResID_ndx';

implementation
{$IFDEF VERSION6}
  uses
    Variants;
{$ENDIF}

{=== TVpCustomDBDataStore ====================================================}
constructor TVpCustomDBDataStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { initialize internal fields }
  FReadOnly := False;
end;

procedure TVpCustomDBDataStore.CreateFieldDefs(const TableName: string;
  FieldDefs: TFieldDefs);
begin
  if TableName = ResourceTableName then begin
    with FieldDefs do begin
      Clear;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Description }
      with AddFieldDef do begin
        Name := 'Description';
        DataType := ftString;
        Size := 255;
        Required := false;
      end;
      { Notes }
      with AddFieldDef do begin
        Name := 'Notes';
        DataType := ftString;
        Size := 1024;
        Required := false;
      end;
      { Image Index }
      with AddFieldDef do begin
        Name := 'ImageIndex';
        DataType := ftInteger;
        Required := false;
      end;
      { Active }
      with AddFieldDef do begin
        Name := 'ResourceActive';
        DataType := ftBoolean;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end; {with FieldDefs do}
  end else
  if TableName = EventsTableName then begin
    with FieldDefs do begin
      Clear;
      { Record ID }
      with AddFieldDef do begin
        Name := 'RecordID';
        DataType := ftInteger;
        Required := true;
      end;
      { StartTime }
      with AddFieldDef do begin
        Name := 'StartTime';
        DataType := ftDateTime;
        Required := true;
      end;
      { EndTime }
      with AddFieldDef do begin
        Name := 'EndTime';
        DataType := ftDateTime;
        Required := true;
      end;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Description }
      with AddFieldDef do begin
        Name := 'Description';
        DataType := ftString;
        Size := 255;
        Required := false;
      end;
      { Locataion }              // new
      with AddFieldDef do begin
        Name := 'Location';
        DataType := ftString;
        Size := 255;
        Required := false;
      end;
      { Note }
      with AddFieldDef do begin
        Name := 'Notes';
        DataType := ftString;
        Size := 1024;
        Required := false;
      end;
      { Category }
      with AddFieldDef do begin
        Name := 'Category';
        DataType := ftInteger;
        Required := false;
      end;
      { AllDayEvent }
      with AddFieldDef do begin
        Name := 'AllDayEvent';
        DataType := ftBoolean;
        Required := false;
      end;
      { DingPath }
      with AddFieldDef do begin
        Name := 'DingPath';
        DataType := ftString;
        Size := 255;
        Required := false;
      end;
      { AlarmSet }
      with AddFieldDef do begin
        Name := 'AlarmSet';
        DataType := ftBoolean;
        Required := false;
      end;
      { Alarm Advance }
      with AddFieldDef do begin
        Name := 'AlarmAdvance';
        DataType := ftInteger;
        Required := false;
      end;
      { Alarm Advance Type }
      with AddFieldDef do begin
        Name := 'AlarmAdvanceType';
        DataType := ftInteger;
        Required := false;
      end;
      { SnoozeTime }
      with AddFieldDef do begin
        Name := 'SnoozeTime';
        DataType := ftDateTime;
        Required := false;
      end;
      { Repeat Code }
      with AddFieldDef do begin
        Name := 'RepeatCode';
        DataType := ftInteger;
        Required := false;
      end;
      { Repeat Range End }
      with AddFieldDef do begin
        Name := 'RepeatRangeEnd';
        DataType := ftDateTime;
        Required := false;
      end;
      { Custom Repeat Interval }
      with AddFieldDef do begin
        Name := 'CustomInterval';
        DataType := ftInteger;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end; {with FieldDefs do}
  end else
  if TableName = ContactsTableName then begin
    with FieldDefs do begin
      Clear;
      { Record ID }
      with AddFieldDef do begin
        Name := 'RecordID';
        DataType := ftInteger;
        Required := true;
      end;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { FirstName }
      with AddFieldDef do begin
        Name := 'FirstName';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { LastName }
      with AddFieldDef do begin
        Name := 'LastName';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { Birthdate }
      with AddFieldDef do begin
        Name := 'Birthdate';
        DataType := ftDate;
        Required := false;
      end;
      { Anniversary }
      with AddFieldDef do begin
        Name := 'Anniversary';
        DataType := ftDate;
        Required := false;
      end;
      { Title }
      with AddFieldDef do begin
        Name := 'Title';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { Company }
      with AddFieldDef do begin
        Name := 'Company';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { Department }
      with AddFieldDef do begin
        Name := 'Department';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { Position }
      with AddFieldDef do begin
        Name := 'Job_Position';
        DataType := ftString;
        Size := 30;
        Required := false;
      end;
      { AddressType1 }
      with AddFieldDef do begin
        Name := 'AddressType1';
        DataType := ftInteger;
        Required := false;
      end;
      { Address1 }
      with AddFieldDef do begin
        Name := 'Address1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { City1 }
      with AddFieldDef do begin
        Name := 'City1';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { State1 }
      with AddFieldDef do begin
        Name := 'State1';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Zip1 }
      with AddFieldDef do begin
        Name := 'Zip1';
        DataType := ftString;
        Size := 10;
        Required := false;
      end;
      { Country1 }
      with AddFieldDef do begin
        Name := 'Country1';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { AddressType2 }
      with AddFieldDef do begin
        Name := 'AddressType2';
        DataType := ftInteger;
        Required := false;
      end;
      { Address2 }
      with AddFieldDef do begin
        Name := 'Address2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { City2 }
      with AddFieldDef do begin
        Name := 'City2';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { State2 }
      with AddFieldDef do begin
        Name := 'State2';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Zip2 }
      with AddFieldDef do begin
        Name := 'Zip2';
        DataType := ftString;
        Size := 10;
        Required := false;
      end;
      { Country2 }
      with AddFieldDef do begin
        Name := 'Country2';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Note }
      with AddFieldDef do begin
        Name := 'Notes';          // was: "Note" in old version
        DataType := ftString;
        Size := 1024;
        Required := false;
      end;
      { Phone1 }
      with AddFieldDef do begin
        Name := 'Phone1';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone2 }
      with AddFieldDef do begin
        Name := 'Phone2';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone3 }
      with AddFieldDef do begin
        Name := 'Phone3';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone4 }
      with AddFieldDef do begin
        Name := 'Phone4';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone5 }
      with AddFieldDef do begin
        Name := 'Phone5';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { PhoneType1 }
      with AddFieldDef do begin
        Name := 'PhoneType1';
        DataType := ftInteger;
        Required := false;
      end;
      { PhoneType2 }
      with AddFieldDef do begin
        Name := 'PhoneType2';
        DataType := ftInteger;
        Required := false;
      end;
      { PhoneType3 }
      with AddFieldDef do begin
        Name := 'PhoneType3';
        DataType := ftInteger;
        Required := false;
      end;
      { PhoneType4 }
      with AddFieldDef do begin
        Name := 'PhoneType4';
        DataType := ftInteger;
        Required := false;
      end;
      { PhoneType5 }
      with AddFieldDef do begin
        Name := 'PhoneType5';
        DataType := ftInteger;
        Required := false;
      end;
      { EMail1 }
      with AddFieldDef do begin
        Name := 'EMail1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { EMail2 }
      with AddFieldDef do begin
        Name := 'EMail2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { EMail3 }
      with AddFieldDef do begin
        Name := 'EMail3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { EMailType1 }
      with AddFieldDef do begin
        Name := 'EMailType1';
        DataType := ftInteger;
        Required := false;
      end;
      { EMailType2 }
      with AddFieldDef do begin
        Name := 'EMailType2';
        DataType := ftInteger;
        Required := false;
      end;
      { EMailType3 }
      with AddFieldDef do begin
        Name := 'EMailType3';
        DataType := ftInteger;
        Required := false;
      end;
      { Website1 }
      with AddFieldDef do begin
        Name := 'Website1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Website2 }
      with AddFieldDef do begin
        Name := 'Website2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { WebsiteType1 }
      with AddFieldDef do begin
        Name := 'WebsiteType1';
        DataType := ftInteger;
        Required := false;
      end;
      { WebsiteType2 }
      with AddFieldDef do begin
        Name := 'WebsiteType2';
        DataType := ftInteger;
        Required := false;
      end;
      { Category }
      with AddFieldDef do begin
        Name := 'Category';
        DataType := ftInteger;
        Required := false;
      end;
      { Custom1 }
      with AddFieldDef do begin
        Name := 'Custom1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Custom2 }
      with AddFieldDef do begin
        Name := 'Custom2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Custom3 }
      with AddFieldDef do begin
        Name := 'Custom3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Custom4 }
      with AddFieldDef do begin
        Name := 'Custom4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end; {with FieldDefs do}
  end else
  if TableName = TasksTableName then begin
    with FieldDefs do begin
      Clear;
      { Record ID }
      with AddFieldDef do begin
        Name := 'RecordID';
        DataType := ftInteger;
        Required := true;
      end;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Complete }
      with AddFieldDef do begin
        Name := 'Complete';
        DataType := ftBoolean;
        Required := false;
      end;
      { Description }
      with AddFieldDef do begin
        Name := 'Description';
        DataType := ftString;
        Size := 255;
        Required := false;
      end;
      { Details }
      with AddFieldDef do begin
        Name := 'Details';
        DataType := ftString;
        Size := 1024;
        Required := false;
      end;
      { Created On Date }
      with AddFieldDef do begin
        Name := 'CreatedOn';
        DataType := ftDateTime;
        Required := false;
      end;
      { Priority }
      with AddFieldDef do begin
        Name := 'Priority';
        DataType := ftInteger;
        Required := false;
      end;
      { Category }
      with AddFieldDef do begin
        Name := 'Category';
        DataType := ftInteger;
        Required := false;
      end;
      { Completed On Date }
      with AddFieldDef do begin
        Name := 'CompletedOn';
        DataType := ftDateTime;
        Required := false;
      end;
      { Due Date }
      with AddFieldDef do begin
        Name := 'DueDate';
        DataType := ftDateTime;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end; {with FieldDefs do}
  end else if TableName = RecordIDTableName then begin
    { The RecordID Table has only one record with 4 fields                }
    { each field contains the last record ID for each of the other tables }
    with FieldDefs do begin
      Clear;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Event ID }
      with AddFieldDef do begin
        Name := 'EventID';
        DataType := ftInteger;
        Required := true;
      end;
      { Task ID }
      with AddFieldDef do begin
        Name := 'TaskID';
        DataType := ftInteger;
        Required := true;
      end;
      { Contact ID }
      with AddFieldDef do begin
        Name := 'ContactID';
        DataType := ftInteger;
        Required := true;
      end;
    end; {with FieldDefs do}
  end;
end;

procedure TVpCustomDBDataStore.CreateIndexDefs(const TableName: string;
  IndexDefs: TIndexDefs);
begin
  if TableName = ResourceTableName then begin
    with IndexDefs do begin
      with AddIndexDef do begin
        Name := VpcIndexNameResID;
        Fields := 'ResourceID';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'Descr_ndx';
        Fields := 'Description';
        Options := [ixCaseInsensitive];
      end;
    end;
  end else if TableName = EventsTableName then begin
    with IndexDefs do begin
      { added a new index on the ResourceID and StartTime fields.              }
      { this index is used by the FlashFiler 2 DataStore for setting ranges    }
      { instead of using filters on the Events table.                          }
      { tables created with unpatched 1.0 or 1.01 versions of Visual PlanIt    }
      { will not have this new index available so they will continue to use    }
      { filters.                                                               }
      with AddIndexDef do begin
        Name := 'rid_st_ndx';
        Fields := 'ResourceID;StartTime';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'st_ndx';
        Fields := 'StartTime';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'et_ndx';
        Fields := 'EndTime';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := VpcIndexNameResID;
        Fields := 'ResourceID';
        Options := [ixCaseInsensitive];
      end;
    end;
  end else if TableName = ContactsTableName then begin
    with IndexDefs do begin
      with AddIndexDef do begin
        Name := VpcIndexNameResID;
        Fields := 'ResourceID';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'LName_ndx';
        Fields := 'LastName';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'Company_ndx';
        Fields := 'Company';
        Options := [ixCaseInsensitive];
      end;
    end;
  end else if TableName = TasksTableName then begin
    with IndexDefs do begin
      with AddIndexDef do begin
        Name := VpcIndexNameResID;
        Fields := 'ResourceID';
        Options := [ixCaseInsensitive];
      end;
      { Rule: A single field case sensitive index on a Paradox table must }
      {       have the same name as the field the index is on.            }
      with AddIndexDef do begin
        Name := 'DueDate';
        Fields := 'DueDate';
        Options := [];
      end;
      with AddIndexDef do begin
        Name := 'CompletedOn';
        Fields := 'CompletedOn';
        Options := [];
      end;
    end;
  end;
end;

destructor TVpCustomDBDataStore.Destroy;
begin
  inherited Destroy;
end;

procedure TVpCustomDBDataStore.Load;
var
  Res: TVpResource;
  F: TField;
begin
  Loading := true;
  try
    if not ResourceTable.Active then
      ResourceTable.Open;

    Resource := nil;
    Resources.ClearResources;

    ResourceTable.First;
    while not ResourceTable.EOF do begin
      { Load this resource into memory }
      Res := Resources.AddResource(-1);
      Res.Loading := true;
      Res.ResourceID := ResourceTable.FieldByName('ResourceID').AsInteger;
      Res.Description := ResourceTable.FieldByName('Description').AsString;
      Res.Notes := ResourceTable.FieldByName('Notes').AsString;
      Res.ResourceActive := ResourceTable.FieldByName('ResourceActive').AsBoolean;
      F := ResourceTable.FindField('UserField0');
      if F <> nil then Res.UserField0 := F.AsString;
      F := ResourceTable.FindField('UserField1');
      if F <> nil then Res.UserField1 := F.AsString;
      F := ResourceTable.FindField('UserField2');
      if F <> nil then Res.UserField2 := F.AsString;
      F := ResourceTable.FindField('UserField3');
      if F <> nil then Res.UserField3 := F.AsString;
      F := ResourceTable.FindField('UserField4');
      if F <> nil then Res.UserField4 := F.AsString;
      F := ResourceTable.FindField('UserField5');
      if F <> nil then Res.UserField5 := F.AsString;
      F := ResourceTable.FindField('UserField6');
      if F <> nil then Res.UserField6 := F.AsString;
      F := ResourceTable.FindField('UserField7');
      if F <> nil then Res.UserField7 := F.AsString;
      F := ResourceTable.FindField('UserField8');
      if F <> nil then Res.UserField8 := F.AsString;
      F := ResourceTable.FindField('UserField9');
      if F <> nil then Res.UserField9 := F.AsString;
      Res.Loading := false;

      { Add events, contacts and tasks for the currently selected resource }
      if (Res.ResourceID = ResourceID) and Res.ResourceActive then begin
        Resource := Res;
        LoadEvents;
        LoadContacts;
        LoadTasks;
      end;

      ResourceTable.Next;
    end;
  finally
    Loading := false;
  end;
  
  inherited;
end;

{ Load the events of the specified resource into memory. The loaded events are
  attached to the current resource. Note that if AResID is different from the
  current resource id then the events are used for overlaying. }
procedure TVpCustomDBDataStore.LoadEventsOfResource(AResID: Integer);
var
  Event: TVpEvent;
  F: TField;
begin
  if Resource <> nil then
    with EventsTable do begin
      SetFilterCriteria(EventsTable, true, AResID, TimeRange.StartTime, TimeRange.EndTime);
      First;
      while not EventsTable.EOF do begin
        Event := Resource.Schedule.AddEvent(
          FieldByName('RecordID').AsInteger,
          FieldByName('StartTime').AsDateTime,
          FieldByName('EndTime').AsDateTime
        );
        if Event <> nil then begin
          Event.Loading := true;
          Event.ResourceID := AResID;
          Event.Description := FieldByName('Description').AsString;
          F := FieldByName('Location');
          if F <> nil then Event.Location := F.AsString;
          Event.Notes := FieldByName('Notes').AsString;
          Event.Category := FieldByName('Category').AsInteger;
          Event.DingPath := FieldByName('DingPath').AsString;
          Event.AllDayEvent := FieldByName('AllDayEvent').AsBoolean;
          Event.AlarmSet := FieldByName('AlarmSet').AsBoolean;
          Event.AlarmAdvance := FieldByName('AlarmAdvance').AsInteger;
          Event.AlarmAdvanceType := TVpAlarmAdvType(FieldByName('AlarmAdvanceType').AsInteger);
          Event.SnoozeTime := FieldByName('SnoozeTime').AsDateTime;
          Event.RepeatCode := TVpRepeatType(FieldByName('RepeatCode').AsInteger);
          Event.RepeatRangeEnd := FieldByName('RepeatRangeEnd').AsDateTime;
          Event.CustomInterval := FieldByName('CustomInterval').AsInteger;
          F := FindField('UserField0');
          if F <> nil then Event.UserField0 := F.AsString;
          F := FindField('UserField1');
          if F <> nil then Event.UserField1 := F.AsString;
          F := FindField('UserField2');
          if F <> nil then Event.UserField2 := F.AsString;
          F := FindField('UserField3');
          if F <> nil then Event.UserField3 := F.AsString;
          F := FindField('UserField4');
          if F <> nil then Event.UserField4 := F.AsString;
          F := FindField('UserField5');
          if F <> nil then Event.UserField5 := F.AsString;
          F := FindField('UserField6');
          if F <> nil then Event.UserField6 := F.AsString;
          F := FindField('UserField7');
          if F <> nil then Event.UserField7 := F.AsString;
          F := FindField('UserField8');
          if F <> nil then Event.UserField8 := F.AsString;
          F := FindField('UserField9');
          if F <> nil then Event.UserField9 := F.AsString;
          Event.Loading := false;
        end;
        Next;
      end; {while}
    end; {with EventsTable}
end;

{ Loads the contact from the current cursor position of the contacts table }
procedure TVpCustomDBDataStore.LoadContact(AContact: TVpContact);
var
  F: TField;
begin
  with ContactsTable do
  begin
    AContact.RecordID := FieldByName('RecordID').AsInteger;
    AContact.FirstName := FieldByName('FirstName').AsString;
    AContact.LastName := FieldByName('LastName').AsString;
    AContact.Category := FieldByName('Category').AsInteger;
    AContact.Birthdate := FieldByName('BirthDate').AsDateTime;
    AContact.Anniversary := FieldByName('Anniversary').AsDateTime;
    AContact.Title := FieldByName('Title').AsString;
    AContact.Company := FieldByName('Company').AsString;
    AContact.Job_Position := FieldByName('Job_Position').AsString;

    // Department -- new in 1.05
    F := FindField('Department');
    if F <> nil then AContact.Department := F.AsString;

    F := FindField('Notes');
    if F = nil then F := FindField('Note');  // deprecated
    if F <> nil then AContact.Notes := F.AsString;

    // two address types - new in 1.05
    F := FindField('AddressType1');
    if F <> nil then AContact.AddressType1 := F.AsInteger;
    F := FindField('AddressType2');
    if F <> nil then AContact.AddressType2 := F.AsInteger;
    F := FindField('Address1');
    if F = nil then F := FindField('Address');
    if F <> nil then AContact.Address1 := F.AsString;
    F := FindField('Address2');
    if F <> nil then AContact.Address2 := F.AsString;
    F := FindField('City1');
    if F = nil then F := FindField('City');
    if F <> nil then AContact.City1 := F.AsString;
    F := FindField('City2');
    if F <> nil then AContact.City2 := F.AsString;
    F := FindField('State1');
    if F = nil then F := FindField('State');
    if F <> nil then AContact.State1 := F.AsString;
    F := FindField('State2');
    if F <> nil then AContact.State2 := F.AsString;
    F := FindField('Zip1');
    if F = nil then F := FindField('Zip');
    if F <> nil then AContact.Zip1 := F.AsString;
    F := FindField('Zip2');
    if F <> nil then AContact.Zip2 := F.AsString;
    F := FindField('Country1');
    if F = nil then F := FindField('Country');
    if F <> nil then AContact.Country1 := F.AsString;
    F := FindField('Country2');
    if F <> nil then AContact.Country2 := F.AsString;
    {
    Contact.Address := FieldByName('Address').AsString;
    Contact.City := FieldByName('City').AsString;
    Contact.State := FieldByName('State').AsString;
    Contact.Zip := FieldByName('Zip').AsString;
    Contact.Country := FieldByName('Country').AsString;
    }

    AContact.Phone1 := FieldByName('Phone1').AsString;
    AContact.Phone2 := FieldByName('Phone2').AsString;
    AContact.Phone3 := FieldByName('Phone3').AsString;
    AContact.Phone4 := FieldByName('Phone4').AsString;
    AContact.Phone5 := FieldByName('Phone5').AsString;
    AContact.PhoneType1 := FieldByName('PhoneType1').AsInteger;
    AContact.PhoneType2 := FieldByName('PhoneType2').AsInteger;
    AContact.PhoneType3 := FieldByName('PhoneType3').AsInteger;
    AContact.PhoneType4 := FieldByName('PhoneType4').AsInteger;
    AContact.PhoneType5 := FieldByName('PhoneType5').AsInteger;

    // EMail fields - new in 1.05
    F := FindField('EMail1');
    if F = nil then F := FindField('EMail');
    if F <> nil then AContact.EMail1 := F.AsString;
    F := FindField('EMail2');
    if F <> nil then AContact.EMail2 := F.AsString;
    F := FindField('EMail3');
    if F <> nil then AContact.EMail3 := F.AsString;
    F := FindField('EMailType1');
    if F <> nil then AContact.EMailType1 := F.AsInteger;
    F := FindField('EMailType2');
    if F <> nil then AContact.EMailType2 := F.AsInteger;
    F := FindField('EMailType3');
    if F <> nil then AContact.EMailType3 := F.AsInteger;

    // Website fields - new in 1.05
    F := FindField('Website1');
    if F <> nil then AContact.Website1 := F.AsString;
    F := FindField('Website2');
    if F <> nil then AContact.Website2 := F.AsString;
    F := FindField('WebsiteType1');
    if F <> nil then AContact.WebsiteType1 := F.AsInteger;
    F := FindField('WebsiteType2');
    if F <> nil then AContact.WebsiteType2 := F.AsInteger;

    // Custom fields
    F := FindField('Custom1');
    if F <> nil then AContact.Custom1 := F.AsString;
    F := FindField('Custom2');
    if F <> nil then AContact.Custom2 := F.AsString;
    F := FindField('Custom3');
    if F <> nil then AContact.Custom3 := F.AsString;
    F := FindField('Custom4');
    if F <> nil then AContact.Custom4 := F.AsString;

    // User-defined fields
    F := FindField('UserField0');
    if F <> nil then AContact.UserField0 := F.AsString;
    F := FindField('UserField1');
    if F <> nil then AContact.UserField1 := F.AsString;
    F := FindField('UserField2');
    if F <> nil then AContact.UserField2 := F.AsString;
    F := FindField('UserField3');
    if F <> nil then AContact.UserField3 := F.AsString;
    F := FindField('UserField4');
    if F <> nil then AContact.UserField4 := F.AsString;
    F := FindField('UserField5');
    if F <> nil then AContact.UserField5 := F.AsString;
    F := FindField('UserField6');
    if F <> nil then AContact.UserField6 := F.AsString;
    F := FindField('UserField7');
    if F <> nil then AContact.UserField7 := F.AsString;
    F := FindField('UserField8');
    if F <> nil then AContact.UserField8 := F.AsString;
    F := FindField('UserField9');
    if F <> nil then AContact.UserField9 := F.AsString;
  end;
end;

procedure TVpCustomDBDataStore.LoadContacts;
var
  contact: TVpContact;
begin
  if (Resource <> nil) then
    with ContactsTable do begin
      SetFilterCriteria(ContactsTable, False,
        ResourceTable.FieldByName('ResourceID').AsInteger,
        0, 0
      );
      First;
      while not EOF do begin
        contact := Resource.Contacts.AddContact(GetNextID(ContactsTableName));
        contact.Loading := true;
        LoadContact(contact);
        contact.Loading := false;
        Next;
      end;
    end;
end;

{ Loads the task from the current cursor position of the task table }
procedure TVpCustomDBDatastore.LoadTask(ATask: TVpTask);
var
  F: TField;
begin
  with TasksTable do begin
    ATask.RecordID := FieldByName('RecordID').AsInteger;
    ATask.Complete := FieldByName('Complete').AsBoolean;
    ATask.Description := FieldByName('Description').AsString;
    ATask.Details := FieldByName('Details').AsString;
    ATask.CreatedOn := FieldByName('CreatedOn').AsDateTime;
    ATask.CompletedOn := FieldByName('CompletedOn').AsDateTime;
    ATask.Priority := FieldByName('Priority').AsInteger;
    ATask.Category := FieldByName('Category').AsInteger;
    ATask.DueDate := FieldByName('DueDate').AsDateTime;
    F := FindField('UserField0');
    if F <> nil then ATask.UserField0 := F.AsString;
    F := FindField('UserField1');
    if F <> nil then ATask.UserField1 := F.AsString;
    F := FindField('UserField2');
    if F <> nil then ATask.UserField2 := F.AsString;
    F := FindField('UserField3');
    if F <> nil then ATask.UserField3 := F.AsString;
    F := FindField('UserField4');
    if F <> nil then ATask.UserField4 := F.AsString;
    F := FindField('UserField5');
    if F <> nil then ATask.UserField5 := F.AsString;
    F := FindField('UserField6');
    if F <> nil then ATask.UserField6 := F.AsString;
    F := FindField('UserField7');
    if F <> nil then ATask.UserField7 := F.AsString;
    F := FindField('UserField8');
    if F <> nil then ATask.UserField8 := F.AsString;
    F := FindField('UserField9');
    if F <> nil then ATask.UserField9 := F.AsString;
  end;
end;

procedure TVpCustomDBDataStore.LoadTasks;
var
  Task: TVpTask;
begin
  if (Resource <> nil) then
    with TasksTable do begin
      SetFilterCriteria(TasksTable, False,
        ResourceTable.FieldByName('ResourceID').AsInteger,
        0, 0
      );
      First;
      while not EOF do begin
        Task := Resource.Tasks.AddTask(GetNextID(TasksTableName));
        Task.loading := true;
        LoadTask(Task);
        Task.Loading := false;
        Next;
      end; {while}
    end; {with TasksTable}
end;

procedure TVpCustomDBDataStore.PostContacts;
var
  I: Integer;
  Contact: TVpContact;
  F: TField;
begin
  if (Resource <> nil) and Resource.ContactsDirty then begin
    { Dump this resource's dirty contacts to the DB }
    if ResourceTable.Locate('ResourceID', Resource.ResourceID, []) then begin
      SetFilterCriteria(ContactsTable, False, Resource.ResourceID, 0, 0);

      for I := pred(Resource.Contacts.Count) downto 0 do begin
        Contact := Resource.Contacts.GetContact(I);
        { if the delete flag is set then delete the record }
        { and free the event instance }
        if Contact.Deleted then begin
          if ContactsTable.Locate('RecordID', Contact.RecordID, [])
          then ContactsTable.Delete;
          Contact.Free;
          Continue;
        end;

        if Contact.Changed then begin
          if ContactsTable.Locate('RecordID', Contact.RecordID, []) then
            { this event already exists in the database so update it }
            ContactsTable.Edit
          else begin
            { this record doesn't exist in the database, so it's a new event }
            ContactsTable.Append;
          end;
          try
            { DataStore descendants that can use an autoincrement RecordID }
            { field set the RecordID to -1 by default.  If the RecordID is }
            { -1 then this is a new record and we shouldn't overwrite      }
            { RecordID with a bogus value }
            if Contact.RecordID > -1 then
              ContactsTable.FieldByName('RecordID').AsInteger := Contact.RecordID;

            ContactsTable.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
            ContactsTable.FieldByName('FirstName').AsString := Contact.FirstName;
            ContactsTable.FieldByName('LastName').AsString := Contact.LastName;
            ContactsTable.FieldByName('Birthdate').AsDateTime := Contact.Birthdate;
            ContactsTable.FieldByName('Anniversary').AsDateTime := Contact.Anniversary;
            ContactsTable.FieldByName('Title').AsString := Contact.Title;
            ContactsTable.FieldByName('Company').AsString := Contact.Company;
            ContactsTable.FieldByName('Job_Position').AsString := Contact.Job_Position;
            ContactsTable.FieldByName('Category').AsInteger := Contact.Category;

            F := ContactsTable.FindField('Notes');
            if F = nil then F := ContactsTable.FindField('Note');
            if F <> nil then F.AsString := Contact.Notes;

            // Department -- new in 1.05
            F := ContactsTable.FindField('Department');
            if F <> nil then F.AsString := Contact.Department;

            // two address types -- new in 1.05
            F := ContactsTable.FindField('AddressType1');
            if F <> nil then F.AsInteger := Contact.AddressType1;
            F := ContactsTable.FindField('AddressType2');
            if F <> nil then F.AsInteger := Contact.AddressType2;
            F := ContactsTable.FindField('Address1');
            if F = nil then F := ContactsTable.FindField('Address');
            if F <> nil then F.AsString := Contact.Address1;
            F := ContactsTable.FindField('Address2');
            if F <> nil then F.AsString := Contact.Address2;
            F := ContactsTable.FindField('City1');
            if F = nil then F := ContactsTable.FindField('City');
            if F <> nil then F.AsString := Contact.City1;
            F := ContactsTable.FindField('City2');
            if F <> nil then F.AsString := Contact.City2;
            F := ContactsTable.FindField('State1');
            if F = nil then F := ContactsTable.FindField('State');
            if F <> nil then F.AsString := Contact.State1;
            F := ContactsTable.FindField('State2');
            if F <> nil then F.AsString := Contact.State2;
            F := ContactsTable.FindField('Zip1');
            if F = nil then F := ContactsTable.FindField('Zip');
            if F <> nil then F.AsString := Contact.Zip1;
            F := ContactsTable.FindField('Zip2');
            if F <> nil then F.AsString := Contact.Zip2;
            F := ContactsTable.FindField('Country1');
            if F = nil then F := ContactsTable.FindField('Country');
            if F <> nil then F.AsString := Contact.Country1;
            F := ContactsTable.FindField('Country2');
            if F <> nil then F.AsString := Contact.Country2;

            // Telephones
            ContactsTable.FieldByName('Phone1').AsString := Contact.Phone1;
            ContactsTable.FieldByName('Phone2').AsString := Contact.Phone2;
            ContactsTable.FieldByName('Phone3').AsString := Contact.Phone3;
            ContactsTable.FieldByName('Phone4').AsString := Contact.Phone4;
            ContactsTable.FieldByName('Phone5').AsString := Contact.Phone5;
            ContactsTable.FieldByName('PhoneType1').AsInteger := Contact.PhoneType1;
            ContactsTable.FieldByName('PhoneType2').AsInteger := Contact.PhoneType2;
            ContactsTable.FieldByName('PhoneType3').AsInteger := Contact.PhoneType3;
            ContactsTable.FieldByName('PhoneType4').AsInteger := Contact.PhoneType4;
            ContactsTable.FieldByName('PhoneType5').AsInteger := Contact.PhoneType5;

            // Three e-mail types -- new in 1.05
            F := ContactsTable.FindField('EMail1');
            if F = nil then F := ContactsTable.FindField('EMail');
            if F <> nil then F.AsString := Contact.EMail1;
            F := ContactsTable.FindField('EMail2');
            if F <> nil then F.AsString := Contact.EMail2;
            F := ContactsTable.FindField('EMail3');
            if F <> nil then F.AsString := Contact.EMail3;
            F := ContactsTable.FindField('EMailType1');
            if F <> nil then F.AsInteger := Contact.EMailType1;
            F := ContactsTable.FindField('EMailType2');
            if F <> nil then F.AsInteger := Contact.EMailType2;
            F := ContactsTable.FindField('EMailType3');
            if F <> nil then F.AsInteger := Contact.EMailType3;
            //ContactsTable.FieldByName('EMail').AsString := Contact.EMail;

            // two websites -- new in 1.05
            F := ContactsTable.FindField('Website1');
            if F <> nil then F.AsString := Contact.Website1;
            F := ContactsTable.FindField('Website2');
            if F <> nil then F.AsString := Contact.Website2;
            F := ContactsTable.FindField('WebsiteType1');
            if F <> nil then F.AsInteger := Contact.WebsiteType1;
            F := ContactsTable.FindField('WebsiteType2');
            if F <> nil then F.AsInteger := Contact.WebsiteType2;

            // Custom fields
            F := ContactsTable.FindField('Custom1');
            if F <> nil then F.AsString := Contact.Custom1;
            F := ContactsTable.FindField('Custom2');
            if F <> nil then F.AsString := Contact.Custom2;
            F := ContactsTable.FindField('Custom3');
            if F <> nil then F.AsString := Contact.Custom3;
            F := ContactsTable.FindField('Custom4');
            if F <> nil then F.AsString := Contact.Custom4;

            // User-defined fields
            F := ContactsTable.FindField('UserField0');
            if F <> nil then F.AsString := Contact.UserField0;
            F := ContactsTable.FindField('UserField1');
            if F <> nil then F.AsString := Contact.UserField1;
            F := ContactsTable.FindField('UserField2');
            if F <> nil then F.AsString := Contact.UserField2;
            F := ContactsTable.FindField('UserField3');
            if F <> nil then F.AsString := Contact.UserField3;
            F := ContactsTable.FindField('UserField4');
            if F <> nil then F.AsString := Contact.UserField4;
            F := ContactsTable.FindField('UserField5');
            if F <> nil then F.AsString := Contact.UserField5;
            F := ContactsTable.FindField('UserField6');
            if F <> nil then F.AsString := Contact.UserField6;
            F := ContactsTable.FindField('UserField7');
            if F <> nil then F.AsString := Contact.UserField7;
            F := ContactsTable.FindField('UserField8');
            if F <> nil then F.AsString := Contact.UserField8;
            F := ContactsTable.FindField('UserField9');
            if F <> nil then F.AsString := Contact.UserField9;

            ContactsTable.Post;

          except
            ContactsTable.Cancel;
            raise EDBPostError.Create;
          end;

          { DataStore descendants that can use an autoincrement RecordID }
          { field set the RecordID to -1 by default.  If the RecordID is }
          { -1 then this is a new record and we need to assign the real  }
          { record ID value from the dataset. }
          if Contact.RecordID = -1 then
            Contact.RecordID := ContactsTable.FieldByName('RecordID').AsInteger;

          Contact.Changed := false;
        end;
      end;
    end;
    Resource.ContactsDirty := false;

    if not Loading then
      NotifyDependents;

    if Assigned(AfterPostContacts) then
      FAfterPostContacts(self);
  end;
end;

procedure TVpCustomDBDataStore.PostEvents;
var
  J: Integer;
  Event: TVpEvent;
  F: TField;
begin
  if (Resource <> nil) and Resource.EventsDirty then begin
    { Dump this resource's dirty events to the DB }
    if ResourceTable.Locate('ResourceID', Resource.ResourceID, [])
    then begin
      SetFilterCriteria(EventsTable, False, Resource.ResourceID, 0, 0);

      for J := pred(Resource.Schedule.EventCount) downto 0 do begin
        Event := Resource.Schedule.GetEvent(J);

        { if the delete flag is set then delete it from the database }
        { and free the event instance }
        if Event.Deleted then begin
          if EventsTable.Locate('RecordID', Event.RecordID, []) then
            EventsTable.Delete;
          Event.Free;
          Continue;
        end;

        if Event.Changed then begin
          if EventsTable.Locate('RecordID', Event.RecordID, []) then  
            { this event already exists in the database so update it }
            EventsTable.Edit
          else begin
            EventsTable.Append;
          end;
          try
            { if a particular descendant datastore uses autoincrementing }
            { RecordID fields, then don't overwrite them here. }
            if Event.RecordID <> -1 then
              EventsTable.FieldByName('RecordID').AsInteger := Event.RecordID;

            EventsTable.FieldByName('StartTime').AsDateTime := Event.StartTime;
            EventsTable.FieldByName('EndTime').AsDateTime := Event.EndTime;
            EventsTable.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
            EventsTable.FieldByName('Description').AsString := Event.Description;
            F := EventsTable.FieldByName('Location');  // new
            if F <> nil then F.AsString := Event.Location;
            EventsTable.FieldByName('Notes').AsString := Event.Notes;
            EventsTable.FieldByName('Category').AsInteger := Event.Category;
            EventsTable.FieldByName('DingPath').AsString := Event.DingPath;
            EventsTable.FieldByName('AllDayEvent').AsBoolean := Event.AllDayEvent;
            EventsTable.FieldByName('AlarmSet').AsBoolean := Event.AlarmSet;
            EventsTable.FieldByName('AlarmAdvance').AsInteger := Event.AlarmAdvance;
            EventsTable.FieldByName('AlarmAdvanceType').AsInteger := Ord(Event.AlarmAdvanceType);
            EventsTable.FieldByName('SnoozeTime').AsDateTime := Event.SnoozeTime;
            EventsTable.FieldByName('RepeatCode').AsInteger := Ord(Event.RepeatCode);
            EventsTable.FieldByName('RepeatRangeEnd').AsDateTime := Event.RepeatRangeEnd;
            EventsTable.FieldByName('CustomInterval').AsInteger := Event.CustomInterval;

            F := EventsTable.FindField('Userfield0');
            if F <> nil then F.AsString := Event.UserField0;
            F := EventsTable.FindField('Userfield1');
            if F <> nil then F.AsString := Event.UserField1;
            F := EventsTable.FindField('Userfield2');
            if F <> nil then F.AsString := Event.UserField2;
            F := EventsTable.FindField('Userfield3');
            if F <> nil then F.AsString := Event.UserField3;
            F := EventsTable.FindField('Userfield4');
            if F <> nil then F.AsString := Event.UserField4;
            F := EventsTable.FindField('Userfield5');
            if F <> nil then F.AsString := Event.UserField5;
            F := EventsTable.FindField('Userfield6');
            if F <> nil then F.AsString := Event.UserField6;
            F := EventsTable.FindField('Userfield7');
            if F <> nil then F.AsString := Event.UserField7;
            F := EventsTable.FindField('Userfield8');
            if F <> nil then F.AsString := Event.UserField8;
            F := EventsTable.FindField('Userfield9');
            if F <> nil then F.AsString := Event.UserField9;

            EventsTable.Post;
          except
            EventsTable.Cancel;
            raise EDBPostError.Create;
          end;

          { if a particular descendant datastore uses autoincrementing    }
          { RecordID fields then the RecordID is assigned by the database }
          { and needs to be assigned here...}
          if Event.RecordID = -1 then
            Event.RecordID := EventsTable.FieldByName('RecordID').AsInteger;

          Event.Changed := false;
        end;
      end;
    end;
    Resource.EventsDirty := false;
    Resource.Schedule.Sort;

    if not Loading then
      NotifyDependents;

    if Assigned(AfterPostEvents) then
      FAfterPostEvents(self);
  end;
end;

procedure TVpCustomDBDataStore.PostTasks;
var
  I: Integer;
  Task: TVpTask;
  F: TField;
begin
  if (Resource <> nil) and Resource.TasksDirty then begin
    { Dump this resource's dirty contacts to the DB }
    if ResourceTable.Locate('ResourceID', Resource.ResourceID, [])
    then begin
      SetFilterCriteria(TasksTable, False, Resource.ResourceID, 0, 0);

      for I := pred(Resource.Tasks.Count) downto 0 do begin
        Task := Resource.Tasks.GetTask(I);

        { if the delete flag is set then delete the record }
        { and free the event instance }
        if Task.Deleted then begin
          if TasksTable.Locate('RecordID', Task.RecordID, []) then
            TasksTable.Delete;
          Task.Free;
          Continue;
        end;

        if Task.Changed then begin
          if TasksTable.Locate('RecordID', Task.RecordID, [])
          then
            { this event already exists in the database so update it }
            TasksTable.Edit
          else
            { this record doesn't exist in the database, so it's a new event }
            TasksTable.Append;
          try
            TasksTable.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
            TasksTable.FieldByName('Description').AsString := Task.Description;
            TasksTable.FieldByName('Details').AsString := Task.Details;
            TasksTable.FieldByName('Complete').AsBoolean := Task.Complete;
            TasksTable.FieldByName('DueDate').AsDateTime := Task.DueDate;
            TasksTable.FieldByName('CreatedOn').AsDateTime := Task.CreatedOn;
            TasksTable.FieldByName('CompletedOn').AsDateTime := Task.CompletedOn;
            TasksTable.FieldByName('Priority').AsInteger := Task.Priority;
            TasksTable.FieldByName('Category').AsInteger := Task.Category;

            F := TasksTable.FindField('UserField0');
            if F <> nil then F.AsString := Task.UserField0;
            F := TasksTable.FindField('UserField1');
            if F <> nil then F.AsString := Task.UserField1;
            F := TasksTable.FindField('UserField2');
            if F <> nil then F.AsString := Task.UserField2;
            F := TasksTable.FindField('UserField3');
            if F <> nil then F.AsString := Task.UserField3;
            F := TasksTable.FindField('UserField4');
            if F <> nil then F.AsString := Task.UserField4;
            F := TasksTable.FindField('UserField5');
            if F <> nil then F.AsString := Task.UserField5;
            F := TasksTable.FindField('UserField6');
            if F <> nil then F.AsString := Task.UserField6;
            F := TasksTable.FindField('UserField7');
            if F <> nil then F.AsString := Task.UserField7;
            F := TasksTable.FindField('UserField8');
            if F <> nil then F.AsString := Task.UserField8;
            F := TasksTable.FindField('UserField9');
            if F <> nil then F.AsString := Task.UserField9;

            TasksTable.Post;
            Task.RecordID := TasksTable.FieldByName('RecordID').AsInteger;
          except
            TasksTable.Cancel;
            raise EDBPostError.Create;
          end;
          Task.Changed := false;
        end;
      end;
    end;
    Resource.TasksDirty := false;

    if not Loading then
      NotifyDependents;

    if Assigned(AfterPostTasks) then
      FAfterPostTasks(self);
  end;
end;

procedure TVpCustomDBDataStore.PostResources;
var
  I: Integer;
  Res: TVpResource;
  F: TField;
begin
  Loading := true;
  try
    if (Resources.Count > 0) then begin
      if not ResourceTable.Active then
        ResourceTable.Open;
      ResourceTable.First;
      for I := 0 to pred(Resources.Count) do begin
        Res := Resources.Items[I];

        if (Res <> nil) and Res.Deleted then begin
          PurgeEvents(Res);
          PurgeContacts(Res);
          PurgeTasks(Res);
          if ResourceTable.Locate('ResourceID', Res.ResourceID, [])   
          then ResourceTable.Delete;
          if Resource = Res then
            ResourceID := -1;
          Res.Free;
          Continue;
        end;

        { Dump this resource to the DB }
        if (Res <> nil) and Res.Changed then begin
          with ResourceTable do begin
            if Locate('ResourceID', Res.ResourceID, []) then
              { existing record found }
              Edit
            else
              { this is a new record}
              Append;

            try
              if Res.ResourceID > -1 then
                FieldByName('ResourceID').AsInteger := Res.ResourceID;

              FieldByName('Description').AsString := Res.Description;
              FieldByName('Notes').AsString := Res.Notes;
              FieldByName('ResourceActive').AsBoolean := Res.ResourceActive;
              F := FindField('UserField0');
              if F <> nil then F.AsString := Res.UserField0;
              F := FindField('UserField1');
              if F <> nil then F.AsString := Res.UserField1;
              F := FindField('UserField2');
              if F <> nil then F.AsString := Res.UserField2;
              F := FindField('UserField3');
              if F <> nil then F.AsString := Res.UserField3;
              F := FindField('UserField4');
              if F <> nil then F.AsString := Res.UserField4;
              F := FindField('UserField5');
              if F <> nil then F.AsString := Res.UserField5;
              F := FindField('UserField6');
              if F <> nil then F.AsString := Res.UserField6;
              F := FindField('UserField7');
              if F <> nil then F.AsString := Res.UserField7;
              F := FindField('UserField8');
              if F <> nil then F.AsString := Res.UserField8;
              F := FindField('UserField9');
              if F <> nil then F.AsString := Res.UserField9;
              Post;
            except
              Cancel;
              raise EDBPostError.Create;
            end;
              if Res.ResourceID = -1 then
                Res.ResourceID := FieldByName('ResourceID').AsInteger;
          end;
          if Res.ResourceID = ResourceID then begin
            PostEvents;
            PostContacts;
            PostTasks;
          end;
          Res.Changed := false;
        end;
      end;
      if not Loading then
        NotifyDependents;

      if Assigned(AfterPostEvents) then
        FAfterPostEvents(self);
    end;
  finally
    Loading := false;
  end;
end;

{ - Added}
procedure TVpCustomDBDataStore.PurgeResource(Res: TVpResource);
begin
  Res.Deleted := true;
  PostResources;
  Load;
end;

procedure TVpCustomDBDataStore.PurgeEvents(Res: TVpResource);
begin
  { Purging the events from the database is done by the descendant     !!.01}
  { classes                                                            !!.01}
  inherited;
end;

procedure TVpCustomDBDataStore.PurgeContacts(Res: TVpResource);
begin
  { Purging the contacts from the database is done by the descendant   !!.01}
  { classes                                                            !!.01}
  inherited;
end;

procedure TVpCustomDBDataStore.PurgeTasks(Res: TVpResource);
begin
  { Purging the tasks from the database is done by the descendant      !!.01}
  { classes                                                            !!.01}
  inherited;
end;
{ - End}

procedure TVpCustomDBDataStore.SetResourceByName(Value: string);
var
  I: integer;
  Res : TVpResource;
begin
  for I := 0 to pred(Resources.Count) do begin
    Res := Resources.Items[I];
    if Res = nil then
      Continue;

    if Res.Description = Value then begin
      if ResourceID <> Res.ResourceID then begin
        ResourceID := Res.ResourceID;
        RefreshResource;
      end;
      Exit;
    end;
  end;
end;

procedure TVpCustomDBDataStore.RefreshContacts;
var
  contact: TVpContact;
begin
  if Resource <> nil then begin
    Resource.Contacts.ClearContacts;
    with ContactsTable do begin
      SetFilterCriteria(ContactsTable, False, Resource.ResourceID, 0, 0);
      First;
      while not EOF do begin
        contact := Resource.Contacts.AddContact(FieldByName('RecordID').AsInteger);
        contact.Loading := true;
        LoadContact(contact);
        contact.Loading := false;
        Next;
      end;
    end;
//    LoadContacts;
  end;
  inherited;
end;

procedure TVpCustomDBDataStore.RefreshEvents;
begin
  if Resource <> nil then begin
    Resource.Schedule.ClearEvents;
    LoadEvents;
  end;
  inherited;
end;

procedure TVpCustomDBDataStore.RefreshTasks;
var
  task: TVpTask;
begin
  if Resource <> nil then begin
    Resource.Tasks.ClearTasks;
    with TasksTable do begin
      SetFilterCriteria(TasksTable, False, Resource.ResourceID, 0, 0);
      First;
      while not EOF do begin
        task := Resource.Tasks.AddTask(FieldByName('RecordID').AsInteger);
        task.Loading := true;
        LoadTask(task);
        task.Loading := false;
        Next;
      end;
    end;
  //    LoadContacts;
  end;
  inherited;
end;
(*
  if Resource <> nil then begin
    Resource.Tasks.ClearTasks;
    LoadTasks;
  end;
  inherited;
end;
*)

procedure TVpCustomDBDataStore.RefreshResource;
var
  F: TField;
begin
  if (not ResourceTable.Active) or (ResourceID = -1) then
    Exit;

  {clear the resource}
  if Resource <> nil then begin
    Resource.Schedule.ClearEvents;
    Resource.Tasks.ClearTasks;
    Resource.Contacts.ClearContacts;
  end;

{  if Resource = nil then
    Resource := Resources.GetResource(ResourceID);}

  with ResourceTable do begin
    { if a resource }
    if Locate('ResourceID', ResourceID, []) then begin
      Resource.ResourceID := ResourceID;
      Resource.Description := FieldByName('Description').AsString;
      Resource.Notes := FieldByName('Notes').AsString;
      Resource.ResourceActive := FieldByName('ResourceActive').AsBoolean;
      F := FindField('UserField0');
      if F <> nil then Resource.UserField0 := F.AsString;
      F := FindField('UserField1');
      if F <> nil then Resource.UserField1 := F.AsString;
      F := FindField('UserField2');
      if F <> nil then Resource.UserField2 := F.AsString;
      F := FindField('UserField3');
      if F <> nil then Resource.UserField3 := F.AsString;
      F := FindField('UserField4');
      if F <> nil then Resource.UserField4 := F.AsString;
      F := FindField('UserField5');
      if F <> nil then Resource.UserField5 := F.AsString;
      F := FindField('UserField6');
      if F <> nil then Resource.UserField6 := F.AsString;
      F := FindField('UserField7');
      if F <> nil then Resource.UserField7 := F.AsString;
      F := FindField('UserField8');
      if F <> nil then Resource.UserField8 := F.AsString;
      F := FindField('UserField9');
      if F <> nil then Resource.UserField9 := F.AsString;
      LoadEvents;
      LoadContacts;
      LoadTasks;
    end;
  end;

  inherited;
end;

procedure TVpCustomDBDataStore.SetReadOnly(const Value: boolean);
var
  I: Integer;
begin
  FReadOnly := Value;
  if FReadOnly then begin
    for I := 0 to pred(Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpLinkableControl)
      and (TVpLinkableControl(Owner.Components[I]).DataStore = self) then
        TVpLinkableControl(Owner.Components[I]).ReadOnly := true;
    end;
  end;
end;

{ NOTE: Depending on the syntax dialect used by the dataset components an
  'EDatabaseError' may be raised here with message: "Index based on unknown field '>='."
  In this case add an event handler for OnSetFilterCriteria or override the
  SetFilterCriteria method. TBufDataset, TSQLQuery and TDbf, for example,
  require this syntax for date checking:
    Format(' ... (DTOS(StartTime) >= %s) ...', [
      FormatDateTime('yyyymmdd') ...])
  }
procedure TVpCustomDBDataStore.SetFilterCriteria(ATable: TDataset;
  AUseDateTime: Boolean; AResourceID: Integer; AStartDateTime, AEndDateTime: TDateTime);
var
  filter: String;
begin
  if AUseDateTime then
   {$IFDEF DELPHI}
    filter := Format('ResourceID = %d '
      + 'and ( ( (StartTime >= %s) and (EndTime <= %s) ) '
      + '     or ( (RepeatCode > 0) and (%s <= RepeatRangeEnd) ) )',
      [AResourceID,
       QuotedStr(FormatDateTime('c', AStartDateTime)),
       QuotedStr(FormatDateTime('c', AEndDateTime)),
       QuotedStr(FormatDateTime('c', AStartDateTime))])
   {$ELSE}
    filter := Format('ResourceID = %d '
      + 'and ( ( (DTOS(StartTime) >= %s) and (DTOS(EndTime) <= %s) ) '
      + '     or ( (RepeatCode > 0) and (%s <= DTOS(RepeatRangeEnd)) ) )',
      [AResourceID,
       QuotedStr(FormatDateTime('yyyymmdd', AStartDateTime)),
       QuotedStr(FormatDateTime('yyyymmdd', AEndDateTime)),
       QuotedStr(FormatDateTime('yyyymmdd', AStartDateTime))])
   {$ENDIF}
  else
    filter := Format('ResourceID = %d', [AResourceID]);
  ATable.Filter := filter;
  ATable.Filtered := true;
end;

end.
