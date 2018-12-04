{*********************************************************}
{*                   VPDATA.PAS 1.03                     *}
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

{$MODE ObjFPC}{$H+}

//{$I vp.inc}

unit VpData;
  { Data classes for Visual PlanIt's resources, events, tasks, contacts, etc... }

interface

uses
  LCLProc, LCLType,
  SysUtils, Classes, Dialogs, Graphics,
  VpSR, VpVCard, VpICal;

type
  TVpEventRec = packed record
    Rec: TRect;
    IconRect: TRect;
    Event: Pointer;
  end;

type
  TVpEventArray = array of TVpEventRec;
  
  TVpAlarmAdvType = (atMinutes, atHours, atDays);

  TVpRepeatType = (rtNone=0, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate,
    rtYearlyByDay, rtYearlyByDate, rtCustom);

  TVpContactSort = (csLastFirst, csFirstLast);

  TVpTaskPriority = (tpLow=-1, tpNormal=0, tpHigh=1);

  TVpOverlayPattern = (opSolid, opClear, opHorizontal, opVertical,
                       opFDiagonal, opBDiagonal, opCross, opDiagCross);

  TVpOverlayDetail = (odResource, odEventDescription, odEventCategory);
  TVpOverlayDetails = set of TVpOverlayDetail;

  { forward declarations }
  TVpResource = class;
  TVpResourceGroup = class;
  TVpTasks = class;
  TVpSchedule = class;
  TVpEvent = class;
  TVpContacts = class;
  TVpContact = class;
  TVpTask = class;

  TVpResources = class
  private
    FOwner: TObject;   // This is the Datastore.
    FResourceGroups: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TVpResource;
    function GetResourceGroup(Index: Integer): TVpResourceGroup;
    function GetResourceGroupCount: Integer;
  protected
    FResourceList: TList;
    function NextResourceID: Integer;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    function AddResource(ResID: Integer): TVpResource;
    function AddResourceGroup(const AResIDs: array of Integer;
      ACaption: String = ''): TVpResourceGroup;
    procedure ClearResources;
    procedure ClearResourceGroups;
    function FindResourceByName(AName : string) : TVpResource;
    function FindResourceGroupByName(AName: String): TVpResourceGroup;
    function GetResource(ID: Integer): TVpResource;
    procedure RemoveResource(Resource: TVpResource);
    procedure RemoveResourceGroup(AGroup: TVpResourceGroup);
    procedure Sort;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TVpResource read GetItem;
    property Owner: TObject read FOwner;
    property ResourceGroupCount: Integer read GetResourceGroupCount;
    property ResourceGroups[Index: Integer]: TVpResourceGroup read GetResourceGroup;
  end;

  TVpResource = class
  private
    FOwner: TVpResources;
    FChanged: Boolean;
    FDeleted: Boolean;
    FEventsDirty: Boolean;
    FContactsDirty: Boolean;
    FLoading : Boolean;
    FTasksDirty: Boolean;
    FSchedule: TVpSchedule;
    FTasks: TVpTasks;
    FContacts: TVpContacts;
    FGroup: TVpResourceGroup;
    FActive: Boolean;    // Internal flag whether to display  this resource
    FNotes: string;
    FDescription: string;
    { reserved for your use }
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;
    FResourceID: Integer;
    function GetSchedule: TVpSchedule;
    procedure SetChanged(Value: Boolean);
    procedure SetContacts(const Value: TVpContacts);
    procedure SetDeleted(Value: Boolean);
    procedure SetDescription(const Value: string);
    procedure SetGroup(const AValue: TVpResourceGroup);
    procedure SetNotes(const Value: string);
    procedure SetResourceID(const Value: Integer);
    procedure SetSchedule(const Value: TVpSchedule);
    procedure SetTasks(const Value: TVpTasks);

  public
    constructor Create(Owner: TVpResources);
    destructor Destroy; override;
    procedure GetResourceGroups(AList: TList);
    property Loading: Boolean read FLoading write FLoading;
    property Changed: Boolean read FChanged write SetChanged;
    property Deleted: Boolean read FDeleted write SetDeleted;
    property EventsDirty: Boolean read FEventsDirty write FEventsDirty;
    property ContactsDirty: Boolean read FContactsDirty write FContactsDirty;
    property TasksDirty: Boolean read FTasksDirty write FTasksDirty;
    property Active: Boolean read FActive write FActive; deprecated 'Use "ResourceActive" instead';
    property Owner: TVpResources read FOwner;
//    property ItemIndex: integer read FItemIndex;
    property Schedule: TVpSchedule read GetSchedule write SetSchedule;
    property Tasks: TVpTasks read FTasks write SetTasks;
    property Contacts: TVpContacts read FContacts write SetContacts;
    property Group: TVpResourceGroup read FGroup write SetGroup;

{$ifdef WITHRTTI}
  published
{$else}
  public
{$endif}
    property ResourceID: Integer read FResourceID write SetResourceID;
    property Description: string read FDescription write SetDescription;
    property Notes: string read FNotes write SetNotes;
    property ResourceActive: Boolean read FActive write FActive;
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;

  TVpResourceGroup = class
  private
    FOwner: TVpResources;
    FResourceID: Integer;
    FCaption: String;
    FIDs: Array of Integer;
    FReadOnly: Boolean;
    FPattern: TVpOverlayPattern;
    FShowDetails: TVpOverlayDetails;
    function GetCount: integer;
    function GetItem(AIndex: Integer): TVpResource;
    procedure SetPattern(AValue: TVpOverlayPattern);
    procedure SetShowDetails(AValue: TVpOverlayDetails);
  public
    constructor Create(AOwner: TVpResources; AResourceID: Integer; ACaption: String);
    destructor Destroy; override;
    function AddID(AResourceID: Integer): Integer;
    function AsString(ASeparator: Char = ';'): String;
    procedure Clear;
    function IndexOfID(AResourceID: Integer): Integer;
    procedure Remove(AResourceID: Integer);
    property Caption: String read FCaption;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TVpResource read GetItem; default;
    property Pattern: TVpOverlayPattern read FPattern write SetPattern;
    property ResourceID: Integer read FResourceID;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property ShowDetails: TVpOverlayDetails read FShowDetails write SetShowDetails;
  end;

  TVpSchedule = class
  private
    FOwner: TVpResource;
    function GetCount: Integer;
  protected
    FEventList: TList;
    FBatchUpdate: Integer;
//    function FindTimeSlot(StartTime, EndTime: TDateTime): Boolean;
  public
    constructor Create(Owner: TVpResource);
    destructor Destroy; override;
    function AddEvent(RecordID: Integer; StartTime, EndTime: TDateTime): TVpEvent;
    procedure AllDayEventsByDate(Date: TDateTime; EventList: TList);
    procedure BatchUpdate(Value: Boolean);
    procedure ClearEvents;
    procedure ClearGroupEvents;
    procedure DeleteEvent(Event: TVpEvent);
    function EventCountByDay(Value: TDateTime): Integer;
    procedure EventsByDate(Date: TDateTime; EventList: TList);
    function GetEvent(Index: Integer): TVpEvent;
    function RepeatsOn(Event: TVpEvent; Day: TDateTime): Boolean;
    procedure Sort;
    property Owner: TVpResource read FOwner;
    property EventCount: Integer read GetCount;
  end;

  { TVpEvent }

  TVpEvent = class
  private
    FOwner: TVpSchedule;
    FChanged: Boolean;
    FDeleted: Boolean;
    FLoading: Boolean;
    //FPrivateEvent: Boolean;
    FAlarmSet: Boolean;
    FDingPath: string;
    FAllDayEvent: Boolean;
    FCategory: Integer;
    FAlarmAdv: Integer;
    FAlertDisplayed: Boolean;
    FAlarmAdvType: TVpAlarmAdvType;
    FRecordID: Integer;
    FResourceID: Integer;
    FLocation: string;
    FNotes: string;
    FDescription: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FSnoozeTime: TDateTime;
    FRepeatCode: TVpRepeatType;
    FRepeatRangeEnd: TDateTime;
    FCustInterval: Integer;
    { reserved for your use }
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;
    procedure SetAllDayEvent(Value: Boolean);
//    procedure SetItemIndex(Value: Integer);
    procedure SetChanged(Value: Boolean);
    procedure SetDeleted(Value: Boolean);
    procedure SetDingPath(Value: string);
    procedure SetAlarmAdv(Value: Integer);
    procedure SetAlarmAdvType(Value: TVpAlarmAdvType);
    procedure SetSnoozeTime(Value: TDateTime);
    procedure SetAlarmSet(Value: Boolean);
    procedure SetCategory(Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetEndTime(Value: TDateTime);
    procedure SetLocation(const Value: String);
    procedure SetNotes(const Value: string);
    procedure SetRecordID(Value: Integer);
    procedure SetStartTime(Value: TDateTime);
    procedure SetCustInterval(Value: Integer);
    procedure SetRepeatCode(Value: TVpRepeatType);
    procedure SetRepeatRangeEnd(Value: TDateTime);
  public
    constructor Create(Owner: TVpSchedule);
    destructor Destroy; override;
    function CanEdit: Boolean;
    function GetResource: TVpResource;
    function IsOverlayed: Boolean;
    procedure LoadFromICalendar(AEntry: TVpICalEvent);
    property Owner: TVpSchedule read FOwner;
    property ResourceID: Integer read FResourceID write FResourceID;
    property Loading : Boolean read FLoading write FLoading;
    property Changed: Boolean read FChanged write SetChanged;
    property Deleted: Boolean read FDeleted write SetDeleted;
{$ifdef WITHRTTI}
  published
{$else}
  public
{$endif}
    property RecordID: Integer read FRecordID write SetRecordID;
    property DingPath: string read FDingPath write SetDingPath;
    property AlarmWavPath: string read FDingPath write SetDingPath; deprecated 'Use "DingPath" instead';
    property AlertDisplayed: Boolean read FAlertDisplayed write FAlertDisplayed;
    property AllDayEvent: Boolean read FAllDayEvent write SetAllDayEvent;
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property EndTime: TDateTime read FEndTime write SetEndTime;
    property Description: string read FDescription write SetDescription;
    property Notes: string read FNotes write SetNotes;
    property Note: String read FNotes write SetNotes; deprecated 'Use "Notes" instead';
    property Category: Integer read FCategory write SetCategory;
    property AlarmSet: Boolean read FAlarmSet write SetAlarmSet;
    property AlarmAdvance: Integer read FAlarmAdv write SetAlarmAdv;
    property AlarmAdv: Integer read FAlarmAdv write SetAlarmAdv; deprecated 'Use "AlarmAdvance" instead';
    property Location: string read FLocation write SetLocation;
    { 0=Minutes, 1=Hours, 2=Days   }
    property AlarmAdvanceType: TVpAlarmAdvType read FAlarmAdvType write SetAlarmAdvType;
    property AlarmAdvType: TVpAlarmAdvType read FAlarmAdvType write SetAlarmAdvType; deprecated 'Use "AlarmAdvanceType" instead';
    property SnoozeTime: TDateTime read FSnoozeTime write SetSnoozeTime;
    { rtNone, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate, }
    { rtYearlyByDay, rtYearlyByDate, rtCustom                     }
    property RepeatCode: TVpRepeatType read FRepeatCode write SetRepeatCode;
    property RepeatRangeEnd: TDateTime read FRepeatRangeEnd write SetRepeatRangeEnd;
    { Custom Repeat Interval in seconds }
    { is Zero if IntervalCode <> 7      }
    property CustomInterval: Integer read FCustInterval write SetCustInterval;
    property CustInterval: Integer read FCustInterval write SetCustInterval; deprecated 'Use "CustomInterval" instead';
    { Reserved for your use }
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;

  TVpTasks = class
  private
    FOwner: TVpResource;
  protected
    FTaskList: TList;
    FBatchUpdate: Integer;
  public
    constructor Create(Owner: TVpResource);
    destructor Destroy; override;
    function AddTask(RecordID: Integer): TVpTask;
    procedure BatchUpdate(value: Boolean);
    procedure ClearTasks;
    function Count: Integer;
    function CountByDay(Date: TDateTime): Integer;
    procedure DeleteTask(Task: TVpTask);
    function First: TVpTask;
    function FirstByDay(Date: TDateTime): TVpTask;
    function IndexOf(ATask: TVpTask): Integer;
    function Last: TVpTask;
    function LastByDay(Date: TDateTime): TVpTask;
    procedure Sort;
    function GetTask(Index: Integer): TVpTask;
    property Owner: TVpREsource read FOwner;
  end;

  TVpTask = class
  private
    FOwner: TVpTasks;
    FLoading: Boolean;
    FChanged: Boolean;
    FDeleted: Boolean;
    FItemIndex: Integer;
    FPriority: Integer;
    FCategory: Integer;
    FComplete: Boolean;
    FDescription: string;
    FDetails: string;
    FCreatedOn: TDateTime;
    FCompletedOn: TDateTIme;
    FRecordID: Integer;
    FDueDate: TDateTime;
    { reserved for your use }
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;

    procedure SetCategory(const Value: Integer);
    procedure SetChanged(const Value: Boolean);
    procedure SetComplete(const Value: Boolean);
    procedure SetCompletedOn(const Value: TDateTime);
    procedure SetCreatedOn(const Value: TDateTime);
    procedure SetDescription(const Value: string);
    procedure SetDetails(const Value: string);
    procedure SetDueDate(const Value: TDateTime);
    procedure SetPriority(const Value: Integer);
  protected
    function IsOverdue: Boolean;
  public
    constructor Create(Owner: TVpTasks);
    destructor Destroy; override;
    procedure LoadFromICalendar(AEntry: TVpICalToDo);
    property Loading: Boolean read FLoading write FLoading;
    property Changed: Boolean read FChanged write SetChanged;
    property Deleted: Boolean read FDeleted write FDeleted;
    property Owner: TVpTasks read FOwner;
{$ifdef WITHRTTI}
  published
{$else}
  public
{$endif}
    property RecordID: Integer read FRecordID write FRecordID;
    property DueDate: TDateTime read FDueDate write SetDueDate;
    property Description: string read FDescription write SetDescription;
    property ItemIndex: Integer read FItemIndex;
    property Details: string read FDetails write SetDetails;
    property Complete: Boolean read FComplete write SetComplete;
    property CreatedOn: TDateTime read FCreatedOn write SetCreatedOn;
    property CompletedOn: TDateTIme read FCompletedOn write SetCompletedOn;
    property Priority: Integer read FPriority write SetPriority;   // --> TVpTaskPriority
    property Category: Integer read FCategory write SetCategory;   // --> TVpCategoryType

    { Reserved for your use }
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;

  TVpContacts  = class
  private
    FOwner: TVpResource;
    FContactsList: TList;
    FContactSort: TVpContactSort;
    procedure SetContactSort(const v: TVpContactSort);
  protected
    FBatchUpdate: Integer;
  public
    constructor Create(Owner: TVpResource);
    destructor Destroy; override;
    function AddContact(RecordID: Integer): TVpContact;
    procedure BatchUpdate(Value: Boolean);
    procedure ClearContacts;
    function Count: Integer;
    procedure DeleteContact(Contact: TVpContact);
    function First: TVpContact;
    function FindContactByName(const AName: string;
      CaseInsensitive: Boolean = True): TVpContact;
    function FindContactIndexByName(const Name: string;
      CaseInsensitive: Boolean = True): Integer;
    function GetContact(Index: Integer): TVpContact;
    function Last:TVpContact;
    procedure Sort;

    property ContactsList: TList
      read FContactsList;
    property ContactSort: TVpContactSort
      read FContactSort write SetContactSort default csLastFirst;
  end;

  TVpContact = class
  private
    FOwner: TVpContacts;
    FLoading: Boolean;
    FChanged: Boolean;
    FRecordID: Integer;
    FDeleted: Boolean;
    FPosition: string;
    FLastName: string;
    FFirstName: string;
    FBirthDate: TDateTime;
    FAnniversary: TDateTime;
    FTitle: string;
    FCompany: string;
    FDepartment: String;
    FEMail1: string;
    FEMail2: String;
    FEMail3: String;
    FEMailType1: integer;
    FEMailType2: integer;
    FEMailType3: integer;
    FPhone1: string;
    FPhone2: string;
    FPhone3: string;
    FPhone4: string;
    FPhone5: string;
    FPhoneType1: integer;
    FPhoneType2: integer;
    FPhoneType3: integer;
    FPhoneType4: integer;
    FPhoneType5: integer;
    FWebsite1: String;
    FWebsite2: String;
    FWebsiteType1: Integer;
    FWebsiteType2: Integer;
    FAddressType1: Integer;
    FAddressType2: Integer;
    FAddress1: string;
    FAddress2: String;
    FCity1: string;
    FCity2: String;
    FState1: string;
    FState2: String;
    FZip1: string;
    FZip2: String;
    FCountry1: string;
    FCountry2: String;
    FNotes: string;
    //FPrivateRec: boolean;
    FCategory: integer;
    FCustom1: string;
    FCustom2: string;
    FCustom3: string;
    FCustom4: string;
    { reserved for your use }
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;
    procedure SetAddress1(const Value: string);
    procedure SetAddress2(const Value: String);
    procedure SetAddressType1(Value: Integer);
    procedure SetAddressType2(Value: Integer);
    procedure SetBirthDate(Value: TDateTime);
    procedure SetAnniversary(Value: TDateTime);
    procedure SetCategory(Value: integer);
    procedure SetChanged(Value: Boolean);
    procedure SetCity1(const Value: string);
    procedure SetCity2(const Value: String);
    procedure SetCompany(const Value: string);
    procedure SetCountry1(const Value: string);
    procedure SetCountry2(const Value: string);
    procedure SetCustom1(const Value: string);
    procedure SetCustom2(const Value: string);
    procedure SetCustom3(const Value: string);
    procedure SetCustom4(const Value: string);
    procedure SetDeleted(Value: Boolean);
    procedure SetDepartment(const Value: String);
    procedure SetEMail1(const Value: string);
    procedure SetEMail2(const Value: string);
    procedure SetEMail3(const Value: string);
    procedure SetEMailType1(const Value: Integer);
    procedure SetEMailType2(const Value: Integer);
    procedure SetEMailType3(const Value: Integer);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetNotes(const Value: string);
    procedure SetPhone1(const Value: string);
    procedure SetPhone2(const Value: string);
    procedure SetPhone3(const Value: string);
    procedure SetPhone4(const Value: string);
    procedure SetPhone5(const Value: string);
    procedure SetPhoneType1(Value: integer);
    procedure SetPhoneType2(Value: integer);
    procedure SetPhoneType3(Value: integer);
    procedure SetPhoneType4(Value: integer);
    procedure SetPhoneType5(Value: integer);
    procedure SetPosition(const Value: string);
    procedure SetRecordID(Value: Integer);
    procedure SetState1(const Value: string);
    procedure SetState2(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetWebsite1(Value: String);
    procedure SetWebsite2(Value: String);
    procedure SetWebsiteType1(Value: integer);
    procedure SetWebsiteType2(Value: integer);
    procedure SetZip1(const Value: string);
    procedure SetZip2(const Value: string);

  public
    constructor Create(Owner: TVpContacts);
    destructor Destroy; override;
    function ContainsContactData: Boolean;
    function ContainsWorkData: Boolean;
    function ContainsHomeData: Boolean;
    function FullName: string;
    procedure LoadFromVCard(ACard: TVpVCard);

    property Loading: Boolean read FLoading write FLoading;
    property Changed: Boolean read FChanged write SetChanged;
    property Deleted: Boolean read FDeleted write SetDeleted;
    property Owner: TVpContacts read FOwner write FOwner;

{$ifdef WITHRTTI}
  published
{$else}
  public
{$endif}
    property RecordID: Integer read FRecordID write SetRecordID;
    property Job_Position: string read FPosition write SetPosition;
    property Position: string read FPosition write SetPosition; deprecated 'Use "Job_Position" instead';
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property BirthDate: TDateTime read FBirthdate write SetBirthdate;
    property Anniversary: TDateTime read FAnniversary write SetAnniversary;
    property Title: string read FTitle write SetTitle;
    property Company: string read FCompany write SetCompany;
    property Department: String read FDepartment write SetDepartment;
    property EMail: string read FEmail1 write SetEMail1; deprecated 'Use "EMail1" instead';
    property EMail1: String read FEmail1 write SetEMail1;
    property EMail2: String read FEmail2 write SetEmail2;
    property EMail3: String read FEmail3 write SetEmail3;
    property EMailType1: integer read FEMailType1 write SetEMailType1;
    property EMailType2: integer read FEMailType2 write SetEMailType2;
    property EMailType3: integer read FEMailType3 write SetEMailType3;
    property Phone1: string read FPhone1 write SetPhone1;
    property Phone2: string read FPhone2 write SetPhone2;
    property Phone3: string read FPhone3 write SetPhone3;
    property Phone4: string read FPhone4 write SetPhone4;
    property Phone5: string read FPhone5 write SetPhone5;
    property PhoneType1: integer read FPhoneType1 write SetPhoneType1;
    property PhoneType2: integer read FPhoneType2 write SetPhoneType2;
    property PhoneType3: integer read FPhoneType3 write SetPhoneType3;
    property PhoneType4: integer read FPhoneType4 write SetPhoneType4;
    property PhoneType5: integer read FPhoneType5 write SetPhoneType5;
    property Website1: string read FWebsite1 write SetWebsite1;
    property Website2: string read FWebsite2 write SetWebsite2;
    property WebsiteType1: integer read FWebsiteType1 write SetWebsiteType1;
    property WebsiteType2: integer read FWebsiteType2 write SetWebsiteType2;
    property Address: string read FAddress1 write SetAddress1; deprecated 'Use "Address1" instead';
    property Address1: string read FAddress1 write SetAddress1;
    property Address2: string read FAddress2 write SetAddress2;
    property City: string read FCity1 write SetCity1; deprecated 'Use "City1" instead';
    property City1: string read FCity1 write SetCity1;
    property City2: string read FCity2 write SetCity2;
    property State: string read FState1 write SetState1; deprecated 'Use "State1" instead';
    property State1: string read FState1 write SetState1;
    property State2: string read FState2 write SetState2;
    property Zip: string read FZip1 write SetZip1; deprecated 'Use "Zip1" instead';
    property Zip1: string read FZip1 write SetZip1;
    property Zip2: string read FZip2 write SetZip2;
    property Country: string read FCountry1 write SetCountry1; deprecated 'Use "Country1" instead';
    property Country1: string read FCountry1 write SetCountry1;
    property Country2: string read FCountry2 write SetCountry2;
    property AddressType1: integer read FAddressType1 write SetAddressType1;
    property AddressType2: integer read FAddressType2 write SetAddressType2;
    property Note: string read FNotes write SetNotes; deprecated 'Use "Notes" instead';
    property Notes: string read FNotes write SetNotes;
    property Category: integer read FCategory write SetCategory;
    property Custom1: string read FCustom1 write SetCustom1;
    property Custom2: string read FCustom2 write SetCustom2;
    property Custom3: string read FCustom3 write SetCustom3;
    property Custom4: string read FCustom4 write SetCustom4;
    { Reserved for your use }
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;

function CompareEventsByTimeOnly(Item1, Item2: Pointer): Integer;


implementation

uses
  Math, DateUtils,
  VpException, VpConst, VpMisc, VpBaseDS;

const
  TIME_EPS = 1.0 / SecondsInDay;  // Epsilon for comparing times

{ Compare function for sorting resources: Compares the resource descriptions }
function CompareResources(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TVpResource(Item1).Description, TVpResource(Item2).Description);
  // CompareTEXT --> ignore case
end;

{ Compare function for sorting events: Compares the start times of two events.
  If the times are equal (within 1 seconds) then end times are compared. }
function CompareEvents(Item1, Item2: Pointer): Integer;
begin
  if SameValue(TVpEvent(Item1).StartTime, TVpEvent(Item2).StartTime, TIME_EPS) then
    Result := CompareValue(TVpEvent(Item1).EndTime, TVpEvent(Item2).EndTime)
  else
    Result := CompareValue(TVpEvent(Item1).StartTime, TVpEvent(Item2).StartTime);
end;

{ Call back function for TList.Sort. Sorting of events by time only, date part
  is ignored. }
function CompareEventsByTimeOnly(Item1, Item2: Pointer): Integer;
var
  event1, event2: TVpEvent;
begin
  event1 := TVpEvent(Item1);
  event2 := TVpEvent(Item2);
  Result := CompareValue(frac(event1.StartTime), frac(event2.StartTime));
  if Result = 0 then
    Result := CompareValue(frac(event1.EndTime), frac(event2.EndTime));
end;

{ Compare function for sorting tasks: Compares the due dates. If they are equal
  then the task descriptions are used. }
function CompareTasks(Item1, Item2: Pointer): Integer;
begin
  if SameValue(TVpTask(Item1).DueDate, TVpTask(Item2).DueDate, TIME_EPS) then
    Result := CompareText(TVpTask(Item1).Description, TVpTask(Item2).Description)
  else
    Result := CompareValue(TVpTask(Item1).DueDate, TVpTask(Item2).DueDate);
end;

function CompareContacts_Minors(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TVpContact(Item1).Email1, TVpContact(Item2).EMail1);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).Phone1, TVpContact(Item2).Phone1);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).Phone2, TVpContact(Item2).Phone2);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).Phone3, TVpContact(Item2).Phone3);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).Phone4, TVpContact(Item2).Phone4);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).Phone5, TVpContact(Item2).Phone5);
end;

{ Compare function for sorting contacts: Compare the first names of the contacts,
  if equal compare the last names. }
function CompareContacts_FirstLast(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TVpContact(Item1).FirstName, TVpContact(Item2).Firstname);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).LastName, TVpContact(Item2).LastName);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).Company, TVpContact(Item2).Company);
  if Result = 0 then
    Result := CompareContacts_Minors(Item1, Item2);
end;

{ Compare function for sorting contacts: Compare the last names of the contacts,
  if equal compare the first names. }
function CompareContacts_LastFirst(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TVpContact(Item1).LastName, TVpContact(Item2).Lastname);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).FirstName, TVpContact(Item2).FirstName);
  if Result = 0 then
    Result := CompareText(TVpContact(Item1).Company, TVpContact(Item2).Company);
  if Result = 0 then
    Result := CompareContacts_Minors(Item1, Item2);
end;


(*****************************************************************************)
{ TVpResources }
(*****************************************************************************)

constructor TVpResources.Create(Owner: TObject);
begin
  inherited Create;
  FOwner := Owner;
  FResourceList := TList.Create;
  FResourceGroups := TList.Create;
end;

destructor TVpResources.Destroy;
begin
  ClearResourceGroups;
  FResourceGroups.Free;

  ClearResources;
  FResourceList.Free;

  inherited;
end;

function TVpResources.AddResource(ResID: Integer): TVpResource;
var
  Resource: TVpResource;
begin
  Resource := TVpResource.Create(Self);
  try
    Resource.Loading := true;
    FResourceList.Add(Resource);
    Resource.ResourceID := ResID;
    Resource.ResourceActive := true;
    Resource.Loading := false;
    Result := Resource;
  except
    Resource.Free;
    raise EFailToCreateResource.Create;
  end;
end;

function TVpResources.AddResourceGroup(const AResIDs: Array of Integer;
  ACaption: String = ''): TVpResourceGroup;
var
  grp: TVpResourceGroup;
  res: TVpResource;
  i: Integer;
begin
  if Length(AResIDs) < 2 then
    raise Exception.Create('Resource group must contain at least one additional resource.');

  // Use resource descriptions if ACaption is not specified or empty.
  if ACaption = '' then begin
    for i:=Low(AResIDs) + 1 to High(AResIDs) do begin
      res := GetResource(AResIDs[i]);
      if res <> nil then
        ACaption := ACaption + ', ' + res.Description;
    end;
    if ACaption <> '' then Delete(ACaption, 1, 2);
  end;

  // Enforce unique group name.
  grp := FindResourceGroupByName(ACaption);
  if grp = nil then begin
    // Index 0 refers to the resource to which the other resources are added.
    Result := TVpResourceGroup.Create(Self, AResIDs[0], ACaption);
    FResourceGroups.Add(Result);
  end else begin
    grp.Clear;  // Make sure that the group is empty before adding overlayed resources
    Result := grp;
  end;
  for i:=1 to High(AResIDs) do
    Result.AddID(AResIDs[i]);
end;

procedure TVpResources.ClearResources;
begin
  while FResourceList.Count > 0 do
    TVpResource(FResourceList.Last).Free;
end;

procedure TVpResources.ClearResourceGroups;
begin
  while FResourceGroups.Count > 0 do
    TVpResourceGroup(FResourceGroups.Last).Free;
end;

function TVpResources.FindResourceByName (AName : string) : TVpResource;
var
  i: Integer;
begin
  Result := nil;
  AName := LowerCase(AName);
  for i := 0 to Count - 1 do
    if LowerCase(Items[i].Description) = AName then begin
      Result := Items[i];
      Break;
    end;
end;

function TVpResources.FindResourceGroupByName(AName: String): TVpResourceGroup;
var
  i: Integer;
begin
  for i:=0 to FResourceGroups.Count-1 do begin
    Result := TVpResourceGroup(FResourceGroups.Items[i]);
    if Result.Caption = AName then
      exit;
  end;
  Result := nil;
end;

function TVpResources.GetCount: Integer;
begin
  Result := FResourceList.Count;
end;

function TVpResources.GetItem(Index: Integer): TVpResource;
begin
  Result := TVpResource(FResourceList.List^[Index]);
end;

function TVpResources.GetResource(ID: integer): TVpResource;
var
  I: Integer;
  Res: TVpResource;
begin
  result := nil;
  for I := 0 to pred(FResourceList.Count) do begin
    res := TVpResource(FResourceList.Items[I]);
    if Res.ResourceID = ID then begin
      result := Res;
      Exit;
    end;
  end;
end;

function TVpResources.GetResourceGroupCount: Integer;
begin
  Result := FResourceGroups.Count;
end;

function TVpResources.GetResourceGroup(Index: Integer): TVpResourceGroup;
begin
  Result := TVpResourceGroup(FResourceGroups[Index]);
end;

function TVpResources.NextResourceID: Integer;
var
  I : Integer;
  ID: Integer;
  Res: TVpResource;
begin
  ID := 0;
  for I := 0 to pred(FResourceList.Count) do begin
    Res := GetResource(I);
    if (Res <> nil)
    and (ID <= Res.ResourceID) then
      Inc(ID);
  end;
  Result := ID;
end;

procedure TVpResources.RemoveResource(Resource: TVpREsource);
begin
  // The resource removes the list entry in its destructor
  Resource.Free;
end;

procedure TVpResources.RemoveResourceGroup(AGroup: TVpResourceGroup);
begin
  // The resource group removes the list entry in its destructor.
  AGroup.Free;
end;

procedure TVpResources.Sort;
begin
  FResourceList.Sort(@CompareResources);
end;


(*****************************************************************************)
{ TVpResource }
(*****************************************************************************)

constructor TVpResource.Create(Owner: TVpResources);
begin
  inherited Create;
  FOwner := Owner;
  FSchedule := TVpSchedule.Create(Self);
  FTasks := TVpTasks.Create(Self);
  FContacts := TVpContacts.Create(Self);
  FActive := false;
end;

destructor TVpResource.Destroy;
var
  idx: Integer;
begin
  { Clear and free the schedule, tasks and contacts }
  FSchedule.ClearEvents;
  FSchedule.Free;
  FTasks.ClearTasks;
  FTasks.Free;
  FContacts.ClearContacts;
  FContacts.Free;

  { remove self from Resources list }
  if FOwner <> nil then begin
    idx := FOwner.FResourceList.IndexOf(self);
    if idx > -1 then FOwner.FResourceList.Delete(idx);
  end;

  inherited;
end;

{ Returns all resource groups attached to this resource }
procedure TVpResource.GetResourceGroups(AList: TList);
var
  i: Integer;
  grp: TVpResourceGroup;
begin
  for i:=0 to Owner.ResourceGroupCount - 1 do begin
    grp := Owner.ResourceGroups[i];
    if grp.ResourceID = FResourceID then
      AList.Add(grp);
  end;
end;

procedure TVpResource.SetContacts(const Value: TVpContacts);
begin
  FContacts := Value;
end;

procedure TVpResource.SetChanged(Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
  end;
end;

procedure TVpResource.SetDeleted(Value: Boolean);
begin
  if FDeleted <> Value then begin
    FDeleted := Value;
    Changed := true;
  end;
end;

function TVpResource.GetSchedule: TVpSchedule;
begin
  if FSchedule = nil then
    FSchedule := TVpSchedule.Create(self);
  result := FSchedule;
end;

procedure TVpResource.SetDescription(const Value: string);
begin
  if Value <> FDescription then begin
    if Assigned(Owner) then begin
      if Owner.FindResourceByName(Value) <> nil then
        raise EDuplicateResource.Create;
    end;

    FDescription := Value;
    FChanged := true;
  end;
end;

procedure TVpResource.SetGroup(const AValue: TVpResourceGroup);
begin
  FGroup := AValue;
end;

procedure TVpResource.SetNotes(const Value: string);
begin
  FNotes := Value;
  FChanged := true;
end;

procedure TVpResource.SetResourceID(const Value: Integer);
begin
  FResourceID := Value;
end;

procedure TVpResource.SetSchedule(const Value: TVpSchedule);
begin
  FSchedule := Value;
end;

procedure TVpResource.SetTasks(const Value: TVpTasks);
begin
  FTasks := Value;
end;


(*****************************************************************************)
{ TVpResourceGroup }
(*****************************************************************************)
constructor TVpResourceGroup.Create(AOwner: TVpResources; AResourceID: Integer;
  ACaption: String);
begin
  inherited Create;
  FOwner := AOwner;
  FResourceID := AResourceID;
  FCaption := ACaption;
  FPattern := opBDiagonal;
  FReadOnly := true;
  FShowDetails := [odResource];
  Clear;
end;

destructor TVpResourceGroup.Destroy;
var
  idx: Integer;
begin
  Clear;
  { remove self from Owner's resource group list }
  if FOwner <> nil then begin
    idx := FOwner.FResourceGroups.IndexOf(self);
    if idx > -1 then FOwner.FResourceGroups.Delete(idx);
  end;
  inherited Destroy;
end;

function TVpResourceGroup.AddID(AResourceID: Integer): Integer;
begin
  Result := -1;
  if (AResourceID = FResourceID) then
    exit;
  Result := IndexOfID(AResourceID);
  if Result = -1 then begin
    SetLength(FIDs, Length(FIDs) + 1);
    FIDs[High(FIDs)] := AResourceID;
  end;
end;

function TVpResourceGroup.AsString(ASeparator: Char = ';'): String;
var
  list: TStrings;
  i: Integer;
begin
  list := TStringList.Create;
  try
    list.Delimiter := ASeparator;
    list.StrictDelimiter := true;
    list.Add(IntToStr(FResourceID));
    for i:=0 to High(FIDs) do
      list.Add(IntToStr(FIDs[i]));
    Result := list.DelimitedText;
  finally
    list.Free;
  end;
end;

procedure TVpResourceGroup.Clear;
begin
  SetLength(FIDs, 0);
end;

function TVpResourceGroup.GetCount: Integer;
begin
  Result := Length(FIDs);
end;

function TVpResourceGroup.GetItem(AIndex: Integer): TVpResource;
begin
  Result := FOwner.GetResource(FIDs[AIndex]);
end;

function TVpResourceGroup.IndexOfID(AResourceID: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to High(FIDs) do
    if FIDs[i] = AResourceID then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

procedure TVpResourceGroup.Remove(AResourceID: Integer);
var
  i: Integer;
begin
  i := 0;
  while i < Length(FIDs) do begin
    if FIDs[i] = AResourceID then begin
      inc(i);
      while i < Length(FIDs) do begin
        FIDs[i-1] := FIDs[i];
        inc(i);
      end;
      SetLength(FIDs, Length(FIDs)-1);
      exit;
    end;
    inc(i);
  end;
end;

procedure TVpResourceGroup.SetPattern(AValue: TVpOverlayPattern);
begin
  if FPattern = AValue then
    exit;
  FPattern := AValue;
  // to do: repaint the controls
end;

procedure TVpResourceGroup.SetShowDetails(AValue: TVpOverlayDetails);
begin
  if FShowDetails = AValue then
    exit;
  FShowDetails := AValue;
  // To do: repaint the controls
end;

(*****************************************************************************)
{ TVpEvent }
(*****************************************************************************)
constructor TVpEvent.Create(Owner: TVpSchedule);
begin
  inherited Create;
  FAlertDisplayed := false;
  FOwner := Owner;
  FChanged := false;
  FSnoozeTime := 0.0;
end;

destructor TVpEvent.Destroy;
var
  idx: Integer;
begin
  if (FOwner <> nil) then begin
    idx := FOwner.FEventList.IndexOf(self);
    if idx > -1 then FOwner.FEventList.Delete(idx);
  end;
  inherited;
end;

{ Returs false if the event cannot be edited. This is happens if the event is
  overlayed and its resourcegroup is readonly }
function TVpEvent.CanEdit: Boolean;
var
  res: TVpResource;
  grp: TVpResourceGroup;
begin
  Result := true;
  if IsOverlayed then begin
    res := GetResource;
    if res <> nil then begin
      grp := res.Group;
      if grp.ReadOnly then Result := false;
    end;
  end;
end;

{ Returns the resource to which the event belongs. }
function TVpEvent.GetResource: TVpResource;
begin
  Result := FOwner.Owner;
end;

{ The event is overlayed if its ResourceID is different from that of the
  resource to which it belongs. }
function TVpEvent.IsOverlayed: Boolean;
var
  res: TVpResource;  // resource to which the event belongs
begin
  Result := false;
  if (FOwner <> nil) and (FResourceID > 0) then begin
    res := FOwner.FOwner;
    if (res <> nil) and (res.ResourceID <> FResourceID) then
      Result := true;
  end;
end;

function IsInteger(d, Epsilon: Double): Boolean;
begin
  Result := abs(d - round(d)) < Epsilon;
end;

procedure TVpEvent.LoadFromICalendar(AEntry: TVpICalEvent);
var
  dt: Double;
  cat: String;
  i, j, k: Integer;
  datastore: TVpCustomDatastore;
begin
  if AEntry = nil then
    exit;

  { Standard event properties }
  FDescription := AEntry.Summary;
  FNotes := AEntry.Description;
  FLocation := AEntry.Location;
  // Start and end time already have been set --> Skip .

  { Category }
  { tvplanit has only 1 category, ical may have several. We pick the first one
    defined in the datastore. If none is defined we create the first one. }
  if AEntry.CategoryCount > 0 then begin
    datastore := TVpCustomDatastore(Owner.Owner.Owner.Owner);
    k := -1;
    for i := 0 to AEntry.CategoryCount-1 do begin
      cat := AEntry.category[i];
      j := datastore.CategoryColorMap.IndexOfCategory(cat);
      if j <> -1 then begin
        k := j;
        break;
      end;
    end;
    if k = -1 then begin // category not found in data store
      k := datastore.CategoryColorMap.IndexOfFirstUnusedCategory;
      if k <> -1 then
        datastore.CategoryColorMap.SetCategoryName(k, AEntry.Category[0]);
    end;
    if k <> -1 then
      FCategory := k;
  end;

  { All-day event }
  FAllDayEvent := (frac(FStartTime) = 0) and (frac(FEndTime) = 0);

  { Alarm properties }
  if AEntry.Alarm <> nil then begin
    FAlarmSet := true;
    dt := abs(AEntry.Alarm.Trigger);
    if IsInteger(dt, 1.0 / SecondsInDay) then begin
      FAlarmAdvType := atDays;
      FAlarmAdv := round(dt);
    end else
    if IsInteger(dt*HoursInDay, HoursInDay / SecondsInDay) then begin
      FAlarmAdvType := atHours;
      FAlarmAdv := round(dt * HoursInDay);
    end else begin
      FAlarmAdvType := atMinutes;
      FAlarmAdv := round(dt * MinutesInDay);
    end;
    FDingPath := AEntry.Alarm.AudioSrc;
    if not FileExists(FDingPath) then FDingPath := '';
  end else
    FAlarmSet := false;

  { Recurrence }
  FRepeatCode := rtNone;
  FRepeatRangeEnd := 0;
  case AEntry.RecurrenceFrequency of
    'YEARLY':
      if AEntry.RecurrenceInterval = 0 then
        FRepeatCode := rtYearlyByDate   // or rtYearlyByDay ?
      else begin
        FRepeatCode := rtCustom;
        FCustInterval := AEntry.RecurrenceInterval * 365; // * SecondsInDay;
      end;
    'MONTHLY':
      if AEntry.RecurrenceInterval = 0 then
        FRepeatCode := rtMonthlyByDate  // or rtMonthlyByDay ?
      else begin
        FRepeatCode := rtCustom;
        FCustInterval := AEntry.RecurrenceInterval * 30; // * SecondsInDay;
      end;
    'WEEKLY':
      if AEntry.RecurrenceInterval = 0 then
        FRepeatCode := rtWeekly
      else begin
        FRepeatCode := rtCustom;
        FCustInterval := AEntry.RecurrenceInterval * 7; // * SecondsInDay;
      end;
    'DAILY':
      if AEntry.RecurrenceInterval = 0 then
        FRepeatCode := rtDaily
      else begin
        FRepeatCode := rtCustom;
        FCustInterval := AEntry.RecurrenceInterval; // * SecondsInDay;
      end;
  end;
  if (AEntry.RecurrenceEndDate = 0) and (AEntry.RecurrenceCount > 0) then begin
    FRepeatRangeEnd := trunc(FStartTime);
    case FRepeatCode of
      rtYearlyByDate:
        FRepeatRangeEnd := IncYear(FRepeatRangeEnd, AEntry.RecurrenceCount);
      rtMonthlyByDate:
        FRepeatRangeEnd := IncMonth(FRepeatRangeEnd, AEntry.RecurrenceCount);
      rtWeekly:
        FRepeatRangeEnd := FRepeatRangeEnd + 7 * AEntry.RecurrenceCount;
      rtDaily:
        FRepeatRangeEnd := FRepeatRangeEnd + AEntry.RecurrenceCount;
    end;
  end else
    FRepeatRangeEnd := AEntry.RecurrenceEndDate;
end;

procedure TVpEvent.SetAlarmAdv(Value: Integer);
begin
  if Value <> FAlarmAdv then begin
    FAlarmAdv := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetAlarmAdvType(Value: TVpAlarmAdvType);
begin
  if Value <> FAlarmAdvType then begin
    FAlarmAdvType := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetSnoozeTime(Value: TDateTime);
begin
  if Value <> FSnoozeTime then begin
    FSnoozeTime := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetAlarmSet(Value: Boolean);
begin
  if Value <> FAlarmSet then begin
    FAlarmSet := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetCategory(Value: Integer);
begin
  if Value <> FCategory then begin
    FCategory := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetDescription(const Value: string);
begin
  if Value <> FDescription then begin
    FDescription := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetEndTime(Value: TDateTime);
begin
  if Value <> FEndTIme then begin
    FEndTime := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetAllDayEvent(Value: Boolean);
begin
  if Value <> FAllDayEvent then
    begin
      FAllDayEvent := Value;
      Changed := true;
    end;
end;

procedure TVpEvent.SetChanged(Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
    if FChanged then
      Owner.FOwner.EventsDirty := true;
  end;
end;

procedure TVpEvent.SetDeleted(Value: Boolean);
begin
  if Value <> FDeleted then begin
    FDeleted := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetDingPath(Value: string);
begin
  if Value <> FDingPath then begin
    FDingPath := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetLocation(const Value: String);
begin
  if Value <> FLocation then begin
    FLocation := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetNotes(const Value: string);
begin
  if Value <> FNotes then begin
    FNotes := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetRecordID(Value: Integer);
begin
  if Value <> FRecordID then begin
    FRecordID := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetRepeatCode(Value: TVpRepeatType);
begin
  { rtNone, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate, }
  { rtYearlyByDay, rtYearlyByDate, rtCustom                     }
  if Value <> FRepeatCode then begin
    FRepeatCode := Value;
    if value <> rtCustom then
      SetCustInterval(0);
    Changed := true;
  end;
end;

procedure TVpEvent.SetRepeatRangeEnd(Value: TDateTime);
begin
  if Value > StartTime then begin
    FRepeatRangeEnd := Value;
    Changed := true;
  end;
end;

procedure TVpEvent.SetCustInterval(Value: Integer);
begin
  if Value <> FCustInterval then begin
    if RepeatCode = rtCustom then
      FCustInterval := Value
    else
      FCustInterval := 0;
    Changed := true;
  end;
end;

procedure TVpEvent.SetStartTime(Value: TDateTime);
begin
  if Value <> FStartTIme then begin
    FStartTime := Value;
    Changed := true;
  end;
end;


(*****************************************************************************)
{ TVpSchedule }
(*****************************************************************************)

constructor TVpSchedule.Create(Owner: TVpResource);
begin
  inherited Create;
  FOwner := Owner;
  FBatchUpdate := 0;
  FEventList := TList.Create;
end;

destructor TVpSchedule.Destroy;
begin
  ClearEvents;
  FEventList.Free;
  inherited;
end;

procedure TVpSchedule.Sort;
begin
  { for greater performance, we don't sort while doing batch updates. }
  if FBatchUpdate > 0 then
    exit;

  { WARNING!!  The DayView component is heavily dependent upon the events
    being properly sorted. Sorting is based on the CompareEventTimes function.
    If you change the way this procedure works, you WILL break the DayView
    component!!! }
  FEventList.Sort(@CompareEvents);
end;

{Adds the event to the eventlist and returns a pointer to it, or nil on failure}
function TVpSchedule.AddEvent(RecordID: Integer; StartTime,
  EndTime: TDateTime): TVpEvent;
begin
  Result := TVpEvent.Create(Self);
  try
    Result.Loading := true;
    FEventList.Add(Result);
    Result.RecordID := RecordID;
    Result.StartTime := StartTime;
    Result.EndTime := EndTime;
    Result.Loading := false;
    Sort;
  except
    Result.Free;
    raise EFailToCreateEvent.Create;
  end;
end;

procedure TVpSchedule.ClearEvents;
begin
  BatchUpdate(true);
  try
    while FEventList.Count > 0 do
      TVpEvent(FEventList.Last).Free;
  finally
    BatchUpdate(false);
  end;
end;

procedure TVpSchedule.ClearGroupEvents;
var
  i: Integer;
  event: TVpEvent;
begin
  for i := FEventList.Count-1 downto 0 do begin
    event := TVpEvent(FEventList[i]);
    if event.IsOverlayed then begin
      FEventList.Delete(i);
      event.Free;
    end;
  end;
end;

procedure TVpSchedule.BatchUpdate(Value: Boolean);
begin
  if Value then
    FBatchUpdate := FBatchUpdate + 1
  else
    FBatchUpdate := FBatchUpdate - 1;

  if FBatchUpdate < 1 then begin
    FBatchUpdate := 0;
    Sort;
  end;
end;

{ Initiates destruction of the specified event which also removes it from the
  list. }
procedure TVpSchedule.DeleteEvent(Event: TVpEvent);
begin
  Event.Deleted := true;
  Owner.EventsDirty := true;
end;

function TVpSchedule.GetEvent(Index: Integer): TVpEvent;
begin
  { Returns an event on success or nil on failure }
  result := TVpEvent(FEventList[Index]);
end;

function TVpSchedule.RepeatsOn(Event: TVpEvent; Day: TDateTime): Boolean;
var
  EY, EM, ED: Word;
  NY, NM, ND: Word;
  EventWkDay, EventDayCount: Word;
  ThisWkDay, ThisDayCount: Word;
  EventJulian, ThisJulian: Word;
  DayInRepeatRange: Boolean;
begin
  result := false;

  DayInRepeatRange := (Day > trunc(Event.StartTime)) and
    ((Event.RepeatRangeEnd = 0) or (Day < trunc(Event.RepeatRangeEnd) + 1));

  if (Event.RepeatCode <> rtNone) and
     ((Event.RepeatRangeEnd = 0) or (trunc(Event.RepeatRangeEnd + 1) > now)) then
  begin
    case Event.RepeatCode of
      rtDaily:
        if DayInRepeatRange then
//        if (Day < trunc(Event.RepeatRangeEnd) + 1) and (Day > trunc(Event.StartTime)) then
          result := true;

      rtWeekly:
        if DayInRepeatRange then
//        if (Day < trunc(Event.RepeatRangeEnd) + 1) and (Day > trunc(Event.StartTime)) then
          result := (Trunc(Day) - Trunc(Event.StartTime)) mod 7 = 0;

      rtMonthlyByDay:
        if DayInRepeatRange then
        //if (Day < trunc(Event.RepeatRangeEnd) + 1) and (Day > trunc(Event.StartTime)) then
        begin
          // Get the year, month and day of the first event in the series
          DecodeDate(Event.StartTime, EY, EM, ED);
          // Get the weekday of the first event in the series
          EventWkDay := DayOfWeek(Event.StartTime);
          // Get the occurence of the first event in the series (First Monday, Third Monday, etc...)
          EventDayCount := ED div 7 + 1;
          // Get the year, month and day of the "Day" parameter
          DecodeDate(Day, NY, NM, ND);
          // Get the weekday of the "Day" parameter
          ThisWkDay := DayOfWeek(Day);
          // Get the weekday occurence of the "Day" parameter (First Monday, Third Monday, etc...)
          ThisDayCount := ND div 7 + 1;
          // If (ThisWeekDay is equal to EventWkDay) and (ThisDayCount is equal to EventDayCount)
          // then we have a recurrence on this day
          result := (ThisWkDay = EventWkDay) and (ThisDayCount = EventDayCount);
        end;

      rtMonthlyByDate:
        if DayInRepeatRange then
//        if (Day < trunc(Event.RepeatRangeEnd) + 1) and (Day > trunc(Event.StartTime)) then
        begin
          // Get the year, month and day of the first event in the series
          DecodeDate(Event.StartTime, EY, EM, ED);
          // Get the year, month and day of the "Day" parameter
          DecodeDate(Day, NY, NM, ND);
          // If the day values are equal then we have a recurrence on this day
          result := ED = ND;
        end;

      rtYearlyByDay:
        if DayInRepeatRange then
//        if (Day < trunc(Event.RepeatRangeEnd) + 1) and (Day > trunc(Event.StartTime)) then
        begin
          // Get the julian date of the first event in the series
          EventJulian := GetJulianDate(Event.StartTime);
          // Get the julian date of the "Day" parameter
          ThisJulian := GetJulianDate(Day);
          // Ff  the julian values are equal then we have a recurrence on this day
          result := EventJulian = ThisJulian;
        end;

      rtYearlyByDate:
        if DayInRepeatRange then
//        if (Day < trunc(Event.RepeatRangeEnd) + 1) and (Day > trunc(Event.StartTime)) then
        begin
          // Get the year, month and day of the first event in the series.
          DecodeDate(Event.StartTime, EY, EM, ED);
          // Get the year, month and day of the "Day" parameter.
          DecodeDate(Day, NY, NM, ND);
          // If  the day values and month values are equal then we have a recurrence on this day
          result := (ED = ND) and (EM = NM);
        end;

      rtCustom:
        if DayInRepeatRange and (Event.CustomInterval > 0) then
//        if (Day < trunc(Event.RepeatRangeEnd) + 1) and (Day > trunc(Event.StartTime)) then
        begin
          // If the number of elapsed days between the "Day" parameter and
          // the event start time is evenly divisible by the event's custom
          // interval, then we have a recurrence on this day
          result := (Trunc(Day) - Trunc(Event.StartTime)) mod Event.CustomInterval = 0;
        end;
    end;
  end;
end;
{=====}

function TVpSchedule.EventCountByDay(Value: TDateTime): Integer;
var
  I: Integer;
  Event: TVpEvent;
begin
  result := 0;
  for I := 0 to pred(EventCount) do begin
    Event := GetEvent(I);
    // If this is a repeating event and it falls on today then inc result
    if (Event.RepeatCode > rtNone) and RepeatsOn(Event, Value) then
      Inc(Result)
    // Otherwise if it is an event that naturally falls on today, then inc result
//    else if ((trunc(Value) >= trunc(Event.StartTime))
//         and (trunc(Value) <= trunc(Event.EndTime))) then
    else
    if DateInRange(Value, Event.StartTime, Event.EndTime, true) then
      Inc(Result);
  end;
end;
{=====}

procedure TVpSchedule.EventsByDate(Date: TDateTime; EventList: TList);
var
  I: Integer;
  Event: TVpEvent;
begin
  if EventCountByDay(Date) = 0 then
    EventList.Clear

  else begin
    // Add this day's events to the Event List.
    for I := 0 to pred(EventCount) do begin
      Event := GetEvent(I);

      // If this is a repeating event and it falls on "Date" then add it to the list.
      if (Event.RepeatCode > rtNone) and RepeatsOn(Event, Date) then
        EventList.Add(Event)
      else
      // otherwise if this event naturally falls on "Date" then add it to the list.
      if DateInRange(Date, Event.StartTime, Event.EndTime, true) then
        EventList.Add(Event);
    end;
  end;
end;
{=====}

procedure TVpSchedule.AllDayEventsByDate(Date: TDateTime; EventList: TList);
var
  I: Integer;
  Event: TVpEvent;
begin
  EventList.Clear;

  if EventCountByDay(Date) = 0 then
    Exit

  else begin
    // Add this days events to the Event List.
    for I := 0 to pred(EventCount) do begin
      Event := GetEvent(I);
      if Event.AllDayEvent and
        (DateInRange(Date, Event.StartTime, Event.EndTime, true) or RepeatsOn(Event, Date))
      then
        EventList.Add(Event);
    end;
  end;
end;
{=====}

(*  wp: Commented because it is not called from anywhere...

{ binary search }
function TVpSchedule.FindTimeSlot(StartTime, EndTime: TDateTime): Boolean;
var
  L, R, M: Integer;
  CStart, CEnd, CompStart, CompEnd: integer; { comparison results }
  HitStart, HitEnd, HitStraddle: Boolean;
begin
  HitStart := false;
  HitEnd := false;
  HItStraddle := false;
  { Set left and right indexes }
  L := 0;
  R := Pred(FEventList.Count);
  while (L <= R) do begin
    { Calculate the middle index }
    M := (L + R) div 2;

    { Check to see if the middle item straddles our start time                }
    { Compare the the middle item's starttime against the passed in times     }
    CStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);
    CEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, StartTime);
    { if the middle item's starttime is less than or equal to the given       }
    { starttime AND the middle item's endtime is greater than or equal to the }
    { given starttime then we've hit at the start time                        }
    if ((CStart <= 0) and (CEnd >= 0)) then HitStart := true;

    { Check to see if the middle item straddles our end time                  }
    { Compare the the middle item's Endtime against the passed in times       }
    CStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, EndTime);
    CEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, EndTime);
    { if the middle item's starttime is less than or equal to the given       }
    { endtime AND the middle item's endtime is greater than or equal to the   }
    { given endtime then we've hit at the end time                            }
    if ((CStart <= 0) and (CEnd >= 0)) then HitEnd := true;

    if (not HitStart) and (not HitEnd) then begin
      { Check to see if our times fall completely within the middle item      }
      CompStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);
      CompEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, EndTime);
      { if the middle item's starttime is less than our starttime AND its     }
      { endtime is greater than our endtime, then teh middle item straddles   }
      { our times                                                             }
      if ((CompStart <= 0) and (CompEnd >= 0)) then HitStraddle := true;

      if not HItStraddle then
      { Check to see if the middle item falls completely inside our times     }
      CompStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);          // wp: Is this correct? Strange indentation!
      CompEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, EndTime);
      { if the middle item's starttime is less than our starttime AND its     }
      { endtime is greater than our endtime, then teh middle item straddles   }
      { our times                                                             }
      if ((CompStart >= 0) and (CompEnd <= 0)) then HitStraddle := true;
    end;

    if (HitStart or HitEnd or HitStraddle) then begin
      result := true;
      exit;
    end
    else begin
      { No hit so keep going }
      CStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);
      { if the middle item's starttime is less than or equal to the given       }
      { starttime AND the middle item's endtime is greater than or equal to the }
      { given starttime then we've hit at the start time                        }
      if (CStart < 0) then
        L := Succ(M)
      else
        R := Pred(M);
    end;
  end;
  { if we got here then we didn't hit an existing item }
  result := false;
end;
{=====}        *)

function TVpSchedule.GetCount: Integer;
begin
  result := FEventList.Count;
end;


(*****************************************************************************)
{ TVpContact }
(*****************************************************************************)
constructor TVpContact.Create(Owner: TVpContacts);
begin
  inherited Create;

  FChanged := false;
  FOwner := Owner;

  FPhoneType1 := Ord(ptWork);
  FPhoneType2 := Ord(ptHome);
  FPhoneType3 := Ord(ptWorkFax);
  FPhoneType4 := Ord(ptMobile);
  FPhoneType5 := Ord(ptAssistant);

  FEMailType1 := ord(mtWork);
  FEMailType2 := ord(mtHome);
  FEMailType3 := ord(mtOther);

  FWebsiteType1 := ord(wtBusiness);
  FWebsiteType2 := ord(wtPersonal);

  FAddressType1 := ord(atWork);
  FAddressType2 := ord(atHome);
end;

destructor TVpContact.Destroy;
var
  idx: Integer;
begin
  // Remove self from owners list
  if (FOwner <> nil) then begin
    idx := FOwner.FContactsList.IndexOf(self);
    if idx > -1 then FOwner.FContactsList.Delete(idx);
  end;
  inherited;
end;

function TVpContact.ContainsContactData: Boolean;
begin
  Result := (FPhone1 <> '') or (FPhone3 <> '') or (FPhone3 <> '') or
            (FPhone4 <> '') or (FPhone5 <> '') or
            (FEMail1 <> '') or (FEMail2 <> '') or (FEMail3 <> '') or
            (FWebsite1 <> '') or (FWebsite2 <> '');
end;

function TVpContact.ContainsHomeData: Boolean;
begin
  Result := (FAddress2 <> '') or (FCity2 <> '') or (FState2 <> '') or (FCountry2 <> '');
end;

function TVpContact.ContainsWorkData: Boolean;
begin
  Result := (Address1 <> '') or (FCity1 <> '') or (FState1 <> '') or (FCountry1 <> '');
end;

function TVpContact.FullName : string;
begin
  if (FFirstName = '') and (FLastName = '') then
    Result := ''
  else
    Result := FFirstName + ' ' + FLastName;
end;

procedure TVpContact.LoadFromVCard(ACard: TVpVCard);
const
  NUM_PHONES = 5;
var
  s: String;
  dt: TDateTime;
  phoneIdx: Integer;
  phones: array[1..NUM_PHONES] of ^String;
  phonetypes: array[1..NUM_PHONES] of ^Integer;
begin
  phones[1] := @FPhone1;   phonetypes[1] := @FPhoneType1;
  phones[2] := @FPhone2;   phonetypes[2] := @FPhoneType2;
  phones[3] := @FPhone3;   phonetypes[3] := @FPhoneType3;
  phones[4] := @FPhone4;   phonetypes[4] := @FPhoneType4;
  phones[5] := @FPhone5;   phonetypes[5] := @FPhoneType5;

  FLastName := ACard.LastName;
  FFirstName := ACard.FirstName;
  FTitle := ACard.Title;

  FCompany := ACard.Company;
  FAddress1 := ACard.WorkAddress;
  FCity1 := ACard.WorkCity;
  FZip1 := ACard.WorkZip;
  FState1 := ACard.WorkState;
  FCountry1 := ACard.WorkCountry;
  FAddressType1 := ord(atWork);

  FAddress2 := ACard.HomeAddress;
  FCity2 := ACard.HomeCity;
  FZip2 := ACard.HomeZip;
  FState2 := ACard.HomeState;
  FCountry2 := ACard.HomeCountry;
  FAddressType2 := ord(atHome);

  FEmail1 := ACard.WorkEMail;
  FEMailType1 := ord(mtWork);
  FEmail2 := ACard.HomeEMail;
  FEMailType2 := ord(mtHome);

  phoneIdx := 1;
  s := ACard.Mobile;
  if s <> '' then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptMobile);
    inc(phoneidx);
  end;
  s := ACard.WorkPhone;
  if s <> '' then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptWork);
    inc(phoneidx);
  end;
  s := ACard.WorkFax;
  if s <> '' then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptWorkFax);
    inc(phoneidx);
  end;
  s := ACard.Pager;
  if s <> '' then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptPager);
    inc(phoneidx);
  end;
  s := ACard.CarPhone;
  if (s <> '') and (phoneIdx <= NUM_PHONES) then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptCar);
    inc(phoneidx);
  end;
  s := ACard.HomePhone;
  if (s <> '') and (phoneIdx <= NUM_PHONES) then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptHome);
    inc(phoneidx);
  end;
  s := ACard.HomeFax;
  if (s <> '') and (phoneIdx <= NUM_PHONES) then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptHomeFax);
    inc(phoneidx);
  end;
  s := ACard.ISDN;
  if (s <> '') and (phoneIdx <= NUM_PHONES) then begin
    phones[phoneidx]^ := s;
    phonetypes[phoneidx]^ := ord(ptISDN);
    inc(phoneidx);
  end;

  s := ACard.Value['BDAY', ''];
  if s <> '' then begin
    dt := VCardDate(s);
    if dt > -1 then FBirthdate := dt;
  end;

  s := ACard.Value['ANNIVERSARY', ''];
  if s <> '' then begin
    dt := VCardDate(s);
    if dt > -1 then FAnniversary := dt;
  end;

  FNotes := ACard.Value['NOTE', ''];

  FPosition := ACard.Value['TITLE', ''];
  s := ACard.Value['ROLE', ''];
  if s <> '' then begin
    if FPosition = '' then
      FPosition := s
    else
      FPosition := FPosition + '; ' + s;
  end;

  FOwner.FOwner.ContactsDirty := true;
  FChanged := true;
end;


procedure TVpContact.SetBirthDate(Value: TDateTIme);
begin
  if Value <> FBirthdate then begin
    FBirthdate := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetAnniversary(Value: TDateTIme);
begin
  if Value <> FAnniversary then begin
    FAnniversary := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetAddress1(const Value: string);
begin
  if Value <> FAddress1 then begin
    FAddress1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetAddress2(const Value: string);
begin
  if Value <> FAddress2 then begin
    FAddress2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetAddressType1(Value: integer);
begin
  if Value <> FAddressType1 then begin
    FAddressType1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetAddressType2(Value: integer);
begin
  if Value <> FAddressType2 then begin
    FAddressType2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCategory(Value: integer);
begin
  if Value <> FCategory then begin
    FCategory := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetChanged(Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
    if FChanged then
      FOwner.FOwner.ContactsDirty := true;
  end;
end;

procedure TVpContact.SetCity1(const Value: string);
begin
  if Value <> FCity1 then begin
    FCity1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCity2(const Value: string);
begin
  if Value <> FCity2 then begin
    FCity2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCompany(const Value: string);
begin
  if Value <> FCompany then begin
    FCompany := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCountry1(const Value: string);
begin
  if Value <> FCountry1 then begin
    FCountry1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCountry2(const Value: string);
begin
  if Value <> FCountry2 then begin
    FCountry2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCustom1(const Value: string);
begin
  if Value <> FCustom1 then begin
    FCustom1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCustom2(const Value: string);
begin
  if Value <> FCustom2 then begin
    FCustom2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCustom3(const Value: string);
begin
  if Value <> FCustom3 then begin
    FCustom3 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetCustom4(const Value: string);
begin
  if Value <> FCustom4 then begin
    FCustom4 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetDeleted(Value: Boolean);
begin
  if Value <> FDeleted then begin
    FDeleted := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetDepartment(const Value: String);
begin
  if Value <> FDepartment then begin
    FDepartment := Value;
    Changed := True;
  end;
end;

procedure TVpContact.SetEMail1(const Value: string);
begin
  if Value <> FEMail1 then begin
    FEMail1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetEMail2(const Value: string);
begin
  if Value <> FEMail2 then begin
    FEMail2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetEMail3(const Value: string);
begin
  if Value <> FEMail3 then begin
    FEMail3 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetEMailType1(const Value: Integer);
begin
  if Value <> FEMailType1 then begin
    FEMailType1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetEMailType2(const Value: Integer);
begin
  if Value <> FEMailType2 then begin
    FEMailType2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetEMailType3(const Value: Integer);
begin
  if Value <> FEMailType3 then begin
    FEMailType3 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetFirstName(const Value: string);
begin
  if Value <> FFirstName then begin
    FFirstName := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetLastName(const Value: string);
begin
  if Value <> FLastName then begin
    FLastName := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetNotes(const Value: string);
begin
  if Value <> FNotes then begin
    FNotes := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhone1(const Value: string);
begin
  if Value <> FPhone1 then begin
    FPhone1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhone2(const Value: string);
begin
  if Value <> FPhone2 then begin
    FPhone2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhone3(const Value: string);
begin
  if Value <> FPhone3 then begin
    FPhone3 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhone4(const Value: string);
begin
  if Value <> FPhone4 then begin
    FPhone4 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhone5(const Value: string);
begin
  if Value <> FPhone5 then begin
    FPhone5 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhoneType1(Value: Integer);
begin
  if Value <> FPhoneType1 then begin
    FPhoneType1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhoneType2(Value: Integer);
begin
  if Value <> FPhoneType2 then begin
    FPhoneType2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhoneType3(Value: Integer);
begin
  if Value <> FPhoneType3 then begin
    FPhoneType3 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhoneType4(Value: Integer);
begin
  if Value <> FPhoneType4 then begin
    FPhoneType4 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPhoneType5(Value: Integer);
begin
  if Value <> FPhoneType5 then begin
    FPhoneType5 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetPosition(const Value: string);
begin
  if Value <> FPosition then begin
    FPosition := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetRecordID(Value: Integer);
begin
  if Value <> FRecordID then begin
    FRecordID := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetState1(const Value: string);
begin
  if Value <> FState1 then begin
    FState1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetState2(const Value: string);
begin
  if Value <> FState2 then begin
    FState2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetTitle(const Value: string);
begin
  if Value <> FTitle then begin
    FTitle := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetWebsite1(Value: String);
begin
  if Value <> FWebsite1 then begin
    FWebsite1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetWebsite2(Value: String);
begin
  if Value <> FWebsite2 then begin
    FWebsite2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetWebsiteType1(Value: Integer);
begin
  if Value <> FWebsiteType1 then begin
    FWebsiteType1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetWebsiteType2(Value: Integer);
begin
  if Value <> FWebsiteType2 then begin
    FWebsiteType2 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetZip1(const Value: string);
begin
  if Value <> FZip1 then begin
    FZip1 := Value;
    Changed := true;
  end;
end;

procedure TVpContact.SetZip2(const Value: string);
begin
  if Value <> FZip2 then begin
    FZip2 := Value;
    Changed := true;
  end;
end;


(*****************************************************************************)
{ TVpContacts }
(*****************************************************************************)

constructor TVpContacts.Create(Owner: TVpResource);
begin
  inherited Create;
  FOwner := Owner;
  FContactsList := TList.Create;

  FContactSort := csLastFirst;
end;

destructor TVpContacts.Destroy;
begin
  ClearContacts;
  FContactsList.Free;
  inherited;
end;

function TVpContacts.AddContact(RecordID: Integer): TVpContact;
var
  Contact: TVpContact;
begin
  Contact := TVpContact.Create(Self);
  try
    Contact.Loading := true;
    FContactsList.Add(Contact);
    Contact.RecordID := RecordID;
    Contact.Loading := false;
    result := Contact;
  except
    Contact.Free;
    raise EFailToCreateContact.Create;
  end;
end;

procedure TVpContacts.BatchUpdate(Value: Boolean);
begin
  if Value then
    Inc(FBatchUpdate)
  else
    Dec(FBatchUpdate);

  if FBatchUpdate < 1 then begin
    FBatchUpdate := 0;
    Sort;
  end;
end;

function TVpContacts.Count: Integer;
begin
  result := FContactsList.Count;
end;

procedure TVpContacts.Sort;
begin
  // For greater performance, we don't sort while doing batch updates.
  if FBatchUpdate > 0 then
    exit;

  // Do the sort
  if ContactSort = csFirstLast then
    FContactsList.Sort(@CompareContacts_FirstLast)
  else
    FContactsList.Sort(@CompareContacts_LastFirst);
end;

function TVpContacts.Last: TVpContact;
begin
  result := TVpContact(FContactsList[FContactsList.Count - 1]);
end;

function TVpContacts.First: TVpContact;
begin
  result := TVpContact(FContactsList[0]);
end;

procedure TVpContacts.DeleteContact(Contact: TVpContact);
begin
  // Contacts automatically remove themselves from the list in their destructor
  Contact.Free;
end;

function TVpContacts.GetContact(Index: Integer): TVpContact;
begin
  result := TVpContact(FContactsList[Index]);
end;

procedure TVpContacts.ClearContacts;
begin
  BatchUpdate(true);
  try
    while FContactsList.Count > 0 do
      TVpContact(FContactsList.Last).Free;
  finally
    BatchUpdate(false);
  end;
end;

{ new function introduced to support the new buttonbar component. }
function TVpContacts.FindContactByName(const AName: string;               
  CaseInsensitive: Boolean): TVpContact;
var                                                                      
  I: Integer;                                                            
  SearchStr: String;                                                     
  SearchLength: Integer;
  SearchName: String;
begin                                                                    
  Result := nil;                                                         
                                                                         
  // To enhance performance, uppercase the input name and get its length only once
  if CaseInsensitive then                                                
    SearchStr := UpperCase(AName)
  else                                                                   
    SearchStr := AName;                                                   
  SearchLength := Length(SearchStr);                                     

  // Iterate the contacts looking for a match
  for I := 0 to FContactsList.Count - 1 do begin
    SearchName := Copy(TVpContact(FContactsList[I]).LastName, 1, SearchLength);
    if CaseInsensitive then
      SearchName := Uppercase(SearchName);
    // We found a match, so return it and bail out
    if SearchName = SearchStr then begin
      Result := TVpContact(FContactsList[I]);
      exit;
    end;
  end;
end;

{ new function introduced to support the new buttonbar component }       
function TVpContacts.FindContactIndexByName(const Name: string;          
  CaseInsensitive: Boolean): Integer;                                    
var                                                                      
  Contact: TVpContact;                                                   
begin                                                                    
  result := -1;                                                          
  Contact := FindContactByName(Name, CaseInsensitive);                   
  if Contact <> nil then                                                 
    Result := FContactsList.IndexOf(Contact);                            
end;                                                                     

procedure TVpContacts.SetContactSort(const v: TVpContactSort);
begin
  if v <> FContactSort then begin
    FContactSort := v;
    Sort;
  end;
end;


(*****************************************************************************)
{ TVpTask }
(*****************************************************************************)

constructor TVpTask.Create(Owner: TVpTasks);
begin
  inherited Create;
  FChanged := false;
  FOwner := Owner;
  SetCreatedOn(Now);
  FDescription := '';
  FItemIndex := -1;
end;

destructor TVpTask.Destroy;
var
  idx: Integer;
begin
  // Remove self from owners list
  if (FOwner <> nil) then begin
    idx := FOwner.FTaskList.IndexOf(Self);
    if idx > -1 then FOwner.FTasklist.Delete(idx);
    FOwner.Sort;
  end;
  inherited;
end;

function TVpTask.IsOverdue: Boolean;
begin
  result := (Trunc(DueDate) < now + 1);
end;

procedure TVpTask.LoadFromICalendar(AEntry: TVpICalToDo);
var
  cat: String;
  i: Integer;
  ct: TVpCategoryType;
  catFound: Boolean;
begin
  if AEntry = nil then
    exit;

  { Standard event properties }
  FDescription := AEntry.Summary;
  FDetails := AEntry.Comment;
  FCreatedOn := AEntry.StartTime[false];
  FDueDate := AEntry.DueTime[false];
  FCompletedOn := AEntry.CompletedTime[false];

  { Status }
  FComplete := SameText(AEntry.Status, 'COMPLETED');

  { Priority }
  case AEntry.Priority of
    1, 2, 3: FPriority := ord(tpHigh);
    4, 5, 6: FPriority := ord(tpNormal);
    7, 8, 9: FPriority := ord(tpLow);
    else     FPriority := ord(tpNormal);
  end;

  { Category }
  { tvplanit has only 1 category, ical may have several. We pick the first one
    defined by TVpCategorytype. If none is found we select ctOther. }
  FCategory := ord(ctOther);
  catFound := false;
  if AEntry.CategoryCount > 0 then begin
    for i := 0 to AEntry.CategoryCount-1 do begin
      cat := AEntry.category[i];
      for ct in TVpCategoryType do begin
        if cat = CategoryLabel(ct) then begin
          FCategory := ord(ct);
          catFound := true;
          break;
        end;
      end;
      if catFound then break;
    end;
  end;
end;

procedure TVpTask.SetCategory(const Value: Integer);
begin
  if Value <> FCategory then begin
    FCategory := Value;
    Changed := true;
  end;
end;

procedure TVpTask.SetChanged(const Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
    if FChanged then
      Owner.FOwner.TasksDirty := true;                                   
  end;
end;

procedure TVpTask.SetComplete(const Value: Boolean);
begin
  if Value <> FComplete then begin
    FComplete := Value;
    if FComplete then
      SetCompletedOn(Now)
    else
      SetCompletedOn(0.0);
  end;
end;

procedure TVpTask.SetCompletedOn(const Value: TDateTIme);
begin
  if Value <> FCompletedOn then begin
    FCompletedOn := Value;
    Changed := true;
  end;
end;

procedure TVpTask.SetCreatedOn(const Value: TDateTime);
begin
  if Value <> FCreatedOn then begin
    FCreatedOn := Value;
    Changed := true;
  end;
end;

procedure TVpTask.SetDescription(const Value: string);
begin
  if Value <> FDescription then begin
    FDescription := Value;
    Changed := true;
  end;
end;

procedure TVpTask.SetPriority(const Value: Integer);
begin
  if Value <> FPriority then begin
    FPriority := Value;
    Changed := true;
  end;
end;

procedure TVpTask.SetDetails(const Value: string);
begin
  if Value <> FDetails then begin
    FDetails := Value;
    Changed := true;
  end;
end;

procedure TVpTask.SetDueDate(const Value: TDateTime);
begin
  // Trunc the time element from the DueDate value so that it reflects the Date only.
  if FDueDate <> Trunc(Value) then begin
    FDueDate := Trunc(Value);
    Changed := true;
  end;
end;


(*****************************************************************************)
{ TVpTaskList }
(*****************************************************************************)

constructor TVpTasks.Create(Owner: TVpResource);
begin
  inherited Create;
  FOwner := Owner;
  FTaskList := TList.Create;
  FTaskList.Clear;
end;

destructor TVpTasks.Destroy;
begin
  ClearTasks;
  FTaskList.Free;
  inherited;
end;

function TVpTasks.AddTask(RecordID: Integer): TVpTask;
var
  Task: TVpTask;
begin
  Task := TVpTask.Create(Self);
  try
    Result := Task;
    Task.Loading := true;
    Task.FItemIndex := FTaskList.Add(result);
    Task.RecordID := RecordID;
    FOwner.TasksDirty := true;
    Task.Loading := false;
    {the data which to sort by has not yet been added to the object}     
//    Sort;                                                              
  except
    Task.Free;
    raise EFailToCreateTask.Create;
  end;
end;

function TVpTasks.Count: Integer;
begin
  result := FTaskList.Count;
end;

function TVpTasks.IndexOf(ATask: TVpTask): Integer;
begin
  Result := FTaskList.IndexOf(ATask);
end;

function TVpTasks.Last: TVpTask;
begin
  result := TVpTask(FTaskList.Last);
end;

function TVpTasks.First: TVpTask;
begin
  result := TVpTask(FTaskList.First);
end;

function TVpTasks.CountByDay(Date: TDateTime): Integer;
var
  i: Integer;
  ATask: TVpTask;
begin
  Result := 0;

  for i := 0 to pred(Count) do begin
    ATask := GetTask(i);
    if Trunc(ATask.DueDate) = Trunc(Date) then
      Inc(Result);
  end;
end;

function TVpTasks.LastByDay(Date: TDateTime): TVpTask;
var
  i: Integer;
  ATask: TVpTask;
begin
  result := nil;
  for i := 0 to pred(Count) do begin
    ATask := GetTask(i);
    if Trunc(ATask.CreatedOn) = Trunc(Date) then begin
      Result := ATask;
      break;
    end;
  end;
end;

function TVpTasks.FirstByDay(Date: TDateTime): TVpTask;
var
  i: Integer;
  ATask: TVpTask;
begin
  result := nil;
  for i := 0 to pred(Count) do begin
    ATask := GetTask(i);
    if Trunc(ATask.CreatedOn) = Trunc(Date) then begin
      Result := ATask;
      Break;
    end;
  end;
end;

procedure TVpTasks.ClearTasks;
begin
  BatchUpdate(true);
  try
    while FTaskList.Count > 0 do
      TVpTask(FTaskList.Last).Free;
  finally
    BatchUpdate(False);
  end;
end;

procedure TVpTasks.BatchUpdate(value: Boolean);
begin
  if Value then
    Inc(FBatchUpdate)
  else
    Dec(FBatchUpdate);

  if FBatchUpdate < 1 then begin
    FBatchUpdate := 0;
    Sort;
  end;
end;

procedure TVpTasks.Sort;
var
  i: Integer;
begin
  // For greater performance, we don't sort while doing batch updates.
  if FBatchUpdate > 0 then
    exit;
  FTaskList.Sort(@CompareTasks);
  // Fix object embedded ItemIndexes     // wp --- maybe this can be removed
  for i:=0 to FTaskList.Count-1 do
    TVpTask(FTaskList[i]).FItemIndex := i;
end;

procedure TVpTasks.DeleteTask(Task: TVpTask);
begin
  // Tasks automatically remove themselves from the list in their destructor
  Task.Free;
end;

function TVpTasks.GetTask(Index: Integer): TVpTask;
begin
  result := TVpTask(FTaskList[Index]);
end;

end.
