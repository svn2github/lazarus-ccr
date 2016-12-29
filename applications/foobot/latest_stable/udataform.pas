unit udataform;
{ Foobot Interrogator data display

  Copyright (C)2016 Gordon Bamber minsadorada@charcodelvalle.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, Variants, dateutils;

type

  { Tdataform }

  Tdataform = class(TForm)
    BitBtn1: TBitBtn;
    datagrid: TStringGrid;
    grp_data: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  dataform: Tdataform;

implementation

uses umainform,foobot_utility;

{$R *.lfm}

{ Tdataform }

procedure Tdataform.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  Caption := Application.Title + ' Data';
end;

procedure Tdataform.FormShow(Sender: TObject);
var
  J, K, iCount: integer;
  Mydatapoint: variant;
  dtDate, dtStart, dtEnd: TDateTime;
  sStart, sEnd: string;
  iUnixSecs: int64;
begin
  with mainform do
  begin
    dtStart := UnixToDateTime(FoobotDataObject.Start);
    dtEnd := UnixToDateTime(FoobotDataObject.&end);
    sStart := FormatDateTime('dd/mm tt', dtStart);
    sEnd := FormatDateTime('dd/mm tt', dtEnd);

    grp_data.Caption := 'Foobot ' +
      FoobotIdentityObject.FoobotIdentityList.Items[CurrentFoobot].Name +
      ' From ' + sStart + ' to ' + sEnd;
    if mainform.FetchType = dfLast then
      grp_data.Caption := grp_data.Caption + ' Capture last = ' +
        mainform.rg_interval.Items[mainform.rg_interval.ItemIndex] + ', ';
    grp_data.Caption := grp_data.Caption + 'Average by = ' +
      mainform.rg_intervalAverageBy.Items[mainform.rg_intervalAverageBy.ItemIndex] + ')';

    for iCount := 0 to Pred(FoobotDataObject.sensors.Count) do
    begin
      datagrid.Cells[iCount, 0] :=
        FoobotDataObject.sensors[iCount] + ' (' + FoobotDataObject.units[iCount] + ')';
    end;
    // J=Column, K=Row
    for K := VarArrayLowBound(FoobotDataObject.datapoints, 1)
      to VarArrayHighBound(FoobotDataObject.datapoints, 1) do
    begin
      for J := VarArrayLowBound(FoobotDataObject.datapoints[K], 1)
        to VarArrayHighBound(FoobotDataObject.datapoints[K], 1) do
      begin
        Mydatapoint := FoobotDataObject.datapoints[K][J];
        dataGrid.RowCount := K + 2;
        if J = 0 then // First field is a DateTime
        begin
          if K = VarArrayHighBound(FoobotDataObject.datapoints, 1) then
            datagrid.Cells[J, K + 1] := 'Latest' // Last entry is always latest
          else
          begin
            iUnixSecs := int64(Mydatapoint);
            dtDate := UnixToDateTime(iUnixSecs);
            datagrid.Cells[J, K + 1] := FormatDateTime('dd/mm - tt', dtDate);
          end;
        end
        else
          datagrid.Cells[J, K + 1] := VarToStr(Mydatapoint);
      end;
    end;

  end;
end;

end.
