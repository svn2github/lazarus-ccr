unit spkt_Exceptions;

{$mode delphi}

(*******************************************************************************
*                                                                              *
*  File:        spkt_Exceptions.pas                                            *
*  Description: Exception classes of the toolbar                               *
*  Copyright:   (c) 2009 by Spook.                                             *
*  License:     Modified LGPL (with linking exception, like Lazarus LCL)       *
'               See "license.txt" in this installation                         *
*                                                                              *
*******************************************************************************)

interface

uses
  SysUtils;

type
  InternalException = class(Exception);
  AssignException = class(Exception);
  RuntimeException = class(Exception);
  ListException = class(Exception);

implementation

end.
