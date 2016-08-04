{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2010 by Michael Van Canneyt, member of the
   Free Pascal development team

   DBUS proxy component

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{$mode objfpc}
{$h+}
unit dbusproxy;

interface

uses sysutils, classes, dbus, dbuscomp;

Type

  { TDBUSProxy }

  TDBUSProxy = Class(TComponent)
  private
    FConnection: TCustomDBUSConnection;
    FDestination: String;
    FObjectPath: String;
    FTimeout: Integer;
    procedure SetConnection(const AValue: TCustomDBUSConnection);
    procedure SetDestination(const AValue: String);
    procedure SetObjectPath(const AValue: String);
  Protected
    Procedure UnhandledArgument(Const AName,AType : String);
    Function GetMethodCallMessage(Const AInterface, AMethodName : String) : TDBusMethodCallMessage;
    Function GetMessageReply(M : TDBUSMethodCallMessage) : TDBUSMessage;
  Public
    Constructor Create(AOwner : TComponent); override;
    Property Connection : TCustomDBUSConnection Read FConnection Write SetConnection;
    Property ObjectPath : String Read FObjectPath Write SetObjectPath;
    Property Destination : String Read FDestination Write SetDestination;
    Property Timeout : Integer Read FTimeout Write FTimeOut;
  end;

Implementation

{ TDBUSProxy }

procedure TDBUSProxy.SetConnection(const AValue: TCustomDBUSConnection);
begin
  if FConnection=AValue then exit;
  FConnection:=AValue;
end;

procedure TDBUSProxy.SetDestination(const AValue: String);
begin
  if FDestination=AValue then exit;
  FDestination:=AValue;
end;

procedure TDBUSProxy.SetObjectPath(const AValue: String);
begin
  if FObjectPath=AValue then exit;
  FObjectPath:=AValue;
end;

procedure TDBUSProxy.UnhandledArgument(const AName, AType: String);
begin
  Raise Exception.CreateFmt('Type of argument "%s" cannot be handled yet : %s is unimplemented.',[AName,AType]);
end;

function TDBUSProxy.GetMethodCallMessage(const AInterface, AMethodName: String
  ): TDBusMethodCallMessage;
begin
  Result:=TDBusMethodCallMessage.Create(FDestination,FObjectPath,AInterface,AMethodName);
end;

function TDBUSProxy.GetMessageReply(M: TDBUSMethodCallMessage): TDBUSMessage;
begin
  Result:=Connection.SendWithReplyAndBlock(M,FTimeout);
  If (Result=Nil) then
    Raise Exception.CreateFmt('No reply on message',[]);
end;

constructor TDBUSProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeout:=5000;
end;

end.
