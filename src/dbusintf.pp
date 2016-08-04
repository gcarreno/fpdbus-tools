{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2010 by Michael Van Canneyt, member of the
   Free Pascal development team

   DBUS standard interfaces: Introspectable, DBUS daemon and Properties

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit dbusintf;

{$mode objfpc}
{$h+}

interface

uses SysUtils, Classes, dbuscomp, dbusproxy;

Const
  Sorg_freedesktop_DBus_Introspectable_Name = 'org.freedesktop.DBus.Introspectable';
  Sorg_freedesktop_DBus_Name = 'org.freedesktop.DBus';
  Sorg_freedesktop_DBus_Properties_Name = 'org.freedesktop.DBus.Properties';

Type

  { org.freedesktop.DBus.Introspectable -> org_freedesktop_DBus_Introspectable }

  Iorg_freedesktop_DBus_Introspectable = Interface
    Function Introspect : String;
    end;

  Torg_freedesktop_DBus_Introspectable_proxy = Class(TDBUSProxy)
  Public
    Function Introspect : String;
  end;
  { org.freedesktop.DBus -> org_freedesktop_DBus } 

  Iorg_freedesktop_DBus = Interface
    Function Hello : String;
    Function RequestName (Arg1 : String;Arg2 : Cardinal) : Cardinal;
    Function ReleaseName (Arg1 : String) : Cardinal;
    Function StartServiceByName (Arg1 : String;Arg2 : Cardinal) : Cardinal;
    Procedure UpdateActivationEnvironment (Arg1 : TDBUSDictionary);
    Function NameHasOwner (Arg1 : String) : Boolean;
    Function ListNames : TStringArray;
    Function ListActivatableNames : TStringArray;
    Procedure AddMatch (Arg1 : String);
    Procedure RemoveMatch (Arg1 : String);
    Function GetNameOwner (Arg1 : String) : String;
    Function ListQueuedOwners (Arg1 : String) : TStringArray;
    Function GetConnectionUnixUser (Arg1 : String) : Cardinal;
    Function GetConnectionUnixProcessID (Arg1 : String) : Cardinal;
    Function GetAdtAuditSessionData (Arg1 : String) : TByteArray;
    Function GetConnectionSELinuxSecurityContext (Arg1 : String) : TByteArray;
    Procedure ReloadConfig;
    Function GetId : String;
    end;

  Torg_freedesktop_DBus_proxy = Class(TDBUSProxy)
  Public
    Function Hello : String;
    Function RequestName (Arg1 : String;Arg2 : Cardinal) : Cardinal;
    Function ReleaseName (Arg1 : String) : Cardinal;
    Function StartServiceByName (Arg1 : String;Arg2 : Cardinal) : Cardinal;
    Procedure UpdateActivationEnvironment (Arg1 : TDBUSDictionary);
    Function NameHasOwner (Arg1 : String) : Boolean;
    Function ListNames : TStringArray;
    Function ListActivatableNames : TStringArray;
    Procedure AddMatch (Arg1 : String);
    Procedure RemoveMatch (Arg1 : String);
    Function GetNameOwner (Arg1 : String) : String;
    Function ListQueuedOwners (Arg1 : String) : TStringArray;
    Function GetConnectionUnixUser (Arg1 : String) : Cardinal;
    Function GetConnectionUnixProcessID (Arg1 : String) : Cardinal;
    Function GetAdtAuditSessionData (Arg1 : String) : TByteArray;
    Function GetConnectionSELinuxSecurityContext (Arg1 : String) : TByteArray;
    Procedure ReloadConfig;
    Function GetId : String;
  end;

  { org.freedesktop.DBus.Properties -> org_freedesktop_DBus_Properties }

  Iorg_freedesktop_DBus_Properties = Interface
    Function Get (interface_name : String;property_name : String) : Variant;
    Procedure aSet (interface_name : String;property_name : String;value : Variant);
    Function GetAll (interface_name : String) : TDBUSDictionary;
    end;

  Torg_freedesktop_DBus_Properties_proxy = Class(TDBUSProxy, Iorg_freedesktop_DBus_Properties)
  Public
    Function Get (interface_name : String;property_name : String) : Variant;
    Procedure aSet (interface_name : String;property_name : String;value : Variant);
    Function GetAll (interface_name : String) : TDBUSDictionary;
  end;

implementation


{ org.freedesktop.DBus.Introspectable -> org_freedesktop_DBus_Introspectable } 

Function Torg_freedesktop_DBus_Introspectable_proxy.Introspect : String;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Introspectable_Name, 'Introspect');
  try
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


{ org.freedesktop.DBus -> org_freedesktop_DBus } 

Function Torg_freedesktop_DBus_proxy.Hello : String;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'Hello');
  try
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.RequestName (Arg1 : String;Arg2 : Cardinal) : Cardinal;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'RequestName');
  try
    M.AppendArgument(Arg1);
    M.AppendArgument(Arg2);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.ReleaseName (Arg1 : String) : Cardinal;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'ReleaseName');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.StartServiceByName (Arg1 : String;Arg2 : Cardinal) : Cardinal;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'StartServiceByName');
  try
    M.AppendArgument(Arg1);
    M.AppendArgument(Arg2);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Procedure Torg_freedesktop_DBus_proxy.UpdateActivationEnvironment (Arg1 : TDBUSDictionary);

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'UpdateActivationEnvironment');
  try
    UnhandledArgument('Arg1','Array');
    R:=GetMessageReply(M);
    try
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.NameHasOwner (Arg1 : String) : Boolean;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'NameHasOwner');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.ListNames : TStringArray;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'ListNames');
  try
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.ListActivatableNames : TStringArray;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'ListActivatableNames');
  try
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Procedure Torg_freedesktop_DBus_proxy.AddMatch (Arg1 : String);

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'AddMatch');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Procedure Torg_freedesktop_DBus_proxy.RemoveMatch (Arg1 : String);

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'RemoveMatch');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.GetNameOwner (Arg1 : String) : String;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'GetNameOwner');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.ListQueuedOwners (Arg1 : String) : TStringArray;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'ListQueuedOwners');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.GetConnectionUnixUser (Arg1 : String) : Cardinal;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'GetConnectionUnixUser');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.GetConnectionUnixProcessID (Arg1 : String) : Cardinal;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'GetConnectionUnixProcessID');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.GetAdtAuditSessionData (Arg1 : String) : TByteArray;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'GetAdtAuditSessionData');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.GetConnectionSELinuxSecurityContext (Arg1 : String) : TByteArray;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'GetConnectionSELinuxSecurityContext');
  try
    M.AppendArgument(Arg1);
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Procedure Torg_freedesktop_DBus_proxy.ReloadConfig;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'ReloadConfig');
  try
    R:=GetMessageReply(M);
    try
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_proxy.GetId : String;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Name, 'GetId');
  try
    R:=GetMessageReply(M);
    try
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


{ org.freedesktop.DBus.Properties -> org_freedesktop_DBus_Properties }

Function Torg_freedesktop_DBus_Properties_proxy.Get (interface_name : String;property_name : String) : Variant;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Properties_Name, 'Get');
  try
    M.AppendArgument(interface_name);
    M.AppendArgument(property_name);
    R:=GetMessageReply(M);
    try
//      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Procedure Torg_freedesktop_DBus_Properties_proxy.aSet (interface_name : String;property_name : String;value : Variant);

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Properties_Name, 'aSet');
  try
    M.AppendArgument(interface_name);
    M.AppendArgument(property_name);
    M.AppendArgument(value);
    R:=GetMessageReply(M);
    try
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


Function Torg_freedesktop_DBus_Properties_proxy.GetAll (interface_name : String) : TDBUSDictionary;

Var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Properties_Name, 'GetAll');
  try
    M.AppendArgument(interface_name);
    R:=GetMessageReply(M);
    try
      UnhandledArgument('values','Array');
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;


end.
