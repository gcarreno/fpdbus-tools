unit dbusprop;

{$mode objfpc}
{$h+}

interface

uses SysUtils, Classes, dbuscomp, dbusproxy;

Const
  Sorg_freedesktop_DBus_Properties_Name = 'org.freedesktop.DBus.Properties';

Type

  { org.freedesktop.DBus.Properties -> org_freedesktop_DBus_Properties } 


  TPropertiesGetAllResultItem = Class(TDBUSDictItem)
  Private
    FKey : String;
    FValue : Variant;
  Public
    Procedure Assign (Source: TPersistent); override;
    Procedure Load(I : TDBUSMessageIterator); override;
    Procedure Save(I : TDBUSMessageIterator); override;
    Property Key : String Read FKey Write FKey;
    Property Value : Variant Read FValue Write FValue;
  end;

  TPropertiesGetAllResult = Class(TDBUSDictionary)
  Private
    Function GetE(AIndex : Integer) : TPropertiesGetAllResultItem;
    Procedure SetE(AIndex : Integer; AValue : TPropertiesGetAllResultItem);
  Public
    Constructor CreateDict;
    Function AddElement : TPropertiesGetAllResultItem;
    Function IndexOfKey(AKey : String) : Integer;
    Function FindElement(AKey : String) : TPropertiesGetAllResultItem;
    Function ValueByKey(AKey : String) : Variant;
    Property Elements[AIndex : Integer] : TPropertiesGetAllResultItem Read GetE Write SetE;Default;
  end;

  IProperties = Interface
    Function Get (interface_name : String;property_name : String) : Variant;
    Procedure aSet (interface_name : String;property_name : String;value : Variant);
    Function GetAll (interface_name : String) : TPropertiesGetAllResult;
  end;

  TProperties_proxy = Class(TDBUSProxy)
  Public
    Function Get (interface_name : String;property_name : String) : Variant;
    Procedure aSet (interface_name : String;property_name : String;value : Variant);
    Function GetAll (interface_name : String) : TPropertiesGetAllResult;
  end;
  { org.freedesktop.DBus.Introspectable -> org_freedesktop_DBus_Introspectable } 


implementation

Procedure TPropertiesGetAllResultItem.Assign (Source: TPersistent);

var
  I : TPropertiesGetAllResultItem;

begin
  if (Source is TPropertiesGetAllResultItem) then
    begin
    I:=Source as TPropertiesGetAllResultItem;
    Fkey:=I.Key;
    FValue:=I.Value;
    end
  else
    Inherited;
end;


Procedure TPropertiesGetAllResultItem.Load(I : TDBUSMessageIterator);

begin
    I.GetArgument(FKey);
    I.GetArgument(FValue);
end;


Procedure TPropertiesGetAllResultItem.Save(I : TDBUSMessageIterator);

begin
    I.AppendArgument(FKey);
    I.AppendArgument(FValue);
end;


Constructor TPropertiesGetAllResult.CreateDict;

begin
  inherited Create(TPropertiesGetAllResultItem);
end;


Function TPropertiesGetAllResult.GetE(AIndex : Integer) : TPropertiesGetAllResultItem;

begin
  Result:=Items[AIndex] as TPropertiesGetAllResultItem;
end;


Procedure TPropertiesGetAllResult.SetE(AIndex : Integer; AValue : TPropertiesGetAllResultItem);

begin
  Items[AIndex]:=AValue;
end;


Function TPropertiesGetAllResult.AddElement : TPropertiesGetAllResultItem;

begin
  Result:=Add as TPropertiesGetAllResultItem;
end;


Function TPropertiesGetAllResult.IndexOfKey(AKey : String) : Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (GetE(Result).Key<>AKey) do
    Dec(Result);
end;


Function TPropertiesGetAllResult.FindElement(AKey : String) : TPropertiesGetAllResultItem;

var
  I : Integer;

begin
  I:=IndexOfKey(AKey);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetE(I);
end;


Function TPropertiesGetAllResult.ValueByKey(AKey : String) : Variant;

var
  I : Integer;

begin
  I:=IndexOfKey(AKey);
  If (I=-1) then
    RaiseDBUSError(ClassName,'No value found for key')
  else
    Result:=GetE(I).Value;
end;



{ org.freedesktop.DBus.Properties -> Properties } 

Function TProperties_proxy.Get (interface_name : String;property_name : String) : Variant;

var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Properties_Name, 'Get');
  try
    M.AppendArgument(interface_name);
    M.AppendArgument(property_name);
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


Procedure TProperties_proxy.aSet (interface_name : String;property_name : String;value : Variant);

var
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


Function TProperties_proxy.GetAll (interface_name : String) : TPropertiesGetAllResult;

var
  M : TDBUSMethodCallMessage;
  R : TDBUSMessage;
  I : TDBUSMessageIterator;
begin
  M:=GetMethodCallMessage(Sorg_freedesktop_DBus_Properties_Name, 'GetAll');
  try
    M.AppendArgument(interface_name);
    R:=GetMessageReply(M);
    try
      Result:=TPropertiesGetAllResult.CreateDict;
      R.GetArgument(Result);
    finally
      R.Free;
    end;
  finally
    M.Free;
  end;
end;



end.
