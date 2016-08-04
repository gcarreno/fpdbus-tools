 {
    This file is part of the Free Pascal run time library.
    Copyright (c) 2010 by Michael Van Canneyt, member of the
    Free Pascal development team

    DBUS Introspection and code generation components.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dbusintro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dom, xmlread;

Type
  TDBUSDataType = (ddtUnknown,ddtByte,ddtBoolean,ddtInt16,ddtUint16,ddtInt32,ddtUint32,
                   ddtInt64,ddtUint64,ddtDouble,ddtString,ddtObjectPath,
                   ddtSignature, ddtArray, ddtVariant,ddtStruct,ddtDict,ddFD);
  TDBUSDataTypes = Set of TDBUSDataType;
  TDBUSDictDef = Array[0..1] of TDBUSDataType;

Const
  DBUSCompoundTypes : TDBUSDataTypes = [ddtArray, ddtDict, ddtStruct];
  DBUSTypeNames : array[TDBUSDataType] of char
                = ('?','y','b','n','q','i','u',
                   'x','t','d','s','o',
                   'g','a','v','r','e','h');
  DBUSPascalTypeNames : Array[TDBUSDataType] of String
                      = ('','Byte','Boolean','Smallint','Word','Integer','Cardinal',
                         'Int64','QWord','Double','String','String',
                         'TDBUSGUID','Array','Variant','Record','TDBUSDict','THandle');

Type
  { TDBUSNamedItem }

  TDBUSNamedItem = Class(TCollectionItem)
  private
    FAnnotations: TStrings;
    FName: String;
    function GetAnnotation: TStrings;
    procedure SetAnnotation(const AValue: TStrings);
  protected
    function GetDisplayName: string; override;
  Public
    Destructor destroy; override;
    Function HasAnnotations : Boolean;
    Property Name : String Read FName Write FName;
    Property Annotations : TStrings Read GetAnnotation Write SetAnnotation;
  end;

  TDBUSStructDef = Class;

  { TDBUSStructItem }

  TDBUSStructItem = Class(TCollectionItem)
  private
    FDataType: TDBUSDataType;
    FStruct : TDBUSStructDef;
    function GetStruct: TDBUSStructDef;
    procedure SetStruct(const AValue: TDBUSStructDef);
  Public
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent);
    Property DataType : TDBUSDataType Read FDataType Write FDataType;
    Property Struct : TDBUSStructDef Read GetStruct Write SetStruct;
  end;

  { TDBUSStructDef }

  TDBUSStructDef = Class(TCollection)
  private
    FName: String;
    function GetF(AIndex : Integer): TDBUSStructItem;
    procedure SetF(AIndex : Integer; const AValue: TDBUSStructItem);
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Name : String Read FName Write FName;
    property Fields[AIndex : Integer] : TDBUSStructItem Read GetF Write SetF; default;
  end;



  { TDBUSNamedCollection }

  TDBUSNamedCollection = Class(TCollection)
  private
    function GetN(AIndex : Integer): TDBUSNamedItem;
    procedure SetN(AIndex : Integer; const AValue: TDBUSNamedItem);
  Public
    Function IndexOfName(Const AName: String) : Integer;
    Property NamedItems[AIndex : Integer] : TDBUSNamedItem Read GetN Write SetN; default;
  end;

  TDBUSVariables = Class;
  { TDBUSVarDef }

  TDBUSVarDef = Class(TDBUSNamedItem)
  private
    FDataType: TDBUSDataType;
    FDictDef: TDBUSDictDef;
    FElementType: TDBUSDataType;
    FStruct : TDBUSVariables;
    FStructType : String;
    function GetStructDef: TDBUSVariables;
    procedure SetStructDef(const AValue: TDBUSVariables);
  protected
    Property StructType : String Read FStructType;
  Public
    Procedure Assign(Source : TPersistent); override;
    Property DataType : TDBUSDataType Read FDataType Write FDataType;
    Property ElementType : TDBUSDataType Read FElementType Write FElementType;
    Property StructDef : TDBUSVariables Read GetStructDef Write SetStructDef;
    Property DictDef : TDBUSDictDef Read FDictDef Write FDictDef;
  end;

  { TDBUSVariables }

  TDBUSVariables = Class(TDBUSNamedCollection)
  private
    function GetV(AIndex : Integer): TDBUSVarDef;
    procedure SetV(AIndex : Integer; const AValue: TDBUSVarDef);
  Public
    Function AddVariable(Const AName : String) : TDBUSVarDef;
    Function FindVariableByName(Const AName : String) : TDBUSVarDef;
    Function GetVariableByName(Const AName : String) : TDBUSVarDef;
    Property Variables[AIndex : Integer] : TDBUSVarDef Read GetV Write SetV; default;
  end;

  { TDBUSPropertyDef }

  TDBUSPropertyAccess = (dpaRead,dpaWrite,dpaReadWrite);
  TDBUSPropertyDef = Class(TDBUSVarDef)
  private
    FAccess: TDBUSPropertyAccess;
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Access : TDBUSPropertyAccess Read FAccess Write FAccess;
  end;


  { TDBUSProperties }

  TDBUSProperties = Class(TDBUSNamedCollection)
  private
    function GetA(AIndex : Integer): TDBUSPropertyDef;
    procedure SetA(AIndex : Integer; const AValue: TDBUSPropertyDef);
  Public
    Function AddProperty(Const AName : String) : TDBUSPropertyDef;
    Function FindPropertyByName(Const AName : String) : TDBUSPropertyDef;
    Function GetPropertyByName(Const AName : String) : TDBUSPropertyDef;
    Property Properties[AIndex : Integer] : TDBUSPropertyDef Read GetA Write SetA; default;
  end;

  TArgumentDirection = (adnone,adIn,adOut);

  { TDBusArgumentDef }

  TDBusArgumentDef = Class(TDBUSVarDef)
  private
    FDirection: TArgumentDirection;
  Public
    procedure Assign(Source: TPersistent); override;
    Function ArgName : String;
    Property Direction : TArgumentDirection Read FDirection Write FDirection;
  end;

  { TDBUSArguments }

  TDBUSArguments = Class(TDBUSNamedCollection)
  private
    function GetA(AIndex : Integer): TDBUSArgumentDef;
    procedure SetA(AIndex : Integer; const AValue: TDBUSArgumentDef);
  Public
    Function AddArgument(Const AName : String) : TDBUSArgumentDef;
    Function FindArgumentByName(Const AName : String) : TDBUSArgumentDef;
    Function GetArgumentByName(Const AName : String) : TDBUSArgumentDef;
    Property Arguments[AIndex : Integer] : TDBUSArgumentDef Read GetA Write SetA;  default;
  end;

  { TDBUSMethodDef }

  TDBUSMethodDef = Class(TDBUSNamedItem)
  private
    FArguments: TDBUSArguments;
    procedure SetArguments(const AValue: TDBUSArguments);
  Public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    Property Arguments : TDBUSArguments Read FArguments Write SetArguments;
  end;

  { TDBUSSignalDef }

  TDBUSSignalDef = Class(TDBUSNamedItem)
  private
    FVariables: TDBUSVariables;
    procedure SetVariables(const AValue: TDBUSVariables);
  Public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    Property Variables : TDBUSVariables Read FVariables Write SetVariables;
  end;

  { TDBUSMethods }

  TDBUSMethods = Class(TDBUSNamedCollection)
  private
    function GetM(AIndex : Integer): TDBUSMethodDef;
    procedure SetM(AIndex : Integer; const AValue: TDBUSMethodDef);
  Public
    Function AddMethod(Const AName : String) : TDBUSMethodDef;
    Function FindMethodByName(Const AName : String) : TDBUSMethodDef;
    Function GetMethodByName(Const AName : String) : TDBUSMethodDef;
    Property Methods [AIndex : Integer] : TDBUSMethodDef Read GetM Write SetM; default;
  end;

  { TDBUSSignals }

  TDBUSSignals = Class(TDBUSNamedCollection)
  private
    function GetS(AIndex : Integer): TDBUSSignalDef;
    procedure SetS(AIndex : Integer; const AValue: TDBUSSignalDef);
  Public
    Function AddSignal(Const AName : String) : TDBUSSignalDef;
    Function FindSignalByName(Const AName : String) : TDBUSSignalDef;
    Function GetSignalByName(Const AName : String) : TDBUSSignalDef;
    Property Signals [AIndex : Integer] : TDBUSSignalDef Read GetS Write SetS; default;
  end;

  TDBUSInterfaces = Class;
  { TDBUSInterfaceDef }

  TDBUSInterfaceDef = Class(TDBUSNamedItem)
  private
    FMethods: TDBUSMethods;
    FProperties: TDBUSProperties;
    FSignals: TDBUSSignals;
    procedure SetMethods(const AValue: TDBUSMethods);
    procedure SetProperties(const AValue: TDBUSProperties);
    procedure SetSignals(const AValue: TDBUSSignals);
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Property Methods : TDBUSMethods Read FMethods Write SetMethods;
    Property Signals : TDBUSSignals Read FSignals Write SetSignals;
    Property Properties : TDBUSProperties Read FProperties Write SetProperties;
  end;

  { TDBUSInterfaces }

  TDBUSInterfaces = Class(TDBUSNamedCollection)
  private
    function GetI(AIndex : Integer): TDBUSinterfaceDef;
    procedure SetI(AIndex : Integer; const AValue: TDBUSinterfaceDef);
  Public
    Function AddInterface(Const AName : String) : TDBUSInterfaceDef;
    Function FindInterfaceByName(Const AName : String) : TDBUSInterfaceDef;
    Function GetInterfaceByName(Const AName : String) : TDBUSInterfaceDef;
    Property Interfaces[AIndex : Integer] : TDBUSinterfaceDef Read GetI Write SetI; default;
  end;

  { TDBUSNodeItem }
  TDBUSNodes = Class;

  TDBUSNodeItem = Class(TDBUSNamedItem)
  private
    FInterfaces: TDBUSInterfaces;
    FNodes: TDBUSNodes;
    procedure SetInterfaces(const AValue: TDBUSInterfaces);
    procedure SetNodes(const AValue: TDBUSNodes);
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure Clear;
    Property Interfaces : TDBUSInterfaces Read FInterfaces Write SetInterfaces;
    Property Nodes : TDBUSNodes Read FNodes Write SetNodes;
  end;

  { TDBUSNodes }

  TDBUSNodes =Class(TDBUSNamedCollection)
  private
    function GetN(AIndex : Integer): TDBUSNodeItem;
    procedure SetN(AIndex : Integer; const AValue: TDBUSNodeItem);
  Public
    Function AddNode(Const AName : String) : TDBUSNodeItem;
    Function FindNode(Const AName : String) : TDBUSNodeItem;
    Function GetNodeByName(Const AName : String) : TDBUSNodeItem;
    Property Nodes[AIndex : Integer] : TDBUSNodeItem Read GetN Write SetN; default;
  end;
  { TDBUSIntrospection }

  TUnknownInterfaceElementEvent = Procedure(Sender : TObject; AInterface : TDBUSInterfaceDef;AElement : TDOMElement);
  TUnknownMethodElementEvent = Procedure(Sender : TObject; AMethod : TDBUSMethodDef;AElement : TDOMElement);
  TUnknownSignalElementEvent = Procedure(Sender : TObject; ASignal : TDBUSSignalDef;AElement : TDOMElement);
  TUnknownPropertyElementEvent = Procedure(Sender : TObject; AProperty : TDBUSPropertyDef;AElement : TDOMElement);

  TDBUSIntrospection = Class(TComponent)
  private
    FOnUnknownInterfaceElement: TUnknownInterfaceElementEvent;
    FOnUnknownMethodElement: TUnknownMethodElementEvent;
    FOnUnknownPropertyElement: TUnknownPropertyElementEvent;
    FOnUnknownSignalElement: TUnknownSignalElementEvent;
    FRootNode: TDBUSNodeItem;
    FWarnings: TStrings;
    function GetInterfaces: TDBUSInterfaces;
  protected
    procedure ParseStructDef(var S: String; AVariable: TDBUSVarDef);
    procedure ParseDictDef(ADef: String; AVariable: TDBUSVarDef);
    procedure ParseVariableDef(var S: String; AVariable: TDBUSVarDef);
    procedure ParseNode(DBUSNode : TDBUSNodeItem; ANode: TDOMElement);
    procedure ParseVariable(AVariable: TDBUSVarDef;  EVariable: TDOMelement);
    procedure ParseArgument(AArgument: TDBUSArgumentDef;  EArgument: TDOMelement);
    procedure ParseInterface(Intf: TDBUSInterfaceDef; EIntf: TDOMelement);
    procedure ParseMethod(AMethod : TDBUSMethodDef; EMethod: TDOMelement);
    procedure ParseSignal(ASignal : TDBUSSignalDef; ESignal: TDOMelement);
    procedure ParseProperty(AProperty: TDBUSPropertyDef; EProperty: TDOMelement);
    Procedure Warn(Const Fmt : String; Args : Array of const);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure LoadFromFile(Const AFileName : String);
    Procedure LoadFromXML(Const XML : TXMLDocument);
    Property Interfaces : TDBUSInterfaces Read GetInterfaces;
    Property RootNode : TDBUSNodeItem Read FRootNode;
    Property OnUnknownInterfaceElement : TUnknownInterfaceElementEvent Read FOnUnknownInterfaceElement Write FOnUnknownInterfaceElement;
    Property OnUnknownMethodElement : TUnknownMethodElementEvent Read FOnUnknownMethodElement Write FOnUnknownMethodElement;
    Property OnUnknownSignalElement : TUnknownSignalElementEvent Read FOnUnknownSignalElement Write FOnUnknownSignalElement;
    Property OnUnknownPropertyElement : TUnknownPropertyElementEvent Read FOnUnknownPropertyElement Write FOnUnknownPropertyElement;
    Property Warnings : TStrings Read FWarnings;
  end;

  TDBUSCodeOption = (dcoGenerateInterface,  // Generate interface declaration
                     dcoGenerateProxy,      // Generate proxy declaration
                     dcoProxyHasInterface,  // Proxy implements interface.
                     dcoProxyUsesProtected, // Proxy methods are protected
                     dcoUseFunction,        // Use functions for methods with OUT parameter
                     dcoIncludeSystemInterfaces, // Include system interfaces
                     dcoLastPartInterfaceName); // InterfaceName is constructed from last part of DBUS name if none specified
  TDBUSCodeOptions = Set of TDBUSCodeOption;

  { TDBUSInterfaceCodeOptionItem }

  TDBUSInterfaceCodeOptionItem = Class(TDBUSNameditem)
  private
    FPascalName: String;
    FSkip: Boolean;
  public
    procedure Assign(Source : TPersistent);
  Published
    Property PascalName : String Read FPascalName Write FPascalName;
    Property Skip : Boolean Read FSkip Write FSkip;
  end;

  { TDBUSInterfaceCodeOptions }

  TDBUSInterfaceCodeOptions = Class(TDBUSNamedCollection)
  private
    function GetO(AIndex : Integer): TDBUSInterfaceCodeOptionItem;
    procedure SetO(AIndex : Integer; const AValue: TDBUSInterfaceCodeOptionItem);
  Public
    Function AddOption(AName : String) : TDBUSInterfaceCodeOptionItem;
    Property CodeOptions[AIndex : Integer] : TDBUSInterfaceCodeOptionItem Read GetO Write SetO;  default;
  end;

  { TDBUSCodeGenerator }

  TDBUSCodeGenerator = Class(TComponent)
  private
    FInterfaceOptions: TDBUSInterfaceCodeOptions;
    FOptions: TDBUSCodeOptions;
    FReserved : TStringList;
    FKeyWordPrefix: String;
    FUnitName: String;
    FCurrentIndent : Integer;
  Protected
    // Source code generation auxiliary routines
    function GetIndent : string;
    Procedure AddLine(Source : TStrings; Const ALine : String);
    Procedure AddIndentLine(Source : TStrings; Const ALine : String);
    Procedure DoIndent;
    Procedure DoUnIndent;
    procedure AddVarDecl(Source: TStrings; AName, AType: String; First: Boolean = False);
    procedure Procbegin(Source: Tstrings);
    procedure ProcEnd(Source: Tstrings);
    Procedure GetKeyWords(List : TStrings); virtual;
    // DBUS specifics
    Function CreateDeclaration(Intf : TDBUSInterfaceDef) : boolean;
    Function GetInterfaceName(Intf : TDBUSInterfaceDef) : String;
    procedure CheckDictClassDef(AIntfName: String; Method: TDBUSMethodDef; Source: TStrings);
    procedure GenerateDictClassDef(A: TDBUSVarDef; Source: TStrings);
    procedure CheckDictClassImpl(AIntfName: String; Method: TDBUSMethodDef; Source: TStrings);
    procedure GenerateDictClassImpl(A: TDBUSVarDef; Source: TStrings);
    procedure GenerateDictItemClassImpl(A: TDBUSVarDef; Source: TStrings);
    procedure SetInterfaceOptions(const AValue: TDBUSInterfaceCodeOptions);
    procedure WritePassMethodInParams(const MsgVarName: String; Args: TDBUSArguments; Source: TStrings);
    procedure WriteReadMethodOutParams(const MsgVarName: String;  Args: TDBUSArguments; Source: TStrings);
    function DBUSPropertyGetterName(AProperty: TDBUSPropertyDef): String;
    function DBUSPropertySetterName(AProperty: TDBUSPropertyDef): String;
    procedure GenerateUnitImplementation(Intf: TDBUSInterfaces; Source: TStrings );
    procedure GenerateUnitInterface(Intf: TDBUSInterfaces; Source: TStrings);
    procedure GeneratePropertyGetterProxyImplementation(const AClassName: String; AIntf: TDBUSInterfaceDef; AProp: TDBUSPropertyDef;Source: TStrings);
    procedure GeneratePropertySetterProxyImplementation(const AClassName: String; AIntf: TDBUSInterfaceDef; AProp: TDBUSPropertyDef; Source: TStrings);
    procedure GenerateProxyMethodImplementation(const AClassName: String; AIntf: TDBUSInterfaceDef; AMethod: TDBUSMethodDef; Source: TStrings);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    // Create valid pascal identifier
    Function CreateCleanIdentifier(Const AName : String) : String;
    // Create type definition
    Function DBUSTypeDef(Const AType : TDBUSVarDef) : String;
    // Create Method argument definition
    Function DBUSArgToParamDef(Const Arg : TDBUSArgumentDef; IsResult : Boolean) : String;
    // Create method declaration. No ending semicolon.
    function GenerateMethodDeclaration(const AClassname: String; AMethod: TDBUSMethodDef): String; virtual;
    // Create property getter declaration. No ending semicolon.
    function GenerateGetPropertyDeclaration(const AClassName: String; AProperty: TDBUSPropertyDef): String;
    // Create property setter declaration. No ending semicolon.
    function GenerateSetPropertyDeclaration(const AClassName: String; AProperty: TDBUSPropertyDef): String;
    // Create property declaration. No ending semicolon.
    function GeneratePropertyDeclaration(const AClassName: String; AProperty: TDBUSPropertyDef): String;
    // Add method implementation to source.
    Procedure GenerateMethodDefinition(Const AClassname : String; AMethod : TDBUSMethodDef; Source : TStrings);
    // Add Interface definition to source.
    Procedure GenerateInterfaceDefinition(Intf : TDBUSInterfaceDef; Source : TStrings);
    // Add proxy definition to source.
    Procedure GenerateProxyDefinition(Intf : TDBUSInterfaceDef; Source : TStrings);
    // Add proxy implementation to source.
    procedure GenerateProxyImplementation(AIntf: TDBUSInterfaceDef; Source: TStrings);
    // Generate complete unit.
    Procedure GenerateUnit(Intf : TDBUSInterfaces; Source : TStrings);
    // Generate complete unit and save to file.
    Procedure SaveUnitToFile(Intf : TDBUSInterfaces; Const AFileName : String);
  Published
    // Prefix to use when keywords are encountered as names.
    Property KeyWordPrefix : String Read FKeyWordPrefix Write FKeyWordPrefix;
    // Options controlling the output.
    Property Options : TDBUSCodeOptions Read FOptions Write FOptions;
    // Unit name for unit generation.
    Property UnitName : String Read FUnitName Write FUnitName;
    // List of interfaces to treat. If non-empty, a declaration is generated only for those that are listed.
    // If Skip=true for one of these in here, then it is also not generated
    Property InterfaceOptions : TDBUSInterfaceCodeOptions Read FInterfaceOptions Write SetInterfaceOptions;
  end;

  EDBUSIntrospect = Class(Exception);

Function CharToDBUSDataType(C : Char) : TDBUSDataType;

Const
  DefaultCodeOptions = [dcogenerateInterface,dcogenerateProxy,dcoUseFunction];

implementation

Resourcestring

  SErrUnknown = 'Unknown %s : %s';
  SArgument   = 'argument';
  SMethod     = 'method';
  SSignal     = 'signal';
  SInterface  = 'interface';
  SProperty   = 'property';
  SVariable   = 'variable';
  SNode       = 'node';

  SErrNoRootNode = 'Root node "node" not found.';
  SWarnUnknownElement = 'Unknown element "%s" in definition of %s "%s". Ignoring';
  SWarnUnknownTypeSignature = 'Unknown type signature : %s';
  SWarnWrongDictType = 'Wrong dict type definition : %s';

Function CharToDBUSDataType(C : Char) : TDBUSDataType;

begin
  Result:=High(TDBUSDataType);
  While (Result>ddtUnknown) and (DBUSTypeNames[Result]<>C) do
    Result:=Pred(Result);
end;

Procedure DBUSError(Msg : String; Args : Array of const);

begin
  Raise EDBUSIntrospect.CreateFmt(Msg,Args);
end;

{ TDBUSNamedCollection }

function TDBUSNamedCollection.GetN(AIndex : Integer): TDBUSNamedItem;
begin
  Result:=TDBUSNamedItem(Items[AIndex]);
end;

procedure TDBUSNamedCollection.SetN(AIndex : Integer;
  const AValue: TDBUSNamedItem);
begin
  Items[AIndex]:=AValue;
end;

function TDBUSNamedCollection.IndexOfName(const AName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(AName,GetN(Result).Name)<>0) do
    Dec(Result);
end;

{ TDBUSArguments }

function TDBUSArguments.GetA(AIndex : Integer): TDBUSArgumentDef;
begin
  Result:=TDBUSArgumentDef(Items[AIndex]);
end;

procedure TDBUSArguments.SetA(AIndex : Integer; const AValue: TDBUSArgumentDef);
begin
  Items[AIndex]:=AValue;
end;

function TDBUSArguments.AddArgument(const AName: String): TDBUSArgumentDef;
begin
  Result:=Add as TDBUSArgumentDef;
  Result.FName:=AName;
end;

function TDBUSArguments.FindArgumentByName(const AName: String
  ): TDBUSArgumentDef;

Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If I=-1 then
    Result:=Nil
  else
    Result:=GetA(I);
end;

function TDBUSArguments.GetArgumentByName(const AName: String
  ): TDBUSArgumentDef;
begin
  Result:=FindArgumentByName(AName);
  If (result=Nil) then
   DBUSError(SErrUnknown,[SArgument,AName]);
end;

{ TDBusArgumentDef }

procedure TDBusArgumentDef.Assign(Source: TPersistent);
Var
  A : TDBusArgumentDef;
begin
  inherited Assign(Source);
  If (Source is TDBusArgumentDef) then
    begin
    A:=Source as TDBusArgumentDef;
    FDirection:=A.Direction;
    FStructType:=A.StructType;
    If A.HasAnnotations then
      Annotations.Assign(A.Annotations);
    end
end;

function TDBusArgumentDef.ArgName: String;
begin
  Result:=Name;
  If Result='' then
    Result:='Arg'+IntToStr(Index+1);
end;

{ TDBUSArgumentsDef }

procedure TDBUSMethodDef.SetArguments(const AValue: TDBUSArguments);
begin
  if FArguments=AValue then exit;
  FArguments.Assign(AValue);
end;

constructor TDBUSMethodDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FArguments:=TDBUSArguments.Create(TDBUSArgumentDef);
end;

destructor TDBUSMethodDef.Destroy;
begin
  FreeAndNil(FArguments);
  inherited Destroy;
end;

procedure TDBUSMethodDef.Assign(Source: TPersistent);

Var
   M : TDBUSMethodDef;

begin
  if (Source is TDBUSMethodDef) then
    begin
    M:=Source as TDBUSMethodDef;
    FName:=M.Name;
    If M.HasAnnotations then
      Annotations:=M.Annotations;
    FArguments.Assign(M.Arguments);
    end
  else
    inherited Assign(Source);
end;

{ TDBUSMethods }

function TDBUSMethods.GetM(AIndex : Integer): TDBUSMethodDef;
begin
  Result:=TDBUSMethodDef(Items[Aindex]);
end;

procedure TDBUSMethods.SetM(AIndex : Integer; const AValue: TDBUSMethodDef);
begin
  Items[AIndex]:=AValue;
end;

function TDBUSMethods.AddMethod(const AName: String): TDBUSMethodDef;
begin
  Result:=add as TDBUSMethodDef;
  Result.FName:=AName;
end;

function TDBUSMethods.FindMethodByName(const AName: String): TDBUSMethodDef;

Var
  I : integer;

begin
  I:=IndexOfName(Aname);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetM(I);
end;

function TDBUSMethods.GetMethodByName(const AName: String): TDBUSMethodDef;
begin
  Result:=FindMethodByName(AName);
  If (Result=Nil) then
    DBUSError(SErrUnknown,[SMethod,AName]);
end;

{ TDBUSSignals }

function TDBUSSignals.GetS(AIndex: Integer): TDBUSSignalDef;
begin
  Result:=TDBUSSignalDef(Items[AIndex]);
end;

procedure TDBUSSignals.Sets(AIndex: Integer; const AValue: TDBUSSignalDef);
begin
  Items[AIndex]:=AValue;
end;

function TDBUSSignals.AddSignal(const AName: String): TDBUSSignalDef;
begin
  Result:=Add as TDBUSSignalDef;
  Result.FName:=AName;
end;

function TDBUSSignals.FindSignalByName(const AName: String): TDBUSSignalDef;

Var
  I : integer;

begin
  I:=IndexOfName(AName);
  If I=-1 then
    Result:=Nil
  else
    Result:=GetS(I);
end;

function TDBUSSignals.GetSignalByName(const AName: String): TDBUSSignalDef;
begin
  Result:=FindSignalByname(AName);
  If (Result=Nil) then
    DBUSError(SErrUnknown,[SSignal,AName]);
end;

{ TDBUSInterfaceDef }

procedure TDBUSInterfaceDef.SetMethods(const AValue: TDBUSMethods);
begin
  if FMethods=AValue then exit;
  FMethods.Assign(AValue);
end;

procedure TDBUSInterfaceDef.SetProperties(const AValue: TDBUSProperties);
begin
  if FProperties=AValue then exit;
  FProperties.Assign(AValue);
end;

procedure TDBUSInterfaceDef.SetSignals(const AValue: TDBUSSignals);
begin
  if FSignals=AValue then exit;
  FSignals.Assign(AValue);
end;

constructor TDBUSInterfaceDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMethods:=TDBUSMethods.Create(TDBUSMethodDef);
  FSignals:=TDBUSSignals.Create(TDBUSSignalDef);
  FProperties:=TDBUSProperties.Create(TDBUSPropertyDef);
end;

destructor TDBUSInterfaceDef.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FSignals);
  FreeAndNil(FMethods);
  inherited Destroy;
end;

{ TDBUSInterfaces }

function TDBUSInterfaces.GetI(AIndex : Integer): TDBUSinterfaceDef;
begin
  Result:=TDBUSinterfaceDef(Items[AIndex]);
end;

procedure TDBUSInterfaces.SetI(AIndex : Integer; const AValue: TDBUSinterfaceDef
  );
begin
  Items[AIndex]:=AValue;
end;

function TDBUSInterfaces.AddInterface(const AName: String): TDBUSInterfaceDef;
begin
  Result:=Add as TDBUSInterfaceDef;
  Result.FName:=AName;
end;

function TDBUSInterfaces.FindInterfaceByName(const AName: String
  ): TDBUSInterfaceDef;
Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetI(I);
end;

function TDBUSInterfaces.GetInterfaceByName(const AName: String
  ): TDBUSInterfaceDef;
begin
  Result:=FindInterfaceByName(AName);
  If Result=Nil then
    DBUSError(SErrUnknown,[SInterface,AName])

end;

{ TDBUSIntrospection }

constructor TDBUSIntrospection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWarnings:=TStringList.Create;
  FRootNode:=TDBUSNodeItem.Create(Nil);
end;

destructor TDBUSIntrospection.Destroy;
begin
  FreeAndNil(FRootNode);
  FreeAndNil(FWarnings);
  inherited Destroy;
end;

procedure TDBUSIntrospection.LoadFromFile(const AFileName: String);

Var
  XML : TXMLDocument;

begin
  ReadXMLFile(XML,AFileName);
  try
    LoadFromXML(XML);
  finally
    XML.Free
  end;
end;

function TDBUSIntrospection.GetInterfaces: TDBUSInterfaces;
begin
  Result:=FRootNode.Interfaces;
end;

Procedure TDBUSIntrospection.ParseStructDef(Var S : String; AVariable: TDBUSVarDef);

Var
  it : TDBUSVarDef;

begin
  Repeat
    IT:=AVariable.Structdef.AddVariable('field'+IntToStr(AVariable.Structdef.Count+1));
    ParseVariableDef(S,it);
    Delete(S,1,1);
  Until S[1]=')';
  Delete(S,1,1);
end;

Procedure TDBUSIntrospection.ParseVariableDef(Var S : String; AVariable: TDBUSVarDef);

Var
  C : char;

begin
  If (Length(S)=0) then
    AVariable.DataType:=ddtUnknown
  else
    begin
    AVariable.DataType:=CharToDBUSDataType(S[1]);
    If (AVariable.DataType=ddtArray) and (Length(S)>1) then
      AVariable.ElementType:=CharToDBUSDataType(S[2]);
    end;
  if (AVariable.DataType=ddtUnknown)
     or ((AVariable.DataType=ddtArray) and (AVariable.ElementType=ddtUnknown)) then
    begin
    if (AVariable.DataType=ddtArray) then
      begin
      C:=S[2];
      Delete(S,1,2);
      end
    else
      begin
      C:=S[1];
      Delete(S,1,1);
      end;
    Case C of
      '(' : begin
            if (AVariable.DataType=ddtUnknown) then
              AVariable.DataType:=ddtStruct
            else
              AVariable.ElementType:=ddtStruct;
            ParseStructDef(S,AVariable);
            end;
      '{' : begin
            if (AVariable.DataType=ddtUnknown) then
              AVariable.DataType:=ddtDict
            else
              AVariable.ElementType:=ddtDict;
            ParseDictDef(S,AVariable);
            end;
    else
      Warn(SWarnUnknownTypeSignature,[C]);
    end;
    end;
end;

Procedure TDBUSIntrospection.ParseDictDef(ADef : String; AVariable: TDBUSVarDef);

Var
  C : Char;
  DD : TDBUSDictDef;

begin
  If (Length(ADef)<>3) or (ADef[3]<>'}') then
    Warn(SWarnWrongDictType,[ADef]);
  DD[0]:=CharToDBUSDataType(ADef[1]);
  DD[1]:=CharToDBUSDataType(ADef[2]);
  AVariable.DictDef:=DD;
  if (AVariable.DictDef[1]=ddtUnknown) then
    begin
    C:=ADef[1];
    Delete(ADef,1,1);
    if (C='(') then
      ParseStructDef(ADef,AVariable);
    end;
end;

procedure TDBUSIntrospection.ParseVariable(AVariable: TDBUSVarDef;
  EVariable: TDOMelement);

Var
  C : Char;
  S : String;

begin
  AVariable.Name:=EVariable['name'];
  AVariable.ElementType:=ddtUnknown;
  S:=EVariable['type'];
  ParseVariableDef(S,AVariable);
end;

procedure TDBUSIntrospection.ParseArgument(AArgument: TDBUSArgumentDef; EArgument: TDOMelement);

Var
  S : String;

begin
  ParseVariable(AArgument,EArgument);
  S:=EArgument['direction'];
  if s='in' then
    AArgument.Direction:=adIn
  else if s='out' then
    AArgument.Direction:=adOut
  else
    AArgument.Direction:=adNone;
end;

procedure TDBUSIntrospection.ParseInterface(Intf: TDBUSInterfaceDef; EIntf : TDOMelement);

Var
  N : TDOMNode;
  E : TDOMElement;
  M : TDBUSMethodDef;
  S : TDBUSSignalDef;
  P : TDBUSPropertyDef;

begin
  N:=EIntf.FirstChild;
  While (N<>Nil) do
    begin
    If N.NodeType=ELEMENT_NODE then
      begin
      E:=TDOMElement(N);
      If (E.NodeName='method') then
        begin
        M:=Intf.Methods.AddMethod(E['name']);
        ParseMethod(M,E);
        end
      else if (E.NodeName='signal') then
        begin
        S:=Intf.Signals.AddSignal(E['name']);
        ParseSignal(S,E);
        end
      else if (E.NodeName='property') then
        begin
        P:=Intf.Properties.AddProperty(E['name']);
        ParseProperty(P,E);
        end
      else if (E.NodeName='annotation') then
        Intf.Annotations.Add(E['name']+'='+E['value'])
      else
        If Assigned(FOnUnknownInterfaceElement) then
          FOnUnknownInterfaceElement(Self,Intf,E)
        else
          Warn(SWarnUnknownElement,[E.NodeName,SInterface,Intf.Name])
      end;
    N:=N.NextSibling;
    end;
end;

procedure TDBUSIntrospection.ParseMethod(AMethod: TDBUSMethodDef;
  EMethod: TDOMelement);

Var
  E : TDOMElement;
  N : TDOMNode;
  A : TDBUSArgumentDef;

begin
  N:=EMethod.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeType=ELEMENT_NODE) then
      begin
      E:=TDOMElement(N);
      if (E.NodeName='arg') then
        begin
        A:=AMethod.Arguments.AddArgument(E['name']);
        ParseArgument(A,E);
        end
      else if (E.NodeName='annotation') then
        AMethod.Annotations.Add(E['name']+'='+E['value'])
      else
        If Assigned(FOnUnknownMethodElement) then
          FOnUnknownMethodElement(Self,AMethod,E)
        else
          Warn(SWarnUnknownElement,[E.NodeName,SMethod,AMethod.Name])
      end;
    N:=N.NextSibling;
    end;
end;

procedure TDBUSIntrospection.ParseProperty(AProperty: TDBUSPropertyDef;
  EProperty: TDOMelement);

Var
  E : TDOMElement;
  N : TDOMNode;
  A : TDBUSArgumentDef;
  S : String;
begin
  AProperty.ElementType:=ddtUnknown;
  S:=EProperty['type'];
  ParseVariableDef(S,AProperty);
  N:=EProperty.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeType=ELEMENT_NODE) then
      begin
      E:=TDOMElement(N);
      if (E.NodeName='annotation') then
        AProperty.Annotations.Add(E['name']+'='+E['value'])
      else
        If Assigned(FOnUnknownPropertyElement) then
          FOnUnknownPropertyElement(Self,AProperty,E)
        else
          Warn(SWarnUnknownElement,[E.NodeName,SProperty,AProperty.Name])
      end;
    N:=N.NextSibling;
    end;
end;

procedure TDBUSIntrospection.ParseSignal(ASignal: TDBUSSignalDef;
  ESignal: TDOMelement);
Var
  E : TDOMElement;
  N : TDOMNode;
  V : TDBUSVarDef;

begin
  N:=ESignal.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeType=ELEMENT_NODE) then
      begin
      E:=TDOMElement(N);
      if (E.NodeName='arg') then
        begin
        V:=ASignal.Variables.Addvariable(E['name']);
        ParseVariable(V,E);
        end
      else if (E.NodeName='annotation') then
        ASignal.Annotations.Add(E['name']+'='+E['value'])
      else
        If Assigned(FOnUnknownSignalElement) then
          FOnUnknownSignalElement(Self,ASignal,E)
        else
          Warn(SWarnUnknownElement,[E.NodeName,SSignal,ASignal.Name])
      end;
    N:=N.NextSibling;
    end;
end;


procedure TDBUSIntrospection.Warn(const Fmt: String; Args: array of const);
begin
  FWarnings.Add(Format(Fmt,Args));
end;

procedure TDBUSIntrospection.ParseNode(DBUSNode : TDBUSNodeItem; ANode : TDOMElement);

Var
  N : TDOMNode;
  I : TDBUSInterfaceDef;
  No : TDBUSNOdeItem;
  E : TDOMElement;

begin
  N:=ANode.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeType=ELEMENT_NODE) and (TDomElement(N).NodeName='interface') then
      begin
      E:=TDOMElement(N);
      ParseInterface(DBUSNode.Interfaces.AddInterface(E['name']),E);
      end
    else If (N.NodeType=ELEMENT_NODE) and (TDomElement(N).NodeName='node') then
      begin
      E:=TDOMElement(N);
      No:=DBUSNode.Nodes.AddNode(E['name']);
      ParseNode(No,E);
      end;
    N:=N.NextSibling;
    end;
end;

procedure TDBUSIntrospection.LoadFromXML(const XML: TXMLDocument);

Var
  E : TDOMElement;

begin
  FRootNode.Clear;
  FWarnings.Clear;
  E:=XML.DocumentElement;
  If (E.NodeName<>'node') then
    DBUSError(SErrNoRootNode,[]);
  ParseNode(FRootNode,E);
end;


{ TDBUSVarDef }

function TDBUSVarDef.GetStructDef: TDBUSVariables;
begin
  If not Assigned(FStruct) then
    FStruct:=TDBUSVariables.Create(TDBUSVarDef);
  Result:=FStruct;
end;

procedure TDBUSVarDef.SetStructDef(const AValue: TDBUSVariables);
begin
  StructDef.Assign(Avalue);
end;

procedure TDBUSVarDef.Assign(Source: TPersistent);

Var
  V : TDBUSVarDef;

begin
  if (Source is TDBUSVarDef) then
    begin
    v:=Source as TDBUSVarDef;
    FName:=V.Name;
    FDataType:=V.DataType;
    FElementType:=V.ElementType;
    FDictDef:=V.FDictDef;
    FStructType:=V.StructType;
    If Assigned(V.FStruct) then
      StructDef:=V.FStruct;
    end
  else
    inherited Assign(Source);
end;

{ TDBUSPropertyDef }

procedure TDBUSPropertyDef.Assign(Source: TPersistent);

Var
  P : TDBUSPropertyDef;

begin
  inherited Assign(Source);
  if (Source is TDBUSPropertyDef) then
    begin
    P:=Source as TDBUSPropertyDef;
    FName:=P.Name;
    If P.HasAnnotations then
     Annotations:=P.Annotations;
    FAccess:=P.Access;
    end;
end;

{ TDBUSProperties }

function TDBUSProperties.GetA(AIndex: Integer): TDBUSPropertyDef;
begin
  Result:=TDBUSPropertyDef(Items[AIndex]);
end;

procedure TDBUSProperties.SetA(AIndex: Integer; const AValue: TDBUSPropertyDef);
begin
  Items[AIndex]:=AValue;
end;

function TDBUSProperties.AddProperty(const AName: String): TDBUSPropertyDef;
begin
  Result:=Add as TDBUSPropertyDef;
  Result.Name:=AName;
end;

function TDBUSProperties.FindPropertyByName(const AName: String): TDBUSPropertyDef;

Var
  I : integer;

begin
  I:=IndexOfName(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetA(I);
end;

function TDBUSProperties.GetPropertyByName(const AName: String
  ): TDBUSPropertyDef;
begin
  Result:=FindPropertyByName(Aname);
  If (Result=Nil) then
    DBUSError(SerrUnknown,[SProperty,AName]);
end;

{ TDBUSVariables }

function TDBUSVariables.GetV(AIndex: Integer): TDBUSVarDef;
begin
  Result:=TDBUSVarDef(Items[AIndex]);
end;

procedure TDBUSVariables.SetV(AIndex: Integer; const AValue: TDBUSVarDef);
begin
  Items[AIndex]:=AValue;
end;

function TDBUSVariables.AddVariable(const AName: String): TDBUSVarDef;
begin
  Result:=Add as TDBUSVarDef;
  Result.FName:=Aname;
end;

function TDBUSVariables.FindVariableByName(const AName: String): TDBUSVarDef;

Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetV(I);
end;

function TDBUSVariables.GetVariableByName(const AName: String): TDBUSVarDef;


begin
  Result:=FindVariableByName(AName);
  If (Result=Nil) then
    DBUSError(SErrUnknown,[SVariable,AName])
end;

{ TDBUSSignalDef }

procedure TDBUSSignalDef.SetVariables(const AValue: TDBUSVariables);
begin
  Fvariables.Assign(AValue);
end;

constructor TDBUSSignalDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVariables:=TDBUSVariables.Create(TDBUSVarDef);
end;

destructor TDBUSSignalDef.Destroy;
begin
  FreeAndNil(FVariables);
  inherited Destroy;
end;

procedure TDBUSSignalDef.Assign(Source: TPersistent);
Var
  S : TDBUSSignalDef;
begin
  inherited Assign(Source);
  If (source is TDBUSSignalDef) then
    begin
    S:=Source as TDBUSSignalDef;
    FVariables.Assign(S.Variables);
    If S.Hasannotations then
      Annotations:=S.Annotations;
    end;
end;

{ ---------------------------------------------------------------------
  TDBUSCodeGenerator
  ---------------------------------------------------------------------}

function TDBUSCodeGenerator.GetIndent: string;
begin
  Result:=StringOFchar(' ',FCurrentIndent)
end;

procedure TDBUSCodeGenerator.AddLine(Source: TStrings; const ALine: String);
begin
  If (ALine<>'') then
    Source.Add(GetIndent+ALine)
  else
    Source.Add('');
end;

procedure TDBUSCodeGenerator.AddIndentLine(Source: TStrings; const ALine: String
  );

begin
  DoIndent;
  AddLine(Source,Aline);
  DoUnindent;
end;

procedure TDBUSCodeGenerator.DoIndent;
begin
  FCurrentIndent:=FCurrentIndent+2;
end;

procedure TDBUSCodeGenerator.DoUnIndent;
begin
  If FCurrentIndent>1 then
    FCurrentIndent:=FCurrentIndent-2;
end;

procedure TDBUSCodeGenerator.GetKeyWords(List: TStrings);


Const
  KeyCount  = 78;
  Keys :  Array[1..KeyCount] of String = ('absolute', 'and', 'array',
      'asm', 'begin', 'case', 'const','constructor','destructor','div','do',
      'downto','else','end','file','for','function','goto','if','implementation',
      'in','inherited','inline','interface','label','mod','nil','not','object',
      'of','on','operator','or','packed','procedure','program','record',
      'reintroduce', 'repeat','self','set','shl','shr','string','then','to',
      'type','unit','until','uses','var','while','with','xor','dispose','exit',
      'false','new','true','as','class','dispinterface','except','exports',
      'finalization','finally','initialization','inline','is','library','on',
      'out','packed','property','raise','resourcestring','threadvar','try');

Var
  I : Integer;

begin
  For I:=1 to KeyCount do
    List.add(Keys[i]);
end;

constructor TDBUSCodeGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReserved:=TStringList.Create;
  GetKeyWords(FReserved);
  FReserved.Sorted:=True;
  FkeywordPrefix:='a';
  Foptions:=DefaultCodeOptions;
  FInterfaceoptions:=TDBUSInterfaceCodeOptions.Create(TDBUSInterfaceCodeOptionItem);
end;

destructor TDBUSCodeGenerator.Destroy;
begin
  FreeAndNil(Freserved);
  FreeAndNil(FInterfaceoptions);
  inherited Destroy;
end;

function TDBUSCodeGenerator.CreateCleanIdentifier(const AName: String): String;

Const
  AllowedChars = ['a'..'z','A'..'Z','0'..'9','_'];

Var
  I : Integer;

begin
  Result:=AName;
  For I:=1 to Length(Result) do
    If Not (Result[i] in AllowedChars) then
      Result[i]:='_';
  If (Length(Result)>0) and (Result[1] in ['0'..'9']) then
    Result:='_'+Result;
  While (FReserved.IndexOf(Result)<>-1) do
    Result:=FKeyWordPrefix+Result;
end;

function TDBUSCodeGenerator.DBUSTypeDef(const AType: TDBUSVarDef): String;

Var
  I : Integer;

begin
  If (AType.StructType<>'') then
    Result:=AType.StructType
  else if (AType.DataType<>ddtArray) then
    Result:=DBUSPascalTypeNames[AType.DataType]
  else
    Case AType.ElementType of
      ddtDict :
        begin
        Result:='TDBUSDictionary';
        end;
      ddtStruct :
        begin
        Result:='Array of Record ';
        For I:=0 to AType.StructDef.Count-1 do
          begin
          Result:=Result+AType.StructDef[I].Name+' : '+DBUSTypeDef(AType.StructDef[I])+'; ';
          end;
        Result:=Result+'End';
        end;
    else
      Result:='T'+DBUSPascalTypeNames[AType.ElementType]+'Array';
    end;
end;

function TDBUSCodeGenerator.DBUSArgToParamDef(const Arg: TDBUSArgumentDef;
  IsResult: Boolean): String;
begin
  If not IsResult then
    Result:=CreateCleanIdentifier(Arg.ArgName);
  Result:=Result+' : '+DBUSTypeDef(Arg);
end;

Function TDBUSCodeGenerator.GenerateMethodDeclaration(Const AClassname : String; AMethod: TDBUSMethodDef) : String;

Const
  Prefixes : Array[Boolean] of string = ('Procedure','Function');

Var
  I,La : Integer;
  IsF : Boolean;

begin
  Isf:=dcoUseFunction in Options;
  LA:=AMethod.Arguments.Count-1;
  If IsF then
    isf:=(La>=0) and (AMethod.Arguments[LA].Direction=adOut);
  For I:=0 to AMethod.Arguments.Count-1-Ord(Isf) do
    begin
    If (Result<>'') then
      Result:=Result+';';
    Result:=Result+DBUSArgToParamDef(AMethod.Arguments[i],False);
    end;
  If (Result<>'') then
    Result:=' ('+Result+')';
  Result:=CreateCleanIdentifier(AMethod.Name)+Result;
  if IsF then
    Result:=Result+DBUSArgToParamDef(AMethod.Arguments[LA],true);
  If (AClassName<>'') then
    Result:=AClassName+'.'+Result;
  Result:=Prefixes[Isf]+' '+Result;
end;

Procedure TDBUSCodeGenerator.GenerateMethodDefinition(Const AClassname : String; AMethod: TDBUSMethodDef;
  Source: TStrings);
begin
  AddLine(Source,GenerateMethodDeclaration(AClassName,AMethod)+';');
end;

Function TDBUSCodeGenerator.DBUSPropertyGetterName(AProperty : TDBUSPropertyDef) : String;

begin
  Result:='DBUSPropGet'+CreateCleanIdentifier(AProperty.Name);
end;

Function TDBUSCodeGenerator.DBUSPropertySetterName(AProperty : TDBUSPropertyDef) : String;

begin
  Result:='DBUSPropSet'+CreateCleanIdentifier(AProperty.Name);
end;

Function TDBUSCodeGenerator.GeneratePropertyDeclaration(Const AClassName : String; AProperty : TDBUSPropertyDef) : String;

begin
  Result:='property '+CreateCleanIdentifier(AProperty.Name)+' : ';
  Result:=Result+DBUSTypeDef(AProperty);
  If (Aproperty.Access in [dpaRead,dpaReadWrite]) then
    Result:=Result+' read '+DBUSPropertyGetterName(AProperty);
  If (Aproperty.Access in [dpaWrite,dpaReadWrite]) then
    Result:=Result+' read '+DBUSPropertySetterName(AProperty);
end;

Function TDBUSCodeGenerator.GenerateGetPropertyDeclaration(Const AClassName : String; AProperty : TDBUSPropertyDef) : String;

begin
  Result:=DBUSPropertyGetterName(AProperty);
  If (AClassName<>'') then
    Result:=AClassName+'.'+Result;
  Result:='Function '+Result+' : '+DBUSTypeDef(AProperty);
end;

Function TDBUSCodeGenerator.GenerateSetPropertyDeclaration(Const AClassName : String; AProperty : TDBUSPropertyDef) : String;

begin
  Result:=DBUSPropertyGetterName(AProperty);
  If (AClassName<>'') then
    Result:=AClassName+'.'+Result;
  Result:='Procedure '+Result+'(AValue : '+DBUSTypeDef(AProperty)+')';
end;

procedure TDBUSCodeGenerator.GenerateDictClassDef(A : TDBUSVarDef; Source : TStrings);

Var
  S,DIN,NKT,NVT : String;

begin
  // Item class
  AddLine(Source,'');
  DIN:=A.StructType+'Item';
  AddLine(Source,Din+' = Class(TDBUSDictItem)');
  AddLine(Source,'Private');
  DoIndent;
  NKT:=DBUSPascalTypeNames[A.DictDef[0]];
  NVT:=DBUSPascalTypeNames[A.DictDef[1]];
  AddLine(Source,'FKey : '+NKT+';');
  // Only simple or record types for the moment.
  AddLine(Source,'FValue : '+NVT+';');
  DoUnindent;
  AddLine(Source,'Public');
  DoIndent;
  AddLine(Source,'Procedure Assign (Source: TPersistent); override;');
  AddLine(Source,'Procedure Load(I : TDBUSMessageIterator); override;');
  AddLine(Source,'Procedure Save(I : TDBUSMessageIterator); override;');
  AddLine(Source,'Property Key : '+NKT+' Read FKey Write FKey;');
  AddLine(Source,'Property Value : '+NVT+' Read FValue Write FValue;');
  DoUnindent;
  AddLine(Source,'end;');
  AddLine(Source,'');
  AddLine(Source,A.StructType+' = Class(TDBUSDictionary)');
  AddLine(Source,'Private');
  DoIndent;
  AddLine(Source,'Function GetE(AIndex : Integer) : '+DIN+';');
  AddLine(Source,'Procedure SetE(AIndex : Integer; AValue : '+DIN+');');
  DoUnIndent;
  AddLine(Source,'Public');
  DoIndent;
  AddLine(Source,'Constructor CreateDict;');
  AddLine(Source,'Function AddElement : '+Din+';');
  AddLine(Source,'Function IndexOfKey(AKey : '+NKT+') : Integer;');
  AddLine(Source,'Function FindElement(AKey : '+NKT+') : '+Din+';');
  AddLine(Source,'Function ValueByKey(AKey : '+NKT+') : '+NVT+';');
  AddLine(Source,'Property Elements[AIndex : Integer] : '+DIN+' Read GetE Write SetE;Default;');
  DoUnIndent;
  AddLine(Source,'end;');
  AddLine(Source,'');
end;

procedure TDBUSCodeGenerator.CheckDictClassImpl(AIntfName: String;
  Method: TDBUSMethodDef; Source: TStrings);
Var
  I : integer;
  A : TDBUSargumentDef;

begin
  For I:=0 to Method.Arguments.Count-1 do
    begin
    A:=Method.Arguments[i];
    If (A.DataType=ddtArray) and (A.ElementType=ddtDict) then
      begin
      GenerateDictItemClassImpl(A,Source);
      GenerateDictClassImpl(A,Source);
      end;
    end;
end;

procedure TDBUSCodeGenerator.AddVarDecl(Source : TStrings; AName,AType : String; First : Boolean = False);

begin
  if First then
    begin
    AddLine(Source,'');
    AddLine(Source,'var');
    end;
  DoIndent;
  AddLine(Source,AName+' : '+AType+';');
  DoUnIndent;
end;

procedure TDBUSCodeGenerator.Procbegin(Source : Tstrings);

begin
  AddLine(Source,'');
  AddLine(Source,'begin');
  DoIndent;
end;
procedure TDBUSCodeGenerator.ProcEnd(Source : Tstrings);

begin
  DoUnIndent;
  AddLine(Source,'end;');
  AddLine(Source,'');
  AddLine(Source,'');
end;

procedure TDBUSCodeGenerator.GenerateDictItemClassImpl(A: TDBUSVarDef;
  Source: TStrings);

Var
  Din : String;

begin
  DIN:=A.StructType+'Item';
  AddLine(Source,'Procedure '+Din+'.Assign (Source: TPersistent);');
  AddVarDecl(Source,'I',Din,true);
  ProcBegin(Source);
  AddLine(Source,'if (Source is '+Din+') then');
  DoIndent;
  AddLine(Source,'begin');
  AddLine(Source,'I:=Source as '+Din+';');
  AddLine(Source,'Fkey:=I.Key;');
  AddLine(Source,'FValue:=I.Value;');
  AddLine(Source,'end');
  DoUnIndent;
  AddLine(Source,'else');
  DoIndent;
  AddLine(Source,'Inherited;');
  DoUnIndent;
  ProcEnd(Source);
  AddLine(Source,'Procedure '+Din+'.Load(I : TDBUSMessageIterator);');
  Procbegin(Source);
  AddLine(Source,'I.GetArgument(FKey);');
  AddLine(Source,'I.GetArgument(FValue);');
  ProcEnd(Source);
  AddLine(Source,'Procedure '+Din+'.Save(I : TDBUSMessageIterator);');
  Procbegin(Source);
  AddLine(Source,'I.AppendArgument(FKey);');
  AddLine(Source,'I.AppendArgument(FValue);');
  ProcEnd(Source);
end;

procedure TDBUSCodeGenerator.GenerateDictClassImpl(A: TDBUSVarDef;Source: TStrings);

Var
  Din,Nvt,NKT : String;

begin
  Din:=A.StructType+'Item';
  NKT:=DBUSPascalTypeNames[A.DictDef[0]];
  NVT:=DBUSPascalTypeNames[A.DictDef[1]];
  AddLine(Source,'Constructor '+A.StructType+'.CreateDict;');
  procBegin(Source);
  AddLine(Source,'inherited Create('+Din+');');
  procend(Source);
  AddLine(Source,'Function '+A.StructType+'.GetE(AIndex : Integer) : '+DIN+';');
  ProcBegin(Source);
  AddLine(Source,'Result:=Items[AIndex] as '+Din+';');
  ProcEnd(Source);
  AddLine(Source,'Procedure '+A.StructType+'.SetE(AIndex : Integer; AValue : '+DIN+');');
  ProcBegin(Source);
  AddLine(Source,'Items[AIndex]:=AValue;');
  ProcEnd(Source);
  AddLine(Source,'Function '+A.StructType+'.AddElement : '+Din+';');
  ProcBegin(Source);
  AddLine(Source,'Result:=Add as '+DIn+';');
  ProcEnd(Source);
  AddLine(Source,'Function '+A.StructType+'.IndexOfKey(AKey : '+NKT+') : Integer;');
  ProcBegin(Source);
  AddLine(Source,'Result:=Count-1;');
  AddLine(Source,'While (Result>=0) and (GetE(Result).Key<>AKey) do');
  DoIndent;
  AddLine(Source,'Dec(Result);');
  DoUnIndent;
  ProcEnd(Source);
  AddLine(Source,'Function '+A.StructType+'.FindElement(AKey : '+NKT+') : '+Din+';');
  AddVarDecl(Source,'I','Integer',True);
  ProcBegin(Source);
  AddLine(Source,'I:=IndexOfKey(AKey);');
  AddLine(Source,'If (I=-1) then');
  DoIndent;
  AddLine(Source,'Result:=Nil');
  DoUnIndent;
  AddLine(Source,'else');
  DoIndent;
  AddLine(Source,'Result:=GetE(I);');
  DoUnIndent;
  ProcEnd(Source);
  AddLine(Source,'Function '+A.StructType+'.ValueByKey(AKey : '+NKT+') : '+NVT+';');
  AddVarDecl(Source,'I','Integer',True);
  ProcBegin(Source);
  AddLine(Source,'I:=IndexOfKey(AKey);');
  AddLine(Source,'If (I=-1) then');
  DoIndent;
  AddLine(Source,'RaiseDBUSError(ClassName,''No value found for key'')');
  DoUnIndent;
  AddLine(Source,'else');
  DoIndent;
  AddLine(Source,'Result:=GetE(I).Value;');
  DoUnIndent;
  ProcEnd(Source);
end;


procedure TDBUSCodeGenerator.CheckDictClassDef(AIntfName : String; Method : TDBUSMethodDef; Source : TStrings);

Var
  I : integer;
  A : TDBUSargumentDef;
  S : String;

begin
  For I:=0 to Method.Arguments.Count-1 do
    begin
    A:=Method.Arguments[i];
    If (A.DataType=ddtArray) and (A.ElementType=ddtDict) then
      begin
      If (A.Direction=adout) and (I=Method.Arguments.Count-1) then
        S:='Result'
      else
        S:=A.ArgName;
      A.FStructType:='T'+AIntfName+Method.Name+S;
      GenerateDictClassDef(A,Source);
      end;
    end;
end;

procedure TDBUSCodeGenerator.GenerateInterfaceDefinition(
  Intf: TDBUSInterfaceDef; Source: TStrings);

Var
  S : String;
  I : Integer;

begin
  For I:=0 to Intf.Methods.Count-1 do
   CheckDictClassDef(GetInterfaceName(Intf),Intf.Methods[i],Source);
  S:='I'+GetInterfaceName(Intf)+' = Interface';
  AddLine(Source,S);
  DoIndent;
  For I:=0 to Intf.Methods.Count-1 do
    AddLine(Source,GenerateMethodDeclaration('',Intf.Methods[i])+';');
  For I:=0 to Intf.Properties.Count-1 do
    begin
    If (Intf.Properties[i].Access in [dpaRead,dpaReadWrite]) then
      AddLine(Source,GenerateGetPropertyDeclaration('',Intf.Properties[i])+';');
    If (Intf.Properties[i].Access in [dpaWrite,dpaReadWrite]) then
      AddLine(Source,GenerateSetPropertyDeclaration('',Intf.Properties[i])+';');
    end;
  For I:=0 to Intf.Properties.Count-1 do
    AddLine(Source,GeneratePropertyDeclaration('',Intf.Properties[i])+';');
  DoUnindent;
  AddLine(Source,'end;');
end;

procedure TDBUSCodeGenerator.GenerateProxyDefinition(Intf: TDBUSInterfaceDef;
  Source: TStrings);

Var
  S,NI : String;
  I : Integer;

begin
  NI:=GetInterfaceName(Intf);
  S:='T'+NI+'Proxy = Class(TDBUSProxy';
  If dcoProxyHasInterface in Options then
    S:=S+', I'+NI;
  S:=S+')';
  AddLine(Source,S);
  If Intf.Properties.Count>0 then
    begin
    AddLine(Source,'Protected');
    DoIndent;
    For I:=0 to Intf.Properties.Count-1 do
      begin
      If (Intf.Properties[i].Access in [dpaRead,dpaReadWrite]) then
        AddLine(Source,GenerateGetPropertyDeclaration('',Intf.Properties[i])+';');
      If (Intf.Properties[i].Access in [dpaWrite,dpaReadWrite]) then
        AddLine(Source,GenerateSetPropertyDeclaration('',Intf.Properties[i])+';');
      end;
    DoUnIndent;
    end;
  // Need protected methods ?
  If (dcoProxyHasInterface in Options) and (dcoProxyUsesProtected in Options) then
    begin
    // Not yet in protected section ?
    If (Intf.Properties.Count=0) then
      AddLine(Source,'Protected');
    end
  else
    AddLine(Source,'Public');
  DoIndent;
  For I:=0 to Intf.Methods.Count-1 do
    AddLine(Source,GenerateMethodDeclaration('',Intf.Methods[i])+';');
  For I:=0 to Intf.Properties.Count-1 do
    AddLine(Source,GeneratePropertyDeclaration('',Intf.Properties[i])+';');
  DoUnIndent;
  AddLine(Source,'end;');
end;

procedure TDBUSCodeGenerator.GenerateUnit(Intf: TDBUSInterfaces;
  Source: TStrings);
begin
  FCurrentIndent:=0;
  AddLine(Source,'unit '+Unitname+';');
  AddLine(Source,'');
  AddLine(Source,'{$mode objfpc}');
  AddLine(Source,'{$h+}');
  AddLine(Source,'');
  AddLine(Source,'interface');
  AddLine(Source,'');
  AddLine(Source,'uses SysUtils, Classes, dbuscomp, dbusproxy;');
  AddLine(Source,'');
  GenerateUnitInterface(Intf,Source);
  AddLine(Source,'');
  AddLine(Source,'implementation');
  AddLine(Source,'');
  GenerateUnitImplementation(Intf,Source);
  AddLine(Source,'');
  AddLine(Source,'end.');
end;

procedure TDBUSCodeGenerator.SaveUnitToFile(Intf: TDBUSInterfaces;
  const AFileName: String);

Var
  Src : TStrings;

begin
  Src:=TStringList.Create;
  try
    GenerateUnit(intf,Src);
    Src.SaveToFile(AFileName);
  finally
    Src.Free;
  end;
end;

procedure TDBUSCodeGenerator.GenerateUnitInterface(Intf: TDBUSInterfaces; Source: TStrings);

Var
  I : Integer;

begin
  AddLine(Source,'Const');
  DoIndent;
  For I:=0 to Intf.Count-1 do
    if CreateDeclaration(Intf[i]) then
      AddLine(Source,'S'+GetInterfaceName(Intf[i])+'Name = '''+Intf[i].Name+''';');
  DoUnIndent;
  AddLine(Source,'');
  AddLine(Source,'Type');
  AddLine(Source,'');
  DoIndent;
  For I:=0 to Intf.Count-1 do
    if CreateDeclaration(Intf[i]) then
      begin
      AddLine(Source,'{ '+intf[i].Name+' -> '+CreateCleanIdentifier(intf[i].Name)+' } ');
      If DCOGenerateinterface in options then
        begin
        AddLine(Source,'');
        GenerateInterfaceDefinition(Intf[i],Source);
        end;
      If DCOGenerateProxy in options then
        begin
        AddLine(Source,'');
        GenerateProxyDefinition(Intf[i],Source);
        end;
      end;
  DoUnIndent;
end;

procedure TDBUSCodeGenerator.GenerateUnitImplementation(Intf: TDBUSInterfaces; Source: TStrings);

Var
  I,J : Integer;
  AIntf : TDBUSInterfaceDef;

begin
  For I:=0 to Intf.Count-1 do
    begin
    AIntf:=Intf[i];
    if CreateDeclaration(AIntf) then
      begin
      For J:=0 to AIntf.Methods.Count-1 do
       CheckDictClassImpl(GetInterfaceName(AIntf),AIntf.Methods[J],Source);
      end;
    end;
  if dcoGenerateProxy in Options then
    begin
    AddLine(Source,'');
    For I:=0 to Intf.Count-1 do
      if CreateDeclaration(Intf[i]) then
        begin
        AIntf:=Intf[i];
        AddLine(Source,'{ '+Aintf.Name+' -> '+GetInterfaceName(Aintf)+' } ');
        AddLine(Source,'');
        GenerateProxyImplementation(AIntf,Source);
        end;
      end;
end;

Procedure TDBUSCodeGenerator.GenerateProxyImplementation(AIntf : TDBUSInterfaceDef; Source : TStrings);

Var
  I : integer;
  CN : String;

begin
 CN:='T'+GetInterfaceName(AIntf)+'Proxy';
  For I:=0 to AIntf.Properties.Count-1 do
    begin
    GeneratePropertyGetterProxyImplementation(CN,AIntf,AIntf.Properties[i],Source);
    AddLine(Source,'');
    AddLine(Source,'');
    GeneratePropertySetterProxyImplementation(CN,AIntf,AIntf.Properties[i],Source);
    AddLine(Source,'');
    AddLine(Source,'');
    end;
  For I:=0 to AIntf.Methods.Count-1 do
    begin
    GenerateProxyMethodImplementation(CN,AIntf,AIntf.Methods[i],Source);
    AddLine(Source,'');
    AddLine(Source,'');
    end;
end;


Procedure TDBUSCodeGenerator.GeneratePropertyGetterProxyImplementation(Const AClassName : String;
   AIntf : TDBUSInterfaceDef;
   AProp : TDBUSPropertyDef;
   Source : TStrings);

begin
  AddLine(Source,GenerateGetPropertyDeclaration(AClassName,AProp)+';');
  AddLine(Source,'');
  AddLine(Source,'begin');
  AddLine(Source,'end;');
end;

Procedure TDBUSCodeGenerator.GeneratePropertySetterProxyImplementation(Const AClassName : String;
   AIntf : TDBUSInterfaceDef;
   AProp : TDBUSPropertyDef;
   Source : TStrings);

begin
  AddLine(Source,GenerateSetPropertyDeclaration(AClassName,AProp)+';');
  AddLine(Source,'');
  AddLine(Source,'begin');
  AddLine(Source,'end;');
end;

procedure TDBUSCodeGenerator.WritePassMethodInParams(Const MsgVarName : String; Args : TDBUSArguments; Source : TStrings);

Var
  I : Integer;
  A : TDBUSArgumentDef;

begin
  For I:=0 to Args.Count-1 do
    begin
    A:=Args[i];
    If (A.Direction=adIn) then
      begin
      If ((A.DataType in [ddtStruct,ddtDict])
         or ((A.DataType=ddtArray) and (A.ElementType in [ddtDict,ddtStruct])))
         then
           AddLine(Source,'UnhandledArgument('''+A.ArgName+''','''+DBUSPascalTypeNames[A.DataType]+''');')
      else
         AddLine(Source,MsgVarName+'.AppendArgument('+A.ArgName+');');
      end;
    end;
end;

procedure TDBUSCodeGenerator.SetInterfaceOptions(
  const AValue: TDBUSInterfaceCodeOptions);
begin
  if FInterfaceOptions=AValue then exit;
  FInterfaceOptions.Assign(AValue);
end;

procedure TDBUSCodeGenerator.WriteReadMethodOutParams(Const MsgVarName : String; Args : TDBUSArguments; Source : TStrings);

Var
  I,La : Integer;
  IsF,ISDD : Boolean;
  A : TDBUSArgumentDef;
  S : String;

begin
  Isf:=dcoUseFunction in Options;
  LA:=Args.Count-1;
  If IsF then
    isf:=(La>=0) and (Args[LA].Direction=adOut);
  For I:=0 to Args.Count-1 do
    begin
    A:=Args[i];
    IsDD:=(A.DataType=ddtArray) and (A.ElementType=ddtDict);
    If (A.Direction=adOut) then
      begin
      If ((A.DataType=ddtStruct) or ((A.DataType=ddtArray) and (A.ElementType=ddtStruct)))
         then
           AddLine(Source,'UnhandledArgument('''+A.ArgName+''','''+DBUSPascalTypeNames[A.DataType]+''');')
      else
        begin
        if Isf and (I=La) then
          S:='Result'
        else
          S:=A.ArgName;
        If isDD then
          AddLine(Source,S+':='+A.StructType+'.CreateDict;');
        AddLine(Source,MsgVarName+'.GetArgument('+S+');');
        end;
      end;
    end;
end;

function TDBUSCodeGenerator.CreateDeclaration(Intf: TDBUSInterfaceDef): boolean;

Const
  SysIntf1 = 'org.freedesktop.DBus';
  SysIntf2 = 'org.freedesktop.DBus.Introspectable';
  SysIntf3 = 'org.freedesktop.DBus.Properties';

Var
  N : String;
  i : Integer;

begin
  n:=Intf.Name;
  If (N=Sysintf1) or (N=SysIntf2) or (N=SysIntf3) then
   result:=dcoIncludeSystemInterfaces in Options
  else
    begin
    Result:=(FInterfaceoptions.Count=0);
    If Not Result then
      begin
      I:=FInterfaceoptions.IndexOfName(N);
      Result:=I<>-1;
      If Result then
        Result:=Not FInterfaceoptions[I].Skip;
      end;
    end;
end;

function TDBUSCodeGenerator.GetInterfaceName(Intf: TDBUSInterfaceDef): String;

Var
  I : Integer;

begin
  Result:=Intf.Name;
  I:=InterfaceOptions.IndexOfName(Result);
  If (I=-1) or (InterfaceOptions[i].PascalName='') then
    begin
    if (dcoLastPartInterfaceName in Options) and (pos('.',Result)<>0) then
      begin
      Result:=ExtractFileExt(Result);
      Delete(Result,1,1);
      end;
    end
  else
    Result:=InterfaceOptions[i].PascalName;
  Result:=CreateCleanIdentifier(Result);
end;

procedure TDBUSCodeGenerator.GenerateProxyMethodImplementation(
  const AClassName: String; AIntf: TDBUSInterfaceDef; AMethod: TDBUSMethodDef;
  Source: TStrings);

var
  S : string;
  Ok : Boolean;
  I : integer;


begin
  //   M:=TDBUSMethodCallMessage.Create('org.freedesktop.DBus','/','org.freedesktop.DBus','ListNames');
  AddLine(Source,GenerateMethodDeclaration(AClassName,AMethod)+';');
  AddVarDecl(Source,'M','TDBUSMethodCallMessage',True);
  AddVarDecl(Source,'R','TDBUSMessage');
  Ok:=False;
  For I:=0 to AMethod.Arguments.Count-1 do
    if (AMethod.Arguments[i].DataType=ddtArray) and (AMethod.Arguments[i].ElementType=ddtDict) then
      Ok:=true;
  if Ok Then
    AddVarDecl(Source,'I','TDBUSMessageIterator');
  AddLine(Source,'begin');
  DoIndent;
  S:='S'+GetInterfaceName(AIntf)+'Name, '''+CreateCleanIdentifier(AMethod.Name)+'''';
  AddLine(Source,'M:=GetMethodCallMessage('+S+');');
  AddLine(Source,'try');
  DoIndent;
  WritePassMethodInParams('M',AMethod.Arguments,Source);
  AddLine(Source,'R:=GetMessageReply(M);');
  AddLine(Source,'try');
  DoIndent;
  WriteReadMethodOutParams('R',AMethod.Arguments,Source);
  DoUnindent;
  AddLine(Source,'finally');
  AddIndentLine(Source,'R.Free;');
  AddLine(Source,'end;');
  DoUnindent;
  AddLine(Source,'finally');
  AddIndentLine(Source,'M.Free;');
  AddLine(Source,'end;');
  DoUnindent;
  AddLine(Source,'end;');
end;

{ TDBUSStructDef }

function TDBUSStructDef.GetF(AIndex : Integer): TDBUSStructItem;
begin
  Result:=TDBUSStructItem(Items[AIndex]);
end;

procedure TDBUSStructDef.SetF(AIndex : Integer; const AValue: TDBUSStructItem);
begin
  Items[AIndex]:=Avalue;
end;

procedure TDBUSStructDef.Assign(Source: TPersistent);

Var
  S : TDBUSStructDef;

begin
  If (Source is TDBUSStructDef) then
    begin
    S:=TDBUSStructDef(Source);
    FName:=S.Name;
    end;
  inherited Assign(Source);
end;

{ TDBUSStructItem }

function TDBUSStructItem.GetStruct: TDBUSStructDef;
begin
  If not Assigned(FStruct) then
    FStruct:=TDBUSStructDef.Create(TDBUSStructItem);
  Result:=FStruct;
end;

procedure TDBUSStructItem.SetStruct(const AValue: TDBUSStructDef);
begin
  If not Assigned(FStruct) then
    FStruct:=TDBUSStructDef.Create(TDBUSStructItem);
  FStruct.Assign(AValue);
end;

destructor TDBUSStructItem.Destroy;
begin
  FreeAndNil(FStruct);
  inherited Destroy;
end;

procedure TDBUSStructItem.Assign(Source: TPersistent);

Var
  S : TDBUSStructItem;


begin
  If (Source is TDBUSStructItem) then
    begin
    S:=TDBUSStructItem(Source);
    FDataType:=S.DataType;
    If Assigned(S.FStruct) then
      Struct:=S.FStruct;
    end
  else
    Inherited;
end;

{ TDBUSNamedItem }

function TDBUSNamedItem.GetAnnotation: TStrings;
begin
  If FAnnotations=Nil then
    FAnnotations:=TstringList.Create;
  Result:=FAnnotations;
end;

procedure TDBUSNamedItem.SetAnnotation(const AValue: TStrings);
begin
  Annotations.Assign(AValue);
end;

function TDBUSNamedItem.GetDisplayName: string;
begin
  If (FName<>'') then
    Result:=FName
  else
    Result:=inherited GetDisplayName;
end;

destructor TDBUSNamedItem.destroy;
begin
  FreeAndNil(FAnnotations);
  inherited destroy;
end;

function TDBUSNamedItem.HasAnnotations: Boolean;
begin
  Result:=Assigned(FAnnotations);
end;

{ TDBUSInterfaceCodeOptionItem }

procedure TDBUSInterfaceCodeOptionItem.Assign(Source: TPersistent);

Var
  O : TDBUSInterfaceCodeOptionItem;

begin
  If Source is TDBUSInterfaceCodeOptionItem then
    begin
    O:=TDBUSInterfaceCodeOptionItem(Source);
    FName:=O.Name;
    If O.HasAnnotations then
      FAnnotations:=O.Annotations;
    FPascalName:=O.Pascalname;
    FSkip:=O.Skip;
    end
  else
    Inherited;
end;

{ TDBUSInterfaceCodeOptions }

function TDBUSInterfaceCodeOptions.GetO(AIndex : Integer
  ): TDBUSInterfaceCodeOptionItem;
begin
  Result:=Items[AIndex] as TDBUSInterfaceCodeOptionItem
end;

procedure TDBUSInterfaceCodeOptions.SetO(AIndex : Integer;
  const AValue: TDBUSInterfaceCodeOptionItem);
begin
  Items[AIndex]:=Avalue;
end;

function TDBUSInterfaceCodeOptions.AddOption(AName: String
  ): TDBUSInterfaceCodeOptionItem;
begin
  Result:=Add as TDBUSInterfaceCodeOptionItem;
  Result.FName:=Aname;
end;


{ TDBUSNodes }

function TDBUSNodes.GetN(AIndex : Integer): TDBUSNodeItem;
begin
  Result:=Items[Aindex] as TDBUSNodeItem;
end;

procedure TDBUSNodes.SetN(AIndex : Integer; const AValue: TDBUSNodeItem);
begin
  Items[Aindex]:=AValue;
end;

function TDBUSNodes.AddNode(const AName: String): TDBUSNodeItem;
begin
  Result:=Add as TDBUSNodeItem;
  Result.Fname:=AName;
end;

function TDBUSNodes.FindNode(const AName: String): TDBUSNodeItem;

Var
  I : Integer;

begin
  I:=IndexOfName(Aname);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetN(I)
end;

function TDBUSNodes.GetNodeByName(const AName: String): TDBUSNodeItem;
begin
  Result:=FindNode(Aname);
  If Result=Nil then
    DBUSError(SErrUnknown,[SNode,AName]);
end;

{ TDBUSNodeItem }

procedure TDBUSNodeItem.SetInterfaces(const AValue: TDBUSInterfaces);
begin
  If (FInterfaces=AValue) then
     Exit;
  FInterfaces.Assign(AValue);
end;

procedure TDBUSNodeItem.SetNodes(const AValue: TDBUSNodes);
begin
  if FNodes=AValue then exit;
  FNodes:=AValue;
end;

constructor TDBUSNodeItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FNodes:=TDBUSNodes.Create(TDBUSNodeItem);
  FInterFaces:=TDBUSInterfaces.Create(TDBUSInterfaceDef);
end;

destructor TDBUSNodeItem.Destroy;
begin
  FreeAndNil(FNodes);
  FreeAndNil(FInterFaces);
  inherited Destroy;
end;

procedure TDBUSNodeItem.Clear;
begin
  FNodes.Clear;
  FInterfaces.Clear;
end;

end.

