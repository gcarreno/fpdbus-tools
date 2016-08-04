{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2010 by Michael Van Canneyt, member of the
   Free Pascal development team

   DBUS Introspection viewer and code generation.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, StdCtrls, ComCtrls, dbuscomp, dbusintro;

type

  { TMainForm }

  TMainForm = class(TForm)
    AGetprop: TAction;
    AGenCode: TAction;
    AQuit: TAction;
    ASession: TAction;
    ASystem: TAction;
    ActionList1: TActionList;
    ILDBUS: TImageList;
    Label1: TLabel;
    LServiceInterfaces: TLabel;
    LBServices: TListBox;
    MainMenu1: TMainMenu;
    MDBUS: TMenuItem;
    MenuItem2: TMenuItem;
    MIGetprop: TMenuItem;
    MICode: TMenuItem;
    MLog: TMemo;
    MenuItem1: TMenuItem;
    MIQuit: TMenuItem;
    MISession: TMenuItem;
    MISystem: TMenuItem;
    Panel1: TPanel;
    PNames: TPanel;
    Panel2: TPanel;
    PMTree: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TVDBUS: TTreeView;
    procedure AGenCodeExecute(Sender: TObject);
    procedure AGetpropExecute(Sender: TObject);
    procedure AGetpropUpdate(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure ASessionExecute(Sender: TObject);
    procedure ASystemExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBServicesClick(Sender: TObject);
    procedure TVDBUSDblClick(Sender: TObject);
  private
    { private declarations }
    FConn : TDBUSConnection;
    FServiceName : String;
    procedure DisplayAnnotation(ANode: TTreeNode; AItem: TDBUSNamedItem);
    procedure DisplayDBUSNode(DBUSNode: TDBUSNodeItem; AParent: TTreeNode);
    procedure DisplayNames;
    procedure DisplayServiceInfo(const AServiceName, AObjectPath: String; AParent : TTreeNode);
    procedure DisplayInterfaces(Intf: TDBUSInterfaces; AParent : TTreeNode);
    procedure DisplayInterface(ANode: TTreeNode; Intf: TDBUSInterfaceDef);
    procedure DisplayMethod(ANode: TTreeNode; AMethod: TDBUSMethodDef);
    procedure DisplayProperty(ANode: TTreeNode; AProperty: TDBUSPropertyDef);
    procedure DisplaySignal(ANode: TTreeNode; ASignal: TDBUSSignalDef);
    function ExtractPropertyName(ANode: TTreeNode): string;
    function FindPropertiesInterFace(ANode: TTreeNode): TTreeNode;
    procedure GenerateCodeForIntrospection(AIntro: TDBUSIntroSpection;  AInterfaceName,AUnitName: String);
    function GetInterfaceNode(ANode: TTreeNode): TTreeNode;
    function GetIntrospection(const AServiceName, AObjectPath: String ): TDBUSIntrospection;
    function GetNodeNode(ANode: TTreeNode): TTreeNode;
    function GetNodeWithType(ANode: TTreeNode; AType: Integer): TTreeNode;
    procedure ShowPropertyValue(ANode : TTreeNode);
    function ObjectpathFromNode(N: TTreeNode): String;
    procedure ShowError(E: Exception);
  public
    { public declarations }
    Procedure ConnectToBus(AKind : TConnectionKind);
  end; 

var
  MainForm: TMainForm;

implementation

uses dbusintf, dom, xmlread, frmcodeoptions, dbusprop;

{$R *.lfm}

Const
  SystemDest = 'org.freedesktop.DBus';

  II_Interfaces = 0;
  II_Interface  = 1;
  II_Node       = 2;
  II_Method     = 3;
  II_Property   = 4;
  II_Signal     = 5;
  II_ArgIn      = 6;
  II_ArgOut     = 7;
  II_Annotation = 8;

ResourceString
  SServiceInterfaces = 'Interfaces for service "%s"';

{ TMainForm }

procedure TMainForm.ASystemExecute(Sender: TObject);
begin
  ConnectToBus(ckSystem);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ConnectToBus(ckSession);
end;

procedure TMainForm.LBServicesClick(Sender: TObject);
begin
  With LBServices do
    if ItemIndex>=0 then
      begin
      TVDBUS.Items.Clear;
      DisplayServiceInfo(Items[ItemIndex],'/',Nil);
      end;
end;

procedure TMainForm.TVDBUSDblClick(Sender: TObject);

Var
  N,N2 : TTreeNode;
  OP : String;

begin
  N:=TVDBUS.Selected;
  If (N=Nil) then
      exit;
  If (N.ImageIndex=II_NODE) and (N.Count=0) then
    begin
    OP:=ObjectPathFromNode(N);
    DisplayServiceInfo(FServiceName,OP,N);
    end;
end;

procedure TMainForm.DisplayNames;

Var
  P : Torg_freedesktop_DBus_proxy;
  NS : TStringArray;
  I : Integer;

begin
  LBServices.Items.beginUpdate;
  try
    LBServices.Items.Clear;
    P:=Torg_freedesktop_DBus_proxy.Create(Self);
    try
      P.Connection:=FConn;
      P.Destination:=SystemDest;
      P.ObjectPath:='/';
      NS:=P.ListNames;
      For I:=0 to Length(NS)-1 do
        LBServices.Items.Add(NS[i]);
      I:=LBServices.Items.IndexOf(SystemDest);
      If (I<>-1) then
        begin
        LBServices.ItemIndex:=I;
        TVDBUS.Items.Clear;
        DisplayServiceInfo(SystemDest,'/',Nil);
        end;
      SetLength(NS,0);
    finally
      P.Free;
    end;
  finally
    LBServices.Items.EndUpdate;
  end;
end;

Function TMainForm.GetIntrospection(Const AServiceName, AObjectPath : String) : TDBUSIntrospection;

Var
  T : TStringStream;
  XML : TXMLDocument;
  P : TOrg_freedesktop_DBus_Introspectable_proxy;
  S : String;
  I : Integer;

begin
  P:=Torg_freedesktop_DBus_Introspectable_proxy.create(Self);
  try
    P.Connection:=FConn;
    P.ObjectPath:=AObjectPath;
    P.Destination:=AServiceName;
    S:=P.Introspect;
  finally
    P.Free;
  end;
  T:=TStringStream.Create(S);
  try
    ReadXMLFile(XML,T);
    try
      Result:=TDBUSIntrospection.Create(Self);
      try
        Result.LoadFromXML(Xml);
        For I:=0 to Result.Warnings.Count-1 do
          MLog.Lines.add(Result.Warnings[i]);
      except
        FreeAndNil(Result);
        Raise;
      end;
    finally
      XML.Free;
    end;
  finally
    T.free;
  end;
end;

Function TMainForm.ExtractPropertyName(ANode : TTreeNode) : string;

Var
  P : integer;

begin
  Result:=ANode.Text;
  P:=Pos(': ',Result);
  Delete(Result,1,P+1);
  P:=Pos(' ',Result);
  Result:=Trim(Copy(Result,1,P-1));
end;

Function TMainForm.GetNodeWithType(ANode : TTreeNode; AType : Integer) : TTreeNode;

begin
  Result:=ANode;
  While (Result<>Nil) and (Result.ImageIndex<>AType) do
    Result:=Result.Parent;
end;

Function TMainForm.GetNodeNode(ANode : TTreeNode) : TTreeNode;

begin
  Result:=GetNodeWithType(ANode,II_NODE);
end;

Function TMainForm.GetInterfaceNode(ANode : TTreeNode) : TTreeNode;

begin
  Result:=GetNodeWithType(ANode,II_INTERFACE);
end;

Function TMainForm.FindPropertiesInterFace(ANode : TTreeNode) : TTreeNode;

begin
  If Assigned(Anode) then
    Result:=ANode.GetFirstChild
  else if TVDBUS.Items.Count>0 then
    Result:=TVDBUS.Items[0]
  else
    Result:=nil;
  While (Result<>Nil) and ((Result.ImageIndex<>II_Interface) or (Result.Text<>Sorg_freedesktop_DBus_Properties_Name)) do
    Result:=Result.GetNextSibling;
end;

procedure TMainForm.ShowPropertyValue(ANode: TTreeNode);

Var
  N,NI,NO : TTreeNode;
  P : TProperties_proxy;
  V : Variant;
  intfN,S,OP : String;

begin
  Ni:=GetInterFaceNode(ANode);
  If Assigned(Ni) then
    No:=GetNodeNode(Ni.Parent)
  else
    begin
    ShowMessage('No interface node found.');
    Exit;
    end;
  N:=FindPropertiesInterFace(No);
  If not Assigned(N) then
    begin
    ShowMessage('No properties interface found.');
    Exit;
    end;
  OP:=ObjectPathFromNode(No);
  P:=TProperties_Proxy.Create(Self);
  try
    P.Connection:=FConn;
    P.ObjectPath:=OP;
    P.Destination:=FServiceName;
    S:=ExtractPropertyName(ANode);
    intfn:=Ni.Text;
    V:=P.Get(Intfn,S);
    try
      OP:=V;
    except
      OP:='Variant value cannot be displayed';
    end;
    Mlog.Lines.Add('Property '+S+' = '+OP);
  finally
    P.Free;
  end;
end;

procedure TMainForm.DisplayServiceInfo(Const AServiceName, AObjectPath : String; AParent : TTreeNode);

Var
  Intro : TDBUSIntrospection;

begin
  LServiceInterfaces.Caption:=Format(SServiceInterfaces,[AServiceName]);
  FServiceName:=AServiceName;
  try
    Intro:=GetIntroSpection(AServiceName,AObjectPath);
    try
      DisplayDBUSNode(Intro.RootNode,AParent);
    finally
      Intro.Free;
    end;
  except
    On E : Exception do
      ShowError(E);
  end;
end;

Procedure TMainForm.DisplayDBUSNode(DBUSNode : TDBUSNodeItem;AParent : TTreeNode);

Var
  N : TTreeNode;
  I : integer;

begin
  With TVDBUS do
    begin
    Items.BeginUpdate;
    try
      If AParent=Nil then
        Items.Clear;
      DisplayInterfaces(DBUSNode.Interfaces,AParent);
      For I:=0 to DBUSNode.Nodes.Count-1 do
        begin
        N:=TVDBUS.Items.AddChild(AParent,DBUSNode.Nodes[i].Name+'/');
        N.ImageIndex:=II_Node;
        N.SelectedIndex:=II_Node;
        DisplayDBUSNode(DBUSNode.Nodes[i],N);
        end;
    finally
      Items.EndUpdate;
    end;
    end;
end;

procedure TMainForm.ShowError(E : Exception);

begin
  MLog.Lines.Add('Error : '+E.Message);
end;

procedure TMainForm.ConnectToBus(AKind: TConnectionKind);

Const
  Buses : Array [TConnectionKind] of string
        = ('Custom','System','Session','Starter');

begin
  try
    If Not Assigned(FConn) then
      FCOnn:=TDBUSConnection.Create(Self)
    else
      FConn.Disconnect;
    FConn.Kind:=AKind;
    FConn.Connect;
    Caption:=Format('DBUS Inspector : %s bus',[Buses[AKind]]);
    Mlog.Lines.Add(Format('Connected to %s bus',[Buses[AKind]]));
    DisplayNames;
  except
    On E : Exception do
      ShowError(E);
  end;
end;

procedure TMainForm.ASessionExecute(Sender: TObject);
begin
  ConnectToBus(ckSession);
end;

procedure TMainForm.AQuitExecute(Sender: TObject);
begin
  Close;
end;

Function TMainForm.ObjectpathFromNode(N : TTreeNode) : String;

begin
  While (N<>Nil) do
    begin
    Result:=N.Text+Result;
    N:=N.Parent;
    end;
  Result:='/'+Copy(Result,1,Length(Result)-1);
end;

procedure TMainForm.GenerateCodeForIntrospection(AIntro : TDBUSIntroSpection; AInterfaceName,AUnitName : String);

Var
  CG : TDBUSCodeGenerator;
  F : TCodeOptionsForm;
  I : Integer;
  IO : TDBUSInterfaceCodeOptionItem;
  Intf : TDBUSInterfaceDef;
  S : String;
begin
  CG:=TDBUSCodeGenerator.Create(Self);
  try
    if (UnitName<>'') then
      CG.UnitName:=AUnitName
    else
      begin
      S:=ExtractFileExt(AInterfaceName);
      Delete(S,1,1);
      CG.UnitName:=S
      end;
    For I:=0 to AIntro.Interfaces.Count-1 do
      begin
      Intf:=AIntro.interfaces[i];
      IO:=CG.InterfaceOptions.Addoption(Intf.Name);
      IO.Skip:=Not ((AInterfaceName='') or (AInterfaceName=Intf.Name));
      end;
     With TCodeOptionsForm.Create(Self) do
       try
         CodeGen:=CG;
         Intro:=AIntro;
         ShowModal;
       finally
         Free;
       end;
  finally
    CG.Free;
  end;
end;

procedure TMainForm.AGenCodeExecute(Sender: TObject);

Var
  N,Ni : TTreeNode;
  Op,UN : String;
  Intro : TDBUSIntroSpection;


begin
  Ni:=Nil;
  N:=TVDBUS.Selected;
  While (N<>Nil) and (N.ImageIndex<>II_NODE) do
    begin
    If (N.ImageIndex=II_Interface) then
      NI:=N;
    N:=N.Parent;
    end;
  OP:=ObjectPathFromNode(N);
  Un:=ExtractFileName(OP);
  Intro:=GetIntrospection(FServiceName,OP);
  try
    If (Ni<>Nil) then
      OP:=Ni.Text
    else
      OP:='';
    GenerateCodeForIntrospection(Intro,OP,Un);
  finally
    Intro.free;
  end;
end;

procedure TMainForm.AGetpropExecute(Sender: TObject);

begin
  ShowPropertyValue(TVDBUS.Selected);
end;

procedure TMainForm.AGetpropUpdate(Sender: TObject);

Var
  N : TTreeNode;

begin

  N:=GetNodeNode(TVDBUS.Selected);
  (Sender as TAction).Enabled:=(FindPropertiesInterFace(N)<>Nil) and (TVDBUS.Selected.ImageIndex=II_Property);

end;

procedure TMainForm.DisplayInterfaces(Intf : TDBUSInterfaces; AParent : TTreeNode);

Var
  N : TTreeNode;
  I : Integer;

begin
   For I:=0 to Intf.Count-1 do
     begin
     N:=TVDBUS.Items.AddChild(AParent,Intf[i].Name);
     N.ImageIndex:=II_Interface;
     N.SelectedIndex:=II_Interface;
     DisplayInterface(N,Intf[i]);
     end;
end;

procedure TMainForm.DisplayInterface(ANode : TTreeNode;Intf : TDBUSInterfaceDef);

Var
  I : Integer;
  N : TTreeNode;

begin
  For I:=0 to Intf.Methods.Count-1 do
    begin
    N:=TVDBUS.Items.AddChild(ANode,'Method : '+Intf.Methods[i].Name);
    N.ImageIndex:=II_Method;
    N.SelectedIndex:=II_Method;
    DisplayMethod(N,Intf.Methods[i]);
    end;
  For I:=0 to Intf.Signals.Count-1 do
    begin
    N:=TVDBUS.Items.AddChild(ANode,'Signal : '+Intf.Signals[i].Name);
    N.ImageIndex:=II_Signal;
    N.SelectedIndex:=II_Signal;
    DisplaySignal(N,Intf.Signals[i]);
    end;
  For I:=0 to Intf.Properties.Count-1 do
    begin
    N:=TVDBUS.Items.AddChild(ANode,'Property : '+Intf.Properties[i].Name);
    N.ImageIndex:=II_Property;
    N.SelectedIndex:=II_Property;
    DisplayProperty(N,Intf.Properties[i]);
    end;
  DisplayAnnotation(ANode,Intf);
end;

procedure TMainForm.DisplayAnnotation(ANode : TTreeNode; AItem : TDBUSNamedItem);

Var
  N : TTreeNode;
  I : Integer;
  AN,AV : String;

begin
  If Not AItem.HasAnnotations then
    Exit;
  For i:=0 to AItem.Annotations.Count-1 do
    begin
    AItem.Annotations.GetNameValue(I,AN,AV);
    N:=TVDBUS.Items.AddChild(ANode,Format('Annotation "%s" : %s',[AN,AV]));
    N.ImageIndex:=II_Annotation;
    N.SelectedIndex:=II_Annotation;
    end;

end;


procedure TMainForm.DisplayMethod(ANode : TTreeNode; AMethod : TDBUSMethodDef);

Var
  I : Integer;
  S,D : String;
  A : TDBUSArgumentDef;
  N : TTreeNode;

begin
  For I:=0 to AMethod.Arguments.Count-1 do
    begin
    A:=AMethod.Arguments[i];
    S:=A.ArgName;
    Case AMethod.Arguments[i].Direction of
      adnone : S:='? '+S;
      adOut  : S:='out '+S;
      adIn   : S:='in '+S;
    end;
    D:=DBUSPascalTypeNames[A.DataType];
    If (D<>'') then
      S:=S+' : '+D;
    If (A.DataType=ddtArray) then
      S:=S+' of '+DBUSPascalTypeNames[A.ElementType];
    N:=TVDBUS.Items.AddChild(ANode,S);
    N.Data:=A;
    Case AMethod.Arguments[i].Direction of
        adOut  : N.ImageIndex:=II_ArgOut;
        adIn   : N.ImageIndex:=II_ArgIn;
    end;
    N.SelectedIndex:=N.ImageIndex;
    end;
  DisplayAnnotation(ANode,AMethod);
end;

procedure TMainForm.DisplaySignal(ANode : TTreeNode; ASignal : TDBUSSignalDef);

Var
  I : Integer;
  S,D : String;
  N : TTreeNode;
  V : TDBUSvarDef;
begin
  For I:=0 to ASignal.Variables.Count-1 do
    begin
    S:=ASignal.Variables[i].Name;
    If (S='') then
      S:='var'+inttostr(i);
    V:=ASignal.Variables[I];
    D:=DBUSPascalTypeNames[V.DataType];
    If (D<>'') then
      S:=S+' : '+D;
    If (V.DataType=ddtArray) then
      S:=S+DBUSPascalTypeNames[V.ElementType];
    N:=TVDBUS.Items.AddChild(ANode,S);
    N.Data:=V;
    end;
  DisplayAnnotation(ANode,ASignal);
end;

procedure TMainForm.DisplayProperty(ANode : TTreeNode; AProperty : TDBUSPropertyDef);

Var
  I : Integer;
  S,D : String;
  N : TTreeNode;
  V : TDBUSvarDef;
begin
  S:=DBUSPascalTypeNames[AProperty.DataType];
  If (V.DataType=ddtArray) then
    S:=S+DBUSPascalTypeNames[V.ElementType];
  Case AProperty.Access of
    dpaRead : S:=S+' (read-only)';
    dpaWrite : S:=S+' (write-only)';
    dpaReadWrite : S:=S+' (wead/write)';
  end;
  ANode.Text:=ANode.Text+' : '+S;
  DisplayAnnotation(ANode,AProperty);
end;

end.

