program dbus2pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cwstring,
  {$ENDIF}
  Classes, SysUtils, CustApp, xmlread,typinfo,
  dom, dbuscomp, dbusintro, dbusintf;

type

  { TDBUSIntfApplication }

  TDBUSIntfApplication = class(TCustomApplication)
  private
    procedure GenerateCode(Intro: TDBUSIntrospection);
    function GetCodeOptions: TDBUSCodeOptions;
    function GetXML(out XML: TXMLDocument): Boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TDBUSIntfApplication }

Function TDBUSIntfApplication.GetXML(Out XML : TXMLDocument) : Boolean;

Var
  S : TStringStream;
  Conn : TDBUSConnection;
  P : Torg_freedesktop_DBus_Introspectable_proxy;

begin
  If HasOption('f','file') then
    begin
    ReadXMlFile(XML,GetOptionValue('f','file'));
    Result:=true;
    end
  else if not HasOption('d','destination') then
    begin
    WriteHelp;
    Result:=False;
    end
  else
    begin
    Conn:=TDBUSConnection.Create(Self);
    try
      if HasOption('s','system') then
        Conn.Kind:=ckSystem
      else
        Conn.Kind:=ckSession;
      Conn.Connect;
      P:=Torg_freedesktop_DBus_Introspectable_proxy.create(Self);
      try
        P.Connection:=Conn;
        If HasOption('p','objectpath') then
          P.ObjectPath:=GetOptionValue('p','objectpath')
        else
          P.ObjectPath:='/';
        P.Destination:=GetOptionValue('d','destination');
        S:=TStringStream.Create(P.Introspect);
        try
          If HasOption('x','xmlfile') then
            With TFileStream.Create(GetOPtionvalue('x','xmlfile'),fmCreate) do
              Try
                CopyFrom(S,0);
                S.Position:=0;
              finally
                Free;
              end;
          ReadXMLFile(Xml,S);
          Result:=True;
        finally
          S.Free;
        end;
      finally
        P.Free;
      end;
    finally
      Conn.Free;
    end;
    end;
end;

procedure TDBUSIntfApplication.DoRun;
var
  ErrorMsg: String;
  XMl : TXMLDocument;
  Intro : TDBUSintrospection;

begin
  ErrorMsg:=CheckOptions('hpsdfuockx','help objectpath system destination file unitname output codeoptions keywordprefix xmlfile');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  If GetXml(Xml) then
    begin
    try
      Intro:=TDBUSIntrospection.Create(Self);
      try
        Intro.LoadFromXML(XML);
        GenerateCode(Intro);
      finally
        Intro.Free;
      end;
    finally
      XML.Free;
    end;
    end;
  Terminate;
end;

Function TDBUSIntfApplication.GetCodeOptions : TDBUSCodeOptions;

Var
  p : Integer;
  S,O : String;

begin
  if not HasOption('c','codeoptions') then
    Result:=DefaultCodeOptions
  else
    begin
    Result:=[];
    S:=GetOptionValue('c','codeoptions');
    While (S<>'') do
      begin
      P:=Pos(',',S);
      If P=0 then
        P:=Length(S)+1;
      O:=Trim(Copy(S,1,P-1));
      Delete(S,1,P);
      P:=GetEnumValue(TypeInfo(TDBUSCodeOption),'dco'+O);
      If (P=-1) then
        Writeln('Unknown code option : ',O)
      else
        Include(Result,TDBUSCodeOption(P));
       end;
    end;
end;
Procedure TDBUSIntfApplication.GenerateCode(Intro : TDBUSIntrospection);

Var
  Code : TDBUSCodegenerator;
  Fn,UN : String;

begin
  If HasOption('u','unitname') then
    UN:=GetOptionValue('u','unitname');
  If HasOption('o','output') then
    FN:=GetOptionValue('o','output')
  else if (UN<>'') then
    FN:=UN+'.pp'
  else
    FN:='unit1.pp';
  if (UN='') then
    if (FN<>'') then
      UN:=changefileext(ExtractFileName(FN),'');
  Code:=TDBUSCodegenerator.Create(Self);
  try
    Code.UnitName:=UN;
    Code.Options:=GetCodeOptions;
    if HasOption('k','keywordprefix') then
      Code.KeyWordPrefix:=GetoptionValue('k','keywordprefix');

    Code.SaveUnitToFile(Intro.Interfaces,FN);
  finally
    Code.Free;
  end;
end;

constructor TDBUSIntfApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDBUSIntfApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TDBUSIntfApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' options ');
  writeln('With options one of:');
  Writeln('-h | --help            print this help message');
  Writeln('-s | --system          connect to system bus');
  Writeln('-p | --objectpath=O    object to introspect in service (default is /)');
  Writeln('-o | --output=N        filename to write to (defaults to unitname)');
  Writeln('-d | --destination=D   destination service to connect to');
  Writeln('-f | --file=N          file to read XML from');
  Writeln('-u | --unitname=N      unitname (equals output if not set)');
  Writeln('-c | --codeoptions=N   Set code generation options. Comma-separated list of');
  Writeln('    GenerateInterface       Generate interface declaration');
  Writeln('    GenerateProxy           Generate proxy declaration');
  Writeln('    ProxyHasInterface       Proxy implements interface');
  Writeln('    ProxyUsesProtected      Proxy methods are protected');
  Writeln('    UseFunction             Use functions for methods with OUT parameter');
  Writeln('    IncludeSystemInterfaces Include system interfaces');
  Writeln('    LastPartInterfaceName   InterfaceName is constructed from last part of DBUS name if none specified');
  Writeln('-k | --keywordprefix=p Prefix for pascal identifier names');
  Writeln('-x | --xmlfile=N       Write introspection XML to file (only with -d)');
end;

var
  Application: TDBUSIntfApplication;

{$R *.res}

begin
  Application:=TDBUSIntfApplication.Create(nil);
  Application.Title:='DBUS code pascal code generator';
  Application.Run;
  Application.Free;
end.

