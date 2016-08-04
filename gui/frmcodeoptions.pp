{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2010 by Michael Van Canneyt, member of the
   Free Pascal development team

   DBUS Introspection code generation options

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
unit frmcodeoptions;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  StdCtrls, ExtCtrls, EditBtn, ButtonPanel, dbusintro;

type

  { TCodeOptionsForm }

  TCodeOptionsForm = class(TForm)
    BPreview: TButton;
    ButtonPanel1: TButtonPanel;
    Interfaces: TLabel;
    CGOptions: TCheckGroup;
    CLBInterfaces: TCheckListBox;
    EUnit: TEdit;
    EPRefix: TEdit;
    FEUnit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    LPrefix: TLabel;
    LUnitName: TLabel;
    procedure BPreviewClick(Sender: TObject);
    procedure ButtonPanel1Click(Sender: TObject);
    procedure CLBInterfacesDblClick(Sender: TObject);
    procedure EUnitExit(Sender: TObject);
  private
    FCodegen: TDBUSCodeGenerator;
    FIntro: TDBUSIntrospection;
    procedure FormToCodegen;
    procedure SetCodeGen(const AValue: TDBUSCodeGenerator);
    { private declarations }
  public
    { public declarations }
    Property Intro : TDBUSIntrospection Read FIntro Write Fintro;
    Property CodeGen : TDBUSCodeGenerator Read FCodegen Write SetCodeGen;
  end; 

var
  CodeOptionsForm: TCodeOptionsForm;

implementation

uses frmcode;

resourcestring
  SInterfaceName = 'Pascal name for interface';
  SGetInterfaceName = 'Enter a new name for interface "%s". This must be a Pascal identifier';

{$R *.lfm}

{ TCodeOptionsForm }

procedure TCodeOptionsForm.EUnitExit(Sender: TObject);
begin
  FEunit.FileName:=ExtractFilePath(FEunit.FileName)+EUnit.Text+'.pp';
end;

procedure TCodeOptionsForm.BPreviewClick(Sender: TObject);
begin
  FormToCodeGen;
  With TCodeForm.Create(Self) do
    try
      FCodeGen.GenerateUnit(Fintro.Interfaces,Code);
      ShowModal;
    Finally
      Free;
    end;
end;

procedure TCodeOptionsForm.ButtonPanel1Click(Sender: TObject);
begin
  FormToCodeGen;
  FCodeGen.SaveUnitToFile(Fintro.Interfaces,FEUNit.FileName);
end;

procedure TCodeOptionsForm.CLBInterfacesDblClick(Sender: TObject);

Var
  I : Integer;
  O : TDBUSInterfaceCodeOptionItem;
  S : String;

begin
  I:=CLBInterfaces.ItemIndex;
  If I=-1 then exit;
  O:=CLBInterfaces.Items.Objects[I] as TDBUSInterfaceCodeOptionItem;
  S:=O.PascalName;
  If (S='') then
    S:=O.Name;
  if InputQuery(SInterfaceName,Format(SGetInterfaceName,[O.Name]),S) then
    begin
    O.PascalName:=S;
    CLBInterfaces.Items[i]:=O.Name+' -> '+S;
    end;

end;

procedure TCodeOptionsForm.FormToCodegen;

var
  O : TDBUSCodeOption;
  S : TDBUSCodeOptionS;
  I : Integer;

begin
  FCodeGen.UnitName      := EUNit.Text;
  FCodegen.KeyWordPrefix := EPrefix.Text;
  S:=[];
  For O:=Low(TDBUSCodeOption) to High(TDBUSCodeOption) do
    if CGOptions.Checked[Ord(O)] then
      Include(S,O);
  FCodeGen.Options:=S;
  For I:=0 to FCodeGen.Interfaceoptions.Count-1 do
    begin
//    CLBInterfaces.Items.AddObject(FCodeGen.Interfaceoptions[i].Name,FCodeGen.Interfaceoptions);
    FCodeGen.Interfaceoptions[i].Skip:=Not CLBInterfaces.Checked[i];
    end;
end;

procedure TCodeOptionsForm.SetCodeGen(const AValue: TDBUSCodeGenerator);

Var
  O : TDBUSCodeOption;
  I : Integer;
  S : String;

begin
  FCodeGen:=AValue;
  EUNit.Text:=FCodeGen.UnitName;
  EPrefix.Text:=FCodegen.KeyWordPrefix;
  For O :=Low(TDBUSCodeOption) to High(TDBUSCodeOption) do
    CGOptions.Checked[Ord(O)]:=(O in FCodeGen.Options);
  For I:=0 to FCodeGen.Interfaceoptions.Count-1 do
    begin
    S:=FCodeGen.Interfaceoptions[i].PascalName;
    If S='' then
      S:=FCodeGen.Interfaceoptions[i].Name
    else
    S:=FCodeGen.Interfaceoptions[i].Name+' -> '+S;
    CLBInterfaces.Items.AddObject(S,FCodeGen.Interfaceoptions[i]);
    CLBInterfaces.Checked[i]:=Not FCodeGen.Interfaceoptions[i].Skip;
    end;
  FEunit.FileName:=EUnit.Text;
end;

end.

