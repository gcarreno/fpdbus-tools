{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2010 by Michael Van Canneyt, member of the
   Free Pascal development team

   DBUS Introspection generated code preview

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit frmcode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  SynMemo, SynHighlighterPas;

type

  { TCodeForm }

  TCodeForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynMemo1: TSynMemo;
  private
    function GetC: TStrings;
    procedure SetC(const AValue: TStrings);
    { private declarations }
  public
    { public declarations }
    Property Code : TStrings Read GetC Write SetC;
  end; 

var
  CodeForm: TCodeForm;

implementation

{$R *.lfm}

{ TCodeForm }

function TCodeForm.GetC: TStrings;
begin
  Result:=SynMemo1.Lines;
end;

procedure TCodeForm.SetC(const AValue: TStrings);
begin
  SynMemo1.Lines.Assign(AValue);
end;

end.

