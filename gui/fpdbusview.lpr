program fpdbusview;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, frmcodeoptions, frmcode
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='DBUS Viewer and code generation application';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCodeOptionsForm, CodeOptionsForm);
  Application.Run;
end.

