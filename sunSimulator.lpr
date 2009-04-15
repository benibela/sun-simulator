program sunSimulator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, maingui, LResources, colorDiagramModels,
  monitorControl, menuManager;

{$IFDEF WINDOWS}{$R sunSimulator.rc}{$ENDIF}

begin
  Application.Title:='Sun-Simulator';
  {$I sunSimulator.lrs}
  Application.ShowMainForm:=false;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  if not((ParamStr(1)='--autostart') or (ParamStr(1)='/autostart')) then
    Form1.Show;
  Application.Run;
end.

