program sunSimulator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, maingui, LResources, colorDiagramModels,
  monitorControl;

{$IFDEF WINDOWS}{$R sunSimulator.rc}{$ENDIF}

begin
  Application.Title:='Sun-Simulator';
  {$I sunSimulator.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

