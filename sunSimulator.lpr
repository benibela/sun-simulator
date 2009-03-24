program sunSimulator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, maingui, LResources, colorDiagramModels;

{$IFDEF WINDOWS}{$R sunSimulator.rc}{$ENDIF}

begin
  {$I sunSimulator.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

