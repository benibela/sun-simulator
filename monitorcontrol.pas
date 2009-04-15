unit monitorControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,colorDiagramModels,
  {$IFDEF WIN32}
  windows
  {$endif}
  ;

type

{ TMonitorControl }

TMonitorControl=class
  procedure reset;virtual;abstract;
  procedure setTo(map: TColorMapper);virtual;abstract;
end;

{$IFDEF WIN32}

{ TWin32MonitorControl }

TWin32MonitorControl=class(tmonitorcontrol)
private
  def: array[0..2] of array[0..255] of word;
  dev: HDC;
public
  constructor create;
  destructor destroy;override;
  procedure reset;override;
  procedure setTo(map: TColorMapper);override;
end;
{$ENDIF}

var monitorControlInstance: TMonitorControl = nil;
implementation

{ TWin32MonitorControl }

{$IFDEF WIN32}

constructor TWin32MonitorControl.create;
begin
  dev:=CreateDC('DISPLAY',nil,nil,nil);
  GetDeviceGammaRamp(dev,@def);
end;

destructor TWin32MonitorControl.destroy;
begin
  DeleteDC(dev);
  inherited destroy;
end;

procedure TWin32MonitorControl.reset;
begin
  SetDeviceGammaRamp(dev,@def);
end;

procedure TWin32MonitorControl.setTo(map: TColorMapper);
var d:array[0..2] of array[0..255] of word;
  i: Integer;
begin
  for i:=0 to 255 do d[0,i]:=min($FFFF,max(0,round(map.map(cnRed,i/255)*$FFFF)));
  for i:=0 to 255 do d[1,i]:=min($FFFF,max(0,round(map.map(cnGreen,i/255)*$FFFF)));
  for i:=0 to 255 do d[2,i]:=min($FFFF,max(0,round(map.map(cnBlue,i/255)*$FFFF)));
  SetDeviceGammaRamp(dev,@d);
end;
{$ENDIF}

initialization
  {$IFDEF WIN32}
  monitorControlInstance:= TWin32MonitorControl.create;
  {$ENDIF}
finalization
  FreeAndNil(monitorControlInstance);

end.

