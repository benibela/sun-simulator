unit colorDiagramModels;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,math,diagram,Graphics;

type

  TModelColorRole=(crGamma, crContrast, crBrightness);
  TModelColorFrequency=(cfDay, cfYear);
{ TDiagramColorModel }

TDiagramColorModel = class (TDiagramFixedWidthCircularDataListModel)
private
  FshowSingleColors: boolean;
  procedure SetshowSingleColors(const AValue: boolean);
public
  constructor create(freq: TModelColorFrequency; role:TModelColorRole) ;

published
  property showSingleColors: boolean read FshowSingleColors write SetshowSingleColors;
end;

  { TModelColorModelManager }

  TModelColorModelManager = class
    models: array of array of TDiagramColorModel;  //array of frequency, array of role(Gamma, kontrast, helligkeit)
    constructor create;
    destructor destroy;override;
    function selectModel(view: TDiagramView; freq: TModelColorFrequency; role: TModelColorRole):TDiagramColorModel;
  end;

implementation
const FrequenceMax: array[TModelColorFrequency] of longint = (24, 365);
      RoleDefault: array[TModelColorRole] of float = (1,100,0);

{ TModelColorModelManager }

constructor TModelColorModelManager.create;
var i,j:longint;
begin
  setlength(models,2,3);
  for i:=0 to high(models) do for j:=0 to high(models[i]) do begin
    models[i,j]:=TDiagramColorModel.create(TModelColorFrequency(i),TModelColorRole(j));
  end;
end;

destructor TModelColorModelManager.destroy;
var i,j:longint;
begin
  for i:=0 to high(models) do for j:=0 to high(models[i]) do begin
    models[i,j].free;
  end;
  inherited;
end;

function TModelColorModelManager.selectModel(view: TDiagramView; freq: TModelColorFrequency;
  role: TModelColorRole):TDiagramColorModel;
begin
  view.Drawer.AutoSetRangeX:=false;
  view.Drawer.AutoSetRangeY:=false;
  case role of
    crGamma: begin
      view.Drawer.RangeMaxY:=4;
      view.Drawer.RangeMinY:=0.5;
    end;
    crContrast: begin
      view.Drawer.RangeMaxY:=200;
      view.Drawer.RangeMinY:=0;
    end;
    crBrightness: begin
      view.Drawer.RangeMaxY:=100;
      view.Drawer.RangeMinY:=-100;
    end;
  end;
  view.Drawer.RangeMinX:=0;
  view.Drawer.RangeMaxX:=FrequenceMax[freq];
  result:=models[integer(freq), integer(role)];
  view.SetModel(result);
end;

{ TDiagramColorModel }

procedure TDiagramColorModel.SetshowSingleColors(const AValue: boolean);
var i:longint;
begin
  if FshowSingleColors=AValue then exit;
  FshowSingleColors:=AValue;
  if FshowSingleColors then begin
    setDataRows(3);
    lists[1].assign(lists[0]);
    lists[2].assign(lists[0]);
    lists[0].color:=clRed;
    lists[1].color:=clLime;
    lists[2].color:=clBlue;
  end else begin
    setDataRows(4); //temporary row
    //set to mean
    for i:=0 to lists[0].count-1 do
      lists[3].addPoint(dataX(0,i),(dataY(0,i)+lineYatX(lsCubicSpline,1,dataX(0,i))+lineYatX(lsCubicSpline,2,dataX(0,i)))/3);
    for i:=0 to lists[1].count-1 do
      lists[3].addPoint(dataX(1,i),(dataY(1,i)+lineYatX(lsCubicSpline,0,dataX(1,i))+lineYatX(lsCubicSpline,2,dataX(1,i)))/3);
    for i:=0 to lists[2].count-1 do
      lists[3].addPoint(dataX(2,i),(dataY(2,i)+lineYatX(lsCubicSpline,0,dataX(2,i))+lineYatX(lsCubicSpline,1,dataX(2,i)))/3);
    //remove old
    deleteDataRow(2);
    deleteDataRow(1);
    deleteDataRow(0);

    lists[0].color:=clWhite;
  end;
  doModified;
end;

constructor TDiagramColorModel.create(freq: TModelColorFrequency; role:TModelColorRole);
var i:longint;
begin
  inherited create;
  setDataRows(3);
  Flags:=[mfEditable];
  FshowSingleColors:=true;
  lists[0].color:=clRed;
  lists[1].color:=clLime;
  lists[2].color:=clBlue;
  for i:=0 to 2 do begin
    lists[i].addPoint(0,RoleDefault[role]);
    lists[i].addPoint(FrequenceMax[freq],RoleDefault[role]);
  end;
end;

end.

