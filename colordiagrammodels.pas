unit colorDiagramModels;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,math,diagram,Graphics;

type

  TModelColorName=(cnRed, cnGreen, cnBlue);
  TModelColorRole=(crGamma, crContrast, crBrightness);
  TModelColorFrequency=(cfDay, cfYear);

  { TColorMapper }

  TColorMapper = class
    gamma, brightness, contrast: array[TModelColorName] of float;
    //interval [0,1] -> [0,1]
    constructor create;
    function map(name:TModelColorName; color: float): float;
    procedure draw(w,h:longint;c: TCanvas);
  end;

  { TDiagramColorModel }

  TDiagramColorModel = class (TDiagramFixedWidthCircularDataListModel)
  private
    FshowSingleColors: boolean;
    procedure SetshowSingleColors(const AValue: boolean);
  public
    constructor create(freq: TModelColorFrequency; role:TModelColorRole) ;
    function getValue(colorName:TModelColorName; x: float):float;
  published
    property showSingleColors: boolean read FshowSingleColors write SetshowSingleColors;
  end;

  { TModelColorModelManager }

  TModelColorModelManager = class
  private
    FOnModified: TNotifyEvent;
    procedure SetOnModified(const AValue: TNotifyEvent);
  public
    mapper: TColorMapper;
    models: array of array of TDiagramColorModel;  //array of frequency (day, year), array of role(Gamma, kontrast, helligkeit)
    constructor create;
    destructor destroy;override;
    function selectModel(view: TDiagramView; freq: TModelColorFrequency; role: TModelColorRole):TDiagramColorModel;
    function getMapper(time: TDateTime): TColorMapper;

    property OnModelModified: TNotifyEvent read FOnModified write SetOnModified;
  end;


implementation
const FrequenceMax: array[TModelColorFrequency] of longint = (24, 365);
      RoleDefault: array[TModelColorRole] of float = (1,1,0);

{ TModelColorModelManager }

procedure TModelColorModelManager.SetOnModified(const AValue: TNotifyEvent);
var i,j:longint;
begin
  if FOnModified=AValue then exit;
  FOnModified:=AValue;
  for i:=0 to high(models) do for j:=0 to high(models[i]) do
    models[i,j].onModified:=AValue;
end;

constructor TModelColorModelManager.create;
var i,j:longint;
begin
  setlength(models,2,3);
  for i:=0 to high(models) do for j:=0 to high(models[i]) do begin
    models[i,j]:=TDiagramColorModel.create(TModelColorFrequency(i),TModelColorRole(j));
  end;
  mapper:=TColorMapper.create;
end;

destructor TModelColorModelManager.destroy;
var i,j:longint;
begin
  for i:=0 to high(models) do for j:=0 to high(models[i]) do begin
    models[i,j].free;
  end;
  mapper.free;
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
      view.Drawer.RangeMaxY:=2;
      view.Drawer.RangeMinY:=0;
    end;
    crBrightness: begin
      view.Drawer.RangeMaxY:=1;
      view.Drawer.RangeMinY:=-1;
    end;
  end;
  view.Drawer.RangeMinX:=0;
  view.Drawer.RangeMaxX:=FrequenceMax[freq];
  result:=models[integer(freq), integer(role)];
  view.SetModel(result);
end;

function TModelColorModelManager.getMapper(time: TDateTime): TColorMapper;
var n:TModelColorName;
    year,month,day:word;
    t:float;
begin
  //day freq
  t:=frac(time)*24;
  for n:=cnRed to cnBlue do begin
    mapper.gamma[n]:=models[0][0].getValue(n,t);
    mapper.contrast[n]:=models[0][1].getValue(n,t);
    mapper.brightness[n]:=models[0][2].getValue(n,t);
  end;
  //year freq
  DecodeDate(time,year,month,day);
  t:=time-EncodeDate(year,1,1);
  if IsLeapYear(year) then t:=t*365/366;
  for n:=cnRed to cnBlue do begin
    mapper.gamma[n]*=models[1][0].getValue(n,t);
    mapper.contrast[n]*=models[1][1].getValue(n,t);
    mapper.brightness[n]+=models[1][2].getValue(n,t);
  end;
  result:=mapper;
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

function TDiagramColorModel.getValue(colorName: TModelColorName; x: float
  ): float;
begin
  if not showSingleColors then exit(lineYatX(lsCubicSpline,0,x))
  else exit(lineYatX(lsCubicSpline,integer(colorName),x))
end;

{ TColorMapper }

constructor TColorMapper.create;
var n:TModelColorName;
begin
  for n:=cnRed to cnBlue do begin
    gamma[n]:=1;
    brightness[n]:=0;
    contrast[n]:=1;
  end;
end;

function TColorMapper.map(name: TModelColorName; color: float): float;
var g:float;
begin
  if gamma[name]<1e-10 then exit(0);
  g:=1/gamma[name];
  Result:=power(color, g)*contrast[name]+brightness[name];
  if result<0 then result:=0;
  if result>1 then result:=1;
end;

procedure TColorMapper.draw(w, h: longint; c: TCanvas);
var i:longint;
    n:TModelColorName;
const ColorColor: array[TModelColorName] of TColor = (clRed, clLime, clBlue);
begin
  c.Brush.Color:=clBlack;
  c.FillRect(0,0,w,h);
  for n:=low(TModelColorName) to high(TModelColorName) do begin
    c.Pen.Color:=ColorColor[n];
    c.MoveTo(0,h-round(h*map(n,0)));
    for i:=1 to w do
      c.LineTo(i,h-round(h*map(n,i/w)));
  end;
end;

end.

