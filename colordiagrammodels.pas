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
  private
    FPatternImage: TBitmap;
    lastTestPattern: longint;
  public
    gamma, brightness, contrast: array[TModelColorName] of float;
    //interval [0,1] -> [0,1]
    constructor create;
    destructor destroy;override;
    function map(name:TModelColorName; color: float): float;
    procedure draw(w,h:longint;c: TCanvas);

    procedure drawTestPattern(id:longint; w,h:longint;c: TCanvas);
  end;

  { TDiagramColorModel }

  TDiagramColorModel = class (TDiagramFixedWidthCircularDataListModel)
  private
    FshowSingleColors: boolean;
    procedure SetshowSingleColors(const AValue: boolean);
  public
    constructor create(freq: TModelColorFrequency; role:TModelColorRole) ;
    function getValue(colorName:TModelColorName; x: float):float;
    function saveToString(prefix:string): string;
  published
    property showSingleColors: boolean read FshowSingleColors write SetshowSingleColors;
  end;

  { TModelColorModelManager }

  TModelColorModelManager = class
  private
    FOnModified: TNotifyEvent;
    procedure SetOnModified(const AValue: TNotifyEvent);
    procedure setModelTime(const day, year:float);
    procedure setMarkTime(const day1, year1, day2, year2:float);
  public
    mapper: TColorMapper;
    timeModels: array of TDiagramDataListModel;
    models: array of array of TDiagramColorModel;  //array of frequency (day, year), array of role(Gamma, kontrast, helligkeit)
    constructor create;
    destructor destroy;override;
    function selectModel(view: TDiagramView; freq: TModelColorFrequency; role: TModelColorRole):TDiagramColorModel;
    function getSelectedModel(view: TDiagramView): TDiagramColorModel;
    class procedure decodeTime(time: TDateTime; out day, year: float);
    class function encodeTime(const day, year: float):TDateTime;

    procedure setTime(const time: TDateTime);
    procedure setPreviewTime(const rtime: TDateTime; const oTime: float; oFreq: TModelColorFrequency);

    function saveToString: string;

    property OnModelModified: TNotifyEvent read FOnModified write SetOnModified;
  end;


implementation
uses IntfGraphics,FPimage;
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

procedure TModelColorModelManager.setModelTime(const day, year: float);
var n:TModelColorName;
begin
  //day freq
  for n:=cnRed to cnBlue do begin
    mapper.gamma[n]:=models[0][0].getValue(n,day);
    mapper.contrast[n]:=models[0][1].getValue(n,day);
    mapper.brightness[n]:=models[0][2].getValue(n,day);
  end;


  //year freq
  for n:=cnRed to cnBlue do begin
    mapper.gamma[n]*=models[1][0].getValue(n,year);
    mapper.contrast[n]*=models[1][1].getValue(n,year);
    mapper.brightness[n]+=models[1][2].getValue(n,year);
  end;
end;

procedure TModelColorModelManager.setMarkTime(const day1, year1, day2,
  year2: float);
begin
  //day freq
  timeModels[0].lists[0].clear();
  timeModels[0].lists[1].clear();
  timeModels[0].lists[0].setPoint(0,day1,0);
  if not IsNan(day2) then
    timeModels[0].lists[1].setPoint(0,day2,0);

  //year freq
  timeModels[1].lists[0].clear();
  timeModels[1].lists[1].clear();
  timeModels[1].lists[0].setPoint(0,year1,0);
  if not isNan(year2) then
    timeModels[1].lists[1].setPoint(0,year2,0);
end;

constructor TModelColorModelManager.create;
var i,j:longint;
begin
  setlength(models,2,3);
  for i:=0 to high(models) do for j:=0 to high(models[i]) do begin
    models[i,j]:=TDiagramColorModel.create(TModelColorFrequency(i),TModelColorRole(j));
  end;
  mapper:=TColorMapper.create;
  setlength(timeModels,2);
  for i:=0 to high(timeModels) do begin
    timeModels[i]:=TDiagramDataListModel.create;
    timeModels[i].addDataList;
    timeModels[i].lists[0].LineStyle:=lsNone;
    timeModels[i].lists[0].PointStyle:=psNone;
    timeModels[i].lists[0].Flags:=[rfFullY];
    timeModels[i].lists[0].Color:=clAqua;
    timeModels[i].addDataList;
    timeModels[i].lists[1].LineStyle:=lsNone;
    timeModels[i].lists[1].PointStyle:=psNone;
    timeModels[i].lists[1].Flags:=[rfFullY];
    timeModels[i].lists[1].Color:=clFuchsia;
  end;
end;

destructor TModelColorModelManager.destroy;
var i,j:longint;
begin
  for i:=0 to high(models) do for j:=0 to high(models[i]) do begin
    models[i,j].free;
  end;
  mapper.free;
  for i:=0 to high(timeModels) do timeModels[i].Free;
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
  if view.Model=nil then begin
    view.SetModel(TDiagramModelMerger.create(timeModels[integer(freq)],result),true);
    TDiagramModelMerger(view.model).BaseModel:=1;
  end else begin
    TDiagramModelMerger(view.Model).SetModel(0, timeModels[integer(freq)]);
    TDiagramModelMerger(view.Model).SetModel(1,result);
  end;
  //view.Drawer.LineStyle:=lsLinear;
end;

function TModelColorModelManager.getSelectedModel(view: TDiagramView
  ): TDiagramColorModel;
begin
  result:=(view.Model as TDiagramModelMerger).Models[1] as TDiagramColorModel;
end;

class procedure TModelColorModelManager.decodeTime(time: TDateTime; out day,
  year: float);
var yearw,monthw,dayw:word;
begin
  day:=frac(time)*24;
  DecodeDate(time,yearw,monthw,dayw);
  year:=time-EncodeDate(yearw,1,1);
  if IsLeapYear(yearw) then year:=year*365/366;
end;

class function TModelColorModelManager.encodeTime(const day, year: float):TDateTime;
begin
  result:=EncodeDate(2009,1,1) + year + day/24;
end;

procedure TModelColorModelManager.setTime(const time: TDateTime);
var n:TModelColorName;
    day,year:float;
begin
  decodeTime(time,day,year);
  setModelTime(day,year);
  setMarkTime(day,year,nan,nan);
end;

procedure TModelColorModelManager.setPreviewTime(const rtime: TDateTime;
  const oTime: float; oFreq: TModelColorFrequency);
var n:TModelColorName;
    day,year:float;
begin
  decodeTime(time,day,year);
  if oFreq=cfDay then begin
    setModelTime(oTime,year);
    setMarkTime(day,year,oTime,nan);
  end else begin
    setModelTime(oTime,day);
    setMarkTime(day,year,nan,oTime);
  end;
end;

function TModelColorModelManager.saveToString: string;
const FNAMEID: array[0..1] of string=('D','Y');
      TNAMEID: array[0..2] of string=('G','C','B');
//saves to a very simple comma (;) separated list
var i,j:Integer;
begin
  result:='';
  for i:=0 to 1 do
    for j:=0 to 2 do
      result+=models[i,j].saveToString(FNAMEID[i]+'-'+TNAMEID[j]+'-');
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
      lists[3].addPoint(dataX(0,i),(dataY(0,i)+lineApproximationAtX(lsCubicSpline,1,dataX(0,i))+lineApproximationAtX(lsCubicSpline,2,dataX(0,i)))/3);
    for i:=0 to lists[1].count-1 do
      lists[3].addPoint(dataX(1,i),(dataY(1,i)+lineApproximationAtX(lsCubicSpline,0,dataX(1,i))+lineApproximationAtX(lsCubicSpline,2,dataX(1,i)))/3);
    for i:=0 to lists[2].count-1 do
      lists[3].addPoint(dataX(2,i),(dataY(2,i)+lineApproximationAtX(lsCubicSpline,0,dataX(2,i))+lineApproximationAtX(lsCubicSpline,1,dataX(2,i)))/3);
    //remove old
    deleteDataRow(2);
    deleteDataRow(1);
    deleteDataRow(0);

    lists[0].color:=clWhite;
  end;
  doModified();
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
  if not showSingleColors then exit(lineApproximationAtX(lsCubicSpline,0,x))
  else exit(lineApproximationAtX(lsCubicSpline,integer(colorName),x))
end;

function TDiagramColorModel.saveToString(prefix: string): string;
const NAMEID: array[0..2] of string = ('R','G','B');
var i:longint;
  j: Integer;
begin
  if not FshowSingleColors then begin
    result:=prefix+'W';
    for i:=0 to dataPoints(0)-1 do
      result+=Format('-(%.5f;%.5f)',[dataX(0,i),dataY(0,i)]);
    result+=#13#10;
  end else begin
    result:='';
    for j:=0 to 2 do begin
      result+=prefix+NAMEID[j];
      for i:=0 to dataPoints(j)-1 do
        result+=Format('-(%.5f;%.5f)',[dataX(j,i),dataY(j,i)]);
      result+=#13#10;
    end;
  end;
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
  FPatternImage:=TBitmap.Create;
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
  c.pen.Mode:=pmXor;
  for n:=low(TModelColorName) to high(TModelColorName) do begin
    c.Pen.Color:=ColorColor[n];
    c.MoveTo(0,h-round(h*map(n,0)));
    for i:=1 to w do
      c.LineTo(i,h-round(h*map(n,i/w)));
  end;
end;

procedure TColorMapper.drawTestPattern(id: longint; w, h: longint; c: TCanvas);
var lz: TLazIntfImage;
    x,y,t,wm,hm,maxsize:longint;
    bitmap,tempmaskbitmap: THandle;
begin
  if (lastTestPattern<>id) or (FPatternImage.Height<>h) or (FPatternImage.Width<>w) then begin
    lastTestPattern:=id;
    FPatternImage.Height:=h;
    FPatternImage.Width:=w;
    lz:=TLazIntfImage.Create(0,0);
    lz.LoadFromBitmap(FPatternImage.Handle,0); //otherwise create bitmaps fail
    lz.BeginUpdate;
    wm:=w div 2;
    hm:=h div 2;
    if wm>hm then maxsize:=wm
    else maxsize:=hm;
    case id of
      0: for x:=0 to w-1 do //modified http://brighamrad.harvard.edu/research/topics/vispercep/tutorial.html
           for y:=0 to h-1 do begin
             t:=round($FFFF*sqrt(((x-wm)*(x-wm)+(y-hm)*(y-hm)) / (maxsize*maxsize)));
             if t<0 then t:=0;
             if t>$FFFF then t:=$FFFF;
             if abs(x-wm) > abs(y-hm) then t:=$FFFF-t;
             if x-wm <= y-hm then begin
               if y-hm >= -(x-wm) then
                 lz[x,y]:=fpColor(t,0,0)
                else
                 lz[x,y]:=fpColor(0,t,0);
             end else begin //left/top area
               if y-hm > -(x-wm) then
                 lz[x,y]:=fpColor(0,0,t)
                else
                 lz[x,y]:=fpColor(t,t,t);
             end;
           end;
      1: for x:=0 to w-1 do //http://brighamrad.harvard.edu/research/topics/vispercep/tutorial.html
           for y:=0 to h-1 do begin
             t:=round($FFFF*sqrt(((x-wm)*(x-wm)+(y-hm)*(y-hm)) / (maxsize*maxsize)));
             if t<0 then t:=0;
             if t>$FFFF then t:=$FFFF;
             if abs(x-wm) > abs(y-hm) then t:=$FFFF-t;
             lz[x,y]:=fpColor(t,t,t);
           end;
    end;
    lz.EndUpdate;
    lz.CreateBitmaps(bitmap,tempmaskbitmap,true);
    FPatternImage.Handle:=bitmap;
    lz.free;
  end;
  c.Draw(0,0,FPatternImage);
end;

destructor TColorMapper.destroy;
begin
  FPatternImage.Free;
  inherited destroy;
end;

end.

