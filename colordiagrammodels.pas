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
    ffreq: TModelColorFrequency;
    frole:TModelColorRole;
    procedure SetshowSingleColors(const AValue: boolean);
  public
    procedure resetToDefault;
    constructor create(freq: TModelColorFrequency; role:TModelColorRole) ;
    function getValue(colorName:TModelColorName; x: float):float;
    function saveToString(prefix:string): string;
    procedure loadFromString(str: string);
  published
    property showSingleColors: boolean read FshowSingleColors write SetshowSingleColors;
  end;

  { TModelColorModelManager }

  TModelColorModelManager = class
  private
    FOnModified: TNotifyEvent;
    FState: set of (msLoading);
    procedure DoModelModified(sender:TObject);
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

    function isValid:boolean;
    procedure setTime(const time: TDateTime);
    procedure setPreviewTime(const rtime: TDateTime; const oTime: float; oFreq: TModelColorFrequency);

    function saveToString: string;
    procedure loadFromString(s: string);

    property OnModelModified: TNotifyEvent read FOnModified write FOnModified;
  end;


implementation
uses IntfGraphics,FPimage;
const FrequenceMax: array[TModelColorFrequency] of longint = (24, 365);
      RoleDefault: array[TModelColorRole] of float = (1,1,0);
const FNAMEID: array[0..1] of string=('D','Y');
      TNAMEID: array[0..2] of string=('G','C','B');

{ TModelColorModelManager }

procedure TModelColorModelManager.DoModelModified(sender: TObject);
begin
  if assigned(FOnModified) and not (msLoading in FState) then FOnModified(sender);
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
  timeModels[0].lists[0].setPoint(0,day1,9999);
  if not IsNan(day2) then
    timeModels[0].lists[1].setPoint(0,day2,9999);

  //year freq
  timeModels[1].lists[0].clear();
  timeModels[1].lists[1].clear();
  timeModels[1].lists[0].setPoint(0,year1,9999);
  if not isNan(year2) then
    timeModels[1].lists[1].setPoint(0,year2,9999);
end;

constructor TModelColorModelManager.create;
var i,j:longint;
begin
  setlength(models,2,3);
  for i:=0 to high(models) do for j:=0 to high(models[i]) do begin
    models[i,j]:=TDiagramColorModel.create(TModelColorFrequency(i),TModelColorRole(j));
    models[i,j].onModified:=@DoModelModified;
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

function TModelColorModelManager.isValid: boolean;
var
  k: Integer;
  j: Integer;
  i: Integer;
begin
  for i:=0 to high(models) do
    for j:=0 to high(models[i]) do begin
      if models[i,j].dataRows()<=0 then exit(false);
      for k:=0 to models[i,j].dataRows()-1 do
        if models[i,j].dataPoints(k)<2 then exit(false);
    end;
end;

procedure TModelColorModelManager.setTime(const time: TDateTime);
var n:TModelColorName;
    day,year:float;
begin
  if not isValid then exit;
  decodeTime(time,day,year);
  setModelTime(day,year);
  setMarkTime(day,year,nan,nan);
end;

procedure TModelColorModelManager.setPreviewTime(const rtime: TDateTime;
  const oTime: float; oFreq: TModelColorFrequency);
var n:TModelColorName;
    day,year:float;
begin
  if not isValid then exit;
  decodeTime(rtime,day,year);
  if oFreq=cfDay then begin
    setModelTime(oTime,year);
    setMarkTime(day,year,oTime,nan);
  end else begin
    setModelTime(day,oTime);
    setMarkTime(day,year,nan,oTime);
  end;
end;

function TModelColorModelManager.saveToString: string;
//saves to a very simple comma (;) separated list
var i,j:Integer;
begin
  result:='';
  for i:=0 to 1 do
    for j:=0 to 2 do
      result+=models[i,j].saveToString(FNAMEID[i]+'-'+TNAMEID[j]+'-');
end;

procedure TModelColorModelManager.loadFromString(s: string);
var sl:TStringList;
  l: Integer;
  id:string;
  j: Integer;
  i: Integer;
begin
  include(FState,msLoading);
  try
    sl:=TStringList.Create;
    for i:=0 to 1 do
      for j:=0 to 2 do
        models[i,j].resetToDefault;
    sl.Text:=s;
    for l:=0 to sl.Count-1 do begin
      if length(sl[l])<5 then continue;
      id:=copy(sl[l],1,4);
      for i:=0 to 1 do
        for j:=0 to 2 do
          if id=FNAMEID[i]+'-'+TNAMEID[j]+'-' then
            models[i,j].loadFromString(copy(sl[l],5,length(sl[l])-4));
    end;
  finally
    exclude(FState,msLoading);
    sl.free;
  end;
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

procedure TDiagramColorModel.resetToDefault;
var
  i: Integer;
begin
  FshowSingleColors:=true;
  setDataRows(3);
  lists[0].color:=clRed;
  lists[1].color:=clLime;
  lists[2].color:=clBlue;
  for i:=0 to 2 do begin
    lists[i].clear();
    lists[i].addPoint(0,RoleDefault[frole]);
    lists[i].addPoint(FrequenceMax[ffreq],RoleDefault[frole]);
  end;
end;

constructor TDiagramColorModel.create(freq: TModelColorFrequency; role:TModelColorRole);
var i:longint;
begin
  inherited create;
  Flags:=[mfEditable];
  ffreq:=freq;
  frole:=role;
  resetToDefault;
end;

function TDiagramColorModel.getValue(colorName: TModelColorName; x: float
  ): float;
begin
  if not showSingleColors then exit(lineApproximationAtX(lsCubicSpline,0,x))
  else exit(lineApproximationAtX(lsCubicSpline,integer(colorName),x))
end;

const ModelFormatSettings: TFormatSettings = (
  CurrencyFormat: 1;
  NegCurrFormat: 5;
  ThousandSeparator: ',';
  DecimalSeparator: '.';
  CurrencyDecimals: 2;
  DateSeparator: '-';
  TimeSeparator: ':';
  ListSeparator: ',';
  CurrencyString: '$';
  ShortDateFormat: 'd/m/y';
  LongDateFormat: 'dd" "mmmm" "yyyy';
  TimeAMString: 'AM';
  TimePMString: 'PM';
  ShortTimeFormat: 'hh:nn';
  LongTimeFormat: 'hh:nn:ss';
  ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                    'Jul','Aug','Sep','Oct','Nov','Dec');
  LongMonthNames: ('January','February','March','April','May','June',
                   'July','August','September','October','November','December');
  ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
  TwoDigitYearCenturyWindow: 50;
);

function TDiagramColorModel.saveToString(prefix: string): string;
const NAMEID: array[0..2] of string = ('R','G','B');
var i:longint;
  j: Integer;
begin
  if not FshowSingleColors then begin
    result:=prefix+'W';
    for i:=0 to dataPoints(0)-1 do
      result+=Format('-(%.5f;%.5f)',[dataX(0,i),dataY(0,i)], ModelFormatSettings);
    result+=#13#10;
  end else begin
    result:='';
    for j:=0 to 2 do begin
      result+=prefix+NAMEID[j];
      for i:=0 to dataPoints(j)-1 do
        result+=Format('-(%.5f;%.5f)',[dataX(j,i),dataY(j,i)], ModelFormatSettings);
      result+=#13#10;
    end;
  end;
end;

procedure TDiagramColorModel.loadFromString(str: string);
  procedure scanFStr(m:longint; s:string);
  const fmt:string='-(x;y)';
  var n,p:longint;
      s1:string;
      x:float;
  begin
    p:=1;
    n:=1;
    lists[m].clear();
    while n<=length(s) do begin
      if (fmt[p] in ['x','y']) then begin
        while (Length(s) > n) and (s[n] = ' ')  do inc(n);
        s1:='';
        while (Length(s) >= n) and
            (s[n] in ['0'..'9', '+', '-', '.', ',', 'e', 'E']) do
        begin
          if s[n] = ',' then s1 := s1 + '.'
          else s1 := s1+s[n];
          inc(n);
        end;
        if fmt[p]='x' then x:=StrToFloat(s1, ModelFormatSettings)
        else lists[m].addPoint(x,StrToFloat(s1, ModelFormatSettings));
      end else begin
        if fmt[p]<>s[n] then raise Exception.Create('Invalid color format: '+s+#13#10'  expected: '+fmt[p]+' at position '+IntToStr(n)+' but got '+s[n]);
        n+=1;
      end;
      p+=1;if p>length(fmt) then p:=1;
    end;
  end;
const NAMEID: array[0..2] of string = ('R','G','B');
var
  i: Integer;
begin
  if str[1]='W' then begin
    SetshowSingleColors(false);
    scanFStr(0,copy(str,2,length(str)-1));
  end else begin
    SetshowSingleColors(true);
    for i:=0 to 2 do
      if NAMEID[i]=str[1] then
        scanFStr(i,copy(str,2,length(str)-1));
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

