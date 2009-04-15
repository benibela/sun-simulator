unit maingui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, diagram,
  ExtCtrls, StdCtrls,colorDiagramModels,monitorControl, Menus,XMLCfg,LCLType,registry,menuManager,ptranslateutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    btnSave: TButton;
    autopreview: TCheckBox;
    btnProfileReset: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Panel1: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    freeControl: TTimer;
    TrayIcon1: TTrayIcon;
    procedure btnProfileResetClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ChangeSingleColors(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ComboBox1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ComboBox1Select(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CurModelChanged(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure freeControlStartTimer(Sender: TObject);
    procedure freeControlTimer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure modelManagerModelModified(Sender: TObject);
    procedure PaintBox1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox2Resize(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure PopupMenu1Close(Sender: TObject);
    procedure ProfileMenuClicked(sender: TObject; selected: longint);
    procedure RemovePanel(Sender: TObject);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure viewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure viewMouseLeave(Sender: TObject);
    procedure viewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure viewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    procedure changeModel(viewPanel:TPanel);
    procedure lazyModelModified;
  public
    { public declarations }
    mouseIsDown,modelWasModified:boolean;
    curModel: longint;
    config: TXMLConfig;
    modelManager:TModelColorModelManager;
    profileData: TStringList;
    profileMenu: TListMenu;
  end;

var
  Form1: TForm1; 
  tr: TPascalTranslator = nil;

resourcestring
  rsModified = 'modified';
  rsDayColorGrad = '* Day color gradient';
  rsDayLightness = '* Day lightness gradient';
  rsDayYearColor = '* Day/Year color gradient';
  rsDayYearLight = '* Day/Year lightness gradient';
  rsWindowsDefau = '* Windows default';
  rsShow3Colors = 'Show 3 Colors';
  rsGamma = 'Gamma';
  rsBrightness = 'Brightness';
  rsContrast = 'Contrast';
  rsYearDaySetti = 'Day settings'#13#10'Year settings';
implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var model:longint;
    i: Integer;
    r: TRegistry;
begin
  initGlobalTranslation(ExtractFilePath(ParamStr(0)),'sunSimulator');
  initUnitTranslation('maingui',tr);
  tr.translate(self);
  config:=TXMLConfig.Create(self);
  config.Filename:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'sunSimulatorConfig.xml';
  autopreview.Checked:=config.GetValue('UI/autoPreview',true);
  CheckBox2.Checked:=config.GetValue('OS/autoStart',true);
  if CheckBox2.Checked then begin
    r:=TRegistry.Create;
    r.RootKey:=HKEY_CURRENT_USER;
    r.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',true);
    r.WriteString('SunSimulatorAutoStart','"'+ParamStr(0)+'" --autostart');
    r.free;
  end;
  profileData:=TStringList.Create;
  modelManager:=TModelColorModelManager.create;
  modelManager.OnModelModified:=@modelManagerModelModified;
  profileMenu:=TListMenu.create(MenuItem5);
  profileMenu.OnItemClick:=@ProfileMenuClicked;
  btnProfileResetClick(nil);
  model:=config.GetValue('models/curModel',0);
  if model>profileData.Count-1 then model:=profileData.Count-1;
  if model<0 then model:=0;
  ComboBox1.ItemIndex:=model;
  ComboBox1Select(nil);
  TrayIcon1.Icon:=Application.Icon;
  TrayIcon1.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  config.SetValue('UI/autoPreview',autopreview.Checked);
  modelManager.Free;
  profileData.Free;
  config.free;

end;

procedure TForm1.CurModelChanged(Sender: TObject);
begin
  changeModel (TComponent(sender).owner as tpanel);
end;

procedure TForm1.FormResize(Sender: TObject);
begin

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  OnShow:=nil;
  Button1.Click;
end;

procedure TForm1.FormWindowStateChange(Sender: TObject);
begin
  if WindowState=wsMinimized then Visible:=false;
end;

procedure TForm1.freeControlStartTimer(Sender: TObject);
begin
end;

procedure TForm1.freeControlTimer(Sender: TObject);
begin
  if TComponent(sender).tag=0 then exit;
  TControl(TControl(sender).tag).Free;
  TComponent(sender).tag:=0;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  WindowState:=wsNormal;
  Show;
  BringToFront;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin

end;

procedure TForm1.modelManagerModelModified(Sender: TObject);
begin
  if not autopreview.checked then Timer1Timer(timer1);
  if ComboBox1.Text ='' then exit;
  if mouseIsDown then modelWasModified:=true
  else lazyModelModified;
end;

procedure TForm1.PaintBox1Click(Sender: TObject);
begin
  PaintBox1.Tag:=(PaintBox1.Tag+1) mod 2;
  modelManager.mapper.drawTestPattern(PaintBox1.Tag, PaintBox1.Width,PaintBox1.Height,PaintBox1.Canvas);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  modelManager.mapper.drawTestPattern(PaintBox1.Tag, PaintBox1.Width,PaintBox1.Height,PaintBox1.Canvas);
end;

procedure TForm1.PaintBox2Paint(Sender: TObject);
begin
  modelManager.mapper.draw(PaintBox2.Width,PaintBox2.Height,PaintBox2.Canvas);
end;

procedure TForm1.PaintBox2Resize(Sender: TObject);
begin

end;

procedure TForm1.Panel1Resize(Sender: TObject);
var s:longint;
begin
  //center x,y with max size and quadratic
  s:=(Panel1.Width-30) div 2;
  if Panel1.Height-20 < s then s:=Panel1.Height-20;
  PaintBox1.Height:=s;
  PaintBox2.Height:=s;
  PaintBox1.Width:=PaintBox1.Height;
  PaintBox2.Width:=PaintBox2.Height;
  PaintBox1.Left:=10+(Panel1.Width-2*s-30) div 2;
  PaintBox2.Left:=PaintBox1.Left+PaintBox1.Width+10;
  PaintBox1.Top:=(Panel1.Height-PaintBox1.Height) div 2;
  PaintBox2.Top:=PaintBox1.top;
end;

procedure TForm1.PopupMenu1Close(Sender: TObject);
begin
  if WindowState=wsMinimized then Visible:=false;
end;

procedure TForm1.ProfileMenuClicked(sender: TObject; selected: longint);
begin
  ComboBox1.ItemIndex:=selected;
  ComboBox1Select(nil);
end;

procedure TForm1.RemovePanel(Sender: TObject);
begin
 TControl(TComponent(sender).Owner).visible:=false;
 freeControl.Tag:=longint(TComponent(sender).Owner);
 freeControl.Enabled:=true;//cann't free parent in event handler
end;

procedure TForm1.Timer1StartTimer(Sender: TObject);
begin
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  modelManager.setTime(now);
  monitorControlInstance.setTo(modelManager.mapper);
  modelManager.mapper.draw(PaintBox2.Width,PaintBox2.Height,PaintBox2.Canvas);
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin

end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  MenuItem2Click(nil);
end;

procedure TForm1.viewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mouseIsDown:=true;
  modelWasModified:=false;
end;

procedure TForm1.viewMouseLeave(Sender: TObject);
begin
  Timer1.Enabled:=true;
  if autopreview.Checked then Timer1Timer(self);
end;

procedure TForm1.viewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var view:TDiagramView;
    panel: TPanel;
begin
  view:=sender as TDiagramView;
  if (x<view.Drawer.valueAreaX) or (x>view.Drawer.ValueAreaRight) or (not autopreview.Checked) then begin
    Timer1.Enabled:=true;
    exit;
  end;
  Timer1.Enabled:=false;
  panel:=TComponent(sender).owner as tpanel;
  if (panel.FindComponent('frequency') as TComboBox).ItemIndex=0 then
    modelManager.setPreviewTime(now,view.Drawer.posToDataX(x),cfDay)
   else
    modelManager.setPreviewTime(now,view.Drawer.posToDataX(x),cfYear);
  monitorControlInstance.setTo(modelManager.mapper);
  modelManager.mapper.draw(PaintBox2.Width,PaintBox2.Height,PaintBox2.Canvas);
end;

procedure TForm1.viewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mouseIsDown:=false;
  if modelWasModified then begin
    lazyModelModified;
    modelWasModified:=false;
  end;
end;

procedure TForm1.changeModel(viewPanel: TPanel);
var freq: TModelColorFrequency;
    role: TModelColorRole;
begin
  if (viewPanel.FindComponent('gamma') as TRadioButton).Checked then role:=crGamma
  else if (viewPanel.FindComponent('contrast') as TRadioButton).Checked then role:=crContrast
  else if (viewPanel.FindComponent('brightness') as TRadioButton).Checked then role:=crBrightness
  else exit;
  freq:=TModelColorFrequency((viewPanel.FindComponent('frequency') as TComboBox).ItemIndex);
  (viewPanel.FindComponent('colors') as TCheckBox).Checked:=
    modelManager.selectModel(viewPanel.FindComponent('view') as TDiagramView,freq,role).showSingleColors;
end;

procedure TForm1.lazyModelModified;
var t:string;
  i: Integer;
begin
  if ComboBox1.Text[1]='*' then begin
    t:=copy(ComboBox1.Text, 2, length(ComboBox1.Text)-1)+' ('+rsModified;
    if ComboBox1.Items.IndexOf(t+')')=-1 then ComboBox1.Items.Add(t+ ')')
    else for i:=2 to ComboBox1.Items.count+2 do
      if ComboBox1.Items.IndexOf(t+',' +IntToStr(i)+')')=-1 then   begin
        ComboBox1.Items.Add(t+',' +IntToStr(i)+')');
        break;
      end;
    ComboBox1.ItemIndex:=ComboBox1.Items.Count-1;
    profileData.Add(modelManager.saveToString);
  end else profileData[ComboBox1.ItemIndex]:=modelManager.saveToString;
  curModel:=ComboBox1.ItemIndex;
  profileMenu.update(ComboBox1.Items);
  profileMenu.CheckedIndex:=curModel;
end;
procedure TForm1.Button1Click(Sender: TObject);
var nvp, cp: tpanel;
    view: TDiagramView;
begin
  //The layout is really tricky
  nvp:=TPanel.Create(self);
  nvp.top:=height;
  nvp.Height:=200;
  nvp.Align:=alTop;
  view:=TDiagramView.create(nvp);
  view.Align:=alClient;
  view.Drawer.LineStyle:=lsCubicSpline;
  view.Drawer.PointStyle:=psCircle;
  view.Drawer.legend.visible:=false;
  view.Drawer.LeftAxis.gridLinePen.Color:=$222222;
  view.Drawer.TopAxis.Visible:=true;
  view.Drawer.RightAxis.Visible:=true;
  view.Drawer.DataBackColor:=$111111;
  view.name:='view';
  view.AllowedEditActions:=[eaAddPoints,eaDeletePoints,eaMovePoints];
  view.Parent:=nvp;
  nvp.Parent:=self;
  cp:=TPanel.Create(self);
  cp.Height:=22;
  cp.Align:=alTop;
  cp.Parent:=nvp;
  //create in reversed order
  with TCheckBox.Create(nvp) do begin
    visible:=false;
    Caption:=rsShow3Colors;
    parent:=cp;
    left:=5000;
    Align:=alLeft;
    BorderSpacing.Right:=20;
    name:='colors';
    OnClick:=@ChangeSingleColors;
    checked:=modelManager.selectModel(view,cfDay,crGamma).showSingleColors;
    visible:=true;
  end;


  with TRadioButton.Create(nvp) do begin
    Caption:=rsGamma;
    parent:=cp;
    left:=5000;
    Align:=alLeft;
    name:='gamma';
    Checked:=true;
    OnClick:=@CurModelChanged;
  end;
  with TRadioButton.Create(nvp) do begin
    Caption:=rsBrightness;
    parent:=cp;
    left:=5000;
    Align:=alLeft;
    name:='brightness';
    OnClick:=@CurModelChanged;
  end;

  with TRadioButton.Create(nvp) do begin
    Caption:=rsContrast;
    parent:=cp;
    left:=5000;
    Align:=alLeft;
    name:='contrast';
    OnClick:=@CurModelChanged;
  end;
  with TComboBox.Create(nvp) do begin
    items.text:=rsYearDaySetti;
    Text:=items[0];
    parent:=cp;
    left:=5000;
    Align:=alLeft;
    BorderSpacing.Left:=20;
    OnSelect:=@CurModelChanged;
    name:='frequency';
  end;
  with TButton.create(nvp) do begin
    caption:='X';
    parent:=cp;
    width:=20;
    align:=alRight;
    OnClick:=@RemovePanel;
  end;
  view.OnMouseDown:=@viewMouseDown;
  view.OnMouseUp:=@viewMouseUp;
  view.OnMouseMove:=@viewMouseMove;
  view.onMouseLeave:=@viewMouseLeave;

  with TSplitter.Create(nvp) do begin
    top:=nvp.top+nvp.height+10;
    Align:=alTop;
    Parent:=self;
    AutoSnap:=true;
  end;
end;

procedure TForm1.btnProfileResetClick(Sender: TObject);
var
  curIndex,profCount: integer;
  i: Integer;
begin
  curIndex:=ComboBox1.ItemIndex;
  ComboBox1.Items.Clear;
  profileData.Clear;
  profCount:=config.GetValue('profiles/count',0);
  if profCount=0 then begin
    //default profiles
    profCount:=5;
    ComboBox1.items.Add(rsDayColorGrad);
    profileData.Add('D-G-R-(0,00000;0,87692)-(4,51447;1,49615)-(12,03859;0,93077)-(20,18006;1,44231)-(24,00000;0,87692)'#13#10'D-G-G-(0,00000;0,87692)-(4,51447;1,49615)-(12,03859;0,93077)-(20,18006;1,44231)-(24,00000;0,87692)'#13#10+
                    'D-G-B-(0,00000;0,87692)-(4,20579;0,93077)-(12,03859;1,60385)-(20,29582;0,85000)-(24,00000;0,87692)'#13#10'D-B-R-(0,00000;-0,36923)-(5,13183;-0,06154)-(12,07717;-0,06154)-(19,06109;-0,06154)-(24,00000;-0,36923)'#13#10+
                    'D-B-G-(0,00000;-0,36923)-(5,13183;-0,12308)-(12,07717;-0,06154)-(19,02251;-0,13846)-(24,00000;-0,36923)'#13#10'D-B-B-(0,00000;-0,36923)-(5,01608;-0,26154)-(12,03859;0,18462)-(18,98392;-0,26154)-(24,00000;-0,36923)');
    ComboBox1.items.Add(rsDayLightness);
    profileData.Add('D-G-W-(0,00000;0,87692)-(12,00000;1,22692)-(24,00000;0,87692)'#13#10'D-B-W-(0,00000;-0,36923)-(5,01608;-0,26154)-(12,03859;0,18462)-(18,98392;-0,26154)-(24,00000;-0,36923)');
    ComboBox1.items.Add(rsDayYearColor);
    profileData.Add('D-G-R-(0,00000;0,87692)-(4,51447;1,49615)-(12,03859;0,93077)-(20,18006;1,44231)-(24,00000;0,87692)'#13#10'D-G-G-(0,00000;0,87692)-(4,51447;1,49615)-(12,03859;0,93077)-(20,18006;1,44231)-(24,00000;0,87692)'#13#10+
    'D-G-B-(0,00000;0,87692)-(4,20579;0,93077)-(12,03859;1,60385)-(20,29582;0,85000)-(24,00000;0,87692)'#13#10'D-C-R-(0,00000;1,00000)-(24,00000;1,00000)'#13#10'D-C-G-(0,00000;1,00000)-(24,00000;1,00000)'#13#10'D-C-B-(0,00000;1,00000)-(24,00000;1,00000)'#13#10+
    'D-B-R-(0,00000;-0,36923)-(5,13183;-0,06154)-(12,07717;-0,06154)-(19,06109;-0,06154)-(24,00000;-0,36923)'#13#10'D-B-G-(0,00000;-0,36923)-(5,13183;-0,12308)-(12,07717;-0,06154)-(19,02251;-0,13846)-(24,00000;-0,36923)'#13#10+
    'D-B-B-(0,00000;-0,36923)-(5,01608;-0,26154)-(12,03859;0,18462)-(18,98392;-0,26154)-(24,00000;-0,36923)'#13#10'Y-G-R-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-G-G-(0,00000;1,00000)-(365,00000;1,00000)'#13#10+
    'Y-G-B-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-C-R-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-C-G-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-C-B-(0,00000;1,00000)-(365,00000;1,00000)'#13#10+
    'Y-B-W-(0,00000;-0,20000)-(183,08682;0,21538)-(365,00000;-0,20000)');
    ComboBox1.items.Add(rsDayYearLight);
    profileData.Add('D-G-W-(0,00000;0,87692)-(12,00000;1,22692)-(24,00000;0,87692)'#13#10'D-C-R-(0,00000;1,00000)-(24,00000;1,00000)'#13#10'D-C-G-(0,00000;1,00000)-(24,00000;1,00000)'#13#10'D-C-B-(0,00000;1,00000)-(24,00000;1,00000)'#13#10+
    'D-B-W-(0,00000;-0,36923)-(5,01608;-0,26154)-(12,03859;0,18462)-(18,98392;-0,26154)-(24,00000;-0,36923)'#13#10'Y-G-R-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-G-G-(0,00000;1,00000)-(365,00000;1,00000)'#13#10+
    'Y-G-B-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-C-R-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-C-G-(0,00000;1,00000)-(365,00000;1,00000)'#13#10'Y-C-B-(0,00000;1,00000)-(365,00000;1,00000)'#13#10+
    'Y-B-W-(0,00000;-0,20000)-(186,60772;0,16923)-(365,00000;-0,20000)');
    ComboBox1.items.Add(rsWindowsDefau);
    profileData.Add('');
    exit;
  end;
  for i:=1 to profCount do begin
    ComboBox1.Items.Add(config.GetValue('profiles/profile'+IntToStr(i)+'/title','unnamed'));
    profileData.Add(config.GetValue('profiles/profile'+IntToStr(i)+'/data/value',''));
    //  showmessage(profileData[profileData.Count-1]);
  end;
  if Visible then begin
    if curIndex>=ComboBox1.Items.Count then curIndex:=ComboBox1.Items.Count-1;
    ComboBox1.ItemIndex:=curIndex;
    ComboBox1Select(nil);
  end else begin
    profileMenu.update(ComboBox1.Items);
    //profileMenu.CheckedIndex:=ComboBox1.ItemIndex;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  timer1.Enabled:=false;
  monitorControlInstance.reset;
  CheckBox1.Checked:=false;
  Close;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  i: Integer;
begin
  config.SetValue('profiles/count',profileData.Count);
  for i:=1 to profileData.Count do begin
    config.SetValue('profiles/profile'+IntToStr(i)+'/title',ComboBox1.Items[i-1]);
    config.SetValue('profiles/profile'+IntToStr(i)+'/data/value',profileData[i-1]);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  WindowState:=wsMinimized;
end;

procedure TForm1.ChangeSingleColors(Sender: TObject);
var i:longint;
begin
  //set showSingleColors of the model of the view on the current panel
  modelManager.getSelectedModel((TComponent(sender).owner as tpanel).FindComponent('view') as TDiagramView).showSingleColors:=(sender as TCheckBox).Checked;
  //update all checkboxes (including other views)
  for i:=0 to ControlCount-1 do
    if controls[i] is TPanel then
      if controls[i].FindComponent('colors')<>nil then
        (controls[i].FindComponent('colors') as TCheckBox).Checked:=modelManager.getSelectedModel((controls[i] as tpanel).FindComponent('view') as TDiagramView).showSingleColors;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
var
  r: TRegistry;
begin
  r:=TRegistry.Create;
  r.RootKey:=HKEY_CURRENT_USER;
  r.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',true);
  if CheckBox2.Checked then r.WriteString('SunSimulatorAutoStart','"'+ParamStr(0)+'" --autostart')
  else r.DeleteValue('SunSimulatorAutoStart');
  r.free;
  config.SetValue('OS/autoStart',CheckBox2.Checked);
end;

procedure TForm1.ComboBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin   {
  if copy(ComboBox1.Items[Index],1,1)='*' then begin
    ComboBox1.Canvas.Font.Style:=[fsBold];
    ComboBox1.Canvas.TextRect(ARect,ARect.left,ARect.top,copy(ComboBox1.Items[Index],2,length(ComboBox1.Items[Index])))
  end else begin
    ComboBox1.Canvas.Font.Style:=[];
    ComboBox1.Canvas.TextRect(ARect,ARect.left,ARect.top,ComboBox1.Items[Index]);
  end;}
end;

procedure TForm1.ComboBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=vk_return then
    if (curModel>=0) and (curModel< ComboBox1.Items.Count) then
      if copy(ComboBox1.Items[curModel],1,1)='*' then ComboBox1.Text:=ComboBox1.Items[curModel]
      else if copy(ComboBox1.Text,1,1)='*' then ComboBox1.Items[curModel]:=copy(ComboBox1.Text,2,length(ComboBox1.Text)-1)
      else ComboBox1.Items[curModel]:=ComboBox1.Text;

end;

procedure TForm1.ComboBox1Select(Sender: TObject);
var i:longint;
begin
  curModel:=ComboBox1.ItemIndex;
  modelManager.loadFromString(profileData[curModel]);
  //update all checkboxes
  for i:=0 to ControlCount-1 do
    if controls[i] is TPanel then
      if controls[i].FindComponent('colors')<>nil then
        (controls[i].FindComponent('colors') as TCheckBox).Checked:=modelManager.getSelectedModel((controls[i] as tpanel).FindComponent('view') as TDiagramView).showSingleColors;
  Timer1Timer(nil);
  profileMenu.update(ComboBox1.Items);
  profileMenu.CheckedIndex:=ComboBox1.ItemIndex;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CheckBox1.Checked then begin
    btnSave.Click;
    config.SetValue('models/curModel',curModel);
  end;
end;

initialization
  {$I maingui.lrs}

end.

