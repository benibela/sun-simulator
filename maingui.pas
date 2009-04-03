unit maingui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, diagram,
  ExtCtrls, StdCtrls,colorDiagramModels,monitorControl, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    autopreview: TCheckBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ChangeSingleColors(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CurModelChanged(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure modelManagerModelModified(Sender: TObject);
    procedure PaintBox1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox2Resize(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure viewMouseLeave(Sender: TObject);
    procedure viewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    procedure changeModel(viewPanel:TPanel);
  public
    { public declarations }
    modelManager:TModelColorModelManager;
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  modelManager:=TModelColorModelManager.create;
  modelManager.OnModelModified:=@modelManagerModelModified;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  modelManager.Free;
end;

procedure TForm1.CurModelChanged(Sender: TObject);
begin
  changeModel (TComponent(sender).owner as tpanel);
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

procedure TForm1.modelManagerModelModified(Sender: TObject);
begin
  if not autopreview.checked then Timer1Timer(timer1);
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
begin
  PaintBox1.Width:=PaintBox1.Height;
  PaintBox2.Width:=PaintBox2.Height;
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

procedure TForm1.Button1Click(Sender: TObject);
var nvp, cp: tpanel;
    view: TDiagramView;
begin
  nvp:=TPanel.Create(self);
  nvp.Height:=200;
  nvp.Align:=alBottom;
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
  with TSplitter.Create(self) do begin
    Align:=alBottom;
    Parent:=self;
    AutoSnap:=true;
    top:=0;
  end;
  cp:=TPanel.Create(self);
  cp.Height:=22;
  cp.Align:=alTop;
  cp.Parent:=nvp;
  //create in reversed order
  with TComboBox.Create(nvp) do begin
    items.text:='Day settings'#13#10'Year settings';
    Text:=items[0];
    parent:=cp;
    Align:=alLeft;
    BorderSpacing.Left:=20;
    OnSelect:=@CurModelChanged;
    name:='frequency';
  end;
  with TRadioButton.Create(nvp) do begin
    Caption:='Contrast';
    parent:=cp;
    Align:=alLeft;
    name:='contrast';
    OnClick:=@CurModelChanged;
  end;
  with TRadioButton.Create(nvp) do begin
    Caption:='Brightness';
    parent:=cp;
    Align:=alLeft;
    name:='brightness';
    OnClick:=@CurModelChanged;
  end;
  with TRadioButton.Create(nvp) do begin
    Caption:='Gamma';
    parent:=cp;
    Align:=alLeft;
    name:='gamma';
    Checked:=true;
    OnClick:=@CurModelChanged;
  end;
  with TCheckBox.Create(nvp) do begin
    Caption:='Show 3 Colors';
    parent:=cp;
    Align:=alLeft;
    BorderSpacing.Right:=20;
    name:='colors';
    OnClick:=@ChangeSingleColors;
    checked:=modelManager.selectModel(view,cfDay,crGamma).showSingleColors;
  end;

  view.OnMouseMove:=@viewMouseMove;
  view.onMouseLeave:=@viewMouseLeave;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  timer1.Enabled:=false;
  monitorControlInstance.reset;
  Close;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowMessage(modelManager.saveToString);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin

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

initialization
  {$I maingui.lrs}

end.

