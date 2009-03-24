unit maingui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, diagram,
  ExtCtrls, StdCtrls,colorDiagramModels;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Panel5: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ChangeSingleColors(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CurModelChanged(Sender: TObject);
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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  modelManager.Free;
end;

procedure TForm1.CurModelChanged(Sender: TObject);
begin
  changeModel (TComponent(sender).owner as tpanel);
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
end;

procedure TForm1.ChangeSingleColors(Sender: TObject);
var i:longint;
begin
  //set showSingleColors of the model of the view on the current panel
  (((TComponent(sender).owner as tpanel).FindComponent('view') as TDiagramView).Model as TDiagramColorModel).showSingleColors:=(sender as TCheckBox).Checked;
  //update all checkboxes (including other views)
  for i:=0 to ControlCount-1 do
    if controls[i] is TPanel then
      if controls[i].FindComponent('colors')<>nil then
        (controls[i].FindComponent('colors') as TCheckBox).Checked:=(((controls[i] as tpanel).FindComponent('view') as TDiagramView).Model as TDiagramColorModel).showSingleColors;
end;

initialization
  {$I maingui.lrs}

end.

