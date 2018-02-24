unit menuManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Menus;

type

{ TListMenu }

TSelectEvent = procedure (sender:TObject; selected: longint) of object;
TListMenu=class(TObject)
private
  FCheckedIndex: longint;
  FMenu: TMenuItem;
  FOnSelect: TSelectEvent;
  procedure ItemClicked(Sender:TObject);
  procedure SetCheckedIndex(const AValue: longint);
  procedure SetOnSelect(const AValue: TSelectEvent);
public
  constructor create(parentMenuItem: TMenuItem);
  procedure update(list: TStrings);
  property OnItemClick:TSelectEvent read FOnSelect write SetOnSelect;
  property CheckedIndex: longint read FCheckedIndex write SetCheckedIndex;
end;
//procedure stringsToMenu(s: TStrings; on checkedIndex: integer;

implementation

{ TListMenu }

procedure TListMenu.ItemClicked(Sender: TObject);
begin
  if assigned(FOnSelect) then
    FOnSelect(self,TMenuItem(sender).tag);
end;

procedure TListMenu.SetCheckedIndex(const AValue: longint);
var i:longint;
begin
  if FCheckedIndex=AValue then exit;
  if FCheckedIndex>=FMenu.Count then exit;
  FCheckedIndex:=AValue;
  for i:=0 to FMenu.Count-1 do
    FMenu[i].Checked:=false;
  if FCheckedIndex>=0 then FMenu[FCheckedIndex].Checked:=true;
end;

procedure TListMenu.SetOnSelect(const AValue: TSelectEvent);
begin
  if FOnSelect=AValue then exit;
  FOnSelect:=AValue;
end;

constructor TListMenu.create(parentMenuItem: TMenuItem);
begin
  FMenu:=parentMenuItem;
end;

procedure TListMenu.update(list: TStrings);
var
  i: Integer;
  diff:boolean;
  mi:TMenuItem;
begin
  if FMenu.Count=list.count then begin
    diff:=false;
    for i:=0 to list.count-1 do
      if FMenu[i].Caption<>list[i] then diff:=true;
    if not diff then exit;
  end;
  FMenu.Clear;
  if FCheckedIndex>=list.count then FCheckedIndex:=-1;
  for i:=0 to list.Count-1 do begin
    mi:=TMenuItem.Create(FMenu);
    FMenu.Add(mi);
    mi.Caption:=list[i];
    mi.OnClick:=@ItemClicked;
    mi.Tag:=i;
    if i=FCheckedIndex then mi.Checked:=true;
  end;
end;

end.

