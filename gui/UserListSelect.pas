// UserListSelect.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit UserListSelect;

interface

uses
  StringUtils, Classes;

type
  TListType = (ltAll, ltOnline, ltFriend);

  TUserListSelect = class
  private
    FActiveItem: TListType;
    FListValue: array [TListType] of DataString;
    FOnSelectItem: TNotifyEvent;

    procedure UpdateText;
    function GetCount: Integer;
  public
    constructor Create(ActiveItem: TListType);
    destructor Destroy; override;

    procedure SelectList(Index: Integer); overload;
    procedure SelectList(Item: TListType); overload;

    function GetActiveText: string;
    function GetText(Index: Integer): string; overload;
    function GetText(Item: TListType): string; overload;

    property ActiveItem: TListType read FActiveItem;
    property Count: Integer read GetCount;

    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  end;


implementation

{ TUserListSelect }

constructor TUserListSelect.Create(ActiveItem: TListType);
begin
  FActiveItem := ActiveItem;
  UpdateText;
end;

destructor TUserListSelect.Destroy;
begin

  inherited;
end;

function TUserListSelect.GetActiveText: string;
begin
  Result := FListValue[FActiveItem];
end;

function TUserListSelect.GetCount: Integer;
begin
  Result := Length(FListValue);
end;

function TUserListSelect.GetText(Item: TListType): string;
begin
  Result := FListValue[Item];
end;

procedure TUserListSelect.SelectList(Index: Integer);
begin
  SelectList(TListType(Index));
end;

procedure TUserListSelect.SelectList(Item: TListType);
begin
  if FActiveItem <> Item then
  begin
    FActiveItem := Item;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self);
  end;
end;

function TUserListSelect.GetText(Index: Integer): string;
begin
  Result := GetText(TListType(Index));
end;

procedure TUserListSelect.UpdateText;
begin
  FListValue[ltAll] := {$IFDEF FPC}UTF8Encode{$ENDIF}('Все');
  FListValue[ltOnline] := {$IFDEF FPC}UTF8Encode{$ENDIF}('Только в сети');
  FListValue[ltFriend] := {$IFDEF FPC}UTF8Encode{$ENDIF}('Только друзья');
end;

end.
