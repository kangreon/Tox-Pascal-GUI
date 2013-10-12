//  ClientAddress.pas
//
//  Содержит класс, осуществляющий преобразование адреса пользователя из
//  бинарного представления в хеш.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ClientAddress;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  libtox, Classes, SysUtils, StringUtils;

type
  TAddressError = (aeClear, aeIncorrectSize);

  TFriendAddress = class
  private
    FDataHex: string;
    FDataBin: PByte;
    FValidAddress: Boolean;
    FError: TAddressError;
    procedure SetDataHex(const Value: string);
    procedure SetDataBin(const Value: PByte);

  public
    constructor Create; overload;
    constructor Create(Value: PByte); overload;
    constructor Create(Value: DataString); overload;

    destructor Destroy; override;

    procedure Clear;
    function Clone: TFriendAddress;
    function IsCompare(const Value: TFriendAddress): Boolean; overload;
    function IsCompare(const Value: DataString): Boolean; overload;

    property DataHex: string read FDataHex write SetDataHex;
    property DataBin: PByte read FDataBin write SetDataBin;
    property ValidAddress: Boolean read FValidAddress;
    property Error: TAddressError read FError;
  end;

  TClientId = class
  private
    FDataBin: PByte;
    FValidAddress: Boolean;
    FError: TAddressError;
    FDataHex: DataString;
    procedure Initialize;
    procedure SetDataBin(const Value: PByte);
    procedure SetDataHex(const Value: DataString);
  public
    constructor Create; overload;
    constructor Create(const Value: PByte); overload;
    constructor Create(const Value: DataString); overload;
    constructor Create(const Value: TFriendAddress); overload;
    destructor Destroy; override;

    procedure Clear;
    function Clone: TClientId;
    function IsCompare(const Value: DataString): Boolean; overload;
    function IsCompare(const Value: TClientId): Boolean; overload;

    property DataHex: DataString read FDataHex write SetDataHex;
    property DataBin: PByte read FDataBin write SetDataBin;
    property ValidAddress: Boolean read FValidAddress;
    property Error: TAddressError read FError;
  end;

function bin_to_hex_string(data: PByte; size: Integer): string;
function hex_string_to_bin(s: string): Pointer;

implementation

{ TClientAddress }

function hex_string_to_bin(s: string): Pointer;
var
  c, dc: Integer;
  sa: AnsiString;
begin
  sa := AnsiString(s);
  c := Length(s);
  dc := c div 2;
  Result := GetMemory(dc);
  HexToBin(PAnsiChar(@sa[1]), PAnsiChar(Result), dc);
end;

function bin_to_hex_string(data: PByte; size: Integer): string;
var
  sa: AnsiString;
begin
  SetLength(sa, size * 2);
  BinToHex(PAnsiChar(data), PAnsiChar(@sa[1]), size);
  Result := string(sa);
end;

constructor TFriendAddress.Create;
begin
  FDataBin := GetMemory(FRIEND_ADDRESS_SIZE);
  SetLength(FDataHex, FRIEND_ADDRESS_SIZE * 2);

  Clear;
end;

constructor TFriendAddress.Create(Value: PByte);
begin
  FDataBin := GetMemory(FRIEND_ADDRESS_SIZE);
  SetLength(FDataHex, FRIEND_ADDRESS_SIZE * 2);

  Clear;
  DataBin := Value;
end;

function TFriendAddress.Clone: TFriendAddress;
var
  Item: TFriendAddress;
begin
  Item := TFriendAddress.Create;
  Item.DataHex := DataHex;

  Result := Item;
end;

constructor TFriendAddress.Create(Value: DataString);
begin
  FDataBin := GetMemory(FRIEND_ADDRESS_SIZE);
  SetLength(FDataHex, FRIEND_ADDRESS_SIZE * 2);

  Clear;
  DataHex := Value;
end;

destructor TFriendAddress.Destroy;
begin
  FreeMemory(FDataBin);
  inherited;
end;

function TFriendAddress.IsCompare(const Value: TFriendAddress): Boolean;
begin
  Result := IsCompare(Value.DataHex);
end;

function TFriendAddress.IsCompare(const Value: DataString): Boolean;
begin
  Result := (CompareStr(UpperCase(Value), FDataHex) = 0) and FValidAddress;
end;

procedure TFriendAddress.Clear;
begin
  FError := aeClear;
  FValidAddress := False;
end;

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
begin
  Move(Source^, Destination^, Length);
end;

procedure TFriendAddress.SetDataBin(const Value: PByte);
begin
  CopyMemory(FDataBin, Value, FRIEND_ADDRESS_SIZE);
  DataHex := bin_to_hex_string(Value, FRIEND_ADDRESS_SIZE);
  if Length(DataHex) = FRIEND_ADDRESS_SIZE * 2 then
    FValidAddress := True
  else
  begin
    FValidAddress := False;
    FError := aeIncorrectSize;
  end;
end;

procedure TFriendAddress.SetDataHex(const Value: string);
var
  data: PByte;
begin
  FValidAddress := False;

  if Length(Value) = FRIEND_ADDRESS_SIZE * 2 then
  begin
    FDataHex := Value;

    data := hex_string_to_bin(Value);
    try
      CopyMemory(FDataBin, data, FRIEND_ADDRESS_SIZE);
    finally
      FValidAddress := True;
      FreeMemory(data);
    end;
  end
  else
  begin
    FValidAddress := False;
    FError := aeIncorrectSize;
  end;
end;

{ TClientId }

constructor TClientId.Create;
begin
  Initialize;
end;

constructor TClientId.Create(const Value: PByte);
begin
  Initialize;
  DataBin := Value;
end;

constructor TClientId.Create(const Value: DataString);
begin
  Initialize;
  DataHex := Value;
end;

constructor TClientId.Create(const Value: TFriendAddress);
begin
  Initialize;
  DataHex := Copy(Value.DataHex, 1, (TOX_CLIENT_ID_SIZE * 2));
end;

destructor TClientId.Destroy;
begin
  FreeMem(FDataBin);
  inherited;
end;

procedure TClientId.Clear;
begin
  FValidAddress := False;
  FError := aeClear;
end;

function TClientId.Clone: TClientId;
var
  Client: TClientId;
begin
  Client := TClientId.Create;
  Client.SetDataHex(FDataHex);

  Result := Client;
end;

procedure TClientId.Initialize;
begin
  GetMem(FDataBin, TOX_CLIENT_ID_SIZE);
  Clear;
end;

function TClientId.IsCompare(const Value: TClientId): Boolean;
begin
  Result := IsCompare(Value.DataHex);
end;

function TClientId.IsCompare(const Value: DataString): Boolean;
begin
  Result := (CompareStr(UpperCase(Value), FDataHex) = 0) and FValidAddress;
end;

procedure TClientId.SetDataBin(const Value: PByte);
begin
  FDataHex := bin_to_hex_string(Value, TOX_CLIENT_ID_SIZE);
  move(Value^, FDataBin^, TOX_CLIENT_ID_SIZE);

end;

procedure TClientId.SetDataHex(const Value: DataString);
var
  data: PByte;
begin
  FValidAddress := False;

  if Length(Value) = TOX_CLIENT_ID_SIZE * 2 then
  begin
    FDataHex := UpperCase(Value);

    data := hex_string_to_bin(FDataHex);
    try
      Move(data^, FDataBin^, TOX_CLIENT_ID_SIZE);
      FValidAddress := True;
    finally
      FreeMem(data);
    end;
  end
  else
  begin
    FValidAddress := False;
    FError := aeIncorrectSize;
  end;
end;

end.

