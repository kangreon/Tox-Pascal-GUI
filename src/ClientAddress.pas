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
    destructor Destroy; override;

    procedure Clear;

    property DataHex: DataString read FDataHex write SetDataHex;
    property DataBin: PByte read FDataBin write SetDataBin;
    property ValidAddress: Boolean read FValidAddress;
    property Error: TAddressError read FError;
  end;

  TClientAddress = class
  private
    FDataHex: string;
    FDataBin: PByte;
    FValidAddress: Boolean;
    FError: TAddressError;
    procedure SetDataHex(const Value: string);
    procedure SetDataBin(const Value: PByte);

  public
    constructor Create; overload;
    constructor Create(data: PByte); overload;

    destructor Destroy; override;

    procedure Clear;

    property DataHex: string read FDataHex write SetDataHex;
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

procedure TClientAddress.Clear;
begin
  FError := aeClear;
  FValidAddress := False;
end;

constructor TClientAddress.Create;
begin
  FDataBin := GetMemory(FRIEND_ADDRESS_SIZE);
  SetLength(FDataHex, FRIEND_ADDRESS_SIZE * 2);

  Clear;
end;

constructor TClientAddress.Create(data: PByte);
begin
  FDataBin := GetMemory(FRIEND_ADDRESS_SIZE);
  SetLength(FDataHex, FRIEND_ADDRESS_SIZE * 2);

  Clear;
  DataBin := data;
end;

destructor TClientAddress.Destroy;
begin
  FreeMemory(FDataBin);
  inherited;
end;

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
begin
  Move(Source^, Destination^, Length);
end;

procedure TClientAddress.SetDataBin(const Value: PByte);
begin
  //Move(Value^, FDataBin^, FRIEND_ADDRESS_SIZE);
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

procedure TClientAddress.SetDataHex(const Value: string);
var
  data: PByte;
begin
  if Length(Value) = FRIEND_ADDRESS_SIZE * 2 then
  begin
    FDataHex := Value;

    data := hex_string_to_bin(Value);
    try
      //Move(FDataBin^, data^, FRIEND_ADDRESS_SIZE);
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

procedure TClientId.Clear;
begin
  FValidAddress := False;
  FError := aeClear;
end;

constructor TClientId.Create(const Value: DataString);
begin
  Initialize;
  DataHex := Value;
end;

destructor TClientId.Destroy;
begin
  FreeMem(FDataBin);
  inherited;
end;

procedure TClientId.Initialize;
begin
  GetMem(FDataBin, TOX_CLIENT_ID_SIZE);
  Clear;
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
  if Length(Value) = TOX_CLIENT_ID_SIZE * 2 then
  begin
    FDataHex := Value;

    data := hex_string_to_bin(Value);
    try
      Move(data^, FDataBin^, TOX_CLIENT_ID_SIZE);
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

