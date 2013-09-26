//  StringUtils
//
//  Этот файл содержит определение нового типа строки и содержит функции
//  кодирования строк.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit StringUtils;

interface
  {$I tox.inc}

type
  DataString = {$IFDEF NEW_DELPHI}string{$ELSE}WideString{$ENDIF};

function GetTextFromUTF8Byte(data: PByte; data_size: Integer): DataString;
function GetUtf8Text(text: DataString; out data_size: Integer): PByte;

implementation

// Конвертирует строку в кодировку UTF8
function GetUtf8Text(text: DataString; out data_size: Integer): PByte;
var
  {$IFDEF NEW_DELPHI}
    {$IFDEF FPC}
      data_text: UTF8String;
    {$ELSE}
      data_text: RawByteString;
    {$ENDIF}
  {$ELSE}
  data_text: UTF8String;
  {$ENDIF}
  data_length: Integer;
  data: PByte;
begin
  {$IFDEF FPC}
    data_text := text;
  {$ELSE}
    data_text := UTF8Encode(text);
  {$ENDIF}

  data_length := Length(data_text) + 1;

  data := GetMemory(data_length);
  FillChar(data^, data_length, 0);
  move(data_text[1], data^, data_length - 1);

  Result := data;
  data_size := data_length;
end;

function GetTextFromUTF8Byte(data: PByte; data_size: Integer): DataString;
var
  {$IFDEF NEW_DELPHI}
    {$IFDEF FPC}
      data_text: UTF8String;
    {$ELSE}
      data_text: RawByteString;
    {$ENDIF}
  {$ELSE}
  data_text: UTF8String;
  {$ENDIF}
begin
  if data_size > 1 then
  begin
    SetLength(data_text, data_size - 1);
    move(data^, data_text[1], data_size - 1);
    {$IFDEF NEW_DELPHI}
      Result := UTF8ToString(data_text);
    {$ELSE}
    {$IFDEF FPC}
      Result := data_text;
    {$ELSE}
      Result := UTF8Decode(data_text);
    {$ENDIF}
    {$ENDIF}
  end;
end;

end.
