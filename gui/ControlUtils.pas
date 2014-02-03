//  ControlUtils.pas
//
//  Содержит функции для совместимости графической библиотеки Lazarus и Delphi
//
//  The MIT License (MIT)
//
//  Copyright (c) 2014 Dmitry
//
unit ControlUtils;

interface

uses
  Controls;

procedure SetMargins(AControl: TControl; Left, Top, Right, Bottom: Integer);
procedure SetMarginsDefault(AContol: TControl);

implementation

{ *  Устанавливает отступы для компонента AControl в Delphi и в Lazarus
  * }
procedure SetMargins(AControl: TControl; Left, Top, Right, Bottom: Integer);
begin
  {$IFDEF FPC}
    AControl.BorderSpacing.Left := Left;
    AControl.BorderSpacing.Top := Top;
    AControl.BorderSpacing.Right := Right;
    AControl.BorderSpacing.Bottom := Bottom;
  {$ELSE}
    AControl.Margins.Left := Left;
    AControl.Margins.Top := Top;
    AControl.Margins.Right  := Right;
    AControl.Margins.Bottom := Bottom;

    AControl.AlignWithMargins := True;
  {$ENDIF}
end;

{ *  Устанавливает отступ со всех сторон равный 3
  * }
procedure SetMarginsDefault(AContol: TControl);
begin
  SetMargins(AContol, 3, 3, 3, 3);
end;

end.
