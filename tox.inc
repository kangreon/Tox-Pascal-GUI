{$IFDEF FPC}
  {$MODE Delphi}
  {$CODEPAGE UTF8}
{$ELSE}
  {$IF CompilerVersion >= 20}
    {$DEFINE NEW_DELPHI}
  {$ELSE}
    {$DEFINE OLD_DELPHI}
  {$IFEND}
{$ENDIF}