unit MessageHeaderStyle;

interface
  {$I tox.inc}

uses
  Graphics;

type
  TMHStyle = class
  public const
    ControlHeight: Integer = 59;
    BackgroundColor: TColor = $EFEFEF;
    ButtonVideoMarginRight: Integer = 10;
    ButtonAudioMarginRight: Integer = 10;
    ButtonAudioMarginLeft: Integer = 10;

    IconMarginLeft: Integer = 10;

    UserNameFontName: string = 'Fira Sans';
    UserNameSize: Integer = 10;
    UserNameStyle: TFontStyles = [fsBold];
    UserNameColor: TColor = $424040;

    StatusFontName: string = 'Fira Sans';
    StatusSize: Integer = 9;
    StatusStyle: TFontStyles = [];
    StatusColor: TColor = $5c5a5a;

    UserNameMarginLeft: Integer = 7;
  end;
implementation

end.
