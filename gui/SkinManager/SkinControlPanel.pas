unit SkinControlPanel;

interface

uses
  SkinTypes, Graphics;

type
  TSkinControlPanel = class
  private
    FBackColor: TColor;
    FHeight: Integer;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FImgNewFriend: TStateImage;
    FImgNewGroup: TStateImage;
    FImgSettings: TStateImage;
  public
    // Цвет фона компонента
    property BackColor: TColor read FBackColor write FBackColor;

    // Высота компонента
    property Height: Integer read FHeight write FHeight;

    // Расстояние от левого края до первого элемента
    property MarginLeft: Integer read FMarginLeft write FMarginLeft;
    // Расстояние от последнего элемента до правого края компонента
    property MarginRight: Integer read FMarginRight write FMarginRight;

    property ImgNewFriend: TStateImage read FImgNewFriend write FImgNewFriend;
    property ImgNewGroup: TStateImage read FImgNewGroup write FImgNewGroup;
    property ImgSettings: TStateImage read FImgSettings write FImgSettings;
  end;

implementation

end.
