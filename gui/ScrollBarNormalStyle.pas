//  ScrollBarNormalStyle.pas
//
//  Описывает постоянные значения GUI для виджета полосы прокрутки
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ScrollBarNormalStyle;

interface
  {$I tox.inc}

uses
  Graphics;

type
  TSBNStyle = class
  public const
    //  Минимальный размер:
    //    - два пикселя по бокам - граница
    //    - по два пиксела с двух сторон отводится на закругление (4)
    //    - 4 пиксела - центр полосы прокрутки
    MinWidth: Integer = 10;
    // Цвет фона
    BackgroundColor: TColor = $1C1C1C;
    // Цвет обычной полоски
    ColorNormal: TColor = $414042;
    // Минимальный размер слайдера
    SliderMinHeight: Integer = 20;

  end;

implementation

end.
