// TabSelectList.pas
//
// Вкладка выбора фильтра для списка пользователей
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit TabSelectList;

interface

uses
  SkinTabList, Classes, Graphics, Controls, StringUtils, Types, ImageUtils,
  ActiveRegion, Math, UserListSelect;

type
  TProcSelectItem = procedure(Sender: TObject; Index: Integer) of object;

  TTabSelectList = class(TGraphicControl)
  private
    FListSelect: TUserListSelect;
    FRegion: TActiveRegion;
    FItemRect: array of TRect;
    FSkin: TSkinTabList;
    FItems: TStringList;
    FItemSelect: Integer;
    FActiveCaption: DataString;
    FMargin: TRect;
    FContent: TRect;
    FOnSelectItem: TProcSelectItem;
    procedure ItemChange(Sender: TObject);
    procedure DrawItem(Index: Integer; Caption: DataString; IsSelect: Boolean);
    procedure ItemMessages(Sender: TObject; RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent; Skin: TSkinTabList;
      ListSelect: TUserListSelect); reintroduce;
    destructor Destroy; override;

    procedure CalcHeight;
    procedure SetMargin(Left, Top, Right, Bottom: Integer); overload;
    procedure SetMargin(Value: TRect); overload;

    property ActiveCaption: DataString read FActiveCaption;
    property Items: TStringList read FItems;

    property OnSelectItem: TProcSelectItem read FOnSelectItem write FOnSelectItem;
  end;

implementation

{ TTabSelectList }

constructor TTabSelectList.Create(AOwner: TComponent; Skin: TSkinTabList;
  ListSelect: TUserListSelect);
var
  i, c: Integer;
begin
  inherited Create(AOwner);
  FListSelect := ListSelect;
  FSkin := Skin;

  FRegion := TActiveRegion.Create(Self);
  FRegion.OnCursorMessage := ItemMessages;

  FItems := TStringList.Create;
  FItems.OnChange := ItemChange;
  FItems.BeginUpdate;
  try
    c := FListSelect.Count;
    for i := 0 to c - 1 do
      FItems.Add(FListSelect.GetText(i));

    FActiveCaption := FListSelect.GetActiveText;
  finally
    FItems.EndUpdate;
  end;

  Width := 100;
  Align := alBottom;

  FItemSelect := -1;
  CalcHeight;
end;

destructor TTabSelectList.Destroy;
begin
  FItems.Free;
  FRegion.Free;
  inherited;
end;

{ *  Рассчитывает размер компонента на основе введенных в него данных
  * }
procedure TTabSelectList.CalcHeight;
begin
  Height := FItems.Count * FSkin.ItemHeight + FMargin.Top + FMargin.Bottom;
end;

{ *  Событие на изменение состава меню
  * }
procedure TTabSelectList.ItemChange(Sender: TObject);
begin
  CalcHeight;
  SetLength(FItemRect, FItems.Count);

  if Assigned(Parent) and Visible then
    Invalidate;
end;

procedure TTabSelectList.ItemMessages(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
var
  ItemHeight, SelectItem: Integer;
begin
  ItemHeight := FSkin.ItemHeight;

  case RegionMessage of
    rmMouseLeave:
      begin
        if FItemSelect <> -1 then
        begin
          FItemSelect := -1;
          Invalidate;
        end;
      end;

    rmMouseMove:
      begin
        SelectItem := y div ItemHeight;
        if SelectItem <> FItemSelect then
        begin
          FItemSelect := SelectItem;
          Invalidate;
        end;
      end;

    rmMouseClick:
      begin
        SelectItem := y div ItemHeight;
        FActiveCaption := FItems[SelectItem];

        FListSelect.SelectList(SelectItem);

        if Assigned(FOnSelectItem) then
          FOnSelectItem(Self, SelectItem);
      end;
  end;
end;

{ *  Рисуется элемент списка в позиции Index с текстом Caption. Если установлен
  *  IsSelect в True, то рисуется фон за элементом
  * }
procedure TTabSelectList.DrawItem(Index: Integer; Caption: DataString;
  IsSelect: Boolean);
var
  DrawRect, DrawText: TRect;
  TextSize: TSize;
  ItemHeight: Integer;
begin
  ItemHeight := FSkin.ItemHeight;
  TextSize := TextExtentW(Canvas, Caption);

  DrawRect := FContent;
  DrawRect.Top := DrawRect.Top + Index * ItemHeight;
  DrawRect.Bottom := DrawRect.Top + ItemHeight;

  DrawText := DrawRect;
  DrawText.Left := DrawText.Left + FSkin.CaptionMarginLeft;
  DrawText.Right := DrawText.Right - FSkin.CaptionMarginRight;
  DrawText.Top := DrawText.Top + (ItemHeight - TextSize.cy) div 2;

  FItemRect[Index] := DrawRect;
  if IsSelect then
  begin
    Canvas.Brush.Color := FSkin.ItemColor[1];
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(DrawRect);
    Canvas.Brush.Style := bsClear;
  end;

  if RectWidth(DrawText) >= TextSize.cx then
  begin
    TextOutW(Canvas, DrawText, Caption);
  end
  else
  begin
    TextRectEndEllipsis(Canvas, DrawText, Caption);
  end;
end;

procedure TTabSelectList.Paint;
var
  i: Integer;
begin
  inherited;
  FSkin.ItemFont.SetCanvas(Canvas);

  for i := 0 to FItems.Count - 1 do
    DrawItem(i, FItems[i], FItemSelect = i);
end;

procedure TTabSelectList.Resize;
begin
  inherited;
  FContent := ClientRect;
  FContent.Left := FContent.Left + FMargin.Left;
  FContent.Top := FContent.Top + FMargin.Top;
  FContent.Right := FContent.Right - FMargin.Right;
  FContent.Bottom := FContent.Bottom - FMargin.Bottom;

  FRegion.SetRect(FContent, BoundsRect);
end;

procedure TTabSelectList.SetMargin(Value: TRect);
begin
  FMargin := Value;
end;

procedure TTabSelectList.VisibleChanging;
begin
  inherited;
  if not Visible and Assigned(Parent) then
  begin
    FRegion.Parent := Parent;
    FRegion.Visible := True;
  end
  else
  begin
    FRegion.Visible := False;
  end;
end;

procedure TTabSelectList.SetMargin(Left, Top, Right, Bottom: Integer);
begin
  SetMargin(Rect(Left, Top, Right, Bottom));
end;

end.
