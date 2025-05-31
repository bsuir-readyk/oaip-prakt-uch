unit DataTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  MAX_STRING_LENGTH = 255;

type
  { Строка фиксированной длины для использования в типизированных файлах }
  TFixedString = array[0..MAX_STRING_LENGTH] of Char;

  { Предварительные объявления типов указателей }
  PComponentType = ^TComponentType;
  PComponent = ^TComponent;
  PCompatibility = ^TCompatibility;
  PPCBuildOption = ^TPCBuildOption;
  POrder = ^TOrder;
  PComponentTypeNode = ^TComponentTypeNode;
  PComponentNode = ^TComponentNode;
  PCompatibilityNode = ^TCompatibilityNode;
  PPCBuildOptionNode = ^TPCBuildOptionNode;
  POrderNode = ^TOrderNode;

  { Тип комплектующей }
  TComponentType = record
    TypeCode: Integer;     // Код типа комплектующей
    Name: TFixedString;    // Название типа (например, "Процессор", "Видеокарта" и т.д.)
  end;

  { Комплектующая }
  TComponent = record
    Code: Integer;             // Уникальный код комплектующей
    TypeCode: Integer;         // Код типа комплектующей (связь с TComponentType)
    Manufacturer: TFixedString; // Фирма-изготовитель
    Model: TFixedString;       // Модель
    Parameters: TFixedString;  // Параметры (строка с описанием характеристик)
    Price: Real;               // Цена
    InStock: Integer;          // Количество в наличии
  end;

  { Совместимость комплектующих }
  TCompatibility = record
    ComponentCode1: Integer; // Код первой комплектующей
    ComponentCode2: Integer; // Код второй комплектующей (совместимой с первой)
  end;

  { Вариант комплектации ПК }
  TPCBuildOption = record
    ID: Integer;                // Уникальный идентификатор варианта
    ComponentCodes: array of Integer; // Массив кодов комплектующих в этой конфигурации
    TotalPrice: Real;           // Общая стоимость конфигурации
  end;

  { Заказ }
  TOrder = record
    ID: Integer;                // Уникальный идентификатор заказа
    BuildOptionID: Integer;     // ID выбранного варианта комплектации
    CustomerName: TFixedString; // Имя заказчика
    CustomerPhone: TFixedString; // Телефон заказчика
    OrderDate: TDateTime;       // Дата заказа
  end;

  { Узел односвязного списка для TComponentType }
  TComponentTypeNode = record
    Data: TComponentType;
    Next: PComponentTypeNode;
  end;

  { Узел односвязного списка для TComponent }
  TComponentNode = record
    Data: TComponent;
    Next: PComponentNode;
  end;

  { Узел односвязного списка для TCompatibility }
  TCompatibilityNode = record
    Data: TCompatibility;
    Next: PCompatibilityNode;
  end;

  { Узел односвязного списка для TPCBuildOption }
  TPCBuildOptionNode = record
    Data: TPCBuildOption;
    Next: PPCBuildOptionNode;
  end;

  { Узел односвязного списка для TOrder }
  TOrderNode = record
    Data: TOrder;
    Next: POrderNode;
  end;

  { Списки для каждого типа данных }
  TComponentTypeList = record
    Head: PComponentTypeNode;
    Count: Integer;
  end;

  TComponentList = record
    Head: PComponentNode;
    Count: Integer;
  end;

  TCompatibilityList = record
    Head: PCompatibilityNode;
    Count: Integer;
  end;

  TPCBuildOptionList = record
    Head: PPCBuildOptionNode;
    Count: Integer;
  end;

  TOrderList = record
    Head: POrderNode;
    Count: Integer;
  end;

{ Функции для работы с фиксированными строками }
function StringToFixed(const S: string): TFixedString;
function FixedToString(const F: TFixedString): string;

implementation

{ Функции для работы с фиксированными строками }

function StringToFixed(const S: string): TFixedString;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  for i := 1 to Length(S) do
  begin
    if i > MAX_STRING_LENGTH then
      Break;
    Result[i-1] := S[i];
  end;
end;

function FixedToString(const F: TFixedString): string;
var
  i: Integer;
begin
  Result := '';
  i := 0;
  while (i <= MAX_STRING_LENGTH) and (F[i] <> #0) do
  begin
    Result := Result + F[i];
    Inc(i);
  end;
end;

end.
