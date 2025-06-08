unit UI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Crt, DataTypes, DynamicLists, InputUtils;

{ Константы для пунктов меню }
const
  MENU_LOAD_DATA = 1;
  MENU_VIEW_LIST = 2;
  MENU_SORT_DATA = 3;
  MENU_SEARCH_DATA = 4;
  MENU_ADD_DATA = 5;
  MENU_REMOVE_DATA = 6;
  MENU_EDIT_DATA = 7;
  MENU_SPECIAL_FUNCTIONS = 8;
  MENU_EXIT_WITHOUT_SAVE = 9;
  MENU_EXIT_WITH_SAVE = 10;

{ Константы для подменю списков }
const
  SUBMENU_COMPONENT_TYPES = 1;
  SUBMENU_COMPONENTS = 2;
  SUBMENU_COMPATIBILITY = 3;
  SUBMENU_ORDERS = 4;

{ Константы для подменю специальных функций }
const
  SUBMENU_SF_BUILD_PC = 1;
  SUBMENU_SF_CREATE_ORDER = 2;
  SUBMENU_SF_COMPATIBLE_COMPONENTS = 3;

{ Процедуры для отображения меню и подменю }
procedure DisplayMainMenu;
function GetMenuChoice: Integer;
procedure DisplayListSubmenu;
function GetListSubmenuChoice: Integer;
procedure DisplayAddDataSubmenu;
function GetAddDataSubmenuChoice: Integer;
procedure DisplaySpecialFunctionsSubmenu;
function GetSpecialFunctionsSubmenuChoice: Integer;

{ Процедуры для отображения данных }
procedure DisplayComponentTypes(const List: TComponentTypeList);
procedure DisplayComponents(const List: TComponentList; const TypesList: TComponentTypeList);
procedure DisplayCompatibility(const List: TCompatibilityList; const ComponentsList: TComponentList);
procedure DisplayOrders(const List: TOrderList; const BuildOptionsList: TPCBuildOptionList);
procedure DisplayOrdersDetailed(const List: TOrderList; const BuildOptionsList: TPCBuildOptionList; const ComponentsList: TComponentList);
procedure DisplayPCBuildOptions(const List: TPCBuildOptionList; const ComponentsList: TComponentList);

{ Процедуры для ввода данных }
function InputComponentType(var ComponentType: TComponentType; const List: TComponentTypeList): Boolean;
function InputComponent(var Component: TComponent; const TypesList: TComponentTypeList; const List: TComponentList): Boolean;
function InputCompatibility(var Compatibility: TCompatibility; const ComponentsList: TComponentList): Boolean;
function InputOrder(var Order: TOrder; const BuildOptionsList: TPCBuildOptionList; const List: TOrderList): Boolean;

{ Вспомогательные процедуры }
procedure ClearScreen;
procedure PressEnterToContinue;
function GetNextID(const List: TComponentTypeList): Integer; overload;
function GetNextID(const List: TComponentList): Integer; overload;
function GetNextID(const List: TPCBuildOptionList): Integer; overload;
function GetNextID(const List: TOrderList): Integer; overload;
function GetComponentTypeName(TypeCode: Integer; const TypesList: TComponentTypeList): string;

{ Глобальные счетчики для уникальных ID }
var
  NextComponentTypeID: Integer = 1;
  NextComponentID: Integer = 1;
  NextPCBuildOptionID: Integer = 1;
  NextOrderID: Integer = 1;

implementation

{ Реализация процедур для отображения меню и подменю }

procedure DisplayMainMenu;
begin
  ClearScreen;
  WriteLn('=== ГЛАВНОЕ МЕНЮ ===');
  WriteLn('1. Чтение данных из файла');
  WriteLn('2. Просмотр всего списка');
  WriteLn('3. Сортировка данных');
  WriteLn('4. Поиск данных с использованием фильтров');
  WriteLn('5. Добавление данных в список');
  WriteLn('6. Удаление данных из списка');
  WriteLn('7. Редактирование данных');
  WriteLn('8. Сборка ПК и Заказы');
  WriteLn('9. Выход из программы без сохранения изменений');
  WriteLn('10. Выход с сохранением изменений');
  WriteLn('==================');
  Write('Выберите пункт меню: ');
end;

function GetMenuChoice: Integer;
begin
  Result := SafeReadInteger('', MENU_LOAD_DATA, MENU_EXIT_WITH_SAVE,
                          'Ошибка: Выберите пункт меню от 1 до 10.');
end;

procedure DisplayListSubmenu;
begin
  ClearScreen;
  WriteLn('=== ВЫБЕРИТЕ СПИСОК ===');
  WriteLn('1. Типы комплектующих');
  WriteLn('2. Комплектующие');
  WriteLn('3. Совместимость комплектующих');
  WriteLn('4. Заказы');
  WriteLn('==================');
  Write('Выберите список: ');
end;

function GetListSubmenuChoice: Integer;
begin
  Result := SafeReadInteger('', SUBMENU_COMPONENT_TYPES, SUBMENU_ORDERS,
                          'Ошибка: Выберите пункт подменю от 1 до 4.');
end;

procedure DisplayAddDataSubmenu;
begin
  ClearScreen;
  WriteLn('=== ВЫБЕРИТЕ ТИП ДАННЫХ ДЛЯ ДОБАВЛЕНИЯ ===');
  WriteLn('1. Типы комплектующих');
  WriteLn('2. Комплектующие');
  WriteLn('3. Совместимость комплектующих');
  WriteLn('==================');
  Write('Выберите тип данных: ');
end;

function GetAddDataSubmenuChoice: Integer;
begin
  Result := SafeReadInteger('', SUBMENU_COMPONENT_TYPES, SUBMENU_COMPATIBILITY,
                          'Ошибка: Выберите пункт подменю от 1 до 3.');
end;

procedure DisplaySpecialFunctionsSubmenu;
begin
  ClearScreen;
  WriteLn('=== СПЕЦИАЛЬНЫЕ ФУНКЦИИ ===');
  WriteLn('1. Подбор вариантов комплектации ПК в заданном ценовом диапазоне');
  WriteLn('2. Оформление заказа');
  WriteLn('3. Поиск совместимых комплектующих заданного типа');
  WriteLn('==================');
  Write('Выберите функцию: ');
end;

function GetSpecialFunctionsSubmenuChoice: Integer;
begin
  Result := SafeReadInteger('', SUBMENU_SF_BUILD_PC, SUBMENU_SF_COMPATIBLE_COMPONENTS,
                          'Ошибка: Выберите пункт подменю от 1 до 3.');
end;

{ Реализация процедур для отображения данных }

procedure DisplayComponentTypes(const List: TComponentTypeList);
var
  Current: PComponentTypeNode;
  Count: Integer;
begin
  ClearScreen;
  WriteLn('=== СПИСОК ТИПОВ КОМПЛЕКТУЮЩИХ ===');
  WriteLn('Код типа │ Название');
  WriteLn('---------┼------------------------');
  
  Current := List.Head;
  Count := 0;
  
  while Current <> nil do
  begin
    WriteLn(Current^.Data.TypeCode:8, ' │ ', Current^.Data.Name);
    Current := Current^.Next;
    Inc(Count);
  end;
  
  WriteLn('---------┴------------------------');
  WriteLn('Всего записей: ', Count);
  WriteLn;
  
  PressEnterToContinue;
end;

procedure DisplayComponents(const List: TComponentList; const TypesList: TComponentTypeList);
var
  Current: PComponentNode;
  Count: Integer;
  TypeName: string;
begin
  ClearScreen;
  WriteLn('=== СПИСОК КОМПЛЕКТУЮЩИХ ===');
  WriteLn('Код │ Тип        │ Производитель │ Модель     │ Параметры       │ Цена     │ В наличии');
  WriteLn('----┼------------┼---------------┼------------┼-----------------┼----------┼----------');
  
  Current := List.Head;
  Count := 0;
  
  while Current <> nil do
  begin
    TypeName := GetComponentTypeName(Current^.Data.TypeCode, TypesList);
    
    WriteLn(Current^.Data.Code:3, ' │ ', 
            TypeName:10, ' │ ', 
            Current^.Data.Manufacturer:13, ' │ ', 
            Current^.Data.Model:10, ' │ ', 
            Current^.Data.Parameters:15, ' │ ', 
            Current^.Data.Price:8:2, ' │ ', 
            Current^.Data.InStock:9);
    
    Current := Current^.Next;
    Inc(Count);
  end;
  
  WriteLn('----┴------------┴---------------┴------------┴-----------------┴----------┴----------');
  WriteLn('Всего записей: ', Count);
  WriteLn;
  
  PressEnterToContinue;
end;

procedure DisplayCompatibility(const List: TCompatibilityList; const ComponentsList: TComponentList);
var
  Current: PCompatibilityNode;
  Count: Integer;
  Component1, Component2: PComponentNode;
begin
  ClearScreen;
  WriteLn('=== СПИСОК СОВМЕСТИМОСТИ КОМПЛЕКТУЮЩИХ ===');
  WriteLn('Код 1 │ Комплектующая 1      │ Код 2 │ Комплектующая 2');
  WriteLn('------┼----------------------┼-------┼------------------------');
  
  Current := List.Head;
  Count := 0;
  
  while Current <> nil do
  begin
    Component1 := FindComponent(ComponentsList, Current^.Data.ComponentCode1);
    Component2 := FindComponent(ComponentsList, Current^.Data.ComponentCode2);
    
    Write(Current^.Data.ComponentCode1:5, ' │ ');
    
    if Component1 <> nil then
      Write((Component1^.Data.Manufacturer + ' ' + Component1^.Data.Model):20)
    else
      Write('Неизвестно':20);
    
    Write(' │ ', Current^.Data.ComponentCode2:5, ' │ ');
    
    if Component2 <> nil then
      WriteLn((Component2^.Data.Manufacturer + ' ' + Component2^.Data.Model):20)
    else
      WriteLn('Неизвестно':20);
    
    Current := Current^.Next;
    Inc(Count);
  end;
  
  WriteLn('------┴----------------------┴-------┴------------------------');
  WriteLn('Всего записей: ', Count);
  WriteLn;
  
  PressEnterToContinue;
end;

procedure DisplayOrders(const List: TOrderList; const BuildOptionsList: TPCBuildOptionList);
var
  Current: POrderNode;
  Count: Integer;
  BuildOption: PPCBuildOptionNode;
begin
  ClearScreen;
  WriteLn('=== СПИСОК ЗАКАЗОВ ===');
  WriteLn('ID │ Вариант │ Заказчик     │ Телефон  │ Дата заказа │ Стоимость │ Компонентов');
  WriteLn('---┼---------┼--------------┼----------┼-------------┼-----------┼------------');
  
  Current := List.Head;
  Count := 0;
  
  while Current <> nil do
  begin
    BuildOption := FindPCBuildOption(BuildOptionsList, Current^.Data.BuildOptionID);
    
    Write(Current^.Data.ID:2, ' │ ', 
          Current^.Data.BuildOptionID:7, ' │ ', 
          Current^.Data.CustomerName:12, ' │ ', 
          Current^.Data.CustomerPhone:8, ' │ ', 
          FormatDateTime('dd.mm.yyyy', Current^.Data.OrderDate):11, ' │ ');
    
    if BuildOption <> nil then
    begin
      Write(BuildOption^.Data.TotalPrice:9:2, ' │ ');
      WriteLn(Length(BuildOption^.Data.ComponentCodes):11);
    end
    else
      WriteLn(' Неизвестно │  Неизвестно');
    
    Current := Current^.Next;
    Inc(Count);
  end;
  
  WriteLn('---┴---------┴--------------┴----------┴-------------┴-----------┴------------');
  WriteLn('Всего записей: ', Count);
  WriteLn;
  
  PressEnterToContinue;
end;

procedure DisplayOrdersDetailed(const List: TOrderList; const BuildOptionsList: TPCBuildOptionList; const ComponentsList: TComponentList);
var
  Current: POrderNode;
  Count, i: Integer;
  BuildOption: PPCBuildOptionNode;
  ComponentNode: PComponentNode;
begin
  ClearScreen;
  WriteLn('=== ПОДРОБНЫЙ СПИСОК ЗАКАЗОВ ===');
  
  Current := List.Head;
  Count := 0;
  
  while Current <> nil do
  begin
    Inc(Count);
    WriteLn;
    WriteLn('Заказ #', Current^.Data.ID);
    WriteLn('══════════════════════════════════════════════════════════════');
    WriteLn('Заказчик: ', Current^.Data.CustomerName);
    WriteLn('Телефон: ', Current^.Data.CustomerPhone);
    WriteLn('Дата заказа: ', FormatDateTime('dd.mm.yyyy hh:nn', Current^.Data.OrderDate));
    WriteLn('Вариант сборки: #', Current^.Data.BuildOptionID);
    
    BuildOption := FindPCBuildOption(BuildOptionsList, Current^.Data.BuildOptionID);
    if BuildOption <> nil then
    begin
      WriteLn('Общая стоимость: ', BuildOption^.Data.TotalPrice:0:2, ' руб.');
      WriteLn('Количество компонентов: ', Length(BuildOption^.Data.ComponentCodes));
      WriteLn('Состав комплектации:');
      
      for i := 0 to Length(BuildOption^.Data.ComponentCodes) - 1 do
      begin
        ComponentNode := FindComponent(ComponentsList, BuildOption^.Data.ComponentCodes[i]);
        if ComponentNode <> nil then
        begin
          WriteLn('  • ', ComponentNode^.Data.Manufacturer, ' ', 
                       ComponentNode^.Data.Model, ' — ', 
                       ComponentNode^.Data.Price:0:2, ' руб.');
        end
        else
          WriteLn('  • Неизвестный компонент (код: ', BuildOption^.Data.ComponentCodes[i], ')');
      end;
    end
    else
    begin
      WriteLn('Общая стоимость: Неизвестно (вариант сборки не найден)');
      WriteLn('Состав комплектации: Недоступен');
    end;
    
    Current := Current^.Next;
  end;
  
  WriteLn;
  WriteLn('══════════════════════════════════════════════════════════════');
  WriteLn('Всего заказов: ', Count);
  WriteLn;
  
  PressEnterToContinue;
end;

procedure DisplayPCBuildOptions(const List: TPCBuildOptionList; const ComponentsList: TComponentList);
var
  Current: PPCBuildOptionNode;
  Count, i: Integer;
  ComponentNode: PComponentNode;
begin
  ClearScreen;
  WriteLn('=== ВАРИАНТЫ КОМПЛЕКТАЦИИ ПК ===');
  
  Current := List.Head;
  Count := 0;
  
  while Current <> nil do
  begin
    WriteLn('Вариант #', Current^.Data.ID);
    WriteLn('Общая стоимость: ', Current^.Data.TotalPrice:0:2, ' руб.');
    WriteLn('Комплектующие:');
    
    for i := 0 to Length(Current^.Data.ComponentCodes) - 1 do
    begin
      ComponentNode := FindComponent(ComponentsList, Current^.Data.ComponentCodes[i]);
      
      if ComponentNode <> nil then
      begin
        WriteLn('  • ', ComponentNode^.Data.Manufacturer, ' ', 
                     ComponentNode^.Data.Model, ' (', 
                     ComponentNode^.Data.Price:0:2, ' руб.)');
      end;
    end;
    
    WriteLn('------------------------');
    
    Current := Current^.Next;
    Inc(Count);
  end;
  
  WriteLn('Всего вариантов: ', Count);
  WriteLn;
  
  PressEnterToContinue;
end;

{ Реализация процедур для ввода данных }

function InputComponentType(var ComponentType: TComponentType; const List: TComponentTypeList): Boolean;
var
  TempName: string;
begin
  Result := False;
  
  // Автоматически генерируем код типа комплектующей
  ComponentType.TypeCode := GetNextID(List);
  WriteLn('Код типа комплектующей (генерируется автоматически): ', ComponentType.TypeCode);
  
  // Безопасный ввод названия типа комплектующей
  TempName := SafeReadString('Введите название типа комплектующей: ', 1, MAX_STRING_LENGTH);
  
  // Проверка на команду возврата в меню
  if IsReturnToMenuCommand(TempName) or IsCancelCommand(TempName) then
    Exit;
  
  ComponentType.Name := StringToFixed(TempName);
  Result := True;
end;

function InputComponent(var Component: TComponent; const TypesList: TComponentTypeList; const List: TComponentList): Boolean;
var
  TypeNode: PComponentTypeNode;
  TempManufacturer, TempModel, TempParams: string;
  TypeCode: Integer;
begin
  Result := False;
  
  // Автоматически генерируем код комплектующей
  Component.Code := GetNextID(List);
  WriteLn('Код комплектующей (генерируется автоматически): ', Component.Code);
  
  // Ввод и проверка кода типа
  repeat
    TypeCode := SafeReadInteger('Введите код типа комплектующей: ', 1);
    
    // Проверка на команду возврата в меню
    if TypeCode < 1 then
      Exit;
    
    Component.TypeCode := TypeCode;
    TypeNode := FindComponentType(TypesList, Component.TypeCode);
    
    if TypeNode = nil then
      WriteLn('Ошибка: Тип с кодом ', Component.TypeCode, ' не найден. Пожалуйста, введите существующий код типа.');
  until TypeNode <> nil;
  
  // Безопасный ввод фирмы-изготовителя
  TempManufacturer := SafeReadString('Введите фирму-изготовителя: ', 1, MAX_STRING_LENGTH);
  if IsReturnToMenuCommand(TempManufacturer) or IsCancelCommand(TempManufacturer) then
    Exit;
  Component.Manufacturer := StringToFixed(TempManufacturer);
  
  // Безопасный ввод модели
  TempModel := SafeReadString('Введите модель: ', 1, MAX_STRING_LENGTH);
  if IsReturnToMenuCommand(TempModel) or IsCancelCommand(TempModel) then
    Exit;
  Component.Model := StringToFixed(TempModel);
  
  // Безопасный ввод параметров
  TempParams := SafeReadString('Введите параметры: ', 0, MAX_STRING_LENGTH);
  if IsReturnToMenuCommand(TempParams) or IsCancelCommand(TempParams) then
    Exit;
  Component.Parameters := StringToFixed(TempParams);
  
  // Безопасный ввод цены
  Component.Price := SafeReadFloat('Введите цену: ', 0);
  if Component.Price < 0 then
    Exit;
  
  // Безопасный ввод количества в наличии
  Component.InStock := SafeReadInteger('Введите количество в наличии: ', 0);
  if Component.InStock < 0 then
    Exit;
  
  Result := True;
end;

function InputCompatibility(var Compatibility: TCompatibility; const ComponentsList: TComponentList): Boolean;
var
  Component1, Component2: PComponentNode;
  ComponentCode1, ComponentCode2: Integer;
begin
  Result := False;
  
  try
    // Ввод и проверка кода первой комплектующей
    repeat
      ComponentCode1 := SafeReadInteger('Введите код первой комплектующей: ', 1);
      
      // Проверка на команду возврата в меню или отмены
      if ComponentCode1 < 1 then
        Exit;
      
      Compatibility.ComponentCode1 := ComponentCode1;
      Component1 := FindComponent(ComponentsList, Compatibility.ComponentCode1);
      if Component1 = nil then
        WriteLn('Ошибка: Комплектующая с кодом ', Compatibility.ComponentCode1, ' не найдена. Пожалуйста, введите существующий код.');
    until Component1 <> nil;
    
    // Ввод и проверка кода второй комплектующей
    repeat
      ComponentCode2 := SafeReadInteger('Введите код второй комплектующей (совместимой с первой): ', 1);
      
      // Проверка на команду возврата в меню или отмены
      if ComponentCode2 < 1 then
        Exit;
      
      Compatibility.ComponentCode2 := ComponentCode2;
      Component2 := FindComponent(ComponentsList, Compatibility.ComponentCode2);
      if Component2 = nil then
        WriteLn('Ошибка: Комплектующая с кодом ', Compatibility.ComponentCode2, ' не найдена. Пожалуйста, введите существующий код.');
    until Component2 <> nil;
    
    // Проверка, что коды различны
    if Compatibility.ComponentCode1 = Compatibility.ComponentCode2 then
    begin
      WriteLn('Ошибка: Нельзя создать запись о совместимости комплектующей с самой собой.');
      Exit;
    end;
    
    Result := True;
  except
    WriteLn('Ошибка при вводе данных.');
  end;
end;

function InputOrder(var Order: TOrder; const BuildOptionsList: TPCBuildOptionList; const List: TOrderList): Boolean;
var
  BuildOption: PPCBuildOptionNode;
  TempName, TempPhone: string;
  BuildOptionID: Integer;
begin
  Result := False;
  
  try
    // Автоматически генерируем ID заказа
    Order.ID := GetNextID(List);
    WriteLn('ID заказа (генерируется автоматически): ', Order.ID);
    
    // Ввод и проверка ID варианта комплектации
    repeat
      BuildOptionID := SafeReadInteger('Введите ID варианта комплектации: ', 1);
      
      // Проверка на команду возврата в меню или отмены
      if BuildOptionID < 1 then
        Exit;
      
      Order.BuildOptionID := BuildOptionID;
      BuildOption := FindPCBuildOption(BuildOptionsList, Order.BuildOptionID);
      if BuildOption = nil then
        WriteLn('Ошибка: Вариант комплектации с ID ', Order.BuildOptionID, ' не найден. Пожалуйста, введите существующий ID.');
    until BuildOption <> nil;
    
    TempName := SafeReadString('Введите имя заказчика: ', 1, MAX_STRING_LENGTH);
    if IsReturnToMenuCommand(TempName) or IsCancelCommand(TempName) then
      Exit;
    Order.CustomerName := StringToFixed(TempName);
    
    TempPhone := SafeReadString('Введите телефон заказчика: ', 1, MAX_STRING_LENGTH);
    if IsReturnToMenuCommand(TempPhone) or IsCancelCommand(TempPhone) then
      Exit;
    Order.CustomerPhone := StringToFixed(TempPhone);
    
    // Устанавливаем текущую дату и время
    Order.OrderDate := Now;
    
    Result := True;
  except
    WriteLn('Ошибка при вводе данных.');
  end;
end;

{ Реализация вспомогательных процедур }

procedure ClearScreen;
begin
  ClrScr;
end;

procedure PressEnterToContinue;
begin
  Write('Нажмите Enter для продолжения...');
  ReadLn;
end;

function GetNextID(const List: TComponentTypeList): Integer;
var
  Current: PComponentTypeNode;
  MaxID: Integer;
begin
  // Инициализируем глобальный счетчик на основе существующих данных (только при первом вызове)
  if NextComponentTypeID = 1 then
  begin
    MaxID := 0;
    Current := List.Head;
    
    while Current <> nil do
    begin
      if Current^.Data.TypeCode > MaxID then
        MaxID := Current^.Data.TypeCode;
      Current := Current^.Next;
    end;
    
    NextComponentTypeID := MaxID + 1;
  end;
  
  Result := NextComponentTypeID;
  Inc(NextComponentTypeID);
end;

function GetNextID(const List: TComponentList): Integer;
var
  Current: PComponentNode;
  MaxID: Integer;
begin
  // Инициализируем глобальный счетчик на основе существующих данных (только при первом вызове)
  if NextComponentID = 1 then
  begin
    MaxID := 0;
    Current := List.Head;
    
    while Current <> nil do
    begin
      if Current^.Data.Code > MaxID then
        MaxID := Current^.Data.Code;
      Current := Current^.Next;
    end;
    
    NextComponentID := MaxID + 1;
  end;
  
  Result := NextComponentID;
  Inc(NextComponentID);
end;

function GetNextID(const List: TPCBuildOptionList): Integer;
var
  Current: PPCBuildOptionNode;
  MaxID: Integer;
begin
  // Инициализируем глобальный счетчик на основе существующих данных (только при первом вызове)
  if NextPCBuildOptionID = 1 then
  begin
    MaxID := 0;
    Current := List.Head;
    
    while Current <> nil do
    begin
      if Current^.Data.ID > MaxID then
        MaxID := Current^.Data.ID;
      Current := Current^.Next;
    end;
    
    NextPCBuildOptionID := MaxID + 1;
  end;
  
  Result := NextPCBuildOptionID;
  Inc(NextPCBuildOptionID);
end;

function GetNextID(const List: TOrderList): Integer;
var
  Current: POrderNode;
  MaxID: Integer;
begin
  // Инициализируем глобальный счетчик на основе существующих данных (только при первом вызове)
  if NextOrderID = 1 then
  begin
    MaxID := 0;
    Current := List.Head;
    
    while Current <> nil do
    begin
      if Current^.Data.ID > MaxID then
        MaxID := Current^.Data.ID;
      Current := Current^.Next;
    end;
    
    NextOrderID := MaxID + 1;
  end;
  
  Result := NextOrderID;
  Inc(NextOrderID);
end;

function GetComponentTypeName(TypeCode: Integer; const TypesList: TComponentTypeList): string;
var
  TypeNode: PComponentTypeNode;
begin
  TypeNode := FindComponentType(TypesList, TypeCode);
  
  if TypeNode <> nil then
    Result := TypeNode^.Data.Name
  else
    Result := 'Неизвестно';
end;

end.
