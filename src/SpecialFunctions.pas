unit SpecialFunctions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DataTypes, DynamicLists, FileOperations, UI;

{ Функция для подбора вариантов комплектации ПК в заданном ценовом диапазоне }
function FindPCBuildOptions(const ComponentList: TComponentList;
                           const CompatibilityList: TCompatibilityList;
                           const TypesList: TComponentTypeList;
                           MinPrice, MaxPrice: Real;
                           var BuildOptionsList: TPCBuildOptionList): Boolean;

{ Функция для оформления заказа }
function CreateOrder(const BuildOption: TPCBuildOption;
                    const ComponentList: TComponentList;
                    var OrdersList: TOrderList): Boolean;

{ Функция для поиска совместимых комплектующих заданного типа }
function FindCompatibleComponents(const ComponentList: TComponentList;
                                 const CompatibilityList: TCompatibilityList;
                                 const TypesList: TComponentTypeList;
                                 ComponentCode, TypeCode: Integer;
                                 var CompatibleComponents: TComponentList): Boolean;

implementation

{ Вспомогательная функция для проверки совместимости двух комплектующих }
function AreComponentsCompatible(ComponentCode1, ComponentCode2: Integer;
                               const CompatibilityList: TCompatibilityList): Boolean;
var
  Current: PCompatibilityNode;
begin
  Result := False;
  Current := CompatibilityList.Head;
  
  while Current <> nil do
  begin
    // Проверяем совместимость в обоих направлениях
    if ((Current^.Data.ComponentCode1 = ComponentCode1) and (Current^.Data.ComponentCode2 = ComponentCode2)) or
       ((Current^.Data.ComponentCode1 = ComponentCode2) and (Current^.Data.ComponentCode2 = ComponentCode1)) then
    begin
      Result := True;
      Exit;
    end;
    
    Current := Current^.Next;
  end;
end;

{ Вспомогательная функция для проверки совместимости комплектующей со всеми в наборе }
function IsCompatibleWithAll(ComponentCode: Integer;
                           const ComponentCodes: array of Integer;
                           const CompatibilityList: TCompatibilityList): Boolean;
var
  i: Integer;
begin
  Result := True;
  
  for i := 0 to Length(ComponentCodes) - 1 do
  begin
    if not AreComponentsCompatible(ComponentCode, ComponentCodes[i], CompatibilityList) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{ Вспомогательная функция для получения списка комплектующих по типу }
function GetComponentsByType(const ComponentList: TComponentList;
                           TypeCode: Integer;
                           var ResultList: TComponentList): Boolean;
var
  Current: PComponentNode;
begin
  Result := False;
  InitComponentList(ResultList);
  
  Current := ComponentList.Head;
  while Current <> nil do
  begin
    if Current^.Data.TypeCode = TypeCode then
      AddComponent(ResultList, Current^.Data);
    
    Current := Current^.Next;
  end;
  
  Result := ResultList.Count > 0;
end;

{ Вспомогательная функция для рекурсивного построения вариантов комплектации }
procedure BuildConfigurationsRecursive(const ComponentList: TComponentList;
                                     const CompatibilityList: TCompatibilityList;
                                     const TypesList: TComponentTypeList;
                                     var BuildOptionsList: TPCBuildOptionList;
                                     CurrentComponents: array of Integer;
                                     CurrentPrice: Real;
                                     MinPrice, MaxPrice: Real;
                                     CurrentTypeIndex: Integer;
                                     var NextBuildID: Integer);
var
  TypeNode: PComponentTypeNode;
  ComponentsOfType: TComponentList;
  ComponentNode: PComponentNode;
  NewComponents: array of Integer;
  i, j: Integer;
  BuildOption: TPCBuildOption;
begin
  // Получаем текущий тип комплектующих
  TypeNode := TypesList.Head;
  for i := 1 to CurrentTypeIndex - 1 do
  begin
    if TypeNode = nil then
      Exit;
    TypeNode := TypeNode^.Next;
  end;
  
  // Если достигли конца списка типов, создаем вариант комплектации
  if TypeNode = nil then
  begin
    // Проверяем, что цена в заданном диапазоне
    if (CurrentPrice >= MinPrice) and (CurrentPrice <= MaxPrice) then
    begin
      // Создаем новый вариант комплектации
      BuildOption.ID := NextBuildID;
      Inc(NextBuildID);
      
      SetLength(BuildOption.ComponentCodes, Length(CurrentComponents));
      for i := 0 to Length(CurrentComponents) - 1 do
        BuildOption.ComponentCodes[i] := CurrentComponents[i];
      
      BuildOption.TotalPrice := CurrentPrice;
      
      // Добавляем в список вариантов
      AddPCBuildOption(BuildOptionsList, BuildOption);
    end;
    Exit;
  end;
  
  // Получаем все комплектующие текущего типа
  if not GetComponentsByType(ComponentList, TypeNode^.Data.TypeCode, ComponentsOfType) then
  begin
    // Если нет комплектующих этого типа, переходим к следующему типу
    BuildConfigurationsRecursive(ComponentList, CompatibilityList, TypesList,
                               BuildOptionsList, CurrentComponents, CurrentPrice,
                               MinPrice, MaxPrice, CurrentTypeIndex + 1, NextBuildID);
    Exit;
  end;
  
  // Для каждой комплектующей текущего типа
  ComponentNode := ComponentsOfType.Head;
  while ComponentNode <> nil do
  begin
    // Проверяем совместимость с уже выбранными комплектующими
    if IsCompatibleWithAll(ComponentNode^.Data.Code, CurrentComponents, CompatibilityList) then
    begin
      // Добавляем комплектующую к текущему набору
      SetLength(NewComponents, Length(CurrentComponents) + 1);
      for i := 0 to Length(CurrentComponents) - 1 do
        NewComponents[i] := CurrentComponents[i];
      NewComponents[Length(NewComponents) - 1] := ComponentNode^.Data.Code;
      
      // Рекурсивно продолжаем построение для следующего типа
      BuildConfigurationsRecursive(ComponentList, CompatibilityList, TypesList,
                                 BuildOptionsList, NewComponents,
                                 CurrentPrice + ComponentNode^.Data.Price,
                                 MinPrice, MaxPrice, CurrentTypeIndex + 1, NextBuildID);
    end;
    
    ComponentNode := ComponentNode^.Next;
  end;
  
  // Освобождаем память
  ClearComponentList(ComponentsOfType);
end;

{ Вспомогательная функция для сортировки вариантов комплектации по цене }
procedure SortBuildOptionsByPrice(var List: TPCBuildOptionList);
var
  Sorted: Boolean;
  Current, Next: PPCBuildOptionNode;
  TempData: TPCBuildOption;
begin
  if (List.Head = nil) or (List.Head^.Next = nil) then
    Exit; // Список пуст или содержит только один элемент
  
  repeat
    Sorted := True;
    Current := List.Head;
    
    while (Current <> nil) and (Current^.Next <> nil) do
    begin
      Next := Current^.Next;
      
      if Current^.Data.TotalPrice > Next^.Data.TotalPrice then
      begin
        // Меняем местами данные узлов
        TempData := Current^.Data;
        Current^.Data := Next^.Data;
        Next^.Data := TempData;
        
        Sorted := False;
      end;
      
      Current := Current^.Next;
    end;
  until Sorted;
end;

{ Реализация функции для подбора вариантов комплектации ПК в заданном ценовом диапазоне }
function FindPCBuildOptions(const ComponentList: TComponentList;
                           const CompatibilityList: TCompatibilityList;
                           const TypesList: TComponentTypeList;
                           MinPrice, MaxPrice: Real;
                           var BuildOptionsList: TPCBuildOptionList): Boolean;
var
  EmptyArray: array of Integer;
  NextBuildID: Integer;
begin
  Result := False;
  
  // Инициализируем список вариантов комплектации
  InitPCBuildOptionList(BuildOptionsList);
  
  // Инициализируем пустой массив комплектующих
  SetLength(EmptyArray, 0);
  
  // Получаем следующий доступный ID
  NextBuildID := UI.GetNextID(BuildOptionsList);
  
  // Рекурсивно строим все возможные варианты комплектации
  BuildConfigurationsRecursive(ComponentList, CompatibilityList, TypesList,
                             BuildOptionsList, EmptyArray, 0,
                             MinPrice, MaxPrice, 1, NextBuildID);
  
  // Сортируем варианты по цене
  SortBuildOptionsByPrice(BuildOptionsList);
  
  Result := BuildOptionsList.Count > 0;
end;

{ Реализация функции для оформления заказа }
function CreateOrder(const BuildOption: TPCBuildOption;
                    const ComponentList: TComponentList;
                    var OrdersList: TOrderList): Boolean;
var
  Order: TOrder;
  i: Integer;
  ComponentNode: PComponentNode;
  TempName, TempPhone: string;
begin
  Result := False;
  
  // Запрашиваем данные заказчика
  Write('Введите имя заказчика: ');
  ReadLn(TempName);
  Order.CustomerName := StringToFixed(TempName);
  
  Write('Введите телефон заказчика: ');
  ReadLn(TempPhone);
  Order.CustomerPhone := StringToFixed(TempPhone);
  
  // Генерируем ID заказа с использованием функции GetNextID
  Order.ID := UI.GetNextID(OrdersList);
  
  // Устанавливаем ID варианта комплектации и дату заказа
  Order.BuildOptionID := BuildOption.ID;
  Order.OrderDate := Now;
  
  // Проверяем наличие комплектующих и уменьшаем их количество
  for i := 0 to Length(BuildOption.ComponentCodes) - 1 do
  begin
    ComponentNode := FindComponent(ComponentList, BuildOption.ComponentCodes[i]);
    
    if ComponentNode = nil then
    begin
      WriteLn('Ошибка: Комплектующая с кодом ', BuildOption.ComponentCodes[i], ' не найдена.');
      Exit;
    end;
    
    if ComponentNode^.Data.InStock <= 0 then
    begin
      WriteLn('Ошибка: Комплектующая ', ComponentNode^.Data.Manufacturer, ' ', 
              ComponentNode^.Data.Model, ' отсутствует на складе.');
      Exit;
    end;
    
    // Уменьшаем количество на складе
    Dec(ComponentNode^.Data.InStock);
  end;
  
  // Добавляем заказ в список
  if not AddOrder(OrdersList, Order) then
  begin
    WriteLn('Ошибка при добавлении заказа в список.');
    Exit;
  end;
  
  // Сохраняем информацию о заказе в текстовый файл
  if not SaveOrderToTextFile(Order, BuildOption, ComponentList, '') then
  begin
    WriteLn('Ошибка при сохранении информации о заказе в файл.');
    Exit;
  end;
  
  Result := True;
end;

{ Реализация функции для поиска совместимых комплектующих заданного типа }
function FindCompatibleComponents(const ComponentList: TComponentList;
                                 const CompatibilityList: TCompatibilityList;
                                 const TypesList: TComponentTypeList;
                                 ComponentCode, TypeCode: Integer;
                                 var CompatibleComponents: TComponentList): Boolean;
var
  BaseComponent: PComponentNode;
  Current: PComponentNode;
begin
  Result := False;
  
  // Инициализируем список совместимых комплектующих
  InitComponentList(CompatibleComponents);
  
  // Находим базовую комплектующую
  BaseComponent := FindComponent(ComponentList, ComponentCode);
  if BaseComponent = nil then
  begin
    WriteLn('Ошибка: Комплектующая с кодом ', ComponentCode, ' не найдена.');
    Exit;
  end;
  
  // Проходим по всем комплектующим заданного типа
  Current := ComponentList.Head;
  while Current <> nil do
  begin
    // Проверяем, что комплектующая имеет заданный тип и совместима с базовой
    if (Current^.Data.TypeCode = TypeCode) and 
       AreComponentsCompatible(BaseComponent^.Data.Code, Current^.Data.Code, CompatibilityList) then
    begin
      // Добавляем в список совместимых
      AddComponent(CompatibleComponents, Current^.Data);
    end;
    
    Current := Current^.Next;
  end;
  
  Result := CompatibleComponents.Count > 0;
end;

end.
