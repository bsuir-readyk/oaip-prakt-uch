unit SpecialFunctions;

interface

uses
  SysUtils, Types, Components, ComponentTypes, Compatibility, FileIO, Orders, UI;

// Подбор вариантов комплектации компьютера в заданном ценовом диапазоне
function FindComputerConfigurations(MinPrice, MaxPrice: Real; SelectedTypes: array of Integer): PConfigurationNode;

// Оформление заказа на основе выбранной конфигурации
function CreateOrder(ConfigurationIndex: Integer; const CustomerName, CustomerPhone: string): TOrder;

// Поиск совместимых комплектующих заданного типа
function FindCompatibleComponentsOfType(ComponentCode, TypeCode: Integer): PComponentNode;

// Обновление наличия комплектующей
procedure UpdateComponentStock(ComponentCode: Integer; InStock: Boolean);

// Вывод списка конфигураций в виде таблицы
procedure PrintConfigurations(Configurations: PConfigurationNode);

implementation

// Вывод списка конфигураций в виде таблицы
procedure PrintConfigurations(Configurations: PConfigurationNode);
var
  Current: PConfigurationNode;
  ColumnWidths: array[0..3] of Integer;
  Alignments: array[0..3] of Char;
  Values: array[0..3] of string;
  i, j, ConfigCount: Integer;
  ComponentTypeNode: PComponentTypeNode;
begin
  Current := Configurations;
  
  WriteLn('Результаты подбора конфигураций:');
  
  if Current = nil then
    WriteLn('Не найдено подходящих конфигураций.')
  else
  begin
    // Определяем ширину столбцов
    ColumnWidths[0] := 5;  // Номер конфигурации
    ColumnWidths[1] := 15; // Общая стоимость
    ColumnWidths[2] := 20; // Тип комплектующей
    ColumnWidths[3] := 30; // Комплектующая
    
    // Определяем выравнивание столбцов
    Alignments[0] := 'C'; // Номер конфигурации - по правому краю
    Alignments[1] := 'C'; // Общая стоимость - по правому краю
    Alignments[2] := 'C'; // Тип комплектующей - по левому краю
    Alignments[3] := 'C'; // Комплектующая - по левому краю
    
    // Выводим заголовок таблицы
    PrintTableHorizontalLine(ColumnWidths, 'T');
    
    Values[0] := 'Number';
    Values[1] := 'Price';
    Values[2] := 'Type';
    Values[3] := 'Component';
    PrintTableRow(Values, ColumnWidths, Alignments);
    
    PrintTableHorizontalLine(ColumnWidths, 'M');
    
    // Выводим данные
    ConfigCount := 0;
    i := 1;
    
    while Current <> nil do
    begin
      // Выводим информацию о конфигурации
      Values[0] := IntToStr(i);
      Values[1] := Format('%.2f', [Current^.Data.TotalPrice]);
      Values[2] := '';
      Values[3] := '';
      PrintTableRow(Values, ColumnWidths, Alignments);
      
      // Выводим комплектующие
      for j := 0 to Current^.Data.ComponentCount - 1 do
      begin
        componentTypeNode := FindComponentTypeByCode(Current^.Data.Components[j].TypeCode);
        
        if componentTypeNode <> nil then
          Values[2] := componentTypeNode^.Data.Name
        else
          Values[2] := 'Type ' + IntToStr(Current^.Data.Components[j].TypeCode);
        
        Values[0] := '';
        Values[1] := '';
        Values[3] := Current^.Data.Components[j].Manufacturer + ' ' +
                     Current^.Data.Components[j].Model + ' - ' +
                     Format('%.2f', [Current^.Data.Components[j].Price]);
        
        PrintTableRow(Values, ColumnWidths, Alignments);
      end;
      
      PrintTableHorizontalLine(ColumnWidths, 'M');
      
      Current := Current^.Next;
      Inc(ConfigCount);
      Inc(i);
    end;
    
    PrintTableHorizontalLine(ColumnWidths, 'B');
    WriteLn('Total configurations: ', ConfigCount);
  end;
end;

// Проверка совместимости всех компонентов в конфигурации
function AreAllComponentsCompatible(Components: array of TComponent; Count: Integer): Boolean;
var
  i, j: Integer;
  Compatible: Boolean;
begin
  Compatible := True;
  
  for i := 0 to Count - 2 do
    for j := i + 1 to Count - 1 do
      if not AreComponentsCompatible(Components[i].Code, Components[j].Code) then
      begin
        Compatible := False;
        Break;
      end;
  
  AreAllComponentsCompatible := Compatible;
end;

// Рекурсивная функция для генерации всех возможных комбинаций комплектующих
procedure GenerateConfigurations(
  var CurrentComponents: array of TComponent;
  var CurrentCount: Integer;
  ComponentsByType: array of PComponentNode;
  TypeCount: Integer;
  CurrentTypeIndex: Integer;
  MinPrice, MaxPrice: Real;
  var Configurations: PConfigurationNode);
var
  Current: PComponentNode;
  TotalPrice: Real;
  i: Integer;
  NewConfiguration: PConfigurationNode;
  isCompatible: Boolean;
  inPriceRange: Boolean;
begin
  // Если мы обработали все типы комплектующих, проверяем конфигурацию
  if CurrentTypeIndex >= TypeCount then
  begin
    // Проверяем совместимость всех компонентов
    isCompatible := AreAllComponentsCompatible(CurrentComponents, CurrentCount);
    
    if isCompatible then
    begin
      // Вычисляем общую стоимость
      TotalPrice := 0;
      for i := 0 to CurrentCount - 1 do
        TotalPrice := TotalPrice + CurrentComponents[i].Price;
      
      // Проверяем, что стоимость в заданном диапазоне
      inPriceRange := (TotalPrice >= MinPrice) and (TotalPrice <= MaxPrice);
      
      if inPriceRange then
      begin
        // Создаем новую конфигурацию
        New(NewConfiguration);
        NewConfiguration^.Data.ComponentCount := CurrentCount;
        SetLength(NewConfiguration^.Data.Components, CurrentCount);
        
        for i := 0 to CurrentCount - 1 do
          NewConfiguration^.Data.Components[i] := CurrentComponents[i];
        
        NewConfiguration^.Data.TotalPrice := TotalPrice;
        
        // Добавляем конфигурацию в список
        NewConfiguration^.Next := Configurations;
        Configurations := NewConfiguration;
      end;
    end;
  end
  else
  begin
    // Перебираем все комплектующие текущего типа
    Current := ComponentsByType[CurrentTypeIndex];
    
    while Current <> nil do
    begin
      // Добавляем текущую комплектующую в конфигурацию
      CurrentComponents[CurrentCount] := Current^.Data;
      Inc(CurrentCount);
      
      // Рекурсивно генерируем конфигурации для следующего типа
      GenerateConfigurations(
        CurrentComponents,
        CurrentCount,
        ComponentsByType,
        TypeCount,
        CurrentTypeIndex + 1,
        MinPrice,
        MaxPrice,
        Configurations);
      
      // Удаляем текущую комплектующую из конфигурации
      Dec(CurrentCount);
      
      Current := Current^.Next;
    end;
  end;
end;

// Подбор вариантов комплектации компьютера в заданном ценовом диапазоне
function FindComputerConfigurations(MinPrice, MaxPrice: Real; SelectedTypes: array of Integer): PConfigurationNode;
var
  ComponentsByType: array of PComponentNode;
  CurrentComponents: array of TComponent;
  CurrentCount: Integer;
  TypeCount: Integer;
  i: Integer;
  Configurations: PConfigurationNode;
  Current: PComponentNode;
  hasSelectedTypes: Boolean;
begin
  TypeCount := Length(SelectedTypes);
  hasSelectedTypes := TypeCount > 0;
  Configurations := nil;
  
  if hasSelectedTypes then
  begin
    // Инициализируем массив списков комплектующих по типам
    SetLength(ComponentsByType, TypeCount);
    
    for i := 0 to TypeCount - 1 do
      ComponentsByType[i] := FindComponentsByType(SelectedTypes[i]);
    
    // Инициализируем массив для текущей конфигурации
    SetLength(CurrentComponents, TypeCount);
    CurrentCount := 0;
    
    // Генерируем все возможные конфигурации
    GenerateConfigurations(
      CurrentComponents,
      CurrentCount,
      ComponentsByType,
      TypeCount,
      0,
      MinPrice,
      MaxPrice,
      Configurations);
    
    // Освобождаем память, занятую временными списками
    for i := 0 to TypeCount - 1 do
    begin
      while ComponentsByType[i] <> nil do
      begin
        Current := ComponentsByType[i];
        ComponentsByType[i] := ComponentsByType[i]^.Next;
        Dispose(Current);
      end;
    end;
  end;
  
  FindComputerConfigurations := Configurations;
end;

// Обновление наличия комплектующей
procedure UpdateComponentStock(ComponentCode: Integer; InStock: Boolean);
var
  Component: PComponentNode;
begin
  Component := FindComponentByCode(ComponentCode);
  
  if Component <> nil then
  begin
    Component^.Data.InStock := InStock;
    EditComponent(Component^.Data);
  end;
end;

// Оформление заказа на основе выбранной конфигурации
function CreateOrder(ConfigurationIndex: Integer; const CustomerName, CustomerPhone: string): TOrder;
var
  Current: PConfigurationNode;
  i, Index: Integer;
  Order: TOrder;
begin
  // Инициализируем заказ
  Order.OrderNumber := GetNewOrderNumber;
  Order.Date := Now;
  Order.CustomerName := CustomerName;
  Order.CustomerPhone := CustomerPhone;
  Order.TotalPrice := 0;
  Order.ComponentCount := 0;
  SetLength(Order.Components, 0);
  
  // Ищем конфигурацию с заданным индексом
  Current := DataLists.Configurations;
  Index := 1;
  
  while (Current <> nil) and (Index <> ConfigurationIndex) do
  begin
    Current := Current^.Next;
    Inc(Index);
  end;
  
  // Если конфигурация найдена, заполняем заказ
  if Current <> nil then
  begin
    Order.TotalPrice := Current^.Data.TotalPrice;
    Order.ComponentCount := Current^.Data.ComponentCount;
    SetLength(Order.Components, Order.ComponentCount);
    
    for i := 0 to Order.ComponentCount - 1 do
    begin
      Order.Components[i] := Current^.Data.Components[i].Code;
      
      // Обновляем наличие комплектующей
      UpdateComponentStock(Current^.Data.Components[i].Code, False);
    end;
  end;
  
  CreateOrder := Order;
end;

// Поиск совместимых комплектующих заданного типа
function FindCompatibleComponentsOfType(ComponentCode, TypeCode: Integer): PComponentNode;
var
  CompatibleComponents: PComponentNode;
begin
  CompatibleComponents := FindCompatibleComponentsByType(ComponentCode, TypeCode);
  FindCompatibleComponentsOfType := CompatibleComponents;
end;

end.
