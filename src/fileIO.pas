unit FileIO;

interface

uses
  SysUtils, Types, Components, ComponentTypes, Compatibility, Orders;

// Проверка существования файлов данных
function DataFilesExist: Boolean;

// Создание пустых файлов данных
procedure CreateEmptyDataFiles;

// Чтение данных из файлов
procedure ReadDataFromFiles;

// Запись данных в файлы
procedure WriteDataToFiles;

// Запись результатов подбора конфигураций в текстовый файл
procedure WriteConfigurationsToTextFile(Configurations: PConfigurationNode);

// Запись информации о заказе в текстовый файл
procedure WriteOrderInfoToTextFile(const Order: TOrder);

// Запись списка совместимых комплектующих в текстовый файл
procedure WriteCompatibleComponentsToTextFile(Components: PComponentNode);

implementation

// Функция IfThen для булевых значений
function IfThen(Condition: Boolean; TrueValue, FalseValue: string): string;
begin
  if Condition then
    IfThen := TrueValue
  else
    IfThen := FalseValue;
end;

// Проверка существования файлов данных
function DataFilesExist: Boolean;
var
  Exists: Boolean;
begin
  Exists := FileExists(COMPONENTS_FILE) and
            FileExists(COMPONENT_TYPES_FILE) and
            FileExists(COMPATIBILITY_FILE) and
            FileExists(ORDERS_FILE);
  DataFilesExist := Exists;
end;

// Создание пустых файлов данных
procedure CreateEmptyDataFiles;
var
  ComponentsFile: File;
  ComponentTypesFile: File;
  CompatibilityFile: File;
  OrdersFile: File;
  DataDir: string;
begin
  // Проверяем существование папки data/
  DataDir := 'data';
  if not DirectoryExists(DataDir) then
    CreateDir(DataDir);
  
  // Создаем пустой файл комплектующих
  Assign(ComponentsFile, COMPONENTS_FILE);
  Rewrite(ComponentsFile, SizeOf(TComponent));
  Close(ComponentsFile);
  
  // Создаем пустой файл типов комплектующих
  Assign(ComponentTypesFile, COMPONENT_TYPES_FILE);
  Rewrite(ComponentTypesFile, SizeOf(TComponentType));
  Close(ComponentTypesFile);
  
  // Создаем пустой файл совместимости
  Assign(CompatibilityFile, COMPATIBILITY_FILE);
  Rewrite(CompatibilityFile, SizeOf(TCompatibility));
  Close(CompatibilityFile);
  
  // Создаем пустой файл заказов
  Assign(OrdersFile, ORDERS_FILE);
  Rewrite(OrdersFile, SizeOf(TOrder));
  Close(OrdersFile);
  
  WriteLn('Созданы пустые файлы данных.');
end;

// Чтение данных из файлов
procedure ReadDataFromFiles;
var
  ComponentsFile: File;
  ComponentTypesFile: File;
  CompatibilityFile: File;
  OrdersFile: File;
  Component: TComponent;
  ComponentType: TComponentType;
  Compatibility: TCompatibility;
  Order: TOrder;
  ComponentCount, TypeCount, CompatibilityCount, OrderCount: Integer;
begin
  // Очищаем текущие списки
  FreeComponentsList;
  FreeComponentTypesList;
  FreeCompatibilityList;
  FreeOrdersList;
  
  // Инициализируем списки
  InitComponentsList;
  InitComponentTypesList;
  InitCompatibilityList;
  InitOrdersList;
  
  ComponentCount := 0;
  TypeCount := 0;
  CompatibilityCount := 0;
  OrderCount := 0;
  
  // Читаем данные о комплектующих
  if FileExists(COMPONENTS_FILE) then
  begin
    Assign(ComponentsFile, COMPONENTS_FILE);
    Reset(ComponentsFile, SizeOf(TComponent));
    
    while not Eof(ComponentsFile) do
    begin
      BlockRead(ComponentsFile, Component, 1);
      AddComponent(Component);
      Inc(ComponentCount);
    end;
    
    Close(ComponentsFile);
  end;
  
  // Читаем данные о типах комплектующих
  if FileExists(COMPONENT_TYPES_FILE) then
  begin
    Assign(ComponentTypesFile, COMPONENT_TYPES_FILE);
    Reset(ComponentTypesFile, SizeOf(TComponentType));
    
    while not Eof(ComponentTypesFile) do
    begin
      BlockRead(ComponentTypesFile, ComponentType, 1);
      AddComponentType(ComponentType);
      Inc(TypeCount);
    end;
    
    Close(ComponentTypesFile);
  end;
  
  // Читаем данные о совместимости
  if FileExists(COMPATIBILITY_FILE) then
  begin
    Assign(CompatibilityFile, COMPATIBILITY_FILE);
    Reset(CompatibilityFile, SizeOf(TCompatibility));
    
    while not Eof(CompatibilityFile) do
    begin
      BlockRead(CompatibilityFile, Compatibility, 1);
      AddCompatibility(Compatibility);
      Inc(CompatibilityCount);
    end;
    
    Close(CompatibilityFile);
  end;
  
  // Читаем данные о заказах
  if FileExists(ORDERS_FILE) then
  begin
    Assign(OrdersFile, ORDERS_FILE);
    Reset(OrdersFile, SizeOf(TOrder));
    
    while not Eof(OrdersFile) do
    begin
      BlockRead(OrdersFile, Order, 1);
      AddOrder(Order);
      Inc(OrderCount);
    end;
    
    Close(OrdersFile);
  end;
  
  WriteLn('Данные загружены из файлов:');
  WriteLn('- Комплектующих: ', ComponentCount);
  WriteLn('- Типов комплектующих: ', TypeCount);
  WriteLn('- Записей о совместимости: ', CompatibilityCount div 2); // Делим на 2, так как каждая запись дублируется
  WriteLn('- Заказов: ', OrderCount);
end;

// Запись данных в файлы
procedure WriteDataToFiles;
var
  ComponentsFile: File;
  ComponentTypesFile: File;
  CompatibilityFile: File;
  OrdersFile: File;
  CurrentComponent: PComponentNode;
  CurrentType: PComponentTypeNode;
  CurrentCompatibility: PCompatibilityNode;
  CurrentOrder: POrderNode;
  ComponentCount, TypeCount, CompatibilityCount, OrderCount: Integer;
  ProcessedCodes: array of Integer;
  ProcessedCodesCount: Integer;
  i: Integer;
  Found: Boolean;
begin
  ComponentCount := 0;
  TypeCount := 0;
  CompatibilityCount := 0;
  OrderCount := 0;
  
  // Записываем данные о комплектующих
  Assign(ComponentsFile, COMPONENTS_FILE);
  Rewrite(ComponentsFile, SizeOf(TComponent));
  
  CurrentComponent := DataLists.Components;
  while CurrentComponent <> nil do
  begin
    BlockWrite(ComponentsFile, CurrentComponent^.Data, 1);
    Inc(ComponentCount);
    CurrentComponent := CurrentComponent^.Next;
  end;
  
  Close(ComponentsFile);
  
  // Записываем данные о типах комплектующих
  Assign(ComponentTypesFile, COMPONENT_TYPES_FILE);
  Rewrite(ComponentTypesFile, SizeOf(TComponentType));
  
  CurrentType := DataLists.ComponentTypes;
  while CurrentType <> nil do
  begin
    BlockWrite(ComponentTypesFile, CurrentType^.Data, 1);
    Inc(TypeCount);
    CurrentType := CurrentType^.Next;
  end;
  
  Close(ComponentTypesFile);
  
  // Записываем данные о совместимости
  // Для совместимости нужно избежать дублирования записей
  Assign(CompatibilityFile, COMPATIBILITY_FILE);
  Rewrite(CompatibilityFile, SizeOf(TCompatibility));
  
  ProcessedCodesCount := 0;
  SetLength(ProcessedCodes, 0);
  
  CurrentCompatibility := DataLists.Compatibilities;
  while CurrentCompatibility <> nil do
  begin
    // Проверяем, не обрабатывали ли мы уже эту пару кодов
    Found := False;
    for i := 0 to ProcessedCodesCount - 1 do
    begin
      if (ProcessedCodes[i] = CurrentCompatibility^.Data.ComponentCode1 * 10000 + CurrentCompatibility^.Data.ComponentCode2) or
         (ProcessedCodes[i] = CurrentCompatibility^.Data.ComponentCode2 * 10000 + CurrentCompatibility^.Data.ComponentCode1) then
      begin
        Found := True;
        Break;
      end;
    end;
    
    if not Found then
    begin
      BlockWrite(CompatibilityFile, CurrentCompatibility^.Data, 1);
      Inc(CompatibilityCount);
      
      // Добавляем пару кодов в список обработанных
      Inc(ProcessedCodesCount);
      SetLength(ProcessedCodes, ProcessedCodesCount);
      ProcessedCodes[ProcessedCodesCount - 1] := CurrentCompatibility^.Data.ComponentCode1 * 10000 + CurrentCompatibility^.Data.ComponentCode2;
    end;
    
    CurrentCompatibility := CurrentCompatibility^.Next;
  end;
  
  Close(CompatibilityFile);
  
  // Записываем данные о заказах
  Assign(OrdersFile, ORDERS_FILE);
  Rewrite(OrdersFile, SizeOf(TOrder));
  
  CurrentOrder := DataLists.Orders;
  while CurrentOrder <> nil do
  begin
    BlockWrite(OrdersFile, CurrentOrder^.Data, 1);
    Inc(OrderCount);
    CurrentOrder := CurrentOrder^.Next;
  end;
  
  Close(OrdersFile);
  
  WriteLn('Данные сохранены в файлы:');
  WriteLn('- Комплектующих: ', ComponentCount);
  WriteLn('- Типов комплектующих: ', TypeCount);
  WriteLn('- Записей о совместимости: ', CompatibilityCount);
  WriteLn('- Заказов: ', OrderCount);
end;

// Запись результатов подбора конфигураций в текстовый файл
procedure WriteConfigurationsToTextFile(Configurations: PConfigurationNode);
var
  TextFile: Text;
  Current: PConfigurationNode;
  i: Integer;
  ComponentType: PComponentTypeNode;
begin
  Assign(TextFile, CONFIGURATIONS_TEXT_FILE);
  Rewrite(TextFile);
  
  WriteLn(TextFile, 'Результаты подбора конфигураций компьютера');
  WriteLn(TextFile, '==========================================');
  WriteLn(TextFile);
  
  Current := Configurations;
  
  if Current = nil then
    WriteLn(TextFile, 'Не найдено подходящих конфигураций.')
  else
  begin
    while Current <> nil do
    begin
      WriteLn(TextFile, 'Конфигурация #', Current^.Data.ComponentCount);
      WriteLn(TextFile, '--------------------');
      WriteLn(TextFile, 'Общая стоимость: ', Current^.Data.TotalPrice:0:2);
      WriteLn(TextFile);
      WriteLn(TextFile, 'Комплектующие:');
      
      for i := 0 to Current^.Data.ComponentCount - 1 do
      begin
        ComponentType := FindComponentTypeByCode(Current^.Data.Components[i].TypeCode);
        
        if ComponentType <> nil then
          WriteLn(TextFile, ComponentType^.Data.Name, ': ', Current^.Data.Components[i].Manufacturer, ' ', Current^.Data.Components[i].Model, ' - ', Current^.Data.Components[i].Price:0:2)
        else
          WriteLn(TextFile, 'Тип ', Current^.Data.Components[i].TypeCode, ': ', Current^.Data.Components[i].Manufacturer, ' ', Current^.Data.Components[i].Model, ' - ', Current^.Data.Components[i].Price:0:2);
      end;
      
      WriteLn(TextFile);
      WriteLn(TextFile, '==========================================');
      WriteLn(TextFile);
      
      Current := Current^.Next;
    end;
  end;
  
  Close(TextFile);
  
  WriteLn('Результаты подбора конфигураций сохранены в файл ', CONFIGURATIONS_TEXT_FILE);
end;

// Запись информации о заказе в текстовый файл
procedure WriteOrderInfoToTextFile(const Order: TOrder);
var
  TextFile: Text;
  i: Integer;
  Component: PComponentNode;
  ComponentType: PComponentTypeNode;
begin
  Assign(TextFile, ORDER_INFO_TEXT_FILE);
  Rewrite(TextFile);
  
  WriteLn(TextFile, 'Информация о заказе #', Order.OrderNumber);
  WriteLn(TextFile, '==========================================');
  WriteLn(TextFile);
  WriteLn(TextFile, 'Дата заказа: ', DateToStr(Order.Date));
  WriteLn(TextFile, 'Заказчик: ', Order.CustomerName);
  WriteLn(TextFile, 'Телефон: ', Order.CustomerPhone);
  WriteLn(TextFile, 'Общая стоимость: ', Order.TotalPrice:0:2);
  WriteLn(TextFile);
  WriteLn(TextFile, 'Комплектующие:');
  
  for i := 0 to Order.ComponentCount - 1 do
  begin
    Component := FindComponentByCode(Order.Components[i]);
    
    if Component <> nil then
    begin
      ComponentType := FindComponentTypeByCode(Component^.Data.TypeCode);
      
      if ComponentType <> nil then
        WriteLn(TextFile, ComponentType^.Data.Name, ': ', Component^.Data.Manufacturer, ' ', Component^.Data.Model, ' - ', Component^.Data.Price:0:2)
      else
        WriteLn(TextFile, 'Тип ', Component^.Data.TypeCode, ': ', Component^.Data.Manufacturer, ' ', Component^.Data.Model, ' - ', Component^.Data.Price:0:2);
    end
    else
      WriteLn(TextFile, 'Комплектующая с кодом ', Order.Components[i], ' не найдена.');
  end;
  
  WriteLn(TextFile);
  WriteLn(TextFile, '==========================================');
  
  Close(TextFile);
  
  WriteLn('Информация о заказе сохранена в файл ', ORDER_INFO_TEXT_FILE);
end;

// Запись списка совместимых комплектующих в текстовый файл
procedure WriteCompatibleComponentsToTextFile(Components: PComponentNode);
var
  TextFile: Text;
  Current: PComponentNode;
  ComponentType: PComponentTypeNode;
begin
  Assign(TextFile, COMPATIBLE_COMPONENTS_TEXT_FILE);
  Rewrite(TextFile);
  
  WriteLn(TextFile, 'Список совместимых комплектующих');
  WriteLn(TextFile, '================================');
  WriteLn(TextFile);
  
  Current := Components;
  
  if Current = nil then
    WriteLn(TextFile, 'Не найдено совместимых комплектующих.')
  else
  begin
    while Current <> nil do
    begin
      ComponentType := FindComponentTypeByCode(Current^.Data.TypeCode);
      
      if ComponentType <> nil then
        WriteLn(TextFile, ComponentType^.Data.Name, ': ', Current^.Data.Manufacturer, ' ', Current^.Data.Model)
      else
        WriteLn(TextFile, 'Тип ', Current^.Data.TypeCode, ': ', Current^.Data.Manufacturer, ' ', Current^.Data.Model);
      
      WriteLn(TextFile, 'Цена: ', Current^.Data.Price:0:2);
      WriteLn(TextFile, 'Параметры: ', Current^.Data.Parameters);
      WriteLn(TextFile, 'Наличие: ', IfThen(Current^.Data.InStock, 'Да', 'Нет'));
      WriteLn(TextFile, '--------------------------------');
      
      Current := Current^.Next;
    end;
  end;
  
  Close(TextFile);
  
  WriteLn('Список совместимых комплектующих сохранен в файл ', COMPATIBLE_COMPONENTS_TEXT_FILE);
end;

end.
