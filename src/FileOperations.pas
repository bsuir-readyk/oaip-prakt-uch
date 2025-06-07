unit FileOperations;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DataTypes, DynamicLists;

const
  COMPONENTS_FILE = 'data/components.dat';
  COMPONENT_TYPES_FILE = 'data/component_types.dat';
  COMPATIBILITY_FILE = 'data/compatibility.dat';
  ORDERS_FILE = 'data/orders.dat';

{ Функции для работы с файлом типов комплектующих }
function LoadComponentTypes(var List: TComponentTypeList; const FileName: string = COMPONENT_TYPES_FILE): Boolean;
function SaveComponentTypes(const List: TComponentTypeList; const FileName: string = COMPONENT_TYPES_FILE): Boolean;

{ Функции для работы с файлом комплектующих }
function LoadComponents(var List: TComponentList; const FileName: string = COMPONENTS_FILE): Boolean;
function SaveComponents(const List: TComponentList; const FileName: string = COMPONENTS_FILE): Boolean;

{ Функции для работы с файлом совместимости }
function LoadCompatibility(var List: TCompatibilityList; const FileName: string = COMPATIBILITY_FILE): Boolean;
function SaveCompatibility(const List: TCompatibilityList; const FileName: string = COMPATIBILITY_FILE): Boolean;

{ Функции для работы с файлом заказов }
function LoadOrders(var List: TOrderList; const FileName: string = ORDERS_FILE): Boolean;
function SaveOrders(const List: TOrderList; const FileName: string = ORDERS_FILE): Boolean;

{ Функции для сохранения результатов специальных функций в текстовые файлы }
function SavePCBuildOptionsToTextFile(const List: TPCBuildOptionList; 
                                      const ComponentList: TComponentList;
                                      const FileName: string = 'data/pc_build_options.txt'): Boolean;
function SaveOrderToTextFile(const Order: TOrder; 
                            const BuildOption: TPCBuildOption;
                            const ComponentList: TComponentList;
                            const FileName: string = ''): Boolean;

implementation

{ Реализация функций для работы с файлом типов комплектующих }

function LoadComponentTypes(var List: TComponentTypeList; const FileName: string): Boolean;
var
  F: file of TComponentType;
  Item: TComponentType;
begin
  Result := False;
  
  // Очищаем список перед загрузкой
  ClearComponentTypeList(List);
  
  // Проверяем существование файла
  if not FileExists(FileName) then
    Exit;
  
  try
    // Открываем файл для чтения
    AssignFile(F, FileName);
    Reset(F);
    
    // Читаем записи из файла и добавляем их в список
    while not Eof(F) do
    begin
      Read(F, Item);
      // Преобразуем фиксированную строку в обычную
      Item.Name := FixedToString(Item.Name);
      AddComponentType(List, Item);
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

function SaveComponentTypes(const List: TComponentTypeList; const FileName: string): Boolean;
var
  F: file of TComponentType;
  Current: PComponentTypeNode;
  TempData: TComponentType;
begin
  Result := False;
  
  try
    // Открываем файл для записи
    AssignFile(F, FileName);
    Rewrite(F);
    
    // Проходим по списку и записываем каждый элемент в файл
    Current := List.Head;
    while Current <> nil do
    begin
      // Создаем временную копию с фиксированными строками
      TempData.TypeCode := Current^.Data.TypeCode;
      TempData.Name := StringToFixed(Current^.Data.Name);
      
      Write(F, TempData);
      Current := Current^.Next;
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

{ Реализация функций для работы с файлом комплектующих }

function LoadComponents(var List: TComponentList; const FileName: string): Boolean;
var
  F: file of TComponent;
  Item: TComponent;
begin
  Result := False;
  
  // Очищаем список перед загрузкой
  ClearComponentList(List);
  
  // Проверяем существование файла
  if not FileExists(FileName) then
    Exit;
  
  try
    // Открываем файл для чтения
    AssignFile(F, FileName);
    Reset(F);
    
    // Читаем записи из файла и добавляем их в список
    while not Eof(F) do
    begin
      Read(F, Item);
      // Преобразуем фиксированные строки в обычные
      Item.Manufacturer := FixedToString(Item.Manufacturer);
      Item.Model := FixedToString(Item.Model);
      Item.Parameters := FixedToString(Item.Parameters);
      AddComponent(List, Item);
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

function SaveComponents(const List: TComponentList; const FileName: string): Boolean;
var
  F: file of TComponent;
  Current: PComponentNode;
  TempData: TComponent;
begin
  Result := False;
  
  try
    // Открываем файл для записи
    AssignFile(F, FileName);
    Rewrite(F);
    
    // Проходим по списку и записываем каждый элемент в файл
    Current := List.Head;
    while Current <> nil do
    begin
      // Создаем временную копию с фиксированными строками
      TempData.Code := Current^.Data.Code;
      TempData.TypeCode := Current^.Data.TypeCode;
      TempData.Manufacturer := StringToFixed(Current^.Data.Manufacturer);
      TempData.Model := StringToFixed(Current^.Data.Model);
      TempData.Parameters := StringToFixed(Current^.Data.Parameters);
      TempData.Price := Current^.Data.Price;
      TempData.InStock := Current^.Data.InStock;
      
      Write(F, TempData);
      Current := Current^.Next;
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

{ Реализация функций для работы с файлом совместимости }

function LoadCompatibility(var List: TCompatibilityList; const FileName: string): Boolean;
var
  F: file of TCompatibility;
  Item: TCompatibility;
begin
  Result := False;
  
  // Очищаем список перед загрузкой
  ClearCompatibilityList(List);
  
  // Проверяем существование файла
  if not FileExists(FileName) then
    Exit;
  
  try
    // Открываем файл для чтения
    AssignFile(F, FileName);
    Reset(F);
    
    // Читаем записи из файла и добавляем их в список
    while not Eof(F) do
    begin
      Read(F, Item);
      AddCompatibility(List, Item);
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

function SaveCompatibility(const List: TCompatibilityList; const FileName: string): Boolean;
var
  F: file of TCompatibility;
  Current: PCompatibilityNode;
begin
  Result := False;
  
  try
    // Открываем файл для записи
    AssignFile(F, FileName);
    Rewrite(F);
    
    // Проходим по списку и записываем каждый элемент в файл
    Current := List.Head;
    while Current <> nil do
    begin
      Write(F, Current^.Data);
      Current := Current^.Next;
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

{ Реализация функций для работы с файлом заказов }

function LoadOrders(var List: TOrderList; const FileName: string): Boolean;
var
  F: file of TOrder;
  Item: TOrder;
begin
  Result := False;
  
  // Очищаем список перед загрузкой
  ClearOrderList(List);
  
  // Проверяем существование файла
  if not FileExists(FileName) then
    Exit;
  
  try
    // Открываем файл для чтения
    AssignFile(F, FileName);
    Reset(F);
    
    // Читаем записи из файла и добавляем их в список
    while not Eof(F) do
    begin
      Read(F, Item);
      // Преобразуем фиксированные строки в обычные
      Item.CustomerName := FixedToString(Item.CustomerName);
      Item.CustomerPhone := FixedToString(Item.CustomerPhone);
      AddOrder(List, Item);
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

function SaveOrders(const List: TOrderList; const FileName: string): Boolean;
var
  F: file of TOrder;
  Current: POrderNode;
  TempData: TOrder;
begin
  Result := False;
  
  try
    // Открываем файл для записи
    AssignFile(F, FileName);
    Rewrite(F);
    
    // Проходим по списку и записываем каждый элемент в файл
    Current := List.Head;
    while Current <> nil do
    begin
      // Создаем временную копию с фиксированными строками
      TempData.ID := Current^.Data.ID;
      TempData.BuildOptionID := Current^.Data.BuildOptionID;
      TempData.CustomerName := StringToFixed(Current^.Data.CustomerName);
      TempData.CustomerPhone := StringToFixed(Current^.Data.CustomerPhone);
      TempData.OrderDate := Current^.Data.OrderDate;
      
      Write(F, TempData);
      Current := Current^.Next;
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

{ Реализация функций для сохранения результатов специальных функций в текстовые файлы }

function SavePCBuildOptionsToTextFile(const List: TPCBuildOptionList; 
                                     const ComponentList: TComponentList;
                                     const FileName: string): Boolean;
var
  F: TextFile;
  Current: PPCBuildOptionNode;
  ComponentNode: PComponentNode;
  i: Integer;
  ComponentCode: Integer;
begin
  Result := False;
  
  try
    // Открываем текстовый файл для записи
    AssignFile(F, FileName);
    Rewrite(F);
    
    // Записываем заголовок
    WriteLn(F, 'Варианты комплектации ПК');
    WriteLn(F, '════════════════════════');
    WriteLn(F);
    
    // Проходим по списку вариантов комплектации
    Current := List.Head;
    while Current <> nil do
    begin
      WriteLn(F, 'Вариант #', Current^.Data.ID);
      WriteLn(F, 'Общая стоимость: ', Current^.Data.TotalPrice:0:2, ' руб.');
      WriteLn(F, 'Комплектующие:');
      
      // Выводим информацию о каждой комплектующей в этом варианте
      for i := 0 to Length(Current^.Data.ComponentCodes) - 1 do
      begin
        ComponentCode := Current^.Data.ComponentCodes[i];
        ComponentNode := FindComponent(ComponentList, ComponentCode);
        
        if ComponentNode <> nil then
        begin
          WriteLn(F, '  • ', ComponentNode^.Data.Manufacturer, ' ', 
                     ComponentNode^.Data.Model, ' (', 
                     ComponentNode^.Data.Price:0:2, ' руб.)');
        end;
      end;
      
      WriteLn(F, '------------------------');
      WriteLn(F);
      
      Current := Current^.Next;
    end;
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

function SaveOrderToTextFile(const Order: TOrder; 
                           const BuildOption: TPCBuildOption;
                           const ComponentList: TComponentList;
                           const FileName: string): Boolean;
var
  F: TextFile;
  ComponentNode: PComponentNode;
  i: Integer;
  ComponentCode: Integer;
  ActualFileName: string;
begin
  Result := False;
  
  // Если имя файла не указано, генерируем его на основе ID заказа
  if FileName = '' then
    ActualFileName := 'data/order_confirmation_' + IntToStr(Order.ID) + '.txt'
  else
    ActualFileName := FileName;
  
  try
    // Открываем текстовый файл для записи
    AssignFile(F, ActualFileName);
    Rewrite(F);
    
    // Записываем информацию о заказе
    WriteLn(F, 'Подтверждение заказа #', Order.ID);
    WriteLn(F, '═══════════════════════════════');
    WriteLn(F);
    WriteLn(F, 'Дата заказа: ', FormatDateTime('dd.mm.yyyy hh:nn', Order.OrderDate));
    WriteLn(F, 'Заказчик: ', Order.CustomerName);
    WriteLn(F, 'Телефон: ', Order.CustomerPhone);
    WriteLn(F, 'Общая стоимость: ', BuildOption.TotalPrice:0:2, ' руб.');
    WriteLn(F);
    WriteLn(F, 'Комплектующие:');
    
    // Выводим информацию о каждой комплектующей в заказе
    for i := 0 to Length(BuildOption.ComponentCodes) - 1 do
    begin
      ComponentCode := BuildOption.ComponentCodes[i];
      ComponentNode := FindComponent(ComponentList, ComponentCode);
      
      if ComponentNode <> nil then
      begin
        WriteLn(F, '  • ', ComponentNode^.Data.Manufacturer, ' ', 
                   ComponentNode^.Data.Model, ' (', 
                   ComponentNode^.Data.Price:0:2, ' руб.)');
      end;
    end;
    
    WriteLn(F);
    WriteLn(F, 'Спасибо за ваш заказ!');
    
    // Закрываем файл
    CloseFile(F);
    Result := True;
  except
    // В случае ошибки закрываем файл и возвращаем False
    if IOResult = 0 then
      CloseFile(F);
    Result := False;
  end;
end;

end.
