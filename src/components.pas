unit Components;

interface

uses
  SysUtils, Types;

// Инициализация списка комплектующих
procedure InitComponentsList;

// Освобождение памяти, занятой списком комплектующих
procedure FreeComponentsList;

// Добавление комплектующей в список
procedure AddComponent(const Component: TComponent);

// Удаление комплектующей из списка по коду
function DeleteComponent(Code: Integer): Boolean;

// Редактирование комплектующей
function EditComponent(const Component: TComponent): Boolean;

// Поиск комплектующей по коду
function FindComponentByCode(Code: Integer): PComponentNode;

// Поиск комплектующих по типу
function FindComponentsByType(TypeCode: Integer): PComponentNode;

// Поиск комплектующих по производителю
function FindComponentsByManufacturer(const Manufacturer: string): PComponentNode;

// Поиск комплектующих в ценовом диапазоне
function FindComponentsByPriceRange(MinPrice, MaxPrice: Real): PComponentNode;

// Сортировка комплектующих по цене
procedure SortComponentsByPrice(var List: PComponentNode);

// Сортировка комплектующих по наличию
procedure SortComponentsByStock(var List: PComponentNode);

// Сортировка комплектующих по производителю
procedure SortComponentsByManufacturer(var List: PComponentNode);

// Получение нового уникального кода для комплектующей
function GetNewComponentCode: Integer;

// Вывод списка комплектующих
procedure PrintComponents(List: PComponentNode);

implementation

// Инициализация списка комплектующих
procedure InitComponentsList;
begin
  DataLists.Components := nil;
end;

// Освобождение памяти, занятой списком комплектующих
procedure FreeComponentsList;
var
  Current, Temp: PComponentNode;
begin
  Current := DataLists.Components;
  while Current <> nil do
  begin
    Temp := Current;
    Current := Current^.Next;
    Dispose(Temp);
  end;
  DataLists.Components := nil;
end;

// Добавление комплектующей в список
procedure AddComponent(const Component: TComponent);
var
  NewNode: PComponentNode;
begin
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := Component;
  NewNode^.Next := nil;
  
  // Добавляем узел в начало списка
  if DataLists.Components = nil then
    DataLists.Components := NewNode
  else
  begin
    NewNode^.Next := DataLists.Components;
    DataLists.Components := NewNode;
  end;
end;

// Удаление комплектующей из списка по коду
function DeleteComponent(Code: Integer): Boolean;
var
  Current, Previous: PComponentNode;
  Found: Boolean;
begin
  Current := DataLists.Components;
  Previous := nil;
  Found := False;
  
  // Ищем комплектующую с заданным кодом
  while (Current <> nil) and (Current^.Data.Code <> Code) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если комплектующая найдена, удаляем ее
  if Current <> nil then
  begin
    if Previous = nil then
      DataLists.Components := Current^.Next
    else
      Previous^.Next := Current^.Next;
    
    Dispose(Current);
    Found := True;
  end;
  
  DeleteComponent := Found;
end;

// Редактирование комплектующей
function EditComponent(const Component: TComponent): Boolean;
var
  Current: PComponentNode;
  Found: Boolean;
begin
  Current := FindComponentByCode(Component.Code);
  Found := False;
  
  if Current <> nil then
  begin
    Current^.Data := Component;
    Found := True;
  end;
  
  EditComponent := Found;
end;

// Поиск комплектующей по коду
function FindComponentByCode(Code: Integer): PComponentNode;
var
  Current: PComponentNode;
begin
  Current := DataLists.Components;
  
  while (Current <> nil) and (Current^.Data.Code <> Code) do
    Current := Current^.Next;
  
  FindComponentByCode := Current;
end;

// Поиск комплектующих по типу
function FindComponentsByType(TypeCode: Integer): PComponentNode;
var
  Current, ResultList, Last: PComponentNode;
begin
  ResultList := nil;
  Last := nil;
  Current := DataLists.Components;
  
  while Current <> nil do
  begin
    if Current^.Data.TypeCode = TypeCode then
    begin
      // Создаем копию узла
      New(Last);
      Last^.Data := Current^.Data;
      Last^.Next := nil;
      
      // Добавляем узел в результирующий список
      if ResultList = nil then
        ResultList := Last
      else
      begin
        Last^.Next := ResultList;
        ResultList := Last;
      end;
    end;
    
    Current := Current^.Next;
  end;
  
  FindComponentsByType := ResultList;
end;

// Поиск комплектующих по производителю
function FindComponentsByManufacturer(const Manufacturer: string): PComponentNode;
var
  Current, ResultList, Last: PComponentNode;
begin
  ResultList := nil;
  Last := nil;
  Current := DataLists.Components;
  
  while Current <> nil do
  begin
    if Current^.Data.Manufacturer = Manufacturer then
    begin
      // Создаем копию узла
      New(Last);
      Last^.Data := Current^.Data;
      Last^.Next := nil;
      
      // Добавляем узел в результирующий список
      if ResultList = nil then
        ResultList := Last
      else
      begin
        Last^.Next := ResultList;
        ResultList := Last;
      end;
    end;
    
    Current := Current^.Next;
  end;
  
  FindComponentsByManufacturer := ResultList;
end;

// Поиск комплектующих в ценовом диапазоне
function FindComponentsByPriceRange(MinPrice, MaxPrice: Real): PComponentNode;
var
  Current, ResultList, Last: PComponentNode;
begin
  ResultList := nil;
  Last := nil;
  Current := DataLists.Components;
  
  while Current <> nil do
  begin
    if (Current^.Data.Price >= MinPrice) and (Current^.Data.Price <= MaxPrice) then
    begin
      // Создаем копию узла
      New(Last);
      Last^.Data := Current^.Data;
      Last^.Next := nil;
      
      // Добавляем узел в результирующий список
      if ResultList = nil then
        ResultList := Last
      else
      begin
        Last^.Next := ResultList;
        ResultList := Last;
      end;
    end;
    
    Current := Current^.Next;
  end;
  
  FindComponentsByPriceRange := ResultList;
end;

// Сортировка комплектующих по цене
procedure SortComponentsByPrice(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
  needsSorting: Boolean;
begin
  needsSorting := (List <> nil) and (List^.Next <> nil);
  
  if needsSorting then
  begin
    Sorted := nil;
    
    while List <> nil do
    begin
      Current := List;
      List := List^.Next;
      
      if (Sorted = nil) or (Current^.Data.Price < Sorted^.Data.Price) then
      begin
        Current^.Next := Sorted;
        Sorted := Current;
      end
      else
      begin
        Temp := Sorted;
        
        while (Temp^.Next <> nil) and (Current^.Data.Price >= Temp^.Next^.Data.Price) do
          Temp := Temp^.Next;
        
        Current^.Next := Temp^.Next;
        Temp^.Next := Current;
      end;
    end;
    
    List := Sorted;
  end;
end;

// Сортировка комплектующих по наличию
procedure SortComponentsByStock(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
  needsSorting: Boolean;
begin
  needsSorting := (List <> nil) and (List^.Next <> nil);
  
  if needsSorting then
  begin
    Sorted := nil;
    
    while List <> nil do
    begin
      Current := List;
      List := List^.Next;
      
      if (Sorted = nil) or (Current^.Data.InStock and not Sorted^.Data.InStock) then
      begin
        Current^.Next := Sorted;
        Sorted := Current;
      end
      else
      begin
        Temp := Sorted;
        
        while (Temp^.Next <> nil) and (not Current^.Data.InStock or Temp^.Next^.Data.InStock) do
          Temp := Temp^.Next;
        
        Current^.Next := Temp^.Next;
        Temp^.Next := Current;
      end;
    end;
    
    List := Sorted;
  end;
end;

// Сортировка комплектующих по производителю
procedure SortComponentsByManufacturer(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
  needsSorting: Boolean;
begin
  needsSorting := (List <> nil) and (List^.Next <> nil);
  
  if needsSorting then
  begin
    Sorted := nil;
    
    while List <> nil do
    begin
      Current := List;
      List := List^.Next;
      
      if (Sorted = nil) or (Current^.Data.Manufacturer < Sorted^.Data.Manufacturer) then
      begin
        Current^.Next := Sorted;
        Sorted := Current;
      end
      else
      begin
        Temp := Sorted;
        
        while (Temp^.Next <> nil) and (Current^.Data.Manufacturer >= Temp^.Next^.Data.Manufacturer) do
          Temp := Temp^.Next;
        
        Current^.Next := Temp^.Next;
        Temp^.Next := Current;
      end;
    end;
    
    List := Sorted;
  end;
end;

// Получение нового уникального кода для комплектующей
function GetNewComponentCode: Integer;
var
  Current: PComponentNode;
  MaxCode: Integer;
begin
  MaxCode := 0;
  Current := DataLists.Components;
  
  while Current <> nil do
  begin
    if Current^.Data.Code > MaxCode then
      MaxCode := Current^.Data.Code;
    
    Current := Current^.Next;
  end;
  
  GetNewComponentCode := MaxCode + 1;
end;

// Вывод списка комплектующих
procedure PrintComponents(List: PComponentNode);
var
  Current: PComponentNode;
begin
  Current := List;
  
  WriteLn('Список комплектующих:');
  WriteLn('--------------------');
  
  if Current = nil then
    WriteLn('Список пуст')
  else
    while Current <> nil do
    begin
      with Current^.Data do
      begin
        WriteLn('Код: ', Code);
        WriteLn('Тип: ', TypeCode);
        WriteLn('Производитель: ', Manufacturer);
        WriteLn('Модель: ', Model);
        WriteLn('Параметры: ', Parameters);
        WriteLn('Цена: ', Price:0:2);
        Write('Наличие: ');
        
        if InStock then
          WriteLn('Да')
        else
          WriteLn('Нет');
        
        WriteLn('--------------------');
      end;
      
      Current := Current^.Next;
    end;
end;

end.
