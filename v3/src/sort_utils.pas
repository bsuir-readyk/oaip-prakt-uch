unit sort_utils;

interface

uses
  SysUtils, types, list_utils;

// Сортировка списка комплектующих
procedure SortComponentsByPrice(var list: TComponentList; ascending: Boolean);
procedure SortComponentsByManufacturer(var list: TComponentList);
procedure SortComponentsByModel(var list: TComponentList);
procedure SortComponentsByType(var list: TComponentList);

// Сортировка списка компьютерных сборок
procedure SortComputerSetsByPrice(var list: TComputerSetList; ascending: Boolean);

implementation

// Вспомогательная функция для обмена значений двух комплектующих
procedure SwapComponents(var comp1, comp2: TComponent);
var
  temp: TComponent;
begin
  temp := comp1;
  comp1 := comp2;
  comp2 := temp;
end;

// Вспомогательная функция для обмена значений двух компьютерных сборок
procedure SwapComputerSets(var set1, set2: TComputerSet);
var
  temp: TComputerSet;
begin
  temp := set1;
  set1 := set2;
  set2 := temp;
end;

// Функция для получения длины списка комплектующих
function GetComponentListLength(const list: TComponentList): Integer;
var
  current: TComponentList;
begin
  Result := 0;
  current := list;
  
  while current <> nil do
  begin
    Inc(Result);
    current := current^.next;
  end;
end;

// Функция для получения длины списка компьютерных сборок
function GetComputerSetListLength(const list: TComputerSetList): Integer;
var
  current: TComputerSetList;
begin
  Result := 0;
  current := list;
  
  while current <> nil do
  begin
    Inc(Result);
    current := current^.next;
  end;
end;

// Функция для получения указателя на элемент списка комплектующих по индексу
function GetComponentNodeByIndex(const list: TComponentList; index: Integer): TComponentList;
var
  current: TComponentList;
  i: Integer;
begin
  Result := nil;
  
  if (index < 0) or (list = nil) then
    Exit;
  
  current := list;
  i := 0;
  
  while (current <> nil) and (i < index) do
  begin
    current := current^.next;
    Inc(i);
  end;
  
  if i = index then
    Result := current;
end;

// Функция для получения указателя на элемент списка компьютерных сборок по индексу
function GetComputerSetNodeByIndex(const list: TComputerSetList; index: Integer): TComputerSetList;
var
  current: TComputerSetList;
  i: Integer;
begin
  Result := nil;
  
  if (index < 0) or (list = nil) then
    Exit;
  
  current := list;
  i := 0;
  
  while (current <> nil) and (i < index) do
  begin
    current := current^.next;
    Inc(i);
  end;
  
  if i = index then
    Result := current;
end;

// Реализация сортировки списка комплектующих по цене
procedure SortComponentsByPrice(var list: TComponentList; ascending: Boolean);
var
  i, j, n: Integer;
  node_i, node_j: TComponentList;
begin
  n := GetComponentListLength(list);
  
  if n <= 1 then
    Exit;
  
  // Сортировка пузырьком
  for i := 0 to n - 2 do
  begin
    for j := 0 to n - i - 2 do
    begin
      node_j := GetComponentNodeByIndex(list, j);
      node_j.next := GetComponentNodeByIndex(list, j + 1);
      
      if (ascending and (node_j^.data.cost > node_j.next^.data.cost)) or
         (not ascending and (node_j^.data.cost < node_j.next^.data.cost)) then
      begin
        SwapComponents(node_j^.data, node_j.next^.data);
      end;
    end;
  end;
end;

// Реализация сортировки списка комплектующих по производителю
procedure SortComponentsByManufacturer(var list: TComponentList);
var
  i, j, n: Integer;
  node_i, node_j: TComponentList;
begin
  n := GetComponentListLength(list);
  
  if n <= 1 then
    Exit;
  
  // Сортировка пузырьком
  for i := 0 to n - 2 do
  begin
    for j := 0 to n - i - 2 do
    begin
      node_j := GetComponentNodeByIndex(list, j);
      node_j.next := GetComponentNodeByIndex(list, j + 1);
      
      if CompareStr(node_j^.data.manufacturer_name, node_j.next^.data.manufacturer_name) > 0 then
      begin
        SwapComponents(node_j^.data, node_j.next^.data);
      end;
    end;
  end;
end;

// Реализация сортировки списка комплектующих по модели
procedure SortComponentsByModel(var list: TComponentList);
var
  i, j, n: Integer;
  node_i, node_j: TComponentList;
begin
  n := GetComponentListLength(list);
  
  if n <= 1 then
    Exit;
  
  // Сортировка пузырьком
  for i := 0 to n - 2 do
  begin
    for j := 0 to n - i - 2 do
    begin
      node_j := GetComponentNodeByIndex(list, j);
      node_j.next := GetComponentNodeByIndex(list, j + 1);
      
      if CompareStr(node_j^.data.model_name, node_j.next^.data.model_name) > 0 then
      begin
        SwapComponents(node_j^.data, node_j.next^.data);
      end;
    end;
  end;
end;

// Реализация сортировки списка комплектующих по типу
procedure SortComponentsByType(var list: TComponentList);
var
  i, j, n: Integer;
  node_i, node_j: TComponentList;
begin
  n := GetComponentListLength(list);
  
  if n <= 1 then
    Exit;
  
  // Сортировка пузырьком
  for i := 0 to n - 2 do
  begin
    for j := 0 to n - i - 2 do
    begin
      node_j := GetComponentNodeByIndex(list, j);
      node_j.next := GetComponentNodeByIndex(list, j + 1);
      
      if node_j^.data.cTypeId > node_j.next^.data.cTypeId then
      begin
        SwapComponents(node_j^.data, node_j.next^.data);
      end;
    end;
  end;
end;

// Реализация сортировки списка компьютерных сборок по цене
procedure SortComputerSetsByPrice(var list: TComputerSetList; ascending: Boolean);
var
  i, j, n: Integer;
  node_i, node_j: TComputerSetList;
begin
  n := GetComputerSetListLength(list);
  
  if n <= 1 then
    Exit;
  
  // Сортировка пузырьком
  for i := 0 to n - 2 do
  begin
    for j := 0 to n - i - 2 do
    begin
      node_j := GetComputerSetNodeByIndex(list, j);
      node_j.next := GetComputerSetNodeByIndex(list, j + 1);
      
      if (ascending and (node_j^.data.totalCost > node_j.next^.data.totalCost)) or
         (not ascending and (node_j^.data.totalCost < node_j.next^.data.totalCost)) then
      begin
        SwapComputerSets(node_j^.data, node_j.next^.data);
      end;
    end;
  end;
end;

end.
