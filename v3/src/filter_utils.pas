unit filter_utils;

interface

uses
  SysUtils, types, list_utils;

// Фильтрация списка комплектующих
function FilterComponentsByType(const list: TComponentList; typeId: Integer): TComponentList;
function FilterComponentsByPriceRange(const list: TComponentList; minPrice, maxPrice: Real): TComponentList;
function FilterComponentsByManufacturer(const list: TComponentList; const manufacturer: string): TComponentList;
function FilterComponentsByAvailability(const list: TComponentList; available: Boolean): TComponentList;

// Фильтрация списка компьютерных сборок
function FilterComputerSetsByPriceRange(const list: TComputerSetList; minPrice, maxPrice: Real): TComputerSetList;

// Поиск совместимых комплектующих
function FindCompatibleComponents(const componentsList: TComponentList; const compatibilityList: TCompatibilityList; componentId: Integer; typeId: Integer): TComponentList;

implementation

// Реализация фильтрации списка комплектующих по типу
function FilterComponentsByType(const list: TComponentList; typeId: Integer): TComponentList;
var
  current: TComponentList;
  result_list: TComponentList;
begin
  result_list := nil;
  current := list;
  
  while current <> nil do
  begin
    if current^.data.cTypeId = typeId then
      AddComponent(result_list, current^.data);
    
    current := current^.next;
  end;
  
  Result := result_list;
end;

// Реализация фильтрации списка комплектующих по диапазону цен
function FilterComponentsByPriceRange(const list: TComponentList; minPrice, maxPrice: Real): TComponentList;
var
  current: TComponentList;
  result_list: TComponentList;
begin
  result_list := nil;
  current := list;
  
  while current <> nil do
  begin
    if (current^.data.cost >= minPrice) and (current^.data.cost <= maxPrice) then
      AddComponent(result_list, current^.data);
    
    current := current^.next;
  end;
  
  Result := result_list;
end;

// Реализация фильтрации списка комплектующих по производителю
function FilterComponentsByManufacturer(const list: TComponentList; const manufacturer: string): TComponentList;
var
  current: TComponentList;
  result_list: TComponentList;
begin
  result_list := nil;
  current := list;
  
  while current <> nil do
  begin
    if Pos(LowerCase(manufacturer), LowerCase(current^.data.manufacturer_name)) > 0 then
      AddComponent(result_list, current^.data);
    
    current := current^.next;
  end;
  
  Result := result_list;
end;

// Реализация фильтрации списка комплектующих по наличию
function FilterComponentsByAvailability(const list: TComponentList; available: Boolean): TComponentList;
var
  current: TComponentList;
  result_list: TComponentList;
begin
  result_list := nil;
  current := list;
  
  while current <> nil do
  begin
    if (available and (current^.data.availability > 0)) or
       (not available and (current^.data.availability = 0)) then
      AddComponent(result_list, current^.data);
    
    current := current^.next;
  end;
  
  Result := result_list;
end;

// Реализация фильтрации списка компьютерных сборок по диапазону цен
function FilterComputerSetsByPriceRange(const list: TComputerSetList; minPrice, maxPrice: Real): TComputerSetList;
var
  current: TComputerSetList;
  result_list: TComputerSetList;
begin
  result_list := nil;
  current := list;
  
  while current <> nil do
  begin
    if (current^.data.totalCost >= minPrice) and (current^.data.totalCost <= maxPrice) then
      AddComputerSet(result_list, current^.data);
    
    current := current^.next;
  end;
  
  Result := result_list;
end;

// Функция для проверки совместимости двух комплектующих
function AreComponentsCompatible(const compatibilityList: TCompatibilityList; comp1Id, comp2Id: Integer): Boolean;
var
  current: TCompatibilityList;
begin
  Result := False;
  current := compatibilityList;
  
  while current <> nil do
  begin
    if ((current^.data.left_id = comp1Id) and (current^.data.right_id = comp2Id)) or
       ((current^.data.left_id = comp2Id) and (current^.data.right_id = comp1Id)) then
    begin
      Result := True;
      Exit;
    end;
    
    current := current^.next;
  end;
end;

// Реализация поиска совместимых комплектующих
function FindCompatibleComponents(const componentsList: TComponentList; const compatibilityList: TCompatibilityList; componentId: Integer; typeId: Integer): TComponentList;
var
  current: TComponentList;
  result_list: TComponentList;
begin
  result_list := nil;
  
  // Сначала фильтруем по типу
  current := FilterComponentsByType(componentsList, typeId);
  
  // Затем проверяем совместимость
  while current <> nil do
  begin
    if (current^.data.id <> componentId) and AreComponentsCompatible(compatibilityList, componentId, current^.data.id) then
      AddComponent(result_list, current^.data);
    
    current := current^.next;
  end;
  
  Result := result_list;
end;

end.
