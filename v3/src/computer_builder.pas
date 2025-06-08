unit computer_builder;

interface

uses
  SysUtils, types, list_utils, filter_utils;

// Проверка совместимости комплектующих
function AreComponentsCompatible(const compatibilityList: TCompatibilityList; comp1Id, comp2Id: Integer): Boolean;

// Создание всех возможных вариантов компьютерных сборок
function BuildAllPossibleComputerSets(
  const componentsList: TComponentList;
  const componentTypesList: TComponentTypeList;
  const compatibilityList: TCompatibilityList;
  minPrice, maxPrice: Real
): TComputerSetList;

// Создание компьютерной сборки
function CreateComputerSet(
  cpu, motherboard, ram, psu, storage: PComponent
): TComputerSet;

// Расчет общей стоимости компьютерной сборки
function CalculateTotalCost(const computerSet: TComputerSet): Real;

implementation

// Проверка совместимости комплектующих
function AreComponentsCompatible(const compatibilityList: TCompatibilityList; comp1Id, comp2Id: Integer): Boolean;
var
  current: TCompatibilityList;
begin
  Result := False;
  
  // Если один из идентификаторов равен 0, считаем, что комплектующая не выбрана
  if (comp1Id = 0) or (comp2Id = 0) then
  begin
    Result := True;
    Exit;
  end;
  
  // Если идентификаторы одинаковые, комплектующие не совместимы
  if comp1Id = comp2Id then
    Exit;
  
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

// Функция для получения списка комплектующих определенного типа
function GetComponentsByType(const componentsList: TComponentList; typeId: Integer): TComponentList;
begin
  Result := FilterComponentsByType(componentsList, typeId);
end;

// Функция для получения идентификатора типа по названию
function GetTypeIdByName(const componentTypesList: TComponentTypeList; const typeName: string): Integer;
var
  current: TComponentTypeList;
begin
  Result := 0;
  current := componentTypesList;
  
  while current <> nil do
  begin
    if CompareText(current^.data.name, typeName) = 0 then
    begin
      Result := current^.data.id;
      Exit;
    end;
    
    current := current^.next;
  end;
end;

// Создание компьютерной сборки
function CreateComputerSet(
  cpu, motherboard, ram, psu, storage: PComponent
): TComputerSet;
begin
  Result.cpu := cpu;
  Result.motherboard := motherboard;
  Result.ram := ram;
  Result.psu := psu;
  Result.storage := storage;
  Result.totalCost := CalculateTotalCost(Result);
end;

// Расчет общей стоимости компьютерной сборки
function CalculateTotalCost(const computerSet: TComputerSet): Real;
begin
  Result := 0;
  
  if computerSet.cpu <> nil then
    Result := Result + computerSet.cpu^.cost;
  
  if computerSet.motherboard <> nil then
    Result := Result + computerSet.motherboard^.cost;
  
  if computerSet.ram <> nil then
    Result := Result + computerSet.ram^.cost;
  
  if computerSet.psu <> nil then
    Result := Result + computerSet.psu^.cost;
  
  if computerSet.storage <> nil then
    Result := Result + computerSet.storage^.cost;
end;

// Рекурсивная функция для создания всех возможных вариантов компьютерных сборок
procedure BuildComputerSetsRecursive(
  var resultList: TComputerSetList;
  const cpuList, motherboardList, ramList, psuList, storageList: TComponentList;
  const compatibilityList: TCompatibilityList;
  minPrice, maxPrice: Real;
  currentSet: TComputerSet;
  step: Integer
);
var
  current: TComponentList;
  newSet: TComputerSet;
  totalCost: Real;
begin
  case step of
    0: begin // Выбор процессора
      current := cpuList;
      while current <> nil do
      begin
        newSet := currentSet;
        newSet.cpu := @current^.data;
        
        BuildComputerSetsRecursive(
          resultList, cpuList, motherboardList, ramList, psuList, storageList,
          compatibilityList, minPrice, maxPrice, newSet, 1
        );
        
        current := current^.next;
      end;
    end;
    
    1: begin // Выбор материнской платы
      current := motherboardList;
      while current <> nil do
      begin
        // Проверка совместимости с процессором
        if AreComponentsCompatible(compatibilityList, currentSet.cpu^.id, current^.data.id) then
        begin
          newSet := currentSet;
          newSet.motherboard := @current^.data;
          
          BuildComputerSetsRecursive(
            resultList, cpuList, motherboardList, ramList, psuList, storageList,
            compatibilityList, minPrice, maxPrice, newSet, 2
          );
        end;
        
        current := current^.next;
      end;
    end;
    
    2: begin // Выбор оперативной памяти
      current := ramList;
      while current <> nil do
      begin
        // Проверка совместимости с материнской платой
        if AreComponentsCompatible(compatibilityList, currentSet.motherboard^.id, current^.data.id) then
        begin
          newSet := currentSet;
          newSet.ram := @current^.data;
          
          BuildComputerSetsRecursive(
            resultList, cpuList, motherboardList, ramList, psuList, storageList,
            compatibilityList, minPrice, maxPrice, newSet, 3
          );
        end;
        
        current := current^.next;
      end;
    end;
    
    3: begin // Выбор блока питания
      current := psuList;
      while current <> nil do
      begin
        newSet := currentSet;
        newSet.psu := @current^.data;
        
        BuildComputerSetsRecursive(
          resultList, cpuList, motherboardList, ramList, psuList, storageList,
          compatibilityList, minPrice, maxPrice, newSet, 4
        );
        
        current := current^.next;
      end;
    end;
    
    4: begin // Выбор накопителя
      current := storageList;
      while current <> nil do
      begin
        // Проверка совместимости с материнской платой
        if AreComponentsCompatible(compatibilityList, currentSet.motherboard^.id, current^.data.id) then
        begin
          newSet := currentSet;
          newSet.storage := @current^.data;
          
          // Расчет общей стоимости
          newSet.totalCost := CalculateTotalCost(newSet);
          
          // Проверка ценового диапазона
          if (newSet.totalCost >= minPrice) and (newSet.totalCost <= maxPrice) then
          begin
            // Добавление сборки в результирующий список
            AddComputerSet(resultList, newSet);
          end;
        end;
        
        current := current^.next;
      end;
    end;
  end;
end;

// Создание всех возможных вариантов компьютерных сборок
function BuildAllPossibleComputerSets(
  const componentsList: TComponentList;
  const componentTypesList: TComponentTypeList;
  const compatibilityList: TCompatibilityList;
  minPrice, maxPrice: Real
): TComputerSetList;
var
  cpuList, motherboardList, ramList, psuList, storageList: TComponentList;
  resultList: TComputerSetList;
  emptySet: TComputerSet;
  cpuTypeId, motherboardTypeId, ramTypeId, psuTypeId, storageTypeId: Integer;
begin
  resultList := nil;
  
  // Получение идентификаторов типов комплектующих
  cpuTypeId := GetTypeIdByName(componentTypesList, 'Процессор');
  motherboardTypeId := GetTypeIdByName(componentTypesList, 'Материнская плата');
  ramTypeId := GetTypeIdByName(componentTypesList, 'Оперативная память');
  psuTypeId := GetTypeIdByName(componentTypesList, 'Блок питания');
  storageTypeId := GetTypeIdByName(componentTypesList, 'Накопитель');
  
  // Получение списков комплектующих по типам
  cpuList := GetComponentsByType(componentsList, cpuTypeId);
  motherboardList := GetComponentsByType(componentsList, motherboardTypeId);
  ramList := GetComponentsByType(componentsList, ramTypeId);
  psuList := GetComponentsByType(componentsList, psuTypeId);
  storageList := GetComponentsByType(componentsList, storageTypeId);
  
  // Инициализация пустой сборки
  emptySet.cpu := nil;
  emptySet.motherboard := nil;
  emptySet.ram := nil;
  emptySet.psu := nil;
  emptySet.storage := nil;
  emptySet.totalCost := 0;
  
  // Рекурсивное создание всех возможных вариантов сборок
  BuildComputerSetsRecursive(
    resultList, cpuList, motherboardList, ramList, psuList, storageList,
    compatibilityList, minPrice, maxPrice, emptySet, 0
  );
  
  // Освобождение памяти
  ClearComponentList(cpuList);
  ClearComponentList(motherboardList);
  ClearComponentList(ramList);
  ClearComponentList(psuList);
  ClearComponentList(storageList);
  
  Result := resultList;
end;

end.
