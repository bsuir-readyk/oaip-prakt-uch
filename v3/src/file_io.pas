unit file_io;

interface

uses
  SysUtils, types, list_utils;

// Функции для работы с файлом типов комплектующих
procedure LoadComponentTypes(var list: TComponentTypeList; const filename: string);
procedure SaveComponentTypes(const list: TComponentTypeList; const filename: string);

// Функции для работы с файлом комплектующих
procedure LoadComponents(var list: TComponentList; const filename: string);
procedure SaveComponents(const list: TComponentList; const filename: string);

// Функции для работы с файлом совместимости
procedure LoadCompatibility(var list: TCompatibilityList; const filename: string);
procedure SaveCompatibility(const list: TCompatibilityList; const filename: string);

// Функции для работы с файлом заказов
procedure LoadOrders(var list: TOrderList; const filename: string);
procedure SaveOrders(const list: TOrderList; const filename: string);

implementation

// Реализация функций для работы с файлом типов комплектующих
procedure LoadComponentTypes(var list: TComponentTypeList; const filename: string);
var
  f: file of TComponentType;
  data: TComponentType;
  newData: TComponentType;
begin
  // Очистка списка перед загрузкой
  ClearComponentTypeList(list);
  
  // Проверка существования файла
  if not FileExists(filename) then
    Exit;
  
  try
    // Открытие файла для чтения
    AssignFile(f, filename);
    Reset(f);
    
    // Чтение данных из файла
    while not Eof(f) do
    begin
      Read(f, data);
      
      // Преобразование фиксированной строки в обычную
      newData.id := data.id;
      newData.name := FixedToString(data.name);
      
      AddComponentType(list, newData);
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при загрузке типов комплектующих: ', E.Message);
  end;
end;

procedure SaveComponentTypes(const list: TComponentTypeList; const filename: string);
var
  f: file of TComponentType;
  current: TComponentTypeList;
  fileData: TComponentType;
begin
  try
    // Открытие файла для записи
    AssignFile(f, filename);
    Rewrite(f);
    
    // Запись данных в файл
    current := list;
    while current <> nil do
    begin
      // Преобразование обычной строки в фиксированную
      fileData.id := current^.data.id;
      fileData.name := StringToFixed(current^.data.name);
      
      Write(f, fileData);
      current := current^.next;
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при сохранении типов комплектующих: ', E.Message);
  end;
end;

// Реализация функций для работы с файлом комплектующих
procedure LoadComponents(var list: TComponentList; const filename: string);
var
  f: file of TComponent;
  data: TComponent;
  newData: TComponent;
begin
  // Очистка списка перед загрузкой
  ClearComponentList(list);
  
  // Проверка существования файла
  if not FileExists(filename) then
    Exit;
  
  try
    // Открытие файла для чтения
    AssignFile(f, filename);
    Reset(f);
    
    // Чтение данных из файла
    while not Eof(f) do
    begin
      Read(f, data);
      
      // Преобразование фиксированных строк в обычные
      newData.id := data.id;
      newData.cTypeId := data.cTypeId;
      newData.manufacturer_name := FixedToString(data.manufacturer_name);
      newData.model_name := FixedToString(data.model_name);
      newData.description := FixedToString(data.description);
      newData.cost := data.cost;
      newData.availability := data.availability;
      
      AddComponent(list, newData);
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при загрузке комплектующих: ', E.Message);
  end;
end;

procedure SaveComponents(const list: TComponentList; const filename: string);
var
  f: file of TComponent;
  current: TComponentList;
  fileData: TComponent;
begin
  try
    // Открытие файла для записи
    AssignFile(f, filename);
    Rewrite(f);
    
    // Запись данных в файл
    current := list;
    while current <> nil do
    begin
      // Преобразование обычных строк в фиксированные
      fileData.id := current^.data.id;
      fileData.cTypeId := current^.data.cTypeId;
      fileData.manufacturer_name := StringToFixed(current^.data.manufacturer_name);
      fileData.model_name := StringToFixed(current^.data.model_name);
      fileData.description := StringToFixed(current^.data.description);
      fileData.cost := current^.data.cost;
      fileData.availability := current^.data.availability;
      
      Write(f, fileData);
      current := current^.next;
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при сохранении комплектующих: ', E.Message);
  end;
end;

// Реализация функций для работы с файлом совместимости
procedure LoadCompatibility(var list: TCompatibilityList; const filename: string);
var
  f: file of TCompatibility;
  data: TCompatibility;
begin
  // Очистка списка перед загрузкой
  ClearCompatibilityList(list);
  
  // Проверка существования файла
  if not FileExists(filename) then
    Exit;
  
  try
    // Открытие файла для чтения
    AssignFile(f, filename);
    Reset(f);
    
    // Чтение данных из файла
    while not Eof(f) do
    begin
      Read(f, data);
      AddCompatibility(list, data);
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при загрузке совместимости: ', E.Message);
  end;
end;

procedure SaveCompatibility(const list: TCompatibilityList; const filename: string);
var
  f: file of TCompatibility;
  current: TCompatibilityList;
begin
  try
    // Открытие файла для записи
    AssignFile(f, filename);
    Rewrite(f);
    
    // Запись данных в файл
    current := list;
    while current <> nil do
    begin
      Write(f, current^.data);
      current := current^.next;
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при сохранении совместимости: ', E.Message);
  end;
end;

// Реализация функций для работы с файлом заказов
procedure LoadOrders(var list: TOrderList; const filename: string);
var
  f: file of TFileOrder;
  fileData: TFileOrder;
  newData: TOrder;
  componentsList: TComponentList;
begin
  // Очистка списка перед загрузкой
  ClearOrderList(list);
  
  // Проверка существования файла
  if not FileExists(filename) then
    Exit;
  
  try
    // Открытие файла для чтения
    AssignFile(f, filename);
    Reset(f);
    
    // Чтение данных из файла
    while not Eof(f) do
    begin
      Read(f, fileData);
      
      // Преобразование данных из файла в данные для программы
      newData.id := fileData.id;
      newData.orderDate := fileData.orderDate;
      newData.customerName := FixedToString(fileData.customerName);
      newData.customerPhone := FixedToString(fileData.customerPhone);
      newData.customerEmail := FixedToString(fileData.customerEmail);
      newData.status := FixedToString(fileData.status);
      
      // Компьютерная сборка будет заполнена позже при загрузке компонентов
      
      AddOrder(list, newData);
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при загрузке заказов: ', E.Message);
  end;
end;

procedure SaveOrders(const list: TOrderList; const filename: string);
var
  f: file of TFileOrder;
  current: TOrderList;
  fileData: TFileOrder;
begin
  try
    // Открытие файла для записи
    AssignFile(f, filename);
    Rewrite(f);
    
    // Запись данных в файл
    current := list;
    while current <> nil do
    begin
      // Преобразование данных из программы в данные для файла
      fileData.id := current^.data.id;
      fileData.orderDate := current^.data.orderDate;
      fileData.customerName := StringToFixed(current^.data.customerName);
      fileData.customerPhone := StringToFixed(current^.data.customerPhone);
      fileData.customerEmail := StringToFixed(current^.data.customerEmail);
      fileData.status := StringToFixed(current^.data.status);
      
      // Сохранение идентификаторов комплектующих
      if current^.data.computerSet.cpu <> nil then
        fileData.computerSet.cpu_id := current^.data.computerSet.cpu^.id
      else
        fileData.computerSet.cpu_id := 0;
      
      if current^.data.computerSet.motherboard <> nil then
        fileData.computerSet.motherboard_id := current^.data.computerSet.motherboard^.id
      else
        fileData.computerSet.motherboard_id := 0;
      
      if current^.data.computerSet.ram <> nil then
        fileData.computerSet.ram_id := current^.data.computerSet.ram^.id
      else
        fileData.computerSet.ram_id := 0;
      
      if current^.data.computerSet.psu <> nil then
        fileData.computerSet.psu_id := current^.data.computerSet.psu^.id
      else
        fileData.computerSet.psu_id := 0;
      
      if current^.data.computerSet.storage <> nil then
        fileData.computerSet.storage_id := current^.data.computerSet.storage^.id
      else
        fileData.computerSet.storage_id := 0;
      
      fileData.computerSet.totalCost := current^.data.computerSet.totalCost;
      
      Write(f, fileData);
      current := current^.next;
    end;
    
    // Закрытие файла
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при сохранении заказов: ', E.Message);
  end;
end;

end.
