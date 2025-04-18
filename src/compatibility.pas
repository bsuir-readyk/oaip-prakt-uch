unit Compatibility;

interface

uses
  SysUtils, Types, Components, UI;

// Типы функций-компараторов
type
  // Компаратор для поиска записи о совместимости при удалении
  // Возвращает True, если запись о совместимости соответствует критерию удаления
  TCompatibilityDeleteComparator = function(const Compatibility: TCompatibility): Boolean;

// Инициализация списка совместимости
procedure InitCompatibilityList;

// Освобождение памяти, занятой списком совместимости
procedure FreeCompatibilityList;

// Добавление записи о совместимости в список
procedure AddCompatibility(const Compatibility: TCompatibility);

// Создание компаратора для удаления записи о совместимости по кодам компонентов
function CreateDeleteCompatibilityByCodesComparator(ComponentCode1, ComponentCode2: Integer): TCompatibilityDeleteComparator;

// Обобщенная функция удаления записи о совместимости с использованием компаратора
function DeleteCompatibilityWithComparator(var List: PCompatibilityNode; Comparator: TCompatibilityDeleteComparator): Boolean;

// Удаление записи о совместимости из списка
function DeleteCompatibility(ComponentCode1, ComponentCode2: Integer): Boolean;

// Проверка совместимости двух комплектующих
function AreComponentsCompatible(ComponentCode1, ComponentCode2: Integer): Boolean;

// Поиск всех совместимых комплектующих для заданной комплектующей
function FindCompatibleComponents(ComponentCode: Integer): PComponentNode;

// Поиск всех совместимых комплектующих заданного типа для заданной комплектующей
function FindCompatibleComponentsByType(ComponentCode, TypeCode: Integer): PComponentNode;

// Вывод списка совместимости
procedure PrintCompatibility(List: PCompatibilityNode);

implementation

// Инициализация списка совместимости
procedure InitCompatibilityList;
begin
  DataLists.Compatibilities := nil;
end;

// Освобождение памяти, занятой списком совместимости
procedure FreeCompatibilityList;
var
  Current, Temp: PCompatibilityNode;
begin
  Current := DataLists.Compatibilities;
  while Current <> nil do
  begin
    Temp := Current;
    Current := Current^.Next;
    Dispose(Temp);
  end;
  DataLists.Compatibilities := nil;
end;

// Добавление записи о совместимости в список
procedure AddCompatibility(const Compatibility: TCompatibility);
var
  NewNode: PCompatibilityNode;
  alreadyExists: Boolean;
begin
  // Проверяем, существует ли уже запись о совместимости
  alreadyExists := AreComponentsCompatible(Compatibility.ComponentCode1, Compatibility.ComponentCode2);
  
  if not alreadyExists then
  begin
    // Создаем новый узел
    New(NewNode);
    NewNode^.Data := Compatibility;
    NewNode^.Next := nil;
    
    // Добавляем узел в начало списка
    if DataLists.Compatibilities = nil then
      DataLists.Compatibilities := NewNode
    else
    begin
      NewNode^.Next := DataLists.Compatibilities;
      DataLists.Compatibilities := NewNode;
    end;
    
    // Добавляем обратную совместимость (если A совместим с B, то B совместим с A)
    if Compatibility.ComponentCode1 <> Compatibility.ComponentCode2 then
    begin
      New(NewNode);
      NewNode^.Data.ComponentCode1 := Compatibility.ComponentCode2;
      NewNode^.Data.ComponentCode2 := Compatibility.ComponentCode1;
      NewNode^.Next := DataLists.Compatibilities;
      DataLists.Compatibilities := NewNode;
    end;
  end;
end;

// Создание компаратора для удаления записи о совместимости по кодам компонентов
function CreateDeleteCompatibilityByCodesComparator(ComponentCode1, ComponentCode2: Integer): TCompatibilityDeleteComparator;

  // Локальная функция-компаратор для удаления по кодам компонентов
  function CompatibilityCodesComparator(const Compatibility: TCompatibility): Boolean;
  begin
    CompatibilityCodesComparator := (Compatibility.ComponentCode1 = ComponentCode1) and
              (Compatibility.ComponentCode2 = ComponentCode2);
  end;

begin
  CreateDeleteCompatibilityByCodesComparator := @CompatibilityCodesComparator;
end;

// Обобщенная функция удаления записи о совместимости с использованием компаратора
function DeleteCompatibilityWithComparator(var List: PCompatibilityNode; Comparator: TCompatibilityDeleteComparator): Boolean;
var
  Current, Previous: PCompatibilityNode;
  Found: Boolean;
begin
  Current := List;
  Previous := nil;
  Found := False;
  
  // Ищем запись о совместимости, соответствующую критерию компаратора
  while (Current <> nil) and (not Found) do
  begin
    if Comparator(Current^.Data) then
    begin
      if Previous = nil then
        List := Current^.Next
      else
        Previous^.Next := Current^.Next;
      
      Dispose(Current);
      Found := True;
    end
    else
    begin
      Previous := Current;
      Current := Current^.Next;
    end;
  end;
  
  Result := Found;
end;

// Удаление записи о совместимости из списка
function DeleteCompatibility(ComponentCode1, ComponentCode2: Integer): Boolean;
var
  Found, FoundForward, FoundReverse: Boolean;
begin
  // Удаляем запись о совместимости в прямом направлении
  FoundForward := DeleteCompatibilityWithComparator(
    DataLists.Compatibilities,
    CreateDeleteCompatibilityByCodesComparator(ComponentCode1, ComponentCode2)
  );
  
  // Удаляем запись о совместимости в обратном направлении
  FoundReverse := DeleteCompatibilityWithComparator(
    DataLists.Compatibilities,
    CreateDeleteCompatibilityByCodesComparator(ComponentCode2, ComponentCode1)
  );
  
  Found := FoundForward or FoundReverse;
  
  DeleteCompatibility := Found;
end;

// Проверка совместимости двух комплектующих
function AreComponentsCompatible(ComponentCode1, ComponentCode2: Integer): Boolean;
var
  Current: PCompatibilityNode;
  Found: Boolean;
begin
  Current := DataLists.Compatibilities;
  Found := False;
  
  while (Current <> nil) and (not Found) do
  begin
    if (Current^.Data.ComponentCode1 = ComponentCode1) and
       (Current^.Data.ComponentCode2 = ComponentCode2) then
      Found := True
    else
      Current := Current^.Next;
  end;
  
  AreComponentsCompatible := Found;
end;

// Поиск всех совместимых комплектующих для заданной комплектующей
function FindCompatibleComponents(ComponentCode: Integer): PComponentNode;
var
  Current: PCompatibilityNode;
  ResultList, Last: PComponentNode;
  Component: PComponentNode;
begin
  ResultList := nil;
  Last := nil;
  Current := DataLists.Compatibilities;
  
  while Current <> nil do
  begin
    if Current^.Data.ComponentCode1 = ComponentCode then
    begin
      Component := FindComponentByCode(Current^.Data.ComponentCode2);
      
      if Component <> nil then
      begin
        // Создаем копию узла
        New(Last);
        Last^.Data := Component^.Data;
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
    end;
    
    Current := Current^.Next;
  end;
  
  FindCompatibleComponents := ResultList;
end;

// Поиск всех совместимых комплектующих заданного типа для заданной комплектующей
function FindCompatibleComponentsByType(ComponentCode, TypeCode: Integer): PComponentNode;
var
  CompatibleComponents: PComponentNode;
  Current, ResultList, Last: PComponentNode;
begin
  ResultList := nil;
  Last := nil;
  
  // Получаем список всех совместимых комплектующих
  CompatibleComponents := FindCompatibleComponents(ComponentCode);
  Current := CompatibleComponents;
  
  // Фильтруем список по типу
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
  
  // Освобождаем память, занятую временным списком
  Current := CompatibleComponents;
  while Current <> nil do
  begin
    Last := Current;
    Current := Current^.Next;
    Dispose(Last);
  end;
  
  FindCompatibleComponentsByType := ResultList;
end;

// Вывод списка совместимости
procedure PrintCompatibility(List: PCompatibilityNode);
var
  Current: PCompatibilityNode;
  Component1, Component2: PComponentNode;
  ColumnWidths: array[0..3] of Integer;
  Alignments: array[0..3] of Char;
  Values: array[0..3] of string;
  CompatibilityCount: Integer;
begin
  Current := List;
  
  WriteLn('Список совместимости комплектующих:');
  
  if Current = nil then
    WriteLn('Список пуст')
  else
  begin
    // Определяем ширину столбцов
    ColumnWidths[0] := 5;  // Код 1
    ColumnWidths[1] := 25; // Комплектующая 1
    ColumnWidths[2] := 5;  // Код 2
    ColumnWidths[3] := 25; // Комплектующая 2
    
    // Определяем выравнивание столбцов
    Alignments[0] := 'R'; // Код 1 - по правому краю
    Alignments[1] := 'L'; // Комплектующая 1 - по левому краю
    Alignments[2] := 'R'; // Код 2 - по правому краю
    Alignments[3] := 'L'; // Комплектующая 2 - по левому краю
    
    // Выводим заголовок таблицы
    PrintTableHorizontalLine(ColumnWidths, 'T');
    
    Values[0] := 'Code';
    Values[1] := 'Component 1';
    Values[2] := 'Code';
    Values[3] := 'Component 2';
    PrintTableRow(Values, ColumnWidths, Alignments);
    
    PrintTableHorizontalLine(ColumnWidths, 'M');
    
    // Выводим данные
    CompatibilityCount := 0;
    while Current <> nil do
    begin
      Component1 := FindComponentByCode(Current^.Data.ComponentCode1);
      Component2 := FindComponentByCode(Current^.Data.ComponentCode2);
      
      if (Component1 <> nil) and (Component2 <> nil) then
      begin
        Values[0] := IntToStr(Current^.Data.ComponentCode1);
        Values[1] := Component1^.Data.Manufacturer + ' ' + Component1^.Data.Model;
        Values[2] := IntToStr(Current^.Data.ComponentCode2);
        Values[3] := Component2^.Data.Manufacturer + ' ' + Component2^.Data.Model;
        
        PrintTableRow(Values, ColumnWidths, Alignments);
        Inc(CompatibilityCount);
      end;
      
      Current := Current^.Next;
    end;
    
    PrintTableHorizontalLine(ColumnWidths, 'B');
    WriteLn('Всего записей о совместимости: ', CompatibilityCount);
  end;
end;

end.
