unit ComponentTypes;

interface

uses
  SysUtils, Types, UI;

// Инициализация списка типов комплектующих
procedure InitComponentTypesList;

// Освобождение памяти, занятой списком типов комплектующих
procedure FreeComponentTypesList;

// Добавление типа комплектующей в список
procedure AddComponentType(const ComponentType: TComponentType);

// Удаление типа комплектующей из списка по коду
function DeleteComponentType(Code: Integer): Boolean;

// Редактирование типа комплектующей
function EditComponentType(const ComponentType: TComponentType): Boolean;

// Поиск типа комплектующей по коду
function FindComponentTypeByCode(Code: Integer): PComponentTypeNode;

// Поиск типа комплектующей по названию
function FindComponentTypeByName(const Name: string): PComponentTypeNode;

// Получение нового уникального кода для типа комплектующей
function GetNewComponentTypeCode: Integer;

// Вывод списка типов комплектующих
procedure PrintComponentTypes(List: PComponentTypeNode);

implementation

// Инициализация списка типов комплектующих
procedure InitComponentTypesList;
begin
  DataLists.ComponentTypes := nil;
end;

// Освобождение памяти, занятой списком типов комплектующих
procedure FreeComponentTypesList;
var
  Current, Temp: PComponentTypeNode;
begin
  Current := DataLists.ComponentTypes;
  while Current <> nil do
  begin
    Temp := Current;
    Current := Current^.Next;
    Dispose(Temp);
  end;
  DataLists.ComponentTypes := nil;
end;

// Добавление типа комплектующей в список
procedure AddComponentType(const ComponentType: TComponentType);
var
  NewNode: PComponentTypeNode;
begin
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := ComponentType;
  NewNode^.Next := nil;
  
  // Добавляем узел в начало списка
  if DataLists.ComponentTypes = nil then
    DataLists.ComponentTypes := NewNode
  else
  begin
    NewNode^.Next := DataLists.ComponentTypes;
    DataLists.ComponentTypes := NewNode;
  end;
end;

// Удаление типа комплектующей из списка по коду
function DeleteComponentType(Code: Integer): Boolean;
var
  Current, Previous: PComponentTypeNode;
  Found: Boolean;
begin
  Current := DataLists.ComponentTypes;
  Previous := nil;
  Found := False;
  
  // Ищем тип комплектующей с заданным кодом
  while (Current <> nil) and (Current^.Data.Code <> Code) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если тип комплектующей найден, удаляем его
  if Current <> nil then
  begin
    if Previous = nil then
      DataLists.ComponentTypes := Current^.Next
    else
      Previous^.Next := Current^.Next;
    
    Dispose(Current);
    Found := True;
  end;
  
  DeleteComponentType := Found;
end;

// Редактирование типа комплектующей
function EditComponentType(const ComponentType: TComponentType): Boolean;
var
  Current: PComponentTypeNode;
  Found: Boolean;
begin
  Current := FindComponentTypeByCode(ComponentType.Code);
  Found := False;
  
  if Current <> nil then
  begin
    Current^.Data := ComponentType;
    Found := True;
  end;
  
  EditComponentType := Found;
end;

// Поиск типа комплектующей по коду
function FindComponentTypeByCode(Code: Integer): PComponentTypeNode;
var
  Current: PComponentTypeNode;
begin
  Current := DataLists.ComponentTypes;
  
  while (Current <> nil) and (Current^.Data.Code <> Code) do
    Current := Current^.Next;
  
  FindComponentTypeByCode := Current;
end;

// Поиск типа комплектующей по названию
function FindComponentTypeByName(const Name: string): PComponentTypeNode;
var
  Current: PComponentTypeNode;
begin
  Current := DataLists.ComponentTypes;
  
  while (Current <> nil) and (Current^.Data.Name <> Name) do
    Current := Current^.Next;
  
  FindComponentTypeByName := Current;
end;

// Получение нового уникального кода для типа комплектующей
function GetNewComponentTypeCode: Integer;
var
  Current: PComponentTypeNode;
  MaxCode: Integer;
begin
  MaxCode := 0;
  Current := DataLists.ComponentTypes;
  
  while Current <> nil do
  begin
    if Current^.Data.Code > MaxCode then
      MaxCode := Current^.Data.Code;
    
    Current := Current^.Next;
  end;
  
  GetNewComponentTypeCode := MaxCode + 1;
end;

// Вывод списка типов комплектующих
procedure PrintComponentTypes(List: PComponentTypeNode);
var
  Current: PComponentTypeNode;
  ColumnWidths: array[0..2] of Integer;
  Alignments: array[0..2] of Char;
  Values: array[0..2] of string;
  TypeCount: Integer;
begin
  Current := List;
  
  WriteLn('Список типов комплектующих:');
  
  if Current = nil then
    WriteLn('Список пуст')
  else
  begin
    // Определяем ширину столбцов
    ColumnWidths[0] := 5;  // Код
    ColumnWidths[1] := 5;  // Код
    ColumnWidths[2] := 30; // Название
    
    // Определяем выравнивание столбцов
    Alignments[0] := 'R'; // Код - по правому краю
    Alignments[1] := 'C'; // Код - по правому краю
    Alignments[2] := 'L'; // Название - по левому краю
    
    // Выводим заголовок таблицы
    PrintTableHorizontalLine(ColumnWidths, 'T');
    
    Values[0] := 'Code';
    Values[1] := 'Code';
    Values[2] := 'Name';
    PrintTableRow(Values, ColumnWidths, Alignments);
    
    PrintTableHorizontalLine(ColumnWidths, 'M');
    
    // Выводим данные
    TypeCount := 0;
    while Current <> nil do
    begin
      with Current^.Data do
      begin
        Values[0] := IntToStr(Code);
        Values[1] := IntToStr(Code);
        Values[2] := Name;
        
        PrintTableRow(Values, ColumnWidths, Alignments);
      end;
      
      Current := Current^.Next;
      Inc(TypeCount);
    end;
    
    PrintTableHorizontalLine(ColumnWidths, 'B');
    WriteLn('Всего типов комплектующих: ', TypeCount);
  end;
end;

end.
