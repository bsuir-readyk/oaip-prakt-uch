unit DynamicLists;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DataTypes;

{ Функции для работы со списком типов комплектующих }
procedure InitComponentTypeList(var List: TComponentTypeList);
function AddComponentType(var List: TComponentTypeList; const Data: TComponentType): Boolean;
function RemoveComponentType(var List: TComponentTypeList; TypeCode: Integer): Boolean;
function FindComponentType(const List: TComponentTypeList; TypeCode: Integer): PComponentTypeNode;
procedure ClearComponentTypeList(var List: TComponentTypeList);

{ Функции для работы со списком комплектующих }
procedure InitComponentList(var List: TComponentList);
function AddComponent(var List: TComponentList; const Data: TComponent): Boolean;
function RemoveComponent(var List: TComponentList; Code: Integer): Boolean;
function FindComponent(const List: TComponentList; Code: Integer): PComponentNode;
procedure ClearComponentList(var List: TComponentList);

{ Функции для работы со списком совместимости }
procedure InitCompatibilityList(var List: TCompatibilityList);
function AddCompatibility(var List: TCompatibilityList; const Data: TCompatibility): Boolean;
function RemoveCompatibility(var List: TCompatibilityList; ComponentCode1, ComponentCode2: Integer): Boolean;
function FindCompatibility(const List: TCompatibilityList; ComponentCode1, ComponentCode2: Integer): PCompatibilityNode;
procedure ClearCompatibilityList(var List: TCompatibilityList);

{ Функции для работы со списком вариантов комплектации }
procedure InitPCBuildOptionList(var List: TPCBuildOptionList);
function AddPCBuildOption(var List: TPCBuildOptionList; const Data: TPCBuildOption): Boolean;
function RemovePCBuildOption(var List: TPCBuildOptionList; ID: Integer): Boolean;
function FindPCBuildOption(const List: TPCBuildOptionList; ID: Integer): PPCBuildOptionNode;
procedure ClearPCBuildOptionList(var List: TPCBuildOptionList);

{ Функции для работы со списком заказов }
procedure InitOrderList(var List: TOrderList);
function AddOrder(var List: TOrderList; const Data: TOrder): Boolean;
function RemoveOrder(var List: TOrderList; ID: Integer): Boolean;
function FindOrder(const List: TOrderList; ID: Integer): POrderNode;
procedure ClearOrderList(var List: TOrderList);

implementation

{ Реализация функций для работы со списком типов комплектующих }

procedure InitComponentTypeList(var List: TComponentTypeList);
begin
  List.Head := nil;
  List.Count := 0;
end;

function AddComponentType(var List: TComponentTypeList; const Data: TComponentType): Boolean;
var
  NewNode: PComponentTypeNode;
begin
  Result := False;
  
  // Проверяем, не существует ли уже тип с таким кодом
  if FindComponentType(List, Data.TypeCode) <> nil then
    Exit;
  
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := Data;
  NewNode^.Next := List.Head;
  
  // Добавляем в начало списка
  List.Head := NewNode;
  Inc(List.Count);
  
  Result := True;
end;

function RemoveComponentType(var List: TComponentTypeList; TypeCode: Integer): Boolean;
var
  Current, Previous: PComponentTypeNode;
begin
  Result := False;
  
  if List.Head = nil then
    Exit;
  
  // Если удаляемый элемент - первый в списке
  if List.Head^.Data.TypeCode = TypeCode then
  begin
    Current := List.Head;
    List.Head := List.Head^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
    Exit;
  end;
  
  // Ищем элемент в списке
  Previous := List.Head;
  Current := List.Head^.Next;
  
  while (Current <> nil) and (Current^.Data.TypeCode <> TypeCode) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если элемент найден
  if Current <> nil then
  begin
    Previous^.Next := Current^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
  end;
end;

function FindComponentType(const List: TComponentTypeList; TypeCode: Integer): PComponentTypeNode;
var
  Current: PComponentTypeNode;
begin
  Result := nil;
  Current := List.Head;
  
  while Current <> nil do
  begin
    if Current^.Data.TypeCode = TypeCode then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure ClearComponentTypeList(var List: TComponentTypeList);
var
  Current, Next: PComponentTypeNode;
begin
  Current := List.Head;
  
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  
  List.Head := nil;
  List.Count := 0;
end;

{ Реализация функций для работы со списком комплектующих }

procedure InitComponentList(var List: TComponentList);
begin
  List.Head := nil;
  List.Count := 0;
end;

function AddComponent(var List: TComponentList; const Data: TComponent): Boolean;
var
  NewNode: PComponentNode;
begin
  Result := False;
  
  // Проверяем, не существует ли уже комплектующая с таким кодом
  if FindComponent(List, Data.Code) <> nil then
    Exit;
  
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := Data;
  NewNode^.Next := List.Head;
  
  // Добавляем в начало списка
  List.Head := NewNode;
  Inc(List.Count);
  
  Result := True;
end;

function RemoveComponent(var List: TComponentList; Code: Integer): Boolean;
var
  Current, Previous: PComponentNode;
begin
  Result := False;
  
  if List.Head = nil then
    Exit;
  
  // Если удаляемый элемент - первый в списке
  if List.Head^.Data.Code = Code then
  begin
    Current := List.Head;
    List.Head := List.Head^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
    Exit;
  end;
  
  // Ищем элемент в списке
  Previous := List.Head;
  Current := List.Head^.Next;
  
  while (Current <> nil) and (Current^.Data.Code <> Code) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если элемент найден
  if Current <> nil then
  begin
    Previous^.Next := Current^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
  end;
end;

function FindComponent(const List: TComponentList; Code: Integer): PComponentNode;
var
  Current: PComponentNode;
begin
  Result := nil;
  Current := List.Head;
  
  while Current <> nil do
  begin
    if Current^.Data.Code = Code then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure ClearComponentList(var List: TComponentList);
var
  Current, Next: PComponentNode;
begin
  Current := List.Head;
  
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  
  List.Head := nil;
  List.Count := 0;
end;

{ Реализация функций для работы со списком совместимости }

procedure InitCompatibilityList(var List: TCompatibilityList);
begin
  List.Head := nil;
  List.Count := 0;
end;

function AddCompatibility(var List: TCompatibilityList; const Data: TCompatibility): Boolean;
var
  NewNode: PCompatibilityNode;
begin
  Result := False;
  
  // Проверяем, не существует ли уже такая запись о совместимости
  if FindCompatibility(List, Data.ComponentCode1, Data.ComponentCode2) <> nil then
    Exit;
  
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := Data;
  NewNode^.Next := List.Head;
  
  // Добавляем в начало списка
  List.Head := NewNode;
  Inc(List.Count);
  
  Result := True;
end;

function RemoveCompatibility(var List: TCompatibilityList; ComponentCode1, ComponentCode2: Integer): Boolean;
var
  Current, Previous: PCompatibilityNode;
begin
  Result := False;
  
  if List.Head = nil then
    Exit;
  
  // Если удаляемый элемент - первый в списке
  if (List.Head^.Data.ComponentCode1 = ComponentCode1) and 
     (List.Head^.Data.ComponentCode2 = ComponentCode2) then
  begin
    Current := List.Head;
    List.Head := List.Head^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
    Exit;
  end;
  
  // Ищем элемент в списке
  Previous := List.Head;
  Current := List.Head^.Next;
  
  while (Current <> nil) and 
        ((Current^.Data.ComponentCode1 <> ComponentCode1) or 
         (Current^.Data.ComponentCode2 <> ComponentCode2)) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если элемент найден
  if Current <> nil then
  begin
    Previous^.Next := Current^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
  end;
end;

function FindCompatibility(const List: TCompatibilityList; ComponentCode1, ComponentCode2: Integer): PCompatibilityNode;
var
  Current: PCompatibilityNode;
begin
  Result := nil;
  Current := List.Head;
  
  while Current <> nil do
  begin
    if (Current^.Data.ComponentCode1 = ComponentCode1) and 
       (Current^.Data.ComponentCode2 = ComponentCode2) then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure ClearCompatibilityList(var List: TCompatibilityList);
var
  Current, Next: PCompatibilityNode;
begin
  Current := List.Head;
  
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  
  List.Head := nil;
  List.Count := 0;
end;

{ Реализация функций для работы со списком вариантов комплектации }

procedure InitPCBuildOptionList(var List: TPCBuildOptionList);
begin
  List.Head := nil;
  List.Count := 0;
end;

function AddPCBuildOption(var List: TPCBuildOptionList; const Data: TPCBuildOption): Boolean;
var
  NewNode: PPCBuildOptionNode;
begin
  Result := False;
  
  // Проверяем, не существует ли уже вариант с таким ID
  if FindPCBuildOption(List, Data.ID) <> nil then
    Exit;
  
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := Data;
  NewNode^.Next := List.Head;
  
  // Добавляем в начало списка
  List.Head := NewNode;
  Inc(List.Count);
  
  Result := True;
end;

function RemovePCBuildOption(var List: TPCBuildOptionList; ID: Integer): Boolean;
var
  Current, Previous: PPCBuildOptionNode;
begin
  Result := False;
  
  if List.Head = nil then
    Exit;
  
  // Если удаляемый элемент - первый в списке
  if List.Head^.Data.ID = ID then
  begin
    Current := List.Head;
    List.Head := List.Head^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
    Exit;
  end;
  
  // Ищем элемент в списке
  Previous := List.Head;
  Current := List.Head^.Next;
  
  while (Current <> nil) and (Current^.Data.ID <> ID) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если элемент найден
  if Current <> nil then
  begin
    Previous^.Next := Current^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
  end;
end;

function FindPCBuildOption(const List: TPCBuildOptionList; ID: Integer): PPCBuildOptionNode;
var
  Current: PPCBuildOptionNode;
begin
  Result := nil;
  Current := List.Head;
  
  while Current <> nil do
  begin
    if Current^.Data.ID = ID then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure ClearPCBuildOptionList(var List: TPCBuildOptionList);
var
  Current, Next: PPCBuildOptionNode;
begin
  Current := List.Head;
  
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  
  List.Head := nil;
  List.Count := 0;
end;

{ Реализация функций для работы со списком заказов }

procedure InitOrderList(var List: TOrderList);
begin
  List.Head := nil;
  List.Count := 0;
end;

function AddOrder(var List: TOrderList; const Data: TOrder): Boolean;
var
  NewNode: POrderNode;
begin
  Result := False;
  
  // Проверяем, не существует ли уже заказ с таким ID
  if FindOrder(List, Data.ID) <> nil then
    Exit;
  
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := Data;
  NewNode^.Next := List.Head;
  
  // Добавляем в начало списка
  List.Head := NewNode;
  Inc(List.Count);
  
  Result := True;
end;

function RemoveOrder(var List: TOrderList; ID: Integer): Boolean;
var
  Current, Previous: POrderNode;
begin
  Result := False;
  
  if List.Head = nil then
    Exit;
  
  // Если удаляемый элемент - первый в списке
  if List.Head^.Data.ID = ID then
  begin
    Current := List.Head;
    List.Head := List.Head^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
    Exit;
  end;
  
  // Ищем элемент в списке
  Previous := List.Head;
  Current := List.Head^.Next;
  
  while (Current <> nil) and (Current^.Data.ID <> ID) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если элемент найден
  if Current <> nil then
  begin
    Previous^.Next := Current^.Next;
    Dispose(Current);
    Dec(List.Count);
    Result := True;
  end;
end;

function FindOrder(const List: TOrderList; ID: Integer): POrderNode;
var
  Current: POrderNode;
begin
  Result := nil;
  Current := List.Head;
  
  while Current <> nil do
  begin
    if Current^.Data.ID = ID then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure ClearOrderList(var List: TOrderList);
var
  Current, Next: POrderNode;
begin
  Current := List.Head;
  
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  
  List.Head := nil;
  List.Count := 0;
end;

end.
