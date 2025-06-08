unit list_utils;

interface

uses
  SysUtils, types;

// Функции для работы со списком типов комплектующих
function CreateComponentTypeNode(const data: TComponentType): TComponentTypeList;
procedure AddComponentType(var list: TComponentTypeList; const data: TComponentType);
procedure RemoveComponentType(var list: TComponentTypeList; id: Integer);
function FindComponentType(const list: TComponentTypeList; id: Integer): PComponentType;
procedure ClearComponentTypeList(var list: TComponentTypeList);

// Функции для работы со списком комплектующих
function CreateComponentNode(const data: TComponent): TComponentList;
procedure AddComponent(var list: TComponentList; const data: TComponent);
procedure RemoveComponent(var list: TComponentList; id: Integer);
function FindComponent(const list: TComponentList; id: Integer): PComponent;
procedure ClearComponentList(var list: TComponentList);

// Функции для работы со списком совместимости
function CreateCompatibilityNode(const data: TCompatibility): TCompatibilityList;
procedure AddCompatibility(var list: TCompatibilityList; const data: TCompatibility);
procedure RemoveCompatibility(var list: TCompatibilityList; left_id, right_id: Integer);
function FindCompatibility(const list: TCompatibilityList; left_id, right_id: Integer): PCompatibility;
procedure ClearCompatibilityList(var list: TCompatibilityList);

// Функции для работы со списком компьютерных сборок
function CreateComputerSetNode(const data: TComputerSet): TComputerSetList;
procedure AddComputerSet(var list: TComputerSetList; const data: TComputerSet);
procedure ClearComputerSetList(var list: TComputerSetList);

// Функции для работы со списком заказов
function CreateOrderNode(const data: TOrder): TOrderList;
procedure AddOrder(var list: TOrderList; const data: TOrder);
procedure RemoveOrder(var list: TOrderList; id: Integer);
function FindOrder(const list: TOrderList; id: Integer): POrder;
procedure ClearOrderList(var list: TOrderList);

implementation

// Реализация функций для работы со списком типов комплектующих
function CreateComponentTypeNode(const data: TComponentType): TComponentTypeList;
var
  node: TComponentTypeList;
begin
  New(node);
  node^.data := data;
  node^.next := nil;
  Result := node;
end;

procedure AddComponentType(var list: TComponentTypeList; const data: TComponentType);
var
  node, current: TComponentTypeList;
begin
  node := CreateComponentTypeNode(data);
  
  if list = nil then
    list := node
  else
  begin
    current := list;
    while current^.next <> nil do
      current := current^.next;
    current^.next := node;
  end;
end;

procedure RemoveComponentType(var list: TComponentTypeList; id: Integer);
var
  current, prev: TComponentTypeList;
begin
  if list = nil then
    Exit;
  
  if list^.data.id = id then
  begin
    current := list;
    list := list^.next;
    Dispose(current);
  end
  else
  begin
    prev := list;
    current := list^.next;
    
    while (current <> nil) and (current^.data.id <> id) do
    begin
      prev := current;
      current := current^.next;
    end;
    
    if current <> nil then
    begin
      prev^.next := current^.next;
      Dispose(current);
    end;
  end;
end;

function FindComponentType(const list: TComponentTypeList; id: Integer): PComponentType;
var
  current: TComponentTypeList;
begin
  Result := nil;
  current := list;
  
  while current <> nil do
  begin
    if current^.data.id = id then
    begin
      Result := @current^.data;
      Exit;
    end;
    current := current^.next;
  end;
end;

procedure ClearComponentTypeList(var list: TComponentTypeList);
var
  current, temp: TComponentTypeList;
begin
  current := list;
  
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
  
  list := nil;
end;

// Реализация функций для работы со списком комплектующих
function CreateComponentNode(const data: TComponent): TComponentList;
var
  node: TComponentList;
begin
  New(node);
  node^.data := data;
  node^.next := nil;
  Result := node;
end;

procedure AddComponent(var list: TComponentList; const data: TComponent);
var
  node, current: TComponentList;
begin
  node := CreateComponentNode(data);
  
  if list = nil then
    list := node
  else
  begin
    current := list;
    while current^.next <> nil do
      current := current^.next;
    current^.next := node;
  end;
end;

procedure RemoveComponent(var list: TComponentList; id: Integer);
var
  current, prev: TComponentList;
begin
  if list = nil then
    Exit;
  
  if list^.data.id = id then
  begin
    current := list;
    list := list^.next;
    Dispose(current);
  end
  else
  begin
    prev := list;
    current := list^.next;
    
    while (current <> nil) and (current^.data.id <> id) do
    begin
      prev := current;
      current := current^.next;
    end;
    
    if current <> nil then
    begin
      prev^.next := current^.next;
      Dispose(current);
    end;
  end;
end;

function FindComponent(const list: TComponentList; id: Integer): PComponent;
var
  current: TComponentList;
begin
  Result := nil;
  current := list;
  
  while current <> nil do
  begin
    if current^.data.id = id then
    begin
      Result := @current^.data;
      Exit;
    end;
    current := current^.next;
  end;
end;

procedure ClearComponentList(var list: TComponentList);
var
  current, temp: TComponentList;
begin
  current := list;
  
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
  
  list := nil;
end;

// Реализация функций для работы со списком совместимости
function CreateCompatibilityNode(const data: TCompatibility): TCompatibilityList;
var
  node: TCompatibilityList;
begin
  New(node);
  node^.data := data;
  node^.next := nil;
  Result := node;
end;

procedure AddCompatibility(var list: TCompatibilityList; const data: TCompatibility);
var
  node, current: TCompatibilityList;
begin
  node := CreateCompatibilityNode(data);
  
  if list = nil then
    list := node
  else
  begin
    current := list;
    while current^.next <> nil do
      current := current^.next;
    current^.next := node;
  end;
end;

procedure RemoveCompatibility(var list: TCompatibilityList; left_id, right_id: Integer);
var
  current, prev: TCompatibilityList;
begin
  if list = nil then
    Exit;
  
  if (list^.data.left_id = left_id) and (list^.data.right_id = right_id) then
  begin
    current := list;
    list := list^.next;
    Dispose(current);
  end
  else
  begin
    prev := list;
    current := list^.next;
    
    while (current <> nil) and ((current^.data.left_id <> left_id) or (current^.data.right_id <> right_id)) do
    begin
      prev := current;
      current := current^.next;
    end;
    
    if current <> nil then
    begin
      prev^.next := current^.next;
      Dispose(current);
    end;
  end;
end;

function FindCompatibility(const list: TCompatibilityList; left_id, right_id: Integer): PCompatibility;
var
  current: TCompatibilityList;
begin
  Result := nil;
  current := list;
  
  while current <> nil do
  begin
    if (current^.data.left_id = left_id) and (current^.data.right_id = right_id) then
    begin
      Result := @current^.data;
      Exit;
    end;
    current := current^.next;
  end;
end;

procedure ClearCompatibilityList(var list: TCompatibilityList);
var
  current, temp: TCompatibilityList;
begin
  current := list;
  
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
  
  list := nil;
end;

// Реализация функций для работы со списком компьютерных сборок
function CreateComputerSetNode(const data: TComputerSet): TComputerSetList;
var
  node: TComputerSetList;
begin
  New(node);
  node^.data := data;
  node^.next := nil;
  Result := node;
end;

procedure AddComputerSet(var list: TComputerSetList; const data: TComputerSet);
var
  node, current: TComputerSetList;
begin
  node := CreateComputerSetNode(data);
  
  if list = nil then
    list := node
  else
  begin
    current := list;
    while current^.next <> nil do
      current := current^.next;
    current^.next := node;
  end;
end;

procedure ClearComputerSetList(var list: TComputerSetList);
var
  current, temp: TComputerSetList;
begin
  current := list;
  
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
  
  list := nil;
end;

// Реализация функций для работы со списком заказов
function CreateOrderNode(const data: TOrder): TOrderList;
var
  node: TOrderList;
begin
  New(node);
  node^.data := data;
  node^.next := nil;
  Result := node;
end;

procedure AddOrder(var list: TOrderList; const data: TOrder);
var
  node, current: TOrderList;
begin
  node := CreateOrderNode(data);
  
  if list = nil then
    list := node
  else
  begin
    current := list;
    while current^.next <> nil do
      current := current^.next;
    current^.next := node;
  end;
end;

procedure RemoveOrder(var list: TOrderList; id: Integer);
var
  current, prev: TOrderList;
begin
  if list = nil then
    Exit;
  
  if list^.data.id = id then
  begin
    current := list;
    list := list^.next;
    Dispose(current);
  end
  else
  begin
    prev := list;
    current := list^.next;
    
    while (current <> nil) and (current^.data.id <> id) do
    begin
      prev := current;
      current := current^.next;
    end;
    
    if current <> nil then
    begin
      prev^.next := current^.next;
      Dispose(current);
    end;
  end;
end;

function FindOrder(const list: TOrderList; id: Integer): POrder;
var
  current: TOrderList;
begin
  Result := nil;
  current := list;
  
  while current <> nil do
  begin
    if current^.data.id = id then
    begin
      Result := @current^.data;
      Exit;
    end;
    current := current^.next;
  end;
end;

procedure ClearOrderList(var list: TOrderList);
var
  current, temp: TOrderList;
begin
  current := list;
  
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
  
  list := nil;
end;

end.
