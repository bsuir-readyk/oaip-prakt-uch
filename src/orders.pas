unit Orders;

interface

uses
  SysUtils, Types;

// Инициализация списка заказов
procedure InitOrdersList;

// Освобождение памяти, занятой списком заказов
procedure FreeOrdersList;

// Добавление заказа в список
procedure AddOrder(const Order: TOrder);

// Удаление заказа из списка по номеру
function DeleteOrder(OrderNumber: Integer): Boolean;

// Редактирование заказа
function EditOrder(const Order: TOrder): Boolean;

// Поиск заказа по номеру
function FindOrderByNumber(OrderNumber: Integer): POrderNode;

// Получение нового уникального номера для заказа
function GetNewOrderNumber: Integer;

// Вывод списка заказов
procedure PrintOrders(List: POrderNode);

implementation

// Инициализация списка заказов
procedure InitOrdersList;
begin
  DataLists.Orders := nil;
end;

// Освобождение памяти, занятой списком заказов
procedure FreeOrdersList;
var
  Current, Temp: POrderNode;
begin
  Current := DataLists.Orders;
  while Current <> nil do
  begin
    Temp := Current;
    Current := Current^.Next;
    Dispose(Temp);
  end;
  DataLists.Orders := nil;
end;

// Добавление заказа в список
procedure AddOrder(const Order: TOrder);
var
  NewNode: POrderNode;
begin
  // Создаем новый узел
  New(NewNode);
  NewNode^.Data := Order;
  NewNode^.Next := nil;
  
  // Добавляем узел в начало списка
  if DataLists.Orders = nil then
    DataLists.Orders := NewNode
  else
  begin
    NewNode^.Next := DataLists.Orders;
    DataLists.Orders := NewNode;
  end;
end;

// Удаление заказа из списка по номеру
function DeleteOrder(OrderNumber: Integer): Boolean;
var
  Current, Previous: POrderNode;
  Found: Boolean;
begin
  Current := DataLists.Orders;
  Previous := nil;
  Found := False;
  
  // Ищем заказ с заданным номером
  while (Current <> nil) and (Current^.Data.OrderNumber <> OrderNumber) do
  begin
    Previous := Current;
    Current := Current^.Next;
  end;
  
  // Если заказ найден, удаляем его
  if Current <> nil then
  begin
    if Previous = nil then
      DataLists.Orders := Current^.Next
    else
      Previous^.Next := Current^.Next;
    
    Dispose(Current);
    Found := True;
  end;
  
  DeleteOrder := Found;
end;

// Редактирование заказа
function EditOrder(const Order: TOrder): Boolean;
var
  Current: POrderNode;
  Found: Boolean;
begin
  Current := FindOrderByNumber(Order.OrderNumber);
  Found := False;
  
  if Current <> nil then
  begin
    Current^.Data := Order;
    Found := True;
  end;
  
  EditOrder := Found;
end;

// Поиск заказа по номеру
function FindOrderByNumber(OrderNumber: Integer): POrderNode;
var
  Current: POrderNode;
begin
  Current := DataLists.Orders;
  
  while (Current <> nil) and (Current^.Data.OrderNumber <> OrderNumber) do
    Current := Current^.Next;
  
  FindOrderByNumber := Current;
end;

// Получение нового уникального номера для заказа
function GetNewOrderNumber: Integer;
var
  Current: POrderNode;
  MaxNumber: Integer;
begin
  MaxNumber := 0;
  Current := DataLists.Orders;
  
  while Current <> nil do
  begin
    if Current^.Data.OrderNumber > MaxNumber then
      MaxNumber := Current^.Data.OrderNumber;
    
    Current := Current^.Next;
  end;
  
  GetNewOrderNumber := MaxNumber + 1;
end;

// Вывод списка заказов
procedure PrintOrders(List: POrderNode);
var
  Current: POrderNode;
  i: Integer;
begin
  Current := List;
  
  WriteLn('Список заказов:');
  WriteLn('---------------');
  
  if Current = nil then
    WriteLn('Список пуст')
  else
    while Current <> nil do
    begin
      with Current^.Data do
      begin
        WriteLn('Номер заказа: ', OrderNumber);
        WriteLn('Дата: ', DateToStr(Date));
        WriteLn('Заказчик: ', CustomerName);
        WriteLn('Телефон: ', CustomerPhone);
        WriteLn('Общая стоимость: ', TotalPrice:0:2);
        WriteLn('Комплектующие:');
        
        for i := 0 to ComponentCount - 1 do
          WriteLn('  - Код: ', Components[i]);
        
        WriteLn('---------------');
      end;
      
      Current := Current^.Next;
    end;
end;

end.
