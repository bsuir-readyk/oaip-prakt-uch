unit Orders;

interface

uses
  SysUtils, Types, UI;

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
  ColumnWidths: array[0..4] of Integer;
  Alignments: array[0..4] of Char;
  Values: array[0..4] of string;
  OrderCount: Integer;
  ComponentsStr: string;
begin
  Current := List;
  
  WriteLn('Список заказов:');
  
  if Current = nil then
    WriteLn('Список пуст')
  else
  begin
    // Определяем ширину столбцов
    ColumnWidths[0] := 5;  // Номер заказа
    ColumnWidths[1] := 12; // Дата
    ColumnWidths[2] := 20; // Заказчик
    ColumnWidths[3] := 15; // Телефон
    ColumnWidths[4] := 15; // Общая стоимость
    
    // Определяем выравнивание столбцов
    Alignments[0] := 'R'; // Номер заказа - по правому краю
    Alignments[1] := 'C'; // Дата - по центру
    Alignments[2] := 'L'; // Заказчик - по левому краю
    Alignments[3] := 'L'; // Телефон - по левому краю
    Alignments[4] := 'R'; // Общая стоимость - по правому краю
    
    // Выводим заголовок таблицы
    PrintTableHorizontalLine(ColumnWidths, 'T');
    
    Values[0] := 'Number';
    Values[1] := 'Date';
    Values[2] := 'Customer';
    Values[3] := 'Phone';
    Values[4] := 'Price';
    PrintTableRow(Values, ColumnWidths, Alignments);
    
    PrintTableHorizontalLine(ColumnWidths, 'M');
    
    // Выводим данные
    OrderCount := 0;
    while Current <> nil do
    begin
      with Current^.Data do
      begin
        Values[0] := IntToStr(OrderNumber);
        Values[1] := DateToStr(Date);
        Values[2] := CustomerName;
        Values[3] := CustomerPhone;
        Values[4] := Format('%.2f', [TotalPrice]);
        
        PrintTableRow(Values, ColumnWidths, Alignments);
        
        // Выводим список комплектующих
        ComponentsStr := 'Components: ';
        for i := 0 to ComponentCount - 1 do
        begin
          if i > 0 then
            ComponentsStr := ComponentsStr + ', ';
          ComponentsStr := ComponentsStr + IntToStr(Components[i]);
        end;
        
        WriteLn('│ ', PadString(ComponentsStr, ColumnWidths[0] + ColumnWidths[1] + ColumnWidths[2] + ColumnWidths[3] + ColumnWidths[4] + 8, 'L'), ' │');
        PrintTableHorizontalLine(ColumnWidths, 'M');
      end;
      
      Current := Current^.Next;
      Inc(OrderCount);
    end;
    
    PrintTableHorizontalLine(ColumnWidths, 'B');
    WriteLn('Всего заказов: ', OrderCount);
  end;
end;

end.
