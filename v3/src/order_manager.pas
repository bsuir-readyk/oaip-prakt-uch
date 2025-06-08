unit order_manager;

interface

uses
  SysUtils, types, list_utils;

// Создание нового заказа
function CreateOrder(
  const computerSet: TComputerSet;
  const customerName, customerPhone, customerEmail: string
): TOrder;

// Добавление заказа в список
procedure PlaceOrder(var ordersList: TOrderList; const order: TOrder);

// Изменение статуса заказа
procedure UpdateOrderStatus(var order: POrder; const newStatus: string);

// Получение следующего ID для заказа
function GetNextOrderId(const ordersList: TOrderList): Integer;

// Сохранение заказа в текстовый файл
procedure SaveOrderToTextFile(const order: TOrder; const filename: string);

implementation

// Создание нового заказа
function CreateOrder(
  const computerSet: TComputerSet;
  const customerName, customerPhone, customerEmail: string
): TOrder;
begin
  Result.computerSet := computerSet;
  Result.orderDate := Now;
  Result.customerName := customerName;
  Result.customerPhone := customerPhone;
  Result.customerEmail := customerEmail;
  Result.status := 'new';
  // ID будет установлен при добавлении в список
end;

// Добавление заказа в список
procedure PlaceOrder(var ordersList: TOrderList; const order: TOrder);
var
  newOrder: TOrder;
begin
  newOrder := order;
  newOrder.id := GetNextOrderId(ordersList);
  AddOrder(ordersList, newOrder);
end;

// Изменение статуса заказа
procedure UpdateOrderStatus(var order: POrder; const newStatus: string);
begin
  if order <> nil then
    order^.status := newStatus;
end;

// Получение следующего ID для заказа
function GetNextOrderId(const ordersList: TOrderList): Integer;
var
  current: TOrderList;
  maxId: Integer;
begin
  maxId := 0;
  current := ordersList;
  
  while current <> nil do
  begin
    if current^.data.id > maxId then
      maxId := current^.data.id;
    
    current := current^.next;
  end;
  
  Result := maxId + 1;
end;

// Функция для форматирования строки с информацией о комплектующей
function FormatComponentInfo(const component: PComponent): string;
begin
  if component = nil then
    Result := 'Не выбран'
  else
    Result := Format('%s %s (%s) - %.2f', [
      component^.manufacturer_name,
      component^.model_name,
      component^.description,
      component^.cost
    ]);
end;

// Сохранение заказа в текстовый файл
procedure SaveOrderToTextFile(const order: TOrder; const filename: string);
var
  f: TextFile;
begin
  try
    AssignFile(f, filename);
    Rewrite(f);
    
    WriteLn(f, '=== Заказ №', order.id, ' ===');
    WriteLn(f, 'Дата: ', DateTimeToStr(order.orderDate));
    WriteLn(f, 'Статус: ', order.status);
    WriteLn(f);
    
    WriteLn(f, '--- Информация о заказчике ---');
    WriteLn(f, 'Имя: ', order.customerName);
    WriteLn(f, 'Телефон: ', order.customerPhone);
    WriteLn(f, 'Email: ', order.customerEmail);
    WriteLn(f);
    
    WriteLn(f, '--- Комплектация компьютера ---');
    WriteLn(f, 'Процессор: ', FormatComponentInfo(order.computerSet.cpu));
    WriteLn(f, 'Материнская плата: ', FormatComponentInfo(order.computerSet.motherboard));
    WriteLn(f, 'Оперативная память: ', FormatComponentInfo(order.computerSet.ram));
    WriteLn(f, 'Блок питания: ', FormatComponentInfo(order.computerSet.psu));
    WriteLn(f, 'Накопитель: ', FormatComponentInfo(order.computerSet.storage));
    WriteLn(f);
    
    WriteLn(f, '--- Стоимость ---');
    WriteLn(f, 'Общая стоимость: ', order.computerSet.totalCost:0:2);
    WriteLn(f);
    
    WriteLn(f, '=== Конец заказа ===');
    
    CloseFile(f);
  except
    on E: Exception do
      WriteLn('Ошибка при сохранении заказа в текстовый файл: ', E.Message);
  end;
end;

end.
