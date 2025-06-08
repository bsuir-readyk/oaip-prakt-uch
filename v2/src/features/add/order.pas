unit f_add_order;

interface

uses
  SysUtils,
  UiUtils,
  s_types, s_logging, s_utils,
  e_order_t, e_order_crud, e_global, e_order_ui, // e_order_ui might be needed for safeReadOrder or other UI utils if used
  e_component_ui, e_component_crud; // Added for safeReadComponent

procedure uiCreateOrder();

implementation

procedure uiCreateOrder();
var
  tempOrderData: TOrder; // Store data here before passing to Order_create
  items: TArrayInt;
  componentId, quantity, i: Integer;
  log: TLogF;
  customerName, customerPhone, customerEmail, status: string; // Use standard strings for input
begin
  // log := GetLogger(GData.LogLevel);
  // writeln;
  // writeln('--- Создание нового заказа ---');

  // SetLength(items, 0);
  // writeln('Добавление позиций заказа (компонент ID и количество).');
  // writeln('Введите -1 для ID компонента для завершения.');
  // writeln;
  // i := 0;
  
  // while true do
  // begin
  //   writeln(Format('Позиция %d:', [i + 1]));
  //   componentId := safeReadComponent();

  //   if componentId = -1 then
  //     break;

  //   writeln(Format('Количество для компонента ID %d:', [componentId]));
  //   quantity := SafeReadInteger('Введите количество: ', 1, MAXINT);
    
  //   // Ensure items array is expanded correctly. Each component ID is followed by its quantity.
  //   SetLength(items, Length(items) + 2); 
  //   items[High(items) - 1] := componentId;
  //   items[High(items)]     := quantity;
  //   i := i + 1;
    
  //   writeln('Компонент добавлен в заказ');
  //   writeln;
  // end;

  // writeln;
  // writeln('--- Информация о заказе ---');
  
  // writeln('Дата заказа (ггггммдд):');
  // tempOrderData.orderDate := SafeReadInteger('Введите дату: ', 0, MAXINT); // Consider adding date validation
  
  // writeln('Имя покупателя:');
  // customerName := SafeReadString('Введите имя: ');
  
  // writeln('Телефон покупателя:');
  // customerPhone := SafeReadString('Введите телефон: ');
  
  // writeln('Email покупателя:');
  // customerEmail := SafeReadString('Введите email: ');
  
  // writeln('Статус заказа (new, processing, completed, cancelled):');
  // status := SafeReadString('Введите статус: '); // Consider enum or validation

  // // Assign to TFixedString fields in tempOrderData
  // tempOrderData.customerName := customerName;
  // tempOrderData.customerPhone := customerPhone;
  // tempOrderData.customerEmail := customerEmail;
  // tempOrderData.status := status;
  
  // // Note: Order_create expects TArrayInt, not TOrder directly.
  // // And other fields are passed as separate parameters.
  // // The TOrder record in crud.pas Order_create is internal to that function.
  // // Let's ensure the parameters match e_order_crud.Order_create

  // // items is already TArrayInt
  // // tempOrderData.orderDate is Integer
  // // customerName, customerPhone, customerEmail, status are strings

  // if Order_create(items, tempOrderData.orderDate, customerName, customerPhone, customerEmail, status) <> nil then
  // begin
  //   writeln;
  //   writeln('Заказ успешно создан');
  //   // Optionally, display the created order using a stringify function if available and desired.
  //   // writeln(stringifyOrder(GData.Orders^.next)); // This would show the first order, need to find the *newly created* one
  // end
  // else
  // begin
  //   writeln;
  //   writeln('Ошибка при создании заказа');
  // end;
  
  // WaitForKey; // Keep console open
end;

end. 
