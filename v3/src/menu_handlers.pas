unit menu_handlers;

interface

uses
  SysUtils, Crt, types, list_utils, file_io, sort_utils, filter_utils, computer_builder, order_manager, ui_utils;

// Обработчики пунктов главного меню
procedure HandleReadFromFile(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList;
  var ordersList: TOrderList
);

procedure HandleViewList(
  const componentTypesList: TComponentTypeList;
  const componentsList: TComponentList;
  const compatibilityList: TCompatibilityList;
  const ordersList: TOrderList
);

procedure HandleSortData(
  var componentsList: TComponentList;
  var computerSetsList: TComputerSetList
);

procedure HandleSearchWithFilters(
  const componentsList: TComponentList;
  const componentTypesList: TComponentTypeList;
  const compatibilityList: TCompatibilityList
);

procedure HandleAddData(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList
);

procedure HandleRemoveData(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList
);

procedure HandleEditData(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList
);

procedure HandleSpecialFunctions(
  const componentsList: TComponentList;
  const componentTypesList: TComponentTypeList;
  const compatibilityList: TCompatibilityList;
  var ordersList: TOrderList;
  var computerSetsList: TComputerSetList
);

procedure HandleSaveToFile(
  const componentTypesList: TComponentTypeList;
  const componentsList: TComponentList;
  const compatibilityList: TCompatibilityList;
  const ordersList: TOrderList
);

implementation

// Обработчик пункта меню "Чтение данных из файла"
procedure HandleReadFromFile(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList;
  var ordersList: TOrderList
);
begin
  ClrScr;
  WriteLn('=== Чтение данных из файла ===');
  
  WriteLn('Загрузка типов комплектующих...');
  LoadComponentTypes(componentTypesList, 'data/component_types.dat');
  
  WriteLn('Загрузка комплектующих...');
  LoadComponents(componentsList, 'data/components.dat');
  
  WriteLn('Загрузка совместимости...');
  LoadCompatibility(compatibilityList, 'data/compatibility.dat');
  
  WriteLn('Загрузка заказов...');
  LoadOrders(ordersList, 'data/orders.dat');
  
  WriteLn('Данные успешно загружены.');
end;

// Обработчик пункта меню "Просмотр всего списка"
procedure HandleViewList(
  const componentTypesList: TComponentTypeList;
  const componentsList: TComponentList;
  const compatibilityList: TCompatibilityList;
  const ordersList: TOrderList
);
var
  choice: Integer;
begin
  repeat
    ClrScr;
    WriteLn('=== Просмотр списков ===');
    WriteLn('1. Просмотр списка типов комплектующих');
    WriteLn('2. Просмотр списка комплектующих');
    WriteLn('3. Просмотр списка совместимости');
    WriteLn('4. Просмотр списка заказов');
    WriteLn('5. Возврат в главное меню');
    WriteLn('========================');
    
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 5);
    
    case choice of
      1: begin
        DisplayComponentTypesList(componentTypesList);
        WaitForKey;
      end;
      2: begin
        DisplayComponentsList(componentsList);
        WaitForKey;
      end;
      3: begin
        DisplayCompatibilityList(compatibilityList, componentsList);
        WaitForKey;
      end;
      4: begin
        DisplayOrdersList(ordersList);
        WaitForKey;
      end;
      5: begin
        // Возврат в главное меню
      end;
    end;
  until choice = 5;
end;

// Обработчик пункта меню "Сортировка данных"
procedure HandleSortData(
  var componentsList: TComponentList;
  var computerSetsList: TComputerSetList
);
var
  choice: Integer;
  sortChoice: Integer;
begin
  repeat
    ClrScr;
    WriteLn('=== Сортировка данных ===');
    WriteLn('1. Сортировка комплектующих');
    WriteLn('2. Сортировка компьютерных сборок');
    WriteLn('3. Возврат в главное меню');
    WriteLn('========================');
    
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 3);
    
    case choice of
      1: begin
        ClrScr;
        WriteLn('=== Сортировка комплектующих ===');
        WriteLn('1. Сортировка по цене (по возрастанию)');
        WriteLn('2. Сортировка по цене (по убыванию)');
        WriteLn('3. Сортировка по производителю');
        WriteLn('4. Сортировка по модели');
        WriteLn('5. Сортировка по типу');
        WriteLn('6. Возврат в предыдущее меню');
        WriteLn('==============================');
        
        sortChoice := SafeReadInteger('Выберите пункт меню: ', 1, 6);
        
        case sortChoice of
          1: begin
            SortComponentsByPrice(componentsList, True);
            WriteLn('Комплектующие отсортированы по цене (по возрастанию).');
          end;
          2: begin
            SortComponentsByPrice(componentsList, False);
            WriteLn('Комплектующие отсортированы по цене (по убыванию).');
          end;
          3: begin
            SortComponentsByManufacturer(componentsList);
            WriteLn('Комплектующие отсортированы по производителю.');
          end;
          4: begin
            SortComponentsByModel(componentsList);
            WriteLn('Комплектующие отсортированы по модели.');
          end;
          5: begin
            SortComponentsByType(componentsList);
            WriteLn('Комплектующие отсортированы по типу.');
          end;
          6: begin
            // Возврат в предыдущее меню
          end;
        end;
        
        if sortChoice <> 6 then
        begin
          DisplayComponentsList(componentsList);
          WaitForKey;
        end;
      end;
      2: begin
        ClrScr;
        WriteLn('=== Сортировка компьютерных сборок ===');
        WriteLn('1. Сортировка по цене (по возрастанию)');
        WriteLn('2. Сортировка по цене (по убыванию)');
        WriteLn('3. Возврат в предыдущее меню');
        WriteLn('==================================');
        
        sortChoice := SafeReadInteger('Выберите пункт меню: ', 1, 3);
        
        case sortChoice of
          1: begin
            SortComputerSetsByPrice(computerSetsList, True);
            WriteLn('Компьютерные сборки отсортированы по цене (по возрастанию).');
          end;
          2: begin
            SortComputerSetsByPrice(computerSetsList, False);
            WriteLn('Компьютерные сборки отсортированы по цене (по убыванию).');
          end;
          3: begin
            // Возврат в предыдущее меню
          end;
        end;
        
        if sortChoice <> 3 then
        begin
          DisplayComputerSetsList(computerSetsList);
          WaitForKey;
        end;
      end;
      3: begin
        // Возврат в главное меню
      end;
    end;
  until choice = 3;
end;

// Обработчик пункта меню "Поиск данных с использованием фильтров"
procedure HandleSearchWithFilters(
  const componentsList: TComponentList;
  const componentTypesList: TComponentTypeList;
  const compatibilityList: TCompatibilityList
);
var
  choice: Integer;
  typeId: Integer;
  minPrice, maxPrice: Real;
  manufacturer: string;
  available: Boolean;
  filteredList: TComponentList;
  componentId: Integer;
begin
  repeat
    ClrScr;
    WriteLn('=== Поиск данных с использованием фильтров ===');
    WriteLn('1. Фильтрация по типу');
    WriteLn('2. Фильтрация по диапазону цен');
    WriteLn('3. Фильтрация по производителю');
    WriteLn('4. Фильтрация по наличию');
    WriteLn('5. Поиск совместимых комплектующих');
    WriteLn('6. Возврат в главное меню');
    WriteLn('============================================');
    
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 6);
    
    case choice of
      1: begin
        ClrScr;
        WriteLn('=== Фильтрация по типу ===');
        DisplayComponentTypesList(componentTypesList);
        
        typeId := SafeReadInteger('Введите ID типа комплектующей: ', 1, 1000);
        
        filteredList := FilterComponentsByType(componentsList, typeId);
        
        if filteredList <> nil then
        begin
          DisplayComponentsList(filteredList);
          ClearComponentList(filteredList);
        end
        else
          WriteLn('Комплектующие с указанным типом не найдены.');
        
        WaitForKey;
      end;
      2: begin
        ClrScr;
        WriteLn('=== Фильтрация по диапазону цен ===');
        
        minPrice := SafeReadReal('Введите минимальную цену: ', 0, 1000000);
        maxPrice := SafeReadReal('Введите максимальную цену: ', minPrice, 1000000);
        
        filteredList := FilterComponentsByPriceRange(componentsList, minPrice, maxPrice);
        
        if filteredList <> nil then
        begin
          DisplayComponentsList(filteredList);
          ClearComponentList(filteredList);
        end
        else
          WriteLn('Комплектующие в указанном диапазоне цен не найдены.');
        
        WaitForKey;
      end;
      3: begin
        ClrScr;
        WriteLn('=== Фильтрация по производителю ===');
        
        manufacturer := SafeReadString('Введите название производителя: ', 50);
        
        filteredList := FilterComponentsByManufacturer(componentsList, manufacturer);
        
        if filteredList <> nil then
        begin
          DisplayComponentsList(filteredList);
          ClearComponentList(filteredList);
        end
        else
          WriteLn('Комплектующие указанного производителя не найдены.');
        
        WaitForKey;
      end;
      4: begin
        ClrScr;
        WriteLn('=== Фильтрация по наличию ===');
        
        available := SafeReadBoolean('Показать только имеющиеся в наличии?');
        
        filteredList := FilterComponentsByAvailability(componentsList, available);
        
        if filteredList <> nil then
        begin
          DisplayComponentsList(filteredList);
          ClearComponentList(filteredList);
        end
        else
          WriteLn('Комплектующие с указанным статусом наличия не найдены.');
        
        WaitForKey;
      end;
      5: begin
        ClrScr;
        WriteLn('=== Поиск совместимых комплектующих ===');
        
        DisplayComponentsList(componentsList);
        componentId := SafeReadInteger('Введите ID комплектующей: ', 1, 1000);
        
        DisplayComponentTypesList(componentTypesList);
        typeId := SafeReadInteger('Введите ID типа комплектующей для поиска совместимых: ', 1, 1000);
        
        filteredList := FindCompatibleComponents(componentsList, compatibilityList, componentId, typeId);
        
        if filteredList <> nil then
        begin
          DisplayComponentsList(filteredList);
          ClearComponentList(filteredList);
        end
        else
          WriteLn('Совместимые комплектующие не найдены.');
        
        WaitForKey;
      end;
      6: begin
        // Возврат в главное меню
      end;
    end;
  until choice = 6;
end;

// Функция для ввода данных о типе комплектующей
function InputComponentType: TComponentType;
begin
  ClrScr;
  WriteLn('=== Ввод данных о типе комплектующей ===');
  
  Result.id := SafeReadInteger('Введите ID типа: ', 1, 1000);
  Result.name := SafeReadString('Введите название типа: ', 50);
end;

// Функция для ввода данных о комплектующей
function InputComponent: TComponent;
begin
  ClrScr;
  WriteLn('=== Ввод данных о комплектующей ===');
  
  Result.id := SafeReadInteger('Введите ID комплектующей: ', 1, 1000);
  Result.cTypeId := SafeReadInteger('Введите ID типа комплектующей: ', 1, 1000);
  Result.manufacturer_name := SafeReadString('Введите название производителя: ', 50);
  Result.model_name := SafeReadString('Введите название модели: ', 50);
  Result.description := SafeReadString('Введите описание (параметры): ', 100);
  Result.cost := SafeReadReal('Введите цену: ', 0, 1000000);
  Result.availability := SafeReadInteger('Введите количество в наличии: ', 0, 1000);
end;

// Функция для ввода данных о совместимости
function InputCompatibility: TCompatibility;
begin
  ClrScr;
  WriteLn('=== Ввод данных о совместимости ===');
  
  Result.left_id := SafeReadInteger('Введите ID первой комплектующей: ', 1, 1000);
  Result.right_id := SafeReadInteger('Введите ID второй комплектующей: ', 1, 1000);
end;

// Обработчик пункта меню "Добавление данных в список"
procedure HandleAddData(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList
);
var
  choice: Integer;
  componentType: TComponentType;
  component: TComponent;
  compatibility: TCompatibility;
begin
  repeat
    ClrScr;
    WriteLn('=== Добавление данных в список ===');
    WriteLn('1. Добавление типа комплектующей');
    WriteLn('2. Добавление комплектующей');
    WriteLn('3. Добавление совместимости');
    WriteLn('4. Возврат в главное меню');
    WriteLn('===============================');
    
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 4);
    
    case choice of
      1: begin
        componentType := InputComponentType;
        AddComponentType(componentTypesList, componentType);
        WriteLn('Тип комплектующей успешно добавлен.');
        WaitForKey;
      end;
      2: begin
        component := InputComponent;
        AddComponent(componentsList, component);
        WriteLn('Комплектующая успешно добавлена.');
        WaitForKey;
      end;
      3: begin
        compatibility := InputCompatibility;
        AddCompatibility(compatibilityList, compatibility);
        WriteLn('Совместимость успешно добавлена.');
        WaitForKey;
      end;
      4: begin
        // Возврат в главное меню
      end;
    end;
  until choice = 4;
end;

// Обработчик пункта меню "Удаление данных из списка"
procedure HandleRemoveData(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList
);
var
  choice: Integer;
  id, left_id, right_id: Integer;
begin
  repeat
    ClrScr;
    WriteLn('=== Удаление данных из списка ===');
    WriteLn('1. Удаление типа комплектующей');
    WriteLn('2. Удаление комплектующей');
    WriteLn('3. Удаление совместимости');
    WriteLn('4. Возврат в главное меню');
    WriteLn('==============================');
    
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 4);
    
    case choice of
      1: begin
        ClrScr;
        WriteLn('=== Удаление типа комплектующей ===');
        DisplayComponentTypesList(componentTypesList);
        
        id := SafeReadInteger('Введите ID типа комплектующей для удаления: ', 1, 1000);
        RemoveComponentType(componentTypesList, id);
        
        WriteLn('Тип комплектующей успешно удален.');
        WaitForKey;
      end;
      2: begin
        ClrScr;
        WriteLn('=== Удаление комплектующей ===');
        DisplayComponentsList(componentsList);
        
        id := SafeReadInteger('Введите ID комплектующей для удаления: ', 1, 1000);
        RemoveComponent(componentsList, id);
        
        WriteLn('Комплектующая успешно удалена.');
        WaitForKey;
      end;
      3: begin
        ClrScr;
        WriteLn('=== Удаление совместимости ===');
        DisplayCompatibilityList(compatibilityList, componentsList);
        
        left_id := SafeReadInteger('Введите ID первой комплектующей: ', 1, 1000);
        right_id := SafeReadInteger('Введите ID второй комплектующей: ', 1, 1000);
        RemoveCompatibility(compatibilityList, left_id, right_id);
        
        WriteLn('Совместимость успешно удалена.');
        WaitForKey;
      end;
      4: begin
        // Возврат в главное меню
      end;
    end;
  until choice = 4;
end;

// Обработчик пункта меню "Редактирование данных"
procedure HandleEditData(
  var componentTypesList: TComponentTypeList;
  var componentsList: TComponentList;
  var compatibilityList: TCompatibilityList
);
var
  choice: Integer;
  id, left_id, right_id: Integer;
  componentType: PComponentType;
  component: PComponent;
  compatibility: PCompatibility;
begin
  repeat
    ClrScr;
    WriteLn('=== Редактирование данных ===');
    WriteLn('1. Редактирование типа комплектующей');
    WriteLn('2. Редактирование комплектующей');
    WriteLn('3. Редактирование совместимости');
    WriteLn('4. Возврат в главное меню');
    WriteLn('============================');
    
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 4);
    
    case choice of
      1: begin
        ClrScr;
        WriteLn('=== Редактирование типа комплектующей ===');
        DisplayComponentTypesList(componentTypesList);
        
        id := SafeReadInteger('Введите ID типа комплектующей для редактирования: ', 1, 1000);
        componentType := FindComponentType(componentTypesList, id);
        
        if componentType <> nil then
        begin
          componentType^.name := SafeReadString('Введите новое название типа: ', 50);
          WriteLn('Тип комплектующей успешно отредактирован.');
        end
        else
          WriteLn('Тип комплектующей с указанным ID не найден.');
        
        WaitForKey;
      end;
      2: begin
        ClrScr;
        WriteLn('=== Редактирование комплектующей ===');
        DisplayComponentsList(componentsList);
        
        id := SafeReadInteger('Введите ID комплектующей для редактирования: ', 1, 1000);
        component := FindComponent(componentsList, id);
        
        if component <> nil then
        begin
          component^.cTypeId := SafeReadInteger('Введите новый ID типа комплектующей: ', 1, 1000);
          component^.manufacturer_name := SafeReadString('Введите новое название производителя: ', 50);
          component^.model_name := SafeReadString('Введите новое название модели: ', 50);
          component^.description := SafeReadString('Введите новое описание (параметры): ', 100);
          component^.cost := SafeReadReal('Введите новую цену: ', 0, 1000000);
          component^.availability := SafeReadInteger('Введите новое количество в наличии: ', 0, 1000);
          
          WriteLn('Комплектующая успешно отредактирована.');
        end
        else
          WriteLn('Комплектующая с указанным ID не найдена.');
        
        WaitForKey;
      end;
      3: begin
        ClrScr;
        WriteLn('=== Редактирование совместимости ===');
        DisplayCompatibilityList(compatibilityList, componentsList);
        
        left_id := SafeReadInteger('Введите ID первой комплектующей: ', 1, 1000);
        right_id := SafeReadInteger('Введите ID второй комплектующей: ', 1, 1000);
        compatibility := FindCompatibility(compatibilityList, left_id, right_id);
        
        if compatibility <> nil then
        begin
          compatibility^.left_id := SafeReadInteger('Введите новый ID первой комплектующей: ', 1, 1000);
          compatibility^.right_id := SafeReadInteger('Введите новый ID второй комплектующей: ', 1, 1000);
          
          WriteLn('Совместимость успешно отредактирована.');
        end
        else
          WriteLn('Совместимость с указанными ID не найдена.');
        
        WaitForKey;
      end;
      4: begin
        // Возврат в главное меню
      end;
    end;
  until choice = 4;
end;

// Обработчик пункта меню "Специальные функции"
procedure HandleSpecialFunctions(
  const componentsList: TComponentList;
  const componentTypesList: TComponentTypeList;
  const compatibilityList: TCompatibilityList;
  var ordersList: TOrderList;
  var computerSetsList: TComputerSetList
);
var
  choice, setIndex: Integer;
  minPrice, maxPrice: Real;
  customerName, customerPhone, customerEmail: string;
  order: TOrder;
  current: TComputerSetList;
  componentId, typeId: Integer;
  filteredList: TComponentList;
  f: TextFile;
begin
  repeat
    ShowSpecialFunctionsMenu;
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 4);
    
    case choice of
      1: begin // Подбор вариантов комплектации компьютера в заданном ценовом диапазоне
        ClrScr;
        WriteLn('=== Подбор вариантов комплектации компьютера ===');
        
        minPrice := SafeReadReal('Введите минимальную цену: ', 0, 1000000);
        maxPrice := SafeReadReal('Введите максимальную цену: ', minPrice, 1000000);
        
        // Освобождение памяти, если список уже существует
        ClearComputerSetList(computerSetsList);
        
        // Создание всех возможных вариантов компьютерных сборок
        computerSetsList := BuildAllPossibleComputerSets(
          componentsList, componentTypesList, compatibilityList, minPrice, maxPrice
        );
        
        // Отображение результатов
        DisplayComputerSetsList(computerSetsList);
        
        // Сохранение результатов в текстовый файл
        try
          AssignFile(f, 'output/computer_sets.txt');
          Rewrite(f);
          
          current := computerSetsList;
          setIndex := 1;
          
          while current <> nil do
          begin
            with current^.data do
            begin
              WriteLn(f, '=== Вариант комплектации #', setIndex, ' ===');
              
              Write(f, 'Процессор: ');
              if cpu <> nil then
                WriteLn(f, cpu^.manufacturer_name, ' ', cpu^.model_name, ' - ', cpu^.cost:0:2)
              else
                WriteLn(f, 'Не выбран');
              
              Write(f, 'Материнская плата: ');
              if motherboard <> nil then
                WriteLn(f, motherboard^.manufacturer_name, ' ', motherboard^.model_name, ' - ', motherboard^.cost:0:2)
              else
                WriteLn(f, 'Не выбрана');
              
              Write(f, 'Оперативная память: ');
              if ram <> nil then
                WriteLn(f, ram^.manufacturer_name, ' ', ram^.model_name, ' - ', ram^.cost:0:2)
              else
                WriteLn(f, 'Не выбрана');
              
              Write(f, 'Блок питания: ');
              if psu <> nil then
                WriteLn(f, psu^.manufacturer_name, ' ', psu^.model_name, ' - ', psu^.cost:0:2)
              else
                WriteLn(f, 'Не выбран');
              
              Write(f, 'Накопитель: ');
              if storage <> nil then
                WriteLn(f, storage^.manufacturer_name, ' ', storage^.model_name, ' - ', storage^.cost:0:2)
              else
                WriteLn(f, 'Не выбран');
              
              WriteLn(f, 'Общая стоимость: ', totalCost:0:2);
              WriteLn(f);
            end;
            
            current := current^.next;
            Inc(setIndex);
          end;
          
          CloseFile(f);
          WriteLn('Результаты сохранены в файл output/computer_sets.txt');
        except
          on E: Exception do
            WriteLn('Ошибка при сохранении результатов в файл: ', E.Message);
        end;
        
        WaitForKey;
      end;
      2: begin // Оформление заказа понравившегося варианта
        ClrScr;
        WriteLn('=== Оформление заказа ===');
        
        if computerSetsList = nil then
        begin
          WriteLn('Сначала необходимо подобрать варианты комплектации компьютера.');
          WaitForKey;
          Continue;
        end;
        
        DisplayComputerSetsList(computerSetsList);
        
        setIndex := SafeReadInteger('Введите номер варианта для заказа: ', 1, 1000);
        
        // Поиск выбранного варианта
        current := computerSetsList;
        for choice := 1 to setIndex - 1 do
        begin
          if current = nil then
            Break;
          current := current^.next;
        end;
        
        if current = nil then
        begin
          WriteLn('Вариант с указанным номером не найден.');
          WaitForKey;
          Continue;
        end;
        
        // Ввод данных заказчика
        customerName := SafeReadString('Введите имя заказчика: ', 50);
        customerPhone := SafeReadString('Введите телефон заказчика: ', 20);
        customerEmail := SafeReadString('Введите email заказчика: ', 50);
        
        // Создание заказа
        order := CreateOrder(current^.data, customerName, customerPhone, customerEmail);
        
        // Добавление заказа в список
        PlaceOrder(ordersList, order);
        
        // Сохранение заказа в текстовый файл
        SaveOrderToTextFile(order, 'output/order_' + IntToStr(order.id) + '.txt');
        
        WriteLn('Заказ успешно оформлен и сохранен в файл output/order_', order.id, '.txt');
        WaitForKey;
      end;
      3: begin // Вывод комплектующих заданного типа, совместимых с конкретным комплектующим
        ClrScr;
        WriteLn('=== Поиск совместимых комплектующих ===');
        
        DisplayComponentsList(componentsList);
        componentId := SafeReadInteger('Введите ID комплектующей: ', 1, 1000);
        
        DisplayComponentTypesList(componentTypesList);
        typeId := SafeReadInteger('Введите ID типа комплектующей для поиска совместимых: ', 1, 1000);
        
        filteredList := FindCompatibleComponents(componentsList, compatibilityList, componentId, typeId);
        
        if filteredList <> nil then
        begin
          DisplayComponentsList(filteredList);
          
          // Сохранение результатов в текстовый файл
          try
            AssignFile(f, 'output/compatible_components.txt');
            Rewrite(f);
            
            WriteLn(f, '=== Совместимые комплектующие ===');
            WriteLn(f, 'Комплектующая: ', componentId);
            WriteLn(f, 'Тип: ', typeId);
            WriteLn(f);
            WriteLn(f, 'ID | Тип | Производитель | Модель | Описание | Цена | Наличие');
            WriteLn(f, '-------------------------------------------------------------------');
            
            current := filteredList;
            
            while current <> nil do
            begin
              with current^.data do
                WriteLn(f, id, ' | ', cTypeId, ' | ', manufacturer_name, ' | ', model_name, ' | ', 
                        description, ' | ', cost:0:2, ' | ', availability);
              
              current := current^.next;
            end;
            
            CloseFile(f);
            WriteLn('Результаты сохранены в файл output/compatible_components.txt');
          except
            on E: Exception do
              WriteLn('Ошибка при сохранении результатов в файл: ', E.Message);
          end;
          
          ClearComponentList(filteredList);
        end
        else
          WriteLn('Совместимые комплектующие не найдены.');
        
        WaitForKey;
      end;
      4: begin
        // Возврат в главное меню
      end;
    end;
  until choice = 4;
end;

// Обработчик пункта меню "Выход с сохранением изменений"
procedure HandleSaveToFile(
  const componentTypesList: TComponentTypeList;
  const componentsList: TComponentList;
  const compatibilityList: TCompatibilityList;
  const ordersList: TOrderList
);
begin
  ClrScr;
  WriteLn('=== Сохранение данных в файл ===');
  
  WriteLn('Сохранение типов комплектующих...');
  SaveComponentTypes(componentTypesList, 'data/component_types.dat');
  
  WriteLn('Сохранение комплектующих...');
  SaveComponents(componentsList, 'data/components.dat');
  
  WriteLn('Сохранение совместимости...');
  SaveCompatibility(compatibilityList, 'data/compatibility.dat');
  
  WriteLn('Сохранение заказов...');
  SaveOrders(ordersList, 'data/orders.dat');
  
  WriteLn('Данные успешно сохранены.');
end;

end.
