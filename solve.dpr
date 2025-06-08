program Solve;

uses
  SysUtils, Crt,
  Types in 'src/types.pas',
  Components in 'src/components.pas',
  ComponentTypes in 'src/componentTypes.pas',
  Compatibility in 'src/compatibility.pas',
  FileIO in 'src/fileIO.pas',
  UI in 'src/ui.pas',
  SpecialFunctions in 'src/specialFunctions.pas',
  Orders in 'src/orders.pas';

var
  choice, submenuChoice: Integer;
  exitProgram: Boolean;
  exitWithSave: Boolean;
  
  // Переменные для работы с комплектующими
  component: TComponent;
  componentType: TComponentType;
  compatibilityRecord: TCompatibility;
  
  // Переменные для работы с заказами
  order: TOrder;
  
  // Переменные для работы с конфигурациями
  minPrice, maxPrice: Real;
  selectedTypes: array of Integer;
  typeCount, i, j: Integer;
  Current: PConfigurationNode;
  
  // Переменные для поиска
  searchCode, searchTypeCode: Integer;
  searchManufacturer: string;
  searchMinPrice, searchMaxPrice: Real;
  searchResult: PComponentNode;
  
  // Переменные для заказа
  customerName, customerPhone: string;
  
  // Переменная для работы с типами комплектующих
  componentTypeNode: PComponentTypeNode;

  temp: 

begin
  // Инициализация всех списков
  InitComponentsList;
  InitComponentTypesList;
  InitCompatibilityList;
  InitOrdersList;
  
  // Проверка существования файлов данных
  if not DataFilesExist then
    CreateEmptyDataFiles
  else
    ReadDataFromFiles;
  
  exitWithSave := False;
  choice := -1;
  exitProgram := false;
  
  while not exitProgram do
  begin
    ShowMainMenu;
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 10);
    
    case choice of
      1: begin // Чтение данных из файла
        ReadDataFromFiles;
        WaitForKey;
      end;
      
      2: begin // Просмотр списка
        ShowViewSubmenu;
        submenuChoice := SafeReadInteger('Выберите список для просмотра: ', 0, 4);
        
        if submenuChoice <> 0 then
        begin
          case submenuChoice of
            1: PrintComponents(DataLists.Components);
            2: PrintComponentTypes(DataLists.ComponentTypes);
            3: PrintCompatibility(DataLists.Compatibilities);
            4: PrintOrders(DataLists.Orders);
          end;
          
          WaitForKey;
        end;
      end;
      
      3: begin // Сортировка данных
        ShowSortSubmenu;
        submenuChoice := SafeReadInteger('Выберите тип сортировки: ', 0, 3);
        
        if submenuChoice <> 0 then
        begin
          case submenuChoice of
            1: SortComponentsByPrice(DataLists.Components);
            2: SortComponentsByStock(DataLists.Components);
            3: SortComponentsByManufacturer(DataLists.Components);
          end;
          
          PrintComponents(DataLists.Components);
          WaitForKey;
        end;
      end;
      
      4: begin // Поиск данных с использованием фильтров
        ClrScr;
        PrintHeader('ПОИСК ДАННЫХ');
        WriteLn('1. Поиск комплектующей по коду');
        WriteLn('2. Поиск комплектующих по типу');
        WriteLn('3. Поиск комплектующих по производителю');
        WriteLn('4. Поиск комплектующих в ценовом диапазоне');
        WriteLn('0. Назад');
        PrintSeparator;
        
        submenuChoice := SafeReadInteger('Выберите тип поиска: ', 0, 4);
        
        if submenuChoice <> 0 then
        begin
          case submenuChoice of
            1: begin // Поиск комплектующей по коду
              searchCode := SafeReadInteger('Введите код комплектующей: ', 1, 9999);
              searchResult := FindComponentByCode(searchCode);
              
              if searchResult <> nil then
              begin
                ClrScr;
                PrintHeader('РЕЗУЛЬТАТЫ ПОИСКА');
                PrintComponents(searchResult);
              end
              else
                WriteLn('Комплектующая с кодом ', searchCode, ' не найдена.');
            end;
            
            2: begin // Поиск комплектующих по типу
              searchTypeCode := SafeReadInteger('Введите код типа комплектующей: ', 1, 9999);
              searchResult := FindComponentsByType(searchTypeCode);
              
              if searchResult <> nil then
              begin
                ClrScr;
                PrintHeader('РЕЗУЛЬТАТЫ ПОИСКА');
                PrintComponents(searchResult);
              end
              else
                WriteLn('Комплектующие с типом ', searchTypeCode, ' не найдены.');
            end;
            
            3: begin // Поиск комплектующих по производителю
              searchManufacturer := SafeReadString('Введите производителя: ');
              searchResult := FindComponentsByManufacturer(searchManufacturer);
              
              if searchResult <> nil then
              begin
                ClrScr;
                PrintHeader('РЕЗУЛЬТАТЫ ПОИСКА');
                PrintComponents(searchResult);
              end
              else
                WriteLn('Комплектующие производителя ', searchManufacturer, ' не найдены.');
            end;
            
            4: begin // Поиск комплектующих в ценовом диапазоне
              searchMinPrice := SafeReadReal('Введите минимальную цену: ', 0, 1000000);
              searchMaxPrice := SafeReadReal('Введите максимальную цену: ', searchMinPrice, 1000000);
              searchResult := FindComponentsByPriceRange(searchMinPrice, searchMaxPrice);
              
              if searchResult <> nil then
              begin
                ClrScr;
                PrintHeader('РЕЗУЛЬТАТЫ ПОИСКА');
                PrintComponents(searchResult);
              end
              else
                WriteLn('Комплектующие в ценовом диапазоне от ', searchMinPrice:0:2, ' до ', searchMaxPrice:0:2, ' не найдены.');
            end;
          end;
          
          WaitForKey;
        end;
      end;
      
      5: begin // Добавление данных
        ShowAddSubmenu;
        submenuChoice := SafeReadInteger('Выберите тип данных для добавления: ', 0, 3);
        
        if submenuChoice <> 0 then
        begin
          case submenuChoice of
            1: begin // Добавление комплектующей
              ClrScr;
              PrintHeader('ДОБАВЛЕНИЕ КОМПЛЕКТУЮЩЕЙ');
              
              component.Code := GetNewComponentCode;
              WriteLn('Код комплектующей: ', component.Code);
              
              // Выводим список типов комплектующих
              PrintComponentTypes(DataLists.ComponentTypes);
              
              component.TypeCode := SafeReadInteger('Введите код типа комплектующей: ', 1, 9999);
              component.Manufacturer := SafeReadString('Введите производителя: ');
              component.Model := SafeReadString('Введите модель: ');
              component.Parameters := SafeReadString('Введите параметры (в формате "ключ:значение"): ');
              component.Price := SafeReadReal('Введите цену: ', 0, 1000000);
              component.InStock := SafeReadBoolean('Наличие');
              
              AddComponent(component);
              WriteLn('Комплектующая успешно добавлена.');
            end;
            
            2: begin // Добавление типа комплектующей
              ClrScr;
              PrintHeader('ДОБАВЛЕНИЕ ТИПА КОМПЛЕКТУЮЩЕЙ');
              
              componentType.Code := GetNewComponentTypeCode;
              WriteLn('Код типа комплектующей: ', componentType.Code);
              
              componentType.Name := SafeReadString('Введите название типа: ');
              
              AddComponentType(componentType);
              WriteLn('Тип комплектующей успешно добавлен.');
            end;
            
            3: begin // Добавление совместимости
              ClrScr;
              PrintHeader('ДОБАВЛЕНИЕ СОВМЕСТИМОСТИ');
              
              // Выводим список комплектующих
              PrintComponents(DataLists.Components);
              
              compatibilityRecord.ComponentCode1 := SafeReadInteger('Введите код первой комплектующей: ', 1, 9999);
              compatibilityRecord.ComponentCode2 := SafeReadInteger('Введите код второй комплектующей: ', 1, 9999);
              
              if (FindComponentByCode(compatibilityRecord.ComponentCode1) = nil) or
                 (FindComponentByCode(compatibilityRecord.ComponentCode2) = nil) then
                WriteLn('Ошибка: одна из комплектующих не найдена.')
              else
              begin
                AddCompatibility(compatibilityRecord);
                WriteLn('Совместимость успешно добавлена.');
              end;
            end;
          end;
          
          WaitForKey;
        end;
      end;
      
      6: begin // Удаление данных
        ShowDeleteSubmenu;
        submenuChoice := SafeReadInteger('Выберите тип данных для удаления: ', 0, 4);
        
        if submenuChoice <> 0 then
        begin
          case submenuChoice of
            1: begin // Удаление комплектующей
              ClrScr;
              PrintHeader('УДАЛЕНИЕ КОМПЛЕКТУЮЩЕЙ');
              
              // Выводим список комплектующих
              PrintComponents(DataLists.Components);
              
              searchCode := SafeReadInteger('Введите код комплектующей для удаления: ', 1, 9999);
              
              if DeleteComponent(searchCode) then
                WriteLn('Комплектующая успешно удалена.')
              else
                WriteLn('Комплектующая с кодом ', searchCode, ' не найдена.');
            end;
            
            2: begin // Удаление типа комплектующей
              ClrScr;
              PrintHeader('УДАЛЕНИЕ ТИПА КОМПЛЕКТУЮЩЕЙ');
              
              // Выводим список типов комплектующих
              PrintComponentTypes(DataLists.ComponentTypes);
              
              searchCode := SafeReadInteger('Введите код типа комплектующей для удаления: ', 1, 9999);
              
              if DeleteComponentType(searchCode) then
                WriteLn('Тип комплектующей успешно удален.')
              else
                WriteLn('Тип комплектующей с кодом ', searchCode, ' не найден.');
            end;
            
            3: begin // Удаление совместимости
              ClrScr;
              PrintHeader('УДАЛЕНИЕ СОВМЕСТИМОСТИ');
              
              // Выводим список совместимости
              PrintCompatibility(DataLists.Compatibilities);
              
              searchCode := SafeReadInteger('Введите код первой комплектующей: ', 1, 9999);
              searchTypeCode := SafeReadInteger('Введите код второй комплектующей: ', 1, 9999);
              
              if DeleteCompatibility(searchCode, searchTypeCode) then
                WriteLn('Совместимость успешно удалена.')
              else
                WriteLn('Совместимость между комплектующими с кодами ', searchCode, ' и ', searchTypeCode, ' не найдена.');
            end;
            
            4: begin // Удаление заказа
              ClrScr;
              PrintHeader('УДАЛЕНИЕ ЗАКАЗА');
              
              // Выводим список заказов
              PrintOrders(DataLists.Orders);
              
              searchCode := SafeReadInteger('Введите номер заказа для удаления: ', 1, 9999);
              
              if DeleteOrder(searchCode) then
                WriteLn('Заказ успешно удален.')
              else
                WriteLn('Заказ с номером ', searchCode, ' не найден.');
            end;
          end;
          
          WaitForKey;
        end;
      end;
      
      7: begin // Редактирование данных
        ShowEditSubmenu;
        submenuChoice := SafeReadInteger('Выберите тип данных для редактирования: ', 0, 4);
        
        if submenuChoice <> 0 then
        begin
          case submenuChoice of
            1: begin // Редактирование комплектующей
              ClrScr;
              PrintHeader('РЕДАКТИРОВАНИЕ КОМПЛЕКТУЮЩЕЙ');
              
              // Выводим список комплектующих
              PrintComponents(DataLists.Components);
              
              searchCode := SafeReadInteger('Введите код комплектующей для редактирования: ', 1, 9999);
              searchResult := FindComponentByCode(searchCode);
              
              if searchResult <> nil then
              begin
                component := searchResult^.Data;
                
                // Выводим список типов комплектующих
                PrintComponentTypes(DataLists.ComponentTypes);
                
                component.TypeCode := SafeReadInteger('Введите код типа комплектующей: ', 1, 9999);
                component.Manufacturer := SafeReadString('Введите производителя: ');
                component.Model := SafeReadString('Введите модель: ');
                component.Parameters := SafeReadString('Введите параметры (в формате "ключ:значение"): ');
                component.Price := SafeReadReal('Введите цену: ', 0, 1000000);
                component.InStock := SafeReadBoolean('Наличие');
                
                EditComponent(component);
                WriteLn('Комплектующая успешно отредактирована.');
              end
              else
                WriteLn('Комплектующая с кодом ', searchCode, ' не найдена.');
            end;
            
            2: begin // Редактирование типа комплектующей
              ClrScr;
              PrintHeader('РЕДАКТИРОВАНИЕ ТИПА КОМПЛЕКТУЮЩЕЙ');
              
              // Выводим список типов комплектующих
              PrintComponentTypes(DataLists.ComponentTypes);
              
              searchCode := SafeReadInteger('Введите код типа комплектующей для редактирования: ', 1, 9999);
              
              if FindComponentTypeByCode(searchCode) <> nil then
              begin
                componentType.Code := searchCode;
                componentType.Name := SafeReadString('Введите название типа: ');
                
                EditComponentType(componentType);
                WriteLn('Тип комплектующей успешно отредактирован.');
              end
              else
                WriteLn('Тип комплектующей с кодом ', searchCode, ' не найден.');
            end;
            
            3: begin // Редактирование совместимости
              ClrScr;
              PrintHeader('РЕДАКТИРОВАНИЕ СОВМЕСТИМОСТИ');
              
              WriteLn('Для редактирования совместимости необходимо удалить существующую запись и добавить новую.');
              
              // Выводим список совместимости
              PrintCompatibility(DataLists.Compatibilities);
            end;
            
            4: begin // Редактирование заказа
              ClrScr;
              PrintHeader('РЕДАКТИРОВАНИЕ ЗАКАЗА');
              
              // Выводим список заказов
              PrintOrders(DataLists.Orders);
              
              searchCode := SafeReadInteger('Введите номер заказа для редактирования: ', 1, 9999);
              
              if FindOrderByNumber(searchCode) <> nil then
              begin
                order.OrderNumber := searchCode;
                order.Date := Now;
                order.CustomerName := SafeReadString('Введите имя заказчика: ');
                order.CustomerPhone := SafeReadString('Введите телефон заказчика: ');
                
                // Редактирование списка комплектующих в заказе
                // (для простоты оставляем список комплектующих без изменений)
                
                EditOrder(order);
                WriteLn('Заказ успешно отредактирован.');
              end
              else
                WriteLn('Заказ с номером ', searchCode, ' не найден.');
            end;
          end;
          
          WaitForKey;
        end;
      end;
      
      8: begin // Специальные функции
        ShowSpecialFunctionsSubmenu;
        submenuChoice := SafeReadInteger('Выберите специальную функцию: ', 0, 3);
        
        if submenuChoice <> 0 then
        begin
          case submenuChoice of
            1: begin // Подбор вариантов комплектации компьютера
              ClrScr;
              PrintHeader('ПОДБОР ВАРИАНТОВ КОМПЛЕКТАЦИИ КОМПЬЮТЕРА');
              
              minPrice := SafeReadReal('Введите минимальную цену: ', 0, 1000000);
              maxPrice := SafeReadReal('Введите максимальную цену: ', minPrice, 1000000);
              
              // Выводим список типов комплектующих
              PrintComponentTypes(DataLists.ComponentTypes);
              
              typeCount := SafeReadInteger('Введите количество типов комплектующих для конфигурации: ', 1, 10);
              SetLength(selectedTypes, typeCount);
              
              for i := 0 to typeCount - 1 do
                selectedTypes[i] := SafeReadInteger('Введите код типа комплектующей #' + IntToStr(i + 1) + ': ', 1, 9999);
              
              // Подбираем варианты комплектации
              DataLists.Configurations := FindComputerConfigurations(minPrice, maxPrice, selectedTypes);
              
              // Выводим результаты
              ClrScr;
              PrintHeader('РЕЗУЛЬТАТЫ ПОДБОРА КОНФИГУРАЦИЙ');
              
              // Выводим конфигурации в виде таблицы
              PrintConfigurations(DataLists.Configurations);
              
              // Записываем результаты в файл
              if DataLists.Configurations <> nil then
                WriteConfigurationsToTextFile(DataLists.Configurations);
            end;
            
            2: begin // Оформление заказа
              ClrScr;
              PrintHeader('ОФОРМЛЕНИЕ ЗАКАЗА');
              
              if DataLists.Configurations <> nil then
              begin
                // Выводим конфигурации в виде таблицы
                PrintConfigurations(DataLists.Configurations);
                
                // Инициализируем переменную для подсчета количества конфигураций
                Current := DataLists.Configurations;
                i := 0;
                while Current <> nil do
                begin
                  Inc(i);
                  Current := Current^.Next;
                end;
                
                searchCode := SafeReadInteger('Введите номер конфигурации для заказа: ', 1, i);
                
                // Запрашиваем данные заказчика
                customerName := SafeReadString('Введите имя заказчика: ');
                customerPhone := SafeReadString('Введите телефон заказчика: ');
                
                // Создаем заказ
                order := CreateOrder(searchCode, customerName, customerPhone);
                
                // Добавляем заказ в список
                AddOrder(order);
                
                // Выводим информацию о заказе
                ClrScr;
                PrintHeader('ИНФОРМАЦИЯ О ЗАКАЗЕ');
                WriteLn('Номер заказа: ', order.OrderNumber);
                WriteLn('Дата: ', DateToStr(order.Date));
                WriteLn('Заказчик: ', order.CustomerName);
                WriteLn('Телефон: ', order.CustomerPhone);
                WriteLn('Общая стоимость: ', order.TotalPrice:0:2);
                
                // Записываем информацию о заказе в файл
                WriteOrderInfoToTextFile(order);
              end
              else
              begin
                WriteLn('Сначала необходимо подобрать варианты комплектации компьютера (пункт 1).');
                WaitForKey;
              end;
            end;
            
            3: begin // Поиск совместимых комплектующих заданного типа
              ClrScr;
              PrintHeader('ПОИСК СОВМЕСТИМЫХ КОМПЛЕКТУЮЩИХ ЗАДАННОГО ТИПА');
              
              // Выводим список комплектующих
              PrintComponents(DataLists.Components);
              
              searchCode := SafeReadInteger('Введите код комплектующей: ', 1, 9999);
              
              // Выводим список типов комплектующих
              PrintComponentTypes(DataLists.ComponentTypes);
              
              searchTypeCode := SafeReadInteger('Введите код типа комплектующей: ', 1, 9999);
              
              // Ищем совместимые комплектующие
              searchResult := FindCompatibleComponentsOfType(searchCode, searchTypeCode);
              
              // Выводим результаты
              if searchResult <> nil then
              begin
                ClrScr;
                PrintHeader('РЕЗУЛЬТАТЫ ПОИСКА СОВМЕСТИМЫХ КОМПЛЕКТУЮЩИХ');
                PrintComponents(searchResult);
                
                // Записываем результаты в файл
                WriteCompatibleComponentsToTextFile(searchResult);
              end
              else
                WriteLn('Не найдено совместимых комплектующих заданного типа.');
            end;
            
          end;
          
          WaitForKey;
        end;
      end;
      
      9: begin // Выход без сохранения
        exitWithSave := False;
        exitProgram := True;
      end;
      
      10: begin // Выход с сохранением
        exitWithSave := True;
        exitProgram := True;
      end;
    end;
  end;
  
  // Сохранение данных при выходе, если нужно
  if exitWithSave then
    WriteDataToFiles;
  
  // Освобождение памяти
  FreeComponentsList;
  FreeComponentTypesList;
  FreeCompatibilityList;
  FreeOrdersList;
  
  WriteLn('Программа завершена.');
end.
