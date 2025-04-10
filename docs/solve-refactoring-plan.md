# Детальный план рефакторинга файла solve.dpr

## Анализ текущего кода

Файл `solve.dpr` является основным файлом программы и содержит главный цикл обработки меню. В нем используются операторы `Continue` в следующих местах:

1. В обработке подменю просмотра списка (строка 80)
2. В обработке подменю сортировки данных (строка 94)
3. В обработке подменю поиска данных (строка 171)
4. В обработке подменю добавления данных (строка 236)
5. В обработке подменю удаления данных (строка 308)
6. В обработке подменю редактирования данных (строка 407)
7. В обработке подменю специальных функций (строка 488, 562)

## Детальный план рефакторинга

### 1. Рефакторинг главного цикла программы

**Текущий код:**
```pascal
exitWithSave := False;
choice := -1;

while choice <> 9 do
begin
  ShowMainMenu;
  choice := SafeReadInteger('Выберите пункт меню: ', 1, 10);
  
  case choice of
    // Обработка различных пунктов меню
  end;
end;
```

**Рефакторинг:**
```pascal
exitWithSave := False;
choice := -1;
exitProgram := false;

while not exitProgram do
begin
  ShowMainMenu;
  choice := SafeReadInteger('Выберите пункт меню: ', 1, 10);
  
  case choice of
    9: exitProgram := true;
    10: begin
      exitWithSave := True;
      exitProgram := true;
    end;
    // Обработка других пунктов меню
  end;
end;
```

### 2. Рефакторинг обработки подменю просмотра списка

**Текущий код:**
```pascal
2: begin // Просмотр списка
  ShowViewSubmenu;
  submenuChoice := SafeReadInteger('Выберите список для просмотра: ', 0, 4);
  
  case submenuChoice of
    1: PrintComponents(DataLists.Components);
    2: PrintComponentTypes(DataLists.ComponentTypes);
    3: PrintCompatibility(DataLists.Compatibilities);
    4: PrintOrders(DataLists.Orders);
    0: Continue;
  end;
  
  WaitForKey;
end;
```

**Рефакторинг:**
```pascal
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
```

### 3. Рефакторинг обработки подменю сортировки данных

**Текущий код:**
```pascal
3: begin // Сортировка данных
  ShowSortSubmenu;
  submenuChoice := SafeReadInteger('Выберите тип сортировки: ', 0, 3);
  
  case submenuChoice of
    1: SortComponentsByPrice(DataLists.Components);
    2: SortComponentsByStock(DataLists.Components);
    3: SortComponentsByManufacturer(DataLists.Components);
    0: Continue;
  end;
  
  PrintComponents(DataLists.Components);
  WaitForKey;
end;
```

**Рефакторинг:**
```pascal
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
```

### 4. Рефакторинг обработки подменю поиска данных

**Текущий код:**
```pascal
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
  
  case submenuChoice of
    // Различные типы поиска
    0: Continue;
  end;
  
  WaitForKey;
end;
```

**Рефакторинг:**
```pascal
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
      // Различные типы поиска
    end;
    
    WaitForKey;
  end;
end;
```

### 5. Рефакторинг обработки подменю добавления данных

**Текущий код:**
```pascal
5: begin // Добавление данных
  ShowAddSubmenu;
  submenuChoice := SafeReadInteger('Выберите тип данных для добавления: ', 0, 3);
  
  case submenuChoice of
    // Различные типы добавления данных
    0: Continue;
  end;
  
  WaitForKey;
end;
```

**Рефакторинг:**
```pascal
5: begin // Добавление данных
  ShowAddSubmenu;
  submenuChoice := SafeReadInteger('Выберите тип данных для добавления: ', 0, 3);
  
  if submenuChoice <> 0 then
  begin
    case submenuChoice of
      // Различные типы добавления данных
    end;
    
    WaitForKey;
  end;
end;
```

### 6. Рефакторинг обработки подменю удаления данных

**Текущий код:**
```pascal
6: begin // Удаление данных
  ShowDeleteSubmenu;
  submenuChoice := SafeReadInteger('Выберите тип данных для удаления: ', 0, 4);
  
  case submenuChoice of
    // Различные типы удаления данных
    0: Continue;
  end;
  
  WaitForKey;
end;
```

**Рефакторинг:**
```pascal
6: begin // Удаление данных
  ShowDeleteSubmenu;
  submenuChoice := SafeReadInteger('Выберите тип данных для удаления: ', 0, 4);
  
  if submenuChoice <> 0 then
  begin
    case submenuChoice of
      // Различные типы удаления данных
    end;
    
    WaitForKey;
  end;
end;
```

### 7. Рефакторинг обработки подменю редактирования данных

**Текущий код:**
```pascal
7: begin // Редактирование данных
  ShowEditSubmenu;
  submenuChoice := SafeReadInteger('Выберите тип данных для редактирования: ', 0, 4);
  
  case submenuChoice of
    // Различные типы редактирования данных
    0: Continue;
  end;
  
  WaitForKey;
end;
```

**Рефакторинг:**
```pascal
7: begin // Редактирование данных
  ShowEditSubmenu;
  submenuChoice := SafeReadInteger('Выберите тип данных для редактирования: ', 0, 4);
  
  if submenuChoice <> 0 then
  begin
    case submenuChoice of
      // Различные типы редактирования данных
    end;
    
    WaitForKey;
  end;
end;
```

### 8. Рефакторинг обработки подменю специальных функций

**Текущий код:**
```pascal
8: begin // Специальные функции
  ShowSpecialFunctionsSubmenu;
  submenuChoice := SafeReadInteger('Выберите специальную функцию: ', 0, 3);
  
  case submenuChoice of
    // Различные специальные функции
    0: Continue;
  end;
  
  WaitForKey;
end;
```

**Рефакторинг:**
```pascal
8: begin // Специальные функции
  ShowSpecialFunctionsSubmenu;
  submenuChoice := SafeReadInteger('Выберите специальную функцию: ', 0, 3);
  
  if submenuChoice <> 0 then
  begin
    case submenuChoice of
      // Различные специальные функции
    end;
    
    WaitForKey;
  end;
end;
```

### 9. Рефакторинг обработки специальной функции "Оформление заказа"

**Текущий код:**
```pascal
2: begin // Оформление заказа
  ClrScr;
  PrintHeader('ОФОРМЛЕНИЕ ЗАКАЗА');
  
  if DataLists.Configurations = nil then
  begin
    WriteLn('Сначала необходимо подобрать варианты комплектации компьютера (пункт 1).');
    WaitForKey;
    Continue;
  end;
  
  // Код оформления заказа
end;
```

**Рефакторинг:**
```pascal
2: begin // Оформление заказа
  ClrScr;
  PrintHeader('ОФОРМЛЕНИЕ ЗАКАЗА');
  
  if DataLists.Configurations <> nil then
  begin
    // Код оформления заказа
  end
  else
  begin
    WriteLn('Сначала необходимо подобрать варианты комплектации компьютера (пункт 1).');
    WaitForKey;
  end;
end;
```

## Общий подход к рефакторингу

Основной подход к рефакторингу файла solve.dpr заключается в:

1. Замене оператора `Continue` в case-конструкциях на условные конструкции if-then-else
2. Изменении логики главного цикла программы с использованием логического флага `exitProgram`
3. Реорганизации обработки подменю с выносом кода в условные блоки

Этот подход сохраняет исходную функциональность кода, но делает его более структурированным и понятным, следуя принципам структурного программирования.

После рефакторинга файла solve.dpr, мы получим шаблоны для обработки меню, которые можно будет применить к другим частям кода проекта.
