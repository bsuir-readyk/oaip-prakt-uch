# Примеры рефакторированного кода для файла solve.dpr

В этом документе представлены примеры рефакторированного кода для файла `solve.dpr`, где операторы `Continue` заменены на конструкции, соответствующие принципам структурного программирования.

## Главный цикл программы

### Исходный код:

```pascal
exitWithSave := False;
choice := -1;

while choice <> 9 do
begin
  ShowMainMenu;
  choice := SafeReadInteger('Выберите пункт меню: ', 1, 10);
  
  case choice of
    // Обработка различных пунктов меню
    9: // Выход без сохранения
      exitWithSave := False;
    
    10: begin // Выход с сохранением
      exitWithSave := True;
      choice := 9; // Для выхода из цикла
    end;
  end;
end;
```

### Рефакторированный код:

```pascal
exitWithSave := False;
choice := -1;
exitProgram := false;

while not exitProgram do
begin
  ShowMainMenu;
  choice := SafeReadInteger('Выберите пункт меню: ', 1, 10);
  
  case choice of
    // Обработка различных пунктов меню
    9: begin // Выход без сохранения
      exitWithSave := False;
      exitProgram := true;
    end;
    
    10: begin // Выход с сохранением
      exitWithSave := True;
      exitProgram := true;
    end;
  end;
end;
```

## Обработка подменю просмотра списка

### Исходный код:

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

### Рефакторированный код:

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

## Обработка подменю сортировки данных

### Исходный код:

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

### Рефакторированный код:

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

## Обработка подменю поиска данных

### Исходный код:

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

### Рефакторированный код:

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

## Обработка специальной функции "Оформление заказа"

### Исходный код:

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

### Рефакторированный код:

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

## Заключение

Основной подход к рефакторингу файла solve.dpr заключается в:

1. Замене оператора `Continue` в case-конструкциях на условные конструкции if-then-else
2. Изменении логики главного цикла программы с использованием логического флага `exitProgram`
3. Реорганизации обработки подменю с выносом кода в условные блоки
4. Инвертировании условий для устранения ранних выходов из блоков

Этот подход сохраняет исходную функциональность кода, но делает его более структурированным и понятным, следуя принципам структурного программирования.
