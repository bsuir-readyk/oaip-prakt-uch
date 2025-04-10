# Общее руководство по рефакторингу кода для соответствия принципам структурного программирования

## Введение

Данное руководство предоставляет общие инструкции и шаблоны для рефакторинга кода с целью устранения операторов `Continue`, `Break` и `Exit`, которые нарушают принципы структурного программирования. Эти принципы предполагают использование только трех базовых конструкций: последовательность, выбор (if-then-else) и повторение (циклы), с единственным входом и единственным выходом для каждого блока кода.

## Общие шаблоны рефакторинга

### 1. Замена оператора Continue

#### Шаблон 1: Использование условной конструкции if-then-else

```pascal
// До рефакторинга
while condition do
begin
  if skipCondition then
    Continue;
  
  // Код, который выполняется, если не было Continue
end;

// После рефакторинга
while condition do
begin
  if not skipCondition then
  begin
    // Код, который выполняется, если не было Continue
  end;
end;
```

#### Шаблон 2: Использование составных условий в циклах

```pascal
// До рефакторинга
repeat
  // Действие 1
  if errorCondition1 then
  begin
    // Обработка ошибки
    Continue;
  end;
  
  // Действие 2
  if errorCondition2 then
  begin
    // Обработка ошибки
    Continue;
  end;
  
  // Основной код
  Break;
until False;

// После рефакторинга
validInput := false;
repeat
  // Действие 1
  if not errorCondition1 then
  begin
    // Действие 2
    if not errorCondition2 then
    begin
      // Основной код
      validInput := true;
    end;
  end;
until validInput;
```

### 2. Замена оператора Break

#### Шаблон 1: Использование логического флага

```pascal
// До рефакторинга
while condition do
begin
  if exitCondition then
    Break;
  
  // Код, который выполняется, если не было Break
end;

// После рефакторинга
shouldContinue := true;
while condition and shouldContinue do
begin
  if exitCondition then
    shouldContinue := false
  else
  begin
    // Код, который выполняется, если не было Break
  end;
end;
```

#### Шаблон 2: Использование составных условий в заголовке цикла

```pascal
// До рефакторинга
Current := List;
while Current <> nil do
begin
  if Current^.Data.Code = Code then
    Break;
  
  Current := Current^.Next;
end;

// После рефакторинга
Current := List;
found := false;
while (Current <> nil) and (not found) do
begin
  if Current^.Data.Code = Code then
    found := true
  else
    Current := Current^.Next;
end;
```

### 3. Замена оператора Exit

#### Шаблон 1: Использование переменной результата и флага завершения

```pascal
// До рефакторинга
function SomeFunction: Boolean;
begin
  if condition1 then
  begin
    // Действия
    Exit(true);
  end;
  
  if condition2 then
  begin
    // Другие действия
    Exit(false);
  end;
  
  // Код по умолчанию
  SomeFunction := true;
end;

// После рефакторинга
function SomeFunction: Boolean;
var
  result: Boolean;
  done: Boolean;
begin
  result := true; // Значение по умолчанию
  done := false;
  
  if (not done) and condition1 then
  begin
    // Действия
    result := true;
    done := true;
  end;
  
  if (not done) and condition2 then
  begin
    // Другие действия
    result := false;
    done := true;
  end;
  
  // Если не было выполнено ни одно из условий, используется значение по умолчанию
  SomeFunction := result;
end;
```

#### Шаблон 2: Использование условных блоков для раннего выхода из процедуры

```pascal
// До рефакторинга
procedure SomeProc;
begin
  if not condition then
    Exit;
  
  // Основной код процедуры
end;

// После рефакторинга
procedure SomeProc;
begin
  if condition then
  begin
    // Основной код процедуры
  end;
end;
```

## Специфические случаи рефакторинга

### 1. Рефакторинг case-конструкций с Continue

```pascal
// До рефакторинга
case choice of
  1: begin
    // Действия для выбора 1
  end;
  
  2: begin
    // Действия для выбора 2
  end;
  
  0: Continue;
end;

// После рефакторинга
if choice <> 0 then
begin
  case choice of
    1: begin
      // Действия для выбора 1
    end;
    
    2: begin
      // Действия для выбора 2
    end;
  end;
end;
```

### 2. Рефакторинг функций поиска с ранним выходом

```pascal
// До рефакторинга
function FindItem(Code: Integer): PItemNode;
var
  Current: PItemNode;
begin
  Current := List;
  
  while Current <> nil do
  begin
    if Current^.Data.Code = Code then
      Exit(Current);
    
    Current := Current^.Next;
  end;
  
  FindItem := nil;
end;

// После рефакторинга
function FindItem(Code: Integer): PItemNode;
var
  Current: PItemNode;
  found: Boolean;
  result: PItemNode;
begin
  Current := List;
  found := false;
  result := nil;
  
  while (Current <> nil) and (not found) do
  begin
    if Current^.Data.Code = Code then
    begin
      result := Current;
      found := true;
    end
    else
      Current := Current^.Next;
  end;
  
  FindItem := result;
end;
```

### 3. Рефакторинг процедур сортировки с ранним выходом

```pascal
// До рефакторинга
procedure SortItems(var List: PItemNode);
begin
  if (List = nil) or (List^.Next = nil) then
    Exit;
  
  // Код сортировки
end;

// После рефакторинга
procedure SortItems(var List: PItemNode);
var
  needsSorting: Boolean;
begin
  needsSorting := (List <> nil) and (List^.Next <> nil);
  
  if needsSorting then
  begin
    // Код сортировки
  end;
end;
```

### 4. Рефакторинг рекурсивных функций с ранним выходом

```pascal
// До рефакторинга
procedure RecursiveProc(param1, param2);
begin
  if endCondition then
    Exit;
  
  // Обработка текущего шага
  
  // Рекурсивный вызов
  RecursiveProc(newParam1, newParam2);
end;

// После рефакторинга
procedure RecursiveProc(param1, param2);
begin
  if not endCondition then
  begin
    // Обработка текущего шага
    
    // Рекурсивный вызов
    RecursiveProc(newParam1, newParam2);
  end;
end;
```

## Пошаговый процесс рефакторинга

1. **Идентификация проблемных мест**: Найдите все места в коде, где используются операторы `Continue`, `Break` и `Exit`.

2. **Выбор подходящего шаблона**: Для каждого проблемного места выберите подходящий шаблон рефакторинга из предложенных выше.

3. **Применение шаблона**: Примените выбранный шаблон, адаптируя его к конкретному случаю.

4. **Проверка логики**: Убедитесь, что после рефакторинга логика работы кода не изменилась.

5. **Тестирование**: Протестируйте рефакторированный код, чтобы убедиться в его корректности.

## Рекомендации по реализации

1. **Начните с простых случаев**: Сначала рефакторите простые случаи, такие как функции безопасного ввода в файле ui.pas, чтобы отработать методику.

2. **Постепенно переходите к сложным случаям**: После успешного рефакторинга простых случаев переходите к более сложным, таким как рекурсивные функции в файле specialFunctions.pas.

3. **Сохраняйте промежуточные версии**: Создавайте резервные копии файлов перед рефакторингом, чтобы иметь возможность вернуться к работающей версии в случае проблем.

4. **Тестируйте после каждого изменения**: Проверяйте работоспособность программы после рефакторинга каждого файла.

5. **Используйте единый стиль**: Придерживайтесь единого стиля рефакторинга для всего проекта, чтобы код был понятным и согласованным.

## Заключение

Следуя предложенным шаблонам и рекомендациям, вы сможете успешно рефакторить код проекта, устраняя использование операторов `Continue`, `Break` и `Exit`, и приводя его в соответствие с принципами структурного программирования. Это сделает код более понятным, поддерживаемым и менее подверженным ошибкам.
