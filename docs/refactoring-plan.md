# Стратегия рефакторинга кода для соответствия принципам структурного программирования

## Введение

Данный документ описывает стратегию рефакторинга кода проекта для устранения использования операторов `Continue`, `Break` и `Exit`, которые нарушают принципы структурного программирования. Структурное программирование предполагает использование только трех базовых конструкций: последовательность, выбор (if-then-else) и повторение (циклы), с единственным входом и единственным выходом для каждого блока кода.

## Общие принципы рефакторинга

### 1. Замена оператора Continue

Оператор `Continue` используется для пропуска оставшейся части текущей итерации цикла и перехода к следующей итерации. Для его замены будем использовать следующий подход:

```pascal
// Исходный код с Continue
while condition do
begin
  if skipCondition then
    Continue;
  
  // Код, который выполняется, если не было Continue
end;

// Рефакторинг с использованием if-then-else
while condition do
begin
  if not skipCondition then
  begin
    // Код, который выполняется, если не было Continue
  end;
end;
```

### 2. Замена оператора Break

Оператор `Break` используется для досрочного выхода из цикла. Для его замены будем использовать логический флаг:

```pascal
// Исходный код с Break
while condition do
begin
  if exitCondition then
    Break;
  
  // Код, который выполняется, если не было Break
end;

// Рефакторинг с использованием логического флага
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

### 3. Замена оператора Exit

Оператор `Exit` используется для досрочного выхода из процедуры или функции. Для его замены будем использовать переменную результата и условные конструкции:

```pascal
// Исходный код с Exit
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

// Рефакторинг с использованием переменной результата
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

## Типовые шаблоны рефакторинга для конкретных случаев

### 1. Рефакторинг циклов с множественными Continue

```pascal
// Исходный код
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

// Рефакторинг
repeat
  validInput := true;
  
  // Действие 1
  if errorCondition1 then
  begin
    // Обработка ошибки
    validInput := false;
  end;
  
  if validInput then
  begin
    // Действие 2
    if errorCondition2 then
    begin
      // Обработка ошибки
      validInput := false;
    end;
  end;
  
  if validInput then
  begin
    // Основной код
    done := true;
  end;
until done;
```

### 2. Рефакторинг функций поиска с ранним выходом

```pascal
// Исходный код
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

// Рефакторинг
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

### 3. Рефакторинг case-конструкций с Continue

```pascal
// Исходный код
case choice of
  1: begin
    // Действия для выбора 1
  end;
  
  2: begin
    // Действия для выбора 2
  end;
  
  0: Continue;
end;

// Рефакторинг
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

## Стратегия тестирования и валидации

1. **Пошаговый рефакторинг**: Изменения будут вноситься постепенно, файл за файлом, с проверкой работоспособности после каждого изменения.

2. **Сохранение функциональности**: Основной принцип - сохранение исходной функциональности программы. Каждое изменение должно быть эквивалентно исходному коду по логике работы.

3. **Проверка граничных случаев**: Особое внимание будет уделено проверке граничных случаев, где ранее использовались операторы досрочного выхода.

## План рефакторинга по файлам

### 1. ui.pas

Этот файл содержит функции пользовательского интерфейса и безопасного ввода данных, где активно используются операторы `Continue` и `Break`. Рефакторинг этого файла позволит создать шаблоны для дальнейшей работы.

### 2. solve.dpr

Основной файл программы содержит множество операторов `Continue` в обработке меню. Рефакторинг этого файла потребует переработки логики обработки пользовательского ввода.

### 3. compatibility.pas, components.pas, componentTypes.pas

Эти файлы содержат функции для работы с данными, где используются операторы `Break` и `Exit` для раннего выхода из циклов и функций.

### 4. specialFunctions.pas

Этот файл содержит сложные алгоритмы с рекурсией и ранними выходами, что потребует особого внимания при рефакторинге.

### 5. fileIO.pas, orders.pas

Эти файлы содержат меньше проблемных операторов и могут быть рефакторированы в последнюю очередь.

## Заключение

Предложенная стратегия рефакторинга позволит систематически устранить использование операторов `Continue`, `Break` и `Exit` в коде проекта, сохраняя при этом исходную функциональность и улучшая структуру кода в соответствии с принципами структурного программирования.

После утверждения общей стратегии можно приступить к детальному рефакторингу каждого файла, начиная с наиболее критичных частей кода.
