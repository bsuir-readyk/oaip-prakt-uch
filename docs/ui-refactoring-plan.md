# Детальный план рефакторинга файла ui.pas

## Анализ текущего кода

Файл `ui.pas` содержит функции пользовательского интерфейса и безопасного ввода данных. В нем используются операторы `Continue` и `Break` в следующих функциях:

1. `SafeReadInteger` (строки 145-168) - использует `Continue` и `Break` в цикле ввода
2. `SafeReadReal` (строки 171-193) - использует `Continue` и `Break` в цикле ввода
3. `SafeReadString` (строки 196-216) - использует `Continue` и `Break` в цикле ввода
4. `SafeReadBoolean` (строки 219-247) - использует `Break` в цикле ввода

## Детальный план рефакторинга

### 1. Функция SafeReadInteger

**Текущий код:**
```pascal
function SafeReadInteger(const Prompt: string; MinValue, MaxValue: Integer): Integer;
var
  S: string;
  Code: Integer;
  Value: Integer;
begin
  repeat
    Write(Prompt);
    ReadLn(S);
    
    Val(S, Value, Code);
    
    if (Code <> 0) or (Value < MinValue) or (Value > MaxValue) then
    begin
      WriteLn('Ошибка! Введите целое число от ', MinValue, ' до ', MaxValue, '.');
      Continue;
    end;
    
    Break;
  until False;
  
  SafeReadInteger := Value;
end;
```

**Рефакторинг:**
```pascal
function SafeReadInteger(const Prompt: string; MinValue, MaxValue: Integer): Integer;
var
  S: string;
  Code: Integer;
  Value: Integer;
  validInput: Boolean;
begin
  validInput := false;
  
  repeat
    Write(Prompt);
    ReadLn(S);
    
    Val(S, Value, Code);
    
    if (Code <> 0) or (Value < MinValue) or (Value > MaxValue) then
      WriteLn('Ошибка! Введите целое число от ', MinValue, ' до ', MaxValue, '.')
    else
      validInput := true;
  until validInput;
  
  SafeReadInteger := Value;
end;
```

### 2. Функция SafeReadReal

**Текущий код:**
```pascal
function SafeReadReal(const Prompt: string; MinValue, MaxValue: Real): Real;
var
  S: string;
  Code: Integer;
  Value: Real;
begin
  repeat
    Write(Prompt);
    ReadLn(S);
    
    Val(S, Value, Code);
    
    if (Code <> 0) or (Value < MinValue) or (Value > MaxValue) then
    begin
      WriteLn('Ошибка! Введите число от ', MinValue:0:2, ' до ', MaxValue:0:2, '.');
      Continue;
    end;
    
    Break;
  until False;
  
  SafeReadReal := Value;
end;
```

**Рефакторинг:**
```pascal
function SafeReadReal(const Prompt: string; MinValue, MaxValue: Real): Real;
var
  S: string;
  Code: Integer;
  Value: Real;
  validInput: Boolean;
begin
  validInput := false;
  
  repeat
    Write(Prompt);
    ReadLn(S);
    
    Val(S, Value, Code);
    
    if (Code <> 0) or (Value < MinValue) or (Value > MaxValue) then
      WriteLn('Ошибка! Введите число от ', MinValue:0:2, ' до ', MaxValue:0:2, '.')
    else
      validInput := true;
  until validInput;
  
  SafeReadReal := Value;
end;
```

### 3. Функция SafeReadString

**Текущий код:**
```pascal
function SafeReadString(const Prompt: string): string;
var
  S: string;
begin
  repeat
    Write(Prompt);
    ReadLn(S);
    
    S := Trim(S);
    
    if S = '' then
    begin
      WriteLn('Ошибка! Строка не может быть пустой.');
      Continue;
    end;
    
    Break;
  until False;
  
  SafeReadString := S;
end;
```

**Рефакторинг:**
```pascal
function SafeReadString(const Prompt: string): string;
var
  S: string;
  validInput: Boolean;
begin
  validInput := false;
  
  repeat
    Write(Prompt);
    ReadLn(S);
    
    S := Trim(S);
    
    if S = '' then
      WriteLn('Ошибка! Строка не может быть пустой.')
    else
      validInput := true;
  until validInput;
  
  SafeReadString := S;
end;
```

### 4. Функция SafeReadBoolean

**Текущий код:**
```pascal
function SafeReadBoolean(const Prompt: string): Boolean;
var
  S: string;
  BoolValue: Boolean;
begin
  BoolValue := False;
  
  repeat
    Write(Prompt, ' (д/н): ');
    ReadLn(S);
    
    S := LowerCase(Trim(S));
    
    if (S = 'д') or (S = 'да') or (S = 'y') or (S = 'yes') then
    begin
      BoolValue := True;
      Break;
    end
    else if (S = 'н') or (S = 'нет') or (S = 'n') or (S = 'no') then
    begin
      BoolValue := False;
      Break;
    end;
    
    WriteLn('Ошибка! Введите "д" или "н".');
  until False;
  
  SafeReadBoolean := BoolValue;
end;
```

**Рефакторинг:**
```pascal
function SafeReadBoolean(const Prompt: string): Boolean;
var
  S: string;
  BoolValue: Boolean;
  validInput: Boolean;
begin
  BoolValue := False;
  validInput := false;
  
  repeat
    Write(Prompt, ' (д/н): ');
    ReadLn(S);
    
    S := LowerCase(Trim(S));
    
    if (S = 'д') or (S = 'да') or (S = 'y') or (S = 'yes') then
    begin
      BoolValue := True;
      validInput := true;
    end
    else if (S = 'н') or (S = 'нет') or (S = 'n') or (S = 'no') then
    begin
      BoolValue := False;
      validInput := true;
    end
    else
      WriteLn('Ошибка! Введите "д" или "н".');
  until validInput;
  
  SafeReadBoolean := BoolValue;
end;
```

## Общий подход к рефакторингу

Как видно из примеров выше, основной подход к рефакторингу функций безопасного ввода заключается в:

1. Замене конструкции `repeat ... until False` с операторами `Continue` и `Break` на конструкцию `repeat ... until validInput`
2. Введении логической переменной `validInput` для контроля выхода из цикла
3. Использовании условных конструкций if-then-else вместо операторов досрочного выхода

Этот подход сохраняет исходную функциональность кода, но делает его более структурированным и понятным, следуя принципам структурного программирования.

После рефакторинга функций безопасного ввода в файле ui.pas, мы получим шаблоны, которые можно будет применить к другим частям кода проекта.
