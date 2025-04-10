# Примеры рефакторированного кода для файла ui.pas

В этом документе представлены примеры рефакторированного кода для файла `ui.pas`, где операторы `Continue` и `Break` заменены на конструкции, соответствующие принципам структурного программирования.

## Функция SafeReadInteger

### Исходный код:

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

### Рефакторированный код:

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

## Функция SafeReadReal

### Исходный код:

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

### Рефакторированный код:

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

## Функция SafeReadString

### Исходный код:

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

### Рефакторированный код:

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

## Функция SafeReadBoolean

### Исходный код:

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

### Рефакторированный код:

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

## Заключение

Основной подход к рефакторингу функций безопасного ввода заключается в:

1. Замене конструкции `repeat ... until False` с операторами `Continue` и `Break` на конструкцию `repeat ... until validInput`
2. Введении логической переменной `validInput` для контроля выхода из цикла
3. Использовании условных конструкций if-then-else вместо операторов досрочного выхода

Этот подход сохраняет исходную функциональность кода, но делает его более структурированным и понятным, следуя принципам структурного программирования.
