# Детальный план рефакторинга файла components.pas

## Анализ текущего кода

Файл `components.pas` содержит функции для работы с компонентами. В нем используется оператор `Exit` в следующих местах:

1. В процедуре `SortComponentsByPrice` (строка 264) - использует `Exit` для раннего выхода из процедуры
2. В процедуре `SortComponentsByStock` (строка 299) - использует `Exit` для раннего выхода из процедуры
3. В процедуре `SortComponentsByManufacturer` (строка 334) - использует `Exit` для раннего выхода из процедуры

## Детальный план рефакторинга

### 1. Рефакторинг процедуры SortComponentsByPrice

**Текущий код:**
```pascal
procedure SortComponentsByPrice(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
begin
  if (List = nil) or (List^.Next = nil) then
    Exit;
  
  Sorted := nil;
  
  while List <> nil do
  begin
    Current := List;
    List := List^.Next;
    
    if (Sorted = nil) or (Current^.Data.Price < Sorted^.Data.Price) then
    begin
      Current^.Next := Sorted;
      Sorted := Current;
    end
    else
    begin
      Temp := Sorted;
      
      while (Temp^.Next <> nil) and (Current^.Data.Price >= Temp^.Next^.Data.Price) do
        Temp := Temp^.Next;
      
      Current^.Next := Temp^.Next;
      Temp^.Next := Current;
    end;
  end;
  
  List := Sorted;
end;
```

**Рефакторинг:**
```pascal
procedure SortComponentsByPrice(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
  needsSorting: Boolean;
begin
  needsSorting := (List <> nil) and (List^.Next <> nil);
  
  if needsSorting then
  begin
    Sorted := nil;
    
    while List <> nil do
    begin
      Current := List;
      List := List^.Next;
      
      if (Sorted = nil) or (Current^.Data.Price < Sorted^.Data.Price) then
      begin
        Current^.Next := Sorted;
        Sorted := Current;
      end
      else
      begin
        Temp := Sorted;
        
        while (Temp^.Next <> nil) and (Current^.Data.Price >= Temp^.Next^.Data.Price) do
          Temp := Temp^.Next;
        
        Current^.Next := Temp^.Next;
        Temp^.Next := Current;
      end;
    end;
    
    List := Sorted;
  end;
end;
```

### 2. Рефакторинг процедуры SortComponentsByStock

**Текущий код:**
```pascal
procedure SortComponentsByStock(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
begin
  if (List = nil) or (List^.Next = nil) then
    Exit;
  
  Sorted := nil;
  
  while List <> nil do
  begin
    Current := List;
    List := List^.Next;
    
    if (Sorted = nil) or (Current^.Data.InStock and not Sorted^.Data.InStock) then
    begin
      Current^.Next := Sorted;
      Sorted := Current;
    end
    else
    begin
      Temp := Sorted;
      
      while (Temp^.Next <> nil) and (not Current^.Data.InStock or Temp^.Next^.Data.InStock) do
        Temp := Temp^.Next;
      
      Current^.Next := Temp^.Next;
      Temp^.Next := Current;
    end;
  end;
  
  List := Sorted;
end;
```

**Рефакторинг:**
```pascal
procedure SortComponentsByStock(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
  needsSorting: Boolean;
begin
  needsSorting := (List <> nil) and (List^.Next <> nil);
  
  if needsSorting then
  begin
    Sorted := nil;
    
    while List <> nil do
    begin
      Current := List;
      List := List^.Next;
      
      if (Sorted = nil) or (Current^.Data.InStock and not Sorted^.Data.InStock) then
      begin
        Current^.Next := Sorted;
        Sorted := Current;
      end
      else
      begin
        Temp := Sorted;
        
        while (Temp^.Next <> nil) and (not Current^.Data.InStock or Temp^.Next^.Data.InStock) do
          Temp := Temp^.Next;
        
        Current^.Next := Temp^.Next;
        Temp^.Next := Current;
      end;
    end;
    
    List := Sorted;
  end;
end;
```

### 3. Рефакторинг процедуры SortComponentsByManufacturer

**Текущий код:**
```pascal
procedure SortComponentsByManufacturer(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
begin
  if (List = nil) or (List^.Next = nil) then
    Exit;
  
  Sorted := nil;
  
  while List <> nil do
  begin
    Current := List;
    List := List^.Next;
    
    if (Sorted = nil) or (Current^.Data.Manufacturer < Sorted^.Data.Manufacturer) then
    begin
      Current^.Next := Sorted;
      Sorted := Current;
    end
    else
    begin
      Temp := Sorted;
      
      while (Temp^.Next <> nil) and (Current^.Data.Manufacturer >= Temp^.Next^.Data.Manufacturer) do
        Temp := Temp^.Next;
      
      Current^.Next := Temp^.Next;
      Temp^.Next := Current;
    end;
  end;
  
  List := Sorted;
end;
```

**Рефакторинг:**
```pascal
procedure SortComponentsByManufacturer(var List: PComponentNode);
var
  Sorted, Current, Temp: PComponentNode;
  needsSorting: Boolean;
begin
  needsSorting := (List <> nil) and (List^.Next <> nil);
  
  if needsSorting then
  begin
    Sorted := nil;
    
    while List <> nil do
    begin
      Current := List;
      List := List^.Next;
      
      if (Sorted = nil) or (Current^.Data.Manufacturer < Sorted^.Data.Manufacturer) then
      begin
        Current^.Next := Sorted;
        Sorted := Current;
      end
      else
      begin
        Temp := Sorted;
        
        while (Temp^.Next <> nil) and (Current^.Data.Manufacturer >= Temp^.Next^.Data.Manufacturer) do
          Temp := Temp^.Next;
        
        Current^.Next := Temp^.Next;
        Temp^.Next := Current;
      end;
    end;
    
    List := Sorted;
  end;
end;
```

## Общий подход к рефакторингу

Основной подход к рефакторингу файла components.pas заключается в:

1. Замене оператора `Exit` в процедурах сортировки на условную конструкцию if-then-else
2. Введении логической переменной `needsSorting` для контроля выполнения сортировки
3. Обертывании основного кода сортировки в условный блок

Этот подход сохраняет исходную функциональность кода, но делает его более структурированным и понятным, следуя принципам структурного программирования.

Обратите внимание, что все три процедуры сортировки имеют очень похожую структуру, и рефакторинг для них выполняется по одному и тому же шаблону. Это позволяет применить единый подход к рефакторингу всех процедур сортировки в проекте.

После рефакторинга файла components.pas, мы получим шаблоны для процедур сортировки, которые можно будет применить к другим модулям проекта, если в них также используются алгоритмы сортировки.
