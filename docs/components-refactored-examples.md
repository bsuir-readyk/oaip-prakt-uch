# Примеры рефакторированного кода для файла components.pas

В этом документе представлены примеры рефакторированного кода для файла `components.pas`, где операторы `Exit` заменены на конструкции, соответствующие принципам структурного программирования.

## Процедура SortComponentsByPrice

### Исходный код:

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

### Рефакторированный код:

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

## Процедура SortComponentsByStock

### Исходный код:

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

### Рефакторированный код:

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

## Процедура SortComponentsByManufacturer

### Исходный код:

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

### Рефакторированный код:

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

## Заключение

Основной подход к рефакторингу процедур сортировки в файле components.pas заключается в:

1. Замене оператора `Exit` на условную конструкцию if-then-else
2. Введении логической переменной `needsSorting` для контроля выполнения сортировки
3. Обертывании основного кода сортировки в условный блок

Этот подход сохраняет исходную функциональность кода, но делает его более структурированным и понятным, следуя принципам структурного программирования.

Обратите внимание, что все три процедуры сортировки имеют очень похожую структуру, и рефакторинг для них выполняется по одному и тому же шаблону. Это позволяет применить единый подход к рефакторингу всех процедур сортировки в проекте.
