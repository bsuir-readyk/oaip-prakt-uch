# Примеры рефакторированного кода для файла specialFunctions.pas

В этом документе представлены примеры рефакторированного кода для файла `specialFunctions.pas`, где операторы `Exit` заменены на конструкции, соответствующие принципам структурного программирования.

## Процедура GenerateConfigurations

### Исходный код:

```pascal
procedure GenerateConfigurations(
  var CurrentComponents: array of TComponent;
  var CurrentCount: Integer;
  ComponentsByType: array of PComponentNode;
  TypeCount: Integer;
  CurrentTypeIndex: Integer;
  MinPrice, MaxPrice: Real;
  var Configurations: PConfigurationNode);
var
  Current: PComponentNode;
  TotalPrice: Real;
  i: Integer;
  NewConfiguration: PConfigurationNode;
begin
  // Если мы обработали все типы комплектующих, проверяем конфигурацию
  if CurrentTypeIndex >= TypeCount then
  begin
    // Проверяем совместимость всех компонентов
    if not AreAllComponentsCompatible(CurrentComponents, CurrentCount) then
      Exit;
    
    // Вычисляем общую стоимость
    TotalPrice := 0;
    for i := 0 to CurrentCount - 1 do
      TotalPrice := TotalPrice + CurrentComponents[i].Price;
    
    // Проверяем, что стоимость в заданном диапазоне
    if (TotalPrice >= MinPrice) and (TotalPrice <= MaxPrice) then
    begin
      // Создаем новую конфигурацию
      New(NewConfiguration);
      NewConfiguration^.Data.ComponentCount := CurrentCount;
      SetLength(NewConfiguration^.Data.Components, CurrentCount);
      
      for i := 0 to CurrentCount - 1 do
        NewConfiguration^.Data.Components[i] := CurrentComponents[i];
      
      NewConfiguration^.Data.TotalPrice := TotalPrice;
      
      // Добавляем конфигурацию в список
      NewConfiguration^.Next := Configurations;
      Configurations := NewConfiguration;
    end;
    
    Exit;
  end;
  
  // Перебираем все комплектующие текущего типа
  Current := ComponentsByType[CurrentTypeIndex];
  
  while Current <> nil do
  begin
    // Добавляем текущую комплектующую в конфигурацию
    CurrentComponents[CurrentCount] := Current^.Data;
    Inc(CurrentCount);
    
    // Рекурсивно генерируем конфигурации для следующего типа
    GenerateConfigurations(
      CurrentComponents,
      CurrentCount,
      ComponentsByType,
      TypeCount,
      CurrentTypeIndex + 1,
      MinPrice,
      MaxPrice,
      Configurations);
    
    // Удаляем текущую комплектующую из конфигурации
    Dec(CurrentCount);
    
    Current := Current^.Next;
  end;
end;
```

### Рефакторированный код:

```pascal
procedure GenerateConfigurations(
  var CurrentComponents: array of TComponent;
  var CurrentCount: Integer;
  ComponentsByType: array of PComponentNode;
  TypeCount: Integer;
  CurrentTypeIndex: Integer;
  MinPrice, MaxPrice: Real;
  var Configurations: PConfigurationNode);
var
  Current: PComponentNode;
  TotalPrice: Real;
  i: Integer;
  NewConfiguration: PConfigurationNode;
  isCompatible: Boolean;
  inPriceRange: Boolean;
begin
  // Если мы обработали все типы комплектующих, проверяем конфигурацию
  if CurrentTypeIndex >= TypeCount then
  begin
    // Проверяем совместимость всех компонентов
    isCompatible := AreAllComponentsCompatible(CurrentComponents, CurrentCount);
    
    if isCompatible then
    begin
      // Вычисляем общую стоимость
      TotalPrice := 0;
      for i := 0 to CurrentCount - 1 do
        TotalPrice := TotalPrice + CurrentComponents[i].Price;
      
      // Проверяем, что стоимость в заданном диапазоне
      inPriceRange := (TotalPrice >= MinPrice) and (TotalPrice <= MaxPrice);
      
      if inPriceRange then
      begin
        // Создаем новую конфигурацию
        New(NewConfiguration);
        NewConfiguration^.Data.ComponentCount := CurrentCount;
        SetLength(NewConfiguration^.Data.Components, CurrentCount);
        
        for i := 0 to CurrentCount - 1 do
          NewConfiguration^.Data.Components[i] := CurrentComponents[i];
        
        NewConfiguration^.Data.TotalPrice := TotalPrice;
        
        // Добавляем конфигурацию в список
        NewConfiguration^.Next := Configurations;
        Configurations := NewConfiguration;
      end;
    end;
  end
  else
  begin
    // Перебираем все комплектующие текущего типа
    Current := ComponentsByType[CurrentTypeIndex];
    
    while Current <> nil do
    begin
      // Добавляем текущую комплектующую в конфигурацию
      CurrentComponents[CurrentCount] := Current^.Data;
      Inc(CurrentCount);
      
      // Рекурсивно генерируем конфигурации для следующего типа
      GenerateConfigurations(
        CurrentComponents,
        CurrentCount,
        ComponentsByType,
        TypeCount,
        CurrentTypeIndex + 1,
        MinPrice,
        MaxPrice,
        Configurations);
      
      // Удаляем текущую комплектующую из конфигурации
      Dec(CurrentCount);
      
      Current := Current^.Next;
    end;
  end;
end;
```

## Функция FindComputerConfigurations

### Исходный код:

```pascal
function FindComputerConfigurations(MinPrice, MaxPrice: Real; SelectedTypes: array of Integer): PConfigurationNode;
var
  ComponentsByType: array of PComponentNode;
  CurrentComponents: array of TComponent;
  CurrentCount: Integer;
  TypeCount: Integer;
  i: Integer;
  Configurations: PConfigurationNode;
  Current: PComponentNode;
begin
  TypeCount := Length(SelectedTypes);
  
  if TypeCount = 0 then
  begin
    FindComputerConfigurations := nil;
    Exit;
  end;
  
  // Инициализируем массив списков комплектующих по типам
  SetLength(ComponentsByType, TypeCount);
  
  for i := 0 to TypeCount - 1 do
    ComponentsByType[i] := FindComponentsByType(SelectedTypes[i]);
  
  // Инициализируем массив для текущей конфигурации
  SetLength(CurrentComponents, TypeCount);
  CurrentCount := 0;
  
  // Инициализируем список конфигураций
  Configurations := nil;
  
  // Генерируем все возможные конфигурации
  GenerateConfigurations(
    CurrentComponents,
    CurrentCount,
    ComponentsByType,
    TypeCount,
    0,
    MinPrice,
    MaxPrice,
    Configurations);
  
  // Освобождаем память, занятую временными списками
  for i := 0 to TypeCount - 1 do
  begin
    while ComponentsByType[i] <> nil do
    begin
      Current := ComponentsByType[i];
      ComponentsByType[i] := ComponentsByType[i]^.Next;
      Dispose(Current);
    end;
  end;
  
  FindComputerConfigurations := Configurations;
end;
```

### Рефакторированный код:

```pascal
function FindComputerConfigurations(MinPrice, MaxPrice: Real; SelectedTypes: array of Integer): PConfigurationNode;
var
  ComponentsByType: array of PComponentNode;
  CurrentComponents: array of TComponent;
  CurrentCount: Integer;
  TypeCount: Integer;
  i: Integer;
  Configurations: PConfigurationNode;
  Current: PComponentNode;
  hasSelectedTypes: Boolean;
begin
  TypeCount := Length(SelectedTypes);
  hasSelectedTypes := TypeCount > 0;
  Configurations := nil;
  
  if hasSelectedTypes then
  begin
    // Инициализируем массив списков комплектующих по типам
    SetLength(ComponentsByType, TypeCount);
    
    for i := 0 to TypeCount - 1 do
      ComponentsByType[i] := FindComponentsByType(SelectedTypes[i]);
    
    // Инициализируем массив для текущей конфигурации
    SetLength(CurrentComponents, TypeCount);
    CurrentCount := 0;
    
    // Генерируем все возможные конфигурации
    GenerateConfigurations(
      CurrentComponents,
      CurrentCount,
      ComponentsByType,
      TypeCount,
      0,
      MinPrice,
      MaxPrice,
      Configurations);
    
    // Освобождаем память, занятую временными списками
    for i := 0 to TypeCount - 1 do
    begin
      while ComponentsByType[i] <> nil do
      begin
        Current := ComponentsByType[i];
        ComponentsByType[i] := ComponentsByType[i]^.Next;
        Dispose(Current);
      end;
    end;
  end;
  
  FindComputerConfigurations := Configurations;
end;
```

## Заключение

Основной подход к рефакторингу файла specialFunctions.pas заключается в:

1. Замене операторов `Exit` в процедуре `GenerateConfigurations` на условные конструкции if-then-else
2. Реорганизации структуры процедуры `GenerateConfigurations` с использованием логических переменных для контроля выполнения условий
3. Замене раннего выхода из функции `FindComputerConfigurations` на условную конструкцию if-then-else
4. Введении логической переменной `hasSelectedTypes` для контроля выполнения основного блока кода

Этот подход сохраняет исходную функциональность кода, но делает его более структурированным и понятным, следуя принципам структурного программирования.

Рефакторинг рекурсивных функций, таких как `GenerateConfigurations`, требует особого внимания, чтобы сохранить логику работы и избежать ошибок. В данном случае мы заменили ранние выходы из функции на условные блоки, что сделало код более структурированным, но при этом сохранило его функциональность.
