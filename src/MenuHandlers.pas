unit MenuHandlers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  DataTypes,
  DynamicLists,
  FileOperations,
  UI,
  SpecialFunctions,
  InputUtils;

// Процедуры для обработки пунктов меню
procedure LoadAllData(var ComponentTypesList: TComponentTypeList;
                      var ComponentsList: TComponentList;
                      var CompatibilityList: TCompatibilityList;
                      var OrdersList: TOrderList);

procedure SaveAllData(const ComponentTypesList: TComponentTypeList;
                      const ComponentsList: TComponentList;
                      const CompatibilityList: TCompatibilityList;
                      const OrdersList: TOrderList);

procedure ViewList(const ComponentTypesList: TComponentTypeList;
                   const ComponentsList: TComponentList;
                   const CompatibilityList: TCompatibilityList;
                   const OrdersList: TOrderList;
                   const PCBuildOptionsList: TPCBuildOptionList);

procedure AddData(var ComponentTypesList: TComponentTypeList;
                  var ComponentsList: TComponentList;
                  var CompatibilityList: TCompatibilityList);

procedure RemoveData(var ComponentTypesList: TComponentTypeList;
                     var ComponentsList: TComponentList;
                     var CompatibilityList: TCompatibilityList);

procedure EditData(var ComponentTypesList: TComponentTypeList;
                   var ComponentsList: TComponentList;
                   var CompatibilityList: TCompatibilityList);

procedure HandleSpecialFunctions(const ComponentTypesList: TComponentTypeList;
                                 const ComponentsList: TComponentList;
                                 const CompatibilityList: TCompatibilityList;
                                 var PCBuildOptionsList: TPCBuildOptionList;
                                 var OrdersList: TOrderList);

implementation

// Процедуры для инициализации списков
procedure InitAllLists(var ComponentTypesList: TComponentTypeList;
                       var ComponentsList: TComponentList;
                       var CompatibilityList: TCompatibilityList;
                       var PCBuildOptionsList: TPCBuildOptionList;
                       var OrdersList: TOrderList);
begin
  InitComponentTypeList(ComponentTypesList);
  InitComponentList(ComponentsList);
  InitCompatibilityList(CompatibilityList);
  InitPCBuildOptionList(PCBuildOptionsList);
  InitOrderList(OrdersList);
end;

// Процедура для загрузки данных из файлов
procedure LoadAllData(var ComponentTypesList: TComponentTypeList;
                      var ComponentsList: TComponentList;
                      var CompatibilityList: TCompatibilityList;
                      var OrdersList: TOrderList);
var
  Confirmation: string;
  PCBuildOptionsList: TPCBuildOptionList;
begin
  // Запрос подтверждения у пользователя
  WriteLn('ВНИМАНИЕ: Вы собираетесь загрузить данные из файлов.');
  WriteLn('Это действие заменит все текущие данные в памяти.');
  Write('Вы уверены, что хотите продолжить? (да/нет): ');
  ReadLn(Confirmation);
  
  if (Confirmation <> 'да') and (Confirmation <> 'Да') and (Confirmation <> 'ДА') then
  begin
    WriteLn('Загрузка данных отменена.');
    PressEnterToContinue;
    Exit;
  end;
  
  // Инициализация списков
  InitAllLists(ComponentTypesList, ComponentsList, CompatibilityList, PCBuildOptionsList, OrdersList);
  
  // Загрузка данных из файлов
  if not LoadComponentTypes(ComponentTypesList) then
    WriteLn('Файл типов комплектующих не найден или поврежден. Создан пустой список.');
  
  if not LoadComponents(ComponentsList) then
    WriteLn('Файл комплектующих не найден или поврежден. Создан пустой список.');
  
  if not LoadCompatibility(CompatibilityList) then
    WriteLn('Файл совместимости не найден или поврежден. Создан пустой список.');
  
  if not LoadOrders(OrdersList) then
    WriteLn('Файл заказов не найден или поврежден. Создан пустой список.');
  
  WriteLn('Данные загружены.');
  PressEnterToContinue;
end;

// Процедура для сохранения данных в файлы
procedure SaveAllData(const ComponentTypesList: TComponentTypeList;
                      const ComponentsList: TComponentList;
                      const CompatibilityList: TCompatibilityList;
                      const OrdersList: TOrderList);
begin
  if not SaveComponentTypes(ComponentTypesList) then
    WriteLn('Ошибка при сохранении типов комплектующих.');
  
  if not SaveComponents(ComponentsList) then
    WriteLn('Ошибка при сохранении комплектующих.');
  
  if not SaveCompatibility(CompatibilityList) then
    WriteLn('Ошибка при сохранении данных о совместимости.');
  
  if not SaveOrders(OrdersList) then
    WriteLn('Ошибка при сохранении заказов.');
  
  WriteLn('Данные сохранены.');
end;

// Процедура для обработки пункта меню "Просмотр всего списка"
procedure ViewList(const ComponentTypesList: TComponentTypeList;
                   const ComponentsList: TComponentList;
                   const CompatibilityList: TCompatibilityList;
                   const OrdersList: TOrderList;
                   const PCBuildOptionsList: TPCBuildOptionList);
var
  SubmenuChoice: Integer;
begin
  DisplayListSubmenu;
  SubmenuChoice := GetListSubmenuChoice;
  
  case SubmenuChoice of
    SUBMENU_COMPONENT_TYPES:
      DisplayComponentTypes(ComponentTypesList);
    
    SUBMENU_COMPONENTS:
      DisplayComponents(ComponentsList, ComponentTypesList);
    
    SUBMENU_COMPATIBILITY:
      DisplayCompatibility(CompatibilityList, ComponentsList);
    
    SUBMENU_ORDERS:
      DisplayOrders(OrdersList, PCBuildOptionsList);
  end;
end;

// Процедура для обработки пункта меню "Добавление данных в список"
procedure AddData(var ComponentTypesList: TComponentTypeList;
                  var ComponentsList: TComponentList;
                  var CompatibilityList: TCompatibilityList);
var
  SubmenuChoice: Integer;
  NewComponentType: TComponentType;
  NewComponent: TComponent;
  NewCompatibility: TCompatibility;
  Confirmation: string;
begin
  DisplayAddDataSubmenu;
  SubmenuChoice := GetAddDataSubmenuChoice;
  
  // Проверка на отмену операции
  if SubmenuChoice < SUBMENU_COMPONENT_TYPES then
  begin
    WriteLn('Операция добавления отменена.');
    PressEnterToContinue;
    Exit;
  end;
  
  WriteLn('Для прерывания операции в любой момент введите "/cancel" или "/menu"');
  WriteLn;
  
  case SubmenuChoice of
    SUBMENU_COMPONENT_TYPES:
      begin
        WriteLn('=== ДОБАВЛЕНИЕ ТИПА КОМПЛЕКТУЮЩЕЙ ===');
        if InputComponentType(NewComponentType, ComponentTypesList) then
        begin
          if AddComponentType(ComponentTypesList, NewComponentType) then
            WriteLn('Тип комплектующей успешно добавлен.')
          else
            WriteLn('Ошибка: Тип с таким кодом уже существует.');
        end
        else
          WriteLn('Добавление типа комплектующей отменено.');
      end;
    
    SUBMENU_COMPONENTS:
      begin
        WriteLn('=== ДОБАВЛЕНИЕ КОМПЛЕКТУЮЩЕЙ ===');
        if ComponentTypesList.Count = 0 then
        begin
          WriteLn('Ошибка: Сначала необходимо добавить типы комплектующих.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные типы комплектующих:');
        DisplayComponentTypes(ComponentTypesList);
        
        if InputComponent(NewComponent, ComponentTypesList, ComponentsList) then
        begin
          if AddComponent(ComponentsList, NewComponent) then
            WriteLn('Комплектующая успешно добавлена.')
          else
            WriteLn('Ошибка: Комплектующая с таким кодом уже существует.');
        end
        else
          WriteLn('Добавление комплектующей отменено.');
      end;
    
    SUBMENU_COMPATIBILITY:
      begin
        WriteLn('=== ДОБАВЛЕНИЕ ЗАПИСИ О СОВМЕСТИМОСТИ ===');
        if ComponentsList.Count < 2 then
        begin
          WriteLn('Ошибка: Для создания записи о совместимости необходимо минимум 2 комплектующих.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные комплектующие:');
        DisplayComponents(ComponentsList, ComponentTypesList);
        
        if InputCompatibility(NewCompatibility, ComponentsList) then
        begin
          if NewCompatibility.ComponentCode1 = NewCompatibility.ComponentCode2 then
          begin
            WriteLn('Ошибка: Нельзя создать запись о совместимости комплектующей с самой собой.');
          end
          else if AddCompatibility(CompatibilityList, NewCompatibility) then
            WriteLn('Запись о совместимости успешно добавлена.')
          else
            WriteLn('Ошибка: Такая запись о совместимости уже существует.');
        end
        else
          WriteLn('Добавление записи о совместимости отменено.');
      end;
  end;
  
  PressEnterToContinue;
end;

// Процедура для обработки пункта меню "Удаление данных из списка"
procedure RemoveData(var ComponentTypesList: TComponentTypeList;
                     var ComponentsList: TComponentList;
                     var CompatibilityList: TCompatibilityList);
var
  SubmenuChoice: Integer;
  Code, Code1, Code2: Integer;
  ComponentTypeNode: PComponentTypeNode;
  ComponentNode: PComponentNode;
  HasDependentComponents, HasDependentCompatibility: Boolean;
  Current: PComponentNode;
  CompatCurrent: PCompatibilityNode;
  Confirmation: string;
begin
  DisplayListSubmenu;
  SubmenuChoice := GetListSubmenuChoice;
  
  // Проверка на отмену операции
  if SubmenuChoice < SUBMENU_COMPONENT_TYPES then
  begin
    WriteLn('Операция удаления отменена.');
    PressEnterToContinue;
    Exit;
  end;
  
  WriteLn('Для прерывания операции в любой момент введите "/cancel" или "/menu"');
  WriteLn;
  
  case SubmenuChoice of
    SUBMENU_COMPONENT_TYPES:
      begin
        WriteLn('=== УДАЛЕНИЕ ТИПА КОМПЛЕКТУЮЩЕЙ ===');
        if ComponentTypesList.Count = 0 then
        begin
          WriteLn('Список типов комплектующих пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные типы комплектующих:');
        DisplayComponentTypes(ComponentTypesList);
        
        Code := SafeReadInteger('Введите код типа комплектующей для удаления: ', 1);
        if Code < 1 then
        begin
          WriteLn('Удаление типа комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        ComponentTypeNode := FindComponentType(ComponentTypesList, Code);
        if ComponentTypeNode = nil then
        begin
          WriteLn('Ошибка: Тип с таким кодом не найден.');
          PressEnterToContinue;
          Exit;
        end;
        
        // Проверяем, есть ли комплектующие данного типа
        HasDependentComponents := False;
        Current := ComponentsList.Head;
        while Current <> nil do
        begin
          if Current^.Data.TypeCode = Code then
          begin
            HasDependentComponents := True;
            Break;
          end;
          Current := Current^.Next;
        end;
        
        if HasDependentComponents then
        begin
          WriteLn('ВНИМАНИЕ: Существуют комплектующие данного типа.');
          WriteLn('Удаление типа приведет к проблемам с данными.');
          Confirmation := SafeReadString('Вы действительно хотите удалить этот тип? (да/нет): ', 1, 10);
          
          if IsReturnToMenuCommand(Confirmation) or IsCancelCommand(Confirmation) then
          begin
            WriteLn('Удаление типа комплектующей отменено.');
            PressEnterToContinue;
            Exit;
          end;
          
          if not ((Confirmation = 'да') or (Confirmation = 'Да') or (Confirmation = 'ДА')) then
          begin
            WriteLn('Удаление типа комплектующей отменено.');
            PressEnterToContinue;
            Exit;
          end;
        end;
        
        if RemoveComponentType(ComponentTypesList, Code) then
          WriteLn('Тип комплектующей успешно удален.')
        else
          WriteLn('Ошибка при удалении типа комплектующей.');
      end;
    
    SUBMENU_COMPONENTS:
      begin
        WriteLn('=== УДАЛЕНИЕ КОМПЛЕКТУЮЩЕЙ ===');
        if ComponentsList.Count = 0 then
        begin
          WriteLn('Список комплектующих пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные комплектующие:');
        DisplayComponents(ComponentsList, ComponentTypesList);
        
        Code := SafeReadInteger('Введите код комплектующей для удаления: ', 1);
        if Code < 1 then
        begin
          WriteLn('Удаление комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        ComponentNode := FindComponent(ComponentsList, Code);
        if ComponentNode = nil then
        begin
          WriteLn('Ошибка: Комплектующая с таким кодом не найдена.');
          PressEnterToContinue;
          Exit;
        end;
        
        // Проверяем, есть ли записи о совместимости с данной комплектующей
        HasDependentCompatibility := False;
        CompatCurrent := CompatibilityList.Head;
        while CompatCurrent <> nil do
        begin
          if (CompatCurrent^.Data.ComponentCode1 = Code) or 
             (CompatCurrent^.Data.ComponentCode2 = Code) then
          begin
            HasDependentCompatibility := True;
            Break;
          end;
          CompatCurrent := CompatCurrent^.Next;
        end;
        
        if HasDependentCompatibility then
        begin
          WriteLn('ВНИМАНИЕ: Существуют записи о совместимости с данной комплектующей.');
          WriteLn('Удаление комплектующей приведет к потере этих записей.');
          Confirmation := SafeReadString('Вы действительно хотите удалить эту комплектующую? (да/нет): ', 1, 10);
          
          if IsReturnToMenuCommand(Confirmation) or IsCancelCommand(Confirmation) then
          begin
            WriteLn('Удаление комплектующей отменено.');
            PressEnterToContinue;
            Exit;
          end;
          
          if not ((Confirmation = 'да') or (Confirmation = 'Да') or (Confirmation = 'ДА')) then
          begin
            WriteLn('Удаление комплектующей отменено.');
            PressEnterToContinue;
            Exit;
          end;
          
          // Удаляем связанные записи о совместимости
          CompatCurrent := CompatibilityList.Head;
          while CompatCurrent <> nil do
          begin
            if (CompatCurrent^.Data.ComponentCode1 = Code) or 
               (CompatCurrent^.Data.ComponentCode2 = Code) then
            begin
              RemoveCompatibility(CompatibilityList, 
                                CompatCurrent^.Data.ComponentCode1, 
                                CompatCurrent^.Data.ComponentCode2);
              // Начинаем поиск сначала, так как список изменился
              CompatCurrent := CompatibilityList.Head;
            end
            else
              CompatCurrent := CompatCurrent^.Next;
          end;
          WriteLn('Связанные записи о совместимости удалены.');
        end;
        
        if RemoveComponent(ComponentsList, Code) then
          WriteLn('Комплектующая успешно удалена.')
        else
          WriteLn('Ошибка при удалении комплектующей.');
      end;
    
    SUBMENU_COMPATIBILITY:
      begin
        WriteLn('=== УДАЛЕНИЕ ЗАПИСИ О СОВМЕСТИМОСТИ ===');
        if CompatibilityList.Count = 0 then
        begin
          WriteLn('Список записей о совместимости пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные записи о совместимости:');
        DisplayCompatibility(CompatibilityList, ComponentsList);
        
        Code1 := SafeReadInteger('Введите код первой комплектующей: ', 1);
        if Code1 < 1 then
        begin
          WriteLn('Удаление записи о совместимости отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        Code2 := SafeReadInteger('Введите код второй комплектующей: ', 1);
        if Code2 < 1 then
        begin
          WriteLn('Удаление записи о совместимости отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        if FindCompatibility(CompatibilityList, Code1, Code2) = nil then
        begin
          WriteLn('Ошибка: Запись о совместимости не найдена.');
          PressEnterToContinue;
          Exit;
        end;
        
        if RemoveCompatibility(CompatibilityList, Code1, Code2) then
          WriteLn('Запись о совместимости успешно удалена.')
        else
          WriteLn('Ошибка при удалении записи о совместимости.');
      end;
  end;
  
  PressEnterToContinue;
end;

// Процедура для обработки пункта меню "Редактирование данных"
procedure EditData(var ComponentTypesList: TComponentTypeList;
                   var ComponentsList: TComponentList;
                   var CompatibilityList: TCompatibilityList);
var
  SubmenuChoice: Integer;
  Code, i, TypeCode: Integer;
  ComponentTypeNode: PComponentTypeNode;
  ComponentNode: PComponentNode;
  CompatibilityNode: PCompatibilityNode;
  TempString: string;
  TempPrice: Real;
  TempStock: Integer;
  ComponentCode1, ComponentCode2: Integer;
  Component1, Component2: PComponentNode;
begin
  DisplayListSubmenu;
  SubmenuChoice := GetListSubmenuChoice;
  
  // Проверка на отмену операции
  if SubmenuChoice < SUBMENU_COMPONENT_TYPES then
  begin
    WriteLn('Операция редактирования отменена.');
    PressEnterToContinue;
    Exit;
  end;
  
  WriteLn('Для прерывания операции в любой момент введите "/cancel" или "/menu"');
  WriteLn;
  
  case SubmenuChoice of
    SUBMENU_COMPONENT_TYPES:
      begin
        WriteLn('=== РЕДАКТИРОВАНИЕ ТИПА КОМПЛЕКТУЮЩЕЙ ===');
        if ComponentTypesList.Count = 0 then
        begin
          WriteLn('Список типов комплектующих пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные типы комплектующих:');
        DisplayComponentTypes(ComponentTypesList);
        
        Code := SafeReadInteger('Введите код типа комплектующей для редактирования: ', 1);
        if Code < 1 then
        begin
          WriteLn('Редактирование типа комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        ComponentTypeNode := FindComponentType(ComponentTypesList, Code);
        if ComponentTypeNode = nil then
        begin
          WriteLn('Ошибка: Тип с таким кодом не найден.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Текущие данные:');
        WriteLn('Код типа: ', ComponentTypeNode^.Data.TypeCode);
        WriteLn('Название: ', ComponentTypeNode^.Data.Name);
        WriteLn;
        
        WriteLn('Введите новые данные:');
        TempString := SafeReadString('Название типа комплектующей: ', 1, MAX_STRING_LENGTH);
        
        if IsReturnToMenuCommand(TempString) or IsCancelCommand(TempString) then
        begin
          WriteLn('Редактирование типа комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        ComponentTypeNode^.Data.Name := StringToFixed(TempString);
        WriteLn('Тип комплектующей успешно отредактирован.');
      end;
    
    SUBMENU_COMPONENTS:
      begin
        WriteLn('=== РЕДАКТИРОВАНИЕ КОМПЛЕКТУЮЩЕЙ ===');
        if ComponentsList.Count = 0 then
        begin
          WriteLn('Список комплектующих пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные комплектующие:');
        DisplayComponents(ComponentsList, ComponentTypesList);
        
        Code := SafeReadInteger('Введите код комплектующей для редактирования: ', 1);
        if Code < 1 then
        begin
          WriteLn('Редактирование комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        ComponentNode := FindComponent(ComponentsList, Code);
        if ComponentNode = nil then
        begin
          WriteLn('Ошибка: Комплектующая с таким кодом не найдена.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Текущие данные:');
        WriteLn('Код: ', ComponentNode^.Data.Code);
        WriteLn('Код типа: ', ComponentNode^.Data.TypeCode);
        WriteLn('Производитель: ', ComponentNode^.Data.Manufacturer);
        WriteLn('Модель: ', ComponentNode^.Data.Model);
        WriteLn('Параметры: ', ComponentNode^.Data.Parameters);
        WriteLn('Цена: ', ComponentNode^.Data.Price:0:2);
        WriteLn('В наличии: ', ComponentNode^.Data.InStock);
        WriteLn;
        
        WriteLn('Введите новые данные:');
        WriteLn('Доступные типы комплектующих:');
        DisplayComponentTypes(ComponentTypesList);
        
        repeat
          TypeCode := SafeReadInteger('Код типа комплектующей: ', 1);
          if TypeCode < 1 then
          begin
            WriteLn('Редактирование комплектующей отменено.');
            PressEnterToContinue;
            Exit;
          end;
          
          if FindComponentType(ComponentTypesList, TypeCode) = nil then
            WriteLn('Ошибка: Тип с кодом ', TypeCode, ' не найден. Пожалуйста, введите существующий код типа.');
        until FindComponentType(ComponentTypesList, TypeCode) <> nil;
        
        ComponentNode^.Data.TypeCode := TypeCode;
        
        TempString := SafeReadString('Производитель: ', 1, MAX_STRING_LENGTH);
        if IsReturnToMenuCommand(TempString) or IsCancelCommand(TempString) then
        begin
          WriteLn('Редактирование комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        ComponentNode^.Data.Manufacturer := StringToFixed(TempString);
        
        TempString := SafeReadString('Модель: ', 1, MAX_STRING_LENGTH);
        if IsReturnToMenuCommand(TempString) or IsCancelCommand(TempString) then
        begin
          WriteLn('Редактирование комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        ComponentNode^.Data.Model := StringToFixed(TempString);
        
        TempString := SafeReadString('Параметры: ', 0, MAX_STRING_LENGTH);
        if IsReturnToMenuCommand(TempString) or IsCancelCommand(TempString) then
        begin
          WriteLn('Редактирование комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        ComponentNode^.Data.Parameters := StringToFixed(TempString);
        
        TempPrice := SafeReadFloat('Цена: ', 0);
        if TempPrice < 0 then
        begin
          WriteLn('Редактирование комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        ComponentNode^.Data.Price := TempPrice;
        
        TempStock := SafeReadInteger('Количество в наличии: ', 0);
        if TempStock < 0 then
        begin
          WriteLn('Редактирование комплектующей отменено.');
          PressEnterToContinue;
          Exit;
        end;
        ComponentNode^.Data.InStock := TempStock;
        
        WriteLn('Комплектующая успешно отредактирована.');
      end;
    
    SUBMENU_COMPATIBILITY:
      begin
        WriteLn('=== РЕДАКТИРОВАНИЕ ЗАПИСИ О СОВМЕСТИМОСТИ ===');
        if CompatibilityList.Count = 0 then
        begin
          WriteLn('Список записей о совместимости пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные записи о совместимости:');
        DisplayCompatibility(CompatibilityList, ComponentsList);
        
        Code := SafeReadInteger('Введите код первой комплектующей: ', 1);
        if Code < 1 then
        begin
          WriteLn('Редактирование записи о совместимости отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        i := SafeReadInteger('Введите код второй комплектующей: ', 1);
        if i < 1 then
        begin
          WriteLn('Редактирование записи о совместимости отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        CompatibilityNode := FindCompatibility(CompatibilityList, Code, i);
        if CompatibilityNode = nil then
        begin
          WriteLn('Ошибка: Запись о совместимости не найдена.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Текущие данные:');
        WriteLn('Код первой комплектующей: ', CompatibilityNode^.Data.ComponentCode1);
        WriteLn('Код второй комплектующей: ', CompatibilityNode^.Data.ComponentCode2);
        WriteLn;
        
        WriteLn('Введите новые данные:');
        WriteLn('Доступные комплектующие:');
        DisplayComponents(ComponentsList, ComponentTypesList);
        
        repeat
          ComponentCode1 := SafeReadInteger('Код первой комплектующей: ', 1);
          if ComponentCode1 < 1 then
          begin
            WriteLn('Редактирование записи о совместимости отменено.');
            PressEnterToContinue;
            Exit;
          end;
          
          Component1 := FindComponent(ComponentsList, ComponentCode1);
          if Component1 = nil then
            WriteLn('Ошибка: Комплектующая с кодом ', ComponentCode1, ' не найдена. Пожалуйста, введите существующий код.');
        until Component1 <> nil;
        
        repeat
          ComponentCode2 := SafeReadInteger('Код второй комплектующей: ', 1);
          if ComponentCode2 < 1 then
          begin
            WriteLn('Редактирование записи о совместимости отменено.');
            PressEnterToContinue;
            Exit;
          end;
          
          Component2 := FindComponent(ComponentsList, ComponentCode2);
          if Component2 = nil then
            WriteLn('Ошибка: Комплектующая с кодом ', ComponentCode2, ' не найдена. Пожалуйста, введите существующий код.');
        until Component2 <> nil;
        
        if ComponentCode1 = ComponentCode2 then
        begin
          WriteLn('Ошибка: Нельзя создать запись о совместимости комплектующей с самой собой.');
          PressEnterToContinue;
          Exit;
        end;
        
        CompatibilityNode^.Data.ComponentCode1 := ComponentCode1;
        CompatibilityNode^.Data.ComponentCode2 := ComponentCode2;
        
        WriteLn('Запись о совместимости успешно отредактирована.');
      end;
  end;
  
  PressEnterToContinue;
end;

// Процедура для обработки пункта меню "Специальные функции"
procedure HandleSpecialFunctions(const ComponentTypesList: TComponentTypeList;
                                 const ComponentsList: TComponentList;
                                 const CompatibilityList: TCompatibilityList;
                                 var PCBuildOptionsList: TPCBuildOptionList;
                                 var OrdersList: TOrderList);
var
  SubmenuChoice: Integer;
  MinPrice, MaxPrice: Real;
  CompatibleComponentsList: TComponentList;
  ComponentCode, TypeCode: Integer;
  i: Integer;
  BuildOptionNode: PPCBuildOptionNode;
begin
  DisplaySpecialFunctionsSubmenu;
  SubmenuChoice := GetSpecialFunctionsSubmenuChoice;
  
  // Проверка на отмену операции
  if SubmenuChoice < SUBMENU_SF_BUILD_PC then
  begin
    WriteLn('Операция отменена.');
    PressEnterToContinue;
    Exit;
  end;
  
  WriteLn('Для прерывания операции в любой момент введите "/cancel" или "/menu"');
  WriteLn;
  
  case SubmenuChoice of
    SUBMENU_SF_BUILD_PC:
      begin
        WriteLn('=== ПОДБОР ВАРИАНТОВ КОМПЛЕКТАЦИИ ПК ===');
        
        // Проверяем, есть ли необходимые данные
        if ComponentsList.Count = 0 then
        begin
          WriteLn('Ошибка: Список комплектующих пуст. Сначала добавьте комплектующие.');
          PressEnterToContinue;
          Exit;
        end;
        
        if CompatibilityList.Count = 0 then
        begin
          WriteLn('Ошибка: Список совместимости пуст. Сначала добавьте записи о совместимости.');
          PressEnterToContinue;
          Exit;
        end;
        
        // Подбор вариантов комплектации ПК в заданном ценовом диапазоне
        MinPrice := SafeReadFloat('Введите минимальную цену: ', 0);
        if MinPrice < 0 then
        begin
          WriteLn('Подбор вариантов комплектации отменен.');
          PressEnterToContinue;
          Exit;
        end;
        
        MaxPrice := SafeReadFloat('Введите максимальную цену: ', MinPrice);
        if MaxPrice < 0 then
        begin
          WriteLn('Подбор вариантов комплектации отменен.');
          PressEnterToContinue;
          Exit;
        end;
        
        if MaxPrice < MinPrice then
        begin
          WriteLn('Ошибка: Максимальная цена не может быть меньше минимальной.');
          PressEnterToContinue;
          Exit;
        end;
        
        if FindPCBuildOptions(ComponentsList, CompatibilityList, ComponentTypesList,
                            MinPrice, MaxPrice, PCBuildOptionsList) then
        begin
          WriteLn('Найдено ', PCBuildOptionsList.Count, ' вариантов комплектации ПК.');
          DisplayPCBuildOptions(PCBuildOptionsList, ComponentsList);
          
          // Сохраняем результаты в текстовый файл
          if SavePCBuildOptionsToTextFile(PCBuildOptionsList, ComponentsList, 'data/pc_build_options.txt') then
            WriteLn('Результаты сохранены в файл data/pc_build_options.txt')
          else
            WriteLn('Ошибка при сохранении результатов в файл.');
        end
        else
          WriteLn('Не найдено вариантов комплектации ПК в заданном ценовом диапазоне.');
      end;
    
    SUBMENU_SF_CREATE_ORDER:
      begin
        WriteLn('=== ОФОРМЛЕНИЕ ЗАКАЗА ===');
        
        // Оформление заказа
        if PCBuildOptionsList.Count = 0 then
        begin
          WriteLn('Сначала необходимо подобрать варианты комплектации ПК (пункт 1).');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные варианты комплектации:');
        DisplayPCBuildOptions(PCBuildOptionsList, ComponentsList);
        
        i := SafeReadInteger('Введите ID варианта комплектации для заказа: ', 1);
        if i < 1 then
        begin
          WriteLn('Оформление заказа отменено.');
          PressEnterToContinue;
          Exit;
        end;
        
        BuildOptionNode := FindPCBuildOption(PCBuildOptionsList, i);
        if BuildOptionNode = nil then
        begin
          WriteLn('Ошибка: Вариант комплектации с ID ', i, ' не найден.');
          PressEnterToContinue;
          Exit;
        end;
        
        if CreateOrder(BuildOptionNode^.Data, ComponentsList, OrdersList) then
          WriteLn('Заказ успешно оформлен.')
        else
          WriteLn('Ошибка при оформлении заказа.');
      end;
    
    SUBMENU_SF_COMPATIBLE_COMPONENTS:
      begin
        WriteLn('=== ПОИСК СОВМЕСТИМЫХ КОМПЛЕКТУЮЩИХ ===');
        
        // Проверяем, есть ли необходимые данные
        if ComponentsList.Count = 0 then
        begin
          WriteLn('Ошибка: Список комплектующих пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        if CompatibilityList.Count = 0 then
        begin
          WriteLn('Ошибка: Список совместимости пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        if ComponentTypesList.Count = 0 then
        begin
          WriteLn('Ошибка: Список типов комплектующих пуст.');
          PressEnterToContinue;
          Exit;
        end;
        
        // Поиск совместимых комплектующих заданного типа
        WriteLn('Доступные комплектующие:');
        DisplayComponents(ComponentsList, ComponentTypesList);
        
        ComponentCode := SafeReadInteger('Введите код комплектующей: ', 1);
        if ComponentCode < 1 then
        begin
          WriteLn('Поиск совместимых комплектующих отменен.');
          PressEnterToContinue;
          Exit;
        end;
        
        if FindComponent(ComponentsList, ComponentCode) = nil then
        begin
          WriteLn('Ошибка: Комплектующая с кодом ', ComponentCode, ' не найдена.');
          PressEnterToContinue;
          Exit;
        end;
        
        WriteLn('Доступные типы комплектующих:');
        DisplayComponentTypes(ComponentTypesList);
        
        TypeCode := SafeReadInteger('Введите код типа комплектующих для поиска совместимых: ', 1);
        if TypeCode < 1 then
        begin
          WriteLn('Поиск совместимых комплектующих отменен.');
          PressEnterToContinue;
          Exit;
        end;
        
        if FindComponentType(ComponentTypesList, TypeCode) = nil then
        begin
          WriteLn('Ошибка: Тип комплектующих с кодом ', TypeCode, ' не найден.');
          PressEnterToContinue;
          Exit;
        end;
        
        if FindCompatibleComponents(ComponentsList, CompatibilityList, ComponentTypesList,
                                  ComponentCode, TypeCode, CompatibleComponentsList) then
        begin
          WriteLn('Найдено ', CompatibleComponentsList.Count, ' совместимых комплектующих:');
          DisplayComponents(CompatibleComponentsList, ComponentTypesList);
        end
        else
          WriteLn('Не найдено совместимых комплектующих заданного типа.');
      end;
  end;
  
  PressEnterToContinue;
end;

end. 
