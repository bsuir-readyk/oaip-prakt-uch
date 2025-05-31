program solve;

{$mode objfpc}{$H+}

uses
  SysUtils,
  DataTypes,
  DynamicLists,
  FileOperations,
  UI,
  SpecialFunctions;

var
  // Списки данных
  ComponentTypesList: TComponentTypeList;
  ComponentsList: TComponentList;
  CompatibilityList: TCompatibilityList;
  PCBuildOptionsList: TPCBuildOptionList;
  OrdersList: TOrderList;
  
  // Переменные для работы с меню
  MenuChoice, SubmenuChoice: Integer;
  ExitProgram: Boolean;
  
  // Переменные для специальных функций
  MinPrice, MaxPrice: Real;
  CompatibleComponentsList: TComponentList;
  ComponentCode, TypeCode: Integer;
  
  // Переменные для ввода данных
  NewComponentType: TComponentType;
  NewComponent: TComponent;
  NewCompatibility: TCompatibility;
  
  // Вспомогательные переменные
  i: Integer;
  BuildOptionNode: PPCBuildOptionNode;

// Процедура для инициализации списков
procedure InitAllLists;
begin
  InitComponentTypeList(ComponentTypesList);
  InitComponentList(ComponentsList);
  InitCompatibilityList(CompatibilityList);
  InitPCBuildOptionList(PCBuildOptionsList);
  InitOrderList(OrdersList);
end;

// Процедура для загрузки данных из файлов
procedure LoadAllData;
var
  Confirmation: string;
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
  InitAllLists;
  
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
procedure SaveAllData;
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
procedure ViewList;
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
procedure AddData;
begin
  DisplayListSubmenu;
  SubmenuChoice := GetListSubmenuChoice;
  
  case SubmenuChoice of
    SUBMENU_COMPONENT_TYPES:
      begin
        if InputComponentType(NewComponentType, ComponentTypesList) then
        begin
          if AddComponentType(ComponentTypesList, NewComponentType) then
            WriteLn('Тип комплектующей успешно добавлен.')
          else
            WriteLn('Ошибка: Тип с таким кодом уже существует.');
        end;
      end;
    
    SUBMENU_COMPONENTS:
      begin
        if InputComponent(NewComponent, ComponentTypesList, ComponentsList) then
        begin
          if AddComponent(ComponentsList, NewComponent) then
            WriteLn('Комплектующая успешно добавлена.')
          else
            WriteLn('Ошибка: Комплектующая с таким кодом уже существует.');
        end;
      end;
    
    SUBMENU_COMPATIBILITY:
      begin
        if InputCompatibility(NewCompatibility, ComponentsList) then
        begin
          if AddCompatibility(CompatibilityList, NewCompatibility) then
            WriteLn('Запись о совместимости успешно добавлена.')
          else
            WriteLn('Ошибка: Такая запись о совместимости уже существует.');
        end;
      end;
  end;
  
  PressEnterToContinue;
end;

// Процедура для обработки пункта меню "Удаление данных из списка"
procedure RemoveData;
var
  Code, Code1, Code2: Integer;
begin
  DisplayListSubmenu;
  SubmenuChoice := GetListSubmenuChoice;
  
  case SubmenuChoice of
    SUBMENU_COMPONENT_TYPES:
      begin
        Write('Введите код типа комплектующей для удаления: ');
        ReadLn(Code);
        
        if RemoveComponentType(ComponentTypesList, Code) then
          WriteLn('Тип комплектующей успешно удален.')
        else
          WriteLn('Ошибка: Тип с таким кодом не найден.');
      end;
    
    SUBMENU_COMPONENTS:
      begin
        Write('Введите код комплектующей для удаления: ');
        ReadLn(Code);
        
        if RemoveComponent(ComponentsList, Code) then
          WriteLn('Комплектующая успешно удалена.')
        else
          WriteLn('Ошибка: Комплектующая с таким кодом не найдена.');
      end;
    
    SUBMENU_COMPATIBILITY:
      begin
        Write('Введите код первой комплектующей: ');
        ReadLn(Code1);
        
        Write('Введите код второй комплектующей: ');
        ReadLn(Code2);
        
        if RemoveCompatibility(CompatibilityList, Code1, Code2) then
          WriteLn('Запись о совместимости успешно удалена.')
        else
          WriteLn('Ошибка: Запись о совместимости не найдена.');
      end;
  end;
  
  PressEnterToContinue;
end;

// Процедура для обработки пункта меню "Редактирование данных"
procedure EditData;
var
  Code: Integer;
  ComponentTypeNode: PComponentTypeNode;
  ComponentNode: PComponentNode;
  CompatibilityNode: PCompatibilityNode;
begin
  DisplayListSubmenu;
  SubmenuChoice := GetListSubmenuChoice;
  
  case SubmenuChoice of
    SUBMENU_COMPONENT_TYPES:
      begin
        Write('Введите код типа комплектующей для редактирования: ');
        ReadLn(Code);
        
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
        Write('Название типа комплектующей: ');
        ReadLn(ComponentTypeNode^.Data.Name);
        
        WriteLn('Тип комплектующей успешно отредактирован.');
      end;
    
    SUBMENU_COMPONENTS:
      begin
        Write('Введите код комплектующей для редактирования: ');
        ReadLn(Code);
        
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
        
        repeat
          Write('Код типа комплектующей: ');
          ReadLn(ComponentNode^.Data.TypeCode);
          
          if FindComponentType(ComponentTypesList, ComponentNode^.Data.TypeCode) = nil then
            WriteLn('Ошибка: Тип с кодом ', ComponentNode^.Data.TypeCode, ' не найден. Пожалуйста, введите существующий код типа.');
        until FindComponentType(ComponentTypesList, ComponentNode^.Data.TypeCode) <> nil;
        
        Write('Производитель: ');
        ReadLn(ComponentNode^.Data.Manufacturer);
        
        Write('Модель: ');
        ReadLn(ComponentNode^.Data.Model);
        
        Write('Параметры: ');
        ReadLn(ComponentNode^.Data.Parameters);
        
        Write('Цена: ');
        ReadLn(ComponentNode^.Data.Price);
        
        Write('Количество в наличии: ');
        ReadLn(ComponentNode^.Data.InStock);
        
        WriteLn('Комплектующая успешно отредактирована.');
      end;
    
    SUBMENU_COMPATIBILITY:
      begin
        Write('Введите код первой комплектующей: ');
        ReadLn(Code);
        
        Write('Введите код второй комплектующей: ');
        ReadLn(i);
        
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
        
        repeat
          Write('Код первой комплектующей: ');
          ReadLn(CompatibilityNode^.Data.ComponentCode1);
          
          if FindComponent(ComponentsList, CompatibilityNode^.Data.ComponentCode1) = nil then
            WriteLn('Ошибка: Комплектующая с кодом ', CompatibilityNode^.Data.ComponentCode1, ' не найдена. Пожалуйста, введите существующий код.');
        until FindComponent(ComponentsList, CompatibilityNode^.Data.ComponentCode1) <> nil;
        
        repeat
          Write('Код второй комплектующей: ');
          ReadLn(CompatibilityNode^.Data.ComponentCode2);
          
          if FindComponent(ComponentsList, CompatibilityNode^.Data.ComponentCode2) = nil then
            WriteLn('Ошибка: Комплектующая с кодом ', CompatibilityNode^.Data.ComponentCode2, ' не найдена. Пожалуйста, введите существующий код.');
        until FindComponent(ComponentsList, CompatibilityNode^.Data.ComponentCode2) <> nil;
        
        WriteLn('Запись о совместимости успешно отредактирована.');
      end;
  end;
  
  PressEnterToContinue;
end;

// Процедура для обработки пункта меню "Специальные функции"
procedure SpecialFunctions;
begin
  DisplaySpecialFunctionsSubmenu;
  SubmenuChoice := GetSpecialFunctionsSubmenuChoice;
  
  case SubmenuChoice of
    SUBMENU_SF_BUILD_PC:
      begin
        // Подбор вариантов комплектации ПК в заданном ценовом диапазоне
        Write('Введите минимальную цену: ');
        ReadLn(MinPrice);
        
        Write('Введите максимальную цену: ');
        ReadLn(MaxPrice);
        
        if FindPCBuildOptions(ComponentsList, CompatibilityList, ComponentTypesList,
                            MinPrice, MaxPrice, PCBuildOptionsList) then
        begin
          WriteLn('Найдено ', PCBuildOptionsList.Count, ' вариантов комплектации ПК.');
          DisplayPCBuildOptions(PCBuildOptionsList, ComponentsList);
          
          // Сохраняем результаты в текстовый файл
          if SavePCBuildOptionsToTextFile(PCBuildOptionsList, ComponentsList, 'pc_build_options.txt') then
            WriteLn('Результаты сохранены в файл pc_build_options.txt')
          else
            WriteLn('Ошибка при сохранении результатов в файл.');
        end
        else
          WriteLn('Не найдено вариантов комплектации ПК в заданном ценовом диапазоне.');
      end;
    
    SUBMENU_SF_CREATE_ORDER:
      begin
        // Оформление заказа
        if PCBuildOptionsList.Count = 0 then
        begin
          WriteLn('Сначала необходимо подобрать варианты комплектации ПК (пункт 1).');
          PressEnterToContinue;
          Exit;
        end;
        
        Write('Введите ID варианта комплектации для заказа: ');
        ReadLn(i);
        
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
        // Поиск совместимых комплектующих заданного типа
        Write('Введите код комплектующей: ');
        ReadLn(ComponentCode);
        
        Write('Введите код типа комплектующих для поиска совместимых: ');
        ReadLn(TypeCode);
        
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

begin
  // Инициализация
  InitAllLists;
  ExitProgram := False;
  
  // Главный цикл программы
  repeat
    DisplayMainMenu;
    MenuChoice := GetMenuChoice;
    
    case MenuChoice of
      MENU_LOAD_DATA:
        LoadAllData;
      
      MENU_VIEW_LIST:
        ViewList;
      
      MENU_SORT_DATA:
        begin
          WriteLn('Функция сортировки данных будет реализована позже.');
          PressEnterToContinue;
        end;
      
      MENU_SEARCH_DATA:
        begin
          WriteLn('Функция поиска данных с использованием фильтров будет реализована позже.');
          PressEnterToContinue;
        end;
      
      MENU_ADD_DATA:
        AddData;
      
      MENU_REMOVE_DATA:
        RemoveData;
      
      MENU_EDIT_DATA:
        EditData;
      
      MENU_SPECIAL_FUNCTIONS:
        SpecialFunctions;
      
      MENU_EXIT_WITHOUT_SAVE:
        begin
          WriteLn('Выход без сохранения изменений.');
          ExitProgram := True;
        end;
      
      MENU_EXIT_WITH_SAVE:
        begin
          SaveAllData;
          WriteLn('Выход с сохранением изменений.');
          ExitProgram := True;
        end;
    end;
  until ExitProgram;
  
  // Освобождение памяти
  ClearComponentTypeList(ComponentTypesList);
  ClearComponentList(ComponentsList);
  ClearCompatibilityList(CompatibilityList);
  ClearPCBuildOptionList(PCBuildOptionsList);
  ClearOrderList(OrdersList);
end.
