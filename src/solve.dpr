program solve;

{$mode objfpc}{$H+}

uses
  SysUtils,
  DataTypes,
  DynamicLists,
  FileOperations,
  UI,
  SpecialFunctions,
  MenuHandlers,
  MainProgram;

var
  // Списки данных
  ComponentTypesList: TComponentTypeList;
  ComponentsList: TComponentList;
  CompatibilityList: TCompatibilityList;
  PCBuildOptionsList: TPCBuildOptionList;
  OrdersList: TOrderList;
  
  // Переменные для работы с меню
  MenuChoice: Integer;
  ExitProgram: Boolean;

begin
  // Инициализация
  InitAllLists(ComponentTypesList, ComponentsList, CompatibilityList, 
               PCBuildOptionsList, OrdersList);
  ExitProgram := False;
  
  // Главный цикл программы
  repeat
    DisplayMainMenu;
    MenuChoice := GetMenuChoice;
    
    case MenuChoice of
      MENU_LOAD_DATA:
        LoadAllData(ComponentTypesList, ComponentsList, CompatibilityList, OrdersList);
      
      MENU_VIEW_LIST:
        ViewList(ComponentTypesList, ComponentsList, CompatibilityList, 
                 OrdersList, PCBuildOptionsList);
      
      MENU_SORT_DATA:
        HandleSortData(ComponentTypesList, ComponentsList, OrdersList);
      
      MENU_SEARCH_DATA:
        HandleSearchData(ComponentsList, ComponentTypesList);
      
      MENU_ADD_DATA:
        AddData(ComponentTypesList, ComponentsList, CompatibilityList);
      
      MENU_REMOVE_DATA:
        RemoveData(ComponentTypesList, ComponentsList, CompatibilityList);
      
      MENU_EDIT_DATA:
        EditData(ComponentTypesList, ComponentsList, CompatibilityList);
      
      MENU_SPECIAL_FUNCTIONS:
        HandleSpecialFunctions(ComponentTypesList, ComponentsList, CompatibilityList,
                               PCBuildOptionsList, OrdersList);
      
      MENU_EXIT_WITHOUT_SAVE:
        begin
          WriteLn('Выход без сохранения изменений.');
          ExitProgram := True;
        end;
      
      MENU_EXIT_WITH_SAVE:
        begin
          SaveAllData(ComponentTypesList, ComponentsList, CompatibilityList, OrdersList);
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
