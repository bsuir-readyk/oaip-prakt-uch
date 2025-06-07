unit MainProgram;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  DataTypes,
  DynamicLists,
  UI,
  InputUtils,
  MenuHandlers,
  DataSorting,
  DataSearch;

// Процедуры для обработки различных пунктов меню
procedure HandleSortData(var ComponentTypesList: TComponentTypeList;
                         var ComponentsList: TComponentList;
                         var OrdersList: TOrderList);

procedure HandleSearchData(const ComponentsList: TComponentList;
                           const ComponentTypesList: TComponentTypeList);

// Процедура для инициализации всех списков
procedure InitAllLists(var ComponentTypesList: TComponentTypeList;
                       var ComponentsList: TComponentList;
                       var CompatibilityList: TCompatibilityList;
                       var PCBuildOptionsList: TPCBuildOptionList;
                       var OrdersList: TOrderList);

implementation

// Процедура для инициализации списков
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

// Процедура для сортировки данных
procedure HandleSortData(var ComponentTypesList: TComponentTypeList;
                         var ComponentsList: TComponentList;
                         var OrdersList: TOrderList);
var
  SortChoice: Integer;
begin
  WriteLn('=== СОРТИРОВКА ДАННЫХ ===');
  WriteLn('Выберите тип данных для сортировки:');
  WriteLn('1. Типы комплектующих (по коду)');
  WriteLn('2. Комплектующие (по цене)');
  WriteLn('3. Комплектующие (по коду)');
  WriteLn('4. Комплектующие (по производителю)');
  WriteLn('5. Заказы (по дате)');
  WriteLn('========================');
  
  SortChoice := SafeReadInteger('Выберите пункт (1-5): ', 1, 5, 'Ошибка: Выберите пункт от 1 до 5.');
  
  case SortChoice of
    1: SortComponentTypesByCode(ComponentTypesList);
    2: SortComponentsByPrice(ComponentsList);
    3: SortComponentsByCode(ComponentsList);
    4: SortComponentsByManufacturer(ComponentsList);
    5: SortOrdersByDate(OrdersList);
  end;
  
  WriteLn('Данные отсортированы.');
  PressEnterToContinue;
end;

// Процедура для поиска данных с фильтрами
procedure HandleSearchData(const ComponentsList: TComponentList;
                           const ComponentTypesList: TComponentTypeList);
var
  SearchChoice: Integer;
  SearchResults: TComponentList;
  FilterMinPrice, FilterMaxPrice: Real;
  FilterManufacturer, FilterModel: string;
  FilterTypeCode: Integer;
  FilterMinInStock: Integer;
begin
  WriteLn('=== ПОИСК ДАННЫХ С ФИЛЬТРАМИ ===');
  WriteLn('Выберите тип поиска:');
  WriteLn('1. Комплектующие по диапазону цен');
  WriteLn('2. Комплектующие по производителю');
  WriteLn('3. Комплектующие по типу');
  WriteLn('4. Комплектующие по модели');
  WriteLn('5. Комплектующие с минимальным количеством в наличии');
  WriteLn('===============================');
  
  SearchChoice := SafeReadInteger('Выберите пункт (1-5): ', 1, 5, 'Ошибка: Выберите пункт от 1 до 5.');
  
  InitComponentList(SearchResults);
  
  case SearchChoice of
    1: begin
         FilterMinPrice := SafeReadFloat('Введите минимальную цену: ', 0, 1E10);
         FilterMaxPrice := SafeReadFloat('Введите максимальную цену: ', FilterMinPrice, 1E10);
         SearchComponentsByPriceRange(ComponentsList, FilterMinPrice, FilterMaxPrice, SearchResults);
       end;
    2: begin
         FilterManufacturer := SafeReadString('Введите производителя: ', 1, 50);
         SearchComponentsByManufacturer(ComponentsList, FilterManufacturer, SearchResults);
       end;
    3: begin
         FilterTypeCode := SafeReadInteger('Введите код типа комплектующей: ', 1, High(Integer));
         SearchComponentsByType(ComponentsList, FilterTypeCode, SearchResults);
       end;
    4: begin
         FilterModel := SafeReadString('Введите модель: ', 1, 50);
         SearchComponentsByModel(ComponentsList, FilterModel, SearchResults);
       end;
    5: begin
         FilterMinInStock := SafeReadInteger('Введите минимальное количество в наличии: ', 0, High(Integer));
         SearchComponentsByMinStock(ComponentsList, FilterMinInStock, SearchResults);
       end;
  end;
  
  WriteLn;
  WriteLn('=== РЕЗУЛЬТАТЫ ПОИСКА ===');
  if SearchResults.Count > 0 then
  begin
    WriteLn('Найдено записей: ', SearchResults.Count);
    DisplayComponents(SearchResults, ComponentTypesList);
  end
  else
  begin
    WriteLn('По заданным критериям ничего не найдено.');
    PressEnterToContinue;
  end;
  
  ClearComponentList(SearchResults);
end;

end. 
