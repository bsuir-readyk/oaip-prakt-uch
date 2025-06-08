program Solve;

uses
  SysUtils, Crt,
  logging in './src/logging.pas',
  types in './src/types.pas',
  file_io in './src/file_io.pas',
  list_utils in './src/list_utils.pas',
  sort_utils in './src/sort_utils.pas',
  filter_utils in './src/filter_utils.pas',
  computer_builder in './src/computer_builder.pas',
  order_manager in './src/order_manager.pas',
  ui_utils in './src/ui_utils.pas',
  menu_handlers in './src/menu_handlers.pas';

var
  choice: Integer;
  exitProgram: Boolean;
  exitWithSave: Boolean;
  log: TLogF;
  
  // Списки данных
  componentTypesList: TComponentTypeList;
  componentsList: TComponentList;
  compatibilityList: TCompatibilityList;
  ordersList: TOrderList;
  computerSetsList: TComputerSetList;

begin
  log := GetLogger(LL_DEBUG);
  exitProgram := False;
  exitWithSave := False;
  
  // Инициализация списков
  componentTypesList := nil;
  componentsList := nil;
  compatibilityList := nil;
  ordersList := nil;
  computerSetsList := nil;
  
  // Создание директорий для данных и результатов
  if not DirectoryExists('data') then
    CreateDir('data');
  
  if not DirectoryExists('output') then
    CreateDir('output');
  
  // Процедура для отображения главного меню
  procedure ShowMainMenu;
  begin
    ClrScr;
    WriteLn('=== Система управления компьютерными комплектующими ===');
    WriteLn('1. Чтение данных из файла');
    WriteLn('2. Просмотр всего списка');
    WriteLn('3. Сортировка данных');
    WriteLn('4. Поиск данных с использованием фильтров');
    WriteLn('5. Добавление данных в список');
    WriteLn('6. Удаление данных из списка');
    WriteLn('7. Редактирование данных');
    WriteLn('8. Специальные функции');
    WriteLn('9. Выход из программы без сохранения изменений');
    WriteLn('10. Выход с сохранением изменений');
    WriteLn('=======================================================');
  end;
  
  // Функция для безопасного чтения целого числа
  function SafeReadInteger(const prompt: string; min, max: Integer): Integer;
  var
    s: string;
    code: Integer;
    value: Integer;
  begin
    repeat
      Write(prompt);
      ReadLn(s);
      
      Val(s, value, code);
      
      if (code <> 0) or (value < min) or (value > max) then
        WriteLn('Ошибка! Введите целое число от ', min, ' до ', max, '.');
    until (code = 0) and (value >= min) and (value <= max);
    
    Result := value;
  end;
  
  // Процедура для ожидания нажатия клавиши
  procedure WaitForKey;
  begin
    WriteLn;
    Write('Нажмите любую клавишу для продолжения...');
    ReadKey;
  end;
  
  while not exitProgram do
  begin
    ShowMainMenu;
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 10);
    
    case choice of
      1: begin // Чтение данных из файла
        HandleReadFromFile(componentTypesList, componentsList, compatibilityList, ordersList);
        WaitForKey;
      end;
      2: begin // Просмотр всего списка
        HandleViewList(componentTypesList, componentsList, compatibilityList, ordersList);
        WaitForKey;
      end;
      3: begin // Сортировка данных
        HandleSortData(componentsList, computerSetsList);
        WaitForKey;
      end;
      4: begin // Поиск данных с использованием фильтров
        HandleSearchWithFilters(componentsList, componentTypesList, compatibilityList);
        WaitForKey;
      end;
      5: begin // Добавление данных в список
        HandleAddData(componentTypesList, componentsList, compatibilityList);
        WaitForKey;
      end;
      6: begin // Удаление данных из списка
        HandleRemoveData(componentTypesList, componentsList, compatibilityList);
        WaitForKey;
      end;
      7: begin // Редактирование данных
        HandleEditData(componentTypesList, componentsList, compatibilityList);
        WaitForKey;
      end;
      8: begin // Специальные функции
        HandleSpecialFunctions(componentsList, componentTypesList, compatibilityList, ordersList, computerSetsList);
        WaitForKey;
      end;
      9: begin // Выход из программы без сохранения изменений
        log(LL_DEBUG, 'Выход без сохранения');
        exitProgram := True;
      end;
      10: begin // Выход с сохранением изменений
        HandleSaveToFile(componentTypesList, componentsList, compatibilityList, ordersList);
        exitProgram := True;
      end;
    end;
  end;
  
  // Освобождение памяти
  ClearComponentTypeList(componentTypesList);
  ClearComponentList(componentsList);
  ClearCompatibilityList(compatibilityList);
  ClearOrderList(ordersList);
  ClearComputerSetList(computerSetsList);
  
  WriteLn('Программа завершена.');
end.
