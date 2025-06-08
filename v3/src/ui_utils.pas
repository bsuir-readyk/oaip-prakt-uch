unit ui_utils;

interface

uses
  SysUtils, Crt, types, list_utils;

// Функции для отображения меню
procedure ShowMainMenu;
procedure ShowComponentTypesMenu;
procedure ShowComponentsMenu;
procedure ShowCompatibilityMenu;
procedure ShowSpecialFunctionsMenu;

// Функции для отображения списков
procedure DisplayComponentTypesList(const list: TComponentTypeList);
procedure DisplayComponentsList(const list: TComponentList);
procedure DisplayCompatibilityList(const list: TCompatibilityList; const componentsList: TComponentList);
procedure DisplayComputerSetsList(const list: TComputerSetList);
procedure DisplayOrdersList(const list: TOrderList);

// Функции для ввода данных
function SafeReadInteger(const prompt: string; min, max: Integer): Integer;
function SafeReadReal(const prompt: string; min, max: Real): Real;
function SafeReadString(const prompt: string; maxLength: Integer): string;
function SafeReadBoolean(const prompt: string): Boolean;
procedure WaitForKey;

implementation

// Реализация функций для отображения меню
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

procedure ShowComponentTypesMenu;
begin
  ClrScr;
  WriteLn('=== Меню типов комплектующих ===');
  WriteLn('1. Просмотр списка типов комплектующих');
  WriteLn('2. Добавление типа комплектующей');
  WriteLn('3. Удаление типа комплектующей');
  WriteLn('4. Редактирование типа комплектующей');
  WriteLn('5. Возврат в главное меню');
  WriteLn('================================');
end;

procedure ShowComponentsMenu;
begin
  ClrScr;
  WriteLn('=== Меню комплектующих ===');
  WriteLn('1. Просмотр списка комплектующих');
  WriteLn('2. Добавление комплектующей');
  WriteLn('3. Удаление комплектующей');
  WriteLn('4. Редактирование комплектующей');
  WriteLn('5. Возврат в главное меню');
  WriteLn('=========================');
end;

procedure ShowCompatibilityMenu;
begin
  ClrScr;
  WriteLn('=== Меню совместимости ===');
  WriteLn('1. Просмотр списка совместимости');
  WriteLn('2. Добавление совместимости');
  WriteLn('3. Удаление совместимости');
  WriteLn('4. Возврат в главное меню');
  WriteLn('=========================');
end;

procedure ShowSpecialFunctionsMenu;
begin
  ClrScr;
  WriteLn('=== Специальные функции ===');
  WriteLn('1. Подбор вариантов комплектации компьютера в заданном ценовом диапазоне');
  WriteLn('2. Оформление заказа понравившегося варианта');
  WriteLn('3. Вывод комплектующих заданного типа, совместимых с конкретным комплектующим');
  WriteLn('4. Возврат в главное меню');
  WriteLn('==========================');
end;

// Реализация функций для отображения списков
procedure DisplayComponentTypesList(const list: TComponentTypeList);
var
  current: TComponentTypeList;
  count: Integer;
begin
  ClrScr;
  WriteLn('=== Список типов комплектующих ===');
  WriteLn('ID | Название');
  WriteLn('----------------------------');
  
  count := 0;
  current := list;
  
  while current <> nil do
  begin
    WriteLn(current^.data.id, ' | ', current^.data.name);
    current := current^.next;
    Inc(count);
  end;
  
  WriteLn('----------------------------');
  WriteLn('Всего типов: ', count);
  WriteLn('================================');
end;

procedure DisplayComponentsList(const list: TComponentList);
var
  current: TComponentList;
  count: Integer;
begin
  ClrScr;
  WriteLn('=== Список комплектующих ===');
  WriteLn('ID | Тип | Производитель | Модель | Описание | Цена | Наличие');
  WriteLn('-------------------------------------------------------------------');
  
  count := 0;
  current := list;
  
  while current <> nil do
  begin
    with current^.data do
      WriteLn(id, ' | ', cTypeId, ' | ', manufacturer_name, ' | ', model_name, ' | ', 
              description, ' | ', cost:0:2, ' | ', availability);
    current := current^.next;
    Inc(count);
  end;
  
  WriteLn('-------------------------------------------------------------------');
  WriteLn('Всего комплектующих: ', count);
  WriteLn('===============================');
end;

procedure DisplayCompatibilityList(const list: TCompatibilityList; const componentsList: TComponentList);
var
  current: TCompatibilityList;
  comp1, comp2: PComponent;
  count: Integer;
begin
  ClrScr;
  WriteLn('=== Список совместимости ===');
  WriteLn('Комплектующая 1 | Комплектующая 2');
  WriteLn('-------------------------------------------------------------------');
  
  count := 0;
  current := list;
  
  while current <> nil do
  begin
    comp1 := FindComponent(componentsList, current^.data.left_id);
    comp2 := FindComponent(componentsList, current^.data.right_id);
    
    if (comp1 <> nil) and (comp2 <> nil) then
      WriteLn(comp1^.id, ' (', comp1^.model_name, ') | ', comp2^.id, ' (', comp2^.model_name, ')')
    else
      WriteLn(current^.data.left_id, ' | ', current^.data.right_id, ' (Одна из комплектующих не найдена)');
    
    current := current^.next;
    Inc(count);
  end;
  
  WriteLn('-------------------------------------------------------------------');
  WriteLn('Всего записей о совместимости: ', count);
  WriteLn('===============================');
end;

procedure DisplayComputerSetsList(const list: TComputerSetList);
var
  current: TComputerSetList;
  count: Integer;
begin
  ClrScr;
  WriteLn('=== Список компьютерных сборок ===');
  WriteLn('№ | Процессор | Материнская плата | ОЗУ | Блок питания | Накопитель | Общая стоимость');
  WriteLn('-------------------------------------------------------------------');
  
  count := 0;
  current := list;
  
  while current <> nil do
  begin
    with current^.data do
    begin
      Write(count + 1, ' | ');
      
      if cpu <> nil then
        Write(cpu^.model_name)
      else
        Write('Не выбран');
      
      Write(' | ');
      
      if motherboard <> nil then
        Write(motherboard^.model_name)
      else
        Write('Не выбрана');
      
      Write(' | ');
      
      if ram <> nil then
        Write(ram^.model_name)
      else
        Write('Не выбрана');
      
      Write(' | ');
      
      if psu <> nil then
        Write(psu^.model_name)
      else
        Write('Не выбран');
      
      Write(' | ');
      
      if storage <> nil then
        Write(storage^.model_name)
      else
        Write('Не выбран');
      
      WriteLn(' | ', totalCost:0:2);
    end;
    
    current := current^.next;
    Inc(count);
  end;
  
  WriteLn('-------------------------------------------------------------------');
  WriteLn('Всего сборок: ', count);
  WriteLn('===============================');
end;

procedure DisplayOrdersList(const list: TOrderList);
var
  current: TOrderList;
  count: Integer;
begin
  ClrScr;
  WriteLn('=== Список заказов ===');
  WriteLn('ID | Заказчик | Телефон | Email | Дата | Статус | Стоимость');
  WriteLn('-------------------------------------------------------------------');
  
  count := 0;
  current := list;
  
  while current <> nil do
  begin
    with current^.data do
      WriteLn(id, ' | ', customerName, ' | ', customerPhone, ' | ', customerEmail, ' | ', 
              DateToStr(orderDate), ' | ', status, ' | ', computerSet.totalCost:0:2);
    
    current := current^.next;
    Inc(count);
  end;
  
  WriteLn('-------------------------------------------------------------------');
  WriteLn('Всего заказов: ', count);
  WriteLn('===============================');
end;

// Реализация функций для ввода данных
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

function SafeReadReal(const prompt: string; min, max: Real): Real;
var
  s: string;
  code: Integer;
  value: Real;
begin
  repeat
    Write(prompt);
    ReadLn(s);
    
    Val(s, value, code);
    
    if (code <> 0) or (value < min) or (value > max) then
      WriteLn('Ошибка! Введите число от ', min:0:2, ' до ', max:0:2, '.');
  until (code = 0) and (value >= min) and (value <= max);
  
  Result := value;
end;

function SafeReadString(const prompt: string; maxLength: Integer): string;
var
  s: string;
begin
  repeat
    Write(prompt);
    ReadLn(s);
    
    if (Length(s) = 0) or (Length(s) > maxLength) then
      WriteLn('Ошибка! Введите строку длиной от 1 до ', maxLength, ' символов.');
  until (Length(s) > 0) and (Length(s) <= maxLength);
  
  Result := s;
end;

function SafeReadBoolean(const prompt: string): Boolean;
var
  s: string;
begin
  repeat
    Write(prompt, ' (д/н): ');
    ReadLn(s);
    
    s := LowerCase(s);
  until (s = 'д') or (s = 'н') or (s = 'y') or (s = 'n');
  
  Result := (s = 'д') or (s = 'y');
end;

procedure WaitForKey;
begin
  WriteLn;
  Write('Нажмите любую клавишу для продолжения...');
  ReadKey;
end;

end.
