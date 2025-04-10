unit UI;

interface

uses
  SysUtils, Crt, Types;

// Вывод главного меню
procedure ShowMainMenu;

// Вывод подменю просмотра списков
procedure ShowViewSubmenu;

// Вывод подменю сортировки
procedure ShowSortSubmenu;

// Вывод подменю добавления данных
procedure ShowAddSubmenu;

// Вывод подменю удаления данных
procedure ShowDeleteSubmenu;

// Вывод подменю редактирования данных
procedure ShowEditSubmenu;

// Вывод подменю специальных функций
procedure ShowSpecialFunctionsSubmenu;

// Безопасный ввод целого числа
function SafeReadInteger(const Prompt: string; MinValue, MaxValue: Integer): Integer;

// Безопасный ввод вещественного числа
function SafeReadReal(const Prompt: string; MinValue, MaxValue: Real): Real;

// Безопасный ввод строки
function SafeReadString(const Prompt: string): string;

// Безопасный ввод булевого значения
function SafeReadBoolean(const Prompt: string): Boolean;

// Ожидание нажатия клавиши
procedure WaitForKey;

// Вывод разделительной линии
procedure PrintSeparator;

// Вывод заголовка
procedure PrintHeader(const Title: string);

implementation

// Вывод главного меню
procedure ShowMainMenu;
begin
  ClrScr;
  PrintHeader('ГЛАВНОЕ МЕНЮ');
  WriteLn('1. Чтение данных из файла');
  WriteLn('2. Просмотр списка');
  WriteLn('3. Сортировка данных');
  WriteLn('4. Поиск данных с использованием фильтров');
  WriteLn('5. Добавление данных');
  WriteLn('6. Удаление данных');
  WriteLn('7. Редактирование данных');
  WriteLn('8. Специальные функции');
  WriteLn('9. Выход без сохранения');
  WriteLn('10. Выход с сохранением');
  PrintSeparator;
end;

// Вывод подменю просмотра списков
procedure ShowViewSubmenu;
begin
  ClrScr;
  PrintHeader('ПРОСМОТР СПИСКА');
  WriteLn('1. Комплектующие');
  WriteLn('2. Типы комплектующих');
  WriteLn('3. Совместимость');
  WriteLn('4. Заказы');
  WriteLn('0. Назад');
  PrintSeparator;
end;

// Вывод подменю сортировки
procedure ShowSortSubmenu;
begin
  ClrScr;
  PrintHeader('СОРТИРОВКА ДАННЫХ');
  WriteLn('1. По цене');
  WriteLn('2. По наличию');
  WriteLn('3. По производителю');
  WriteLn('0. Назад');
  PrintSeparator;
end;

// Вывод подменю добавления данных
procedure ShowAddSubmenu;
begin
  ClrScr;
  PrintHeader('ДОБАВЛЕНИЕ ДАННЫХ');
  WriteLn('1. Комплектующая');
  WriteLn('2. Тип комплектующей');
  WriteLn('3. Совместимость');
  WriteLn('0. Назад');
  PrintSeparator;
end;

// Вывод подменю удаления данных
procedure ShowDeleteSubmenu;
begin
  ClrScr;
  PrintHeader('УДАЛЕНИЕ ДАННЫХ');
  WriteLn('1. Комплектующая');
  WriteLn('2. Тип комплектующей');
  WriteLn('3. Совместимость');
  WriteLn('4. Заказ');
  WriteLn('0. Назад');
  PrintSeparator;
end;

// Вывод подменю редактирования данных
procedure ShowEditSubmenu;
begin
  ClrScr;
  PrintHeader('РЕДАКТИРОВАНИЕ ДАННЫХ');
  WriteLn('1. Комплектующая');
  WriteLn('2. Тип комплектующей');
  WriteLn('3. Совместимость');
  WriteLn('4. Заказ');
  WriteLn('0. Назад');
  PrintSeparator;
end;

// Вывод подменю специальных функций
procedure ShowSpecialFunctionsSubmenu;
begin
  ClrScr;
  PrintHeader('СПЕЦИАЛЬНЫЕ ФУНКЦИИ');
  WriteLn('1. Подбор вариантов комплектации компьютера');
  WriteLn('2. Оформление заказа');
  WriteLn('3. Поиск совместимых комплектующих заданного типа');
  WriteLn('0. Назад');
  PrintSeparator;
end;

// Безопасный ввод целого числа
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

// Безопасный ввод вещественного числа
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

// Безопасный ввод строки
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

// Безопасный ввод булевого значения
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

// Ожидание нажатия клавиши
procedure WaitForKey;
begin
  WriteLn;
  Write('Нажмите Enter для продолжения...');
  ReadLn;
end;

// Вывод разделительной линии
procedure PrintSeparator;
begin
  WriteLn('----------------------------------------');
end;

// Вывод заголовка
procedure PrintHeader(const Title: string);
begin
  PrintSeparator;
  WriteLn(Title);
  PrintSeparator;
  WriteLn;
end;

end.
