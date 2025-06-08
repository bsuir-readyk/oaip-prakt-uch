unit UI;

interface

uses
  SysUtils, Crt, Types;

procedure ShowMainMenu;
procedure ShowViewSubmenu;
procedure ShowSortSubmenu;
procedure ShowAddSubmenu;
procedure ShowDeleteSubmenu;
procedure ShowEditSubmenu;
procedure ShowSpecialFunctionsSubmenu;
procedure WaitForKey;
procedure PrintSeparator;
procedure PrintHeader(const Title: string);
procedure PrintTableHorizontalLine(const ColumnWidths: array of Integer; LineType: Char);
procedure PrintTableRow(const Values: array of string; const ColumnWidths: array of Integer; const Alignments: array of Char);

function PadString(const S: string; Width: Integer; Alignment: Char): string;
function SafeReadInteger(const Prompt: string; MinValue, MaxValue: Integer): Integer;
function SafeReadReal(const Prompt: string; MinValue, MaxValue: Real): Real;
function SafeReadString(const Prompt: string): string;
function SafeReadBoolean(const Prompt: string): Boolean;

implementation

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

procedure WaitForKey;
begin
  WriteLn;
  Write('Нажмите Enter для продолжения...');
  ReadLn;
end;

procedure PrintSeparator;
begin
  WriteLn('----------------------------------------');
end;

procedure PrintHeader(const Title: string);
begin
  PrintSeparator;
  WriteLn(Title);
  PrintSeparator;
  WriteLn;
end;

function RepeatChar(Ch: string; Count: Integer): string;
var
  i: Integer;
  res: string;
begin
  res := '';
  for i := 1 to Count do
    res := res + Ch;
  RepeatChar := res;
end;

function PadString(const S: string; Width: Integer; Alignment: Char): string;
var
  Padding, res: string;
  PaddingLength: Integer;
begin
  if Length(S) >= Width then
    res := Copy(S, 1, Width)
  else
  begin
    PaddingLength := Width - Length(S);
    Padding := RepeatChar(' ', PaddingLength);
    
    case Alignment of
      'L': res := S + Padding;     
      'R': res := Padding + S;     
      'C': begin                      
             Padding := RepeatChar(' ', PaddingLength div 2);
             res := Padding + S + Padding;
             if Length(res) < Width then
               res := res + ' ';
           end;
      else res := S + Padding;     
    end;
  end;
  
  PadString := res;
end;

procedure PrintTableHorizontalLine(const ColumnWidths: array of Integer; LineType: Char);
var
  i: Integer;
begin
  case LineType of
    'T': begin
           Write('┌');
           for i := low(ColumnWidths) to High(ColumnWidths) do
           begin
             Write(RepeatChar('-', ColumnWidths[i] + 2));
             if i < High(ColumnWidths) then
               Write('┬');
           end;
           WriteLn('┐');
         end;
    'M': begin
           Write('├');
           for i := low(ColumnWidths) to High(ColumnWidths) do
           begin
             Write(RepeatChar('-', ColumnWidths[i] + 2));
             if i < High(ColumnWidths) then
               Write('┼');
           end;
           WriteLn('┤');
         end;
    'B': begin
           Write('└');
           for i := low(ColumnWidths) to High(ColumnWidths) do
           begin
             Write(RepeatChar('-', ColumnWidths[i] + 2));
             if i < High(ColumnWidths) then
               Write('┴');
           end;
           WriteLn('┘');
         end;
  end;
end;

procedure PrintTableRow(const Values: array of string; const ColumnWidths: array of Integer; const Alignments: array of Char);
var
  i: Integer;
begin
  Write('│');
  for i := 0 to High(Values) do
  begin
    if i <= High(ColumnWidths) then
      Write(' ', PadString(Values[i], ColumnWidths[i], Alignments[i]), ' ')
    else
      Write(' ', Values[i], ' ');
    
    Write('│');
  end;
  WriteLn;
end;

end.
