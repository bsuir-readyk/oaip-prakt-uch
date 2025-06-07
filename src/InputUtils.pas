unit InputUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  // Константы для специальных команд ввода
  CMD_RETURN_TO_MENU = '/menu';
  CMD_CANCEL = '/cancel';

// Функции для безопасного чтения различных типов данных
function SafeReadInteger(const Prompt: string; MinValue: Integer = Low(Integer); MaxValue: Integer = High(Integer); 
                         const ErrorMsg: string = ''): Integer;
function SafeReadFloat(const Prompt: string; MinValue: Real = -1E38; MaxValue: Real = 1E38; 
                      const ErrorMsg: string = ''): Real;
function SafeReadString(const Prompt: string; MinLength: Integer = 0; MaxLength: Integer = High(Integer); 
                       const ErrorMsg: string = ''): string;
function SafeReadBoolean(const Prompt: string; const ErrorMsg: string = ''): Boolean;

// Функция для проверки, хочет ли пользователь вернуться в меню
function IsReturnToMenuCommand(const Input: string): Boolean;

// Функция для проверки, хочет ли пользователь отменить текущую операцию
function IsCancelCommand(const Input: string): Boolean;

// Функция для безопасного подтверждения операций
function SafeReadConfirmation(const Prompt: string; const ErrorMsg: string = ''): Boolean;

implementation

function SafeReadInteger(const Prompt: string; MinValue: Integer; MaxValue: Integer; const ErrorMsg: string): Integer;
var
  InputStr: string;
  Code: Integer;
  DefaultErrorMsg: string;
begin
  DefaultErrorMsg := Format('Пожалуйста, введите целое число от %d до %d или "%s" для возврата в меню', 
                           [MinValue, MaxValue, CMD_RETURN_TO_MENU]);
  
  if ErrorMsg <> '' then
    DefaultErrorMsg := ErrorMsg;
  
  repeat
    Write(Prompt);
    ReadLn(InputStr);
    
    // Проверка на команду возврата в меню
    if IsReturnToMenuCommand(InputStr) then
    begin
      Result := MinValue - 1; // Специальное значение для индикации возврата в меню
      Exit;
    end;
    
    // Проверка на команду отмены
    if IsCancelCommand(InputStr) then
    begin
      Result := MinValue - 2; // Специальное значение для индикации отмены
      Exit;
    end;
    
    // Попытка преобразовать строку в целое число
    Val(InputStr, Result, Code);
    
    if (Code <> 0) or (Result < MinValue) or (Result > MaxValue) then
    begin
      WriteLn(DefaultErrorMsg);
      Code := 1; // Установка кода ошибки для повторного запроса
    end;
  until Code = 0;
end;

function SafeReadFloat(const Prompt: string; MinValue: Real; MaxValue: Real; const ErrorMsg: string): Real;
var
  InputStr: string;
  Code: Integer;
  DefaultErrorMsg: string;
begin
  DefaultErrorMsg := Format('Пожалуйста, введите число от %f до %f или "%s" для возврата в меню', 
                           [MinValue, MaxValue, CMD_RETURN_TO_MENU]);
  
  if ErrorMsg <> '' then
    DefaultErrorMsg := ErrorMsg;
  
  repeat
    Write(Prompt);
    ReadLn(InputStr);
    
    // Проверка на команду возврата в меню
    if IsReturnToMenuCommand(InputStr) then
    begin
      Result := MinValue - 1; // Специальное значение для индикации возврата в меню
      Exit;
    end;
    
    // Проверка на команду отмены
    if IsCancelCommand(InputStr) then
    begin
      Result := MinValue - 2; // Специальное значение для индикации отмены
      Exit;
    end;
    
    // Попытка преобразовать строку в число с плавающей точкой
    Val(InputStr, Result, Code);
    
    if (Code <> 0) or (Result < MinValue) or (Result > MaxValue) then
    begin
      WriteLn(DefaultErrorMsg);
      Code := 1; // Установка кода ошибки для повторного запроса
    end;
  until Code = 0;
end;

function SafeReadString(const Prompt: string; MinLength: Integer; MaxLength: Integer; const ErrorMsg: string): string;
var
  InputStr: string;
  DefaultErrorMsg: string;
begin
  DefaultErrorMsg := Format('Пожалуйста, введите строку длиной от %d до %d символов или "%s" для возврата в меню', 
                           [MinLength, MaxLength, CMD_RETURN_TO_MENU]);
  
  if ErrorMsg <> '' then
    DefaultErrorMsg := ErrorMsg;
  
  repeat
    Write(Prompt);
    ReadLn(InputStr);
    
    // Проверка на команду возврата в меню
    if IsReturnToMenuCommand(InputStr) then
    begin
      Result := CMD_RETURN_TO_MENU;
      Exit;
    end;
    
    // Проверка на команду отмены
    if IsCancelCommand(InputStr) then
    begin
      Result := CMD_CANCEL;
      Exit;
    end;
    
    // Проверка длины строки
    if (Length(InputStr) < MinLength) or (Length(InputStr) > MaxLength) then
    begin
      WriteLn(DefaultErrorMsg);
      Continue;
    end;
    
    Result := InputStr;
    Break;
  until False;
end;

function SafeReadBoolean(const Prompt: string; const ErrorMsg: string): Boolean;
var
  InputStr: string;
  DefaultErrorMsg: string;
begin
  DefaultErrorMsg := Format('Пожалуйста, введите "да", "нет" или "%s" для возврата в меню', [CMD_RETURN_TO_MENU]);
  
  if ErrorMsg <> '' then
    DefaultErrorMsg := ErrorMsg;
  
  repeat
    Write(Prompt);
    ReadLn(InputStr);
    
    // Проверка на команду возврата в меню
    if IsReturnToMenuCommand(InputStr) then
    begin
      Result := False; // Специальное значение для индикации возврата в меню
      Exit;
    end;
    
    // Проверка на команду отмены
    if IsCancelCommand(InputStr) then
    begin
      Result := False; // Специальное значение для индикации отмены
      Exit;
    end;
    
    // Проверка на "да"
    if (InputStr = 'да') or (InputStr = 'Да') or (InputStr = 'ДА') or 
       (InputStr = 'y') or (InputStr = 'Y') or (InputStr = 'yes') or (InputStr = 'Yes') then
    begin
      Result := True;
      Exit;
    end;
    
    // Проверка на "нет"
    if (InputStr = 'нет') or (InputStr = 'Нет') or (InputStr = 'НЕТ') or 
       (InputStr = 'n') or (InputStr = 'N') or (InputStr = 'no') or (InputStr = 'No') then
    begin
      Result := False;
      Exit;
    end;
    
    WriteLn(DefaultErrorMsg);
  until False;
end;

function IsReturnToMenuCommand(const Input: string): Boolean;
begin
  Result := (Input = CMD_RETURN_TO_MENU);
end;

function IsCancelCommand(const Input: string): Boolean;
begin
  Result := (Input = CMD_CANCEL);
end;

function SafeReadConfirmation(const Prompt: string; const ErrorMsg: string = ''): Boolean;
var
  InputStr: string;
  DefaultErrorMsg: string;
begin
  DefaultErrorMsg := Format('Пожалуйста, введите "да", "нет" или "%s" для возврата в меню', [CMD_RETURN_TO_MENU]);
  
  if ErrorMsg <> '' then
    DefaultErrorMsg := ErrorMsg;
  
  repeat
    Write(Prompt);
    ReadLn(InputStr);
    
    // Проверка на команду возврата в меню
    if IsReturnToMenuCommand(InputStr) then
    begin
      Result := False; // Специальное значение для индикации возврата в меню
      Exit;
    end;
    
    // Проверка на команду отмены
    if IsCancelCommand(InputStr) then
    begin
      Result := False; // Специальное значение для индикации отмены
      Exit;
    end;
    
    // Проверка на "да"
    if (InputStr = 'да') or (InputStr = 'Да') or (InputStr = 'ДА') or 
       (InputStr = 'y') or (InputStr = 'Y') or (InputStr = 'yes') or (InputStr = 'Yes') then
    begin
      Result := True;
      Exit;
    end;
    
    // Проверка на "нет"
    if (InputStr = 'нет') or (InputStr = 'Нет') or (InputStr = 'НЕТ') or 
       (InputStr = 'n') or (InputStr = 'N') or (InputStr = 'no') or (InputStr = 'No') then
    begin
      Result := False;
      Exit;
    end;
    
    WriteLn(DefaultErrorMsg);
  until False;
end;

end.
