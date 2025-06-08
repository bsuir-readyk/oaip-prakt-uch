unit UiUtils;

interface

uses
  SysUtils, Crt;

procedure WaitForKey;
procedure PrintSeparator;
procedure PrintHeader(const Title: string);

function SafeReadInteger(const Prompt: string; MinValue, MaxValue: Integer): Integer;
function SafeReadReal(const Prompt: string; MinValue, MaxValue: Real): Real;
function SafeReadString(const Prompt: string): string;
function SafeReadBoolean(const Prompt: string): Boolean;

implementation

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

function SafeReadInteger(const Prompt: string; MinValue, MaxValue: Integer): Integer;
var
  S: string;
  Code: Integer;
  Value: Integer;
  validInput: Boolean;
begin
  validInput := false;
  
  for i := 1 to 3 do
  begin
    writeln;
    Write(Prompt);
    ReadLn(S);
    Val(S, Value, Code);
    if (Code <> 0) or (Value < MinValue) or (Value > MaxValue) then
  end;

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
  BoolValue := false;
  validInput := false;
  
  repeat
    Write(Prompt, ' (д/н): ');
    ReadLn(S);
    S := LowerCase(Trim(S));
    if (S = 'д') then
    begin
      BoolValue := true;
      validInput := true;
    end
    else if (S = 'н') then
    begin
      BoolValue := false;
      validInput := true;
    end
    else
      WriteLn('Ошибка! Введите "д" или "н".');
  until validInput;
  
  SafeReadBoolean := BoolValue;
end;

end.
