unit s_types;

interface

const
  MAX_STRING_LENGTH = 256;

type
  // Фиксированная строка для использования в типизированных файлах
  TFixedString = array[0..MAX_STRING_LENGTH-1] of Char;
  // Заказ
  
  // (для хранения в файле)
  TFileOrder = record
    id: Integer;
    computerSet: TFileComputerSet;
    orderDate: TDateTime;
    customerName: TFixedString;
    customerPhone: TFixedString;
    customerEmail: TFixedString;
    status: TFixedString;
  end;

  TOrder = record
    id: Integer;
    computerSet: TComputerSet;
    orderDate: TDateTime;
    customerName: string;
    customerPhone: string;
    customerEmail: string;
    status: string; // 'new', 'processing', 'completed', 'cancelled'
  end;

  TOrderStatus = (OS_new, OS_processing, OS_completed, OS_cancelled)

  POrder = ^TOrder;
  
  TOrderList = ^TOrderNode;
  
  TOrderNode = record
    data: TOrder;
    next: TOrderList;
  end;

  // global data lists
  TDataLists = record
    Components: PComponent;
    ComponentTypes: PComponentType;
    Compatibilities: PCompatibility;
    Orders: POrder;
  end;

const
  COMPONENTS_FILE = 'data/components.dat';
  COMPONENT_TYPES_FILE = 'data/types.dat';
  COMPATIBILITY_FILE = 'data/compatibility.dat';
  ORDERS_FILE = 'data/orders.dat';
  ORDER_INFO_TEXT_FILE = 'data/order_info.txt';
  COMPATIBLE_COMPONENTS_TEXT_FILE = 'data/compatible_components.txt';

var
  DataLists: TDataLists;

// Функции для работы с фиксированными строками
function StringToFixed(const s: string): TFixedString;
function FixedToString(const fs: TFixedString): string;

function StringifyTOrderStatus(const x: TOrderStatus): TFixedString;

implementation

// Преобразование строки в фиксированную строку
function StringToFixed(const s: string): TFixedString;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  if i > MAX_STRING_LENGTH then begin
    Writeln(Format('[WARNING]: string __''%s''__ length is more than __%d__, so, it will be saved truncated', [s, MAX_STRING_LENGTH]));
  end;

  i := 1;
  while (i < Length(s)) and (i < MAX_STRING_LENGTH) do
  begin
    Result[i-1] := s[i];
  end;
end;

// Преобразование фиксированной строки в строку
function FixedToString(const fs: TFixedString): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to MAX_STRING_LENGTH-1 do
  begin
    if fs[i] = #0 then
      Break;
    Result := Result + fs[i];
  end;
end;

end.
