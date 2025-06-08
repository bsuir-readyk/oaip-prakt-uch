unit types;

interface

const
  MAX_STRING_LENGTH = 256;

type
  // Фиксированная строка для использования в типизированных файлах
  TFixedString = array[0..MAX_STRING_LENGTH-1] of Char;

  // Тип комплектующей
  TComponentType = record
    id: Integer;
    name: TFixedString;
  end;
  PComponentType = ^TComponentType;
  TComponentTypeList = ^TComponentTypeNode;
  TComponentTypeNode = record
    data: TComponentType;
    next: TComponentTypeList;
  end;

  // Комплектующая
  TComponent = record
    id: Integer;
    cTypeId: Integer;
    manufacturer_name: TFixedString;
    model_name: TFixedString;
    description: TFixedString;
    cost: Real;
    availability: Integer;
  end;
  PComponent = ^TComponent;
  TComponentList = ^TComponentNode;
  TComponentNode = record
    data: TComponent;
    next: TComponentList;
  end;

  // Совместимость комплектующих
  TCompatibility = record
    left_id: Integer;
    right_id: Integer;
  end;
  PCompatibility = ^TCompatibility;
  TCompatibilityList = ^TCompatibilityNode;
  TCompatibilityNode = record
    data: TCompatibility;
    next: TCompatibilityList;
  end;

  // Компьютерная сборка (для хранения в файле)
  TFileComputerSet = record
    cpu_id: Integer;
    motherboard_id: Integer;
    ram_id: Integer;
    psu_id: Integer;
    storage_id: Integer;
    totalCost: Real;
  end;

  // Компьютерная сборка (для работы в программе)
  TComputerSet = record
    cpu: PComponent;
    motherboard: PComponent;
    ram: PComponent;
    psu: PComponent;
    storage: PComponent;
    totalCost: Real;
  end;
  PComputerSet = ^TComputerSet;
  TComputerSetList = ^TComputerSetNode;
  TComputerSetNode = record
    data: TComputerSet;
    next: TComputerSetList;
  end;

  // Заказ (для хранения в файле)
  TFileOrder = record
    id: Integer;
    computerSet: TFileComputerSet;
    orderDate: TDateTime;
    customerName: TFixedString;
    customerPhone: TFixedString;
    customerEmail: TFixedString;
    status: TFixedString;
  end;

  // Заказ (для работы в программе)
  TOrder = record
    id: Integer;
    computerSet: TComputerSet;
    orderDate: TDateTime;
    customerName: string;
    customerPhone: string;
    customerEmail: string;
    status: string; // 'new', 'processing', 'completed', 'cancelled'
  end;
  POrder = ^TOrder;
  TOrderList = ^TOrderNode;
  TOrderNode = record
    data: TOrder;
    next: TOrderList;
  end;

// Функции для работы с фиксированными строками
function StringToFixed(const s: string): TFixedString;
function FixedToString(const fs: TFixedString): string;

implementation

// Преобразование строки в фиксированную строку
function StringToFixed(const s: string): TFixedString;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  for i := 1 to Length(s) do
  begin
    if i > MAX_STRING_LENGTH then
      Break;
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
