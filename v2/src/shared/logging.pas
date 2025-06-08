unit s_logging;

interface

uses
  SysUtils, TypInfo, Classes, Variants, Rtti, s_types;

type TLOG_LEVEL = (LL_NO_LOGS, LL_DEBUG, LL_INFO, LL_WARN, LL_ERR);
type TLogF = procedure(level: TLOG_LEVEL; msg: string);

// Логирование
function TLOG_LEVEL_Stirngify(ll: TLOG_LEVEL): string;
procedure Log(level: TLOG_LEVEL; msg: string);

procedure LogLNo(level: TLOG_LEVEL; msg: string);
procedure LogLDebug(level: TLOG_LEVEL; msg: string);
procedure LogLInfo(level: TLOG_LEVEL; msg: string);
procedure LogLWarn(level: TLOG_LEVEL; msg: string);
procedure LogLErr(level: TLOG_LEVEL; msg: string);

function GetLogger(ll: TLOG_LEVEL): TLogF;

// Функции для преобразования различных типов в строку
function Stringify(Value: Integer): string; overload;
function Stringify(Value: Int64): string; overload;
function Stringify(Value: Boolean): string; overload;
function Stringify(Value: Double): string; overload;
function Stringify(Value: string): string; overload;
function Stringify(Value: TObject): string; overload;
function Stringify(Value: Pointer): string; overload;
function Stringify(Value: TClass): string; overload;
function Stringify(Value: Variant): string; overload;
function StringifyEnum(Value: Integer; TypeInfo: PTypeInfo): string;
function StringifySet(Value: Integer; TypeInfo: PTypeInfo): string;
// function StringifyRecord(constref ARecord; ATypeInfo: PTypeInfo): string;
function StringifyObject(Obj: TObject): string;
function StringifyArr(v: array of Variant): string;
function StringifyArrInt(v: TArrayInt): string;

implementation

// Базовые типы
function Stringify(Value: Integer): string;
begin
  Result := IntToStr(Value);
end;

function Stringify(Value: Int64): string;
begin
  Result := IntToStr(Value);
end;

function Stringify(Value: Boolean): string;
begin
  Result := BoolToStr(Value, True);
end;

function Stringify(Value: Double): string;
begin
  Result := FloatToStr(Value);
end;


function Stringify(Value: string): string;
begin
  Result := Value;
end;

function Stringify(Value: Pointer): string;
begin
  if Value = nil then
    Result := 'nil'
  else
    Result := Format('0x%p', [Value]);
end;

function Stringify(Value: TClass): string;
begin
  if Value = nil then
    Result := 'nil'
  else
    Result := Value.ClassName;
end;

// Variant
function Stringify(Value: Variant): string;
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    Result := 'null'
  else
    case VarType(Value) of
      varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord, varInt64:
        Result := IntToStr(Value);
      varSingle, varDouble, varCurrency, varDate:
        Result := FloatToStr(Value);
      varBoolean:
        Result := BoolToStr(Value, True);
      varString, varOleStr, varUString:
        Result := Value;
      else
        Result := '[Неподдерживаемый тип Variant]';
    end;
end;

// Объекты
function StringifyObject(Obj: TObject): string;
var
  i: Integer;
  PropList: PPropList;
  PropCount: Integer;
  PropInfo: PPropInfo;
  ObjProp: TObject;
begin
  if Obj = nil then
    Exit('nil');
  
  Result := Obj.ClassName + ' {';
  
  PropCount := GetPropList(Obj.ClassInfo, tkAny, nil);
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(PPropInfo));
    try
      GetPropList(Obj.ClassInfo, tkAny, PropList);
      
      for i := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[i];
        
        if i > 0 then
          Result := Result + ', ';
        
        Result := Result + PropInfo^.Name + ': ';
        
        case PropInfo^.PropType^.Kind of
          tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
            Result := Result + Stringify(GetOrdProp(Obj, PropInfo));
          tkFloat:
            Result := Result + Stringify(GetFloatProp(Obj, PropInfo));
          tkString, tkLString, tkWString, tkUString:
            Result := Result + Stringify(GetStrProp(Obj, PropInfo));
          tkClass:
            begin
              // Избегаем циклических ссылок, просто выводим имя класса
              ObjProp := TObject(GetObjectProp(Obj, PropInfo));
              if ObjProp <> nil then
                Result := Result + ObjProp.ClassName
              else
                Result := Result + 'nil';
            end;
          tkMethod:
            Result := Result + 'Method';
          tkVariant:
            Result := Result + Stringify(GetVariantProp(Obj, PropInfo));
          tkArray, tkDynArray:
            Result := Result + 'Array';
          tkRecord:
            Result := Result + 'Record';
          tkInterface:
            Result := Result + 'Interface';
          tkPointer:
            Result := Result + Stringify(Pointer(GetOrdProp(Obj, PropInfo)));
          else
            Result := Result + '[Неподдерживаемый тип]';
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
  
  Result := Result + '}';
end;

// Перечисления
function StringifyEnum(Value: Integer; TypeInfo: PTypeInfo): string;
begin
  Result := GetEnumName(TypeInfo, Value);
end;

// Множества
function StringifySet(Value: Integer; TypeInfo: PTypeInfo): string;
var
  I: Integer;
  First: Boolean;
begin
  Result := '[';
  First := True;
  
  for I := 0 to 31 do
  begin
    if (Value and (1 shl I)) <> 0 then
    begin
      if not First then
        Result := Result + ', ';
      
      // Используем просто индекс, так как у нас нет доступа к именам элементов множества
      Result := Result + IntToStr(I);
      First := False;
    end;
  end;
  
  Result := Result + ']';
end;

// Массивы вариантов
function StringifyArr(v: array of Variant): string;
var
  i: Integer;
begin
  Result := '[';
  for i := 0 to Length(v) - 1 do
  begin
    if i > 0 then
      Result := Result + ', ';
    Result := Result + Stringify(v[i]);
  end;
  Result := Result + ']';
end;

function StringifyArrInt(v: TArrayInt): string;
var
  i: Integer;
begin
  Result := '[';
  for i := 0 to Length(v) - 1 do
  begin
    if i > 0 then
      Result := Result + ', ';
    Result := Result + Stringify(v[i]);
  end;
  Result := Result + ']';
end;

// Объекты
function Stringify(Value: TObject): string;
begin
  Result := StringifyObject(Value);
end;

// Функции логирования
function TLOG_LEVEL_Stirngify(ll: TLOG_LEVEL): string;
begin
  case ll of
    LL_DEBUG: Result := 'DEBUG';
    LL_INFO: Result := 'INFO';
    LL_WARN: Result := 'WARN';
    LL_ERR: Result := 'ERROR';
    else Result := 'UNKNOWN';
  end;
end;

procedure Log(level: TLOG_LEVEL; msg: string);
begin
  writeln(Format('[%s]: %s', [TLOG_LEVEL_Stirngify(level), msg]));
end;

{ no closures? ok }
procedure LogLNo(level: TLOG_LEVEL; msg: string);
begin end;

procedure LogLDebug(level: TLOG_LEVEL; msg: string);
begin if (level >= LL_DEBUG) then begin Log(level, msg) end; end;

procedure LogLInfo(level: TLOG_LEVEL; msg: string);
begin if (level >= LL_INFO) then begin Log(level, msg) end; end;

procedure LogLWarn(level: TLOG_LEVEL; msg: string);
begin if (level >= LL_WARN) then begin Log(level, msg) end; end;

procedure LogLErr(level: TLOG_LEVEL; msg: string);
begin if (level >= LL_ERR) then begin Log(level, msg) end; end;

function GetLogger(ll: TLOG_LEVEL): TLogF;
begin
  case ll of
    LL_DEBUG: Result := @LogLDebug;
    LL_INFO: Result := @LogLInfo;
    LL_WARN: Result := @LogLWarn;
    LL_ERR: Result := @LogLErr;
    LL_NO_LOGS: Result := @LogLNo;
  end;
end;

end.
