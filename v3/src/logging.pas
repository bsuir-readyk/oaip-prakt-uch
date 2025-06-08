unit logging;

interface

uses
  SysUtils;

type 
  TLOG_LEVEL = (LL_NO_LOGS, LL_DEBUG, LL_INFO, LL_WARN, LL_ERR);
  TLogF = procedure(level: TLOG_LEVEL; msg: string);

// Логирование
function TLOG_LEVEL_Stirngify(ll: TLOG_LEVEL): string;
procedure Log(level: TLOG_LEVEL; msg: string);

procedure LogLNo(level: TLOG_LEVEL; msg: string);
procedure LogLDebug(level: TLOG_LEVEL; msg: string);
procedure LogLInfo(level: TLOG_LEVEL; msg: string);
procedure LogLWarn(level: TLOG_LEVEL; msg: string);
procedure LogLErr(level: TLOG_LEVEL; msg: string);

function GetLogger(ll: TLOG_LEVEL): TLogF;

// Базовые функции для преобразования типов в строку
function Stringify(Value: Integer): string; overload;
function Stringify(Value: Int64): string; overload;
function Stringify(Value: Boolean): string; overload;
function Stringify(Value: Double): string; overload;
function Stringify(Value: string): string; overload;

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
