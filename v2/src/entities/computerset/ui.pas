unit e_computerSet_ui;

interface

uses
  SysUtils, UiUtils, s_types, s_logging, s_utils, e_computerSet_t, e_computerSet_crud, e_global;

function stringifyComputerSet(p: PComputerSetNode): string;
function stringifyComputerSetRow(p: PComputerSetNode): string;
function stringifyComputerSetHeader(): string;
function stringifyComputerSetAll(): string;
function stringifyComputerSetAllTable(): string;
// returns -1 or id of existing row
function safeReadComputerSet(): integer;

implementation // ---------------------------------------------------------


function stringifyComputerSet(p: PComputerSetNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + 'ComputerSet' + #10#13;
        result := result + Format('id: %d' + #10#13, [p^.data.id]);
        result := result + Format('cpu_id: %d' + #10#13, [p^.data.cpu_id]);
        result := result + Format('motherboard_id: %d' + #10#13, [p^.data.motherboard_id]);
        result := result + Format('ram_id: %d' + #10#13, [p^.data.ram_id]);
        result := result + Format('psu_id: %d' + #10#13, [p^.data.psu_id]);
        result := result + Format('storage_id: %d' + #10#13, [p^.data.storage_id]);
        result := result + Format('totalCost: %.2f' + #10#13, [p^.data.totalCost]);
    end else begin
        result := '<nil>';
    end;
end;

function stringifyComputerSetRow(p: PComputerSetNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + Format('│ %5d', [p^.data.id]);
        result := result + Format(' │ %6d', [p^.data.cpu_id]);
        result := result + Format(' │ %14d', [p^.data.motherboard_id]);
        result := result + Format(' │ %6d', [p^.data.ram_id]);
        result := result + Format(' │ %6d', [p^.data.psu_id]);
        result := result + Format(' │ %10d', [p^.data.storage_id]);
        result := result + Format(' │ %9.2f', [p^.data.totalCost]);
        result := result + ' │';
    end else begin
        result := result + Format('│ %5s', ['<nil>']);
        result := result + Format(' │ %6s', ['<nil>']);
        result := result + Format(' │ %14s', ['<nil>']);
        result := result + Format(' │ %6s', ['<nil>']);
        result := result + Format(' │ %6s', ['<nil>']);
        result := result + Format(' │ %10s', ['<nil>']);
        result := result + Format(' │ %9s', ['<nil>']);
        result := result + ' │';
    end;
end;

function stringifyComputerSetHeader(): string;
var i: Integer;
begin
    result := '';
    result := result + Format('│ %5s', ['id']);
    result := result + Format(' │ %6s', ['cpu_id']);
    result := result + Format(' │ %14s', ['motherboard_id']);
    result := result + Format(' │ %6s', ['ram_id']);
    result := result + Format(' │ %6s', ['psu_id']);
    result := result + Format(' │ %10s', ['storage_id']);
    result := result + Format(' │ %9s', ['totalCost']);
    result := result + ' │';

    result := result + #10#13;

    result := result + Format('│%7s', [stringRepeat('-', 5+2)]);
    result := result + Format('│%8s', [stringRepeat('-', 6+2)]);
    result := result + Format('│%16s', [stringRepeat('-', 14+2)]);
    result := result + Format('│%8s', [stringRepeat('-', 6+2)]);
    result := result + Format('│%8s', [stringRepeat('-', 6+2)]);
    result := result + Format('│%12s', [stringRepeat('-', 10+2)]);
    result := result + Format('│%11s', [stringRepeat('-', 9+2)]);
    result := result + '│';
end;

function stringifyComputerSetAll(): string;
var
    node: PComputerSetNode;
begin
    result := '';
    result := result + #10#13;
    node := GData.ComputerSets;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyComputerSet(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function stringifyComputerSetAllTable(): string;
var
    node: PComputerSetNode;
begin
    result := '';
    result := result + stringifyComputerSetHeader();
    result := result + #10#13;
    node := GData.ComputerSets;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyComputerSetRow(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function safeReadComputerSet(): integer;
var
    id: integer;
    node: PComputerSetNode;
begin
    result := SafeReadInteger('ComputerSet id: ', 0, MAXINT);
    node := ComputerSet_read(result);
    while (node = nil) and (result <> -1) do
    begin
        WriteLn('Введен ID несуществующего типа компонента.');
        WriteLn('Чтобы вернуться на предыдущую страницу введите "-1"');
        writeln;
        result := SafeReadInteger('ComputerSet id: ', -1, MAXINT);
        node := ComputerSet_read(result);
    end;
end;

end.
