unit e_componentType_ui;

interface

uses
  SysUtils, UiUtils, s_types, s_logging, s_utils, e_componentType_t, e_componentType_crud, e_global;

function stringifyComponentType(p: PComponentTypeNode): string;
function stringifyComponentTypeRow(p: PComponentTypeNode): string;
function stringifyComponentTypeHeader(): string;
function stringifyComponentTypeAll(): string;
function stringifyComponentTypeAllTable(): string;
// returns -1 or id of existing row
function safeReadComponentType(): integer;


implementation // ---------------------------------------------------------


function stringifyComponentType(p: PComponentTypeNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + 'ComponentType' + #10#13;
        result := result + Format('id: %d' + #10#13, [p^.data.id]);
        result := result + Format('name: %s' + #10#13, [p^.data.name]);
    end else begin
        result := '<nil>';
    end;
end;

function stringifyComponentTypeRow(p: PComponentTypeNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + Format('│ %5d', [p^.data.id]);
        result := result + Format(' │ %20s', [p^.data.name]);
        result := result + ' │';
    end else begin
        result := result + Format('│ %5s', ['<nil>']);
        result := result + Format(' │ %20s', ['<nil>']);
        result := result + ' │';
    end;
end;

function stringifyComponentTypeHeader(): string;
var i: Integer;
begin
    result := '';
    result := result + Format('│ %5s', ['id']);
    result := result + Format(' │ %20s', ['name']);
    result := result + ' │';

    result := result + #10#13;

    result := result + Format('│%7s', [stringRepeat('-', 5+2)]);
    result := result + Format('│%22s', [stringRepeat('-', 20+2)]);
    result := result + '│';
end;

function stringifyComponentTypeAll(): string;
var
    node: PComponentTypeNode;
begin
    result := '';
    node := GData.ComponentTypes;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyComponentType(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function stringifyComponentTypeAllTable(): string;
var
    node: PComponentTypeNode;
begin
    result := '';
    result := result + stringifyComponentTypeHeader();
    result := result + #10#13;
    node := GData.ComponentTypes;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyComponentTypeRow(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function safeReadComponentType(): integer;
var
    id: integer;
    node: PComponentTypeNode;
begin
    result := SafeReadInteger('Component type id: ', 0, MAXINT);
    node := componentType_read(result);
    while (node = nil) and (result <> -1) do
    begin
        WriteLn('Введен ID несуществующего типа компонента.');
        WriteLn('Чтобы вернуться на предыдущую страницу введите "-1"');
        writeln;
        result := SafeReadInteger('Component type id: ', -1, MAXINT);
        node := componentType_read(result);
    end;
end;

end.
