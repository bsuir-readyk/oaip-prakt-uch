unit e_component_ui;

interface

uses
  SysUtils, UiUtils, s_types, s_logging, s_utils, e_component_t, e_component_crud, e_global;

function stringifyComponent(p: PComponentNode): string;
function stringifyComponentRow(p: PComponentNode): string;
function stringifyComponentHeader(): string;
function stringifyComponentAll(): string;
function stringifyComponentAllTable(): string;
// returns -1 or id of existing row
function safeReadComponent(): integer;


implementation // ---------------------------------------------------------


function stringifyComponent(p: PComponentNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + 'Component' + #10#13;
        result := result + Format('id: %d' + #10#13, [p^.data.id]);
        result := result + Format('cTypeId: %d' + #10#13, [p^.data.cTypeId]);
        result := result + Format('manufacturer_name: %s' + #10#13, [p^.data.manufacturer_name]);
        result := result + Format('model_name: %s' + #10#13, [p^.data.model_name]);
        result := result + Format('description: %s' + #10#13, [p^.data.description]);
        result := result + Format('cost: %f' + #10#13, [p^.data.cost]);
        result := result + Format('availability: %d', [p^.data.availability]);
    end else begin
        result := '<nil>';
    end;
end;

function stringifyComponentRow(p: PComponentNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + Format('│ %5d', [p^.data.id]);
        result := result + Format(' │ %7d', [p^.data.cTypeId]);
        result := result + Format(' │ %30s', [p^.data.manufacturer_name]);
        result := result + Format(' │ %20s', [p^.data.model_name]);
        result := result + Format(' │ %30s', [p^.data.description]);
        result := result + Format(' │ %6.2f', [p^.data.cost]);
        result := result + Format(' │ %12d', [p^.data.availability]);
        result := result + ' │';
    end else begin
        result := result + Format('│ %5s', ['<nil>']);
        result := result + Format(' │ %7s', ['<nil>']);
        result := result + Format(' │ %30s', ['<nil>']);
        result := result + Format(' │ %20s', ['<nil>']);
        result := result + Format(' │ %30s', ['<nil>']);
        result := result + Format(' │ %6s', ['<nil>']);
        result := result + Format(' │ %12s', ['<nil>']);
        result := result + ' │';
    end;
end;

function stringifyComponentHeader(): string;
begin
    result := '';
    result := result + Format('│ %5s', ['id']);
    result := result + Format(' │ %7s', ['cTypeId']);
    result := result + Format(' │ %30s', ['manufacturer_name']);
    result := result + Format(' │ %20s', ['model_name']);
    result := result + Format(' │ %30s', ['description']);
    result := result + Format(' │ %6s', ['cost']);
    result := result + Format(' │ %12s', ['availability']);
    result := result + ' │';

    result := result + #10#13;

    result := result + Format('│%7s', [stringRepeat('-', 5+2)]);
    result := result + Format('│%9s', [stringRepeat('-', 7+2)]);
    result := result + Format('│%32s', [stringRepeat('-', 30+2)]);
    result := result + Format('│%22s', [stringRepeat('-', 20+2)]);
    result := result + Format('│%32s', [stringRepeat('-', 30+2)]);
    result := result + Format('│%8s', [stringRepeat('-', 6+2)]);
    result := result + Format('│%14s', [stringRepeat('-', 12+2)]);
    result := result + '│';
end;

function stringifyComponentAll(): string;
var
    node: PComponentNode;
begin
    result := '';
    result := result + #10#13;
    node := GData.Components;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyComponent(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function stringifyComponentAllTable(): string;
var
    node: PComponentNode;
begin
    result := '';
    result := result + stringifyComponentHeader();
    result := result + #10#13;
    node := GData.Components;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyComponentRow(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function safeReadComponent(): integer;
var
    id: integer;
    node: PComponentNode;
begin
    result := SafeReadInteger('Component id: ', 0, MAXINT);
    node := Component_read(result);
    while (node = nil) and (result <> -1) do
    begin
        WriteLn('Введен ID несуществующего Component.');
        WriteLn('Чтобы вернуться на предыдущую страницу введите "-1"');
        writeln;
        result := SafeReadInteger('Component id: ', -1, MAXINT);
        node := Component_read(result);
    end;
end;

end.
