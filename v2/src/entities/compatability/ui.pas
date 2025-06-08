unit e_Compatability_ui;


interface


uses
  SysUtils, UiUtils, s_types, s_logging, s_utils, e_Compatability_t, e_Compatability_crud, e_global;

function stringifyCompatability(p: PCompatabilityNode): string;
function stringifyCompatabilityRow(p: PCompatabilityNode): string;
function stringifyCompatabilityHeader(): string;
function stringifyCompatabilityAll(): string;
function stringifyCompatabilityAllTable(): string;
// returns -1 or id of existing row
function safeReadCompatability(): integer;


implementation // ---------------------------------------------------------


function stringifyCompatability(p: PCompatabilityNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + 'Compatability' + #10#13;
        result := result + Format('left_id: %d' + #10#13, [p^.data.left_id]);
        result := result + Format('right_id: %d' + #10#13, [p^.data.right_id]);
    end else begin
        result := '<nil>';
    end;
end;

function stringifyCompatabilityRow(p: PCompatabilityNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + Format('│ %7d', [p^.data.left_id]);
        result := result + Format(' │ %8d', [p^.data.right_id]);
        result := result + ' │';
    end else begin
        result := result + Format('│ %7s', ['<nil>']);
        result := result + Format(' │ %8s', ['<nil>']);
        result := result + ' │';
    end;
end;

function stringifyCompatabilityHeader(): string;
begin
    result := '';
    result := result + Format('│ %7s', ['left_id']);
    result := result + Format(' │ %8s', ['right_id']);
    result := result + ' │';

    result := result + #10#13;

    result := result + Format('│%9s', [stringRepeat('-', 7+2)]);
    result := result + Format('│%10s', [stringRepeat('-', 7+2)]);
    result := result + '│';
end;

function stringifyCompatabilityAll(): string;
var
    node: PCompatabilityNode;
begin
    result := '';
    result := result + #10#13;
    node := GData.Compatabilities;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyCompatability(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function stringifyCompatabilityAllTable(): string;
var
    node: PCompatabilityNode;
begin
    result := '';
    result := result + stringifyCompatabilityHeader();
    result := result + #10#13;
    node := GData.Compatabilities;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyCompatabilityRow(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function safeReadCompatability(): integer;
var
    id: integer;
    node: TAPCompatabilityNode;
begin
    result := SafeReadInteger('Compatability left_id: ', 0, MAXINT);
    node := Compatability_read(result);
    while (node = nil) and (result <> -1) do
    begin
        WriteLn('Введен LEFT_ID несуществующего compatability.');
        WriteLn('Чтобы вернуться на предыдущую страницу введите "-1"');
        writeln;
        result := SafeReadInteger('Compatability left_id: ', -1, MAXINT);
        node := Compatability_read(result);
    end;
end;

end.
