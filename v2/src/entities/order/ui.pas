unit e_Order_ui;

interface

uses
  SysUtils, UiUtils, s_types, s_logging, s_utils, e_Order_t, e_Order_crud, e_global;

function stringifyOrder(p: POrderNode): string;
function stringifyOrderRow(p: POrderNode): string;
function stringifyOrderHeader(): string;
function stringifyOrderAll(): string;
function stringifyOrderAllTable(): string;
// returns -1 or id of existing row
function safeReadOrder(): integer;

implementation // ---------------------------------------------------------


function stringifyOrder(p: POrderNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + 'Order' + #10#13;
        result := result + Format('id: %d' + #10#13, [p^.data.id]);
        result := result + Format('items: %s' + #10#13, [StringifyArrInt(p^.data.items)]);
        result := result + Format('orderDate: %d' + #10#13, [p^.data.orderDate]);
        result := result + Format('customerName: %s' + #10#13, [p^.data.customerName]);
        result := result + Format('customerPhone: %s' + #10#13, [p^.data.customerPhone]);
        result := result + Format('customerEmail: %s' + #10#13, [p^.data.customerEmail]);
        result := result + Format('status: %s' + #10#13, [p^.data.status]);
    end else begin
        result := '<nil>';
    end;
end;

function stringifyOrderRow(p: POrderNode): string;
begin
    result := '';
    if (p <> nil) then begin
        result := result + Format('│ %5d', [p^.data.id]);
        result := result + Format(' │ %15s', [StringifyArrInt(p^.data.items)]);
        result := result + Format(' │ %15s', [StringifyArrInt(p^.data.items)]);
        result := result + Format(' │ %9d', [p^.data.orderDate]);
        result := result + Format(' │ %30s', [p^.data.customerName]);
        result := result + Format(' │ %20s', [p^.data.customerPhone]);
        result := result + Format(' │ %25s', [p^.data.customerEmail]);
        result := result + Format(' │ %10s', [p^.data.status]);
        result := result + ' │';
    end else begin
        result := result + Format('│ %5s', ['<nil>']);
        result := result + Format(' │ %15s', ['<nil>']);
        result := result + Format(' │ %15s', ['<nil>']);
        result := result + Format(' │ %9s', ['<nil>']);
        result := result + Format(' │ %30s', ['<nil>']);
        result := result + Format(' │ %20s', ['<nil>']);
        result := result + Format(' │ %25s', ['<nil>']);
        result := result + Format(' │ %10s', ['<nil>']);
        result := result + ' │';
    end;
end;

function stringifyOrderHeader(): string;
var i: Integer;
begin
    result := '';
    result := result + Format('│ %5s', ['id']);
    result := result + Format(' │ %15s', ['Comp. ID']);
    result := result + Format(' │ %15s', ['Count']);
    result := result + Format(' │ %9s', ['orderDate']);
    result := result + Format(' │ %20s', ['customerName']);
    result := result + Format(' │ %20s', ['customerPhone']);
    result := result + Format(' │ %20s', ['customerEmail']);
    result := result + Format(' │ %10s', ['status']);
    result := result + ' │';

    result := result + #10#13;

    result := result + Format('│%7s', [stringRepeat('-', 5+2)]);
    result := result + Format('│%17s', [stringRepeat('-', 15+2)]);
    result := result + Format('│%17s', [stringRepeat('-', 15+2)]);
    result := result + Format('│%11s', [stringRepeat('-', 9+2)]);
    result := result + Format('│%32s', [stringRepeat('-', 30+2)]);
    result := result + Format('│%22s', [stringRepeat('-', 20+2)]);
    result := result + Format('│%27s', [stringRepeat('-', 25+2)]);
    result := result + Format('│%12s', [stringRepeat('-', 10+2)]);
    result := result + '│';
end;

function stringifyOrderAll(): string;
var
    node: POrderNode;
begin
    result := '';
    result := result + #10#13;
    node := GData.Orders;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyOrder(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function stringifyOrderAllTable(): string;
var
    node: POrderNode;
begin
    result := '';
    result := result + stringifyOrderHeader();
    result := result + #10#13;
    node := GData.Orders;
    if node^.next <> nil then begin
        repeat
            node := node^.next;
            result := result + stringifyOrderRow(node);
            result := result + #10#13;
        until node^.next = nil;
        result := copy(result, 1, Length(result)-2);
    end else begin
        result := result + '<Empty>';
    end;
end;

function safeReadOrder(): integer;
var
    id: integer;
    node: POrderNode;
begin
    result := SafeReadInteger('Order id: ', 0, MAXINT);
    node := Order_read(result);
    while (node = nil) and (result <> -1) do
    begin
        WriteLn('Введен ID несуществующего типа компонента.');
        WriteLn('Чтобы вернуться на предыдущую страницу введите "-1"');
        writeln;
        result := SafeReadInteger('Order id: ', -1, MAXINT);
        node := Order_read(result);
    end;
end;

end.
