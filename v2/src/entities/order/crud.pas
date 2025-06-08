unit e_Order_crud;

interface

uses
  SysUtils, s_types, s_logging, e_Order_t, e_global;

function Order_create(
    items: TArrayInt;
    orderDate: Integer;
    customerName: string;
    customerPhone: string;
    customerEmail: string;
    status: string
): POrderNode;

function Order_read(
    id: Integer
): POrderNode;

// function Order_update(
//     id: Integer;
//     new_data: TOrder
// ): POrderNode;

// returns pointer to previous
function Order_delete(
    id: Integer
): POrderNode;

implementation

type
  TFileOrder = record // For direct file I/O, excluding dynamic array
    id: Integer;
    orderDate: Integer;
    customerName: TFixedString;
    customerPhone: TFixedString;
    customerEmail: TFixedString;
    status: TFixedString;
  end;

function Order_create(
    items: TArrayInt;
    orderDate: Integer;
    customerName: string;
    customerPhone: string;
    customerEmail: string;
    status: string
): POrderNode;
var
    node: POrderNode;
    data: TOrder;
begin
    data.id := GData.NewOrderId;
    data.items := items;
    data.orderDate := orderDate;
    data.customerName := customerName;
    data.customerPhone := customerPhone;
    data.customerEmail := customerEmail;
    data.status := status;

    Inc(GData.NewOrderId);

    node := new(POrderNode);
    node^.data := data;
    node^.next := nil;

    result := GData.Orders;
    while result^.next <> nil do begin
        result := result^.next;
    end;

    result^.next := node;

    result := node;
end;

function Order_read(
    id: Integer
): POrderNode;
var
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    
    result := GData.orders;
    if result^.next = nil then begin
        log(LL_DEBUG, format('{order_read} Not found order with id __%d__', [id]));
        result := nil;
    end else begin
        repeat
            result := result^.next;
        until (result^.next = nil) or (result^.data.id = id);

        if (result^.next = nil) and (result^.data.id <> id) then begin
            log(LL_DEBUG, format('{order_read} Not found order with id __%d__', [id]));
            result := nil;
        end;
    end;
end;

// function Order_update(
//     id: Integer;
//     new_data: TOrder
// ): POrderNode;
// begin
//     result := Order_read(id);
//     if (result <> nil) then begin
//         result^.data := data;
//     end;
// end;

function Order_delete(
    id: Integer
): POrderNode;
var
    prev: POrderNode;
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    // log := GetLogger(LL_DEBUG);
    result := GData.Orders;
    prev := GData.Orders;
    repeat
        prev := result;
        result := result^.next;
    until (result^.next = nil) or (result^.data.id = id);

    if (result^.next = nil) and (result^.data.id <> id) then begin
        log(LL_DEBUG, format('Not found Order with id __%d__', [id]));
        result := nil;
    end else begin
        prev^.next := result^.next;
        dispose(result);
        result := prev;
    end;
end;

end.
