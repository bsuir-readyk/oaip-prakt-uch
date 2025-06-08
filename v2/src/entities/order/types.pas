unit e_Order_t;

interface

uses
  SysUtils, s_types;

type
  POrder = ^TOrder;
  TOrder = record
    id: Integer;
    items: TArrayInt; // odd numbers are component ids, even numbers are component ids
    orderDate: Integer;
    customerName: TFixedString;
    customerPhone: TFixedString;
    customerEmail: TFixedString;
    status: TFixedString; // 'new', 'processing', 'completed', 'cancelled'
  end;
  
  
  POrderNode = ^TOrderNode;
  TOrderNode = record
    data: TOrder;
    next: POrderNode;
  end;

implementation

end.
