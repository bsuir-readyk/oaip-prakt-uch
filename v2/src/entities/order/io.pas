unit e_order_io;

interface

uses
  SysUtils, Classes, Crt, e_global, e_order_t, s_types, s_logging;

procedure DisposeOrderNodes(AListHead: POrderNode);
procedure LoadOrdersFromDatFile(const AFileName: string; var AOrdersList: POrderNode; var ANextId: Integer);
procedure SaveOrdersToDatFile(const AFileName: string; const AOrdersList: POrderNode);

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

procedure DisposeOrderNodes(AListHead: POrderNode);
var
  CurrentNode, NextNode: POrderNode;
begin
  if AListHead = nil then Exit;
  CurrentNode := AListHead^.next;
  while CurrentNode <> nil do
  begin
    NextNode := CurrentNode^.next;
    SetLength(CurrentNode^.data.items, 0); // Release memory for the dynamic array
    dispose(CurrentNode);
    CurrentNode := NextNode;
  end;
  AListHead^.next := nil;
end;

procedure SaveOrdersToDatFile(const AFileName: string; const AOrdersList: POrderNode);
var
  F: File; // Untyped file for more control
  FileRec: TFileOrder;
  CurrentNode: POrderNode;
  log: TLogF;
  itemCount: Integer;
  i: Integer;
  item: Integer;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Saving Orders to ' + AFileName);
  AssignFile(F, AFileName);
  try
    Rewrite(F, 1); // Open as untyped, record size 1 for byte-level operations
    CurrentNode := AOrdersList;
    if CurrentNode <> nil then
    begin
        CurrentNode := CurrentNode^.next; // Skip dummy head
    end;

    while CurrentNode <> nil do
    begin
      // 1. Prepare FileRec (fixed part of TOrder)
      FileRec.id := CurrentNode^.data.id;
      FileRec.orderDate := CurrentNode^.data.orderDate;
      FileRec.customerName := CurrentNode^.data.customerName;
      FileRec.customerPhone := CurrentNode^.data.customerPhone;
      FileRec.customerEmail := CurrentNode^.data.customerEmail;
      FileRec.status := CurrentNode^.data.status;
      BlockWrite(F, FileRec, SizeOf(FileRec)); // Write the fixed part

      // 2. Write item count
      itemCount := Length(CurrentNode^.data.items);
      BlockWrite(F, itemCount, SizeOf(Integer));

      // 3. Write items
      for i := 0 to itemCount - 1 do
      begin
        item := CurrentNode^.data.items[i];
        BlockWrite(F, item, SizeOf(Integer));
      end;
      CurrentNode := CurrentNode^.next;
    end;
    log(LL_INFO, 'Orders saved successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error saving Orders: ' + E.Message);
  end;
  CloseFile(F);
end;

procedure LoadOrdersFromDatFile(const AFileName: string; var AOrdersList: POrderNode; var ANextId: Integer);
var
  F: File;
  FileRec: TFileOrder;
  TempOrder: TOrder;
  NewNode, LastNode: POrderNode;
  log: TLogF;
  MaxId: Integer;
  itemCount: Integer;
  i: Integer;
  item: Integer;
  BytesRead: LongInt;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Loading Orders from ' + AFileName);

  if AOrdersList = nil then
  begin
    AOrdersList := new(POrderNode);
    AOrdersList^.next := nil;
  end
  else
  begin
    DisposeOrderNodes(AOrdersList);
  end;
  
  LastNode := AOrdersList;
  MaxId := -1;
  ANextId := 0;

  if not FileExists(AFileName) then
  begin
    log(LL_WARN, 'File ' + AFileName + ' not found. No Orders loaded.');
    Exit;
  end;

  AssignFile(F, AFileName);
  try
    Reset(F, 1); // Open as untyped, record size 1
    if System.Eof(F) then // Use System.Eof for untyped files with recsize 1 initially
    begin
      log(LL_INFO, 'File ' + AFileName + ' is empty. No Orders loaded.');
      CloseFile(F);
      Exit;
    end;

    while not System.Eof(F) do
    begin
      BlockRead(F, FileRec, SizeOf(FileRec), BytesRead);
      if BytesRead < SizeOf(FileRec) then Break; // Incomplete record at EOF
      
      TempOrder.id := FileRec.id;
      TempOrder.orderDate := FileRec.orderDate;
      TempOrder.customerName := FileRec.customerName;
      TempOrder.customerPhone := FileRec.customerPhone;
      TempOrder.customerEmail := FileRec.customerEmail;
      TempOrder.status := FileRec.status;

      BlockRead(F, itemCount, SizeOf(Integer), BytesRead);
      if BytesRead < SizeOf(Integer) then begin log(LL_ERR, 'Unexpected EOF reading order items count'); Break; end;
      
      SetLength(TempOrder.items, itemCount);
      for i := 0 to itemCount - 1 do
      begin
        BlockRead(F, item, SizeOf(Integer), BytesRead);
        if BytesRead < SizeOf(Integer) then begin log(LL_ERR, 'Unexpected EOF reading order item ' + IntToStr(i)); SetLength(TempOrder.items, i); Break; end;
        TempOrder.items[i] := item;
      end;

      NewNode := new(POrderNode);
      NewNode^.data := TempOrder;
      NewNode^.next := nil;
      
      LastNode^.next := NewNode;
      LastNode := NewNode;

      if TempOrder.id > MaxId then
        MaxId := TempOrder.id;
    end;
    log(LL_INFO, 'Orders loaded successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error loading Orders: ' + E.Message + ' at pos ' + IntToStr(FilePos(F)));
  end;
  CloseFile(F);
  ANextId := MaxId + 1;
end;

end. 
