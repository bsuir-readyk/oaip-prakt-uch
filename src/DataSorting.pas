unit DataSorting;

{$mode objfpc}{$H+}

interface

uses
  DataTypes,
  DynamicLists;

// Функции сортировки
procedure SortComponentTypesByCode(var List: TComponentTypeList);
procedure SortComponentsByPrice(var List: TComponentList);
procedure SortComponentsByCode(var List: TComponentList);
procedure SortComponentsByManufacturer(var List: TComponentList);
procedure SortOrdersByDate(var List: TOrderList);

implementation

procedure SortComponentTypesByCode(var List: TComponentTypeList);
var
  i, j: Integer;
  CurrentI: PComponentTypeNode;
  TempData: TComponentType;
begin
  if (List.Head = nil) or (List.Head^.Next = nil) then
    Exit; // Список пуст или содержит только один элемент
  
  // Пузырьковая сортировка для односвязного списка
  for i := 0 to List.Count - 2 do
  begin
    CurrentI := List.Head;
    
    for j := 0 to List.Count - 2 - i do
    begin
      if (CurrentI^.Next <> nil) and (CurrentI^.Data.TypeCode > CurrentI^.Next^.Data.TypeCode) then
      begin
        // Обмен данными между узлами
        TempData := CurrentI^.Data;
        CurrentI^.Data := CurrentI^.Next^.Data;
        CurrentI^.Next^.Data := TempData;
      end;
      
      CurrentI := CurrentI^.Next;
      
      if CurrentI = nil then
        Break;
    end;
  end;
end;

procedure SortComponentsByPrice(var List: TComponentList);
var
  i, j: Integer;
  CurrentI: PComponentNode;
  TempData: TComponent;
begin
  if (List.Head = nil) or (List.Head^.Next = nil) then
    Exit;
  
  for i := 0 to List.Count - 2 do
  begin
    CurrentI := List.Head;
    
    for j := 0 to List.Count - 2 - i do
    begin
      if (CurrentI^.Next <> nil) and (CurrentI^.Data.Price > CurrentI^.Next^.Data.Price) then
      begin
        TempData := CurrentI^.Data;
        CurrentI^.Data := CurrentI^.Next^.Data;
        CurrentI^.Next^.Data := TempData;
      end;
      
      CurrentI := CurrentI^.Next;
      if CurrentI = nil then
        Break;
    end;
  end;
end;

procedure SortComponentsByCode(var List: TComponentList);
var
  i, j: Integer;
  CurrentI: PComponentNode;
  TempData: TComponent;
begin
  if (List.Head = nil) or (List.Head^.Next = nil) then
    Exit;
  
  for i := 0 to List.Count - 2 do
  begin
    CurrentI := List.Head;
    
    for j := 0 to List.Count - 2 - i do
    begin
      if (CurrentI^.Next <> nil) and (CurrentI^.Data.Code > CurrentI^.Next^.Data.Code) then
      begin
        TempData := CurrentI^.Data;
        CurrentI^.Data := CurrentI^.Next^.Data;
        CurrentI^.Next^.Data := TempData;
      end;
      
      CurrentI := CurrentI^.Next;
      if CurrentI = nil then
        Break;
    end;
  end;
end;

procedure SortComponentsByManufacturer(var List: TComponentList);
var
  i, j: Integer;
  CurrentI: PComponentNode;
  TempData: TComponent;
begin
  if (List.Head = nil) or (List.Head^.Next = nil) then
    Exit;
  
  for i := 0 to List.Count - 2 do
  begin
    CurrentI := List.Head;
    
    for j := 0 to List.Count - 2 - i do
    begin
      if (CurrentI^.Next <> nil) and 
         (FixedToString(CurrentI^.Data.Manufacturer) > FixedToString(CurrentI^.Next^.Data.Manufacturer)) then
      begin
        TempData := CurrentI^.Data;
        CurrentI^.Data := CurrentI^.Next^.Data;
        CurrentI^.Next^.Data := TempData;
      end;
      
      CurrentI := CurrentI^.Next;
      if CurrentI = nil then
        Break;
    end;
  end;
end;

procedure SortOrdersByDate(var List: TOrderList);
var
  i, j: Integer;
  CurrentI: POrderNode;
  TempData: TOrder;
begin
  if (List.Head = nil) or (List.Head^.Next = nil) then
    Exit;
  
  for i := 0 to List.Count - 2 do
  begin
    CurrentI := List.Head;
    
    for j := 0 to List.Count - 2 - i do
    begin
      if (CurrentI^.Next <> nil) and (CurrentI^.Data.OrderDate > CurrentI^.Next^.Data.OrderDate) then
      begin
        TempData := CurrentI^.Data;
        CurrentI^.Data := CurrentI^.Next^.Data;
        CurrentI^.Next^.Data := TempData;
      end;
      
      CurrentI := CurrentI^.Next;
      if CurrentI = nil then
        Break;
    end;
  end;
end;

end. 
