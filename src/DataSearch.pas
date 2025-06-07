unit DataSearch;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  DataTypes,
  DynamicLists;

// Функции поиска
procedure SearchComponentsByPriceRange(const SourceList: TComponentList; MinPrice, MaxPrice: Real; var ResultList: TComponentList);
procedure SearchComponentsByManufacturer(const SourceList: TComponentList; const Manufacturer: string; var ResultList: TComponentList);
procedure SearchComponentsByType(const SourceList: TComponentList; TypeCode: Integer; var ResultList: TComponentList);
procedure SearchComponentsByModel(const SourceList: TComponentList; const Model: string; var ResultList: TComponentList);
procedure SearchComponentsByMinStock(const SourceList: TComponentList; MinStock: Integer; var ResultList: TComponentList);

implementation

// Функции поиска
procedure SearchComponentsByPriceRange(const SourceList: TComponentList; MinPrice, MaxPrice: Real; var ResultList: TComponentList);
var
  Current: PComponentNode;
begin
  Current := SourceList.Head;
  
  while Current <> nil do
  begin
    if (Current^.Data.Price >= MinPrice) and (Current^.Data.Price <= MaxPrice) then
      AddComponent(ResultList, Current^.Data);
    
    Current := Current^.Next;
  end;
end;

procedure SearchComponentsByManufacturer(const SourceList: TComponentList; const Manufacturer: string; var ResultList: TComponentList);
var
  Current: PComponentNode;
  CurrentManufacturer: string;
begin
  Current := SourceList.Head;
  
  while Current <> nil do
  begin
    CurrentManufacturer := FixedToString(Current^.Data.Manufacturer);
    if Pos(LowerCase(Manufacturer), LowerCase(CurrentManufacturer)) > 0 then
      AddComponent(ResultList, Current^.Data);
    
    Current := Current^.Next;
  end;
end;

procedure SearchComponentsByType(const SourceList: TComponentList; TypeCode: Integer; var ResultList: TComponentList);
var
  Current: PComponentNode;
begin
  Current := SourceList.Head;
  
  while Current <> nil do
  begin
    if Current^.Data.TypeCode = TypeCode then
      AddComponent(ResultList, Current^.Data);
    
    Current := Current^.Next;
  end;
end;

procedure SearchComponentsByModel(const SourceList: TComponentList; const Model: string; var ResultList: TComponentList);
var
  Current: PComponentNode;
  CurrentModel: string;
begin
  Current := SourceList.Head;
  
  while Current <> nil do
  begin
    CurrentModel := FixedToString(Current^.Data.Model);
    if Pos(LowerCase(Model), LowerCase(CurrentModel)) > 0 then
      AddComponent(ResultList, Current^.Data);
    
    Current := Current^.Next;
  end;
end;

procedure SearchComponentsByMinStock(const SourceList: TComponentList; MinStock: Integer; var ResultList: TComponentList);
var
  Current: PComponentNode;
begin
  Current := SourceList.Head;
  
  while Current <> nil do
  begin
    if Current^.Data.InStock >= MinStock then
      AddComponent(ResultList, Current^.Data);
    
    Current := Current^.Next;
  end;
end;

end. 
