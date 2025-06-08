unit e_component_crud;

interface

uses
  SysUtils, s_types, s_logging, e_component_t, e_global;

function component_create(
    cTypeId: Integer;
    manufacturer_name: TFixedString;
    model_name: TFixedString;
    description: TFixedString;
    cost: Real;
    availability: Integer
): PComponentNode;

function component_read(
    id: Integer
): PComponentNode;

// function component_update(
//     id: Integer;
//     new_data: TComponent
// ): PComponentNode;

// returns pointer to previous
function component_delete(
    id: Integer
): PComponentNode;

implementation // ---------------------------------------------------------

function component_create(
    cTypeId: Integer;
    manufacturer_name: TFixedString;
    model_name: TFixedString;
    description: TFixedString;
    cost: Real;
    availability: Integer
): PComponentNode;
var
    node: PComponentNode;
    data: TComponent;
begin
    data.id := GData.NewComponentId;
    data.cTypeId := cTypeId;
    data.manufacturer_name := manufacturer_name;
    data.model_name := model_name;
    data.description := description;
    data.cost := cost;
    data.availability := availability;

    Inc(GData.NewComponentId);

    node := new(PComponentNode);
    node^.data := data;
    node^.next := nil;

    result := GData.Components;
    while result^.next <> nil do begin
        result := result^.next;
    end;

    result^.next := node;

    result := node;
end;



function component_read(
    id: Integer
): PComponentNode;
var
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    
    result := GData.components;
    if result^.next = nil then begin
        log(LL_DEBUG, format('{component_read} Not found component with id __%d__', [id]));
        result := nil;
    end else begin
        repeat
            result := result^.next;
        until (result^.next = nil) or (result^.data.id = id);

        if (result^.next = nil) and (result^.data.id <> id) then begin
            log(LL_DEBUG, format('{component_read} Not found component with id __%d__', [id]));
            result := nil;
        end;
    end;
end;

// function component_update(
//     id: Integer;
//     new_data: TComponent
// ): PComponentNode;
// begin
//     result := component_read(id);
//     if (result <> nil) then begin
//         result^.data := data;
//     end;
// end;

function component_delete(
    id: Integer
): PComponentNode;
var
    prev: PComponentNode;
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    // log := GetLogger(LL_DEBUG);
    result := GData.Components;
    prev := GData.Components;
    repeat
        prev := result;
        result := result^.next;
    until (result^.next = nil) or (result^.data.id = id);

    if (result^.next = nil) and (result^.data.id <> id) then begin
        log(LL_DEBUG, format('Not found component with id __%d__', [id]));
        result := nil;
    end else begin
        prev^.next := result^.next;
        dispose(result);
        result := prev;
    end;
end;

end.
