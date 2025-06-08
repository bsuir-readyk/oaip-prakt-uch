unit e_componenttype_crud;

interface

uses
  SysUtils, s_types, s_logging, e_componenttype_t, e_global;

function componentType_create(
    name: TFixedString
): PComponentTypeNode;

function componentType_read(
    id: Integer
): PComponentTypeNode;

// function componentType_update(
//     id: Integer;
//     new_data: TComponentType
// ): PComponentTypeNode;

// returns pointer to previous
function componentType_delete(
    id: Integer
): PComponentTypeNode;

implementation

function componentType_create(
    name: TFixedString
): PComponentTypeNode;
var
    node: PComponentTypeNode;
    data: TComponentType;
begin
    data.id := GData.NewComponentTypeId;
    data.name := name;
    
    Inc(GData.NewComponentTypeId);

    node := new(PComponentTypeNode);
    node^.data := data;
    node^.next := nil;

    result := GData.ComponentTypes;
    while result^.next <> nil do begin
        result := result^.next;
    end;

    result^.next := node;

    result := node;
end;



function componentType_read(
    id: Integer
): PComponentTypeNode;
var
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    
    result := GData.ComponentTypes;
    if result^.next = nil then begin
        log(LL_DEBUG, format('{componentType_read} Not found componentType with id __%d__', [id]));
        result := nil;
    end else begin
        repeat
            result := result^.next;
        until (result^.next = nil) or (result^.data.id = id);

        if (result^.next = nil) and (result^.data.id <> id) then begin
            log(LL_DEBUG, format('{componentType_read} Not found componentType with id __%d__', [id]));
            result := nil;
        end;
    end;
end;

// function componentType_update(
//     id: Integer;
//     new_data: TComponentType
// ): PComponentTypeNode;
// begin
//     result := componentType_read(id);
//     if (result <> nil) then begin
//         result^.data := data;
//     end;
// end;

function componentType_delete(
    id: Integer
): PComponentTypeNode;
var
    prev: PComponentTypeNode;
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    log := GetLogger(LL_DEBUG);
    result := GData.ComponentTypes;
    prev := GData.ComponentTypes;
    repeat
        prev := result;
        result := result^.next;
    until (result^.next = nil) or (result^.data.id = id);
    
    if (result^.next = nil) and (result^.data.id <> id) then begin
        log(LL_DEBUG, format('Not found componentType with id __%d__', [id]));
        result := nil;
    end else begin
        prev^.next := result^.next;
        dispose(result);
        result := prev;
    end;
end;

end.
