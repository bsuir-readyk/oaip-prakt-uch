unit e_computerset_crud;

interface

uses
  SysUtils, s_types, s_logging, e_computerset_t, e_global;

function computerset_create(
    cpu_id: Integer;
    motherboard_id: Integer;
    ram_id: Integer;
    psu_id: Integer;
    storage_id: Integer;
    totalCost: Real
): PComputersetNode;

function computerset_read(
    id: Integer
): PComputersetNode;

// function computerset_update(
//     id: Integer;
//     new_data: Tcomputerset
// ): PComputersetNode;

// returns pointer to previous
function computerset_delete(
    id: Integer
): PComputersetNode;

implementation

function computerset_create(
    cpu_id: Integer;
    motherboard_id: Integer;
    ram_id: Integer;
    psu_id: Integer;
    storage_id: Integer;
    totalCost: Real
): PComputersetNode;
var
    node: PComputersetNode;
    data: Tcomputerset;
begin
    data.id := GData.NewcomputersetId;
    data.cpu_id := cpu_id;
    data.motherboard_id := motherboard_id;
    data.ram_id := ram_id;
    data.psu_id := psu_id;
    data.storage_id := storage_id;
    data.totalCost := totalCost;

    Inc(GData.NewcomputersetId);

    node := new(PComputersetNode);
    node^.data := data;
    node^.next := nil;

    result := GData.computersets;
    while result^.next <> nil do begin
        result := result^.next;
    end;

    result^.next := node;

    result := node;
end;

function computerset_read(
    id: Integer
): PComputersetNode;
var
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    
    result := GData.computersets;
    if result^.next = nil then begin
        log(LL_DEBUG, format('{computerset_read} Not found computerset with id __%d__', [id]));
        result := nil;
    end else begin
        repeat
            result := result^.next;
        until (result^.next = nil) or (result^.data.id = id);

        if (result^.next = nil) and (result^.data.id <> id) then begin
            log(LL_DEBUG, format('{computerset_read} Not found computerset with id __%d__', [id]));
            result := nil;
        end;
    end;
end;

// function computerset_update(
//     id: Integer;
//     new_data: Tcomputerset
// ): PComputersetNode;
// begin
//     result := computerset_read(id);
//     if (result <> nil) then begin
//         result^.data := data;
//     end;
// end;

function computerset_delete(
    id: Integer
): PComputersetNode;
var
    prev: PComputersetNode;
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    // log := GetLogger(LL_DEBUG);
    result := GData.computersets;
    prev := GData.computersets;
    repeat
        prev := result;
        result := result^.next;
    until (result^.next = nil) or (result^.data.id = id);

    if (result^.next = nil) and (result^.data.id <> id) then begin
        log(LL_DEBUG, format('Not found computerset with id __%d__', [id]));
        result := nil;
    end else begin
        prev^.next := result^.next;
        dispose(result);
        result := prev;
    end;
end;

end.
