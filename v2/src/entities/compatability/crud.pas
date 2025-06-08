unit e_compatability_crud;

interface

uses
  SysUtils, s_types, s_logging, e_compatability_t, e_global;

function compatability_create(
    left_id: integer;
    right_id: integer
): PCompatabilityNode;

function compatability_read(
    left_id: Integer
): TAPCompatabilityNode;

// function compatability_update(
//     id: Integer;
//     new_data: TCompatability
// ): PCompatabilityNode;

// returns pointer to previous
function compatability_delete(
    left_id: integer;
    right_id: integer
): PCompatabilityNode;

implementation

function compatability_create(
    left_id: integer;
    right_id: integer
): PCompatabilityNode;
var
    node: PCompatabilityNode;
    data: TCompatability;
begin
    data.left_id := left_id;
    data.right_id := right_id;

    node := new(PCompatabilityNode);
    node^.data := data;
    node^.next := nil;

    result := GData.Compatabilities;
    while result^.next <> nil do begin
        result := result^.next;
    end;

    result^.next := node;
    
    result := node;
end;

function compatability_read(
    left_id: Integer
): TAPCompatabilityNode;
var
    node: PCompatabilityNode;
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    result := [];

    node := GData.Compatabilities;
    if (node^.next = nil) then begin
        log(LL_DEBUG, format('{compatability_read} Not found compatability with __left_id=%d__', [left_id]));
    end else begin
        repeat
        node := node^.next;
        if (node^.data.left_id = left_id) then begin
            SetLength(result, length(result) + 1);
            result[length(result) - 1] := node;
        end;
        until node^.next = nil;
    end;
end;

// function compatability_update(
//     id: Integer;
//     new_data: TCompatability
// ): PCompatabilityNode;
// begin
//     result := compatability_read(id);
//     if (result <> nil) then begin
//         result^.data := data;
//     end;
// end;

function compatability_delete(
    left_id: integer;
    right_id: integer
): PCompatabilityNode;
var
    prev: PCompatabilityNode;
    log: TLogF;
begin
    log := GetLogger(GData.LogLevel);
    // log := GetLogger(LL_DEBUG);
    result := GData.Compatabilities;
    prev := GData.Compatabilities;
    repeat
        prev := result;
        result := result^.next;
    until (result^.next = nil) or ((result^.data.left_id = left_id) and (result^.data.right_id = right_id));

    if (result^.next = nil) and ((result^.data.left_id <> left_id) or (result^.data.right_id <> right_id)) then begin
        log(LL_DEBUG, format('Not found compatability with __left_id=%d__, __right_id=%d__', [left_id, right_id]));
        result := nil;
    end else begin
        prev^.next := result^.next;
        dispose(result);
        result := prev;
    end;
end;

end.
