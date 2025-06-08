unit f_add_compatability;

interface

uses
    sysutils,
    UiUtils,
    s_types, s_logging,
    e_component_t, e_component_crud, e_component_ui,
    e_compatability_t, e_compatability_crud, e_compatability_ui;

procedure uiCreateCompatability();


implementation


procedure uiCreateCompatability();
var
    left_id: Integer;
    right_id: Integer;
    node: PcompatabilityNode;
begin
    writeln;

    left_id := safeReadComponent();
    if left_id <> -1 then
    begin
        right_id := safeReadComponent();
        if right_id <> -1 then
        begin
            node := compatability_create(
                left_id,
                right_id
            );
            writeln;
            writeln('Created new row:');
            writeln(stringifycompatability(node));
        end else begin
            writeln;
            writeln('Создание отменено.');
        end;
    end else begin
        writeln;
        writeln('Создание отменено.');
    end;
end;

end.
