unit f_add_component;

interface

uses
    sysutils,
    UiUtils,
    s_types, s_logging,
    e_component_t, e_component_crud, e_component_ui,
    e_componenttype_t, e_componenttype_crud, e_componenttype_ui;

procedure uiCreateComponent();


implementation


procedure uiCreateComponent();
var
    cTypeId: Integer;
    manufacturer_name: TFixedString;
    model_name: TFixedString;
    description: TFixedString;
    cost: Real;
    availability: Integer;
    node: PComponentNode;
begin
    writeln;

    cTypeId := safeReadComponentType();
    if cTypeId <> -1 then
    begin
        manufacturer_name := SafeReadString('manufacturer_name: ');

        model_name := SafeReadString('model_name: ');
        
        description := SafeReadString('description: ');
        
        cost := SafeReadInteger('cost: ', 0, MAXINT);
        
        availability := SafeReadInteger('availability: ', 0, MAXINT);

        node := component_create(
            cTypeId,
            manufacturer_name,
            model_name,
            description,
            cost,
            availability
        );

        writeln;
        writeln('Created new row:');
        writeln(stringifyComponent(node));
    end else begin
        writeln;
        writeln('Создание отменено.');
    end;
end;

end.
