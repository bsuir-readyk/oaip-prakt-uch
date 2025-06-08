unit f_add_componentType;

interface

uses
    sysutils,
    UiUtils,
    s_types, s_logging,
    e_componentType_t, e_componentType_crud, e_componentType_ui;

procedure uiCreateComponentType();


implementation


procedure uiCreateComponentType();
var
    name: string;
    node: PComponentTypeNode;
begin
    writeln;

    name := SafeReadString('name: ');
    node := componentType_create(name);
    
    writeln;
    writeln('Created new row:');
    writeln(stringifyComponentType(node));
end;

end.
