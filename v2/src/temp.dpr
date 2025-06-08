program Solve;

uses
    SysUtils, Crt,
    s_types in './shared/types.pas',
    s_utils in './shared/utils.pas',
    s_logging in './shared/logging.pas',
    
    e_component_t in './entities/component/types.pas',
    e_componenttype_t in './entities/componenttype/types.pas',
    e_compatability_t in './entities/compatability/types.pas',
    e_computerset_t in './entities/computerset/types.pas',
    
    e_global in './entities/global/global.pas',

    e_component_ui in './entities/component/ui.pas',
    e_componenttype_ui in './entities/componenttype/ui.pas',
    e_computerset_ui in './entities/computerset/ui.pas',
    e_compatability_ui in './entities/compatability/ui.pas',
    
    e_component_crud in './entities/component/crud.pas',
    e_componenttype_crud in './entities/componenttype/crud.pas',
    e_compatability_crud in './entities/compatability/crud.pas',
    e_computerset_crud in './entities/computerset/crud.pas',
    
    f_mock in './features/mock.pas';

var
    _PCompatabilityNode: PCompatabilityNode;
    _PCompatabilityNodeA: TAPCompatabilityNode;
    i: Integer;
    log: TLogF;
begin
    // log := GetLogger(GData.LogLevel);
    log := GetLogger(LL_DEBUG);

    initGData();
    mock_init();

    // component_delete(0);
    // component_delete(1);
    component_delete(2);
    component_delete(3);
    // component_delete(4);

    writeln(stringifyComponentAllTable);

    writeln();
    writeln('===============');
    writeln();

    componentType_delete(2);
    componentType_delete(3);

    writeln(stringifyComponentTypeAllTable);

    writeln();
    writeln('===============');
    writeln();

    compatability_delete(0,2);
    compatability_delete(2,0);
    
    compatability_delete(0,3);
    compatability_delete(3,0);
    
    writeln(stringifyCompatabilityAllTable());

    writeln();
    writeln('===============');
    writeln();

    computerSet_delete(2);
    computerSet_delete(3);

    writeln(stringifyComputerSetAllTable());
end.
