program Solve;

uses
    SysUtils, Crt,
    UiLists in './src/ui/lists.pas',
    UiUtils in './src/ui/utils.pas',
    
    s_types in './src/shared/types.pas',
    s_utils in './src/shared/utils.pas',
    s_logging in './src/shared/logging.pas',
    
    e_global in './src/entities/global/global.pas',
    
    e_computerset_t in './src/entities/computerset/types.pas',
    e_computerset_crud in './src/entities/computerset/crud.pas',
    e_computerset_ui in './src/entities/computerset/ui.pas',
    e_computerset_io in './src/entities/computerset/io.pas',

    e_componenttype_t in './src/entities/componenttype/types.pas',
    e_componenttype_crud in './src/entities/componenttype/crud.pas',
    e_componenttype_ui in './src/entities/componenttype/ui.pas',
    e_componenttype_io in './src/entities/componenttype/io.pas',

    e_component_t in './src/entities/component/types.pas',
    e_component_crud in './src/entities/component/crud.pas',
    e_component_ui in './src/entities/component/ui.pas',
    e_component_io in './src/entities/component/io.pas',

    e_compatability_t in './src/entities/compatability/types.pas',
    e_compatability_crud in './src/entities/compatability/crud.pas',
    e_compatability_ui in './src/entities/compatability/ui.pas',
    e_compatability_io in './src/entities/compatability/io.pas',

    e_order_t in './src/entities/order/types.pas',
    e_order_crud in './src/entities/order/crud.pas',
    e_order_ui in './src/entities/order/ui.pas',
    e_order_io in './src/entities/order/io.pas',

    f_add_component in './src/features/add/component.pas',
    f_add_componentType in './src/features/add/componentType.pas',
    f_add_compatability in './src/features/add/compatability.pas',
    f_add_order in './src/features/add/order.pas';

const
  COMPONENTTYPES_DAT = 'componenttypes.dat';
  COMPONENTS_DAT = 'components.dat';
  COMPATIBILITIES_DAT = 'compatibilities.dat';
  ORDERS_DAT = 'orders.dat';
  COMPUTERSETS_DAT = 'computersets.dat';

var
    choice, submenuChoice: Int64;
    exitProgram: Boolean;
    exitWithSave: Boolean;
    log: TLogF;
begin
    log := GetLogger(LL_DEBUG);

    initGData();

    exitProgram := false;
    exitWithSave := false;
      
    while not exitProgram do
    begin
    ShowMainMenu;
    choice := SafeReadInteger('Выберите пункт меню: ', 1, 10);
    
    case choice of
        1: begin // Чтение данных из файла
            writeln('Загрузка данных...');
            LoadComponentTypesFromDatFile(COMPONENTTYPES_DAT, GData.ComponentTypes, GData.NewComponentTypeId);
            LoadComponentsFromDatFile(COMPONENTS_DAT, GData.Components, GData.NewComponentId);
            LoadCompatabilitiesFromDatFile(COMPATIBILITIES_DAT, GData.Compatabilities);
            LoadOrdersFromDatFile(ORDERS_DAT, GData.Orders, GData.NewOrderId);
            LoadComputerSetsFromDatFile(COMPUTERSETS_DAT, GData.Computersets, GData.NewComputersetId);
            writeln('Данные загружены.');
            WaitForKey;
        end;
        2: begin // Просмотр всего списка (если несколько списков – выпадает подменю с именами списков для просмотра)
            ShowCRUD_DS;
            choice := SafeReadInteger('Выберите пункт меню: ', 0, 4);
            case choice of
                1: begin // GData.Components
                    writeln(stringifyComponentAllTable);
                end;
                2: begin // GData.ComponentTypes
                    writeln(stringifyComponentTypeAllTable);
                end;
                3: begin // GData.Compatabilities
                    writeln(stringifyCompatabilityAllTable);
                end;
                4: begin // GData.Orders
                    writeln(stringifyOrderAllTable);
                end;
                0: begin // Назад
                end;
            end;

            WaitForKey;
        end;
        3: begin // Сортировка данных в соответствии с заданием
            // handle
            WaitForKey;
        end;
        4: begin // Поиск данных с использованием фильтров
            // handle
            WaitForKey;
        end;
        5: begin // Добавление данных в список (если несколько списков – выпадает подменю с именами списков для добавления)
            // handle
            ShowCRUD_DS;
            choice := SafeReadInteger('Выберите пункт меню: ', 0, 4);
            case choice of
                1: begin
                    uiCreateComponent;
                end;
                2: begin
                    uiCreateComponentType;
                end;
                3: begin
                    uiCreateCompatability;
                end;
                4: begin // GData.Orders
                    uiCreateOrder;
                end;
                0: begin // Назад
                end;
            end;

            WaitForKey;
        end;
        6: begin // Удаление данных из списка (если несколько списков – выпадает подменю с именами списков для удаления)
            // handle
            ShowCRUD_DS;
            choice := SafeReadInteger('Выберите пункт меню: ', 0, 4);
            case choice of
                1: begin // GData.Components
                    // handle
                    WaitForKey;
                end;
                2: begin // GData.ComponentTypes
                    // handle
                    WaitForKey;
                end;
                3: begin // GData.Compatabilities
                    // handle
                    WaitForKey;
                end;
                4: begin // GData.Orders
                    WaitForKey;
                end;
                0: begin // Назад
                end;
            end;

            WaitForKey;
        end;
        7: begin // Редактирование данных (если несколько списков – выпадает подменю с именами списков для редактирования)
            // handle
            ShowCRUD_DS;
            choice := SafeReadInteger('Выберите пункт меню: ', 0, 4);
            case choice of
                1: begin // GData.Components
                    // handle
                    WaitForKey;
                end;
                2: begin // GData.ComponentTypes
                    // handle
                    WaitForKey;
                end;
                3: begin // GData.Compatabilities
                    // handle
                    WaitForKey;
                end;
                4: begin // GData.Orders
                    WaitForKey;
                end;
                0: begin // Назад
                end;
            end;

            WaitForKey;
        end;
        8: begin // Специальные функции задания
            ShowSpecialFunctionsSubmenu;
            choice := SafeReadInteger('Выберите пункт меню: ', 0, 3);
            
            case choice of
                1: begin // Подбор вариантов комплектации компьютера
                    // handle
                    WaitForKey;
                end;
                2: begin // Оформление заказа
                    // handle
                    WaitForKey;
                end;
                3: begin // Поиск совместимых комплектующих заданного типа
                    // handle
                    WaitForKey;
                end;
                0: begin // Назад
                end;
            end;
        end;
        9: begin // Выход из программы без сохранения изменений
            // log(LL_DEBUG, '1');
            exitProgram := true;
            exitWithSave := false;
        end;
        10: begin // Выход с сохранением изменений
            exitProgram := true;
            exitWithSave := true;
        end;
    end;
    end;

    if exitWithSave then
    begin
      writeln('Сохранение данных...');
      SaveComponentTypesToDatFile(COMPONENTTYPES_DAT, GData.ComponentTypes);
      SaveComponentsToDatFile(COMPONENTS_DAT, GData.Components);
      SaveCompatabilitiesToDatFile(COMPATIBILITIES_DAT, GData.Compatabilities);
      SaveOrdersToDatFile(ORDERS_DAT, GData.Orders);
      SaveComputerSetsToDatFile(COMPUTERSETS_DAT, GData.Computersets);
      writeln('Данные сохранены.');
    end
    else
    begin
      writeln('Выход без сохранения.');
    end;
    
    WriteLn('Программа завершена.');
end.
