unit f_mock;

interface

uses
    e_global,
    e_component_crud,
    e_componenttype_crud,
    e_compatability_crud,
    e_computerset_crud;

procedure mock_init();

implementation

procedure mock_init();
begin
    component_create(
        0,
        '0mock_manufacturer_name',
        '0mock_model_name',
        '0mock_description',
        0.1,
        0
    );
    component_create(
        1,
        '1mock_manufacturer_name',
        '1mock_model_name',
        '1mock_description',
        1.1,
        1
    );
    component_create(
        2,
       '2mock_manufacturer_name',
       '2mock_model_name',
       '2mock_description',
        2.1,
        2
    );
    component_create(
        3,
       '3mock_manufacturer_name',
       '3mock_model_name',
       '3mock_description',
        3.1,
        3
    );
    component_create(
        4,
       '4mock_manufacturer_name',
       '4mock_model_name',
       '4mock_description',
        4.1,
        4
    );

    // ----

    componenttype_create(
        '0mock_name'
    );
    componenttype_create(
        '1mock_name'
    );
    componenttype_create(
        '2mock_name'
    );
    componenttype_create(
        '3mock_name'
    );
    componenttype_create(
        '4mock_name'
    );

    // ----

        // 0: 1,2,3,4
        // 1: 0,4
        // 2: 0
        // 3: 0
        // 4: 0,1
    compatability_create(0,1);
    compatability_create(1,0);

    compatability_create(0,2);
    compatability_create(2,0);

    compatability_create(0,3);
    compatability_create(3,0);
    
    compatability_create(0,4);
    compatability_create(4,0);

    compatability_create(1,4);
    compatability_create(4,1);

    // ----

    computerset_create(0, 0, 0, 0, 0, 0.1);
    computerset_create(1, 1, 1, 1, 1, 1.1);
    computerset_create(2, 2, 2, 2, 2, 2.1);
    computerset_create(3, 3, 3, 3, 3, 3.1);
    computerset_create(4, 4, 4, 4, 4, 4.1);
end;

end.
