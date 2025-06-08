## Project Implementation Plan for Computer Configuration Management

**Project Goal:** To complete the development of a computer component and configuration management application as outlined in `solve.dpr`.

**I. Core Data Structures and Management (Existing & Implied)**

*   **Entities:**
    *   `TComputerSet`: Represents a collection of components. (CRUD and UI units exist)
    *   `TComponentType`: Defines types of components (e.g., CPU, GPU, RAM). (CRUD and UI units exist)
    *   `TComponent`: Represents individual components with details like name, type, price, specifications. (CRUD and UI units exist)
    *   `TCompatability`: Defines compatibility rules between components or component types. (CRUD and UI units exist)
    *   `TOrder`: Represents a customer order, likely including a selected computer set and customer details. (CRUD and UI units exist)
*   **Global Data (`GData`):** A global structure likely holding lists/arrays of the above entities. Initialized by `initGData()`.
*   **Logging (`TLogF`):** A logging mechanism is in place.

**II. Main Menu Functionality Implementation Plan (`solve.dpr`)**

The main program loop in `solve.dpr` presents a menu with 10 options. The plan below details the work required for each.

1.  **Чтение данных из файла (Menu Option 1)**
    *   **Objective:** Load all application data (Components, ComponentTypes, Compatibilities, Orders, ComputerSets) from persistent storage.
    *   **Tasks:**
        *   Define a file format (e.g., JSON, XML, or a custom binary/text format). Consider ease of parsing and human readability if necessary.
        *   Implement `LoadDataFromFile(filePath: string)` procedure.
            *   Read data for `GData.Components`.
            *   Read data for `GData.ComponentTypes`.
            *   Read data for `GData.Compatabilities`.
            *   Read data for `GData.Orders`.
            *   Read data for `GData.ComputerSets` (if applicable as a distinct saved entity).
        *   Integrate this procedure into menu option 1.
        *   Add error handling for file not found, incorrect format, etc.
        *   Prompt user for filename or use a default.

2.  **Просмотр всего списка (Menu Option 2)**
    *   **Objective:** Display all records for each data entity.
    *   **Current Status:** Partially implemented. Uses `stringify...AllTable` functions.
    *   **Tasks:**
        *   **Sub-option 1 (Components):** `writeln(stringifyComponentAllTable);` - Review and ensure `stringifyComponentAllTable` in `e_component_ui.pas` is complete and well-formatted.
        *   **Sub-option 2 (ComponentTypes):** `writeln(stringifyComponentTypeAllTable);` - Review and ensure `stringifyComponentTypeAllTable` in `e_componenttype_ui.pas` is complete.
        *   **Sub-option 3 (Compatabilities):** `writeln(stringifyCompatabilityAllTable);` - Review and ensure `stringifyCompatabilityAllTable` in `e_compatability_ui.pas` is complete.
        *   **Sub-option 4 (Orders):** `writeln(stringifyOrderAllTable);` - Review and ensure `stringifyOrderAllTable` in `e_order_ui.pas` is complete.
        *   Consider implementing display for `ComputerSets` if it's a top-level list to view. If `ComputerSets` are derived or part of orders, this might not be a direct view.
        *   Implement pagination or scrolling if lists can be very long.

3.  **Сортировка данных в соответствии с заданием (Menu Option 3)**
    *   **Objective:** Allow users to sort the lists of entities by various criteria.
    *   **Tasks:**
        *   Display a submenu: "Sort Components", "Sort ComponentTypes", "Sort Compatibilities", "Sort Orders".
        *   For each entity:
            *   Identify key fields for sorting (e.g., Component: Name, Price, Type; Order: Date, Total Price).
            *   Implement UI to choose the entity, field to sort by, and sort order (ascending/descending).
            *   Implement sorting procedures within the respective `_crud.pas` or `_ui.pas` units (e.g., `SortComponentsByName`, `SortComponentsByPrice`). These procedures will modify the order of items in `GData`.
            *   After sorting, display the sorted list (reuse Option 2's display logic).

4.  **Поиск данных с использованием фильтров (Menu Option 4)**
    *   **Objective:** Allow users to find specific records based on filter criteria.
    *   **Tasks:**
        *   Display a submenu: "Filter Components", "Filter ComponentTypes", "Filter Compatibilities", "Filter Orders".
        *   For each entity:
            *   Identify key fields for filtering (e.g., Component: by Type, by Name substring, by Price range; Order: by Date range, by Customer).
            *   Implement UI to choose the entity and input filter criteria for relevant fields.
            *   Implement filtering logic. This could involve creating temporary lists of results or displaying filtered views of `GData`.
            *   Display the filtered results (reuse Option 2's display logic or a similar formatted output).

5.  **Добавление данных в список (Menu Option 5)**
    *   **Objective:** Allow users to add new records for each entity.
    *   **Current Status:** Partially implemented for Components, ComponentTypes, Compatibilities using `uiCreate...` procedures.
    *   **Tasks:**
        *   **Sub-option 1 (Components):** `uiCreateComponent` (from `f_add_component.pas` or `e_component_ui.pas`) - Review, test, and ensure it correctly prompts for all necessary `TComponent` fields and adds to `GData.Components`.
        *   **Sub-option 2 (ComponentTypes):** `uiCreateComponentType` (from `f_add_componentType.pas` or `e_componenttype_ui.pas`) - Review, test, and ensure it correctly prompts and adds to `GData.ComponentTypes`.
        *   **Sub-option 3 (Compatabilities):** `uiCreateCompatability` (from `f_add_compatability.pas` or `e_compatability_ui.pas`) - Review, test, and ensure it correctly prompts for component IDs/names to link and adds to `GData.Compatabilities`. This might involve selecting existing components.
        *   **Sub-option 4 (Orders):**
            *   Implement `uiCreateOrder` (likely in `e_order_ui.pas`).
            *   This will be more complex:
                *   Prompt for customer details.
                *   Allow selection/creation of a computer set for the order (potentially linking to functionality in Option 8.1 or allowing manual component selection).
                *   Calculate total price.
                *   Add the new order to `GData.Orders`.
        *   Ensure all `uiCreate...` functions include robust input validation.

6.  **Удаление данных из списка (Menu Option 6)**
    *   **Objective:** Allow users to delete records.
    *   **Tasks:**
        *   For each sub-option (Components, ComponentTypes, Compatibilities, Orders):
            *   Implement `uiDelete<EntityName>` (e.g., `uiDeleteComponent` in `e_component_ui.pas`).
            *   Display a list of existing items (e.g., using a numbered list or their IDs).
            *   Prompt user to select an item to delete.
            *   Ask for confirmation before deletion.
            *   Implement the corresponding `Delete<EntityName>ById` function in the `_crud.pas` unit (e.g., `DeleteComponentById` in `e_component_crud.pas`) to remove the item from `GData`.
            *   Consider cascading deletes or warnings (e.g., if deleting a ComponentType, what happens to Components of that type? If deleting a Component, what about Compatibilities or Orders involving it?).

7.  **Редактирование данных (Menu Option 7)**
    *   **Objective:** Allow users to modify existing records.
    *   **Tasks:**
        *   For each sub-option (Components, ComponentTypes, Compatibilities, Orders):
            *   Implement `uiEdit<EntityName>` (e.g., `uiEditComponent` in `e_component_ui.pas`).
            *   Display a list of existing items to choose from.
            *   Prompt user to select an item to edit.
            *   Display current values of the selected item's fields.
            *   Allow user to input new values for each field they want to change.
            *   Implement the corresponding `Update<EntityName>` function in the `_crud.pas` unit (e.g., `UpdateComponent` in `e_component_crud.pas`) to save changes to `GData`.
            *   Input validation for new values.

8.  **Специальные функции задания (Menu Option 8)**
    *   **Objective:** Implement application-specific core functionalities.
    *   **Tasks:**
        *   **Sub-option 1: Подбор вариантов комплектации компьютера**
            *   Implement `uiSelectComputerConfiguration` (perhaps in a new unit like `f_configure_computerset.pas`).
            *   This is likely the most complex part.
            *   UI: Prompt user for criteria (e.g., budget, desired performance level, specific component types like "Gaming GPU").
            *   Logic:
                *   Access `GData.Components` and `GData.Compatabilities`.
                *   Develop an algorithm to find valid combinations of components that meet criteria and respect compatibility rules.
                *   This could involve backtracking, constraint satisfaction, or other search algorithms.
                *   Present one or more valid configurations (ComputerSets) to the user.
                *   Allow user to save a selected configuration (potentially as a `TComputerSet` instance).
        *   **Sub-option 2: Оформление заказа**
            *   Implement `uiFinalizeOrder` (could reuse/extend `uiCreateOrder` logic or call it).
            *   UI:
                *   Allow user to select a pre-configured `TComputerSet` (from 8.1) or build one ad-hoc (could reuse parts of 8.1 or a simplified component selector).
                *   Prompt for customer information (if not already part of `uiCreateOrder`).
                *   Confirm order details and total price.
            *   Logic: Create and save a `TOrder` record in `GData.Orders`.
        *   **Sub-option 3: Поиск совместимых комплектующих заданного типа**
            *   Implement `uiFindCompatibleComponents` (perhaps in `f_add_compatability.pas` or a new features unit).
            *   UI:
                *   Prompt user to select a primary component (e.g., by listing all components or by first selecting a type then a component).
                *   Prompt user to select a target component type they are looking for compatibility with (e.g., "RAM" if a "Motherboard" was selected).
            *   Logic:
                *   Iterate through `GData.Components` of the target type.
                *   For each, check against `GData.Compatabilities` to see if it's compatible with the primary selected component.
                *   Display a list of compatible components.

9.  **Выход из программы без сохранения изменений (Menu Option 9)**
    *   **Objective:** Terminate the application without writing `GData` to disk.
    *   **Current Status:** `exitProgram := true;` - This is correctly implemented.
    *   **Tasks:** None, unless a confirmation ("Are you sure? Unsaved changes will be lost.") is desired.

10. **Выход с сохранением изменений (Menu Option 10)**
    *   **Objective:** Save all data to a file and then terminate.
    *   **Tasks:**
        *   Implement `SaveDataToFile(filePath: string)` procedure (symmetric to `LoadDataFromFile` from Option 1).
            *   Write `GData.Components`.
            *   Write `GData.ComponentTypes`.
            *   Write `GData.Compatabilities`.
            *   Write `GData.Orders`.
            *   Write `GData.ComputerSets` (if applicable).
        *   Call `SaveDataToFile` before setting `exitProgram := true`.
        *   Prompt for filename or use a default (ideally the one used for loading, or a new one).
        *   Handle potential save errors.

**III. General Tasks & Considerations**

*   **UI Enhancements (`UiLists.pas`, `UiUtils.pas`):**
    *   Review and enhance existing UI functions for clarity, consistency, and ease of use.
    *   Ensure `SafeReadInteger`, `WaitForKey`, and other utility functions are robust.
*   **Error Handling:** Implement comprehensive error handling and user-friendly messages throughout all new functionalities.
*   **Modularity:** Create new Pascal units (`.pas` files) for new features or complex logic as needed (e.g., for configuration algorithms, advanced search UIs) and add them to the `uses` clause in `solve.dpr`.
*   **Testing:** Incrementally test each implemented feature.
*   **Documentation/Comments:** Add comments to new code sections explaining logic, especially for complex algorithms.
*   **Data Persistence Strategy:**
    *   Confirm the choice of file format.
    *   Decide if there's a single data file or multiple (e.g., one per entity type). A single file is often simpler for this scale.
*   **Initial Data (`initGData`):** Decide if `initGData` should populate with sample data for testing if no data file is loaded, or just initialize empty lists. 
