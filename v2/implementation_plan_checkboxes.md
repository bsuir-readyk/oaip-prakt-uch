## Epics and Features Implementation Plan

## [ ] Epic: Core Data Persistence

### [ ] Feature: File-Based Data Loading (Menu Option 1)
#### Plan
- [ ] Define a unified file format for all entities (e.g., JSON, XML, custom text).
- [ ] Implement `LoadDataFromFile(filePath: string)` procedure in a suitable shared unit.
  - [ ] Implement loading for `GData.Components`.
  - [ ] Implement loading for `GData.ComponentTypes`.
  - [ ] Implement loading for `GData.Compatabilities`.
  - [ ] Implement loading for `GData.Orders`.
  - [ ] Implement loading for `GData.ComputerSets` (if treated as a distinct saved entity).
- [ ] Integrate `LoadDataFromFile` into `solve.dpr` for menu option 1.
- [ ] Add UI for prompting user for filename (or use a default).
- [ ] Implement robust error handling (file not found, incorrect format, corrupted data).

### [ ] Feature: File-Based Data Saving (Menu Option 10)
#### Plan
- [ ] Implement `SaveDataToFile(filePath: string)` procedure, mirroring `LoadDataFromFile`.
  - [ ] Implement saving for `GData.Components`.
  - [ ] Implement saving for `GData.ComponentTypes`.
  - [ ] Implement saving for `GData.Compatabilities`.
  - [ ] Implement saving for `GData.Orders`.
  - [ ] Implement saving for `GData.ComputerSets` (if applicable).
- [ ] Integrate `SaveDataToFile` into `solve.dpr` for menu option 10, to be called before exit.
- [ ] Add UI for prompting user for filename (or use a default, potentially the loaded filename).
- [ ] Implement error handling for save operations (disk full, permissions, etc.).

## [ ] Epic: Entity Management (CRUD Operations)

### [ ] Feature: Data Viewing Enhancements (Menu Option 2)
#### Plan
- [ ] Review and ensure `stringifyComponentAllTable` (in `e_component_ui.pas`) is complete, well-formatted, and handles empty lists gracefully.
- [ ] Review and ensure `stringifyComponentTypeAllTable` (in `e_componenttype_ui.pas`) is complete and well-formatted.
- [ ] Review and ensure `stringifyCompatabilityAllTable` (in `e_compatability_ui.pas`) is complete and well-formatted.
- [ ] Review and ensure `stringifyOrderAllTable` (in `e_order_ui.pas`) is complete and well-formatted.
- [ ] **Optional:** Implement `stringifyComputerSetAllTable` if `ComputerSets` are to be viewed as a separate list.
- [ ] **Optional:** Implement pagination or "press key to see more" for long lists in all viewing functions.

### [ ] Feature: Data Sorting (Menu Option 3)
#### Plan
- [ ] Design and implement a submenu in `solve.dpr` for selecting entity to sort (Components, Types, Compatibilities, Orders).
- [ ] For `TComponent`:
  - [ ] Identify sortable fields (e.g., Name, Price, TypeID).
  - [ ] Implement UI in `e_component_ui.pas` to choose field and order (ASC/DESC).
  - [ ] Implement sorting procedures (e.g., `SortComponentsByName`, `SortComponentsByPrice`) in `e_component_crud.pas` or a dedicated sorting unit, modifying `GData.Components`.
- [ ] For `TComponentType`:
  - [ ] Identify sortable fields (e.g., Name, ID).
  - [ ] Implement UI in `e_componenttype_ui.pas`.
  - [ ] Implement sorting procedures in `e_componenttype_crud.pas`.
- [ ] For `TCompatability`:
  - [ ] Identify sortable fields (e.g., Component1ID, Component2ID).
  - [ ] Implement UI in `e_compatability_ui.pas`.
  - [ ] Implement sorting procedures in `e_compatability_crud.pas`.
- [ ] For `TOrder`:
  - [ ] Identify sortable fields (e.g., OrderDate, TotalPrice, CustomerName).
  - [ ] Implement UI in `e_order_ui.pas`.
  - [ ] Implement sorting procedures in `e_order_crud.pas`.
- [ ] After sorting, call the respective `stringify...AllTable` function to display results.

### [ ] Feature: Data Filtering/Searching (Menu Option 4)
#### Plan
- [ ] Design and implement a submenu in `solve.dpr` for selecting entity to filter.
- [ ] For `TComponent`:
  - [ ] Identify filterable fields (e.g., by Type, Name substring, Price range).
  - [ ] Implement UI in `e_component_ui.pas` to input filter criteria.
  - [ ] Implement filtering logic (e.g., `FilterComponents(criteria): TComponentsList`) in `e_component_crud.pas` or a features unit.
  - [ ] Implement a display function for the filtered list (can reuse/adapt `stringifyComponentAllTable`).
- [ ] For `TComponentType`:
  - [ ] Identify filterable fields (e.g., by Name substring).
  - [ ] Implement UI in `e_componenttype_ui.pas`.
  - [ ] Implement filtering logic in `e_componenttype_crud.pas`.
  - [ ] Implement display for filtered list.
- [ ] For `TCompatability`:
  - [ ] Identify filterable fields (e.g., by a specific ComponentID).
  - [ ] Implement UI in `e_compatability_ui.pas`.
  - [ ] Implement filtering logic in `e_compatability_crud.pas`.
  - [ ] Implement display for filtered list.
- [ ] For `TOrder`:
  - [ ] Identify filterable fields (e.g., by Date range, Customer Name).
  - [ ] Implement UI in `e_order_ui.pas`.
  - [ ] Implement filtering logic in `e_order_crud.pas`.
  - [ ] Implement display for filtered list.

### [ ] Feature: Data Addition (Menu Option 5)
#### Plan
- [ ] **Components:** Review `uiCreateComponent` (from `f_add_component.pas` or `e_component_ui.pas`).
  - [ ] Ensure all `TComponent` fields are prompted.
  - [ ] Ensure robust input validation for each field.
  - [ ] Ensure successful addition to `GData.Components`.
- [ ] **ComponentTypes:** Review `uiCreateComponentType` (from `f_add_componentType.pas` or `e_componenttype_ui.pas`).
  - [ ] Ensure all `TComponentType` fields are prompted.
  - [ ] Ensure robust input validation.
  - [ ] Ensure successful addition to `GData.ComponentTypes`.
- [ ] **Compatabilities:** Review `uiCreateCompatability` (from `f_add_compatability.pas` or `e_compatability_ui.pas`).
  - [ ] Implement UI for selecting existing components (e.g., by listing them with IDs).
  - [ ] Ensure robust input validation (e.g., selected components exist).
  - [ ] Ensure successful addition to `GData.Compatabilities`.
- [ ] **Orders (Sub-option 4):**
  - [ ] Implement `uiCreateOrder` in `e_order_ui.pas`.
  - [ ] Prompt for all `TOrder` fields (Customer details, Order Date - possibly auto-generated).
  - [ ] Integrate with Computer Set selection/creation (see Epic: Special Functions).
  - [ ] Calculate total price based on selected components/set.
  - [ ] Ensure robust input validation.
  - [ ] Ensure successful addition to `GData.Orders`.

### [ ] Feature: Data Deletion (Menu Option 6)
#### Plan
- [ ] For each entity (Components, ComponentTypes, Compatibilities, Orders):
  - [ ] Implement `uiDelete<EntityName>` in the respective `e_<entity>_ui.pas` unit.
  - [ ] Display a numbered list of existing items (using ID and a descriptive field).
  - [ ] Prompt user to select an item by ID or number.
  - [ ] Implement confirmation prompt ("Are you sure you want to delete...?").
  - [ ] Call a `Delete<EntityName>ById(id)` function in the respective `e_<entity>_crud.pas` unit.
  - [ ] Address cascading concerns:
    - [ ] Deleting `TComponentType`: Warn if components of this type exist (or disallow/offer to reassign).
    - [ ] Deleting `TComponent`: Warn if this component is in compatibilities or orders (or disallow/offer to remove related).
    - [ ] Deleting `TCompatability`: Straightforward removal.
    - [ ] Deleting `TOrder`: Straightforward removal, or mark as cancelled.

### [ ] Feature: Data Editing (Menu Option 7)
#### Plan
- [ ] For each entity (Components, ComponentTypes, Compatibilities, Orders):
  - [ ] Implement `uiEdit<EntityName>` in the respective `e_<entity>_ui.pas` unit.
  - [ ] Display a numbered list of existing items to select for editing.
  - [ ] Once selected, display all current fields and their values.
  - [ ] Allow user to input new values for each field (or skip fields they don't want to change).
  - [ ] Implement robust input validation for all new values.
  - [ ] Call an `Update<EntityName>(editedRecord)` function in the respective `e_<entity>_crud.pas` unit.
  - [ ] For `TCompatability` editing, ensure component selections remain valid.
  - [ ] For `TOrder` editing, recalculate price if components/set change.

## [ ] Epic: Special Application Functions (Menu Option 8)

### [ ] Feature: Computer Configuration Builder (Sub-option 8.1)
#### Plan
- [ ] Create a new unit (e.g., `f_configure_computerset.pas` or similar).
- [ ] Implement `uiSelectComputerConfiguration`.
- [ ] **UI Tasks:**
  - [ ] Prompt user for configuration criteria (e.g., target budget, primary use-case like gaming/office, specific component type preferences).
  - [ ] Display available components for selection, possibly filtered by type.
- [ ] **Logic Tasks:**
  - [ ] Access `GData.Components` and `GData.Compatabilities`.
  - [ ] Develop an algorithm to find valid component combinations:
    - [ ] Ensure selected components are compatible with each other.
    - [ ] Meet user-defined criteria (budget, types).
    - [ ] Consider essential component types for a full build (e.g., CPU, Motherboard, RAM, Storage, PSU, Case).
  - [ ] Present one or more valid `TComputerSet` configurations.
  - [ ] Allow user to select and optionally save a `TComputerSet` (add to `GData.ComputerSets` or a temporary list for ordering).
- [ ] Define the structure of `TComputerSet` clearly if not already fully defined in `e_computerset_t.pas`.

### [ ] Feature: Order Placement (Sub-option 8.2)
#### Plan
- [ ] Implement `uiFinalizeOrder` (can extend or be called by `uiCreateOrder` from Menu Option 5).
- [ ] **UI Tasks:**
  - [ ] Allow selection of a `TComputerSet` (from 8.1 or a list of saved sets) OR allow building a new configuration ad-hoc (could reuse parts of 8.1 or a simplified component picker).
  - [ ] Prompt for customer information if not already gathered.
  - [ ] Display a summary of the order (selected items, customer info, total price).
  - [ ] Ask for final confirmation.
- [ ] **Logic Tasks:**
  - [ ] Create a `TOrder` record.
  - [ ] Populate it with customer details, items (from `TComputerSet` or individual components), and total price.
  - [ ] Add the new `TOrder` to `GData.Orders`.

### [ ] Feature: Find Compatible Components (Sub-option 8.3)
#### Plan
- [ ] Implement `uiFindCompatibleComponents` (e.g., in `f_add_compatability.pas` or a new features unit).
- [ ] **UI Tasks:**
  - [ ] Prompt user to select a primary component (e.g., list components, allow selection by ID).
  - [ ] Prompt user to select a target component *type* they want to find compatible parts for (e.g., if Motherboard selected, user might look for compatible RAM or CPU).
- [ ] **Logic Tasks:**
  - [ ] Access `GData.Components` and `GData.Compatabilities`.
  - [ ] Iterate through all components of the target type.
  - [ ] For each component, check its compatibility with the primary selected component using `GData.Compatabilities`.
  - [ ] Display a list of all compatible components found.

## [ ] Epic: Application Shell & Usability

### [ ] Feature: Program Exit Logic (Menu Options 9 & 10)
#### Plan
- [ ] **Option 9 (Exit without saving):**
  - [ ] `exitProgram := true;` is already in place.
  - [ ] **Optional:** Add a confirmation prompt ("Are you sure? Unsaved changes will be lost.").
- [ ] **Option 10 (Exit with saving):**
  - [ ] Ensure `SaveDataToFile` (from Core Data Persistence epic) is called before `exitProgram := true;`.

### [ ] Feature: General UI/UX Enhancements
#### Plan
- [ ] Review all `UiLists.pas` and `UiUtils.pas` functions.
  - [ ] Ensure `SafeReadInteger`, `WaitForKey` are robust and user-friendly.
  - [ ] Standardize menu presentation and input prompts.
  - [ ] Improve clarity of on-screen instructions and messages.
- [ ] Implement consistent error messaging across the application.
- [ ] Ensure `initGData` in `e_global.pas` correctly initializes empty lists for all entity types. Consider if it should load default/sample data if no file exists.

### [ ] Feature: Code Modularity and Structure
#### Plan
- [ ] As new complex functionalities are developed (e.g., configuration algorithm), create dedicated Pascal units.
- [ ] Add new units to the `uses` clause in `solve.dpr` and other relevant files.
- [ ] Regularly review code for clarity, add comments for non-trivial logic.

### [ ] Feature: Testing Strategy
#### Plan
- [ ] Manually test each implemented menu option and sub-option thoroughly.
- [ ] Test edge cases (e.g., empty lists, invalid inputs, large amounts of data if feasible).
- [ ] Test data loading and saving with various scenarios (e.g., creating a file, loading it, modifying, saving again). 
