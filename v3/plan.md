# V3

## Requests

### Functional:

#### Crud
- - components
- - compatability
- - componentTypes

#### Business logic
- file io
- filtering
- get all computer sets
- - filter cost
- - sort cost
- - Оформить заказ понравившегося варианта
- all compatable with `componentId` + filtering

#### ui
- tables
- tui

### Data types

#### components
- id
- cTypeId
- manufacturer_name
- mondel_name
- description (параметров)
- cost
- availability (num/bool?)

#### compatability
- left_id
- right_id

#### componentTypes 
- id
- name

#### computerSet
- CPU (compatible with the Motherboard)
- Motherboard (compatible with the CPU)
- RAM (compatible with the Motherboard)
- Power Supply Unit
- Storage

## Arch

fsd
- layers: app, features, entities, shared
- segments:
- - ui: for terminal ui
- - model: for crud operations
- - api: for sorts, filtering and so on
