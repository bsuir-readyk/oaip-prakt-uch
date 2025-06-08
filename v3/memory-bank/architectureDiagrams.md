# Диаграммы архитектуры системы

## 1. Общая архитектура системы

```mermaid
graph TD
    A[Пользователь] --> B[Пользовательский интерфейс]
    B --> C[Бизнес-логика]
    C --> D[Доступ к данным]
    D --> E[Файловая система]
    
    subgraph "Пользовательский интерфейс"
        UI1[Главное меню]
        UI2[Подменю]
        UI3[Формы ввода]
        UI4[Отображение списков]
    end
    
    subgraph "Бизнес-логика"
        BL1[Сортировка]
        BL2[Фильтрация]
        BL3[Создание компьютерных сборок]
        BL4[Управление заказами]
    end
    
    subgraph "Доступ к данным"
        DA1[Работа с файлами]
        DA2[Работа с динамическими списками]
    end
    
    subgraph "Файловая система"
        FS1[Типизированные файлы]
        FS2[Текстовые файлы]
    end
```

## 2. Структура модулей системы

```mermaid
graph TD
    A[solve.dpr] --> B[types.pas]
    A --> C[file_io.pas]
    A --> D[list_utils.pas]
    A --> E[sort_utils.pas]
    A --> F[filter_utils.pas]
    A --> G[computer_builder.pas]
    A --> H[order_manager.pas]
    A --> I[ui_utils.pas]
    A --> J[menu_handlers.pas]
    A --> K[logging.pas]
    
    B --> C
    B --> D
    B --> E
    B --> F
    B --> G
    B --> H
    B --> I
    B --> J
    
    C --> D
    
    D --> E
    D --> F
    D --> G
    D --> H
    
    E --> J
    
    F --> G
    F --> J
    
    G --> H
    G --> J
    
    H --> J
    
    I --> J
```

## 3. Диаграмма потока данных

```mermaid
graph LR
    A[Пользователь] --> B[Пользовательский интерфейс]
    B --> C[Обработчики меню]
    C --> D[Бизнес-логика]
    D --> E[Работа с данными]
    E --> F[Файловая система]
    F --> E
    E --> D
    D --> C
    C --> B
    B --> A
```

## 4. Диаграмма классов (структур данных)

```mermaid
classDiagram
    class TComponentType {
        +id: Integer
        +name: string
    }
    
    class TComponent {
        +id: Integer
        +cTypeId: Integer
        +manufacturer_name: string
        +model_name: string
        +description: string
        +cost: Real
        +availability: Integer
    }
    
    class TCompatibility {
        +left_id: Integer
        +right_id: Integer
    }
    
    class TComputerSet {
        +cpu: PComponent
        +motherboard: PComponent
        +ram: PComponent
        +psu: PComponent
        +storage: PComponent
        +totalCost: Real
    }
    
    class TOrder {
        +id: Integer
        +computerSet: TComputerSet
        +orderDate: TDateTime
        +customerName: string
        +customerPhone: string
        +customerEmail: string
        +status: string
    }
    
    TComponent --> TComponentType : has type
    TCompatibility --> TComponent : connects
    TComputerSet --> TComponent : contains
    TOrder --> TComputerSet : contains
```

## 5. Диаграмма последовательности для подбора компьютерных сборок

```mermaid
sequenceDiagram
    participant User as Пользователь
    participant UI as Пользовательский интерфейс
    participant Handler as Обработчик меню
    participant Builder as Строитель компьютерных сборок
    participant Filter as Фильтр
    participant Data as Работа с данными
    
    User->>UI: Выбор пункта меню "Специальные функции"
    UI->>Handler: Вызов HandleSpecialFunctions()
    Handler->>UI: Отображение подменю специальных функций
    User->>UI: Выбор "Подбор вариантов комплектации"
    UI->>User: Запрос минимальной цены
    User->>UI: Ввод минимальной цены
    UI->>User: Запрос максимальной цены
    User->>UI: Ввод максимальной цены
    UI->>Handler: Передача параметров
    Handler->>Builder: Вызов BuildAllPossibleComputerSets()
    Builder->>Data: Получение списка комплектующих
    Data->>Builder: Возврат списка комплектующих
    Builder->>Data: Получение списка типов комплектующих
    Data->>Builder: Возврат списка типов комплектующих
    Builder->>Data: Получение списка совместимости
    Data->>Builder: Возврат списка совместимости
    Builder->>Builder: Создание всех возможных сборок
    Builder->>Filter: Фильтрация по ценовому диапазону
    Filter->>Builder: Возврат отфильтрованных сборок
    Builder->>Handler: Возврат списка сборок
    Handler->>UI: Отображение списка сборок
    UI->>User: Вывод результатов
```

## 6. Диаграмма последовательности для оформления заказа

```mermaid
sequenceDiagram
    participant User as Пользователь
    participant UI as Пользовательский интерфейс
    participant Handler as Обработчик меню
    participant OrderMgr as Менеджер заказов
    participant Data as Работа с данными
    
    User->>UI: Выбор компьютерной сборки для заказа
    UI->>User: Запрос данных заказчика (имя)
    User->>UI: Ввод имени
    UI->>User: Запрос данных заказчика (телефон)
    User->>UI: Ввод телефона
    UI->>User: Запрос данных заказчика (email)
    User->>UI: Ввод email
    UI->>Handler: Передача данных
    Handler->>OrderMgr: Вызов CreateOrder()
    OrderMgr->>OrderMgr: Создание объекта заказа
    OrderMgr->>Handler: Возврат объекта заказа
    Handler->>Data: Добавление заказа в список
    Data->>Handler: Подтверждение добавления
    Handler->>UI: Сообщение об успешном оформлении
    UI->>User: Вывод сообщения
```

## 7. Диаграмма последовательности для поиска совместимых комплектующих

```mermaid
sequenceDiagram
    participant User as Пользователь
    participant UI as Пользовательский интерфейс
    participant Handler as Обработчик меню
    participant Filter as Фильтр
    participant Data as Работа с данными
    
    User->>UI: Выбор пункта меню "Специальные функции"
    UI->>Handler: Вызов HandleSpecialFunctions()
    Handler->>UI: Отображение подменю специальных функций
    User->>UI: Выбор "Поиск совместимых комплектующих"
    UI->>User: Запрос кода комплектующей
    User->>UI: Ввод кода комплектующей
    UI->>User: Запрос типа комплектующей
    User->>UI: Ввод типа комплектующей
    UI->>Handler: Передача параметров
    Handler->>Filter: Вызов FindCompatibleComponents()
    Filter->>Data: Получение списка комплектующих
    Data->>Filter: Возврат списка комплектующих
    Filter->>Data: Получение списка совместимости
    Data->>Filter: Возврат списка совместимости
    Filter->>Filter: Фильтрация совместимых комплектующих
    Filter->>Handler: Возврат списка совместимых комплектующих
    Handler->>UI: Отображение списка совместимых комплектующих
    UI->>User: Вывод результатов
```

## 8. Диаграмма состояний для заказа

```mermaid
stateDiagram-v2
    [*] --> New: Создание заказа
    New --> Processing: Обработка заказа
    Processing --> Completed: Выполнение заказа
    Processing --> Cancelled: Отмена заказа
    Completed --> [*]
    Cancelled --> [*]
```

## 9. Диаграмма компонентов системы

```mermaid
graph TD
    A[solve.dpr] --> B[Модули ядра]
    A --> C[Модули бизнес-логики]
    A --> D[Модули пользовательского интерфейса]
    
    subgraph "Модули ядра"
        B1[types.pas]
        B2[file_io.pas]
        B3[list_utils.pas]
        B4[logging.pas]
    end
    
    subgraph "Модули бизнес-логики"
        C1[sort_utils.pas]
        C2[filter_utils.pas]
        C3[computer_builder.pas]
        C4[order_manager.pas]
    end
    
    subgraph "Модули пользовательского интерфейса"
        D1[ui_utils.pas]
        D2[menu_handlers.pas]
    end
```

## 10. Диаграмма развертывания

```mermaid
graph TD
    A[Компьютер пользователя] --> B[Операционная система]
    B --> C[Free Pascal Compiler]
    C --> D[ComputerPartsManager]
    
    subgraph "ComputerPartsManager"
        D1[Исполняемый файл]
        D2[Типизированные файлы данных]
        D3[Текстовые файлы отчетов]
    end
