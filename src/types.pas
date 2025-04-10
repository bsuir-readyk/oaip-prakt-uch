unit Types;

interface

uses
  SysUtils;

type
  // Комплектующая
  TComponent = record
    Code: Integer;           // Код комплектующей
    TypeCode: Integer;       // Код типа комплектующей
    Manufacturer: string;    // Фирма-изготовитель
    Model: string;           // Модель
    Parameters: string;      // Параметры в формате "ключ:значение"
    Price: Real;             // Цена
    InStock: Boolean;        // Наличие
  end;

  // Тип комплектующей
  TComponentType = record
    Code: Integer;           // Код типа
    Name: string;            // Название типа
  end;

  // Совместимость комплектующих
  TCompatibility = record
    ComponentCode1: Integer;  // Код первой комплектующей
    ComponentCode2: Integer;  // Код второй комплектующей
  end;

  // Конфигурация компьютера
  TConfiguration = record
    Components: array of TComponent; // Комплектующие в конфигурации
    ComponentCount: Integer;        // Количество комплектующих
    TotalPrice: Real;              // Общая стоимость
  end;

  // Заказ
  TOrder = record
    OrderNumber: Integer;    // Номер заказа
    Date: TDateTime;         // Дата заказа
    CustomerName: string;    // Имя заказчика
    CustomerPhone: string;   // Телефон заказчика
    TotalPrice: Real;        // Общая стоимость
    Components: array of Integer; // Коды комплектующих в заказе
    ComponentCount: Integer; // Количество комплектующих в заказе
  end;

  // Узел списка комплектующих
  PComponentNode = ^TComponentNode;
  TComponentNode = record
    Data: TComponent;        // Данные о комплектующей
    Next: PComponentNode;    // Указатель на следующий узел
  end;

  // Узел списка типов комплектующих
  PComponentTypeNode = ^TComponentTypeNode;
  TComponentTypeNode = record
    Data: TComponentType;    // Данные о типе комплектующей
    Next: PComponentTypeNode; // Указатель на следующий узел
  end;

  // Узел списка совместимости
  PCompatibilityNode = ^TCompatibilityNode;
  TCompatibilityNode = record
    Data: TCompatibility;    // Данные о совместимости
    Next: PCompatibilityNode; // Указатель на следующий узел
  end;

  // Узел списка заказов
  POrderNode = ^TOrderNode;
  TOrderNode = record
    Data: TOrder;            // Данные о заказе
    Next: POrderNode;        // Указатель на следующий узел
  end;

  // Узел списка конфигураций
  PConfigurationNode = ^TConfigurationNode;
  TConfigurationNode = record
    Data: TConfiguration;    // Данные о конфигурации
    Next: PConfigurationNode; // Указатель на следующий узел
  end;

  // Списки данных
  TDataLists = record
    Components: PComponentNode;       // Список комплектующих
    ComponentTypes: PComponentTypeNode; // Список типов комплектующих
    Compatibilities: PCompatibilityNode; // Список совместимостей
    Orders: POrderNode;              // Список заказов
    Configurations: PConfigurationNode; // Список конфигураций (временный)
  end;

// Константы для имен файлов
const
  COMPONENTS_FILE = 'data/components.dat';
  COMPONENT_TYPES_FILE = 'data/types.dat';
  COMPATIBILITY_FILE = 'data/compatibility.dat';
  ORDERS_FILE = 'data/orders.dat';
  CONFIGURATIONS_TEXT_FILE = 'data/configurations.txt';
  ORDER_INFO_TEXT_FILE = 'data/order_info.txt';
  COMPATIBLE_COMPONENTS_TEXT_FILE = 'data/compatible_components.txt';

var
  DataLists: TDataLists; // Глобальная переменная для хранения всех списков

implementation

end.
