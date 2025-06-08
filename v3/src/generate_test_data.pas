program generate_test_data;

uses
  SysUtils, types, list_utils, file_io;

var
  componentTypesList: TComponentTypeList;
  componentsList: TComponentList;
  compatibilityList: TCompatibilityList;
  ordersList: TOrderList;
  
  // Типы комплектующих
  cpuType, motherboardType, ramType, psuType, storageType: TComponentType;
  
  // Комплектующие
  cpu1, cpu2, cpu3: TComponent;
  mb1, mb2, mb3: TComponent;
  ram1, ram2, ram3: TComponent;
  psu1, psu2, psu3: TComponent;
  storage1, storage2, storage3: TComponent;
  
  // Совместимость
  compat1, compat2, compat3, compat4, compat5, compat6, compat7, compat8, compat9: TCompatibility;

begin
  // Создание директорий для данных и результатов
  if not DirectoryExists('data') then
    CreateDir('data');
  
  if not DirectoryExists('output') then
    CreateDir('output');

  // Инициализация списков
  componentTypesList := nil;
  componentsList := nil;
  compatibilityList := nil;
  ordersList := nil;
  
  // Создание типов комплектующих
  cpuType.id := 1;
  cpuType.name := 'Процессор';
  AddComponentType(componentTypesList, cpuType);
  
  motherboardType.id := 2;
  motherboardType.name := 'Материнская плата';
  AddComponentType(componentTypesList, motherboardType);
  
  ramType.id := 3;
  ramType.name := 'Оперативная память';
  AddComponentType(componentTypesList, ramType);
  
  psuType.id := 4;
  psuType.name := 'Блок питания';
  AddComponentType(componentTypesList, psuType);
  
  storageType.id := 5;
  storageType.name := 'Накопитель';
  AddComponentType(componentTypesList, storageType);
  
  // Создание комплектующих
  // Процессоры
  cpu1.id := 1;
  cpu1.cTypeId := 1;
  cpu1.manufacturer_name := 'Intel';
  cpu1.model_name := 'Core i5-12400F';
  cpu1.description := '6 ядер, 12 потоков, 2.5 ГГц';
  cpu1.cost := 15000;
  cpu1.availability := 10;
  AddComponent(componentsList, cpu1);
  
  cpu2.id := 2;
  cpu2.cTypeId := 1;
  cpu2.manufacturer_name := 'AMD';
  cpu2.model_name := 'Ryzen 5 5600X';
  cpu2.description := '6 ядер, 12 потоков, 3.7 ГГц';
  cpu2.cost := 17000;
  cpu2.availability := 8;
  AddComponent(componentsList, cpu2);
  
  cpu3.id := 3;
  cpu3.cTypeId := 1;
  cpu3.manufacturer_name := 'Intel';
  cpu3.model_name := 'Core i7-12700K';
  cpu3.description := '12 ядер, 20 потоков, 3.6 ГГц';
  cpu3.cost := 30000;
  cpu3.availability := 5;
  AddComponent(componentsList, cpu3);
  
  // Материнские платы
  mb1.id := 4;
  mb1.cTypeId := 2;
  mb1.manufacturer_name := 'ASUS';
  mb1.model_name := 'PRIME B660-PLUS';
  mb1.description := 'LGA1700, DDR4, ATX';
  mb1.cost := 12000;
  mb1.availability := 7;
  AddComponent(componentsList, mb1);
  
  mb2.id := 5;
  mb2.cTypeId := 2;
  mb2.manufacturer_name := 'MSI';
  mb2.model_name := 'B550 TOMAHAWK';
  mb2.description := 'AM4, DDR4, ATX';
  mb2.cost := 14000;
  mb2.availability := 6;
  AddComponent(componentsList, mb2);
  
  mb3.id := 6;
  mb3.cTypeId := 2;
  mb3.manufacturer_name := 'ASUS';
  mb3.model_name := 'ROG STRIX Z690-F';
  mb3.description := 'LGA1700, DDR5, ATX';
  mb3.cost := 25000;
  mb3.availability := 3;
  AddComponent(componentsList, mb3);
  
  // Оперативная память
  ram1.id := 7;
  ram1.cTypeId := 3;
  ram1.manufacturer_name := 'Kingston';
  ram1.model_name := 'FURY Beast';
  ram1.description := 'DDR4, 16 ГБ (2x8), 3200 МГц';
  ram1.cost := 5000;
  ram1.availability := 15;
  AddComponent(componentsList, ram1);
  
  ram2.id := 8;
  ram2.cTypeId := 3;
  ram2.manufacturer_name := 'Corsair';
  ram2.model_name := 'Vengeance LPX';
  ram2.description := 'DDR4, 32 ГБ (2x16), 3600 МГц';
  ram2.cost := 10000;
  ram2.availability := 10;
  AddComponent(componentsList, ram2);
  
  ram3.id := 9;
  ram3.cTypeId := 3;
  ram3.manufacturer_name := 'G.Skill';
  ram3.model_name := 'Trident Z5';
  ram3.description := 'DDR5, 32 ГБ (2x16), 5600 МГц';
  ram3.cost := 20000;
  ram3.availability := 5;
  AddComponent(componentsList, ram3);
  
  // Блоки питания
  psu1.id := 10;
  psu1.cTypeId := 4;
  psu1.manufacturer_name := 'be quiet!';
  psu1.model_name := 'Pure Power 11';
  psu1.description := '600 Вт, 80+ Gold';
  psu1.cost := 6000;
  psu1.availability := 12;
  AddComponent(componentsList, psu1);
  
  psu2.id := 11;
  psu2.cTypeId := 4;
  psu2.manufacturer_name := 'Corsair';
  psu2.model_name := 'RM750x';
  psu2.description := '750 Вт, 80+ Gold';
  psu2.cost := 9000;
  psu2.availability := 8;
  AddComponent(componentsList, psu2);
  
  psu3.id := 12;
  psu3.cTypeId := 4;
  psu3.manufacturer_name := 'Seasonic';
  psu3.model_name := 'PRIME TX-1000';
  psu3.description := '1000 Вт, 80+ Titanium';
  psu3.cost := 18000;
  psu3.availability := 3;
  AddComponent(componentsList, psu3);
  
  // Накопители
  storage1.id := 13;
  storage1.cTypeId := 5;
  storage1.manufacturer_name := 'Western Digital';
  storage1.model_name := 'Blue SN570';
  storage1.description := 'SSD, NVMe, 500 ГБ';
  storage1.cost := 4000;
  storage1.availability := 20;
  AddComponent(componentsList, storage1);
  
  storage2.id := 14;
  storage2.cTypeId := 5;
  storage2.manufacturer_name := 'Samsung';
  storage2.model_name := '970 EVO Plus';
  storage2.description := 'SSD, NVMe, 1 ТБ';
  storage2.cost := 8000;
  storage2.availability := 15;
  AddComponent(componentsList, storage2);
  
  storage3.id := 15;
  storage3.cTypeId := 5;
  storage3.manufacturer_name := 'Samsung';
  storage3.model_name := '980 PRO';
  storage3.description := 'SSD, NVMe, 2 ТБ';
  storage3.cost := 16000;
  storage3.availability := 7;
  AddComponent(componentsList, storage3);
  
  // Создание совместимости
  // Совместимость процессоров и материнских плат
  compat1.left_id := 1; // Intel Core i5-12400F
  compat1.right_id := 4; // ASUS PRIME B660-PLUS
  AddCompatibility(compatibilityList, compat1);
  
  compat2.left_id := 2; // AMD Ryzen 5 5600X
  compat2.right_id := 5; // MSI B550 TOMAHAWK
  AddCompatibility(compatibilityList, compat2);
  
  compat3.left_id := 3; // Intel Core i7-12700K
  compat3.right_id := 6; // ASUS ROG STRIX Z690-F
  AddCompatibility(compatibilityList, compat3);
  
  compat4.left_id := 3; // Intel Core i7-12700K
  compat4.right_id := 4; // ASUS PRIME B660-PLUS
  AddCompatibility(compatibilityList, compat4);
  
  // Совместимость материнских плат и оперативной памяти
  compat5.left_id := 4; // ASUS PRIME B660-PLUS
  compat5.right_id := 7; // Kingston FURY Beast DDR4
  AddCompatibility(compatibilityList, compat5);
  
  compat6.left_id := 5; // MSI B550 TOMAHAWK
  compat6.right_id := 7; // Kingston FURY Beast DDR4
  AddCompatibility(compatibilityList, compat6);
  
  compat7.left_id := 4; // ASUS PRIME B660-PLUS
  compat7.right_id := 8; // Corsair Vengeance LPX DDR4
  AddCompatibility(compatibilityList, compat7);
  
  compat8.left_id := 5; // MSI B550 TOMAHAWK
  compat8.right_id := 8; // Corsair Vengeance LPX DDR4
  AddCompatibility(compatibilityList, compat8);
  
  compat9.left_id := 6; // ASUS ROG STRIX Z690-F
  compat9.right_id := 9; // G.Skill Trident Z5 DDR5
  AddCompatibility(compatibilityList, compat9);
  
  // Совместимость материнских плат и накопителей
  // Все материнские платы совместимы со всеми накопителями
  
  // ASUS PRIME B660-PLUS и Western Digital Blue SN570
  compat1.left_id := 4;
  compat1.right_id := 13;
  AddCompatibility(compatibilityList, compat1);
  
  // ASUS PRIME B660-PLUS и Samsung 970 EVO Plus
  compat2.left_id := 4;
  compat2.right_id := 14;
  AddCompatibility(compatibilityList, compat2);
  
  // ASUS PRIME B660-PLUS и Samsung 980 PRO
  compat3.left_id := 4;
  compat3.right_id := 15;
  AddCompatibility(compatibilityList, compat3);
  
  // MSI B550 TOMAHAWK и Western Digital Blue SN570
  compat4.left_id := 5;
  compat4.right_id := 13;
  AddCompatibility(compatibilityList, compat4);
  
  // MSI B550 TOMAHAWK и Samsung 970 EVO Plus
  compat5.left_id := 5;
  compat5.right_id := 14;
  AddCompatibility(compatibilityList, compat5);
  
  // MSI B550 TOMAHAWK и Samsung 980 PRO
  compat6.left_id := 5;
  compat6.right_id := 15;
  AddCompatibility(compatibilityList, compat6);
  
  // ASUS ROG STRIX Z690-F и Western Digital Blue SN570
  compat7.left_id := 6;
  compat7.right_id := 13;
  AddCompatibility(compatibilityList, compat7);
  
  // ASUS ROG STRIX Z690-F и Samsung 970 EVO Plus
  compat8.left_id := 6;
  compat8.right_id := 14;
  AddCompatibility(compatibilityList, compat8);
  
  // ASUS ROG STRIX Z690-F и Samsung 980 PRO
  compat9.left_id := 6;
  compat9.right_id := 15;
  AddCompatibility(compatibilityList, compat9);
  
  // Сохранение данных в файлы
  SaveComponentTypes(componentTypesList, 'data/component_types.dat');
  SaveComponents(componentsList, 'data/components.dat');
  SaveCompatibility(compatibilityList, 'data/compatibility.dat');
  
  WriteLn('Тестовые данные успешно сгенерированы и сохранены в файлы:');
  WriteLn('- data/component_types.dat');
  WriteLn('- data/components.dat');
  WriteLn('- data/compatibility.dat');
  
  // Освобождение памяти
  ClearComponentTypeList(componentTypesList);
  ClearComponentList(componentsList);
  ClearCompatibilityList(compatibilityList);
end.
