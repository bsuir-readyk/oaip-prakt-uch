4. РУКОВОДСТВО ПО УСТАНОВКЕ И ЭКСПЛУА-ТАЦИИ

4.1. Установка программы

1.	Системные требования:

•	Операционная система: Windows, Linux, macOS (программа написана на Free Pascal)

•	Консольное приложение с минимальными требованиями к ресурсам системы

•	**Для сборки из исходного кода:** требуется компилятор Free Pascal (`fpc`) и утилита `make`.
2.	Процесс установки и сборки:

•	**Сборка из исходного кода:**
    - Откройте терминал в корневой директории проекта.
    - Для компиляции программы выполните команду `make build`.
    - Исполняемый файл `bin_solve` будет создан в корневой директории.
    - В Linux и macOS может потребоваться дать права на исполнение: `chmod 777 bin_solve`.
    - Для запуска используйте `./bin_solve` или `make run`.
    - Команда `make` (или `make default`) выполнит полную сборку, очистку и запуск.
    - Команда `make clean` удалит временные файлы.


•	**Использование готового файла:**
    - Программа не требует установки. Исполняемый файл может уже находиться в папке программы.
    - Для запуска программы достаточно открыть файл `solve.exe` (Windows) или `bin_solve` (Linux/macOS) двойным щелчком мыши или из командной строки

•	Дополнительные файлы перед первым запуском не требуются

•	При первом запуске и последующем сохранении данных программа автоматически создаст необходимые типизированные файлы (components.dat, component_types.dat, compatibility.dat, orders.dat) в директории data/

•	При выполнении специальных функций будут генерироваться текстовые файлы с результатами подбора ПК и информацией о заказах

4.2. Эксплуатация программы

4.2.1. Запуск программы

После запуска программы пользователю будет представлено главное меню с следующими опциями:
1.	Чтение данных из файла
2.	Просмотр всего списка
3.	Сортировка данных
4.	Поиск данных по различным критериям
5.	Добавление данных в список
6.	Удаление данных из списка
7.	Редактирование данных
8.	Сборка ПК и Заказы (специальные функции)
9.	Сохранение данных в файл
10.	Выход 

4.2.2. Основные функции программы

Загрузка данных:

•	Загрузка данных запрашивает подтверждение пользователя перед заменой текущих данных в памяти

•	Если файлы данных отсутствуют, программа начнет работу с пустыми списками

•	Загружаются типы комплектующих, комплектующие, правила совместимости и заказы
Просмотр данных:

•	Пункт 2 позволяет просматривать списки типов комплектующих, комплектующих, правил совместимости и заказов

•	Реализован постраничный вывод данных при большом объеме информации
Управление типами комплектующих:

•	Добавление типов (пункт 5): ввод кода типа и названия (например, процессор, видеокарта)

•	Редактирование типов (пункт 7): изменение названия типа комплектующих

•	Удаление типов (пункт 6): удаление возможно только если тип не используется в комплектующих
Управление комплектующими:

•	Добавление комплектующих (пункт 5): ввод кода, типа, производителя, модели, параметров, цены и количества на складе

•	Редактирование комплектующих (пункт 7): изменение любых параметров существующей комплектующей

•	Удаление комплектующих (пункт 6): удаление возможно только если комплектующая не используется в правилах совместимости
Управление совместимостью:

•	Добавление правил совместимости (пункт 5): указание двух совместимых комплектующих

•	Редактирование правил (пункт 7): изменение связей между комплектующими

•	Удаление правил (пункт 6): удаление существующих правил совместимости
Специальные функции (пункт 8):

•	Подбор конфигурации ПК в заданном диапазоне цен: автоматический поиск всех возможных сборок из совместимых комплектующих

•	Поиск совместимых комплектующих: поиск компонентов заданного типа, совместимых с выбранным компонентом

•	Оформление заказа: выбор понравившейся конфигурации и создание заказа с генерацией информационного файла
Поиск и сортировка:

•	Поиск данных (пункт 4): поиск по различным критериям в зависимости от типа данных

•	Сортировка данных (пункт 3): сортировка списков по различным параметрам (цена, код, производитель)
Сохранение данных:

•	Для сохранения всех изменений используйте пункт 9

•	Данные сохраняются в типизированные файлы components.dat, component_types.dat, compatibility.dat и orders.dat в директории data/

4.2.3. Форматы ввода данных

Дата: формат ДД.ММ.ГГГГ

•	Дата должна быть действительной и корректной
Коды элементов: положительные целые числа

•	Коды типов комплектующих, комплектующих должны быть уникальными в пределах каждого типа
Текстовые поля: ограничения по длине

•	Производитель: до 30 символов

•	Модель комплектующих: до 50 символов  

•	Параметры и характеристики: до 100 символов

•	Название типа: до 30 символов
Цена: положительное число с не более чем двумя знаками после десятичной точки

•	Цена должна быть больше нуля
Количество на складе: неотрицательное целое число

4.2.4. Работа с файлами

•	Программа автоматически создает и обновляет типизированные файлы данных при сохранении (пункт 9 главного меню)

•	При подборе конфигураций ПК генерируются текстовые файлы с результатами (PC_Config_[номер].txt)

•	При оформлении заказа создается текстовый файл с детальной информацией о заказе (Order_[номер].txt)

•	Файлы данных (components.dat, component_types.dat, compatibility.dat, orders.dat) имеют бинарный формат и не предназначены для ручного редактирования

•	Все файлы данных сохраняются в директории data/ относительно исполняемого файла программы

4.3. Возможные проблемы и их решения

•	Проблема: Программа сообщает, что невозможно загрузить данные. Решение: Убедитесь, что файлы данных находятся в директории data/ относительно исполняемого файла программы.

•	Проблема: Невозможно удалить тип комплектующих. Решение: Тип не может быть удален, если для него существуют комплектующие. Сначала удалите все связанные комплектующие.

•	Проблема: Невозможно удалить комплектующую. Решение: Комплектующая не может быть удалена, если она используется в правилах совместимости. Сначала удалите все связанные правила совместимости.

•	Проблема: Программа не находит данные при поиске. Решение: Убедитесь, что вы вводите критерии поиска точно так же, как они были введены при создании записей (включая регистр букв).

•	Проблема: Невозможно подобрать конфигурацию ПК. Решение: Убедитесь, что в системе есть комплектующие разных типов и между ними установлены правила совместимости.

•	Проблема: Не удается оформить заказ. Решение: Проверьте, что выбранная конфигурация существует в списке подобранных вариантов и все комплектующие доступны на складе.

•	Проблема: Ошибка при сохранении данных. Решение: Убедитесь, что у программы есть права на запись в директорию data/ и достаточно свободного места на диске.
 
