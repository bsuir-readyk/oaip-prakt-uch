
Unit UiLists;

Interface

Uses 
SysUtils, Crt,
UiUtils in 'src/ui/utils.pas';

Procedure ShowMainMenu;
Procedure ShowSpecialFunctionsSubmenu;
Procedure ShowCRUD_DS;

Implementation

Procedure ShowMainMenu;
Begin
    ClrScr;
    PrintHeader('ГЛАВНОЕ МЕНЮ');
    WriteLn('1. Чтение данных из файла');
    WriteLn('2. Просмотр списка');
    WriteLn('3. Сортировка данных');
    WriteLn('4. Поиск данных с использованием фильтров');
    WriteLn('5. Добавление данных');
    WriteLn('6. Удаление данных');
    WriteLn('7. Редактирование данных');
    WriteLn('8. Специальные функции');
    WriteLn('9. Выход без сохранения');
    WriteLn('10. Выход с сохранением');
    PrintSeparator;
End;

Procedure ShowCRUD_DS;
Begin
    ClrScr;
    PrintHeader('ПРОСМОТР СПИСКА');
    WriteLn('1. Комплектующие');        // GData.Components
    WriteLn('2. Типы комплектующих');   // GData.ComponentTypes
    WriteLn('3. Совместимость');        // GData.Compatabilities
    WriteLn('4. Заказы');               // GData.Orders

    WriteLn('0. Назад');
    PrintSeparator;
End;

Procedure ShowSpecialFunctionsSubmenu;
Begin
    ClrScr;
    PrintHeader('СПЕЦИАЛЬНЫЕ ФУНКЦИИ');
    WriteLn('1. Подбор вариантов комплектации компьютера');
    WriteLn('2. Оформление заказа');
    WriteLn('3. Поиск совместимых комплектующих заданного типа');
    WriteLn('0. Назад');
    PrintSeparator;
End;

End.
