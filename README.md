# Язык программирования на LISP

Здесь представлен язык для заполнения двоичной матрицы.

Вам дана матрица 2х12. Вы находитесь в клетке (0, 0).

Вам доступны следующие операции:
- `up`: шаг вверх
- `down`: шаг вниз
- `left`: шаг влево
- `right`: шаг вниз
- `revert`: изменить ячейку (0 на 1, 1 на 0)
- `print-matrix`: печать матрицы на экран

Команды необходимо записать в файл (каждая в отдельной строку) и передать в качестве аргумента файлу `lang_interpreter.lisp`.

Пример запуска: `clisp .\lang_interpreter.lisp "commands.txt"`
