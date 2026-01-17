<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Гуров Іван Олегович</p>
<p align="right"><b>Рік</b>: 2026</p>

## Загальне завдання

Реалізувати дві частини:
1. **Рефакторинг функціонального алгоритму сортування** з використанням функцій вищого порядку (mapcar, reduce тощо) та іменованих функцій (labels/flet)
2. **Генератор замикання** для використання з функцією reduce

## Варіант 6

**Частина 1:** Рефакторинг Selection Sort з використанням функцій вищого порядку

**Частина 2:** Генератор замикання `rpropagation-reducer` для правостороннього поширення значень

---

## Частина 1: Рефакторинг функціонального сортування

### Лістинг реалізації

```lisp
(defun selection-sort-functional (lst &key (key #'identity) (test #'<))
  "Functional Selection Sort using higher-order functions.
   - lst: The list to sort.
   - key: Function to apply to elements before comparison.
   - test: Comparator function (default is <).
   Returns a new sorted list."

  (labels ((remove-first (target sequence)
             (cond
               ((null sequence) nil)
               ((equal (car sequence) target) (cdr sequence))
               (t (cons (car sequence) (remove-first target (cdr sequence))))))

           (find-best-pair (pairs)
             (reduce (lambda (best current)
                       (if (funcall test (car current) (car best))
                           current
                           best))
                     pairs))

           (sort-pairs (pairs)
             (if (null pairs)
                 nil
                 (let* ((best-pair (find-best-pair pairs))
                        (remaining-pairs (remove-first best-pair pairs))))
                   (cons (cdr best-pair)
                         (sort-pairs remaining-pairs))))))

    (let ((keyed-list (mapcar (lambda (x) (cons (funcall key x) x)) lst)))
      
      (sort-pairs keyed-list))))
```

### Опис реалізації

**Використані функції вищого порядку:**

1. **mapcar** - перетворює вхідний список у список пар `(ключ . значення)`, де ключ обчислюється за допомогою функції `key`

2. **reduce** - знаходить найкращу (мінімальну/максимальну) пару серед списку пар, порівнюючи їх за ключами за допомогою функції `test`

3. **labels** - визначає локальні іменовані функції:
   - `remove-first` - видаляє перше входження елемента зі списку
   - `find-best-pair` - знаходить пару з найкращим ключем (використовує reduce)
   - `sort-pairs` - рекурсивно сортує список пар

**Параметри функції:**
- `lst` - список для сортування
- `:key` - функція для витягування значення для порівняння (за замовчуванням `#'identity`)
- `:test` - функція порівняння (за замовчуванням `#'<`)

**Алгоритм роботи:**
1. Створюється список пар `(ключ . елемент)` за допомогою `mapcar`
2. На кожному кроці рекурсії знаходиться пара з найкращим ключем
3. Ця пара додається до результату, а залишок сортується рекурсивно

### Тестування частини 1

```lisp
CL-USER> (run-tests)

========== PART 1: SORTING TESTS ==========
Test 1 (Basic numbers): (1 1 2 3 4 5 6 9)
Test 2 (Strings by length): (fig kiwi apple banana)
Test 3 (Pairs by second element): ((:B 5) (:A 10) (:C 20))
Test 4 (Descending order): (8 5 3 2 1)
```

**Аналіз результатів:**
- Тест 1: Базове сортування чисел за зростанням
- Тест 2: Сортування рядків за довжиною (`:key #'length`)
- Тест 3: Сортування пар за другим елементом (`:key #'second`)
- Тест 4: Сортування за спаданням (`:test #'>`)

---

## Частина 2: Генератор замикання

### Лістинг реалізації

```lisp
(defun rpropagation-reducer (&key (comparator #'<))
  "Returns a closure to be used with REDUCE.
   Arguments:
     - comparator: Function to compare elements (default <).
   Usage in REDUCE:
     - :from-end must be T.
     - :initial-value must be NIL (to signify the end of the list)."
  
  (lambda (elem acc)
    (if (null acc)
        (list elem)

        (let ((prev-best (car acc)))
          (if (funcall comparator elem prev-best)
              (cons elem acc)
              (cons prev-best acc))))))
```

### Опис роботи замикання

**Призначення:** Функція повертає замикання, яке поширює "найкращі" значення справа наліво при обробці списку функцією `reduce`.

**Параметри:**
- `:comparator` - функція порівняння (за замовчуванням `#'<`)

**Принцип роботи:**
1. Замикання приймає два аргументи: `elem` (поточний елемент) та `acc` (акумулятор)
2. Якщо акумулятор порожній (`nil`), створюється новий список з одного елемента
3. Інакше порівнюється поточний елемент з "найкращим" елементом з акумулятора
4. Якщо поточний елемент краще - він додається до акумулятора
5. Якщо ні - до акумулятора додається попередній найкращий елемент

**Важливо:** Для коректної роботи `reduce` має викликатись з параметрами:
- `:from-end t` - обробка справа наліво
- `:initial-value nil` - початкове значення акумулятора

### Тестування частини 2

```lisp
========== PART 2: REDUCER TESTS ==========
Test A (3 2 1 2 3): (1 1 1 2 3)
Test B (3 1 4 2):   (1 1 2 2)
Test C (1 2 3) [>]: (3 3 3)

========== TESTS FINISHED ==========
```

### Пояснення тестів

**Test A: `(3 2 1 2 3)` з компаратором `<`**

Обробка справа наліво:
1. `3` vs `nil` → `(3)`
2. `2` vs `3`: `2 < 3` = T → `(2 3)`
3. `1` vs `2`: `1 < 2` = T → `(1 2 3)`
4. `2` vs `1`: `2 < 1` = NIL → `(1 1 2 3)` (поширюється 1)
5. `3` vs `1`: `3 < 1` = NIL → `(1 1 1 2 3)` (поширюється 1)

**Результат:** `(1 1 1 2 3)` 

**Test B: `(3 1 4 2)` з компаратором `<`**

Обробка справа наліво:
1. `2` vs `nil` → `(2)`
2. `4` vs `2`: `4 < 2` = NIL → `(2 2)` (поширюється 2)
3. `1` vs `2`: `1 < 2` = T → `(1 2 2)`
4. `3` vs `1`: `3 < 1` = NIL → `(1 1 2 2)` (поширюється 1)

**Результат:** `(1 1 2 2)` 

**Test C: `(1 2 3)` з компаратором `>`**

Обробка справа наліво:
1. `3` vs `nil` → `(3)`
2. `2` vs `3`: `2 > 3` = NIL → `(3 3)` (поширюється 3)
3. `1` vs `3`: `1 > 3` = NIL → `(3 3 3)` (поширюється 3)

**Результат:** `(3 3 3)` 

---
