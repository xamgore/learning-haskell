-- 1) Выражения

{-
   Вычислите с помощью ghci и сохраните в этом файле значения следующих выражений,
   обращая внимание на обозначения и приоритеты операций, стандартные функции,
   расстановку скобок:

    2 + 3
    mod 10 4
    10 `mod` 4
    True && 5 < 10
    5 < 7 || 10 > 3
    sqrt (-2)
    sqrt (sqrt 16)
    abs 5 + abs (-5)
    max 5 10
    let x = 4 in (sin x)^2 + (cos x)^2
    x
    let x = 4
    x
    7^(-1)
    pi/2
    undefined
    error "AAAA!!!!"
    12345^54321
    2 < 3 || 9999954321^99912345 > 12345^54321
    if not False then 1 else 0
    if not False then 1
    let b = if not False then 1 else 0
    b
   Пример оформления ответов:
   > 2 + 3
   5

-}

{-
> 2 + 3
5

> mod 10 4
2

> 10 `mod` 4
2

> True && 5 < 10
True

> 5 < 7 || 10 > 3
True

> sqrt (-2)
NaN

> sqrt (sqrt 16)
2.0

> abs 5 + abs (-5)
10

> max 5 10
10

> let x = 4 in (sin x)^2 + (cos x)^2
1.0

> x
error: Variable not in scope

> let x = 4
> x
4

> 7^(-1)
Exception: Negative exponent

> pi/2
1.5707963267948966

> undefined
Exception: Prelude.undefined

> error "AAAA!!!!"
error:
    • Syntax error on 'aAAA'
      Perhaps you intended to use TemplateHaskell or TemplateHaskellQuotes
    • In the Template Haskell quotation 'aAAA'

> 12345^54321
772203435359258784484805649467339367998780084781946428344052790987879786123275795259301164555263070067806678378810767998870...

> 2 < 3 || 9999954321^99912345 > 12345^54321
True

> if not False then 1 else 0
1

> if not False then 1
parse error (possibly incorrect indentation or mismatched brackets)

> let b = if not False then 1 else 0
> b
1

-}

-- 2) Типы

{-
  Тип выражения можно узнать, воспользовавшись командой интерпретатора :t, например:

> :t 'a'
'a' :: Char
> :t 1
1 :: Num a => a

  Запись "1 :: Num a => a" означает, что выражение "1" имеет тип "a", где "a" принадлежит
  классу типов Num (имеет экземпляр класса типов Num, является числовым типом).

  Определите и сохраните в этом файле типы следующих выражений:
   5
   5.0
   '5'
   "5"
   abs 4
   abs 4.0
   sqrt 4
   sqrt 4.0
   max 4 9
   2 + 3
   5 < 7
   if 2 > 3 then 7 else 5
   5 > 6 && False
   pi

   Команда ":set +t" включает режим, при котором печатается тип каждого вычисляемого выражения.
   Команда ":set +s" включает режим, при котором печатается время вычисления каждого выражения.

-}

{-
*Main> :t 5
5 :: Num t => t
*Main> :t 5.0
5.0 :: Fractional t => t
*Main> :t '5'
'5' :: Char
*Main> :t "5"
"5" :: [Char]
*Main> :t abs 4
abs 4 :: Num a => a
*Main> :t abs 4.0
abs 4.0 :: Fractional a => a
*Main> :t sqrt 4
sqrt 4 :: Floating a => a
*Main> :t sqrt 4.0
sqrt 4.0 :: Floating a => a
*Main> :t max 4 9
max 4 9 :: (Ord a, Num a) => a
*Main> :t 2 + 3
2 + 3 :: Num a => a
*Main> :t 5 < 7
5 < 7 :: Bool
*Main> :t if 2 > 3 then 7 else 5
if 2 > 3 then 7 else 5 :: Num t => t
*Main> :t 5 > 6 && False
5 > 6 && False :: Bool
*Main> :t pi
pi :: Floating a => a

-}

-- Не забудьте зафиксировать изменения
