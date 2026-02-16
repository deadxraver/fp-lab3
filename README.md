# fp-lab3

## Задание

В данной ЛР требовалось реализовать вычматовские алгоритмы (линейный и лагранж в моем случае) интерполяции.
Сделать это все надо было в функциональном стиле и чтобы работало в потоковом режиме (получили достаточно значений - посчитали).

## Ключевые моменты реализации

Обработку конфигураций и т.д. можно найти в [Main.hs](app/Main.hs), это все не очень интересно.

Вот сами алгоритмы интерполяции из [Lib.hs](src/Lib.hs):

```Haskell
-- linear interpolation takes two first numbers if present and interpolates
--                   x          X           Y           res
linearInterpolate :: Double -> [Double] -> [Double] -> Maybe Double
linearInterpolate x (x2 : x1 : _) (y2 : y1 : _) = Just $ y2 + (y1 - y2) / (x1 - x2) * (x - x2)
linearInterpolate _ _ _ = Nothing

--                     x          X           Y           res
lagrangeInterpolate :: Double -> [Double] -> [Double] -> Double
lagrangeInterpolate _ _ [] = 0
lagrangeInterpolate x xarr (y1 : yarr) = y1 * lagrangeInterpolate' xarr + lagrangeInterpolate x xarr yarr
  where
    x1i = xarr !! (length xarr - length yarr - 1)
    lagrangeInterpolate' [] = 1
    lagrangeInterpolate' (x1' : xarr') = (if x1' == x1i then 1 else (x - x1') / (x1i - x1')) * lagrangeInterpolate' xarr'
```

А также в файле содержится вспомогательная функция, которая интерполирует по Лагранжу используя 
заданное количество верхних точек, а не всех, что имеются.

Для рассчета окна использую вспомогательные функции 
из [Main.hs](app/Main.hs)

```Haskell
valsBetween x step lower upper
    | x >= upper = []
    | x < lower = valsBetween (x + step) step lower upper
    | otherwise = x : valsBetween (x + step) step lower upper

calcToBound [] _ _ = return ()
calcToBound (x : xarr) method sign = do
    case method x of
        Just y -> putStrLn $ sign ++ show x ++ " " ++ show y
        Nothing -> return ()
    calcToBound xarr method sign
```

Для потоковой обработки использую "неблокирующий" `getLine` и проверяю,
есть ли еще ввод на каждом шаге с помощью `isEOF`,
чтобы программа завершилась не аварийно при закрытии потока.

## Ввод/вывод программы

`>` означает ввод с клавиатуры,
`^D` - конец потока,
`$` - пользовательская команда

```bash
$ stack run -- --step 0.7 --linear
step=0.7
> 0 0
> 1 1
linear: 0.0 0.0
linear: 0.7 0.7
> 2 2
linear: 1.4 1.4
> 3 3
linear: 2.0999999999999996 2.0999999999999996
linear: 2.8 2.8
> ^D
linear: 3.0 3.0
```

```bash
$ stack run -- --step 1.5 --linear --lagrange -n 2
step=1.5
n=2
> 0 0
> 1 1
linear: 0.0 0.0
lagrange: 0.0 0.0
> 2 2
linear: 1.5 1.5
lagrange: 1.5 1.5
> 3 3
> 4 4
linear: 3.0 3.0
lagrange: 3.0 3.0
> ^D
linear: 4.0 4.0
lagrange: 4.0 4.0
```

```bash
$ stack run -- --step 0.5 --lagrange -n 4
step=0.5
n=4
> 0 0
> 1 1
linear: 0.0 0.0
linear: 0.5 0.5
> 2 2
linear: 1.0 1.0
linear: 1.5 1.5
> 3 3
linear: 2.0 2.0
linear: 2.5 2.5
lagrange: 0.0 0.0
lagrange: 0.5 0.5
lagrange: 1.0 1.0
lagrange: 1.5 1.5
lagrange: 2.0 2.0
lagrange: 2.5 2.5
> 4 4
linear: 3.0 3.0
linear: 3.5 3.5
lagrange: 3.0 3.0
lagrange: 3.5 3.5
> 5 5
linear: 4.0 4.0
linear: 4.5 4.5
lagrange: 4.0 4.0
lagrange: 4.5 4.5
> 6 6
linear: 5.0 5.0
linear: 5.5 5.5
lagrange: 5.0 5.0
lagrange: 5.5 5.5
> 7 7
linear: 6.0 6.0
linear: 6.5 6.5
lagrange: 6.0 6.0
lagrange: 6.5 6.5
> 8 8
linear: 7.0 7.0
linear: 7.5 7.5
lagrange: 7.0 7.0
lagrange: 7.5 7.5
> ^D
linear: 8.0 8.0
lagrange: 8.0 8.0
```

## Выводы

В ходе выполнения лабораторной работы познакомился с 
методми потоковой обработки входных данных в хаскеле,
узнал больше про ввод/вывод, набил руку на использовании
ленивости в коде.
