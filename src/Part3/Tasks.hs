module Part3.Tasks where

import Util (notImplementedYet)

import Data.List (maximumBy, group, sort)
import Data.Ord (comparing)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
-- lst :: [Int]
fincHelper :: [a] -> (Int -> a) -> Int -> [a]
fincHelper list func arg = (func arg) : (fincHelper list func (arg + 1))

finc :: (Int -> a) -> Int -> [a]
finc func arg = fincHelper [] func arg

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ffHelper :: (a -> a) -> a -> [a]
ffHelper func arg = arg : ffHelper func (func arg)

ff :: (a -> a) -> a -> [a]
ff func arg = ffHelper func arg

-- Преобразование числа в список цифр
digits :: Int -> [Int]
digits n = map (read . (:[])) (show (abs n))

-- Основная функция
mostFreq :: [Int] -> Int
mostFreq numbers =
    let allDigits = concatMap digits numbers
        groupedDigits = group . sort $ allDigits
        freqList = map (\g -> (head g, length g)) groupedDigits
    in fst $ maximumBy (comparing snd) freqList

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = if x `elem` xs then uniq xs else x : uniq xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = [(key, filter ((== key) . f) lst) | key <- uniqueKeys]
  where
    uniqueKeys = uniq (map f lst)
    uniq [] = []
    uniq (x:xs) = if x `elem` xs then uniq xs else x : uniq xs
