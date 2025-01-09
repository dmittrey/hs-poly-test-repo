{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map (Map, fromList, findWithDefault, empty, keys, elems, insert, foldlWithKey)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       constructFromElem :: Int -> Int -> Int -> mx
       constructFromLists :: [[Int]] -> mx
       height :: mx -> Int
       width :: mx -> Int
       getElem :: mx -> Int -> Int -> Int
       matrixDeterminant :: mx -> Int

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       constructFromElem _ _ x = x
       constructFromLists x = head (head x)
       height _ = 1
       width _ = 1
       getElem m _ _ = m
       matrixDeterminant x = x
instance Matrix [[Int]] where
       constructFromElem w h x = [[if i == j then x else 0 | j <- [0..w-1]] | i <- [0..h-1]]
       constructFromLists lst = lst
       height m = length m
       width m = length (head m)
       getElem m i j = (m !! i) !! j
       matrixDeterminant [] = 1
       matrixDeterminant [[x]] = x
       matrixDeterminant m = 
        let calcMinor majorMatrix col = 
                [[majorMatrix !! i !! j | j <- [0..(length (head majorMatrix) - 1)], j /= col] | i <- [1..(length majorMatrix - 1)]]
        in sum [(-1)^j * (m !! 0 !! j) * matrixDeterminant (calcMinor m j) | j <- [0..(length (m !! 0) - 1)]]
instance Matrix (SparseMatrix Int) where
       constructFromElem w h x 
              | x == 0 = SparseMatrix w h empty
              | otherwise = SparseMatrix {
                     sparseMatrixWidth = w,
                     sparseMatrixHeight = h,
                     sparseMatrixElements = foldl (\acc i -> insert (i, i) x acc) empty [0..min (w-1) (h-1)]
              }
       constructFromLists lst = SparseMatrix 
        { 
            sparseMatrixWidth = length (head lst),
            sparseMatrixHeight = length lst,
            sparseMatrixElements = foldl (\acc (i, row) -> foldl (\acc' (j, val) -> 
                    if val /= 0 then insert (i, j) val acc' else acc') acc (zip [0..] row)) 
                empty 
                (zip [0..] lst)
        }
       height = sparseMatrixHeight
       width = sparseMatrixWidth
       getElem (SparseMatrix _ _ elements) i j = findWithDefault 0 (i, j) elements
       matrixDeterminant m =
        let toListRepresentation (SparseMatrix w h elements) = 
                [[findWithDefault 0 (i, j) elements | j <- [0..w-1]] | i <- [0..h-1]]
        in matrixDeterminant (toListRepresentation m)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = constructFromElem w w 1
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = constructFromElem w h 0
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b = constructFromLists 
       [[sum [getElem a i k * getElem b k j | k <- [0..(width a)-1]] | j <- [0..(height b)-1]] | i <- [0..(width a)]]
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = matrixDeterminant