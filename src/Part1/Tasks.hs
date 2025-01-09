module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
normalizeAngle :: Double -> Double
normalizeAngle x = x - (2 * pi) * fromIntegral (floor (x / (2 * pi)))

myFactorial :: Integer -> Integer
myFactorial x = if x <= 1 then 1 else x * myFactorial (x - 1)

myPowDouble :: Double -> Integer -> Double
myPowDouble x 0 = 1
myPowDouble x 1 = x
myPowDouble x y = x * myPowDouble x (y - 1) 

mySinTaylor :: Double -> Integer -> Double
mySinTaylor x k =
  (myPowDouble (-1) (k - 1)) * (myPowDouble x (fromIntegral (2 * k - 1))) / fromIntegral (myFactorial (2 * k - 1))

mySinIncrement :: Double -> Integer -> Double -> Double
mySinIncrement x n sum =
    if abs (mySinTaylor x n) <= 0.0001
    then sum + mySinTaylor x n
    else mySinIncrement x (n + 1) (sum + mySinTaylor x n)

mySin :: Double -> Double
mySin x = mySinIncrement (normalizeAngle x) 1 0.0

-- косинус числа (формула Тейлора)
myCosTaylor :: Double -> Integer -> Double
myCosTaylor x k =
  (myPowDouble (-1) k) * (myPowDouble x (fromIntegral (2 * k))) / fromIntegral (myFactorial (2 * k))

myCosIncrement :: Double -> Integer -> Double -> Double
myCosIncrement x n sum =
    if abs (myCosTaylor x n) <= 0.0001
    then sum + myCosTaylor x n
    else myCosIncrement x (n + 1) (sum + myCosTaylor x n)

myCos :: Double -> Double
myCos x = myCosIncrement (normalizeAngle x) 0 0.0

-- наибольший общий делитель двух чисел
myGCDHelper :: Integer -> Integer -> Integer
myGCDHelper x y = 
    if (y == 0)
    then x
    else myGCDHelper y (rem x y)

myGCD :: Integer -> Integer -> Integer
myGCD x y = 
    if (x >= y)
    then myGCDHelper (abs x) (abs y)
    else myGCDHelper (abs y) (abs x)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isLeapYear :: Integer -> Bool
isLeapYear year
  | (year `mod` 4 == 0) && (year `mod` 100 /= 0) = True
  | (year `mod` 400 == 0) = True
  | otherwise = False

isDayCorrectSmall :: Integer -> Integer -> Bool
isDayCorrectMedium :: Integer -> Bool
isDayCorrectLarge :: Integer -> Bool

isDayCorrectSmall day year
  | (day > 0) && (isLeapYear year) && (day <=29) = True
  | (day > 0) && (day <= 28) = True
  | otherwise = False
isDayCorrectMedium day =
    if (day < 1) || (day > 30)
    then False
    else True
isDayCorrectLarge day =
    if (day < 1) || (day > 31)
    then False
    else True

isDayCorrect :: Integer -> Integer -> Integer -> Bool
isDayCorrect day 01 year = isDayCorrectLarge day
isDayCorrect day 02 year = isDayCorrectSmall day year
isDayCorrect day 03 year = isDayCorrectLarge day
isDayCorrect day 04 year = isDayCorrectMedium day
isDayCorrect day 05 year = isDayCorrectLarge day
isDayCorrect day 06 year = isDayCorrectMedium day
isDayCorrect day 07 year = isDayCorrectLarge day
isDayCorrect day 08 year = isDayCorrectLarge day
isDayCorrect day 09 year = isDayCorrectMedium day
isDayCorrect day 10 year = isDayCorrectLarge day
isDayCorrect day 11 year = isDayCorrectMedium day
isDayCorrect day 12 year = isDayCorrectLarge day

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year =
  if (year < 1) || (month < 1) || (month > 12)
  then False
  else isDayCorrect day month year

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x 1 = x
myPow x y = x * myPow x (y - 1) 

-- является ли данное число простым?
isPrimeHelp :: Integer -> Integer -> Bool
isPrimeHelp x del 
  | del < 2         = True
  | x `mod` del == 0 = False
  | otherwise        = isPrimeHelp x (del - 1)

-- Основная функция для проверки числа на простоту
isPrime :: Integer -> Bool
isPrime x 
  | x < 2     = False
  | otherwise = isPrimeHelp x (floor . sqrt $ fromIntegral x)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
gaussFormula :: [Point2D] -> Double
gaussFormula [] = 0
gaussFormula [p] = 0
gaussFormula ((x1, y1):(x2, y2):ps) = (x1 * y2 - y1 * x2) + gaussFormula ((x2, y2):ps)

shapeArea :: [Point2D] -> Double
shapeArea points = abs (gaussFormula (points ++ [head points])) / 2

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
isTriangle :: Double -> Double -> Double -> Bool
isTriangle x y z = x + y > z && x + z > y && y + z > x

isInvalid :: Double -> Double -> Double -> Bool
isInvalid x y z = x <= 0 || y <= 0 || z <= 0 || (not (isTriangle x y z))

-- Теорема Пифагора
isRightTriangle :: Double -> Double -> Double -> Bool
isRightTriangle x y z = (x * x + y * y == z * z) || (x * x + z * z == y * y) || (y * y + z * z == x * x)

-- Сумма квадратов двух других сторон должны быть больше
isAcuteTriangle :: Double -> Double -> Double -> Bool
isAcuteTriangle x y z = 
  (x * x < y * y + z * z) && 
  (y * y < x * x + z * z) && 
  (z * z < x * x + y * y)

triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | isInvalid a b c = -1
    | isAcuteTriangle a b c = 1
    | isRightTriangle a b c = 2
    | otherwise = 0
