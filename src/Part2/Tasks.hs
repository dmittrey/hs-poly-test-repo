module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant lhs) (IntConstant rhs) = IntConstant (lhs + rhs)
(|+|) lhs rhs = BinaryTerm Plus lhs rhs
infixl 7 |+|
(|-|) :: Term -> Term -> Term
(|-|) (IntConstant lhs) (IntConstant rhs) = IntConstant (lhs - rhs)
(|-|) lhs rhs = BinaryTerm Minus lhs rhs
infixl 7 |-|
(|*|) :: Term -> Term -> Term
(|*|) (IntConstant lhs) (IntConstant rhs) = IntConstant (lhs * rhs)
(|*|) lhs rhs = BinaryTerm Times lhs rhs
infixl 8 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant expression) = IntConstant expression
replaceVar varName replacement (Variable expression) = 
   if (expression == varName)
   then replacement
   else Variable expression
replaceVar varName replacement (BinaryTerm op lhv rhv) = BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus innerLHS innerRHS) = (evaluate innerLHS) |+| (evaluate innerRHS)
evaluate (BinaryTerm Minus innerLHS innerRHS) = (evaluate innerLHS) |-| (evaluate innerRHS)
evaluate (BinaryTerm Times innerLHS innerRHS) = (evaluate innerLHS) |*| (evaluate innerRHS)
evaluate (IntConstant val) = IntConstant val
evaluate (Variable name) = Variable name -- Не заходим но покрываем случай