module Lab6 where

-- exercise 6.4
data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

_toLitValues :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
_toLitValues func (Just a) (Just b) = Just $ a `func` b
_toLitValues _ _ _ = Nothing

eval :: Expr -> Maybe Int
eval (Lit n) = Just n
eval (Div e1 (Lit 0)) = Nothing
eval (Add e1 e2) = _toLitValues (+) (eval e1) (eval e2)
eval (Sub e1 e2) = _toLitValues (-) (eval e1) (eval e2)
eval (Mul e1 e2) = _toLitValues (*) (eval e1) (eval e2)
eval (Div e1 e2) = _toLitValues div (eval e1) (eval e2)

-- exercise 6.7
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)
