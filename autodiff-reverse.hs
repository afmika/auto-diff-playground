data Value a = Val a
    | UnaryOp String a (Value a)
    | BinaryOp String a (Value a) (Value a)
    deriving (Eq, Show)

data Gradient a = Grad a | GradPair (Gradient a) (Gradient a)
    deriving (Eq, Show)

instance Functor Value where
    fmap f (Val v) = Val (f v)
    fmap f (BinaryOp _ v _ _) =Val (f v)
    fmap f (UnaryOp _ v _) = Val (f v)


getVal :: Value a -> a
getVal (Val v) = v
getVal (BinaryOp _ v _ _) = v
getVal (UnaryOp _ v _) = v


instance Num a => Num (Value a) where
    (+) left right = BinaryOp "+" (getVal left + getVal right) left right
    (*) left right = BinaryOp "*" (getVal left * getVal right) left right
    abs = fmap abs
    signum = fmap signum
    fromInteger n = Val (fromInteger n)
    negate = fmap negate


-- topdown approach
grad :: Float -> Value Float -> Gradient Float
grad seed (Val _) = Grad seed
grad seed (BinaryOp op _ c1 c2) = case op of
                    "+" -> GradPair (grad seed c1) (grad seed c2)
                    "*" -> GradPair (grad (seed * getVal c2) c1) (grad (seed * getVal c1) c2)
                    v -> error ("operator " ++ v ++ " not supported")

-- TODO generalized
-- eval (Grad g) = g
-- eval (GradPair g g') = eval g + eval g'

-- -- handles 1D for now (+ no scalars)
-- f x = 3*x*x -- f' -> 3*(2+1)

-- main = do
--     let g = grad 1 (f 2)
--     print $ show g
--     print $ show $ eval g -- 17