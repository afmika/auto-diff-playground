data Value a = Val a a deriving (Eq, Show)

instance Functor Value where
    fmap f (Val d g) = Val (f d) (f g)

-- dual number approach
-- Val (usual ops) (partial derivative so far)
instance Num a => Num (Value a) where
    (+) (Val u du) (Val v dv) = Val (u + v) (du + dv)
    (*) (Val u du) (Val v dv) = Val (u * v) (u * dv + v * du)
    abs = fmap abs
    signum = fmap signum
    fromInteger n = Val (fromInteger n) 0
    negate = fmap negate

instance Fractional a => Fractional (Value a) where
  (/) (Val u du) (Val v dv) = Val (u /v) ((u * dv + v * du) / v^2)
  recip (Val u du) = Val (1 / u) (-du / u^2)
  fromRational r = Val (fromRational r) 0

instance Floating a => Floating (Value a) where
  pi = Val pi 0
  exp (Val u du) = Val (exp u) (du * exp u)
  log (Val u du) = Val (log u) (du / u)
  sin (Val u du) = Val (sin u) (du * cos u)
  cos (Val u du) = Val (cos u) (-du * sin u)

-- dual algebra for a few functions
pow' :: Value Float -> Float -> Value Float
pow' (Val u du) n = Val (u ** n) (n * du * (u ** (n - 1)))
-- .. more functions todo!


-- utils
dual :: Float -> Value Float
dual x = Val x 0

eps = Val 0 0.001 :: Value Float


-- definitions
f :: (Value Float, Value Float) -> Value Float
f (x, y) = 4 * x - (pow' x 2 + pow' y 2)

df :: (Value Float, Value Float) -> (Value Float, Value Float)
df (x, y) = (
        f (x + eps, y),
        f (x, y + eps)
    )

{-
    Since f = 4 - (x^2 + y^2)
    Jacobian = [4-2x  -2y]
    Critical point at [2 0]
-}

main = do
    let (Val _ dx, Val _ dy) = df (dual 2, dual 0)
    print $ "df/dx = " ++ show dx -- 0
    print $ "df/dy = " ++ show dy -- 0
