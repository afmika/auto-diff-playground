data Value a = Val a a deriving (Eq, Show)

instance Functor Value where
  fmap f (Val d g) = Val (f d) (f g)

-- dual number approach
-- Val (usual ops) (partial derivative so far)
instance (Num a) => Num (Value a) where
  (+) (Val u du) (Val v dv) = Val (u + v) (du + dv)
  (*) (Val u du) (Val v dv) = Val (u * v) (v * du + u * dv)
  abs (Val u du) = Val (abs u) (du * signum u)
  signum = fmap signum
  fromInteger n = Val (fromInteger n) 0
  negate = fmap negate

instance (Fractional a) => Fractional (Value a) where
  (/) (Val u du) (Val v dv) = Val (u / v) ((v * du - u * dv) / v ^ 2)
  recip (Val u du) = Val (1 / u) (-du / u ^ 2)
  fromRational r = Val (fromRational r) 0

instance (Floating a) => Floating (Value a) where
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

seed = Val 0 1 :: Value Float

-- definitions

type Dim2Function = (Value Float, Value Float) -> Value Float

f :: Dim2Function
f (x, y) = 4 * x - (pow' x 2 + pow' y 2)

g :: Dim2Function
g (x, y) = 4 * x - (x ^ 2 + y ^ 2) -- x^n = x * x * x * ... * x (so it follows dual(x) * dual(x) * ... )

d :: Dim2Function -> (Value Float, Value Float) -> (Value Float, Value Float)
d f (x, y) =
  ( -- seed <- dx/dx = 1
    f (x + seed, y),
    -- seed <- dy/dy = 1
    f (x, y + seed)
  )

{-
    Since f = 4x - (x^2 + y^2)
    Jacobian = [4-2x  -2y]
    Critical point at [2 0]
-}

main = do
  let (Val _ dx, Val _ dy) = d f (dual 2, dual 0)
  let (Val _ dx', Val _ dy') = d g (dual 2, dual 0)
  putStrLn "# With f"
  putStrLn $ "df/dx = " ++ show dx -- 0
  putStrLn $ "df/dy = " ++ show dy -- 0
  putStrLn "# With g (has ^ operator derived from *)"
  putStrLn $ "df/dx = " ++ show dx' -- 0
  putStrLn $ "df/dy = " ++ show dy' -- 0