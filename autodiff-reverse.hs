data Value a = Val a a deriving (Eq, Show)
data Node a = Node (Value a) [Node a]

grad :: Node Float -> Node Float
grad (Node (Val v g) childs) = Node (Val v g') childs'
    where
        childs' = map grad childs
        g' = foldl (\acc (Node (Val _ g) _) -> acc + g) 0 childs'

apply :: (Float -> Float  -> Float) -> (Node Float, Node Float) -> (Node Float, Node Float)
apply f (curr, other) =
    let (Node (Val v g) childs) = curr
        (Node (Val v' g') childs') = other
    in (
        Node (Val (f v v') g) (childs ++ [other]), -- *
        Node (Val v' g') (childs' ++ [curr]) -- 
    )

add = apply (+)
mult = apply (*)

-- TODO
-- .. handle more functions and optimize?