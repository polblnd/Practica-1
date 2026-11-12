data ArbolB a = Vacio | Nodo a (ArbolB a) (ArbolB a) deriving (Eq, Show)

nodes :: Int -> Int
nodes 0 = 0
nodes n = 1 + nodes (n - 1) +  nodes (n - 1)

depth :: Int -> Int
depth n
    | n <= 0 = 0
    | otherwise = 1 + max (depth (n - 1)) (depth (n - 2))
-- Usamos la def de max, la cual inclui asi porque al parecer haskell la tiene integrada, pero si no fuera así seria
-- max {n,m} = if n > m then n else m