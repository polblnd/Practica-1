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

nodosChido :: ArbolB a -> Int
nodosChido Vacio = 0
nodosChido (Nodo _ izq der) = 1 + nodosChido izq + nodosChido der
-- Codigo sin recursion, a parte usando el dato que definimos ArbolB a

depthChido :: ArbolB a -> Int
depthChido Vacio = 0
depthChido (Nodo _ izq der) = 1 + max (depthChido izq) (depthChido der)
-- Codigo sin recursion, a parte usando el dato que definimos ArbolB a