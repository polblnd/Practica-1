data Natural = Cero | S Natural deriving Show

nat :: Int -> Natural
nat 0 = Cero
nat n = S (nat (n - 1))

ent :: Natural -> Int
ent Cero = 0
ent (S n) = 1 + ent n

suma :: Natural -> Natural -> Natural
suma Cero m = m
suma (S n) m = S (suma n m)

mult :: Natural -> Natural -> Natural
mult Cero m = Cero
mult (S n)  m = suma m (mult n m)
