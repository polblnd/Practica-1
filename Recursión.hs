
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mult :: Int -> Int -> Int
mult _ 0 = 0
mult n m = n + mult n (m - 1)

potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia n m = mult n (potencia n (m-1))

factorial :: Int -> Int
factorial 0 = 1
factorial n = mult n (factorial (n-1))

divAlg :: Int -> Int -> (Int, Int)
divAlg a b
  | a < b     = (0, a)
  | otherwise = (x + 1, r)
  where
    (x, r) = divAlg (a - b) b

maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = max x (maximo xs)
