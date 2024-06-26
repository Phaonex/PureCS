Pythagoran triads and Taxicab numbers
____________________________________________________

>
> triads :: Int -> [(Int, Int, Int)]
> triads n = [(x, y, z) | x <- [1..m], y <- [x + 1..n], coprime x y, z <- [y+ 1..n], x*x + y*y == z*z]
>   where m = floor (fromIntegral n / sqrt 2)
> divisors :: Integral a => a -> [a]
> divisors x = [d | d <- [2..x-1], x `mod` d == 0]
> coprime :: Integral a => a -> a -> Bool
> coprime x y = disjoint (divisors x) (divisors y)

> disjoint :: Ord a => [a] -> [a] -> Bool
> disjoint xs [] = True
> disjoint [] ys = True
> disjoint (x:xs) (y:ys)
>   | x < y = disjoint xs (y: ys)
>   | x == y = False
>   | x > y = disjoint (x:xs) ys

> quads :: Int -> [(Int, Int, Int, Int)]
> quads n = [(a, b, c, d) | a <- [1..n], b <- [a..n],
>                             c <- [a + 1..n], d <- [c..n],
>                             a^3 + b^3 == c^3 + d^3 ]
