Simple basics functional funcions utilis implementation

> import Prelude hiding (elem)
> import Data.ByteString (cons)
> inRange :: Ord a => a -> a -> a -> Bool
> inRange min max x = x >= min && x <= max

> secondRange :: Ord a => a -> a -> a -> Bool
> secondRange min max x = 
>    let in_lower_bound = min <= x 
>        in_upper_bound = max >= x 
>    in in_lower_bound && in_upper_bound

> thirdRange :: Ord a => a -> a -> a -> Bool
> thirdRange min max x = ilb && iub
>                 where ilb = min <= x
>                       iub = max >= x

> fact :: (Ord p, Num p) => p -> p
> fact n = if n <= 1 then 1 else n * fact (n-1)

> guardFact :: (Ord p, Num p) => p -> p
> guardFact n | n <= 1      = 1 | otherwise   = n * fact (n - 1)

> accFact :: (Ord t, Num t) => t -> t
> accFact n = aux n 1
>   where
>       aux n acc
>         | n <= 1      = acc
>         | otherwise   = aux (n - 1) (n * acc)

> asc :: Int -> Int -> [Int]
> asc n m
>   | m < n     = []
>   | m == n    = [m]
>   | m > n     = n : asc (n + 1) m

> comprehention :: [Integer]
> comprehention = [2 * x | x <- [1, 2, 3]]

> guardComprehention :: [Integer]
> guardComprehention = [2 * x | x <- [1, 2, 3], x > 1]

> doubleListComprehention :: [(Integer, Integer)]
> doubleListComprehention = [(x, y) | x <- [1, 2, 3], x > 1, y <- [1, 2, 3]]


> add :: [Int] -> Int
> add [] = 0
> add (x:xs) = x + add xs

> evens :: [Int] -> [Int]
> evens [] = []
> evens (x:xs)
>     | mod x 2 == 0 = x : evens xs
>     | otherwise    = evens xs

> addTuples :: [(Int, Int)] -> [Int]
> addTuples xs = [x + y | (x, y) <- xs]
 
> elem :: (Eq a) => a -> [a] -> Bool
> elem _ [] = False
> elem e (x:xs) = (e == x) || (elem e xs)

> nub :: (Eq a ) => [a] -> [a]
> nub [] = []
> nub (x:xs)
>   | x `elem` xs = nub xs
>   | otherwise = x : nub xs

> isAsc :: [Int] -> Bool
> isAsc [] = True
> isAsc [x] = True
> isAsc (x:y:xs) = (x <= y) && isAsc (y:xs)

> hasPath :: [(Int, Int)] -> Int -> Int -> Bool
> hasPath [] x y = x == y
> hasPath xs x y
>   | x == y = True 
>   | otherwise =
>   let  xs' = [(n, m) | (n, m) <- xs, n /= x] in
>   or [hasPath xs' m y | (n, m ) <- xs, n == x]