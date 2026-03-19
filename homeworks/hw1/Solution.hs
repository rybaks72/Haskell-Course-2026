{-# LANGUAGE BangPatterns #-}
module Main where
import System.Win32 (xBUTTON1)
-- 3. **Sieve of Eratosthenes**
--    The *Sieve of Eratosthenes* is an ancient algorithm for finding all primes up to a given limit. It works as follows: starting from the list `[2..n]`, take the first element `p` — it must be prime — then remove all multiples of `p` from the rest of the list and repeat.

--    Implement this as a recursive function `sieve :: [Int] -> [Int]`, where each recursive step uses a list comprehension to filter out multiples of the head. Then define:
--    ```haskell
--    primesTo :: Int -> [Int]
--    primesTo n = sieve [2..n]
--    ```
--    Finally, use `primesTo` to define `isPrime :: Int -> Bool` that checks whether a given positive integer is prime.

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = n `elem` primesTo n

-- 1. **Goldbach Pairs**
--    Write a function `goldbachPairs :: Int -> [(Int, Int)]` that, given an even integer `n ≥ 4`, returns all pairs `(p, q)` satisfying:
--    - `p` and `q` are both prime numbers
--    - `p + q == n`
--    - `p ≤ q`

--    Use a list comprehension to generate the result. Define a helper `isPrime :: Int -> Bool` using Exercise 3.

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
    | n < 4 = []
    | otherwise =
      [ (p, q)
      | p <- [2 .. n `div` 2]
      , let q = n - p
      , isPrime p
      , isPrime q
      ]

-- 2. **Coprime Pairs**
--    Write a function `coprimePairs :: [Int] -> [(Int, Int)]` that takes a list of positive integers and returns all unique pairs 
--    `(x, y)` (with `x < y`) for which `gcd x y == 1`. You may use Haskell's built-in `gcd`.

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x:xs) = [(x, y) | y <- xs, x < y, gcd x y == 1] ++ coprimePairs xs

-- 4. **Matrix Multiplication**
--    Represent a matrix as `[[Int]]` (a list of rows). Write
--    ```haskell
--    matMul :: [[Int]] -> [[Int]] -> [[Int]]
--    ```
--    using nested list comprehensions. If the first matrix has dimensions `m × p` and the second `p × n`, 
--    then the entry at row `i`, column `j` of the product is:
--    ```
--    sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ]
--    ```
--    The outer comprehension should range over row indices `i` and column indices `j`.


-- HELPER beacause built-in `head` function was partailly defined

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] _ = []
matMul _ [] = []
matMul a@(rowA:_) b@(rowB:_) -- DO WYTŁUMACZENIA
  | p /= length b = []
  | otherwise =
      [ [ sum [ (a !! i !! k) * (b !! k !! j) | k <- [0 .. p-1] ]
        | j <- [0 .. n-1]
        ]
      | i <- [0 .. m-1]
      ]
  where
    m = length a
    p = length rowA
    n = length rowB  

-- 5. **Permutations**
--    Write a function
--    ```haskell
--    permutations :: Int -> [a] -> [[a]]
--    ```
--    that generates all k-element permutations (ordered selections without repetition) from a given list.
--    For example, for `k = 2` and list `[1,2,3]` the result should be `[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]`.

permutations :: Int -> [a] -> [[a]]
permutations _ [] = []
permutations k xs
            | k == 0 = [[]]
            | k < 0 = []
            | otherwise =
      [ chosen : restPerm
      | (chosen, restList) <- pickOne xs
      , restPerm <- permutations (k - 1) restList
      ]

-- pickOne returns all ways to pick one element from a list,
-- together with the list that remains after removing it once.
pickOne :: [a] -> [(a, [a])]
pickOne [] = []
pickOne (x:xs) =
  (x, xs) : [ (y, x : ys) | (y, ys) <- pickOne xs ]

-- 6. **Hamming Numbers**
--    A *Hamming number* is a positive integer whose only prime factors are 2, 3, and 5 — numbers of the form 2^a × 3^b × 5^c with a, b, c ≥ 0. 
--    The sequence begins: 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, …

--    (a) Write a helper
--    ```haskell
--    merge :: Ord a => [a] -> [a] -> [a]
--    ```
--    that merges two sorted (potentially infinite) lists into one sorted list, eliminating duplicates.

--    (b) Using `merge`, define the infinite list
--    ```haskell
--    hamming :: [Integer]
--    ```
--    as a single definition. 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x < y = x : merge xs (y:ys)
    | y < x = y : merge (x:xs) ys
    | otherwise = x : merge xs ys


hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming)
                (merge (map (*3) hamming)
                (map (*5) hamming))


-- 7. **Integer Power with Bang Patterns**
--    Write a recursive function `power :: Int -> Int -> Int` that computes `power b e = b ^ e` using an accumulator. 
--    Use bang patterns on the accumulator to ensure strict evaluation.

power :: Int -> Int -> Int
power b e
  | e < 0 = -1
  | otherwise = go e 1
  where
    go 0 !acc = acc
    go n !acc = go (n - 1) (acc * b)

-- 8. **Running Maximum: `seq` vs. Bang Patterns**
--    Implement two versions of a function `listMax :: [Int] -> Int` that returns the maximum element of a non-empty list using a helper with an accumulator:
--    - The first version uses `seq` to force evaluation of the accumulator in the helper function.
--    - The second version uses bang patterns on the accumulator argument of the helper function.

listMaxSeq :: [Int] -> Int
listMaxSeq []     = error "listMaxSeq: empty list"
listMaxSeq (x:xs) = go x xs
  where
    go :: Int -> [Int] -> Int
    go acc []     = acc
    go acc (y:ys) =
      let acc' = max acc y
      in acc' `seq` go acc' ys


listMaxBang :: [Int] -> Int
listMaxBang []     = error "listMaxBang: empty list"
listMaxBang (x:xs) = go x xs
  where
    go :: Int -> [Int] -> Int
    go !acc []     = acc
    go !acc (y:ys) = go (max acc y) ys


-- 9. **Infinite Prime Stream**
--    The `primesTo` function from Exercise 3 only generates primes up to a fixed bound. 
--    Using lazy evaluation we can instead define an *infinite* stream of all primes.

--    (a) Define
--    ```haskell
--    primes :: [Int]
--    ```
--    as an infinite list of all prime numbers, by applying the same sieve idea from Exercise 3 to the infinite list `[2..]`. 
--    Your `sieve` function should be unchanged — only the input changes.

--    (b) Use `primes` to give a new definition of `isPrime :: Int -> Bool` that does not require an explicit upper bound.

primes :: [Int]
primes = sieve [2..]

isPrimeInf :: Int -> Bool
isPrimeInf n
  | n < 2     = False
  | otherwise = n `elem` takeWhile (<= n) primes

-- 10. **Strict Accumulation and Space Leaks**
--     Computing the mean of a list requires knowing both the sum and the length. Write a function
--     ```haskell
--     mean :: [Double] -> Double
--     ```
--     using a tail-recursive helper. Do *not* use any library functions for the recursion.

--     (a) Write a first version with no strictness annotations. 

--     (b) Fix the space leak using bang patterns. Is a bang pattern on the pair itself sufficient, or do the components also need to be forced individually?

--     (c) Generalise your strict solution to compute both the mean and the *variance* σ² = (Σxᵢ²)/n − μ² in a single pass. 
--     Apply bang patterns appropriately to all three components.

meanA :: [Double] -> Double
meanA [] = error "meanA: empty list"
meanA xs = total / fromIntegral count
  where
    (total, count) = go xs (0.0, 0 :: Int)

    go :: [Double] -> (Double, Int) -> (Double, Int)
    go [] acc = acc
    go (x:rest) (s, n) = go rest (s + x, n + 1)

meanB :: [Double] -> Double
meanB [] = error "meanB: empty list"
meanB xs = total / fromIntegral count
  where
    (total, count) = go xs 0.0 (0 :: Int)

    go :: [Double] -> Double -> Int -> (Double, Int)
    go [] !s !n = (s, n)
    go (x:rest) !s !n = go rest (s + x) (n + 1)

meanVar :: [Double] -> (Double, Double)
meanVar [] = error "meanVar: empty list"
meanVar xs = (mu, var)
  where
    (s, s2, n) = go xs 0.0 0.0 (0 :: Int)

    mu  = s / fromIntegral n
    var = (s2 / fromIntegral n) - (mu * mu)

    go :: [Double] -> Double -> Double -> Int -> (Double, Double, Int)
    go [] !s !s2 !n = (s, s2, n)
    go (x:rest) !s !s2 !n = go rest (s + x) (s2 + x * x) (n + 1)


main :: IO ()
main = do
--   print (primesTo 30)
--   print (isPrime 29)
--   print (isPrime 28)

--   print (goldbachPairs 4)
--   print (goldbachPairs 10)

--   print (coprimePairs [2, 3, 4, 6, 7])
  print (matMul [] [])
  print (permutations 2 [1,2,3])