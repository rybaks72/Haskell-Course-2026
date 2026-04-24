module Main where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (permutations)
import Control.Monad (guard)
import Distribution.Compat.CharParsing (CharParsing(text))
import Control.Monad.Writer

-- 1. **Maze navigation**

--    A maze is represented as a map from positions to their neighbours in each direction:
--    ```haskell
--    type Pos = (Int, Int)
--    data Dir = N | S | E | W deriving (Eq, Ord, Show)
--    type Maze = Map Pos (Map Dir Pos)
--    ```
--    A position may not have a neighbour in every direction (walls).

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

--    (a) Write a function
--    ```haskell
--    move :: Maze -> Pos -> Dir -> Maybe Pos
--    ```
--    that attempts a single move in the given direction, returning `Nothing` if blocked by a wall.

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = 
    case Map.lookup pos maze of
        Nothing -> Nothing
        Just neighbours ->
            Map.lookup dir neighbours

--    (b) Write a function
--    ```haskell
--    followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
--    ```
--    that follows a sequence of directions from a starting position, short-circuiting to `Nothing`
--    as soon as any step is blocked. Use the `Maybe` monad — do not pattern-match on `Nothing` manually.

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = Just pos
followPath maze pos (dir : dirs) = do
    nextPos <- move maze pos dir -- <- - if move gives Nothing the whole function returns Nothing
    followPath maze nextPos dirs

--    (c) Write a function
--    ```haskell
--    safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
--    ```
--    that returns the full trace of positions visited (including the start), or `Nothing` if the
--    path is blocked at any point.

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos [] = Just [pos]
safePath maze pos (dir : dirs) = do
    next <- move maze pos dir
    rest <- safePath maze next dirs
    return (pos : rest)


-- 2. **Decoding a message**

--    A substitution cipher maps each character to another. Given a partial decryption key
--    (not every character may be known yet):
--    ```haskell
--    type Key = Map Char Char
--    ```
--    write a function
--    ```haskell
--    decrypt :: Key -> String -> Maybe String
--    ```
--    that decodes the entire string, returning `Nothing` if any character in the input is missing
--    from the key. Use `traverse` with the `Maybe` monad.

--    Then write
--    ```haskell
--    decryptWords :: Key -> [String] -> Maybe [String]
--    ```
--    that decrypts a list of words, failing if any single word cannot be fully decoded.

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key text =
    traverse (\c -> Map.lookup c key) text


-- 3. **Seating arrangements**

--    You are organising a dinner and must assign guests to seats around a table.
--    Some pairs of guests have conflicts and must not sit next to each other.
--    ```haskell
--    type Guest = String
--    type Conflict = (Guest, Guest)
--    ```
--    Write a function
--    ```haskell
--    seatings :: [Guest] -> [Conflict] -> [[Guest]]
--    ```
--    that returns all valid permutations of the guest list such that no two conflicting guests
--    are adjacent (the table is round, so the last and first guests are also neighbours).
--    Use the list monad to generate permutations and `guard` to filter out invalid ones.

type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
--seatings [] conflicts = [[]]
seatings guests conflicts = do
    seating <- permutations guests
    guard (valid seating)
    return seating
    where
        valid [] = True
        valid xs = all ok (zip xs (tail xs ++ [head xs]))
        ok (a,b) = 
            not ((a,b) `elem` conflicts || (b,a) `elem` conflicts)

-- 4. **Result monad with warnings**

--    Define a type
--    ```haskell
--    data Result a = Failure String | Success a [String]
--    ```

data Result a = Failure String | Success a [String]

--    where `Failure msg` represents a computation that failed with an error message, and
--    `Success val warnings` represents a successful computation carrying a value together with
--    a list of accumulated warning messages.

--    (a) Implement `Functor`, `Applicative`, and `Monad` instances for `Result`.
--    Warnings should be accumulated (concatenated) when sequencing computations.

instance Functor Result where
--fmap :: (a -> b) -> Result a -> Result b
    fmap _ (Failure msg) = Failure msg
    fmap f (Success value warnings) = 
        Success (f value) warnings

instance Applicative Result where
--pure :: a -> Result a
    pure x = Success x []
    (<*>) :: Result (a -> b) -> Result a -> Result b
    Failure msg <*> _ = Failure msg
    _ <*> Failure msg = Failure msg
    Success f warnings1 <*> Success x warnings2 = Success (f x) (warnings1 ++ warnings2) 

instance Monad Result where
-- (>>=) :: Result a -> (a -> Result b) -> Result b
    Failure msg >>= _ = Failure msg

    Success x warnings1 >>= f =
        case f x of
            Failure msg -> Failure msg
            Success y warnings2 -> Success y (warnings1 ++ warnings2)

--    (b) Write helper functions:
--    ```haskell
--    warn    :: String -> Result ()
--    failure :: String -> Result a
--    ```

warn :: String -> Result ()
warn text = Success () [text]

failure :: String -> Result a
failure text = Failure text

--    (c) Use the `Result` monad to implement a function
--    ```haskell
--    validateAge :: Int -> Result Int
--    ```
--    that fails if the age is negative, warns if the age is above 150, and otherwise succeeds with the age.
--    Then implement
--    ```haskell
--    validateAges :: [Int] -> Result [Int]
--    ```
--    that validates a list of ages, accumulating all warnings. Use `mapM`.

validateAge :: Int -> Result Int
validateAge age
    | age < 0 = 
        failure "Age is negative"
    | age > 150 = do
        warn "Age is above 150"
        return age
    | otherwise = Success age []

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- 5. **Evaluator with simplification log**

--    Given the expression type:
--    ```haskell
--    data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
--    ```
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

--    write a function
--    ```haskell
--    simplify :: Expr -> Writer [String] Expr
--    ```
--    that applies algebraic simplification rules and logs each rule applied. The rules should include
--    at least:
--    - `Add (Lit 0) e` or `Add e (Lit 0)` simplifies to `e` (additive identity)
--    - `Mul (Lit 1) e` or `Mul e (Lit 1)` simplifies to `e` (multiplicative identity)
--    - `Mul (Lit 0) _` or `Mul _ (Lit 0)` simplifies to `Lit 0` (zero absorption)
--    - `Neg (Neg e)` simplifies to `e` (double negation)
--    - `Add (Lit a) (Lit b)` simplifies to `Lit (a+b)` (constant folding)
--    - `Mul (Lit a) (Lit b)` simplifies to `Lit (a*b)` (constant folding)

--    Each time a rule fires, log a message like `"Add identity: 0 + e -> e"`.
--    Apply simplification recursively (bottom-up: simplify subtrees first, then the root).

simplify :: Expr -> Writer [String] Expr
-- tell :: [String] -> Writer [String] ()
simplify (Lit n) = return (Lit n) -- return = return this value with no log messages
simplify (Add e1 e2) = do
    se1 <- simplify e1
    se2 <- simplify e2

    case (se1, se2) of
        (Lit 0, e) -> do
            tell ["Add identity: 0 + e -> e"]
            return e
        (e, Lit 0) -> do
            tell ["Add identity: e + 0 -> e"]
            return e
        (Lit a, Lit b) -> return (Lit (a + b))
        _ -> return (Add se1 se2)
simplify (Mul e1 e2) = do
    se1 <- simplify e1
    se2 <- simplify e2

    case (se1, se2) of
        (Lit 0, _) -> do
            tell ["Zero absorption: 0 * e -> 0"]
            return (Lit 0)
        (_, Lit 0) -> do
            tell ["Zero absorption: 0 * e -> 0"]
            return (Lit 0)
        (Lit 1, e) -> do
            tell ["Multiply identity: 1 * e -> e"]
            return e
        (e, Lit 1) -> do
            tell ["Multiply identity: e * 1 -> e"]
            return e
        (Lit a, Lit b) -> return (Lit (a * b))
        _ -> return (Mul se1 se2)
simplify (Neg e) = do
    se <- simplify e

    case se of
        Neg ex -> do
            tell ["Double negation: -(-e) -> e"]
            return ex
        _ -> return (Neg se)


main :: IO ()
main = do
    print();