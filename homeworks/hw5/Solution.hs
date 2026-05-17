module Solution where
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)
-- 1. **Stack machine**

--    Define a stack-based instruction set:
--    ```haskell
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG deriving (Show, Eq)
--    ```
--    and implement
--    ```haskell
execInstr :: Instr -> State [Int] ()
execInstr instr = do
    stack <- get
    case instr of
        PUSH x -> put (x:stack)
        POP -> 
            case stack of
                [] -> put stack
                (_:xs) -> put xs
        DUP -> 
            case stack of
                [] -> put stack
                (x:xs) -> put (x:x:xs)
        SWAP -> 
            case stack of
                (x:y:xs) -> put (y:x:xs)
                _ -> put stack
        ADD ->
            case stack of
                (x:y:xs) -> put ((x+y):xs)
                _ -> put stack
        MUL ->
            case stack of
                (x:y:xs) -> put ((x * y) : xs)
                _        -> put stack
        NEG ->
            case stack of
                (x:xs) -> put((-x):xs)
                _ -> put stack
--    ```
--    that executes a single instruction on a stack (a list of `Int`). Then implement
--    ```haskell
execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (instr:instrs) = do
    execInstr instr
    execProg instrs
--    ```
--    that executes a sequence of instructions, and a wrapper
--    ```haskell
runProg :: [Instr] -> [Int]
runProg program = execState (execProg program) []
--    ```
--    that runs the program starting with an empty stack and returns the final stack.
--    If an instruction requires more operands than are on the stack, it should be silently skipped.

-- 2. **Expression evaluator with variable bindings**

--    Consider the following expression language with mutable variables:
--    ```haskell
data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr   -- bind the value of the expression to the name, return that value
    | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right
--    ```
--    Using `Data.Map` as the variable environment, implement
--    ```haskell
eval :: Expr -> State (Map String Int) Int
eval expr = 
    case expr of
        Num n -> return n
        Var name -> do
            env <- get
            return (env Map.! name)
        Add e1 e2 -> do
            v1 <- eval e1
            v2 <- eval e2
            return (v1 + v2)
        Mul e1 e2 -> do
            v1 <- eval e1
            v2 <- eval e2
            return (v1 * v2)
        Neg e -> do
            v <- eval e
            return (-v)
        Assign name e -> do
            v <- eval e
            modify (Map.insert name v)
            return v
        Seq e1 e2 -> do
            eval e1
            eval e2
--    ```
--    so that `Assign` updates the environment, `Var` looks a name up (you may assume all referenced
--    variables have been assigned earlier), and the remaining constructors behave as expected.
--    Use `get`, `put`, and/or `modify`. Then provide the wrapper
--    ```haskell
runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty
--    ```
--    which runs `eval` starting from the empty environment using `evalState`.

-- 3. **Memoised edit (Levenshtein) distance**

--    The edit distance between two strings is the minimum number of single-character insertions,
--    deletions, and substitutions required to transform one into the other. Implement it using
--    the `State` monad so that the overlapping subproblems of the naive recursion are reused.

--    Use a cache of type `Map (Int, Int) Int` whose entry at key `(i, j)` stores the edit distance
--    between the prefixes of length `i` and `j` of the two input strings. Implement
--    ```haskell
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i,j) cache of
        Just value -> return value
        Nothing -> do
            value <- computeEditDist xs ys i j
            modify (Map.insert (i,j) value)
            return value

computeEditDist :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
computeEditDist xs ys i j
    | i == 0 = return j
    | j == 0 = return i
    | xs !! (i - 1) == ys !! (j - 1) = do
        editDistM xs ys (i - 1) (j - 1)
    | otherwise = do
        deletion <- editDistM xs ys (i - 1) j
        insertion <- editDistM xs ys i (j -1)
        substitution <- editDistM xs ys (i - 1) (j - 1)

        return (1 + minimum [deletion, insertion, substitution]) 
--    ```
--    where `editDistM xs ys i j` returns the edit distance between `take i xs` and `take j ys`.
--    Each call should first consult the cache; if the entry is present, return it, otherwise
--    compute it using the standard recurrence
--    ```
--    d(0, j) = j
--    d(i, 0) = i
--    d(i, j) = d(i-1, j-1)                      if  xs!!(i-1) == ys!!(j-1)
--    d(i, j) = 1 + min { d(i-1, j)              -- deletion
--                      , d(i,   j-1)            -- insertion
--                      , d(i-1, j-1) }          -- substitution
--    ```
--    store the result in the cache, and return it. Finally provide the pure wrapper
--    ```haskell
editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty
--    ```

-- 4. **Player movement and decisions**

--    Implement
--    ```haskell
data Location
    = Normal
    | Decision [String]
    | Obstacle Int
    | Treasure Int
    | Trap Int
    | Goal
    deriving (Show, Eq)

data GameState = GameState
    { position :: Int
    , energy   :: Int
    , score    :: Int
    , pathName :: String
    }
    deriving (Show, Eq)

type AdventureGame a = StateT GameState IO a

locationAt :: String -> Int -> Location
locationAt path pos =
    case path of
        "forest" ->
            if pos >= 8 then Goal
            else case pos of
                2 -> Treasure 5
                4 -> Obstacle 2
                6 -> Trap 3
                _ -> Normal
        "cave" ->
            if pos >= 7 then Goal
            else case pos of
                1 -> Trap 2
                3 -> Treasure 8
                5 -> Obstacle 3
                _ -> Normal
        _ ->
            if pos >= 3 then Decision ["forest", "cave"]
            else Normal

movePlayer   :: Int -> AdventureGame Int
movePlayer diceRoll = do
    state <- get
    let currentEnergy = energy state
    let steps = min diceRoll currentEnergy
    let newPosition = position state + steps
    let newEnergy = currentEnergy - steps

    put state {position = newPosition, energy = newEnergy}
    lift $ putStrLn ("You moved " ++ show steps ++ " spaces.")
    return steps

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- lift $ getPlayerChoice options
    state <- get
    put state {pathName = choice, position = 0}
    lift $ putStrLn ("You chose path: " ++ choice)
    return choice
--    ```
--    where `movePlayer` moves the player based on the given dice roll result and returns the number
--    of spaces moved, and `makeDecision` handles a decision point by presenting the player with options
--    and returning their choice.

-- 5. **Game loop**

--    Implement
--    ```haskell
handleLocation :: AdventureGame Bool
handleLocation = do
    state <- get
    let currentPath = pathName state
    let currentPos = position state
    let location = locationAt currentPath currentPos
    
    case location of
        Normal -> do
            lift $ putStrLn "Nothing happens here"
            return False
        Decision options -> do
            lift $ putStrLn "You reached a decision point"
            makeDecision options
            return False
        Obstacle energyLoss -> do
            let newEnergy = max 0 (energy state - energyLoss)
            put state {energy = newEnergy}
            lift $ putStrLn ("Obstacle! You lose " ++ show energyLoss ++ " energy")
            return False
        Treasure points -> do
            let newScore = score state + points
            put state {score = newScore}
            lift $ putStrLn ("Treasure found! You gain " ++ show points ++ " points")
            return False
        Trap pointsLost -> do
            let newScore = max 0 (score state - pointsLost)
            put state {score = newScore}
            lift $ putStrLn ("Trap! You lose " ++ show pointsLost ++ " points")
            return False
        Goal -> do
            lift $ putStrLn "You reached the main treasure!"
            return True

playTurn :: AdventureGame Bool
playTurn = do
    stateBefore <- get
    if energy stateBefore <= 0 
        then do
            lift $ putStrLn "You have no energy left"
            return True
        else do
            diceRoll <- lift getDiceRoll
            movePlayer diceRoll
            reachedGoal <- handleLocation
            stateAfter <- get
            lift $ displayGameState stateAfter

            if reachedGoal
                then return True
                else do
                    finalState <- get
                    if energy finalState <= 0
                        then do
                            lift $ putStrLn "You ran out of energy"
                            return True
                        else return False
playGame :: AdventureGame ()
playGame = do
    ended <- playTurn
    if ended 
        then do
            lift $ putStrLn "Game over"
        else playGame
--    ```
--    where `handleLocation` handles the player's current location (obstacle, treasure, trap) and returns
--    `True` if the player has reached the goal; `playTurn` handles one turn of the game and returns
--    `True` if the game has ended; `playGame` runs the game until it ends, displaying the game state
--    after each move.

-- 6. **User interaction in `IO`**

--    Implement the supporting `IO` functions
--    ```haskell
getDiceRoll      :: IO Int
getDiceRoll = do
    putStrLn "Enter dice roll result, from 1 to 6:"
    input <- getLine

    case readMaybe input of
        Just n -> 
            if n >= 1 && n <= 6
                then return n
                else do
                    putStrLn "Invalid dice roll. Please enter a number from 1 to 6"
                    getDiceRoll
        Nothing -> do
            putStrLn "invalid input. Please enter a number"
            getDiceRoll
displayGameState :: GameState -> IO ()
displayGameState state = do
    putStrLn ""
    putStrLn "=============================="
    putStrLn "Current game state"
    putStrLn "=============================="
    putStrLn ("Path:     " ++ pathName state)
    putStrLn ("Position: " ++ show (position state))
    putStrLn ("Energy:   " ++ show (energy state))
    putStrLn ("Score:    " ++ show (score state))
    putStrLn "=============================="
    putStrLn ""
getPlayerChoice  :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose one of the available paths:"
    printOptions options
    choice <- getLine

    if choice `elem` options
        then return choice
        else do
            putStrLn "Invalid choice. Try again"
            getPlayerChoice options

printOptions :: [String] -> IO ()
printOptions [] = return ()
printOptions (x:xs) = do
    putStrLn ("- " ++ x)
    printOptions xs
--    ```
--    which ask the user to provide a dice roll result, display the current game state, and ask the
--    user to choose one of the given options, respectively.




-- so i can actually run the game:

initialGameState :: GameState
initialGameState =
    GameState
        { position = 0
        , energy = 20
        , score = 0
        , pathName = "start"
        }

runGame :: IO ()
runGame = do
    putStrLn "Treasure Hunters"
    putStrLn "Reach the main treasure before your energy runs out."
    displayGameState initialGameState

    finalState <- execStateT playGame initialGameState

    putStrLn "Final state:"
    displayGameState finalState