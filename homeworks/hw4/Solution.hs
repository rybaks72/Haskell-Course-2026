module Solution where
newtype Reader r a = Reader { runReader :: r -> a }

-- 1. **Functor, Applicative, and Monad instances**

--    Implement the three standard instances for `Reader r`:
--    ```haskell
instance Functor (Reader r) where
     -- fmap :: (a -> b) -> Reader r a -> Reader r b
     fmap f reader = Reader (\env -> f (runReader reader env))

instance Applicative (Reader r) where
     -- pure   :: a -> Reader r a
    pure x = Reader (\env -> x)
     -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
    liftA2 f reader1 reader2= Reader (\env -> f (runReader reader1 env) (runReader reader2 env))

instance Monad (Reader r) where
     -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
     (>>=) reader f = Reader (\env -> let x = runReader reader env in runReader (f x) env )
--    ```
--    The intended semantics are the usual ones: `fmap f r` runs `r` in the environment
--    and then applies `f` to the result; `pure x` ignores the environment and returns `x`;
--    `liftA2 f ra rb` runs both `ra` and `rb` in the same environment and combines their
--    results with the binary function `f`; `(>>=)` sequences two `Reader` computations,
--    passing the same environment to both and letting the second depend on the value
--    produced by the first.


-- 2. **Primitive operations**

--    Implement the basic `Reader` primitives — these are the only "public" interface you
--    should need to write the rest of the code; once they are in place, prefer them (and
--    `do`-notation) over pattern-matching on the `Reader` constructor directly.
--    ```haskell
--    -- Retrieves the entire environment.
ask   :: Reader r r
ask = Reader (\env -> env)

--    -- Retrieves a value derived from the environment by applying a projection,
--    -- e.g. `asks interestRate :: Reader BankConfig Double`.
asks  :: (r -> a) -> Reader r a
asks f = Reader (\env -> f env) 

--    -- Runs a subcomputation in a locally modified environment. The modification
--    -- is only visible inside the passed Reader — once it returns, the outer
--    -- environment is restored (conceptually; there is no mutable state, the
--    -- modified environment simply goes out of scope).
local :: (r -> r) -> Reader r a -> Reader r a
local f reader = Reader (\env -> runReader reader (f env))
--    ```


-- 3. **A practical example — banking system**

--    Consider a small banking system where the bank's configuration (interest rate,
--    fees, limits) is the read-only environment shared by every operation:
--    ```haskell
data BankConfig = BankConfig
    { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
    , transactionFee :: Int     -- flat fee charged per transaction
    , minimumBalance :: Int     -- minimum required balance on an account
    } deriving (Show)

data Account = Account
    { accountId :: String       -- account identifier
    , balance   :: Int          -- current balance
    } deriving (Show)
--    ```
--    Implement the following four functions using the `Reader` monad. Prefer `ask` / `asks`
--    and `do`-notation over pattern-matching on the `Reader` constructor — this is what
--    makes the monadic style pay off:
--    ```haskell
--    -- Computes the interest accrued on the account, based on the configured rate.
--    -- The result should be an Int — round or truncate as you see fit, but be consistent.
calculateInterest   :: Account -> Reader BankConfig Int
calculateInterest acc = do
    rate <- asks interestRate
    return (floor (fromIntegral (balance acc) * rate))

--    -- Deducts the transaction fee from the account and returns the updated account.
--    -- The accountId should remain unchanged.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
    fee <- asks transactionFee
    return Account { accountId = accountId acc, balance = balance acc - fee}

--    -- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
    minBal <- asks minimumBalance
    return (balance acc >= minBal)

--    -- Runs the three operations above on a single account and combines their results.
--    -- The returned tuple contains:
--    --   * the account after the transaction fee has been applied,
--    --   * the interest computed from the ORIGINAL account,
--    --   * whether the ORIGINAL account meets the minimum balance requirement.
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
    accAfterFee <- applyTransactionFee acc
    interest <- calculateInterest acc
    hasMinimum <- checkMinimumBalance acc
    return (accAfterFee, interest, hasMinimum)
--    ```
--    Once everything is implemented, the following should work in GHCi:
--    ```haskell
--    ghci> let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
--    ghci> let acc = Account { accountId = "A-001", balance = 1000 }
--    ghci> runReader (processAccount acc) cfg
--    (Account {accountId = "A-001", balance = 998}, 50, True)
--    ```