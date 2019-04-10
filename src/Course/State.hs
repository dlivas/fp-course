{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State f) =
  snd . f
  -- error "todo: Course.State#exec"

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State f) =
  fst . f
  -- error "todo: Course.State#eval"

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get =
  State ((,) <*> id)

get2 ::
  State s s
get2 =
  State (\s -> (s, s))
  -- error "todo: Course.State#get"

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s =
  State (\_ -> ((),s))

put1 ::
  s
  -> State s ()
put1 s =
  State $ lift1 ((),) (const s)
  -- error "todo: Course.State#put"

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  f <$> State t =
    State $ (\(a, s) -> (f a, s)) . t
    -- error "todo: Course.State#(<$>)"

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a =
    State (a,)
    -- State (\s -> (a, s))
    -- error "todo: Course.State pure#instance (State s)"
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  State t <*> State u =
    State
      (\s ->
        let
          (f, s1) = t s
          (a, s2) = u s1
        in
          (f a, s2)
      )
    -- previous solution:
    -- (\s ->  ( (eval t <*> eval u) s
    --         , (exec u <$> exec t) s
    --         ))
    -- previous solution:
    -- State (\s -> (firstR s, secondR s))
    -- where
    --   firstR s = (eval t s) (eval u s)
    --   secondR s = exec u (exec t s)
    --
    -- error "todo: Course.State (<*>)#instance (State s)"

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  f =<< State t =
    State $ (\(a, k) -> runState (f a) k) . t
    -- previous solution:
    -- State $ flip runState <*> (f <$> eval t)
    -- previous solution:
    -- State $ flip runState <*> (f <$> eval t)
    -- previous solution:
    -- State (\s ->  runState ((f . eval t) s) s)
    -- previous solution:
    -- State (\s ->  ( eval (f $ eval t s) s
    --               , exec (f $ eval t s) s
    --               ))
    -- error "todo: Course.State (=<<)#instance (State s)"

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = return Empty
findM p (a :. t) =
  p a >>=
    \b -> if b
            then return (Full a)
            else findM p t

findM2 ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM2 p l =
  pure . headOr Empty =<< pure . map Full =<< filtering p l

-- error "todo: Course.State#findM"

-- | Utility function used for the implementation of firstRepet and distinct .
--
repeatedAndUniques ::
  Ord a =>
  List a
  -> List (Bool, a)
repeatedAndUniques Nil = Nil
repeatedAndUniques l@(h :. t) =
  map (\(ba, _, _) -> ba) $
    take (length l) $
      produce
        (exec uniquesState)
        ((False, h), t, S.insert h S.empty)
  where
    uniquesState = State uniquesStateFunc
    uniquesStateFunc (_, (a :. l), uniques) =
      ((), ((S.member a uniques, a), l, S.insert a uniques))

-- | Remove all not duplicate elements in a `List`.
--
repeated ::
  Ord a =>
  List a
  -> List a
repeated =
  map snd
  . filter fst
  . repeatedAndUniques

-- | Utility function used for the implementation of firstRepet and distinct .
--
-- repeated ::
--   Ord a =>
--   List a
--   -> List (Optional a)
-- repeated Nil = Nil
-- repeated l@(h :. t) =
--   map (\(a, _, _) -> a) $
--     take (length l) $
--       produce
--         (exec uniquesState)
--         (Empty, t, S.insert h S.empty)
--   where
--     uniquesState = State uniquesStateFunc
--     uniquesStateFunc (_, (a :. l), uniques) =
--       (
--         (),
--         (
--           if S.member a uniques then Full a else Empty,
--           l,
--           S.insert a uniques
--         )
--       )

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat =
  find (const True)
  . repeated

  -- error "todo: Course.State#firstRepeat"

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct =
  map snd
  . filter (not . fst)
  . repeatedAndUniques

  -- error "todo: Course.State#distinct"

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy =
  contains 1 . firstRepeat . take 20 . produce sumSqrInts
  where
    sumSqrInts = toInteger . sum . map (join (*) . digitToInt) . show'
-- Previous solution:
-- isHappy =
--   contains 1 <$> firstRepeat <$> produce sumSqrInts
--   where
--     digiToSquareInt = (*) <*> id <$> toInteger <$> digitToInt
--     sumSqrInts y =
--       foldRight
--         (+)
--         (0 :: Integer)
--         (digiToSquareInt <$> show' y)

isHappyDebug ::
  Integer
  -> List Integer
isHappyDebug =
  take 10 . produce sumSqrInts
  where
    sumSqrInts = toInteger <$> sum <$> map (join (*) <$> digitToInt) <$> show'
-- error "todo: Course.State#isHappy"

--
-- The following types and functions help me to better understand State
-- by implementig the example of "Bank kata in Haskell - dealing with state"
-- -- see https://codurance.com/2019/02/11/bank-kata-in-haskell-state/
--
data Transaction =
  Deposit Int
  | Withdrawal Int

instance Show Transaction where
  show (Deposit a) = "Deposit " P.++ (show a)
  show (Withdrawal a) = "Withdrawal " P.++ (show a)

deposit :: Int -> State (List Transaction) ()
deposit amount = State (\transactions -> ((), transactions ++ (Deposit amount :. Nil)))

withdraw :: Int -> State (List Transaction) ()
withdraw amount = State (\transactions -> ((), transactions ++ (Withdrawal amount :. Nil)))

getStatement :: State (List Transaction) P.String
getStatement = State (\transactions -> (generateStatement transactions, transactions))

generateStatement :: (List Transaction) -> P.String
generateStatement =
  foldRight
    (\t prevList -> (show t) P.++ "\n" P.++ prevList)
    ""

useMyBank :: State (List Transaction) P.String
useMyBank = do
  deposit 200
  withdraw 100
  getStatement
  deposit 150
  getStatement

useMyBank' :: State (List Transaction) P.String
useMyBank' =
      deposit 200
      >>= \_ -> withdraw 100
      >>= \_ -> getStatement
      >>= \_ -> withdraw 50
      >>= \_ -> getStatement
