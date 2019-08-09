{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.StateExamples where

import           Course.Core
import qualified Prelude                       as P
import           Course.Optional
import           Course.List
import           Course.Functor
import           Course.Applicative
import           Course.Monad
import qualified Data.Set                      as S
import           Course.State

--
-- The following types and functions help me to better understand State
-- by implementig the example of "Bank kata in Haskell - dealing with state"
-- -- see https://codurance.com/2019/02/11/bank-kata-in-haskell-state/
--
data Transaction =
  Deposit Int
  | Withdrawal Int

instance Show Transaction where
    show (Deposit    a) = "Deposit " P.++ (show a)
    show (Withdrawal a) = "Withdrawal " P.++ (show a)

deposit :: Int -> State (List Transaction) ()
deposit amount =
    State (\transactions -> ((), transactions ++ (Deposit amount :. Nil)))

withdraw :: Int -> State (List Transaction) ()
withdraw amount =
    State (\transactions -> ((), transactions ++ (Withdrawal amount :. Nil)))

getStatement :: State (List Transaction) P.String
getStatement =
    State (\transactions -> (generateStatement transactions, transactions))

generateStatement :: (List Transaction) -> P.String
generateStatement =
    foldRight (\t prevList -> (show t) P.++ "\n" P.++ prevList) ""

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

-- The following functions will be used to solve the Princess Arjumand
-- colored tape problem
minSubList :: Ord a => S.Set a -> List a -> (S.Set a, List a, List a, List a)
minSubList uniques list =
    exec
        (filtering predicate list)
        (uniques, Nil, Nil, list)
  where
    predicate a =
        State (\(s, cms, oms, l) ->
            case (S.null s, cms, l) of
            (_, _, Nil) -> (False, (s, cms, oms, l))
            (True, _, (_ :. t)) ->
                ( False
                , ( uniques
                  , Nil
                  , ifThenElse (length cms < length oms) cms oms
                  , t
                  )
                )
            (_, Nil, (_ :. t)) -> (False, (a `S.delete` s, a :. Nil, oms, t))
            (_, p@(h :. pt), (_ :. t)) ->
                ( False
                , ( a `S.delete` s
                  , if a == h then pt ++ (a :. Nil) else p ++ (a :. Nil)
                  , oms
                  , t
                  )
                )
        )

minCompletePrefix :: Ord a => S.Set a -> List a -> List a
minCompletePrefix uniques =
    flip eval uniques
    . filtering (\a -> State (\s -> (S.size s >= 1, a `S.delete` s)))

minCompletePrefixSpan :: Ord a => S.Set a -> List a -> (List a, List a)
minCompletePrefixSpan uniques elems =
    let prefix = minCompletePrefix uniques elems
    in  (prefix, drop (length prefix) elems)

minCompleteTrimedPrefixSpan :: Ord a => S.Set a -> List a -> (List a, List a)
minCompleteTrimedPrefixSpan uniques elems =
    let (prefix, elems') = minCompletePrefixSpan uniques elems
        prefix'          = reverse $ minCompletePrefix uniques (reverse prefix)
    in  (prefix', elems')

minCompleteSubList :: Ord a => S.Set a -> List a -> List a
minCompleteSubList us xs = minCompleteSubList' us xs Nil

minCompleteSubList' :: Ord a => S.Set a -> List a -> List a -> List a
minCompleteSubList' _ Nil minElems = minElems
minCompleteSubList' uniques elems minElems
    | S.null uniques
    = minElems
    | otherwise
    = let
        (minElems', elems') = minCompleteTrimedPrefixSpan uniques elems
      in
        minCompleteSubList'
            uniques
            elems'
            (if length minElems' < length minElems || minElems == Nil
                then minElems'
                else minElems
            )

