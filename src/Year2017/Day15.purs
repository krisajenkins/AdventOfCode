module Year2017.Day15 where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.BigInt (BigInt, fromInt, pow)
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Utils (dec, inc)

type State a =
  { a :: a
  , b :: a
  , matchCount :: Int
  , iterations :: Int
  , judgements :: Int
  }

factorA :: BigInt
factorA = fromInt 16807

factorB :: BigInt
factorB = fromInt 48271

divisor :: BigInt
divisor = fromInt 2147483647

type Strategy =
  BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> (BigInt /\ BigInt /\ Maybe Int)

runGenerators :: Strategy -> Int -> Int -> Int -> State BigInt
runGenerators strategy a b n = tailRec (go strategy)
  { a: fromInt a
  , b: fromInt b
  , matchCount: 0
  , iterations: n
  , judgements: n
  }

go :: Strategy -> State BigInt -> Step (State BigInt) (State BigInt)
go strategy state@{a, b, matchCount, iterations, judgements} =
  if judgements <= 0
     then Done state
     else Loop $ (step strategy) state

sigBits :: BigInt
sigBits = pow (fromInt 2) (fromInt 16)

step :: Strategy -> State BigInt -> State BigInt
step strategy state@{a, b, matchCount, iterations, judgements} =
  { iterations: inc iterations
  , judgements: case hit of
                  Nothing -> judgements -- No judgement, no step.
                  Just _ -> dec judgements
  , a: newA
  , b: newB
  , matchCount: matchCount + fromMaybe 0 hit
  }
  where
    intermediateA = mod (a * factorA) divisor
    intermediateB = mod (b * factorB) divisor
    newA /\ newB /\ hit = strategy a b intermediateA intermediateB

strategy1 :: Strategy
strategy1 oldA oldB intermediateA intermediateB =
  intermediateA /\ intermediateB /\ hit
  where
    hit =
      if mod intermediateA sigBits == mod intermediateB sigBits
      then Just 1
      else Just 0

four :: BigInt
four = fromInt 4

eight :: BigInt
eight = fromInt 8

strategy2 :: Strategy
strategy2 oldA oldB intermediateA intermediateB =
  case mod intermediateA four == zero, mod intermediateB eight == zero of
    true, true ->
      intermediateA /\ intermediateB
      /\ if mod intermediateA sigBits == mod intermediateB sigBits
         then Just 1
         else Just 0
    _, true -> intermediateA /\ oldB /\ Nothing
    true, _ -> oldA /\ intermediateB /\ Nothing
    _, _    -> intermediateA /\ intermediateB /\ Nothing

solution1 :: Int -> State BigInt
solution1 = runGenerators strategy1 634 301

solution2 :: Int -> State BigInt
solution2 = runGenerators strategy2 634 301
