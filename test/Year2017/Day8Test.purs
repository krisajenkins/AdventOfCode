module Year2017.Day8Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(Right))
import Data.List (length)
import Node.FS (FS)
import ParserUtils (integer)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Text.Parsing.StringParser (runParser)
import Year2017.Day8 (Condition(..), Direction(..), Instruction(..), Operation(..), conditionParser, lineParser, readInput, solution1, solution2)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day8" do
    parserTests
    solution1Tests
    solution2Tests

parserTests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
parserTests =
  test "Parser" do
    equal (Right 9) (runParser integer "9")
    equal (Right (-9)) (runParser integer "-9")
    equal (Right (Condition { register: "tn"
                            , operation: Gt
                            , value: (-9)
                            }))
          (runParser conditionParser "if tn > -9")
    equal (Right (Instruction { register: "uc"
                                        , direction: Dec
                                        , value: -907
                                        , condition: (Condition { register: "av"
                                                                , operation: Gte
                                                                , value: 800
                                                                })}))
          (runParser lineParser "uc dec -907 if av >= 800")
    instructions <- liftEff readInput
    equal
      1000
      (length instructions)

solution1Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 6012 answer

solution2Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal 6369 answer
