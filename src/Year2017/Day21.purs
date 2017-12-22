module Year2017.Day21 where

-- | Oh god, this is so hideous. Please don't look! :-D

import Prelude

import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Tuple.Nested (type (/\), (/\))
import Control.Alternative ((<|>))
import Control.Fold as Fold
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Node.FS (FS)
import ParserUtils (mustSucceed, parseFile)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
import Text.Parsing.StringParser.String (char, string)
import Utils (dec, inc)
import Utils as Utils

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (List Rule)
readInput =
  parseFile ruleParser "src/Year2017/Day21.txt"
    >>= mustSucceed

type Rule = Tuple Pattern Pattern
type Pattern = Array Boolean

ruleParser :: Parser Rule
ruleParser = do
  from <- patternParser
  _ <- string " => "
  to <- patternParser
  pure $ Tuple from to

patternParser :: Parser Pattern
patternParser =
   Array.concat <<< Array.fromFoldable <$> ((Array.fromFoldable <$> (many1 cellParser)) `sepBy` (char '/'))

cellParser :: Parser Boolean
cellParser =
  (char '#' *> pure true)
  <|>
  (char '.' *> pure false)

flipVertically :: Partial => Pattern -> Pattern
flipVertically
  [a, b
  ,c, d] =
  [c, d
  ,a, b]

flipVertically
  [a, b, c
  ,d, e, f
  ,g, h, i] =
  [g, h, i
  ,d, e, f
  ,a, b, c]

flipVertically
  [a, b, c, d
  ,e, f, g, h
  ,i, j, k, l
  ,m, n, o, p] =
  [m, n, o, p
  ,i, j, k, l
  ,e, f, g, h
  ,a, b, c, d]

flipHorizontally :: Partial => Pattern -> Pattern
flipHorizontally
  [a, b
  ,c, d] =
  [b, a
  ,d, c]

flipHorizontally
  [a, b, c
  ,d, e, f
  ,g, h, i] =
  [c, b, a
  ,f, e, d
  ,i, h, g]

flipHorizontally
  [a, b, c, d
  ,e, f, g, h
  ,i, j, k, l
  ,m, n, o, p] =
  [d, c, b, a
  ,h, g, f, e
  ,l, k, j, i
  ,p, o, n, m]

rotateClockwise :: Partial => Pattern -> Pattern
rotateClockwise
  [a, b
  ,d, c] =
  [d, a
  ,c, b]

rotateClockwise
  [a, b, c
  ,d, e, f
  ,g, h, i] =
  [g, d, a
  ,h, e, b
  ,i, f, c]

rotateClockwise
  [a, b, c, d
  ,e, f, g, h
  ,i, j, k, l
  ,m, n, o, p] =
  [m, i, e, a
  ,n, j, f, b
  ,o, k, g, c
  ,p, l, h, d]

------------------------------------------------------------

permutePattern :: Partial => Pattern -> List Pattern
permutePattern pattern =
  map (\f -> f pattern)
    (List.fromFoldable [ id
                       , flipHorizontally
                       , flipVertically
                       , flipHorizontally >>> flipVertically
                       , rotateClockwise
                       , rotateClockwise >>> flipHorizontally
                       , rotateClockwise >>> flipVertically
                       , rotateClockwise >>> flipHorizontally >>> flipVertically
                       ])

permuteRule :: Rule -> List Rule
permuteRule (Tuple from to) =
  unsafePartial $ map (flip Tuple to) (permutePattern from)

permuteRules :: List Rule -> List Rule
permuteRules = List.concatMap permuteRule

type Rulebook = Map Pattern Pattern

readRules :: forall eff.
  Eff (fs :: FS, exception :: EXCEPTION | eff) Rulebook
readRules = Map.fromFoldable <<< permuteRules <$> readInput

type World = Map (Int /\ Int) Boolean

translate :: forall a b. Semiring a => Semiring b => Tuple a b -> Tuple a b -> Tuple a b
translate (dx /\ dy) (x /\ y) = ((dx + x) /\ (dy + y))

toPoints :: Partial => Pattern -> Array (Tuple (Int /\ Int) Boolean)
toPoints [a, b, c, d] =
  [ Tuple (0 /\ 0) a, Tuple (1 /\ 0) b
  , Tuple (0 /\ 1) c, Tuple (1 /\ 1) d
  ]
toPoints
  [a, b, c
  ,d, e, f
  ,g, h, i] =
  [ Tuple (0 /\ 0) a, Tuple (1 /\ 0) b, Tuple (2 /\ 0) c
  , Tuple (0 /\ 1) d, Tuple (1 /\ 1) e, Tuple (2 /\ 1) f
  , Tuple (0 /\ 2) g, Tuple (1 /\ 2) h, Tuple (2 /\ 2) i
  ]
toPoints
  [a, b, c, d
  ,e, f, g, h
  ,i, j, k, l
  ,m, n, o, p] =
  [ Tuple (0 /\ 0) a, Tuple (1 /\ 0) b, Tuple (2 /\ 0) c, Tuple (3 /\ 0) d
  , Tuple (0 /\ 1) e, Tuple (1 /\ 1) f, Tuple (2 /\ 1) g, Tuple (3 /\ 1) h
  , Tuple (0 /\ 2) i, Tuple (1 /\ 2) j, Tuple (2 /\ 2) k, Tuple (3 /\ 2) l
  , Tuple (0 /\ 3) m, Tuple (1 /\ 3) n, Tuple (2 /\ 3) o, Tuple (3 /\ 3) p
  ]

initialWorld :: World
initialWorld =
  unsafePartial $
  Map.fromFoldable $
  toPoints $
    [ false,  true, false
    , false, false,  true
    ,  true,  true,  true
    ]

type State = { count :: Int, world :: World }

showWorld :: forall eff. Int -> World -> Eff (console :: CONSOLE | eff) Unit
showWorld = Utils.showWorld formatter
  where
    formatter true = '#'
    formatter false = '.'

addCreature :: Int -> Rulebook -> ((Int /\ Int) -> Boolean) -> World -> (Int /\ Int) -> World
addCreature tileSize allRules lookup w point@(x/\y) =
  let from :: Array Boolean
      from = lookup <$> offsetN tileSize point
      to :: Maybe (Array Boolean)
      to = Map.lookup from allRules
      newOrigin =
        (((div x tileSize)) * (inc tileSize))
        /\
        (((div y tileSize)) * (inc tileSize))
      newPoints = Array.foldl (\m (Tuple k v) -> Map.insert k v m) w $
         case to of
          Nothing -> []
          Just x -> unsafePartial $ map (lmap (translate newOrigin)) $ toPoints x
  in newPoints

solveForIterations :: forall eff.
  Int ->
  Eff
    (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff)
    Int
solveForIterations count = do
  allRules <- readRules
  -- showWorld 20 initialWorld
  last <- tailRecM (go allRules) { count, world: initialWorld }
  log "DONE"
  -- showWorld 20 last
  pure $ List.length <<< List.filter id <<< Map.values $ last
  where
    go _ { count: 0, world } =
      pure $ Done world
    go allRules { count: n, world } = do
      let size = inc $ Fold.foldl Fold.maximum $ map fst $ Map.keys world
      log $ ">>>> " <> show n <> " <<<< (" <> show size <> ")"
      -- showWorld size world
      case size, mod size 2, mod size 3 of
           0, _, _ -> throwException $ error $ "World is empty!"
           _, 0, _ ->
             let tls = topLefts 2 size
                 newWorld = Array.foldl (addCreature 2 allRules (flip Map.lookup world >>> fromMaybe false)) Map.empty tls
             in pure $ Loop { count: dec n, world: newWorld }
           _, _, 0 ->
             let tls = topLefts 3 size
                 newWorld = Array.foldl (addCreature 3 allRules (flip Map.lookup world >>> fromMaybe false)) Map.empty tls
             in pure $ Loop { count: dec n, world: newWorld }
           _, _, _ -> throwException $ error $ "Unexpected world size: " <> show size

solution1 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff)
    Int
solution1 = do
  solveForIterations 5

offsetN :: Int -> Tuple Int Int -> Array (Tuple Int Int)
offsetN n (x /\ y) = do
  dy <- dec <$> Array.range 1 n
  dx <- dec <$> Array.range 1 n
  pure (x + dx /\ y + dy)

topLefts :: Int -> Int -> Array (Tuple Int Int)
topLefts divisor size =
  let skip = div size divisor
  in do x <- (*) divisor <<< dec <$> Array.range 1 skip
        y <- (*) divisor <<< dec <$> Array.range 1 skip
        pure (x /\ y)

solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff)
    Int
solution2 = do
  solveForIterations 18
