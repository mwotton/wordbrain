{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Wordbrain where
import Control.Arrow((***))
import Data.Map(Map)
import qualified Data.Map as M
import Data.Char(toLower, isAlpha)
import Debug.Trace(trace)
import Data.Trie.BigEndianPatricia.Base(Trie)
import qualified Data.Trie.BigEndianPatricia.Base as Trie
import Data.Set(Set)
import Data.ByteString(ByteString)
import Data.Maybe(isJust, catMaybes)
import Prelude hiding(lookup,null)
import qualified Data.List.NonEmpty as NEL
import qualified Data.ByteString.Char8 as BS8
import Data.List(tails,group)
import Dict(mkDict,Dict)

dotrace label s = trace (label <> ": " <> show s) s

type Coord = (Int,Int)
newtype Grid = Grid { unGrid :: Map Coord Char }
  deriving (Show,Eq)
type Path = NEL.NonEmpty Coord


data Result = Result
  { complete :: [WordPath]
  , ongoing :: [WordPath]
  } deriving (Show,Eq)

instance Semigroup Result where
  r1 <> r2 = Result (complete r1 <> complete r2) (ongoing r1 <> ongoing r2)
instance Monoid Result where
  mempty = Result [] []

type WordPath = (Path,String)

-- constraints:
--   'a-z' plus ' ' only
-- numbering system from top left corner
readGrid :: String -> Either String Grid
readGrid =
  (\(ok,notOk) ->
     if M.null notOk
     then Right $ Grid ok
     else Left ("bad elements: " <> show notOk))
  . M.partition isAlpha
  . M.fromList
  . filter ((/=' ') . snd)
  . concatMap (\(rownum,row) -> zipWith (\col char -> ((rownum,col),char)) [0..] row)
  . zip [0..] . lines . map toLower

extensions :: Dict -> Grid -> WordPath -> Result
extensions dict grid orig@(path, word) =
  mconcat . map
  (\wp@(p,w) ->
      let (complete,ongoing) = continuations (BS8.pack w) dict
      in Result (if complete then [wp] else [])
                (if ongoing  then [wp] else []))
  . map (\(coord,ch) -> (path <> pure coord, word <> [ch]))
  $ step grid (NEL.last path)

step :: Grid -> Coord -> [(Coord,Char)]
step (Grid grid) coord =
  catMaybes $ map (\c -> (c,) <$> M.lookup c grid)
              (blindSteps coord)

blindSteps (x,y) =
  [(a,b)| a <- [x-1,x,x+1]
        , b <- [y-1,y,y+1]
        , (a,b)/=(x,y)]

continuations :: ByteString -> Dict -> (Bool,Bool)
continuations string = Trie.lookupBy complete string
  where
    complete v trie = (isJust v, not (Trie.null trie))

allStartingPoints :: Grid -> [WordPath]
allStartingPoints = map (pure *** pure) . M.toList . unGrid

searchWhile :: Grid -> Dict -> Int -> Result
searchWhile grid dict wordLen =
  fixpoint stepOnce (Result [] (allStartingPoints grid))

  where
    stepOnce r =
      let rNext = mconcat $ map (extensions dict grid) (ongoing r)
      in Result (filter ((==wordLen) . length . fst) (complete r <> complete rNext))
                (filter ((<wordLen) . length . fst) (ongoing rNext))


fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = head . head
             . dropWhile  ((/=1) . length . group)
             . map (take 2) . tails . iterate f
