module Wordbrain where
import Data.Map(partition, Map,fromList)
import Data.Char(toLower, isAlpha)
import Debug.Trace(trace)

dotrace label s = trace (label <> ": " <> show s) s

type Coord = (Int,Int)

-- constraints:
--   'a-z' plus ' ' only
-- numbering system from top left corner
readGrid :: String -> Either String (Map Coord Char)
readGrid =
  (\(ok,notOk) ->
     if null notOk
     then Right ok
     else Left ("bad elements: " <> show notOk))
  . partition isAlpha
  . fromList
  . filter ((/=' ') . snd)
  . concatMap (\(rownum,row) -> zipWith (\col char -> ((rownum,col),char)) [0..] row)
  . zip [0..] . lines . map toLower
