{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Criterion.Main
import Dict
import Wordbrain
import qualified Data.Map as Map
import Data.Either(isLeft)
import Data.List(nub, sort)
import Text.RawString.QQ(r)


import qualified Data.Trie.BigEndianPatricia.Base as Trie
import qualified Data.Set as Set
import Dict


main =
  let gridstr=[r|
n t e t m n e r
e b d i r u g a
r d s m e u a r
o a y o r a n i
p l i m t r n l
a p e b e i w i
l y e e c l m a
i k l s i t d f|]
      (Right grid) = readGrid (filter (/=' ') $ dropWhile (=='\n') gridstr)
  in

  defaultMain [
    bgroup "grid" [ bench "search"  $ whnfIO (print $ searchWhile grid realDict 8)
                  ]
  ]
