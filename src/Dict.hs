{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Dict where
import Data.FileEmbed(embedFile)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Trie.BigEndianPatricia.Base as Trie


realDictStr = $(embedFile "/home/mark/projects/wordbrain/words_alpha.txt")
realDict = mkDict (BS8.lines realDictStr)

littleDictStr = $(embedFile "/usr/share/dict/words")
littleDict = mkDict (BS8.lines realDictStr)

type Dict = Trie.Trie ()

mkDict = Trie.fromList . map (,())
