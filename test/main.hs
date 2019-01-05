{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Wordbrain
import qualified Data.Map as Map
import Data.Either(isLeft)
import Data.List(nub, sort)
import Text.RawString.QQ(r)
import qualified Data.ByteString.Char8 as BS8
import Data.FileEmbed(embedFile)
import qualified Data.Trie.BigEndianPatricia.Base as Trie

realDictStr = $(embedFile "/home/mark/projects/wordbrain/words_alpha.txt")
realDict = mkDict (BS8.lines realDictStr)

main = hspec spec

spec = describe "wordbrain" $ do
  describe "readgrid" $ do
    it "happy path" $ do
      readGrid "abc\ndef\n" `shouldBe` Right (Map.fromList [((0,0),'a'),((0,1),'b'),((0,2),'c'),((1,0),'d'),((1,1),'e'),((1,2),'f')])

    it "rejects garbage" $ do
      readGrid "123" `shouldSatisfy` isLeft
    it "downcases" $ do
      readGrid "abc\ndEf\n" `shouldBe` Right (Map.fromList [((0,0),'a'),((0,1),'b'),((0,2),'c'),((1,0),'d'),((1,1),'e'),((1,2),'f')])

  describe "step" $ do
    it "goes nowhere from a dot" $ do
      let grid = Map.fromList [((0,0), 'a')]
      step grid (0,0) `shouldBe` []
    it "can proceed along a line" $ do
      let grid = Map.fromList [((0,x),'a') | x <- [0..10]]
      sort (map fst (step grid (0,3))) `shouldBe` sort [(0,2),(0,4)]

  describe "continuations" $ do
    it "finds art and artistic" $ do
      continuations "art" (mkDict ["art","artistic"])
        `shouldBe` (True,True)
    it "finds finishers only" $ do
      continuations "art" (mkDict ["art"])
        `shouldBe` (True,False)
    it "finds continuers only" $ do
      continuations "art" (mkDict ["artistic"])
        `shouldBe` (False,True)
    it "finds garbage " $ do
      continuations "flargle" (mkDict ["artistic"])
        `shouldBe` (False,False)

  describe "extensions" $ do
    let gridstr = [r|a
rt|]
    let (Right grid) = readGrid gridstr
    let dict = mkDict ["art"]
    it "finds art in a simple grid" $ do
      extensions dict grid  ([(0,0)], "a")
        `shouldBe` Result [] [([(0,0),(1,0)], "ar")]

    it "doesn't find art if it starts in the wrong place" $ do
      extensions  dict grid ([(1,0)],"a")
        `shouldBe` Result [] []

  describe "searchUntil" $ do
    let gridstr = [r|a
rt|]
    let (Right grid) = readGrid gridstr
    let dict = mkDict ["art"]

    it "finds from beginning if length is 3" $ do
      let Result{..} = searchWhile grid dict 3
      map snd complete `shouldBe` ["art"]

    it "fails from beginning if length is 2" $ do
      let Result{..} = searchWhile grid dict 2
      map fst complete `shouldBe` []

  describe "fixpoint" $ do
    it "should work" $ do
      let f = fixpoint (min 10 . (+1))
      f 10 `shouldBe` 10
      f 500 `shouldBe` 10
      f (-2000) `shouldBe` 10
    it "works even with id" $ do
      fixpoint id 10 `shouldBe` 10

  describe "real test" $ do
    let gridstr=[r|
s s e t a d
n h u g e f
e l c r l e
t l a p v l
t i h t a t
b a m i o r|]
    let (Right grid) = readGrid (filter (/=' ') $ dropWhile (=='\n') gridstr)

    it "solves" $ do
      let answers = searchWhile grid realDict 7
      fmap snd (complete answers) `shouldSatisfy` ("mittens" `elem`)

    it "solves knight" $ do
      let gridstr=[r|
n t e t m n e r
e b d i r u g a
r d s m e u a r
o a y o r a n i
p l i m t r n l
a p e b e i w i
l y e e c l m a
i k l s i t d f|]
      let (Right grid) = readGrid (filter (/=' ') $ dropWhile (=='\n') gridstr)
      let answers = searchWhile grid realDict 8
      fmap snd (complete answers) `shouldSatisfy` ("familiar" `elem`)
      print answers
