import Test.Hspec
import Wordbrain
import qualified Data.Map as Map
import Data.Either(isLeft)

main = hspec spec

spec = describe "wordbrain" $ do
  describe "readgrid" $ do
    it "happy path" $ do
      readGrid "abc\ndef\n" `shouldBe` Right (Map.fromList  [((0,0),'a'),((0,1),'b'),((0,2),'c'),((1,0),'d'),((1,1),'e'),((1,2),'f')])
    it "rejects garbage" $ do
      readGrid "123" `shouldSatisfy` isLeft
    it "downcases" $ do
      readGrid "abc\ndEf\n" `shouldBe` Right (Map.fromList  [((0,0),'a'),((0,1),'b'),((0,2),'c'),((1,0),'d'),((1,1),'e'),((1,2),'f')])
